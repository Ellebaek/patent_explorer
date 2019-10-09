#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(ggplot2)
library(data.table)
library(readxl)
library(tidyverse)

# read file/lines and replace ," with #" and afterwards " with blank
d_raw  <- readLines("./patent_data_raw.csv")
d_rep  <- gsub(pattern = ',"', replace = '#"', x = d_raw)
d_rep  <- gsub(pattern = '"', replace = '', x = d_rep)

# load data into data frame
patent_data <- read.csv(text = d_rep, sep = "#", header = TRUE)

# column id's to be used throughout the app
col_ids <- c(1,2,3,4)

# select the four column to use in app
d <- patent_data[,col_ids]
for (i in 1:length(col_ids)) {
    d[,i] <- toupper(d[[i]])
}
colnames(d) <- c("earliest", "family_details", "assignees", "state")

# separate into rows
d1 <- separate_rows(d, 2, sep="[|]")
# shrink multiple blanks (white space) into single blank character 
for (i in 1:10) {
    d1[,2] <- gsub("  ", " ", d1[[2]])
}
# replace blank with "|"
d1[,2] <- gsub(" ", "|", d1[[2]])
# separate inte columns
d2 <- separate(d1, 2, c("PT_NO", "PT_CC", "PT_DATE", NA), sep="[|]")
# extract first two characters from patent family number into country column
d2$country <- substr(d2$PT_NO, 1,2)
# convert from data.frame to data.table
d3 <- as.data.table(d2)
# extract unique countries for ui select (pickerInput)
countries <- sort(unique(d3$country))

# separate assignee list into rows
as_patents <- separate_rows(d3, 5, sep="[|]")

# define function to clean and group assignees
clean_assignee <- function(unclean_assignee, assignee_col) {
    cleaned_data <- unclean_assignee
    cleaned_data$AssigneeGroup <- cleaned_data[, ..assignee_col]
    cleaned_data$AssigneeGroup <- gsub("SPRAGUE DAVID", "DAVID SPRAGUE", cleaned_data$AssigneeGroup)
    cleaned_data$AssigneeGroup <- gsub("WRIGHT JENNIFER", "JENNIFER WRIGHT", cleaned_data$AssigneeGroup)
    cleaned_data$AssigneeGroup <- gsub("MEAD JOHNSON", "MJN", cleaned_data$AssigneeGroup)
    cleaned_data$AssigneeGroup <- gsub("BLOCK NUTRITION", "BLOCK NUTRITIONALS", cleaned_data$AssigneeGroup)
    cleaned_data$AssigneeGroup <- gsub("SUPRA NATURALS", "SUPRANATURALS", cleaned_data$AssigneeGroup)
    cleaned_data$AssigneeGroup <- gsub("M JNUS", "MJN", cleaned_data$AssigneeGroup)
    
    # define search terms which will also be used as group name
    needles <- c("FONTERRA", "SAKURA", "LOTTE", "MJN",
                 "PRINCETON NUTRITION", "RECKITT BENCKISER", 
                 "BUILDING BLOCK NUTRITIONALS")
    
    for (i in 1:length(needles)) {
        cleaned_data$AssigneeGroup[grep(needles[i], cleaned_data$AssigneeGroup)] <- needles[i]
    }
    
    return(cleaned_data)
}

# apply the defined clean assignee function
as_patents <- clean_assignee(as_patents, 5)


# select two columns for assignee list generation
as <- d[,c(1,3)]
# seperate into rows
as1 <- separate_rows(as, 2, sep="[|]")
# convert from data.fram to data.table
as2 <- data.table(as1)
# clean assignees
as2 <- clean_assignee(as2,2)
# identify disting patent number and assignee group combinations
as_distinct <- as2[,c(1,3)]
as_distinct <- unique(as_distinct, by = colnames(as_distinct))

# count unique patent numbers for each assignee, order descending by count and ascending by name
ascount <- as_distinct[,.(count=sum(!is.na(earliest))),by=.(AssigneeGroup)][order(-count, AssigneeGroup)]
# combine count and assignee group into new variable to be used in ui and filtering
ascount[,count_assignee:=substr(paste(count, " - ", AssigneeGroup),1,30)]
# set keys for merge/join
setkey(ascount, AssigneeGroup)
setkey(as_patents, AssigneeGroup)
# left join assignee counts onto assignee patents 
# to create base for table in ui
as_patents_display <- ascount[as_patents]
# select and rename columns for display and filtering
as_patents_display <- as_patents_display[,.(`Earliest publication number`=earliest, `Assignee`=AssigneeGroup, `Family legal state`=state, `Patent family numbers`=PT_NO, `Publication date`=PT_DATE, count_assignee)] 
# reduce to list of unique combinations 
# the assignee cleaning corrected spelling mistakes etc. creating duplicates
as_patents_display <- unique(as_patents_display, by = colnames(as_patents_display))
# extract country from patent family number
as_patents_display$country <- substr(as_patents_display$`Patent family numbers`, 1,2)

# order descending assignee count to get the list as wanted in ui 
ascount <- ascount[order(-count, AssigneeGroup)]





# Define UI for application
ui <- fluidPage(
    
    # Application title
    titlePanel("Patent Explorer"),
    
    # Sidebar with inputs
    sidebarLayout(
        sidebarPanel(
            numericInput(inputId = "bar_count",
                         label = "Number of bars to display",
                         value = 10),
            
            checkboxInput("state", "Show state", FALSE),
            checkboxInput("bottom", "Show bottom", FALSE),
            
            pickerInput(inputId = "assignee_list", 
                        label = "Select assignees", 
                        choices = ascount$count_assignee,
                        selected = ascount$count_assignee,
                        options = list(`actions-box` = TRUE),
                        multiple = T),
            
            pickerInput(inputId = "country_list", 
                        label = "Select countries", 
                        choices = countries,
                        selected = countries,
                        options = list(`actions-box` = TRUE),
                        multiple = T)
            
        ),
        
        # display outputs
        mainPanel(
            plotOutput("patent_plot"),
            
            tableOutput("assignee_table")
        )
    )
)


## data driven code, executed at each refresh
server <- function(input, output) {
    output$patent_plot <- renderPlot({
        
        # publication numbers associated with selected assignees        
        publication_numbers <- unique(as_patents_display[count_assignee %in% input$assignee_list]$`Earliest publication number`)
        
        # order (ascending/descending) for bar-chart top/bottom checkbox
        order_sign <- (input$bottom*2 - 1)
        
        # for selected assignees count number of patent family numbers
        dcount <- d3[earliest %in% publication_numbers][,.(country, OCount=sum(!is.na(PT_NO))),by=.(country)][,.(country,OCount)][order(order_sign*OCount)]
        
        # initiate plot
        p <- ggplot()
        # conditional on whether to color by state or not, create data and plot
        if (input$state) {
            # create overall assignee x state table (skeleton for data) 
            d2count <- rbind(cbind(dcount,state=rep("ALIVE",nrow(dcount))),cbind(dcount,state=rep("DEAD",nrow(dcount))))
            # set keys for left join of sub count
            setkeyv(d2count, c("country", "state"))
            # count number of ALIVE and DEAD patent family numbers for each country (sub counts)
            dscount <- d3[earliest %in% publication_numbers][,.(country, state, SCount=sum(!is.na(PT_NO))),by=.(country, state)][,.(country,state,SCount)]
            # set join/merge key
            setkeyv(dscount, c("country", "state"))
            # left join sub counts onto skeleton
            dmerge <- dscount[d2count]
            # fill zero values for all non-present combinations
            dmerge$SCount[is.na(dmerge$SCount)] <- 0 
            
            # determine number of rows to extract for bar-chart creation
            row_count <- max(input$bar_count * 2,2, na.rm = TRUE)
            # filter selected countries, order by overall country count
            input_data <- dmerge[country %in% input$country_list][order(order_sign*OCount)]
            # build plot
            p <- p + geom_col(data=input_data[1:min(row_count,nrow(input_data)),], aes(x=reorder(country, -OCount), y=SCount, fill=state)) + 
                labs(fill = "Family state")  +
                scale_fill_manual("Family state", values = c("#E69F00","#56B4E9"))
        } else {
            # determine number of rows to extract for bar-chart creation
            row_count <- max(input$bar_count,1, na.rm = TRUE)
            # filter selected countries, order by overall country count
            input_data <- dcount[country %in% input$country_list][order(order_sign*OCount)]
            # build plot
            p <- p + geom_col(data=input_data[1:min(row_count,nrow(input_data)),], aes(x=reorder(country, -OCount), y=OCount), fill = "#009E73")
        }
        
        # add labels to plot
        p <- p + 
            labs(x = "Country", y = "Number of patent family numbers") +
            labs(title = "Patents by country")
        # print/return plot
        p
    })
    
    output$assignee_table <- renderTable({
        # copy data table to avoid issues when deleting columns
        t <- copy(as_patents_display)
        # return table corresponding to filter and only including columns to be displayed
        t[count_assignee %in% input$assignee_list][country %in% input$country_list][,c("count_assignee", "country"):=NULL]
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
