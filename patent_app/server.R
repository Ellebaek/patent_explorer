#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


## general code, only executed once
library(shiny)
library(ggplot2)
library(data.table)

source(".../patent_app/shared.R")

## data driven code, executed at each refresh
shinyServer(function(input, output) {
    output$patent_plot <- renderPlot({
        
        # publication numbers associated with selected assignees        
        publication_numbers <- unique(as_patents_display[count_assignee %in% input$assignee_list]$`Earliest publication number`)
        
        # order (ascending/descending) for bar-chart top/bottom checkbox
        order_sign <- (input$bottom*2 - 1)
        
        # for selected assignees count number of patent family numbers
        dcount <- d3[`Earliest publication number` %in% publication_numbers][,.(country, OCount=sum(!is.na(PT_NO))),by=.(country)][,.(country,OCount)][order(order_sign*OCount)]
        
        # initiate plot
        p <- ggplot()
        # conditional on whether to color by state or not, create data and plot
        if (input$state) {
            # create overall assignee x state table (skeleton for data) 
            d2count <- rbind(cbind(dcount,State=rep("ALIVE",nrow(dcount))),cbind(dcount,State=rep("DEAD",nrow(dcount))))
            # set keys for left join of sub count
            setkeyv(d2count, c("country", "State"))
            # count number of ALIVE and DEAD patent family numbers for each country (sub counts)
            dscount <- d3[`Earliest publication number` %in% publication_numbers][,.(country, State=`Family legal state`, SCount=sum(!is.na(PT_NO))),by=.(country, `Family legal state`)][,.(country,State,SCount)]
            # set join/merge key
            setkeyv(dscount, c("country", "State"))
            # left join sub counts onto skeleton
            dmerge <- dscount[d2count]
            # fill zero values for all non-present combinations
            dmerge$SCount[is.na(dmerge$SCount)] <- 0 
            
            # determine number of rows to extract for bar-chart creation
            row_count <- max(input$bar_count * 2,2, na.rm = TRUE)
            # filter selected countries, order by overall country count
            input_data <- dmerge[country %in% input$country_list][order(order_sign*OCount)]
            # build plot
            p <- p + geom_col(data=input_data[1:min(row_count,nrow(input_data)),], aes(x=reorder(country, -OCount), y=SCount, fill=State)) + 
                labs(fill = "State")  +
                scale_fill_manual("State", values = c("#E69F00","#56B4E9"))
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
})