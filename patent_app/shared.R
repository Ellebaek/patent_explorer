library(data.table)
library(readxl)
library(tidyverse)

# load patent data from Excel file
patent_data <- read_excel(".../patent_data.xlsx")

# select the four column to use in app
d <- patent_data[,c(1,2,4,12)]

# separate into rows
d1 <- separate_rows(d, 2, sep="[|]")
# shrink multiple blanks (white space) into single blank character 
for (i in 1:10) {
    d1[,2] <- gsub("  ", " ", d1$`Publication data including kind & date`)
}
# replace blank with "|"
d1[,2] <- gsub(" ", "|", d1$`Publication data including kind & date`)
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
    cleaned_data$AssigneeGroup <- gsub("ALPGA", "ALPHA", cleaned_data$AssigneeGroup)
    cleaned_data$AssigneeGroup <- gsub("YESHIVA UNIVERSITY", "ALBERT EINSTEIN", cleaned_data$AssigneeGroup)
    cleaned_data$AssigneeGroup <- gsub("UNIVERSITY DE LE", "LEON ", cleaned_data$AssigneeGroup)
    cleaned_data$AssigneeGroup <- gsub("TOKIWA", "TOKIWA PHARM", cleaned_data$AssigneeGroup)
    
    # define search terms which will also be used as group name
    needles <- c("ALBION", "ADELAIDE FERTILITY", "ALBERT EINSTEIN", 
                 "WASHINGTON UNIVERSITY", "MERCK", "SBI", "BIOPOLIS",
                 "LEON", "SHENZHEN", "KWANG DONG", "CELLTRION", "TOKIWA PHARM",
                 "SIGMA TAU", "DSM")
    
    for (i in 1:length(needles)) {
        cleaned_data$AssigneeGroup[grep(needles[i], cleaned_data$AssigneeGroup)] <- needles[i]
    }
    
    return(cleaned_data)
}

# apply the defined clean assignee function
as_patents <- clean_assignee(as_patents, 5)


# select two columns for assignee list generation
as <- patent_data[,c(1,4)]
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
ascount <- as_distinct[,.(count=sum(!is.na(`Earliest publication number`))),by=.(AssigneeGroup)][order(-count, AssigneeGroup)]
# combine count and assignee group into new variable to be used in ui and filtering
ascount[,count_assignee:=substr(paste(count, " - ", AssigneeGroup),1,30)]
# set keys for merge/join
setkey(ascount, AssigneeGroup)
setkey(as_patents, AssigneeGroup)
# left join assignee counts onto assignee patents 
# to create base for table in ui
as_patents_display <- ascount[as_patents]
# select and rename columns for display and filtering
as_patents_display <- as_patents_display[,.(`Earliest publication number`, `Assignee`=AssigneeGroup, State=`Family legal state`, `Patent family numbers`=PT_NO, `Publication date`=PT_DATE, count_assignee)] 
# reduce to list of unique combinations 
# the assignee cleaning corrected spelling mistakes etc. creating duplicates
as_patents_display <- unique(as_patents_display, by = colnames(as_patents_display))
# extract country from patent family number
as_patents_display$country <- substr(as_patents_display$`Patent family numbers`, 1,2)

# order descending assignee count to get the list as wanted in ui 
ascount <- ascount[order(-count, AssigneeGroup)]