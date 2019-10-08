#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
source(".../patent_app/shared.R")

# Define UI for application
shinyUI(fluidPage(

    # Application title
    titlePanel("Patent Explorer"),
    
    # Sidebar with inputs
    sidebarLayout(
        sidebarPanel(
            numericInput(inputId = "bar_count",
                         label = "Number of bars to display",
                         value = 5),
            
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
))
