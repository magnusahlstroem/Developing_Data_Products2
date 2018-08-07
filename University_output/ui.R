#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Hello world"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
        submitButton(text = "Apply Changes", icon = NULL, width = NULL),
        
        dateInput("Length.out", "Start date", value = "2018-07-01", min = "2018-07-01", max = "2018-07-31",
                  format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                  language = "en", width = NULL, autoclose = TRUE),
        
        dateInput("To", "End date", value = "2018-07-31", min = "2018-07-01", max = "2018-07-31",
                  format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                  language = "en", width = NULL, autoclose = TRUE)
        
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot")
    )
  )
))
