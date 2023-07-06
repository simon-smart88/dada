#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(leaflet)
library(R6)
library(shiny)
library(disaggregation)
library(raster)
library(ggplot2)
library(dplyr)
library(INLA)
library(tictoc)

source('global.R')

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Disaggregation Regression Demonstration Application"),
   tabsetPanel(
     tabPanel('Upload',
              sidebarLayout(
                sidebarPanel(uploadUI("upload")[1],
                             uploadUI("upload")[2],
                             uploadUI("upload")[3]
                             ),
                mainPanel(
                  fluidRow(
                column(4,uploadUI("upload")[4],uploadUI("upload")[5]),
                column(8,uploadUI("upload")[6])
              )))),
     tabPanel('Prepare',
              fluidRow(
                column(6,prepareUI("prepare")
              ))),
     tabPanel('Model',
              fluidRow(
                column(6,modelUI("model")
                )))
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  
  uploaded_data <- uploadServer("upload")

  prepared_data <- prepareServer("prepare",uploaded_data)
   modelServer("model",uploaded_data,prepared_data)
}

# Run the application 
shinyApp(ui = ui, server = server)

