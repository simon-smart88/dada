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

MB <- 1024^2
UPLOAD_SIZE_MB <- 5000
options(shiny.maxRequestSize = UPLOAD_SIZE_MB*MB)

source('modules/upload.R')
source('modules/prepare.R')
source('modules/model.R')

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
                uploadUI("upload")[4],
                uploadUI("upload")[5],
                uploadUI("upload")[6]
              )))),
     tabPanel('Prepare',
              fluidRow(
                prepare_module_ui("prepare")
              )
              ),
     tabPanel('Model',
              fluidRow(
                model_module_ui("model")
                )
              )
)
)

server <- function(input, output) {
  
  common <- reactiveValues(shape = NULL,
                           popn = NULL,
                           covs = NULL,
                           prep = NULL,
                           fit = NULL)
  
  callModule(upload_module_server, "upload", common)
  callModule(prepare_module_server, "prep", common)
  callModule(model_module_server, "model", common)


}

# Run the application 
shinyApp(ui = ui, server = server)

