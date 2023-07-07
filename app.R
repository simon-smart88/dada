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

ui <- fluidPage(
   
   # Application title
   titlePanel("Disaggregation Regression Demonstration Application"),
   tabsetPanel(
     tabPanel('Upload',
              sidebarLayout(
                sidebarPanel(upload_module_ui("upload")[1],
                             upload_module_ui("upload")[2],
                             upload_module_ui("upload")[3]
                             ),
                mainPanel(
                  fluidRow(
                upload_module_ui("upload")[4],
                upload_module_ui("upload")[5],
                upload_module_ui("upload")[6]
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
  
  common <- reactiveValues()
  
  callModule(upload_module_server, "upload", common)
  callModule(prepare_module_server, "prepare", common)
  callModule(model_module_server, "model", common)


}

shinyApp(ui = ui, server = server)

