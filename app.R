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
              sidebarLayout(sidebarPanel(upload_module_ui("upload")[1:3]),
                mainPanel(fluidRow(upload_module_ui("upload")[4:6])
              ))),
     tabPanel('Prepare',
              sidebarLayout(sidebarPanel(prepare_module_ui("prepare")[1:9]),
                            mainPanel(fluidRow(prepare_module_ui("prepare")[10:11])
                            ))),
     tabPanel('Model',
              sidebarLayout(sidebarPanel(model_module_ui("model")[1:6]),
                            mainPanel(fluidRow(model_module_ui("model")[7:9])
                            ))),
)
)

server <- function(input, output) {
  
  common <- reactiveValues()
  
  callModule(upload_module_server, "upload", common)
  callModule(prepare_module_server, "prepare", common)
  callModule(model_module_server, "model", common)

}

shinyApp(ui = ui, server = server)

