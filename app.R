library(leaflet)
library(leaflet.extras)
library(rgeos)
library(shiny)
library(disaggregation)
library(raster)
library(ggplot2)
library(dplyr)
library(INLA)
library(tictoc)
library(shinyjs)

rm(list = ls())

MB <- 1024^2
UPLOAD_SIZE_MB <- 5000
options(shiny.maxRequestSize = UPLOAD_SIZE_MB*MB)

source('modules/upload.R')
source('modules/prepare.R')
source('modules/model.R')

ui <- fluidPage(
  shinyjs::useShinyjs(),
   # Application title
   titlePanel("Disaggregation Regression Demonstration Application"),
   fluidRow(column(12,leafletOutput("map"))), 
   tabsetPanel(
     tabPanel('Upload',
              sidebarLayout(sidebarPanel(upload_module_ui("upload")[1:5]),
                mainPanel(fluidRow(upload_module_ui("upload")[6:8])
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
  
  # create map
  output$map <- renderLeaflet(
    leaflet() %>%
      setView(0, 0, zoom = 2) %>%
      addProviderTiles('Esri.WorldTopoMap') %>%
      leafem::addMouseCoordinates()
  )
  # create map proxy to make further changes to existing map
  map <- leafletProxy("map")
  
  observeEvent(input$map_draw_new_feature, {
    coords <- unlist(input$map_draw_new_feature$geometry$coordinates)
    xy <- matrix(c(coords[c(TRUE,FALSE)], coords[c(FALSE,TRUE)]), ncol=2)
    colnames(xy) <- c('longitude', 'latitude')
    common$xy <- xy
  })
  
  common <- reactiveValues()
  
  callModule(upload_module_server, "upload", common, map)
  callModule(prepare_module_server, "prepare", common, map)
  callModule(model_module_server, "model", common, map)

}

shinyApp(ui = ui, server = server)

