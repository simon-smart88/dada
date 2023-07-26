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
library(R6)
library(gargoyle)

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
    common$poly <- xy
    trigger("change_poly")
  })
  
  #common <- reactiveValues()
  
 common_class <- R6::R6Class(
    classname = "common",
    public = list(
      shape = NULL,
      popn = NULL,
      covs = NULL,
      prep = NULL,
      fit = NULL,
      pred = NULL,
      map_layers = NULL,
      poly = NULL,
      add_map_layer = function(new_names) {
        for (new_name in new_names){
          if (!(new_name %in% self$map_layers)){
            self$map_layers <- c(self$map_layers,new_name) 
            invisible(self)
          }
        }
      }
    )
  )
 
 common <- common_class$new()
  
 init("change_shape")
 init("change_popn")
 init("change_covs")
 init("change_poly")
 init("change_prep")
 init("change_fit")
 init("change_pred")
 
  callModule(upload_module_server, "upload", common, map)
  callModule(prepare_module_server, "prepare", common, map)
  callModule(model_module_server, "model", common, map)

}

shinyApp(ui = ui, server = server)

