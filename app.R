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

shiny::addResourcePath("resources", 'www/')

ui <- fluidPage(
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(
    script = file.path("resources","js", "shinyjs-funcs.js"),
    functions = c("scrollLogger", "disableModule", "enableModule")
  ),
  #in Wallace this is inside "navbarpage > header ="
    tags$head(tags$link(href = "css/styles.css", rel = "stylesheet")),
    theme = bslib::bs_theme(version = 3,
                          bootswatch = "united"),
   titlePanel("Disaggregation Regression Demonstration Application"),
  fluidRow(
     column(
       4,
       offset = 1,
       align = "left",
       div(style = "margin-top: -10px"),
       strong("Log window"),
       div(style = "margin-top: 5px"),
       div(
         id = "wallaceLog",
         div(id = "logHeader", div(id = "logContent"))
       )
     ),
     column(8,leafletOutput("map"))), 
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
  
  common <- reactiveValues()
  
  # Variable to keep track of current log message
  initLogMsg <- function() {
    intro <- '***WELCOME TO DADA***'
    brk <- paste(rep('------', 14), collapse = '')
    expl <- 'Please find messages for the user in this log window.'
    logInit <- gsub('.{4}$', '', paste(intro, brk, expl, brk, '', sep = '<br>'))
    logInit
  }
  logger <- reactiveVal(initLogMsg())
  common$logger <- logger

  # Write out logs to the log Window
  observeEvent(logger(), {
    shinyjs::html(id = "logHeader", html = logger(), add = FALSE)
    shinyjs::js$scrollLogger()
  })
  
  # create map
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(0, 0, zoom = 2) %>%
      addProviderTiles('Esri.WorldTopoMap') %>%
      leafem::addMouseCoordinates()
    
  })
  # create map proxy to make further changes to existing map
  map <- leafletProxy("map")
  
  observeEvent(input$map_draw_new_feature, {
    coords <- unlist(input$map_draw_new_feature$geometry$coordinates)
    xy <- matrix(c(coords[c(TRUE,FALSE)], coords[c(FALSE,TRUE)]), ncol=2)
    colnames(xy) <- c('longitude', 'latitude')
    common$xy <- xy
  })
  
   
  
  callModule(upload_module_server, "upload", common, map)
  callModule(prepare_module_server, "prepare", common, map)
  callModule(model_module_server, "model", common, map)

}

shinyApp(ui = ui, server = server)

