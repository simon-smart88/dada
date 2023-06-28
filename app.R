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

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Disaggregation Regression Demonstration Application"),
   tabsetPanel(   
     tabPanel('Prepare data',
              fluidRow( 
                column(4,prepareUI("prep")[1],prepareUI("prep")[2]),
                column(8,prepareUI("prep")[3])
              )),
     tabPanel('Model',
              fluidRow( 
                column(6,plotOutput("mesh_plot")
              ))),
     tabPanel('Results',
              fluidRow( 
                column(6,plotOutput("results_plot")
                )))

))

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  data <- R6Class("data", list())
  
  #needed to upload larger files 
  options(shiny.maxRequestSize=30*1024^2) 
  
  data_for_model <-  reactive({
    tic('prepare')
    out <- disaggregation::prepare_data(polygon_shapefile = data$incid, 
                                 covariate_rasters = data$cov, 
                                 aggregation_raster = data$popn,
                                 id_var = 'ID_2',
                                 response_var = 'inc',
                                 mesh.args = list(max.edge = c(0.7, 8), 
                                                  cut = 0.05, 
                                                  offset = c(1, 2)),
                                 ncores = 8,
                                 na.action = TRUE)
    toc()
    return(out)
  })
  
  fitted_model <- reactive({
    tic('fit')
    out <- disaggregation::disag_model(data = data_for_model(),
                              family = 'poisson',
                              link = 'log')
    toc()
    return(out)
  })
  
  model_prediction <- reactive({
    tic('predict')
    out <- predict(fitted_model())
    toc()
    return(out)
  })
  
   output$mesh_plot <- renderPlot({
     plot(data_for_model())[[3]]
   })
   
   output$results_plot <- renderPlot({
     plot(mask(model_prediction()$mean_prediction$field, data$popn))
   })
   
   prepareServer("prep",data)
}

# Run the application 
shinyApp(ui = ui, server = server)

