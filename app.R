#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(leaflet)

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
                column(6,plotOutput("incid_plot"),plotOutput("popn_plot")),
                column(6,plotOutput("cov_plot"))
                )),
     tabPanel('Model',
              fluidRow( 
                column(6,plotOutput("mesh_plot")
              ))),
     tabPanel('Results',
              fluidRow( 
                column(6,plotOutput("results_plot")
                )))
     # tabPanel('Runtime',
     #          fluidRow( 
     #            column(6,plotOutput("results_plot")
     #            )))
     )
   
   
   
   # Sidebar with a slider input for number of bins 
   # sidebarLayout(
   #    sidebarPanel(
   #      #fileInput("shapefile", "Choose a .shp file containing disease incidence data", accept = ".shp", multiple=T)
   #    ),
   #    
   #    # Show a plot of the generated distribution
   #    mainPanel(
   #      
   #      plotOutput("popn_plot"),
   #      plotOutput("cov_plot")
   #    )
   # )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  #needed to upload larger files 
  options(shiny.maxRequestSize=30*1024^2) 
  
  incid_data <- reactive({
    tic('load incidence')
    shapes <- shapefile('data/shapes/mdg_shapes.shp')
    shapes@data$ID_2 <- as.numeric(shapes@data$ID_2)
    shapes <- subset(shapes, ID_2 > 10101950)
    toc()
    return(shapes)
  })

  popn_data <- reactive({
    tic('load population')
    population_raster <- raster('data/population_lo_res_3.tif')
    population_raster <- mask(population_raster,incid_data()[,1])
    toc()
    return(population_raster)
  })
  
  cov_data <- reactive({
  tic('load covariates')
  covariate_stack <- disaggregation::getCovariateRasters('data/lores covariates', shape = popn_data())
  covariate_stack <- mask(covariate_stack,incid_data()[,1])
  toc()
  return(covariate_stack)
  })
  
  data_for_model <-  reactive({
    tic('prepare')
    out <- disaggregation::prepare_data(polygon_shapefile = incid_data(), 
                                 covariate_rasters = cov_data(), 
                                 aggregation_raster = popn_data(),
                                 id_var = 'ID_1',
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
    #disaggregation::predict(fitted_model())
    tic('predict')
    out <- predict(fitted_model())
    toc()
    return(out)
  })
  
  #could all be plotted on leaflet with a dropdown to select which to view
  
   output$incid_plot <- renderPlot({
     # tic("Load and plot incidence data")
     spplot(incid_data(), 'inc', main = 'Incidence of malaria in Madagascar')
     # toc()
   })
   
   output$popn_plot <- renderPlot({
     # tic("Load and plot population data")
     plot(log10(popn_data()),main='Population density of Madagascar')
     # toc()
   })
   
   output$cov_plot <- renderPlot({
     # tic("Load and plot covariates")
     plot(cov_data())
     # toc()
   })
   
   output$mesh_plot <- renderPlot({
     # tic("Prepare data and plot mesh")
     plot(data_for_model())[[3]]
     # toc()
   })
   
   output$results_plot <- renderPlot({
     # tic("Predict and plot results")
     plot(mask(model_prediction()$mean_prediction$field, popn_data()))
     # toc()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

