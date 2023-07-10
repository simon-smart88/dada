model_module_ui <- function(id) {
  tagList( 
    radioButtons(NS(id,"family"), "Model family", c('gaussian','poisson','binomial'),selected = 'poisson'),
    radioButtons(NS(id,"link"), "Model link", c('logit','log','identity'), selected = 'log'),
    checkboxInput(NS(id,"field"), "Use field", value=TRUE),
    checkboxInput(NS(id,"iid"), "iid", value=TRUE),
    actionButton(NS(id,"fit"), "Fit model",style='background-color: #89eda0; color:#000;'),
    actionButton(NS(id,"predict"), "Plot predictions",style='background-color: #89eda0; color:#000;'),
    plotOutput(NS(id,"model_plot")),
    plotOutput(NS(id,"field_plot")),
    plotOutput(NS(id,"mean_prediction"))
  )
}

model_module_server <- function(input, output, session, common, map, id) {
  
      disable('predict')
  
      observeEvent(input$fit,{
      tic('fit')
      fitted <- disaggregation::disag_model(data = common$prep,
                                         family = input$family,
                                         link = input$link,
                                         iid = input$iid)
      toc()
      
      common$fit <- fitted
      enable('predict')
      })

      
      output$model_plot <- renderPlot({
        req(common$fit)
        plot(common$fit)
      })
      
      observeEvent(input$predict,{
      tic('predict')
      prediction <- predict(common$fit)
      toc()
      common$pred <- prediction
      
      #mask field and set a CRS
      common$pred$mean_prediction$field <- mask(common$pred$mean_prediction$field, common$popn)
      crs(common$pred$mean_prediction$field) <- crs(common$covs[[1]])
      })
      
      
      observeEvent(input$predict,{
        common$map_layers <- c(common$map_layers,'Field','Prediction')
        
        ex <- extent(common$shape)
        
        pal1 <- colorBin("YlOrRd", domain = values(common$pred$mean_prediction$field), bins = 9, na.color ="#00000000")
        pal2 <- colorBin("YlOrRd", domain = values(common$pred$mean_prediction$prediction), bins = 9, na.color ="#00000000")
        map %>%
          addRasterImage(common$pred$mean_prediction$field,group='Field',colors = pal1) %>%
          addLegend(position ="bottomleft",pal = pal1, values = values(common$pred$mean_prediction$field), group='Field', title='Field') %>%
          addRasterImage(common$pred$mean_prediction$prediction,group='Prediction',colors = pal2) %>%
          addLegend(position ="bottomright",pal = pal2, values = values(common$pred$mean_prediction$prediction), group='Prediction', title='Prediction') %>%
          fitBounds(lng1=ex@xmin,lng2=ex@xmax,lat1=ex@ymin,lat2=ex@ymax) %>%
          addLayersControl(overlayGroups = common$map_layers, options = layersControlOptions(collapsed = FALSE)) %>%
          hideGroup(common$map_layers[2:(length(common$map_layers)-2)]) #hide all but first and last two layers
      })
      
      output$field_plot <- renderPlot({
        req(common$pred)
        plot(common$pred$mean_prediction$field)
      })
      
      output$mean_prediction <- renderPlot({
        req(common$pred)
        plot(common$pred$mean_prediction$prediction)
      })
    
}


# model_module_result <- function(id) {
#   ns <- NS(id)
#   # Result UI as html
#   htmlOutput(ns("model_plot"))
#   htmlOutput(ns("field_plot"))
#   htmlOutput(ns("mean_prediction"))
# }

modelApp <- function() {
  ui <- fluidPage(
    shinyjs::useShinyjs(),
    leafletOutput("uploadmap"),
    model_module_ui("model")
  )

  server <- function(input, output, session) {
    
    # create map
    output$uploadmap <- renderLeaflet(
      leaflet() %>%
        setView(0, 0, zoom = 2) %>%
        addProviderTiles('Esri.WorldTopoMap') %>%
        leafem::addMouseCoordinates()
    )
    # create map proxy to make further changes to existing map
    map <- leafletProxy("uploadmap")
    
    input_data_rds <- readRDS('data/input_data.Rds')
    prep_data_rds <- readRDS('data/prepared_data.Rds')

    common <- reactiveValues(shape = input_data_rds$shape,
                             popn = input_data_rds$popn,
                             covs = input_data_rds$cov,
                             prep = prep_data_rds,
                             map_layers = NULL)

    callModule(model_module_server, "model", common, map)

  }
  shinyApp(ui, server)
}

modelApp()