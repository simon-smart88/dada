model_module_ui <- function(id) {
  tagList( 
    radioButtons(NS(id,"family"), "Model family", c('gaussian','poisson','binomial'),selected = 'gaussian'),
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

model_module_server <- function(input, output, session, common) {
  
      observeEvent(input$fit,{
      tic('fit')
      fitted <- disaggregation::disag_model(data = common$prep,
                                         family = input$family,
                                         link = input$link,
                                         iid = input$iid)
      toc()
      
      common$fit <- fitted
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
      
      })
      
      output$field_plot <- renderPlot({
        req(common$pred)
        plot(mask(common$pred$mean_prediction$field, common$popn))
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
    model_module_ui("model")
  )

  server <- function(input, output, session) {

    input_data_rds <- readRDS('data/input_data.Rds')
    prep_data_rds <- readRDS('data/prepared_data.Rds')

    common <- reactiveValues(shape = input_data_rds$shape,
                             popn = input_data_rds$popn,
                             covs = input_data_rds$cov,
                             prep = prep_data_rds,
                             fit = NULL,
                             pred = NULL)

    callModule(model_module_server, "model", common)

  }
  shinyApp(ui, server)
}

modelApp()