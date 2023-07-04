modelUI <- function(id) {
  tagList( 
    radioButtons(NS(id,"family"), "Model family", c('gaussian','poisson','binomial'),selected = 'gaussian'),
    radioButtons(NS(id,"link"), "Model link", c('logit','log','identity'), selected = 'log'),
    checkboxInput(NS(id,"field"), "Use field", value=TRUE),
    checkboxInput(NS(id,"iid"), "iid", value=TRUE),
    actionButton(NS(id,"fit"), "Fit model",style='background-color: #89eda0; color:#000;'),
    #actionButton(NS(id,"predict"), "Plot predictions",style='background-color: #89eda0; color:#000;'), 
    plotOutput(NS(id,"field_plot")),
    plotOutput(NS(id,"mean_prediction"))
  )
}

modelServer <- function(id,input_data,prepped_data) {
  moduleServer(id,function(input,output,session){
  
      fitted_model <- eventReactive(input$fit,{
      tic('fit')
      fitted <- disaggregation::disag_model(data = prepped_data$prep(),
                                         family = input$family,
                                         link = input$link,
                                         iid = input$iid)
      toc()
      # fitted
      # })
      # 
      # prediction <- eventReactive(input$predict,{
      tic('predict')
      #prediction <- predict(fitted_model())
      prediction <- predict(fitted)
      toc()
      prediction
      })
    
      output$field_plot <- renderPlot({
        req(fitted_model())
        #req(prediction())
        #plot(mask(prediction()$mean_prediction$field, input_data$popn()))
        plot(mask(fitted_model()$mean_prediction$field, input_data$popn()))
      })
      
      output$mean_prediction <- renderPlot({
        req(fitted_model())
        #req(prediction())
        #plot(prediction()$mean_prediction$prediction)
        plot(fitted_model()$mean_prediction$prediction)
      })
    })
}


modelApp <- function() {
  ui <- fluidPage(
    modelUI("model")
  )
  
  server <- function(input, output, session) {
    
    input_data_rds <- readRDS('data/input_data.Rds')
    input_data <- list(popn = reactive(input_data_rds$popn))
    prepped_data <- list(prep = reactive(readRDS('data/prepared_data.Rds')))
    
    modelServer("model",input_data,prepped_data)
    
  }
  shinyApp(ui, server)  
}

modelApp()