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

#model_module_server <- function(input, output, session, common,input_data,prepped_data) {
model_module_server <- function(id,input_data,prepped_data) {
    moduleServer(id,function(input,output,session){
  
      fitted_model <- eventReactive(input$fit,{
      tic('fit')
      fitted <- disaggregation::disag_model(data = prepped_data$prep(),
                                         family = input$family,
                                         link = input$link,
                                         iid = input$iid)
      toc()
      output$model_plot <- renderPlot({
        plot(fitted)
      })
      return(fitted)
      })

      #prediction <- eventReactive(input$predict,{
      observeEvent(input$predict,{
      tic('predict')
      prediction <- predict(fitted_model())
      
      output$field_plot <- renderPlot({
        plot(mask(prediction$mean_prediction$field, input_data$popn()))
      })
      
      output$mean_prediction <- renderPlot({
        plot(prediction$mean_prediction$prediction)
      })
      
      toc()
      return(prediction)
      })
    
      # output$model_plot <- renderPlot({
      #   req(fitted_model())
      #   plot(fitted_model())
      # })
      
      # output$field_plot <- renderPlot({
      #   #req(fitted_model())
      #   #plot(mask(fitted_model()$mean_prediction$field, input_data$popn()))
      #   req(prediction())
      #   plot(mask(prediction()$mean_prediction$field, input_data$popn()))
      # })
      # 
      # output$mean_prediction <- renderPlot({
      #   # req(fitted_model())
      #   # plot(fitted_model()$mean_prediction$prediction)
      #   req(prediction())
      #   plot(prediction()$mean_prediction$prediction)
      # })
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
    input_data <- list(popn = reactive(input_data_rds$popn))
    prepped_data <- list(prep = reactive(readRDS('data/prepared_data.Rds')))

    model_module_server("model",input_data,prepped_data)

  }
  shinyApp(ui, server)
}

modelApp()