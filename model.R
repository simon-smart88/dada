modelUI <- function(id) {
  tagList( 
    plotOutput(NS(id,"field_plot")),
    plotOutput(NS(id,"mean_prediction"))
  )
}

modelServer <- function(id,input_data,prepped_data) {
#modelServer <- function(id,prepped_data) {
  moduleServer(id,function(input,output,session){
  
      fitted_model <- reactive({
      tic('fit')
      fitted <- disaggregation::disag_model(data = prepped_data$prep(),
                                         family = 'poisson',
                                         link = 'log')
      toc()
    
      tic('predict')
      prediction <- predict(fitted)
      toc()
      prediction
      })
    
      output$field_plot <- renderPlot({
        plot(mask(fitted_model()$mean_prediction$field, input_data$popn()))
        #plot(fitted_model()$mean_prediction$field)
      })
      
      output$mean_prediction <- renderPlot({
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