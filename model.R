modelUI <- function(id) {
  tagList( 
    plotOutput(NS(id,"results_plot"))
  )
}

modelServer <- function(id,input_data,prepped_data) {
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
    
      output$results_plot <- renderPlot({
        plot(mask(fitted_model()$mean_prediction$field, input_data$popn()))
      })
      
    })
}


modelApp <- function() {
  ui <- fluidPage(
    modelUI("model")
  )
  
  data <- R6Class("data", list())
  
  server <- function(input, output, session) {
    uploadServer("upload",data)
    prepareServer("prep",data)
    modelServer("model",data)
    
  }
  shinyApp(ui, server)  
}

modelApp()