prepareUI <- function(id) {
  tagList( 
    plotOutput(NS(id,"mesh_plot"))
  )
}

prepareServer <- function(id,data) {
  moduleServer(id,function(input,output,session){
  
    prep_data <- reactive({
    tic('prepare')
    prep <- disaggregation::prepare_data(polygon_shapefile = data$shape(), 
                                        covariate_rasters = data$cov(), 
                                        aggregation_raster = data$popn(),
                                        id_var = 'ID_2',
                                        response_var = 'inc',
                                        mesh.args = list(max.edge = c(0.7, 8), 
                                                         cut = 0.05, 
                                                         offset = c(1, 2)),
                                        ncores = 8,
                                        na.action = TRUE)
    toc()
    prep 
    })
    
    output$mesh_plot <- renderPlot({
      req(prep_data())
      plot(prep_data())[[3]]
    })
    
    return(
     list(
        prep = reactive(prep_data())
      )
    )
    
    })
}
    
  prepareApp <- function() {
  ui <- fluidPage(
    uploadUI("upload"),
    prepareUI("prep")
  )
  
  #data <- R6Class("data", list())
  
  server <- function(input, output, session) {
    uploaded_data <- uploadServer("upload")
    
    prepareServer("prep",uploaded_data)
  }
  shinyApp(ui, server)  
}

  prepareApp()