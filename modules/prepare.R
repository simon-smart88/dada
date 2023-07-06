prepare_module_ui<- function(id) {
  tagList( 
    waiter::use_waiter(),
    uiOutput(NS(id,"id_var_out")),
    uiOutput(NS(id,"resp_var_out")),
    sliderInput(NS(id,"mesh_edge"),"Max edge",min=0,max=10,value=c(0.7,8),step = 0.1),
    sliderInput(NS(id,"mesh_cut"),"Cut",min=0,max=1,value=0.05,step = 0.01),
    sliderInput(NS(id,"mesh_offset"),"Offset",min=0,max=10,value=c(1,2),step = 0.1),
    checkboxInput(NS(id,"na_action"), "Handle NAs?", value=F),
    checkboxInput(NS(id,"mesh_make"), "Make mesh?", value=T),
    #verbatimTextOutput(NS(id,"test")),
    actionButton(NS(id,"prepare"), "Prepare data",style='background-color: #89eda0; color:#000;'),
    tableOutput(NS(id,"cov_summary")),
    plotOutput(NS(id,"mesh_plot"))
  )
}

#prepare_module_server <- function(input, output, session, common) {

prepare_module_server <- function(id,data) {
  moduleServer(id,function(input,output,session){
    
    output$id_var_out <- renderUI({
      selectInput(NS(id,"id_var"), "Select ID variable", names(data$shape()),selected = 'ID_2')
    })
    
    output$resp_var_out <- renderUI({
      selectInput(NS(id,"resp_var"), "Select response variable", names(data$shape()),selected = 'inc')
    })

    output$test <- renderPrint({input$mesh_edge})
    
    prep_data <- eventReactive(input$prepare,{
    
      waiter <- waiter::Waiter$new()
      waiter$show()
      on.exit(waiter$hide())
      
    tic('prepare')
    prep <- disaggregation::prepare_data(polygon_shapefile = data$shape(), 
                                        covariate_rasters = data$cov(), 
                                        aggregation_raster = data$popn(),
                                        id_var = as.character(input$id_var),
                                        response_var = as.character(input$resp_var),
                                        mesh.args = list(max.edge = input$mesh_edge, 
                                                         cut = input$mesh_cut, 
                                                         offset = input$mesh_offset),
                                        ncores = 8,
                                        na.action = input$na_action,
                                        makeMesh=input$mesh_make)
    toc()
    prep 
    })
    
    
    output$cov_summary <- renderTable({
      req(prep_data())
      as.data.frame(summary(prep_data()$covariate_rasters))
    },rownames = T)
    
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


# prepare_module_result <- function(id) {
#   ns <- NS(id)
#   # Result UI as html
#   htmlOutput(ns("cov_summary"))
#   htmlOutput(ns("mesh_plot"))
# }



  prepareApp <- function() {
  ui <- fluidPage(
    prepare_module_ui("prep")
  )

  #data <- R6Class("data", list())

  server <- function(input, output, session) {
    input_data_rds <- readRDS('data/input_data.Rds')
    input_data <- list(popn = reactive(input_data_rds$popn), cov = reactive(input_data_rds$cov),shape=reactive(input_data_rds$shape))

    prepare_module_server("prep",input_data)
  }
  shinyApp(ui, server)
}

  prepareApp()