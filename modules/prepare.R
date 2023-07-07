prepare_module_ui<- function(id) {
  ns <- shiny::NS(id)
  tagList( 
    waiter::use_waiter(),
    uiOutput(ns("id_var_out")),
    uiOutput(ns("resp_var_out")),
    sliderInput(NS(id,"mesh_edge"),"Max edge",min=0,max=10,value=c(0.7,8),step = 0.1),
    sliderInput(NS(id,"mesh_cut"),"Cut",min=0,max=1,value=0.05,step = 0.01),
    sliderInput(NS(id,"mesh_offset"),"Offset",min=0,max=10,value=c(1,2),step = 0.1),
    checkboxInput(NS(id,"na_action"), "Handle NAs?", value=F),
    checkboxInput(NS(id,"mesh_make"), "Make mesh?", value=T),
    actionButton(NS(id,"prepare"), "Prepare data",style='background-color: #89eda0; color:#000;'),
    tableOutput(NS(id,"cov_summary")),
    plotOutput(NS(id,"mesh_plot"))
  )
}

prepare_module_server <- function(input, output, session, common) {
    
    output$id_var_out <- renderUI({
      req(common$shape)
      ns <- session$ns
      selectInput(ns("id_var"), "Select ID variable", names(common$shape),selected = 'ID_2')
    })
    
    output$resp_var_out <- renderUI({
      req(common$shape)
      ns <- session$ns
      selectInput(ns("resp_var"), "Select response variable", names(common$shape),selected = 'inc')
    })

    observeEvent(input$prepare,{
    
      waiter <- waiter::Waiter$new()
      waiter$show()
      on.exit(waiter$hide())
      
    tic('prepare')
    prep <- disaggregation::prepare_data(polygon_shapefile = common$shape, 
                                        covariate_rasters = common$covs, 
                                        aggregation_raster = common$popn,
                                        id_var = as.character(input$id_var),
                                        response_var = as.character(input$resp_var),
                                        mesh.args = list(max.edge = input$mesh_edge, 
                                                         cut = input$mesh_cut, 
                                                         offset = input$mesh_offset),
                                        ncores = 8,
                                        na.action = input$na_action,
                                        makeMesh=input$mesh_make)
    toc()
    common$prep <- prep 
    })
    
    
    output$cov_summary <- renderTable({
      req(common$prep)
      as.data.frame(summary(common$prep$covariate_rasters))
    },rownames = T)
    
    output$mesh_plot <- renderPlot({
      req(common$prep)
      plot(common$prep[[3]])
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

  server <- function(input, output, session) {
    input_data_rds <- readRDS('data/input_data.Rds')

    common <- reactiveValues(shape = input_data_rds$shape,
                             popn = input_data_rds$popn,
                             covs = input_data_rds$cov,
                             prep = NULL,
                             fit = NULL,
                             pred = NULL)

    callModule(prepare_module_server, "prep", common)

  }
  shinyApp(ui, server)
}

  prepareApp()