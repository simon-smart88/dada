#https://groups.google.com/g/r-inla-discussion-group/c/z1n1exlZrKM
inla.mesh2sp <- function(mesh) {
  crs <- inla.CRS(inla.CRSargs(mesh$crs))
  isgeocentric <- identical(inla.as.list.CRS(crs)[["proj"]], "geocent")
  if (isgeocentric || (mesh$manifold == "S2")) {
    stop(paste0(
      "'sp' doesn't support storing polygons in geocentric coordinates.\n",
      "Convert to a map projection with inla.spTransform() before
calling inla.mesh2sp()."))
  }

  triangles <- SpatialPolygonsDataFrame(
    Sr = SpatialPolygons(lapply(
      1:nrow(mesh$graph$tv),
      function(x) {
        tv <- mesh$graph$tv[x, , drop = TRUE]
        Polygons(list(Polygon(mesh$loc[tv[c(1, 3, 2, 1)],
                                       1:2,
                                       drop = FALSE])),
                 ID = x)
      }
    ),
    proj4string = crs
    ),
    data = as.data.frame(mesh$graph$tv[, c(1, 3, 2), drop = FALSE]),
    match.ID = FALSE
  )
  vertices <- SpatialPoints(mesh$loc[, 1:2, drop = FALSE], proj4string = crs)
  
  list(triangles = triangles, vertices = vertices)
} 

prepare_module_ui<- function(id) {
  ns <- shiny::NS(id)
  tagList( 
    waiter::use_waiter(),
    uiOutput(ns("id_var_out")),
    uiOutput(ns("resp_var_out")),
    sliderInput(NS(id,"mesh_edge"),"Max edge",min=0,max=10,value=c(0.7,8),step = 0.1),
    sliderInput(NS(id,"mesh_cut"),"Cut",min=0,max=1,value=0.05,step = 0.01),
    sliderInput(NS(id,"mesh_offset"),"Offset",min=0,max=10,value=c(1,2),step = 0.1),
    checkboxInput(NS(id,"na_action"), "Handle NAs?", value=T),
    checkboxInput(NS(id,"mesh_make"), "Make mesh?", value=T),
    actionButton(NS(id,"prepare"), "Prepare data",style='background-color: #89eda0; color:#000;'),
    tableOutput(NS(id,"cov_summary")),
    plotOutput(NS(id,"mesh_plot"))
  )
}

prepare_module_server <- function(input, output, session, common, map) {
    
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
    
      # waiter <- waiter::Waiter$new()
      # waiter$show()
      # on.exit(waiter$hide())
    common$logger %>% writeLog(type='info', 'Data preparation has started - please be patient')  
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
    common$logger %>% writeLog('Data preparation is completed')  
    trigger("change_prep")
    
    common$meta$prep$id_var <- as.character(input$id_var)
    common$meta$prep$resp_var <- as.character(input$resp_var)
    common$meta$prep$mesh_edge <- input$mesh_edge
    common$meta$prep$mesh_cut <- input$mesh_cut
    common$meta$prep$mesh_offset <- input$mesh_offset
    common$meta$prep$na_action <- input$na_action
    common$meta$prep$make_mesh <- input$mesh_make
    })

    observeEvent(watch("change_prep"),{
    req(common$prep)
    mspdf <- inla.mesh2sp(common$prep$mesh)
    ex <- extent(mspdf$triangles)
    
    common$add_map_layer('INLA mesh')
    map %>%
      addPolylines(data=mspdf$triangles,group='INLA mesh',weight=1,color='black') %>%
      #addPolylines(data=mspdf$vertices,group='INLA mesh',weight=2,color='red') %>%
      fitBounds(lng1=ex@xmin,lng2=ex@xmax,lat1=ex@ymin,lat2=ex@ymax) %>%
      addLayersControl(overlayGroups = common$map_layers, options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(common$map_layers[2:(length(common$map_layers)-1)]) #hide all but first and last layers
    })
    
    output$cov_summary <- renderTable({
      req(common$prep)
      as.data.frame(summary(common$prep$covariate_rasters))
    },rownames = T)
    

}


prepare_module_rmd <- function(common){
  list(
    prepare_knit = !is.null(common$prep), 
    prepare_id_var = common$meta$prep$id_var, 
    prepare_resp_var = common$meta$prep$resp_var,
    prepare_mesh_edge = printVecAsis(common$meta$prep$mesh_edge),
    prepare_mesh_cut = common$meta$prep$mesh_cut,
    prepare_mesh_offset = printVecAsis(common$meta$prep$mesh_offset),
    prepare_na_action = common$meta$prep$na_action,
    prepare_make_mesh = common$meta$prep$make_mesh
  )
}



  prepareApp <- function() {
  ui <- fluidPage(
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(
      script = file.path("resources","js", "shinyjs-funcs.js"),
      functions = c("scrollLogger", "disableModule", "enableModule")
    ),
    #in Wallace this is inside "navbarpage > header ="
    tags$head(tags$link(href = "css/styles.css", rel = "stylesheet")),
    theme = bslib::bs_theme(version = 3,
                            bootswatch = "united"),
    titlePanel("Disaggregation Regression Demonstration Application"),
    fluidRow(
      column(
        4,
        offset = 1,
        align = "left",
        div(style = "margin-top: -10px"),
        strong("Log window"),
        div(style = "margin-top: 5px"),
        div(
          id = "wallaceLog",
          div(id = "logHeader", div(id = "logContent"))
        )
      ),
      column(8,leafletOutput("map"))),
    prepare_module_ui("prep")
  )

  server <- function(input, output, session) {
    
    source('helper_functions.R')
    
    # Variable to keep track of current log message
    initLogMsg <- function() {
      intro <- '***WELCOME TO DADA***'
      brk <- paste(rep('------', 14), collapse = '')
      expl <- 'Please find messages for the user in this log window.'
      logInit <- gsub('.{4}$', '', paste(intro, brk, expl, brk, '', sep = '<br>'))
      logInit
    }
    
    # Write out logs to the log Window
    observeEvent(common$logger(), {
      shinyjs::html(id = "logHeader", html = common$logger(), add = FALSE)
      shinyjs::js$scrollLogger()
    })
    
    # create map
    output$map <- renderLeaflet(
      leaflet() %>%
        setView(0, 0, zoom = 2) %>%
        addProviderTiles('Esri.WorldTopoMap') %>%
        leafem::addMouseCoordinates()
    )
    # create map proxy to make further changes to existing map
    map <- leafletProxy("map")
    
    input_data_rds <- readRDS('data/input_data.Rds')


    common_class <- R6::R6Class(
      classname = "common",
      public = list(
        shape = NULL,
        popn = NULL,
        covs = NULL,
        prep = NULL,
        fit = NULL,
        pred = NULL,
        map_layers = NULL,
        poly = NULL,
        logger = NULL,
        add_map_layer = function(new_names) {
          for (new_name in new_names){
            if (!(new_name %in% self$map_layers)){
              self$map_layers <- c(self$map_layers,new_name) 
              invisible(self)
            }
          }
        }
      )
    )
    
    common <- common_class$new()
    common$logger <- reactiveVal(initLogMsg())
    common$shape <- input_data_rds$shape
    common$popn <- input_data_rds$popn
    common$covs <- input_data_rds$cov
    
    init("change_shape")
    init("change_popn")
    init("change_covs")
    init("change_poly")
    init("change_prep")
    init("change_fit")
    init("change_pred")
    
    callModule(prepare_module_server, "prep", common, map)

  }
  shinyApp(ui, server)
}

  prepareApp()