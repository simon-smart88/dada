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
    checkboxInput(NS(id,"na_action"), "Handle NAs?", value=F),
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

    observeEvent(input$prepare,{
    mspdf <- inla.mesh2sp(common$prep$mesh)
    ex <- extent(mspdf$triangles)
    
    common$map_layers <- c(common$map_layers,'INLA mesh')
    map %>%
      addPolylines(data=mspdf$triangles,group='INLA mesh',weight=,color='black') %>%
      fitBounds(lng1=ex@xmin,lng2=ex@xmax,lat1=ex@ymin,lat2=ex@ymax) %>%
      addLayersControl(overlayGroups = common$map_layers,
                       options = layersControlOptions(collapsed = FALSE)
      )
    
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
    leafletOutput("uploadmap"),
    prepare_module_ui("prep")
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
    common <- reactiveValues(shape = input_data_rds$shape,
                             popn = input_data_rds$popn,
                             covs = input_data_rds$cov,
                             map_layers = NULL)

    callModule(prepare_module_server, "prep", common, map)

  }
  shinyApp(ui, server)
}

  prepareApp()