
uploadUI <- function(id) {
  tagList( 
    fileInput(inputId = NS(id,"shape"),
              label = "Upload all shapefile data",
              multiple = TRUE,
              accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj')),
    fileInput(inputId = NS(id,"popn"),
              label = "Upload aggregation data",
              multiple = F,
              accept = c('.tif')),
    fileInput(inputId = NS(id,"cov"),
              label = "Upload covariate data",
              multiple = TRUE,
              accept = c('.tif')),
    plotOutput(NS(id,"incid_plot")),
    plotOutput(NS(id,"popn_plot")),
    plotOutput(NS(id,"cov_plot"))#,
    # leafletOutput(NS(id,"map_plot"))
    )
}

uploadServer <- function(id) {
  moduleServer(id,function(input,output,session){
    

    
# https://www.paulamoraga.com/book-geospatial/sec-shinyexample.html#uploading-data
   map <- reactive({
      req(input$shape)
      shpdf <- input$shape
      validate(need(nrow(shpdf) == 4,"Please upload all 4 shapefiles"))
      tic('read shapefile')

      tempdirname <- dirname(shpdf$datapath[1])
      
      for (i in 1:nrow(shpdf)) {
        file.rename(
          shpdf$datapath[i],
          paste0(tempdirname, "/", shpdf$name[i])
        )
      }
    if (nrow(shpdf) == 4){  
    shape <- shapefile(paste(tempdirname,shpdf$name[grep(pattern = "*.shp$", shpdf$name)],sep = "/"))
    shape@data$ID_2 <- as.numeric(shape@data$ID_2)
    shape <- subset(shape, ID_2 > 10101950)
    } else {shape <- NULL}
    toc()
    shape
    })
   
    output$map_plot <- renderLeaflet({
      # bob <- 4
      #validate(need(map()[2] == 4),"Please upload all 4 shapefiles")
      
      pal <- colorBin("YlOrRd", domain = map()$inc, bins = 7)
      
      #leaflet(data$incid) %>% addTiles() %>%  addPolygons()
      leaflet(map()) %>% addTiles() %>%  addPolygons()
    })
    # output$map_plot <- renderPlot({
    #   plot(map(), 'inc', main = 'Incidence of malaria in Madagascar')
    #   
    # })
    
      # tic('load incidence')
      # shapes <- shapefile('data/shapes/mdg_shapes.shp')
      # shapes@data$ID_2 <- as.numeric(shapes@data$ID_2)
      # data$incid <- subset(shapes, ID_2 > 10101950)
      # toc()
    
    mask_and_crop <- function(ras,map){
      ras <- mask(ras,map)
      ras <- crop(ras,extent(map))
      ras
    }
    
    output$incid_plot <- renderPlot({
      req(input$shape)
      req(map())
      #spplot(data$incid, 'inc', main = 'Incidence of malaria in Madagascar')
      spplot(map(), 'inc', main = 'Incidence of malaria in Madagascar')
    })
    
    
    popn <- reactive({
      req(input$popn)
      tic('load population')
      population_raster <- raster(input$popn$datapath)
      population_raster <- mask_and_crop(population_raster,map()[,1])
      toc()
      population_raster
    })
    
    output$popn_plot <- renderPlot({
      req(input$popn)
      plot(log10(popn()),main='Population density of Madagascar')
    })
    
    cov <- reactive({
      req(input$cov)
    tic('load covariates')
      cov_directory <- dirname(input$cov$datapath[1])
      covariate_stack <- disaggregation::getCovariateRasters(cov_directory, shape = popn())
      names(covariate_stack) <- input$cov$name
      covariate_stack <- mask_and_crop(covariate_stack,map()[,1])
      toc()
      covariate_stack
    })

    output$cov_plot <- renderPlot({
      req(input$cov)
      plot(cov())
    })

    return(
      list(
        shape = reactive(map()),
        popn = reactive(popn()),
        cov = reactive(cov())
      )
    )

  })
  
}

uploadApp <- function() {
  ui <- fluidPage(
    uploadUI("upload")
  )
  
  #data <- R6Class("data", list())
  
  server <- function(input, output, session) {
    uploadServer("upload")
  }
  shinyApp(ui, server)  
}

uploadApp()
