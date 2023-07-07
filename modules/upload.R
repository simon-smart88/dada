
upload_module_ui <- function(id) {
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
    plotOutput(NS(id,"cov_plot"))

    # leafletOutput(NS(id,"map_plot"))
    )
}

upload_module_server <- function(input, output, session, common) {
    
# https://www.paulamoraga.com/book-geospatial/sec-shinyexample.html#uploading-data
    
    observeEvent(input$shape,{
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
    common$shape <- subset(shape, ID_2 > 10101950)
    }
    })
   
    mask_and_crop <- function(ras,map){
      ras <- mask(ras,map)
      ras <- crop(ras,extent(map))
      ras
    }
    
    output$incid_plot <- renderPlot({
      req(common$shape)
      spplot(common$shape, 'inc', main = 'Incidence of malaria in Madagascar')
    })
    
    
    observeEvent(input$popn,{
      tic('load population')
      population_raster <- raster(input$popn$datapath)
      population_raster <- mask_and_crop(population_raster,common$shape[,1])
      toc()
      common$popn <- population_raster
    })
    
    output$popn_plot <- renderPlot({
      req(common$popn)
      plot(log10(common$popn),main='Population density of Madagascar')
    })
    
    observeEvent(input$cov,{
    tic('load covariates')
      cov_directory <- dirname(input$cov$datapath[1])
      covariate_stack <- disaggregation::getCovariateRasters(cov_directory, shape = common$popn)
      names(covariate_stack) <- input$cov$name
      covariate_stack <- mask_and_crop(covariate_stack,common$shape[,1])
      toc()
      common$covs <- covariate_stack
    })

    output$cov_plot <- renderPlot({
      req(common$covs)
      plot(common$covs)
    })

}

uploadApp <- function() {
  ui <- fluidPage(
    upload_module_ui("upload")
  )

  server <- function(input, output, session) {
    common <- reactiveValues(shape = NULL,
                             popn = NULL,
                             covs = NULL,
                             prep = NULL,
                             fit = NULL,
                             pred = NULL)

    callModule(upload_module_server, "upload", common)
  }
  shinyApp(ui, server)
}

uploadApp()
