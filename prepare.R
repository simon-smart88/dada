
prepareUI <- function(id) {
  tagList( 
    plotOutput(NS(id,"incid_plot")),
    plotOutput(NS(id,"popn_plot")),
    plotOutput(NS(id,"cov_plot"))
    )
}

prepareServer <- function(id,data) {
  moduleServer(id,function(input,output,session){
    
      tic('load incidence')
      shapes <- shapefile('data/shapes/mdg_shapes.shp')
      shapes@data$ID_2 <- as.numeric(shapes@data$ID_2)
      data$incid <- subset(shapes, ID_2 > 10101950)
      toc()
    
    mask_and_crop <- function(ras,map){
      ras <- mask(ras,map)
      ras <- crop(ras,extent(map))
      ras
    }
    
    output$incid_plot <- renderPlot({
      spplot(data$incid, 'inc', main = 'Incidence of malaria in Madagascar')
      
    })
    
      tic('load population')
      population_raster <- raster('data/population_lo_res_3.tif')
      data$popn <- mask_and_crop(population_raster,data$incid[,1])
      toc()
    
    output$popn_plot <- renderPlot({
      plot(log10(data$popn),main='Population density of Madagascar')
    })
    
    tic('load covariates')
      covariate_stack <- disaggregation::getCovariateRasters('data/lores covariates', shape = data$popn)
      data$cov <- mask_and_crop(covariate_stack,data$incid[,1])
      toc()
    
    output$cov_plot <- renderPlot({
      plot(data$cov)
    })
    
    
  })
    
}

prepareApp <- function() {
  ui <- fluidPage(
    prepareUI("prep")
  )
  
  data <- R6Class("data", list())
  
  server <- function(input, output, session) {
    prepareServer("prep",data)
  }
  shinyApp(ui, server)  
}

prepareApp()
