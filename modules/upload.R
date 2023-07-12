
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
    checkboxInput(NS(id,"edit"),'Edit data?',FALSE),
    actionButton(NS(id,"crop"), "Crop data",style='background-color: #89eda0; color:#000;'),
    tableOutput(NS(id,"test")),
    plotOutput(NS(id,"incid_plot")),
    plotOutput(NS(id,"popn_plot")),
    plotOutput(NS(id,"cov_plot"))

    )
}

upload_module_server <- function(input, output, session, common, map) {
  
  mask_and_crop <- function(target,template){
    target <- mask(target,template)
    target <- crop(target,extent(template))
    target
  }
  
  #hide these until shapefile has been uploaded
  hide('popn')
  hide('cov')
  hide('edit')
  hide('crop')

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
    # shape@data$ID_2 <- as.numeric(shape@data$ID_2)
    # shape <- subset(shape, ID_2 > 10101950)
    common$shape <- shape
    common$map_layers <- c('Incidence') #need this here so the layer only gets added once
    }
      show('popn')
    })
   
  # observeEvent(input$shape, {
  #   ex <- extent(common$shape)
  #   common$map_layers <- c('Incidence')
  #   map %>%
  #     addPolygons(data=common$shape,fillColor = ~ pal(as.numeric(common$shape$inc)),color='black',fillOpacity = 0.7,weight=3, group="Incidence") %>%
  #     fitBounds(lng1=ex@xmin,lng2=ex@xmax,lat1=ex@ymin,lat2=ex@ymax) %>%
  #     addLegend(position ="bottomright",pal = pal, values = as.numeric(shapes$inc), group="Incidence", title="Incidence") %>%
  #     addLayersControl(overlayGroups = common$map_layers,
  #       options = layersControlOptions(collapsed = FALSE)
  #     )
  #   
  # })
  
    observeEvent(common$shape, {
      ex <- extent(common$shape)
      map %>%
        clearGroup("Incidence") %>%
        addPolygons(data=common$shape,fillColor = ~ pal(as.numeric(common$shape$inc)),color='black',fillOpacity = 0.7,weight=3, group="Incidence") %>%
        fitBounds(lng1=ex@xmin,lng2=ex@xmax,lat1=ex@ymin,lat2=ex@ymax) %>%
        addLegend(position ="bottomright",pal = pal, values = as.numeric(shapes$inc), group="Incidence", title="Incidence") %>%
        addLayersControl(overlayGroups = common$map_layers,
          options = layersControlOptions(collapsed = FALSE)
        )
  })
  
      output$incid_plot <- renderPlot({
      req(common$shape)
      spplot(common$shape, 'inc', main = 'Incidence of malaria in Madagascar')
    })
    
    
    observeEvent(input$popn,{
      tic('load population')
      population_raster <- raster(input$popn$datapath)
      # population_raster <- mask_and_crop(population_raster,common$shape[,1])
      toc()
      common$popn <- population_raster
      show('cov')
      common$map_layers <- c(common$map_layers,'Population density (log 10)') #need a way to name progamatically and which will overwrite if the file is reuploaded
    })
    
    observeEvent(common$popn, {
      pal <- colorBin("YlOrRd", domain = values(log10(common$popn)), bins = 9,na.color ="#00000000")
      map %>%
        clearGroup('Population density (log 10)') %>%
        addRasterImage(log10(common$popn),group='Population density (log 10)',colors = pal) %>%
        addLegend(position ="bottomleft",pal = pal, values = values(log10(common$popn)), group='Population density (log 10)', title='Population density (log10)') %>%
        addLayersControl(overlayGroups = common$map_layers,options = layersControlOptions(collapsed = FALSE)
        )
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
      # covariate_stack <- mask_and_crop(covariate_stack,common$shape[,1])
      toc()
      common$covs <- covariate_stack
      common$map_layers <- c(common$map_layers,names(common$covs))
      show('edit')
      show('crop')
    })

    observeEvent(common$covs, {
        for (s in 1:length(names(common$covs))){
          pal <- colorBin("YlOrRd", domain = values(common$covs[[s]]), bins = 9,na.color ="#00000000")
          map %>% 
            clearGroup(names(common$covs)[s]) %>%
            addRasterImage(common$covs[[s]],group=names(common$covs)[s],colors = pal) %>%
            addLegend(position="bottomleft",pal=pal,values=values(common$covs[[s]]),group=names(common$covs)[s],title=names(common$covs)[s])
        }
      map %>%
        hideGroup(common$map_layers[2:(length(common$map_layers)-1)]) %>%
        addLayersControl(overlayGroups = common$map_layers, options = layersControlOptions(collapsed = FALSE)
        )
    })
    
    output$cov_plot <- renderPlot({
      req(common$covs)
      plot(common$covs)
    })

    observeEvent(input$edit,{
      if (input$edit == T){
      map %>%
        addDrawToolbar(polylineOptions=F,circleOptions = F, rectangleOptions = T, markerOptions = F, circleMarkerOptions = F, singleFeature = T)
        #shinyjs::runjs("$('.leaflet-draw').add()")
      } 
      if (input$edit == F){
        map %>% removeDrawToolbar()
        #shinyjs::runjs("$('.leaflet-draw').remove()")
      }
      }) 
  
    observeEvent(input$crop,{
      poly <- SpatialPolygons(list(Polygons(list(Polygon(common$xy)),1)))
      common$shape <-shapes[which(gContains(poly,common$shape, byid=TRUE)),]
      common$popn <- mask_and_crop(common$popn,common$shape)
      common$covs <- mask_and_crop(common$covs,common$shape)
    })
    
}

uploadApp <- function() {
  ui <- fluidPage(
    shinyjs::useShinyjs(),
    leafletOutput("map"),
    textOutput("test"),
    upload_module_ui("upload")
  )

  server <- function(input, output, session) {
    common <- reactiveValues()
    
    observeEvent(input$map_draw_new_feature, {
      coords <- unlist(input$map_draw_new_feature$geometry$coordinates)
      xy <- matrix(c(coords[c(TRUE,FALSE)], coords[c(FALSE,TRUE)]), ncol=2)
      colnames(xy) <- c('longitude', 'latitude')
      common$xy <- xy
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
    
    callModule(upload_module_server, "upload", common, map)
  }
  shinyApp(ui, server)
}

uploadApp()
