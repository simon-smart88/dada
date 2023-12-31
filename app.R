library(leaflet)
library(leaflet.extras)
library(rgeos)
library(shiny)
library(disaggregation)
library(raster)
library(ggplot2)
library(dplyr)
library(INLA)
library(tictoc)
library(shinyjs)
library(R6)
library(gargoyle)

rm(list = ls())

MB <- 1024^2
UPLOAD_SIZE_MB <- 5000
options(shiny.maxRequestSize = UPLOAD_SIZE_MB*MB)

source('modules/upload.R')
source('modules/prepare.R')
source('modules/model.R')
source('helper_functions.R')

shiny::addResourcePath("resources", 'www/')

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
   tabsetPanel(
     tabPanel('Upload',
              sidebarLayout(sidebarPanel(upload_module_ui("upload")[1:5]),
                mainPanel(fluidRow(upload_module_ui("upload")[6:8])
              ))),
     tabPanel('Prepare',
              sidebarLayout(sidebarPanel(prepare_module_ui("prepare")[1:9]),
                            mainPanel(fluidRow(prepare_module_ui("prepare")[10:11])
                            ))),
     tabPanel('Model',
              sidebarLayout(sidebarPanel(model_module_ui("model")[1:6]),
                            mainPanel(fluidRow(model_module_ui("model")[7:9])
                            ))),
     tabPanel('Save / load',
              sidebarLayout(sidebarPanel(downloadButton("save_session", "Save session"),
              fileInput("load_session", "Load session", accept = ".rds"),
              downloadButton('dlRMD', 'Download Session Code')),
              mainPanel(verbatimTextOutput("common")
              )))
)
)

server <- function(input, output) {
  
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
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(0, 0, zoom = 2) %>%
      addProviderTiles('Esri.WorldTopoMap') %>%
      leafem::addMouseCoordinates()
  })
  
  # create map proxy to make further changes to existing map
  map <- leafletProxy("map")
  
  observeEvent(input$map_draw_new_feature, {
    coords <- unlist(input$map_draw_new_feature$geometry$coordinates)
    xy <- matrix(c(coords[c(TRUE,FALSE)], coords[c(FALSE,TRUE)]), ncol=2)
    colnames(xy) <- c('longitude', 'latitude')
    common$poly <- xy
    trigger("change_poly")
  })
  
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
      meta = NULL,
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

 init("change_shape","change_popn","change_covs","change_poly","change_prep","change_fit","change_pred")
 
  callModule(upload_module_server, "upload", common, map)
  callModule(prepare_module_server, "prepare", common, map)
  callModule(model_module_server, "model", common, map)

  # Save the current session to a file
  save_session <- function(file) {
    saveRDS(common, file)
  }
  
  output$save_session <- downloadHandler(
    filename = function() {
      paste0("dada-session-", Sys.Date(), ".rds")
    },
    content = function(file) {
      save_session(file)
    }
  )
  
    observeEvent(input$load_session, {
      temp <- readRDS(input$load_session$datapath)
      temp_names <- names(temp)
      #exclude the non-public(?) and function objects
      temp_names  <- temp_names[!temp_names %in% c('clone',".__enclos_env__","add_map_layer","logger")]
      for (name in temp_names){
      common[[name]] <- temp[[name]]
      }
      common$logger %>% writeLog(type='info','The previous session has been loaded successfully')
    trigger("change_shape","change_popn","change_covs","change_poly","change_prep","change_fit","change_pred")
   })
  
  output$common <- renderPrint({
    watch("change_shape")
    print(common)})
  
  output$dlRMD <- downloadHandler(
    filename = function() {
      paste0("dada-session-", Sys.Date(), ".Rmd")
    },
    
    content = function(file) {
      md_files <- c()
      md_intro_file <- tempfile(pattern = "intro_", fileext = ".md")
      rmarkdown::render("Rmd/userReport_intro.Rmd",
                        output_format = rmarkdown::github_document(html_preview = FALSE),
                        output_file = md_intro_file,
                        clean = TRUE,
                        encoding = "UTF-8")
      md_files <- c(md_files, md_intro_file)
      
      module_rmds <- NULL
      for (m in c('upload','prepare','model')){
      rmd_vars <- do.call(glue('{m}_module_rmd'), list(common))
      knit_params <- c(file = glue('modules/{m}.Rmd'), rmd_vars)
      
      module_rmd <- do.call(knitr::knit_expand, knit_params)
      module_rmd_file <- tempfile(pattern = paste0("upload", "_"),
                                  fileext = ".Rmd")
      writeLines(module_rmd, module_rmd_file)
      
      module_rmds <- c(module_rmds,module_rmd_file)
      }
      module_md_file <- tempfile(pattern = paste0('upload', "_"),
                                 fileext = ".md")
      rmarkdown::render(input = "Rmd/userReport_module.Rmd",
                        params = list(child_rmds = module_rmds),
                        output_format = rmarkdown::github_document(html_preview = FALSE),
                        output_file = module_md_file,
                        clean = TRUE,
                        encoding = "UTF-8")
      
      md_files <- c(md_files,module_md_file)
      
      combined_md <-
        md_files %>%
        lapply(readLines) %>%
        # lapply(readLines, encoding = "UTF-8") %>%
        lapply(paste, collapse = "\n") %>%
        paste(collapse = "\n\n")
      
      result_file <- 'test.Rmd'
      combined_rmd <- gsub('``` r', '```{r}', combined_md)
      writeLines(combined_rmd, result_file, useBytes = TRUE)
      
      file.rename(result_file, file)
      
    } 
  )

}

shinyApp(ui = ui, server = server)

