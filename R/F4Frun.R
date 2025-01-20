#' @title Run F4F
#' @description function to lunch the F4F shiny application.
#'
#' @param 
#'
#' @author Pernice Simone, Baccega Daniele, Terrone Irene, Frattarola Marco.
#' @import shinydashboard, shinyjs, jsonlite, dplyr, shinythemes, colourpicker, glue, readr, zip, EBImageExtra, sortable, shinyalert, shinybusy, shinyBS, stringr, ggplot2, tidyr, gifski
#' @rawNamespace import(DT, except=c(dataTableOutput,renderDataTable))
#' @rawNamespace import(shiny,except=runExample)
#' @rawNamespace import(shinyWidgets,except=alert)
#' 
#' @examples
#'\dontrun{
#' F4F.run()
#' }
#' @export

F4F.run <-function(FromDocker = F)
{
  
  Appui <- system.file("Shiny","ui.R", package = "F4F")
  Appserver <- system.file("Shiny","server.R", package = "F4F")
  
  source(Appui)
  source(Appserver)
  
  
  if(FromDocker){
          app <-shinyApp(ui, server,
                 options =  options(shiny.maxRequestSize=1000*1024^2)
                    )
       app$staticPaths <- list(
        `/` = httpuv::staticPath(system.file("Shiny","www", package = "F4F"), indexhtml = FALSE, fallthrough = TRUE)
      )
    shiny::runApp(app, host = '0.0.0.0', port = 3838)
  }else{
      app <-shinyApp(ui, server,
                 options =  options(shiny.maxRequestSize=1000*1024^2,
                                    shiny.launch.browser = .rs.invokeShinyWindowExternal)
                    )
  
      app$staticPaths <- list(
        `/` = httpuv::staticPath(system.file("Shiny","www", package = "F4F"), indexhtml = FALSE, fallthrough = TRUE)
      )
    
    shiny::runApp(app)
  }

  # runApp(
  #   appDir = system.file("Shiny", package = "F4F"),
  #   launch.browser = T
  # )
}
