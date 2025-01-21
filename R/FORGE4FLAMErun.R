#' @title Run FORGE4FLAME
#' @description Function to lunch the FORGE4FLAME shiny application.
#'
#' @author Pernice Simone, Baccega Daniele, Terrone Irene, Frattarola Marco.
#' @import shinydashboard
#' @import shinyjs
#' @import jsonlite
#' @import dplyr
#' @import shinythemes
#' @import colourpicker
#' @import glue
#' @import readr
#' @import zip
#' @import EBImageExtra
#' @import sortable
#' @import shinyalert
#' @import shinybusy
#' @import shinyBS
#' @import stringr
#' @import ggplot2
#' @import tidyr
#' @import DT
#' @import shiny
#' @import shinyWidgets
#' @import htmltools
#'
#' @importFrom utils read.table write.table
#'
#' @examples
#'\dontrun{
#' FORGE4FLAME.run()
#' }
#' @export

FORGE4FLAME.run <-function(FromDocker = F)
{

  Appui <- system.file("Shiny","ui.R", package = "FORGE4FLAME")
  Appserver <- system.file("Shiny","server.R", package = "FORGE4FLAME")

  source(Appui)
  source(Appserver)


  if(FromDocker){
          app <-shinyApp(ui, server,
                 options =  options(shiny.maxRequestSize=1000*1024^2)
                    )
       app$staticPaths <- list(
        `/` = httpuv::staticPath(system.file("Shiny","www", package = "FORGE4FLAME"), indexhtml = FALSE, fallthrough = TRUE)
      )
    shiny::runApp(app, host = '0.0.0.0', port = 3838)
  }else{
      app <-shinyApp(ui, server,
                 options =  options(shiny.maxRequestSize=1000*1024^2,
                                    shiny.launch.browser = .rs.invokeShinyWindowExternal)
                    )

      app$staticPaths <- list(
        `/` = httpuv::staticPath(system.file("Shiny","www", package = "FORGE4FLAME"), indexhtml = FALSE, fallthrough = TRUE)
      )

    shiny::runApp(app)
  }

  # runApp(
  #   appDir = system.file("Shiny", package = "FORGE4FLAME"),
  #   launch.browser = T
  # )
}
