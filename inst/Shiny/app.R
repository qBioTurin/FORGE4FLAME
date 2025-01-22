# library(shiny)
#
# source("ui.R")
# source("server.R")
#
# shinyApp(ui, server)

Appui <- system.file("Shiny","ui.R", package = "FORGE4FLAME")
Appserver <- system.file("Shiny","server.R", package = "FORGE4FLAME")

shinyApp(ui, server,
         options =  options(shiny.maxRequestSize=1000*1024^2,
                            shiny.launch.browser = .rs.invokeShinyWindowExternal)
)
