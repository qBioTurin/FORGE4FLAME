Appui <- "../app/ui.R"
Appserver <- "../app/server.R"

source(Appui)
source(Appserver)

app <-shinyApp(ui, server,
               options =  options(shiny.maxRequestSize=1000*1024^2,
                                  shiny.launch.browser = .rs.invokeShinyWindowExternal)
)

app$staticPaths <- list(
  `/` = httpuv::staticPath("www", indexhtml = FALSE, fallthrough = TRUE)
)


shiny::runApp(app, host = '0.0.0.0', port = 3838)
