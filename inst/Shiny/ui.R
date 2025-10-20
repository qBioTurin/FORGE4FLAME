library(shiny)
library(shinydashboard)
library(shinyjs)
library(jsonlite)
library(dplyr)
library(shinythemes)
library(colourpicker)
library(glue)
library(readr)
library(zip)
library(EBImageExtra)
library(sortable)
library(shinyalert)
library(shinybusy)
library(shinyBS)
library(stringr)
library(ggplot2)
library(tidyr)
library(htmltools)
library(DT)
library(shinyFiles)
library(bslib)
library(lubridate)
library(sf)
library(purrr)
library(shinyjqui)

source(system.file("Shiny","Rfunctions.R", package = "FORGE4FLAME"))


jsCode <- '
shinyjs.canvasDimension= function(params) {
  var defaultParams = {
    w : 1000,
    h : 800
  };
  params = shinyjs.getParams(params, defaultParams);

  w = mainCanvas.width = background.width = w_base = params.w;
  h = mainCanvas.height = background.height = h_base = params.h;
  drawBG(ctx)
};

// Function to redraw the canvas
shinyjs.clearCanvas = function() {
    FloorArray = {}
    mainCtx.clearRect(0, 0, w_base, h_base);
    mainCtx.fillStyle = \'white\';
    drawBG(ctx);
}
'

ui <- dashboardPage(
  # theme = bs_theme(
  #   version = 5,
  #   bootswatch = "minty",  # or "flatly", "litera", "yeti", etc.
  #   base_font = font_google("Roboto"),
  #   heading_font = font_google("Raleway"),
  #   bg = "#f8f9fa",   # Light gray background
  #   fg = "#343a40",   # Dark text
  #   primary = "#0d6efd", # Bootstrap blue
  #   border_radius = "lg",  # Rounded corners globally
  #   card_border_color = "#dee2e6",
  #   card_box_shadow = TRUE
  # ),
  dashboardHeader(
    title = "Build your ABM",
    tags$li(
      class = "dropdown d-flex align-items-center",
      tags$head(tags$link(rel = "shortcut icon", href = "F4Ficon.png")),
      tags$head(
        tags$link(href = "https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;500;600;700&family=Raleway:wght@300;400;600;700;800&display=swap", rel = "stylesheet")
      ),
      tags$style(HTML("
        @import url('https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;500;600;700&family=Raleway:wght@300;400;600;700;800&display=swap');

        /* Home page hero section */
        .home-hero {
          text-align: center;
          padding: 50px 20px;
          background: linear-gradient(135deg, rgba(95, 92, 163, 0.08) 0%, rgba(95, 92, 163, 0.03) 100%);
          border-radius: 20px;
          margin-bottom: 40px;
          box-shadow: 0 8px 24px rgba(0,0,0,0.06);
        }

        .home-hero img {
          animation: float 3s ease-in-out infinite;
          filter: drop-shadow(0 8px 16px rgba(95, 92, 163, 0.3));
        }

        @keyframes float {
          0%, 100% { transform: translateY(0px); }
          50% { transform: translateY(-10px); }
        }

        .home-hero h1 {
          font-family: 'Raleway', sans-serif !important;
          font-size: 48px;
          font-weight: 800;
          background: linear-gradient(135deg, #5F5CA3 0%, #7B6FB8 100%);
          -webkit-background-clip: text;
          -webkit-text-fill-color: transparent;
          background-clip: text;
          margin: 20px 0;
        }

        .home-hero .subtitle {
          font-family: 'Poppins', sans-serif;
          font-size: 20px;
          color: #5a5a5a;
          max-width: 700px;
          margin: 20px auto;
          line-height: 1.6;
        }

        .home-hero .highlight {
          color: #5F5CA3;
          font-weight: 600;
        }

        /* Overview section */
        .overview-section {
          text-align: center;
          padding: 30px;
          background: white;
          border-radius: 16px;
          box-shadow: 0 4px 16px rgba(0,0,0,0.06);
          margin-bottom: 40px;
        }

        .overview-section h3 {
          font-family: 'Raleway', sans-serif;
          color: #5F5CA3;
          font-weight: 700;
          font-size: 28px;
          margin-bottom: 15px;
        }

        .overview-section p {
          font-family: 'Poppins', sans-serif;
          font-size: 16px;
          color: #5a5a5a;
          max-width: 900px;
          margin: 0 auto;
          line-height: 1.8;
        }

        /* Feature cards */
        .f4f-card {
          border: none;
          border-radius: 16px;
          box-shadow: 0 8px 24px rgba(0,0,0,0.08);
          margin-bottom: 24px;
          overflow: hidden;
          padding: 24px;
          background: white;
          transition: all 0.3s ease;
          height: 100%;
        }

        .f4f-card:hover {
          transform: translateY(-6px);
          box-shadow: 0 12px 32px rgba(95, 92, 163, 0.2);
        }

        .f4f-card .card-header-custom {
          display: flex;
          align-items: center;
          margin-bottom: 16px;
        }

        .f4f-card .icon-badge {
          width: 56px;
          height: 56px;
          border-radius: 14px;
          display: flex;
          align-items: center;
          justify-content: center;
          margin-right: 16px;
          box-shadow: 0 4px 12px rgba(0,0,0,0.15);
          transition: all 0.3s ease;
        }

        .f4f-card:hover .icon-badge {
          transform: scale(1.1) rotate(5deg);
        }

        .f4f-card .icon-badge i {
          color: white;
          font-size: 28px;
        }

        .f4f-card h4 {
          font-family: 'Raleway', sans-serif;
          font-weight: 700;
          margin: 0;
          font-size: 20px;
          text-transform: uppercase;
          letter-spacing: 1.2px;
        }

        .f4f-card p {
          font-family: 'Poppins', sans-serif;
          color: #5a5a5a;
          font-size: 14px;
          line-height: 1.7;
          margin-bottom: 16px;
        }

        .f4f-card img {
          border-radius: 12px;
          margin-top: 12px;
          transition: all 0.3s ease;
          box-shadow: 0 4px 12px rgba(0,0,0,0.1);
          width: 100%;
        }

        .f4f-card:hover img {
          transform: scale(1.02);
        }
      ")),
      tags$style(HTML(
        ".main-header {max-height: 60px;}
        .box.box-solid.box-primary>.box-header {
          color:#fff;
          background:#5F5CA3
        }
        .box.box-solid.box-primary{
            border-bottom-color:#666666;
            border-left-color:#666666;
            border-right-color:#666666;
            border-top-color:#666666;
        }
                 .icon-container {
                    position: relative;
                    display: inline-block;
                  }
                  .icon-container .icon-text {
                    visibility: hidden;
                    width:300px;
                    background-color: #333;
                    color: #fff;
                    text-align: center;
                    border-radius: 6px;
                    padding: 5px;
                    position: absolute;
                    z-index: 2;
                    top: 50%;
                    left: 110%;
                    transform: translateY(-50%);
                    opacity: 0;
                    transition: opacity 0.3s;
                    font-weight: normal;
                  }
                  .icon-container:hover .icon-text {
                    visibility: visible;
                    opacity: 1;
                  }
                  h3, h5 {
                    padding-top: 0px; /* Adjust the top padding */
                    padding-bottom: 0px; /* Adjust the bottom padding */
                    margin-top: 0px; /* Adjust the top margin */
                    margin-bottom: 0px; /* Adjust the bottom margin */
                  }
                 .home {
                    margin-top: 0.5cm;
                    margin-bottom: 0.5cm;
                 }
                .shiny-notification {
                   font-size: 25px !important;
                   position: fixed;
                   bottom: 20px;
                   right: 20px;
                   top: auto !important;
                   left: auto !important;
                }"
      )
      ),
      tags$a(
        href = "#",
        style = "padding-top: 3px; padding-bottom: 3px;",
        tags$img(src = "F4Ficon.png",
                 height = "44px")
      )
    )
  ),
  dashboardSidebar(
    sidebarMenu(id = "SideTabs",
                menuItem("Home", tabName = "info", icon = icon("home")),
                menuItem(
                  "Canvas",
                  tabName = "canvas_tab",
                  icon = icon("ruler-combined")
                ),
                menuItem("Rooms", tabName = "rooms", icon = icon("bed")),
                menuItem("Agents", tabName = "agents", icon = icon("user")),
                menuItem("Resources", tabName = "resources", icon = icon("chart-simple")),
                menuItem("Infection", tabName = "infection", icon = icon("viruses")),
                menuItem("What-If", tabName = "whatif", icon = icon("question")),
                #menuItem("Advanced", tabName = "advanced", icon = icon("code")),
                menuItem("Configuration", tabName = "configuration", icon = icon("flag-checkered")),
                menuItem("Run", tabName = "run", icon = icon("play")),
                menuItem("Settings", tabName = "settings", icon = icon("cogs")),
                menuItem("Post Processing", tabName = "post_process", icon = icon("file-video"))
    )
  ),
  dashboardBody(
    add_busy_bar(color = "blue", height = "8px"),
    useShinyjs(),
    extendShinyjs(
      text = jsCode,
      functions = c("canvasDimension", "clearCanvas")
    ),
    tabItems(
      ## Tab HOME ####
      tabItem(
        tabName = "info",
        fluidPage(
          fluidRow(
            column(
              width = 12,
              div(style = "text-align:center;",
                  img(src = "F4Ficon.png", height = 100, width = 100),
                  h1(strong("Forge4Flame (F4F)")),
                  h2("A user-friendly R Shiny dashboard for defining FLAME GPU 2 ABM environments.")
              )
            )
          ),
          br(),
          fluidRow(
            column(
              width = 12,
              h3("F4F allows you to define various elements of an agent-based model simulation: environments, agents, disease dynamics, and more. Below are the core components of the platform, showcased with screenshots:")
            )
          ),
          br(),
          div(
            style = "
              display: grid;
              grid-template-columns: repeat(auto-fit, minmax(450px, 1fr));
              gap: 20px;
              margin: 0;
              padding: 0;",
            lapply(
              list(
                list(title = "Canvas", desc = "Define the model's environment using a drag-and-drop interface.", img = "Canvas.png", tab = "canvas_tab", icon = "ruler-combined", color = "#3498db"),
                list(title = "Rooms", desc = "Define new room types.", img = "Rooms.png", tab = "rooms", icon = "bed", color = "#e74c3c"),
                list(title = "Agents", desc = "Define agent types and their movement logic.", img = "Agents.png", tab = "agents", icon = "user", color = "#2ecc71"),
                list(title = "Resources", desc = "Specify room capacity per agent type.", img = "Resources.png", tab = "resources", icon = "chart-simple", color = "#f39c12"),
                list(title = "Infection", desc = "Define the disease model used in simulation.", img = "Infection.png", tab = "infection", icon = "viruses", color = "#9b59b6"),
                list(title = "What-If", desc = "Perform what-if analysis with countermeasures.", img = "Countermeasures.png", tab = "whatif", icon = "question", color = "#1abc9c"),
                list(title = "Configuration", desc = "Set up the initial configuration for simulation.", img = "Configuration.png", tab = "configuration", icon = "flag-checkered", color = "#34495e"),
                list(title = "Run", desc = "Execute the simulation with defined parameters.", img = "Run.png", tab = "run", icon = "play", color = "#e67e22"),
                list(title = "Settings", desc = "Adjust canvas size, load, and save models.", img = "Settings.png", tab = "settings", icon = "cogs", color = "#95a5a6"),
                list(title = "Post Processing", desc = "Analyze simulation outputs with 2D visualization.", img = "2DVisualisation.png", tab = "post_process", icon = "file-video", color = "#c0392b")
              ),
              function(card) {
                div(
                  style = paste0("
                    border-left: 4px solid ", card$color, ";
                    background: linear-gradient(135deg, #ffffff 0%, #f8f9fa 100%);
                    border-radius: 8px;
                    padding: 20px;
                    box-shadow: 0 2px 8px rgba(0,0,0,0.1);
                    transition: all 0.3s ease;
                    cursor: pointer;
                    display: flex;
                    flex-direction: column;
                    min-height: 420px;"),
                  onmouseover = "this.style.boxShadow='0 4px 16px rgba(0,0,0,0.15)'; this.style.transform='translateY(-2px)';",
                  onmouseout = "this.style.boxShadow='0 2px 8px rgba(0,0,0,0.1)'; this.style.transform='translateY(0)';",
                  onclick = paste0("Shiny.setInputValue('link_", card$tab, "', Math.random());"),
                  div(
                    style = "display: flex; align-items: center; margin-bottom: 12px;",
                    icon(card$icon, style = paste0("font-size: 28px; color: ", card$color, "; margin-right: 12px;")),
                    h4(card$title, style = paste0("margin: 0; color: ", card$color, "; font-weight: 600; font-size: 22px;"))
                  ),
                  div(
                    style = "margin-bottom: 15px;",
                    p(card$desc, style = "margin: 0; color: #6c757d; font-size: 15px; line-height: 1.5;")
                  ),
                  div(
                    style = "flex: 1; display: flex; align-items: center; justify-content: center; margin-top: auto;",
                    actionLink(
                      inputId = paste0("link_", card$tab),
                      label = img(
                        src = card$img,
                        style = "width: 100%; max-width: 100%; height: auto; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.1);"
                      ),
                      style = "width: 100%;"
                    )
                  )
                )
              }
            )
          )
        )
      ),
      ## Canvas HOME ####
      tabItem(
        tabName = "canvas_tab",
        fluidRow(
          box(
            title = h3("Define floor"),
            width = 12,
            collapsible = T,
            fluidRow(
              column(5, offset = 1,
                     selectizeInput(
                       inputId = "canvas_selector",
                       label = "Define/Select floor",
                       options = list(create = TRUE, placeholder ="Define or select floor"),
                       choices = c(""),  # Add more choices as needed
                       selected = ""
                     )
              ),
              column(1,
                     actionButton("delete_floor", "Delete floor", style="margin-top:25px;")),
              column(5,
                     uiOutput("FloorRank")
              )
            ),
            fluidRow(
              box(width = 10, title = h3("Floor background image"),
                  collapsible = T, collapsed = T,
                  column(
                    8,
                    offset = 1,
                    fileInput(
                      inputId = "BGfile",
                      label = "Background Image",
                      placeholder = "Select an dxf file.",
                      width = "100%",
                      accept = "dxf",
                      multiple = F
                    )
                  ),
                  column(
                    1,
                    style = "margin-top: 20px;",
                    actionButton(
                      label = "Load",
                      icon = shiny::icon("upload"),
                      inputId = "LoadBG_image"
                    )
                  ),
                  column(
                    1,
                    style = "margin-top: 20px;",
                    checkboxInput(
                      label = "Hide",
                      inputId = "HideBG_image"
                    )
                  )
              )
            )
          )
        ),
        fluidRow(
          box(
            title = h3("Add elements"),
            width = 12,
            collapsible = T,collapsed = T,
            fluidRow(
              column(5, offset = 1,
                     box(
                       title = "Draw rooms",
                       width = 12,
                       collapsible = T,
                       selectizeInput(inputId= "select_room",label="Select the room:", choices = ""),
                       textOutput("length"),
                       textOutput("width"),
                       textOutput("height"),
                       fluidRow(
                         column(10,offset=1,
                                h2(),
                                selectInput(inputId = "door_new_room", label = "Door position:",
                                            choices = c("none","right","left","top","bottom"),selected = "Right")
                         ),
                       ),
                       fluidRow(
                         column(10,offset=1,
                                h2(),
                                selectizeInput(inputId="select_area",label = "Area:",options = list(create = TRUE),
                                               choices = c("None")#,
                                               #             "Senology",
                                               #             "Ophthalmology",
                                               #             "Surgery",
                                               #             "Urology",
                                               #             "Orthopaedics",
                                               #             "Analgesic Therapy",
                                               #             "Dermosurgery",
                                               #             "Radiology")
                                )
                         )
                       ),
                       fluidRow(
                         column(5,offset =0,
                                actionButton("add_room", "Add room")
                         )
                       ),
                       fluidRow(
                         column(9, offset =1,
                                uiOutput(outputId = "Text_SpaceAvailable")
                         )
                       )
                     )
              ),
              column(5,
                     fluidRow(
                       box(
                         title = "Remove rooms",
                         width = 12,
                         collapsible = T,
                         fluidRow(
                           column(8, offset = 1,
                                  selectizeInput(inputId= "select_RemoveRoom",label="Select the room to remove:", choices = "")
                           ),
                           column(1,
                                  actionButton("remove_room", "Remove room", style = 'margin-top:25px', width = 100)
                           )
                         ),
                         fluidRow(
                           column(1, offset=9,
                                  actionButton("clear_all", "Clear floor", width = 100),
                           )
                         )
                       )
                     ),
                     fluidRow(
                       box(
                         #actionbutton che invia il segnale add_point
                         title = "Agents management",
                         width = 12,
                         collapsible = T,
                         fluidRow(
                           column(4, offset = 1,
                                  actionButton("add_point", "Add graph point", width = 150)
                           ),
                           column(4,
                                  actionButton("remove_point", "Remove last graph point")
                           )
                         ),
                         fluidRow(
                           column(4, offset = 1,
                                  actionButton("path_generation", "Visualise graph", width = 150)
                           )
                         )
                       )
                     ),
                     fluidRow(
                       selectizeInput(
                         inputId = "select_fillColor",
                         label = "Colour fill by:",
                         choices = c("Room", "Type", "Area")
                       )
                     )
              )
            )
          )
        ),
        fluidRow(
          column(12,
                 # Include the Canvas.js script here
                 includeCSS(system.file("Shiny","www/dragANDdrop.css", package = "FORGE4FLAME")),
                 includeHTML(system.file("Shiny","www/dragANDdrop.html", package = "FORGE4FLAME")),
                 includeScript(system.file("Shiny","www/Canvas.js", package = "FORGE4FLAME"))
          )
        )
      ),
      ## Tab room ####
      tabItem(
        tabName = "rooms",
        fluidRow(
          box(
            title = h3("Define a new room"),
            width = 12,
            collapsible = T,
            fluidRow(),
            fluidRow(column(1),
                     column(
                       5,
                       textInput(
                         inputId = "id_new_room",
                         label = "Name:",
                         placeholder = "Define a room"
                       )
                     ),
                     column(
                       5,
                       selectizeInput(
                         inputId = "select_type",
                         label = "Type:",
                         options = list(create = TRUE),
                         selected = "",
                         choices = c("", "Normal", "Stair", "Spawnroom", "Fillingroom","Waitingroom")
                       )
                     )),
            fluidRow(column(1),
                     column(
                       5,
                       textInput(
                         inputId = "length_new_room",
                         label = div(class = "icon-container",
                                     h5(tags$b("Length (meter): "), icon("info-circle")),
                                     div(class = "icon-text", "Length refers to the wall with the door.")
                         ),
                         placeholder = "Room length"
                       )
                     ),
                     column(
                       5,
                       textInput(
                         inputId = "width_new_room",
                         label = div(class = "icon-container",
                                     h5(tags$b("Width (meter): "))),
                         placeholder = "Room width"
                       )
                     )),
            fluidRow(column(1),
                     column(
                       5,
                       textInput(
                         inputId = "height_new_room",
                         label = "Height (meter):",
                         placeholder = "Room height"
                       )
                     )),
            fluidRow(column(1),
                     column(
                       10,
                       actionButton("save_room", "Save room")
                     ))
          )
        ),
        fluidRow(
          box(
            title = h3("Set colours legend"),
            width = 12,
            collapsible = T,
            fluidRow(
              column(5, offset = 1,
                     selectInput(inputId = "selectInput_color_room",
                                 label = "Select room:",
                                 choices = ""
                     )
              ),
              column(5,
                     uiOutput("RoomColors")
              )
            ),
            fluidRow(
              column(5, offset = 1,
                     selectInput(inputId = "selectInput_color_type",
                                 label = "Select type:",
                                 choices = ""
                     )
              ),
              column(5,
                     uiOutput("TypeColors")
              )
            ),
            fluidRow(
              column(5, offset = 1,
                     selectInput(inputId = "selectInput_color_area",
                                 label = "Select area:",
                                 choices = ""
                     )
              ),
              column(5,
                     uiOutput("AreaColors")
              )
            )
          )
        )
      ),
      ## Tab resources ####
      tabItem(tabName = "resources",
              fluidRow(
                box(
                  title = h3("Set resources"),
                  width = 12,
                  collapsible = T,
                  fluidRow(
                    column(4,
                           textInput(inputId = "textInput_resources_global",
                                     label = "Select global resources for each room and agent type:", value = 0)
                    ),
                    column(4,
                           uiOutput("selectInput_alternative_resources_global")
                    ),
                    column(1,
                           actionButton("set_resources", "Set", style="margin-top:25px;")
                    )
                  ),
                  fluidRow(
                    column(4,
                           selectInput(inputId = "selectInput_resources_type",
                                       label = "Select type and area:",
                                       choices = "")
                    )
                  ),
                  fluidRow(
                    column(12,
                           DT::dataTableOutput("RoomAgentResTable")
                    )
                  ),
                  fluidRow(
                    column(10,offset = 1,
                           # conditionalPanel(
                           #   condition = "input.WhereWaitingButton == 'Waiting room' || input.WhereWaitingButton == 'Other room'",
                           uiOutput("dynamicSelectizeInputs_waitingRoomsDeter"),
                           uiOutput("dynamicSelectizeInputs_waitingRoomsRand")
                           # )
                    )
                  )
                )
              )
      ),
      ## Tab agents ####
      tabItem(
        tabName = "agents",
        fluidRow(
          box(
            width = 12,
            collapsible = T,
            title = h4("Agent definition ", icon("info-circle")),
            fluidRow(
              column(3,offset = 1,
                     selectizeInput(inputId = "id_new_agent", label = "Agent name:",
                                    options = list(create = TRUE),
                                    choices=c(""))
              ),
              column(2,
                     actionButton("button_rm_agent",label = "Remove agent", style = 'margin-top:25px')
              ),
              column(3,
                     selectizeInput(inputId = "id_agents_to_copy", label = "Copy information from:",
                                    choices=c(""))
              ),
              column(3,
                     actionButton("button_copy_agent",label = "Copy", style = 'margin-top:25px')
              )
            )
          ),
          box(
            width = 12,
            title = div(class = "icon-container",
                        h4("Determined flow ", icon("info-circle")),
                        div(class = "icon-text", "For each determined flow the first and last components must be the Spawnroom and the time associated to the last element of each flow (the Spawnroom) doesn't matter. This is because an agent starts and ends its flow outside the environment and the next entry time will be calculated automatically from the agent's time scheduling.")
            ),
            collapsible = T,
            fluidRow(
              column(3,offset = 1,
                     selectizeInput(inputId= "Det_select_room_flow",
                                    label="Type:",
                                    choices = ""
                     )
              ),
              column(3,offset=2, selectizeInput(inputId = "DetActivity", label = "Activity:", choices = c("", "Very Light - e.g. resting", "Light - e.g. speak while resting", "Quite Hard - e.g. speak/walk while standing", "Hard - e.g. loudly speaking"))
              ),
              column(1,
                     actionButton("add_room_to_det_flow", "Add room", style = 'margin-top:25px')
              ),
              column(1,offset=9,
                    actionButton("remove_room_to_det_flow", "Remove last room")
             )
            ),
            fluidRow(
              column(3,offset=1,
                     selectInput("agentLink_det_flow","Select an agent to link:",choices = "",selected = "")
              ),
              column(3,offset=2,
                     tags$h5(strong("Duration:")),
                     get_distribution_panel("det_flow")
              )
            ),
            conditionalPanel(
              condition="input.agentLink_det_flow != ''",
              fluidRow(
                column(2, offset = 1,
                       radioButtons(inputId = "ckbox_agentLink_det_flow",
                                    label = div(class = "icon-container",
                                                h4(icon("info-circle"), "Select type of link:"),
                                                div(class = "icon-text", "Two options are available: 'Accompaniment only' means the linked agent accompanies the agent only to its destination, while 'Accompaniment and stay' means the linked agent remains with the agent for the entire duration of the support.")
                                    ),
                                    choices = c("Accompaniment only", "Accompaniment and stay"),
                                    selected = "Accompaniment only"
                       )
                )
              )
            ),
            conditionalPanel(
              condition="input.ckbox_entranceFlow != 'Daily Rate'",
              fluidRow(
                column(1, offset = 1,
                       actionButton("add_det_flow", "Add flow", style="margin-top:20px;"),
                ),
                column(1,
                       actionButton("rm_det_flow", "Remove flow", style="margin-top:20px;")
                ),
              )
            ),
            fluidRow(
              column(5,offset = 1,
                     tabsetPanel(
                       id = "DetFlow_tabs"
                     )
              )
            )
          ),
          box(
            width = 12,
            collapsible = T,
            title = div(class = "icon-container",
                        h4("Random flow ", icon("info-circle")),
                        div(class = "icon-text", "A random event should happen rarely and last only a few minutes.")
            ),
            fluidRow(
              column(3,offset = 1,
                     selectizeInput(inputId= "Rand_select_room_flow",
                                    label="Type:",
                                    choices = ""
                     )
              ),
              column(3, offset=2, selectizeInput(inputId = "RandActivity", label = "Activity:", choices = c("", "Very Light - e.g. resting", "Light - e.g. speak while resting", "Quite Hard - e.g. speak/walk while standing", "Hard - e.g. loudly speaking"))
              ),
              column(width = 2,
                     actionButton("add_room_to_rand_flow", "Add room", style = 'margin-top:25px')
              )
            ),
            fluidRow(
              column(3,offset=1,
                     textInput(inputId = "RandWeight", label = "Weight:",placeholder = "")
              ),
              column(3,offset=2,
                     textInput(inputId = "EntryTimeRate_rand_flow", label = "Initial time:", placeholder = "hh:mm")
              ),
              column(3,offset=6,
                     textInput(inputId = "ExitTimeRate_rand_flow", label = "Ending time:", placeholder = "hh:mm")
              )
            ),
            fluidRow(
              column(width = 3,offset=1,
                     selectInput("agentLink_rand_flow","Select an agent to link:",choices = "",selected = "")
              ),
              column(offset = 2, width = 3,
                     tags$h5(strong("Duration:")),
                     get_distribution_panel("rand_flow")
              )
            ),
            conditionalPanel(
              condition="input.agentLink_rand_flow != ''",
              fluidRow(
                column(2, offset = 1,
                       radioButtons(inputId = "ckbox_agentLink_rand_flow",
                                    label = div(class = "icon-container",
                                                h4(icon("info-circle"), "Select type of link:"),
                                                div(class = "icon-text", "Two options are available: 'Accompaniment only' means the linked agent accompanies the agent only to its destination, while 'Accompaniment and stay' means the linked agent remains with the agent for the entire duration of the support.")
                                    ),
                                    choices = c("Accompaniment only", "Accompaniment and stay"),
                                    selected = "Accompaniment only"
                       )
                )
              )
            ),
            fluidRow(
              h3(""),
              column(10, offset=1,
                     div(id="rand_description", style="margin-top:20px;", "Click on an event to remove it. At each time step, the probability of doing nothing is equal to one minus the sum of the other probabilities.", hidden="hidden")
              )
            ),
            fluidRow(
              column(10,offset = 1,
                     DT::dataTableOutput(outputId = 'RandomEvents_table')
              )
            )
          ),
          box(
            width = 12,
            collapsible = T,
            title = "Entry flow",
            fluidRow(
              column(4,offset = 1,
                     fluidRow(
                       column(5,
                              radioButtons(inputId = "ckbox_entranceFlow",
                                           label = "Select type of entrace:",
                                           choices = c("Daily Rate", "Time window"),
                                           inline = TRUE,
                                           selected = "Time window"
                              ),
                              actionButton("set_timeslot", "Save time")
                       )
                     )
              ),
              column(5,offset=1,
                     conditionalPanel(
                       condition="input.ckbox_entranceFlow== 'Daily Rate'",

                       fluidRow(
                         sortableTabsetPanel(id = "Rate_tabs",
                                     tabPanel("1 slot",
                                              value = "slot_1",
                                              column(5,
                                                     tags$b("Entrance rate:"),
                                                     get_distribution_panel("daily_rate_1"),
                                                     textInput(inputId = "EntryTimeRate_1", label = "Initial generation time:", placeholder = "hh:mm"),
                                                     textInput(inputId = "ExitTimeRate_1", label = "Final generation time:", placeholder = "hh:mm")
                                              ),
                                              column(5,
                                                     checkboxGroupInput("selectedDaysRate_1", "Select Days of the Week",
                                                                        choices = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                                                                        selected = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
                                                     )
                                              )
                                     )
                         )
                       ),
                       fluidRow(
                         actionButton("add_slot_rate", "Add slot"),
                         actionButton("rm_slot_rate", "Remove slot")
                       )
                     ),
                     conditionalPanel(
                       condition="input.ckbox_entranceFlow== 'Time window' ",

                       fluidRow(
                         sortableTabsetPanel(id = "Shift_tabs",
                                     tabPanel("1 shift",
                                              value = "shift_1",
                                              fluidRow(
                                                column(4,offset=1,
                                                       textInput(inputId = "num_agent_1", label = "Number of agents:",
                                                                 placeholder = "The number must be a positive integer")
                                               )
                                             ),
                                             fluidRow(
                                               column(11,offset=1,
                                                      sortableTabsetPanel(id = "Time_tabs_1",
                                                             tabPanel("1 slot",
                                                                      value = "slot_1_1",
                                                                      column(7,
                                                                             textInput(inputId = "EntryTime_1_1", label = "Entry time:", placeholder = "hh:mm"),
                                                                             selectInput(inputId = paste0("Select_TimeDetFlow_1_1"),
                                                                                         label = "Associate with a determined flow:" ,
                                                                                         choices = "1 flow" )
                                                                      ),
                                                                      column(5,
                                                                             checkboxGroupInput("selectedDays_1_1", "Select Days of the Week",
                                                                                                choices = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                                                                                                selected = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
                                                                             )

                                                                      )
                                                                      )
                                                            )
                                               )
                                             )
                                     )
                         )
                       ),
                       fluidRow(
                         column(11,offset=1,
                           actionButton("add_slot", "Add slot"),
                           actionButton("rm_slot", "Remove slot")
                         )
                       ),
                       fluidRow(
                         actionButton("add_shift", "Add shift", style="margin-top:20px;"),
                         actionButton("rm_shift", "Remove shift", style="margin-top:20px;")
                       )
                     )
              )
            )
          )
        )
      ),
      ## Tab infection ####
      tabItem(
        tabName = "infection",
        fluidRow(
          box(
            width = 12,
            collapsible = T,
            title = h3("Disease model"),
            fluidRow(
              column(
                2,
                offset = 1,
                selectizeInput(
                  inputId = "disease_model",
                  label = "Model:",
                  choices = c(
                    "SIR",
                    "SIRD",
                    "SEIR",
                    "SEIRD",
                    "SIRS",
                    "SIRDS",
                    "SEIRS",
                    "SEIRDS"
                  ),
                  selected = "SIR"
                )
              ),
              column(
                5,
                offset = 1,
                textOutput("disease_model_value")
              )
            ),
            fluidRow(
              conditionalPanel(condition = 'input.disease_model == "SIR"',
                               img(
                                 src = "SIR.png",
                                 height = 300,
                                 class = "center-block"
                               )),
              conditionalPanel(condition = 'input.disease_model == "SIRD"',
                               img(
                                 src = "SIRD.png",
                                 height = 300,
                                 class = "center-block"
                               )),
              conditionalPanel(condition = 'input.disease_model == "SEIR"',
                               img(
                                 src = "SEIR.png",
                                 height = 300,
                                 class = "center-block"
                               )),
              conditionalPanel(condition = 'input.disease_model == "SEIRD"',
                               img(
                                 src = "SEIRD.png",
                                 height = 300,
                                 class = "center-block"
                               )),
              conditionalPanel(condition = 'input.disease_model == "SIRS"',
                               img(
                                 src = "SIRS.png",
                                 height = 300,
                                 class = "center-block"
                               )),
              conditionalPanel(condition = 'input.disease_model == "SIRDS"',
                               img(
                                 src = "SIRDS.png",
                                 height = 300,
                                 class = "center-block"
                               )),
              conditionalPanel(condition = 'input.disease_model == "SEIRS"',
                               img(
                                 src = "SEIRS.png",
                                 height = 300,
                                 class = "center-block"
                               )),
              conditionalPanel(condition = 'input.disease_model == "SEIRDS"',
                               img(
                                 src = "SEIRDS.png",
                                 height = 300,
                                 class = "center-block"
                               )
              )
            ),
            fluidRow(
              column(10, offset=1,
                     tags$h4("Description:"),
                     textOutput("description"),
                     tags$style("#description { height: 200px; font-size: 16px;}")
              )
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            collapsible = T,
            collapsed = F,
            title = h3("Disease Parameters & Risk Classes"),
            fluidRow(
              column(3, offset = 1,
                     checkboxInput(
                       inputId = "enable_risk_classes",
                       label = "Enable Risk Classes",
                       value = FALSE
                     )
              ),
              column(7,
                     tags$p(style = "font-size: 14px; color: #5a5a5a;",
                            "Configure disease parameters for the epidemic model. ",
                            "Enable Risk Classes to define multiple classes for the infectious state (I) with class-specific parameters."
                     )
              )
            ),
            fluidRow(
              column(3,offset = 1,
              conditionalPanel(
                condition = "input.enable_risk_classes == true",
                       numericInput(
                         inputId = "num_risk_classes",
                         label = "Number of Risk Classes:",
                         value = 2,
                         min = 2,
                         max = 5,
                         step = 1
                       )
                )
              ),
                column(3,offset = 8,
                       tags$div(
                         style = "margin-top: 20px; text-align: right;",
                         actionButton(
                           inputId = "save_values_disease_model",
                           label = "Save Parameters",
                           icon = icon("save"),
                           style = "color: white;",
                           class = "btn-primary"
                         )
                       )
                )
            ),
            conditionalPanel(
              condition = "input.enable_risk_classes == true",
              fluidRow(
                column(10, offset = 1,
                       uiOutput("risk_classes_ui")
                )
              )
            ),
            conditionalPanel(
              condition = "input.enable_risk_classes == false",
              fluidRow(
                column(10, offset = 1,
                       div(
                         style = "border-left: 4px solid #3498db; padding: 15px; margin-bottom: 15px; margin-top: 15px; background-color: #f9f9f9; border-radius: 8px;",
                         tags$h4(style = "color: #3498db; font-weight: 600; margin-bottom: 15px;",
                                 icon("globe"), " Global Disease Parameters"),
                         tags$p(style = "font-size: 13px; color: #6c757d; margin-bottom: 20px;",
                                "These parameters apply uniformly to all agents."
                         ),
                         fluidRow(
                           column(6,
                                  div(class = "icon-container",
                                      tags$label(icon("info-circle"), " β", tags$sub("contact"), ":"),
                                      tags$div(
                                        class = "icon-text",
                                        style = "width: 250px;",
                                        "β", tags$sub("contact"), " represents the contamination risk that refer to the infection due to close-range contacts based on the contagion model in [1]. For example, for the COVID-19 disease the correct value is 0.024. This is not the infection rate."
                                      )
                                  ),
                                  textInput(
                                    "beta_contact",
                                    label = NULL,
                                    width = "100px",
                                    value = 0.024
                                  )
                           ),
                           column(6,
                                  div(class = "icon-container",
                                      tags$label(icon("info-circle"), " β", tags$sub("aerosol"), ":"),
                                      tags$div(
                                        class = "icon-text",
                                        style = "width: 250px;",
                                        "β", tags$sub("aerosol"), " represents the risk constant that refer to the infection due to aerosol based on the contagion model in [2]. For example, for the COVID-19 disease the correct value is 410. This is not the infection rate."
                                      )
                                  ),
                                  textInput(
                                    "beta_aerosol",
                                    label = NULL,
                                    width = "100px",
                                    value = 410
                                  )
                           )
                         ),
                         fluidRow(
                           column(12,
                                  div(class = "icon-container",
                                      h5(icon("info-circle"), " γ (Recovery Rate):"),
                                      div(class = "icon-text", "γ represents the recovery rate.")
                                  ),
                                  get_distribution_panel("gamma")
                           )
                         ),
                         conditionalPanel(
                           condition = 'input.disease_model == "SEIR" || input.disease_model == "SEIRS" || input.disease_model == "SEIRD" || input.disease_model == "SEIRDS"',
                           fluidRow(
                             column(12,
                                    div(class = "icon-container",
                                        h5(icon("info-circle"), " α (Incubation Rate):"),
                                        div(class = "icon-text", "α represents the incubation rate.")
                                    ),
                                    get_distribution_panel("alpha")
                             )
                           )
                         ),
                         conditionalPanel(
                           condition = 'input.disease_model == "SIRD" || input.disease_model == "SEIRD" || input.disease_model == "SEIRDS" || input.disease_model == "SIRDS"',
                           fluidRow(
                             column(12,
                                    div(class = "icon-container",
                                        h5(icon("info-circle"), " λ (Fatality Rate):"),
                                        div(class = "icon-text", "λ represents the fatality rate.")
                                    ),
                                    get_distribution_panel("lambda")
                             )
                           )
                         ),
                         conditionalPanel(
                           condition = 'input.disease_model == "SIRS" || input.disease_model == "SEIRS" || input.disease_model == "SIRDS" || input.disease_model == "SEIRDS"',
                           fluidRow(
                             column(12,
                                    div(class = "icon-container",
                                        h5(icon("info-circle"), " ν (End-of-Immunization Rate):"),
                                        div(class = "icon-text", "ν represents the end-of-immunization rate.")
                                    ),
                                    get_distribution_panel("nu")
                             )
                           )
                         )
                       )
                )
              )
            )
          )
        )
      ),
      ## Tab setting ####
      tabItem(tabName = "settings",
              fluidRow(
                box(width = 12,
                    title = h3("Set floor dimension"),
                    fluidRow(
                      column(
                        offset = 1,
                        width = 8,
                        textInput(
                          "canvasHeight",
                          width = "100%",
                          "Height (meter)",
                          placeholder = "Floor height dimension (default 80m)"
                        ),
                        textInput(
                          "canvasWidth",
                          width = "100%",
                          "Width (meter)",
                          placeholder = "Floor width dimension (default 100m)"
                        ),
                        actionButton("set_canvas", "Set dimension", icon = icon("pen-ruler"))
                      )
                    ))
              ),
              fluidRow(
                box(
                  title = h3("Load a saved model"),
                  width = 12,
                  collapsible = T,
                  collapsed = T,
                  fluidRow(
                    column(
                      8,
                      offset = 1,
                      fileInput(
                        inputId = "RDsImport",
                        label = "",
                        placeholder = "Select an RDs file.",
                        width = "100%",
                        multiple = F
                      )
                    ),
                    column(
                      1,
                      style = "margin-top: 20px;",
                      actionButton(
                        label = "Load",
                        icon = shiny::icon("upload"),
                        inputId = "LoadRDs_Button"
                      )
                    )
                  )
                )
              ),
              fluidRow(
                box(
                  title = h3("Save the model"),
                  width = 12,
                  collapsible = T,
                  collapsed = T,
                  fluidRow(
                    column(
                      2,
                      offset = 1,
                      actionButton(
                        inputId = "check",
                        label = "Check model",
                        icon = icon("check"))
                    ),
                    column(
                      2,
                      offset = 1,
                      downloadButton(
                        outputId = "rds_generation",
                        label = "Save the model",
                        icon = icon("download"))
                    ),
                    column(
                      2,
                      offset = 1,
                      actionButton(
                        inputId = "flamegpu_connection",
                        label = "Link the model to FLAME GPU 2",
                        icon = icon("link"))
                    )
                  ),
                  fluidRow(
                    column(4, offset = 7,
                           textOutput("flame_link")
                    )
                  )
                )
              ),
              fluidRow(
                box(
                  title = h3("References"),
                  width = 12,
                  collapsible = T,
                  collapsed = T,
                  fluidRow(
                    column(
                      10,
                      offset = 1,
                      HTML("[1] HOERTEL, N., Blachier, M., Blanco, C., Olfson, M., Massetti, M., Rico, M. S., Limosin, F., & Leleu, H. (2020). A stochastic agent-based model of the SARS-CoV-2 epidemic in France. Nature Medicine, 26(9), 1417–1421. doi:<a href='https://doi.org/10.1038/s41591-020-1001-6'>https://doi.org/10.1038/s41591-020-1001-6</a><br>
                            [2] GKANTONAS, S., Zabotti, D., Mesquita, L. C., Mastorakos, E., & de Oliveira, P. M. (2021). airborne.cam: A risk calculator of SARS-CoV-2 aerosol transmission under well-mixed ventilation conditions. Available at: <a href='https://airborne.cam'>https://airborne.cam</a><br>
                            [3] J.L. Jimenez and Z. Peng, COVID-19 Aerosol Transmission Estimator. <a href='https://tinyurl.com/covid-estimator'>https://tinyurl.com/covid-estimator</a><br>
                            [4] Tolksdorf K, Buda S, Schuler E, Wieler LH, Haas W. Influenza-associated pneumonia as reference to assess seriousness of coronavirus disease (COVID-19). Euro Surveill. 2020 Mar;25(11):2000258. doi: <a href='https://doi.org/10.2807/1560-7917.ES.2020.25.11.2000258'>https://doi.org/10.2807/1560-7917.ES.2020.25.11.2000258</a>. Epub 2020 Mar 16. PMID: 32186278; PMCID: PMC7096775<br>
                            <br>
                            FLAME GPU 2 references:<br>
                            [5] Richmond, P., Chisholm, R., Heywood, P., Leach, M., Chimeh, M. K. FLAME GPU. Version 2.0.0-rc. Dec. 2022. <a href='https://doi.org/10.5281/zenodo.7434228'>https://doi.org/10.5281/zenodo.7434228</a><br>
                            [6] Richmond, P., Chisholm, R., Heywood, P., Chimeh, M. K, Leach, M. FLAME GPU 2: A framework for flexible and performant agent based simulation on GPUs. In: Software: Practice and Experience (2023). <a href='https://doi.org/10.1002/spe.3207'>https://doi.org/10.1002/spe.3207</a><br>
                            <br>
                            ABM school references:<br>
                            [7] Baccega, Daniele, Pernice, Simone, Terna, Pietro, Castagno, Paolo, Moirano, Giovenale, Richiardi, Lorenzo, Sereno, Matteo, Rabellino, Sergio, Maule, Milena Maria and Beccuti, Marco (2022) 'An Agent-Based Model to Support Infection Control Strategies at School' Journal of Artificial Societies and Social Simulation 25 (3) 2 <a href='http://jasss.soc.surrey.ac.uk/25/3/2.html'>http://jasss.soc.surrey.ac.uk/25/3/2.html</a>. doi: <a href='https://doi.org/10.18564/jasss.4830'>https://doi.org/10.18564/jasss.4830</a><br>
                            [8] Daniele Baccega, Simone Pernice, Paolo Castagno, Matteo Sereno, and Marco Beccuti. Evaluating the Impact of Mask and Quarantine Policies on the Spread of COVID-19 in Schools using computational modeling. In the 18th Conference on Computational Intelligence Methods for Bioinformatics & Biostatistics (CIBB 2023)"),
                    ),
                  )
                )
              )
      ),
      tabItem(tabName = "whatif",
              h2(tags$b("Countermeasures")),
              fluidRow(
                box(width = 12,collapsed = F,collapsible = T,
                    title =  div(class = "icon-container",
                                 h5(tags$b("Saved Countermeasures"), icon("info-circle")),
                                 div(class = "icon-text", "To remove rows in the tables, double click on it.")
                    ),
                    fluidRow(
                      column(width = 10,offset = 1,
                             DT::DTOutput("rooms_whatif")),
                      column(width = 10,offset = 1,
                             DT::DTOutput("agents_whatif"))
                    ))
              ),
              fluidRow(
                box(width = 12,collapsed = T,collapsible = T,
                    title = h3("Ventilation"),
                    fluidRow(
                      column(
                        offset = 1,
                        width = 2,
                        radioButtons(inputId = "ventilation_type",
                                     label = "Ventilation:",
                                     choices = c("Global", "Different for each room"),
                                     selected = "Global"
                        )
                      ),
                      conditionalPanel(
                        condition = 'input.ventilation_type == "Different for each room"',
                        column(
                          width = 2,
                          selectizeInput(
                            inputId = "room_ventilation",
                            label = "Room:",
                            options = list(),
                            choices = c()
                          )
                        )
                      ),
                      column(
                        width = 2,
                        selectizeInput(
                          inputId = "ventilation_params",
                          label = div(class = "icon-container",
                                      h5(tags$b("Ventilation (in ACH): "), icon("info-circle")),
                                      div(class = "icon-text", "For instance, 3 Air Changes per Hour (ACH) means that in 1 hour 300.000 L (or analogous 300 squared meters) of external air are entered into the considered room.")
                          ),
                          choices = c("0 (no ventilation)", "0.3 (poorly ventilated)", "1 (domestic)", "3 (offices/schools)", "5 (well ventilated)", "10 (typical maximum)", "20 (hospital setting)"),
                          selected = "0 (no ventilation)"
                        )
                      ),
                      column(1, numericInput(inputId = "ventilation_time_from", label = "From (day):", value = 1, min = 1)),
                      column(1, numericInput(inputId = "ventilation_time_to", label = "To (day):", value = 10, min = 1)),
                      column(1,offset=11, actionButton("save_ventilation", "Save"))
                    ))
              ),
              fluidRow(
                box(width = 12,collapsed = T,collapsible = T,
                    title = h3("Masks"),
                    fluidRow(
                      column(offset = 1, width =2, radioButtons(inputId = "mask_type", label = "Mask:",
                                                                choices = c("Global", "Different for each agent"), selected = "Global")),
                      conditionalPanel(
                        condition = 'input.mask_type == "Different for each agent"',
                        column(
                          width = 2,
                          selectizeInput(
                            inputId = "agent_mask",
                            label = "Agent:",
                            options = list(),
                            choices = c()
                          )
                        )
                      ),
                      column(2, selectizeInput(inputId = "mask_params",
                                               label = "Mask type:",
                                               choices = c("No mask", "Surgical mask", "FFP2 mask"),
                                               selected = "No mask")),
                      column(
                        width = 2,
                        numericInput(
                          inputId = "mask_fraction",
                          label = div(class = "icon-container",
                                      h5(tags$b("% mask: "), icon("info-circle")),
                                      div(class = "icon-text", "Fraction of agent wearing mask")
                          ),
                          value = 1, min = 0, max = 1
                        )
                      ),
                      column(1, numericInput(inputId = "mask_time_from", label = "From (day):", value = 1, min = 1)),
                      column(1, numericInput(inputId = "mask_time_to", label = "To (day):", value = 10, min = 1)),
                      column(1,offset=11, actionButton("save_masks", "Save"))
                    ))
              ),
              fluidRow(
                box(width = 12,collapsed = T,collapsible = T,
                    title = h3("Vaccination"),
                    fluidRow(
                      column(
                        offset = 1,
                        width = 2,
                        radioButtons(inputId = "vaccination_type",
                                     label = "Vaccination:",
                                     choices = c("Global", "Different for each agent"),
                                     selected = "Global"
                        )
                      ),
                      conditionalPanel(
                        condition = 'input.vaccination_type == "Different for each agent"',
                        column(
                          width = 2,
                          selectizeInput(
                            inputId = "agent_vaccination",
                            label = "Agent:",
                            options = list(),
                            choices = c()
                          )
                        )
                      ),
                      column(
                        width = 2,
                        numericInput(
                          inputId = "vaccination_fraction",
                          label = "Fraction of vaccinated agents:",
                          value = 1, min = 0, max = 1
                        )
                      ),
                      column(
                        width = 2,
                        numericInput(
                          inputId = "vaccination_efficacy",
                          label = "Vaccine efficacy:",
                          value = 1, min = 0, max = 1
                        )
                      )
                    ),
                    div(style = "height:30px"),
                    fluidRow(
                      column(
                        width = 4,offset = 3,
                        div(h5(tags$b("Vaccine coverage (day):"))),
                        get_distribution_panel("vaccination_coverage")
                      )
                    ),
                    fluidRow(
                      column(1, offset=3, numericInput(inputId = "vaccination_time_from", label = "At (day):", value = 1, min = 1)),
                      #column(1, numericInput(inputId = "vaccination_time_to", label = "To (day):", value = 10, min = 1)),
                      column(1,offset=11, actionButton("save_vaccination", "Save"))
                    )
                )
              ),
              fluidRow(
                box(width = 12,collapsed = T,collapsible = T,
                    title = h3("Swabs"),
                    fluidRow(
                      column(
                        offset = 1,
                        width = 2,
                        radioButtons(inputId = "swab_type",
                                     label = "Swab:",
                                     choices = c( "Global", "Different for each agent"),
                                     selected = "Global"
                        )
                      ),
                      column(
                        width = 1,
                        numericInput(
                          inputId = "swab_sensitivity",
                          label = "Sensitivity:",
                          value = 1,min = 0,max =1
                        )
                      ),
                      column(
                        width = 1,
                        numericInput(
                          inputId = "swab_specificity",
                          label = "Specificity:",
                          value = 1,min = 0,max =1
                        )
                      )
                    ),
                    fluidRow(
                      conditionalPanel(
                        condition = 'input.swab_type == "Different for each agent"',
                        column(
                          width = 2,offset = 3,
                          selectizeInput(
                            inputId = "agent_swab",
                            label = "Agent:",
                            options = list(),
                            choices = c()
                          )
                        ),
                        column(
                          width = 2,
                          radioButtons(inputId = "swab_type_specific",
                                       label = "Swab:",
                                       choices = c("No swab", "Swab"),
                                       selected = "Swab"
                          )
                        )
                      )
                    ),
                    fluidRow(
                      conditionalPanel(
                        condition = 'input.swab_type_specific != "No swab"',
                        column(4,offset = 3,
                               div(h5(tags$b("A swab every how many days?"))),
                               get_distribution_panel("swab_days")
                        )
                      ),
                    ),
                    fluidRow(
                      column(1, offset=3, numericInput(inputId = "swab_time_from", label = "From (day):", value = 1, min = 1)),
                      column(1, numericInput(inputId = "swab_time_to", label = "To (day):", value = 10, min = 1)),
                      column(1,offset=11, actionButton("save_swab", "Save"))
                    )
                )
              ),
              fluidRow(
                box(width = 12,collapsed = T,collapsible = T,
                    title = h3("Quarantine"),
                    fluidRow(
                      fluidRow(
                        column(
                          offset = 1,
                          width = 2,
                          radioButtons(inputId = "quarantine_type",
                                       label = "Quarantine:",
                                       choices = c("Global", "Different for each agent"),
                                       selected = "Global"
                          )
                        ),
                        conditionalPanel(
                          condition = 'input.quarantine_type == "Different for each agent"',
                          column(
                            width = 2,
                            selectizeInput(
                              inputId = "agent_quarantine",
                              label = "Agent:",
                              options = list(),
                              choices = c()
                            )
                          ),
                          column(width = 2,
                                 radioButtons(inputId = "quarantine_type_agent",
                                              label = "Quarantine:",
                                              choices = c("No quarantine", "Quarantine"),
                                              selected = "No quarantine"
                                 )
                          )
                        ),
                        conditionalPanel(
                          condition = 'input.quarantine_type == "Global" || (input.quarantine_type == "Different for each agent" && input.quarantine_type_agent != "No quarantine")',
                          conditionalPanel(
                            condition = 'input.quarantine_type == "Global"',
                            column( width = 3,
                                    div(h5(tags$b("Quarantine days:"))),
                                    get_distribution_panel("quarantine_global")
                            )
                          ),
                          conditionalPanel(
                            condition = 'input.quarantine_type == "Different for each agent"',
                            column(offset = 3, width = 3,
                                   div(h5(tags$b("Quarantine days:"))),
                                   get_distribution_panel("quarantine_global")
                            )
                          ),
                          column(
                            width = 2,
                            selectizeInput(
                              inputId = "room_quarantine",
                              label = div(class = "icon-container",
                                          h5(tags$b("Quarantine room for severe cases:"), icon("info-circle")),
                                          div(class = "icon-text", "Select the quarantine room for severe cases, the default is outside the environment (spawnroom).")),
                              options = list(),
                              choices = c()
                            )
                          )
                        ),
                      )
                    ),
                    fluidRow(
                      conditionalPanel(
                        condition = 'input.quarantine_type != "Different for each agent" || input.quarantine_type_agent != "No quarantine"',
                        fluidRow(
                          column( width = 2, offset = 3,
                                  radioButtons(inputId = "quarantine_swab_type_global",
                                               label = "Swab:",
                                               choices = c("No swab", "Swab"),
                                               selected = "Swab"
                                  )
                          ),
                        ),
                        fluidRow(
                          conditionalPanel(
                            condition = 'input.quarantine_swab_type_global == "Swab"',
                            fluidRow(
                              column(
                                width = 2,
                                offset=3,
                                numericInput(
                                  inputId = "quarantine_swab_sensitivity",
                                  label = "Sensitivity:",
                                  value = 1,min = 0,max =1
                                )
                              ),
                              column(
                                width = 2,
                                numericInput(
                                  inputId = "quarantine_swab_specificity",
                                  label = "Specificity:",
                                  value = 1,min = 0,max =1
                                )
                              )
                            ),
                            fluidRow(
                              column(4,offset = 3,
                                     div(h5(tags$b("A swab every how many days?"))),
                                     get_distribution_panel("quarantine_swab_global")
                              )
                            )
                          )
                        ),
                        fluidRow(
                          column(1,offset = 3, numericInput(inputId = "quarantine_time_from", label = "From (day):", value = 1, min = 1)),
                          column(1, numericInput(inputId = "quarantine_time_to", label = "To (day):", value = 10, min = 1)),
                          column(1,offset=11, actionButton("save_quarantine", "Save"))
                        )
                      )
                    )
                )
              ),
              fluidRow(
                box(width = 12,collapsed = T,collapsible = T,
                    title = h3("External screening"),
                    fluidRow(
                      fluidRow(
                        column(
                          offset = 1,
                          width = 2,
                          radioButtons(inputId = "external_screening_type",
                                       label = "External screening:",
                                       choices = c("Global", "Different for each agent"),
                                       selected = "Global"
                          )
                        ),
                        conditionalPanel(
                          condition = 'input.external_screening_type == "Different for each agent"',
                          column(
                            width = 2,
                            selectizeInput(
                              inputId = "agent_external_screening",
                              label = "Agent:",
                              options = list(),
                              choices = c()
                            )
                          )
                        ),
                        column(
                          width = 2,
                          numericInput(
                            inputId = "external_screening_first_global",
                            label = div(class = "icon-container",
                                        h5(tags$b("Screening campaigns:"), icon("info-circle")),
                                        div(class = "icon-text", "Probability to test an agent outside the environment because this agent follows activities that involve screening campaings (like practise sports).")
                            ),
                            value = 1, min = 0, max = 1
                          )
                        ),
                        column(
                          width = 2,
                          numericInput(
                            inputId = "external_screening_second_global",
                            label = div(class = "icon-container",
                                        h5(tags$b("Symptoms:"), icon("info-circle")),
                                        div(class = "icon-text", "Probability to test an infected agent outside the environment due to symptoms.")
                            ),
                            value = 1, min = 0, max = 1
                          )
                        )
                      ),
                      fluidRow(
                        column(1, offset = 3, numericInput(inputId = "external_screening_time_from", label = "From (day):", value = 1, min = 1)),
                        column(1, numericInput(inputId = "external_screening_time_to", label = "To (day):", value = 10, min = 1)),
                        column(1, offset=11, actionButton("save_external_screening", "Save"))
                      )
                    )
                )
              ),
              fluidRow(
                h2(tags$b("Virus parameters"), style="margin-left:15px;")
              ),
              fluidRow(
                box(width = 12, collapsed = T,collapsible = T,
                    title =  div(class = "icon-container",
                                 h5(tags$b("Saved Virus Parameters"), icon("info-circle")),
                                 div(class = "icon-text", "To remove rows in the tables, double click on it (except for variant and severity).")
                    ),
                    fluidRow(
                      column(width = 6,
                             DT::DTOutput("virus_info")),
                      column(width = 6,
                             DT::DTOutput("initialI_info"))
                    ))
              ),
              fluidRow(
                box(width = 12, collapsed = F,collapsible = T,
                    title = h3("Virus"),
                    fluidRow(
                      column(
                        offset = 1,
                        width = 2,
                        div(class = "icon-container",
                            h5(tags$b("Virus variant factor: "), icon("info-circle")),
                            div(class = "icon-text", "The variant of the virus to model. Use the value 1 if there are no variants or if you want to model the base variant. In [3] you can find some example for the Covid-19.")
                        ),
                        numericInput(
                          inputId = "virus_variant",
                          label = NULL,
                          value = 1, min =0
                        )
                      ),
                      column(
                        offset = 1,
                        width = 2,
                        div(class = "icon-container",
                            h5(tags$b("Virus severity: "), icon("info-circle")),
                            div(class = "icon-text", "Probability to show sever symptoms. In [4] you can find an example for the Covid-19.")
                        ),
                        numericInput(
                          inputId = "virus_severity",
                          label = NULL,
                          value = 0.22, max = 1, min = 0
                        )
                      ),
                      column(1, offset=11, actionButton("save_virus", "Save"))
                    ))
              ),
              fluidRow(
                box(width = 12,
                    title = h3("Initial infected"),
                    fluidRow(
                      column(
                        offset = 1,
                        width = 2,
                        radioButtons(inputId = "initial_infected_type",
                                     label =
                                       div(class = "icon-container",
                                           h5(tags$b("Intial infected: "), icon("info-circle")),
                                           div(class = "icon-text", "Set the initial number of infected considering the agents with an entry flow set as 'Time Window'.\n
                                             Random means that the number of initial infected agents is sampled considering all the agents.\n
                                             Global means that thenumber id used to set the initial number of infected agents for all the agents type.")
                                       ),
                                     choices = c("Random", "Global", "Different for each agent"),
                                     selected = "Random"
                        )
                      ),
                      conditionalPanel(
                        condition = 'input.initial_infected_type == "Different for each agent"',
                        column(
                          width = 2,
                          selectizeInput(
                            inputId = "agent_initial_infected",
                            label = "Agent:",
                            options = list(),
                            choices = c()
                          )
                        )
                      ),
                      column(
                        width = 2,
                        numericInput(
                          inputId = "initial_infected_global",
                          label = "Initial infected:",
                          value = 1, min = 0
                        )
                      ),
                      column(1, offset=11, actionButton("save_initial_infected", "Save"))
                    )
                )
              ),
              fluidRow(
                box(width = 12,
                    title = div(class = "icon-container", style="margin-top:20px",
                                h3("Outside contagion ", icon("info-circle")),
                                div(class = "icon-text", "Must be a CSV file with two columns: day and percentage_infected.")
                    ),
                    fluidRow(
                      column(
                        8,
                        offset = 1,
                        fileInput(
                          inputId = "OutsideContagionImport",
                          label = "",
                          placeholder = "Select an csv file.",
                          width = "100%",
                          multiple = F
                        )
                      ),
                      column(
                        1,
                        style = "margin-top: 20px;",
                        actionButton(
                          label = "Load",
                          icon = shiny::icon("upload"),
                          inputId = "LoadCSV_Button_OutsideContagion"
                        )
                      )
                    ),
                    fluidRow(
                      column(
                        offset = 1,
                        width = 2,
                        div(class = "icon-container",
                            h5(tags$b("Population: "), icon("info-circle")),
                            div(class = "icon-text", "If the provided 'percentage_infected' values represent the average number of actual infections rather than percentages, please specify the population size to use for converting these values into percentages.")
                        ),
                        textInput(
                          inputId = "population",
                          label = "",
                          value = 1
                        )
                      )
                    ),
                    fluidRow(
                      column(
                        8, offset = 1,
                        style = "margin-top: 20px;",
                        plotOutput("outside_contagion_plot")
                      )
                    )
                )
              )
      ),
      tabItem(tabName = "configuration",
              fluidRow(
                box(width = 12,
                    title = h3("Configuration parameters"),
                    fluidRow(
                      column(
                        offset = 1,
                        width = 2,
                        textInput(inputId = "simulation_days", label = "Simulation days:", placeholder = "Number of days to simulate", value = "10")
                      ),
                      column(
                        offset=1,
                        width = 2,
                        radioButtons(inputId = "initial_day",
                                     label = "Initial day:",
                                     choices = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                                     selected = "Monday"
                        )
                      ),
                      column(
                        offset=1,
                        width = 1,
                        textInput(inputId = "initial_time", label = "Initial time:", placeholder = "hh:mm", value = "00:00")
                      ),
                      column(offset=1,
                             width = 1,
                             style="margin-top:-20px",
                             selectInput("step", div(class = "icon-container", style="margin-top:20px",
                                                     h5(tags$b("Step: "), icon("info-circle")),
                                                     div(class = "icon-text", "Duration of a FLAME GPU 2 step in seconds.")
                             ),
                             choices = c(1, 2, 3, 4, 5, 6, 10, 12, 15, 20, 30, 60),
                             selected = 60)
                      )
                    )
                ))
      ),
      tabItem(tabName = "run",
              fluidRow(
                box(width = 12,
                    title = h3("Run models"),
                    fluidRow(
                      fluidRow(
                        column(
                          offset = 1,
                          width = 2,
                          textInput(inputId = "seed", label = "Seed:", placeholder = "Simulation seed", value = as.integer(as.numeric(Sys.time())))
                        ),
                        column(
                          offset = 1,
                          width = 2,
                          textInput(inputId = "nrun", label = "Number of simulations:", placeholder = "Number of simulations", value = "100")
                        ),
                        column(
                          offset = 1,
                          width = 2,
                          textInput(inputId = "prun", label = "Number of parallel simulations:", value = "10")
                        )
                      ),
                      fluidRow(
                        column(2,
                               offset = 1,
                               selectInput("run_type", "Select run type:", choices=c("Local with 3D visualisation", "Local", "Docker"), selected = "Docker")
                        )
                      ),
                      fluidRow(
                        column(6, offset = 1, textOutput("error_docker"), tags$style("#error_docker {color:red;}"))
                      ),
                      fluidRow(
                        column(1,offset=1,
                               actionButton(
                                 inputId = "check_run",
                                 label = "Check model",
                                 icon = icon("check"))
                        ),
                        column(1,
                               actionButton("run", "Run", disabled = TRUE, icon = icon("play"))
                        )
                      )
                    ),
                    br(),
                    fluidRow(
                      box(
                        style = "padding-top:20px;",
                        title = "Simulation Log",
                        status = "primary", solidHeader = TRUE, width = 12, height = "600px",
                        div(style = "height: 500px; overflow-y: scroll;",
                            verbatimTextOutput("log_content"))
                      )
                    ),
                    fluidRow(
                      column(1,
                             actionButton("stop_run", "Stop")
                      )
                    )
                )
              )
      ),
      tabItem(tabName = "post_process",
              fluidRow(
                box(width = 12,
                    title = div(class = "icon-container", style="margin-top:20px",
                                h3("Uploading simulation ", icon("info-circle")),
                                div(class = "icon-text", "Must be the folder containing all the simulations obtained throught FLAMEGPU2.")
                    ),
                    fluidRow(
                      column(
                        width = 2,
                        offset = 1,
                        shinyDirButton("dir", "Select Folder", "Upload")
                        # fileInput(
                        #   inputId = "CSVsimulImport",
                        #   label = "",
                        #   placeholder = "Select an csv file.",
                        #   width = "100%",
                        #   multiple = F
                        # )
                      ),
                      column(
                        1,
                        #style = "margin-top: 20px;",
                        actionButton(
                          label = "Load",
                          icon = shiny::icon("upload"),
                          inputId = "LoadFolderPostProc_Button"
                        )
                      ),
                      column(
                        width = 4,
                        #style = "margin-top: 20px;",
                        verbatimTextOutput("dirPath")
                      )
                    ),
                    fluidRow(
                      column(
                        offset = 4,
                        width = 3,
                        style = "margin-top: 20px;",
                        downloadButton("DownloadPostProc_Button", label = "Download processed data")
                      )
                    ),
                    fluidRow(
                      column(6, offset = 1, textOutput("error_docker_postproc"), tags$style("#error_docker_postproc {color:red;}"))
                    ),
                    fluidRow(
                      column(width = 4, offset = 1, style = "margin-top: 20px;",
                             DT::dataTableOutput("Folder_Selection_Compose")
                      )
                    ),
                    conditionalPanel(
                      condition = "input.dir != 'NULL'",
                      fluidRow(
                        column(width = 6,
                               div(class = "icon-container", style="margin-top:20px",
                                   h4("Query on Disease Status", icon("info-circle")),
                                   div(class = "icon-text", "Find the simulations with defined specification on the disease.")
                               ),
                               uiOutput("PostProc_filters")
                        ),
                        column(width = 5,
                               div(class = "icon-container", style="margin-top:20px",
                                   h4("Resulting Simulations", icon("info-circle")),
                                   div(class = "icon-text", "Click on the table to visualise the corresponding disease dynamics.")
                               ),
                               DT::dataTableOutput("PostProc_table")
                        )
                      )
                    )
                )
              ),
              fluidRow(
                box(width = 12,collapsed = T,collapsible = T,
                    title =   div(class = "icon-container", style="margin-top:20px",
                                  h3("Disease Visualisation", icon("info-circle")),
                                  div(class = "icon-text", "Click on the table to visualise the corresponding disease dynamics.")
                    ),
                    fluidRow(
                      column(10,
                             plotOutput("EvolutionPlot", width = "100%", height = "800px")
                      ),
                      column(2,
                             checkboxGroupInput("EvolutionDisease_radioButt",
                                                choices = c("Mean curves", "Area from all simulations"),
                                                label = "Show:",selected = character()
                             )
                      )
                    ),
                    div(style = "height:10px"),
                    fluidRow(
                      column(10,
                             plotOutput("CountersPlot", width = "100%", height = "800px")
                      ),
                      column(2,
                             checkboxGroupInput("CountersDisease_radioButt",
                                                choices = c("Mean curves", "Area from all simulations"),
                                                label = "Show:",selected = character()
                             )
                      )
                    ),
                    div(style = "height:10px"),
                    fluidRow(
                      column(5,offset = 2,
                             selectInput("Room_Counters_A_C_selectize",choices = "",
                                         label = div(class = "icon-container", style="margin-top:20px",
                                                     h3("Choice of the room:", icon("info-circle")),
                                                     div(class = "icon-text", "Select the room to visualize the respective number of contacts and virus concentration over time. A contact between two agents is defined as the situation where they remain close to each other for a certain number of steps without ever separating.")
                                         )
                             )
                      )
                    ),
                    fluidRow(
                      column(10,
                             plotOutput("A_C_CountersPlot", width = "100%")
                      ),
                      column(2,
                             checkboxGroupInput("A_C_CountersDisease_radioButt",
                                                choices = c("Mean curves", "Area from all simulations"),
                                                label = "Show:",selected = character()
                             )
                      )
                    )
                )
              ),
              fluidRow(
                box(width = 12,collapsed = T,collapsible = T,
                    title = div(class = "icon-container", style="margin-top:20px",
                                h3("2D Simulation Visualisation", icon("info-circle")),
                                div(class = "icon-text", "2D visualisation of the agents moving in the modeled system")
                    ),
                    # fluidRow(
                    #   column(2,offset = 1,
                    #          uiOutput("subfolderUI")
                    #   )
                    # ),
                    fluidRow(
                      box(width = 12, status = "primary",
                          solidHeader = TRUE, collapsible = T,
                          title = div(class = "icon-container", style="margin-top:20px",
                                      h4("Features ", icon("info-circle")),
                                      div(class = "icon-text", "Further feature that can be used to costumise the 2D visualisation of the simulation.")
                          ),
                          fluidRow(
                            column(2,
                                   selectizeInput("visualFloor_select","Select floor to visualise:", choices = "All")
                            ),
                            column(2,
                                   selectizeInput("visualAgent_select","Select agent type to visualise:", choices = "All")
                            ),
                            conditionalPanel( "input.visualAgent_select != 'All'",
                                              column(2,
                                                     selectizeInput("visualAgentID_select", "Select agent id to visualise:", choices = "All")
                                              )
                            ),
                            column(2,
                                   selectizeInput("visualColor_select","Select colour room:", choices = c("Name", "Type", "Area",
                                                                                                          "Cumulative #Contacts" = "CumulContact",
                                                                                                          "Aerosol" = "Aerosol",
                                                                                                          "Cumulative Aerosol" = "CumulAerosol"))
                            ),
                            column(2,
                                   radioButtons("visualLabel_select","Show in the plot:",
                                                selected = "None",
                                                choices = c("None", "ID", "Name", "Type", "Area", "Agent ID"))
                            )

                          )
                      )
                    ),
                    fluidRow(
                      column(2,
                             h4("2D visualisation")
                      )
                    ),
                    fluidRow(
                      column(9,
                             sliderInput("animation", "Time in the animation (sec):",
                                         min = 0, max = 1,
                                         value = 0, step = 1,
                                         animate = animationOptions(interval = 1000, loop = TRUE)
                             )
                      ),
                      column(1,
                             style="padding-top:35px;padding-left:50px;",
                             actionButton("next_step_visual", label = HTML("<i class='fa fa-forward'></i> Next"),
                                          class = "btn-primary")
                      ),
                      column(2,
                             style="padding-top:10px;",
                             numericInput("animationStep",label = "Set the animation step (sec):", value = 1, min = 1)
                      )
                    ),
                    fluidRow(
                      column(12,
                             uiOutput("TwoDMapPlots", width = "100%", height = "1200px")
                      )
                    )
                )
              ),
              fluidRow(
                box(width = 12,collapsed = T,collapsible = T,
                    title = h3("Contact Matrix"),
                    plotOutput("ContactMatrix_plot", width = "100%", height = "1200px")
                )
              ),
      )
      #### END tabs ####
    )
  ),
  skin = "purple"
)
