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
  dashboardHeader(
    title = "Build your ABM",
    tags$li(
      class = "dropdown d-flex align-items-center",
      tags$head(tags$link(rel = "shortcut icon", href = "F4Ficon.png")),
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
                menuItem("Settings", tabName = "settings", icon = icon("cogs")),
                menuItem("Run", tabName = "run", icon = icon("play")),
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
        fluidRow(
          box(
            class = "info",
            width = 12,
            img(src = "F4Ficon.png", height = 100, width = 100),
            br(),
            strong(h1("Forge4Flame (F4F)")),
            br(),
            div(
              style="text-align:left;",
              p(h3("")),
              HTML("
                <h2>
                    F4F is a user-friendly dashboard (developed in R Shiny) designed to simplify the definition of an ABM environment for FLAME GPU 2 [5, 6] agent-based models, automatically generating the necessary code.
                    It enables users to define the model’s environment, the agents interacting within it, the disease model, and other components relevant to an ABM simulation.
                    F4F is constituted by the following components (the images refer to the school model defined in [7, 8]):
                </h2>
                <h2>
                  <ul>

                    <li class='home'>
                      <b>Canvas</b>: define the model’s environment using a drag-and-drop interface for rooms.
                      <img class='home' src='Canvas.png' alt='Canvas page', width='100%'>
                    </li>
                    <li class='home'>
                      <b>Rooms</b>: definenew room types.
                      <img class='home' src='Rooms.png' alt='Rooms page', width='100%'>
                    </li>
                    <li class='home'>
                      <b>Agents</b>: define new agent types and their associated movements within the model.
                      <img class='home' src='Agents.png' alt='Agents page', width='100%'>
                    </li>
                    <li class='home'>
                      <b>Resources</b>: specify the number of agents allowed in each room.
                      <img class='home' src='Resources.png' alt='Resources page', width='100%'>
                    </li>
                    <li class='home'>
                      <b>Infection</b>: define the disease model.
                      <img class='home' src='Infection.png' alt='Infection page', width='100%'>
                    </li>
                    <li class='home'>
                      <b>What-If</b>: perform what-if analyses.
                      <img class='home' src='Countermeasures.png' alt='What-If page (countermeasures)', width='100%'>
                      <img class='home' src='Virus.png' alt='What-If page (virus)', width='100%'>
                    </li>
                    <li class='home'>
                      <b>Configuration</b>: set up initial configurations.
                      <img class='home' src='Configuration.png' alt=Configuration page', width='100%'>
                    </li>
                    <li class='home'>
                      <b>Settings</b>: change canvas dimension, and load and save model.
                      <img class='home' src='Settings.png' alt=Settings page', width='100%'>
                    </li>
                    <li class='home'>
                      <b>Post Processing</b>: Post Processing of the simulations.
                      <img class='home' src='2DVisualisation.png' alt=Post Processing page', width='100%'>
                    </li>
                  </ul>
                </h2>
                ")
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
            )
          )
        ),
        fluidRow(
          box(
            title = h3("Add elements"),
            width = 12,
            collapsible = T,
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
                         choices = c("Normal", "Stair", "Spawnroom", "Fillingroom","Waitingroom")
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
            fluidRow(
              column(10,
                     div(class = "icon-container",
                         h4("Agent definition ", icon("info-circle")),
                         div(class = "icon-text", "The agent class represents the higher level class to which an agent belongs. For example, we could have the agents surgeon_senology and surgeon_ophthalmology that belong to the class surgeon or doctor.")
                     )
              ),
            ),
            fluidRow(
              column(3,offset = 1,
                     selectizeInput(inputId = "id_new_agent", label = "Agent name:",
                                    options = list(create = TRUE),
                                    choices=c(""))
              ),
              column(1,
                     actionButton("button_rm_agent",label = "Remove agent", style = 'margin-top:25px')
              ),
              column(3,
                     selectizeInput(inputId = "id_agents_to_copy", label = "Copy information from:",
                                    choices=c(""))
              ),
              column(3,
                     actionButton("button_copy_agent",label = "Copy", style = 'margin-top:25px')
              )
            ),
            fluidRow(
              column(3,offset = 1,
                     selectizeInput(inputId = "id_class_agent", label = "Agent class:",
                                    options = list(create = TRUE),
                                    choices=c(""))
              ),
              column(3,offset=1,
                     textInput(inputId = "num_agent", label = "Number of agents:",
                               placeholder = "The number must be a positive integer")
              )
            ),
          ),
          box(
            width = 12,
            collapsible = T,
            fluidRow(
              column(10,
                     div(class = "icon-container",
                         h4("Determined flow ", icon("info-circle")),
                         div(class = "icon-text", "For each determined flow the first and last components must be the Spawnroom and the time associated to the last element of each flow (the Spawnroom) doesn't matter. This is because an agent starts and ends its flow outside the environment and the next entry time will be calculated automatically from the agent's time scheduling.")
                     )
              )
            ),
            fluidRow(
              column(2,offset = 1,
                     selectizeInput(inputId= "Det_select_room_flow",
                                    label="Type:",
                                    choices = ""
                     )
              ),
              column(2, selectizeInput(inputId = "DetActivity", label = "Activity:", choices = c("", "Very Light - e.g. resting", "Light - e.g. speak while resting", "Quite Hard - e.g. speak/walk while standing", "Hard - e.g. loudly speaking"))
              ),
              column(3,
                     get_distribution_panel("det_flow")
              ),
              fluidRow(
                column(3,
                       actionButton("add_room_to_det_flow", "Add room", style = 'margin-top:25px')
                ),
                column(3,
                       actionButton("remove_room_to_det_flow", "Remove last room", style = 'margin-top:10px')
                )
              )
            ),
            conditionalPanel(
              condition="input.ckbox_entranceFlow != 'Daily Rate'",
              fluidRow(
                column(1, offset = 1,
                       actionButton("add_det_flow", "Add flow"),
                ),
                column(1,
                       actionButton("rm_det_flow", "Remove flow")
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
            fluidRow(column(5, div(class = "icon-container",
                                   h4("Random flow ", icon("info-circle")),
                                   div(class = "icon-text", "A random event should occur infrequently and last for a short duration (few minutes).")
            ))),
            fluidRow(
              column(2,offset = 1,
                     selectizeInput(inputId= "Rand_select_room_flow",
                                    label="Type:",
                                    choices = ""
                     )
              ),
              column(2, selectizeInput(inputId = "RandActivity", label = "Activity:", choices = c("", "Very Light - e.g. resting", "Light - e.g. speak while resting", "Quite Hard - e.g. speak/walk while standing", "Hard - e.g. loudly speaking"))
              ),
              column(1,
                     textInput(inputId = "RandWeight", label = "Weight:",placeholder = "")
              ),
              column(3,
                     get_distribution_panel("rand_flow")
              ),
              column(3,
                     actionButton("add_room_to_rand_flow", "Add room", style = 'margin-top:25px')
              )
            ),
            fluidRow(
              column(10, offset=1,
                     div(id="rand_description", "Click on an event to remove it (except the 'Do nothing' event)", hidden="hidden")
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
            fluidRow(column(5, tags$h4("Entry flow"))),
            fluidRow(
              column(4,offset = 1,
                     fluidRow(
                       column(8,offset = 1,
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
              column(6,
                     conditionalPanel(
                       condition="input.ckbox_entranceFlow== 'Daily Rate'",

                       fluidRow(
                         tabsetPanel(id = "Rate_tabs",
                                     tabPanel(paste0(1," slot"),
                                              value = paste0(1," slot"),
                                              column(7,
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
                         tabsetPanel(id = "Time_tabs",
                                     tabPanel(paste0(1," slot"),
                                              value = paste0(1," slot"),
                                              column(7,
                                                     textInput(inputId = "EntryTime_1", label = "Entry time:", placeholder = "hh:mm"),
                                                     selectInput(inputId = paste0("Select_TimeDetFlow_",1),
                                                                 label = "Associate with a determined flow:" ,
                                                                 choices = "" )
                                              ),
                                              column(5,
                                                     checkboxGroupInput("selectedDays_1", "Select Days of the Week",
                                                                        choices = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                                                                        selected = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
                                                     )

                                              )
                                     )
                         )
                       ),
                       fluidRow(
                         actionButton("add_slot", "Add slot"),
                         actionButton("rm_slot", "Remove slot")
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
              ),
              column(10, offset = 1,
                     div(class = "icon-container",
                         h5(icon("info-circle"), " β", tags$sub("contact"), "="),
                         div(class = "icon-text", "β", tags$sub("contact"), "represents the contamination risk that refer to the infection due to close-range contacts based on the contagion model in [1]. For example, for the COVID-19 disease the correct value is 0.024. This is not the infection rate.")
                     ),
                     textInput(
                       "beta_contact",
                       label = NULL,
                       width = "100px",
                       value = 0.024
                     ),
              ),
              column(10, offset = 1,
                     div(class = "icon-container",
                         h5(icon("info-circle"), " β", tags$sub("aerosol"), "="),
                         div(class = "icon-text", "β", tags$sub("aerosol"), "represents the <b>risk const</b> that refer to the infection due to aerosol based on the contagion model in [2]. For example, for the COVID-19 disease the correct value is 410. This is not the infection rate.")
                     ),
                     textInput(
                       "beta_aerosol",
                       label = NULL,
                       width = "100px",
                       value = 410
                     ),
              ),
              column(10, offset = 1,
                     div(class = "icon-container",
                         h5(icon("info-circle"), " γ ="),
                         div(class = "icon-text", "γ represents the recovery rate.")
                     ),
                     get_distribution_panel("gamma")
              ),
              conditionalPanel(
                condition = 'input.disease_model == "SEIR" || input.disease_model == "SEIRS" || input.disease_model == "SEIRD" || input.disease_model == "SEIRDS"',
                column(10, offset=1,
                       div(class = "icon-container",
                           h5(icon("info-circle"), " α ="),
                           div(class = "icon-text", "α represents the incubation rate.")
                       ),
                       get_distribution_panel("alpha")
                )
              ),
              conditionalPanel(
                condition = 'input.disease_model == "SIRD" || input.disease_model == "SEIRD" || input.disease_model == "SEIRDS" || input.disease_model == "SIRDS"',
                column(10, offset=1,
                       div(class = "icon-container",
                           h5(icon("info-circle"), " λ ="),
                           div(class = "icon-text", "λ represents the fatality rate.")
                       ),
                       get_distribution_panel("lambda")
                )
              ),
              conditionalPanel(
                condition = 'input.disease_model == "SIRS" || input.disease_model == "SEIRS" || input.disease_model == "SIRDS" || input.disease_model == "SEIRDS"',
                column(10, offset=1,
                       div(class = "icon-container",
                           h5(icon("info-circle"), " ν ="),
                           div(class = "icon-text", "ν represents the end-of-immunization rate.")
                       ),
                       get_distribution_panel("nu")
                )
              )
            ),
            fluidRow(
              column(2, offset=1,
                     actionButton(inputId = "save_values_disease_model", label = "Save")
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
                        label = "Save the RDs file",
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
                      column(1, offset=3, numericInput(inputId = "vaccination_time_from", label = "From (day):", value = 1, min = 1)),
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
                        textInput(inputId = "seed", label = "Seed:", placeholder = "Simulation seed", value = as.integer(as.numeric(Sys.time())))
                      ),
                      column(
                        offset = 1,
                        width = 2,
                        textInput(inputId = "simulation_days", label = "Simulation days:", placeholder = "Number of days to simulate", value = "10")
                      ),
                      column(
                        offset = 1,
                        width = 2,
                        textInput(inputId = "nrun", label = "Number of simulations:", placeholder = "Number of simulations", value = "100")
                      )
                    ),
                    fluidRow(
                      column(
                        offset = 1,
                        width = 2,
                        radioButtons(inputId = "initial_day",
                                     label = "Initial day:",
                                     choices = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                                     selected = "Monday"
                        )
                      ),
                      column(
                        offset = 1,
                        width = 1,
                        textInput(inputId = "initial_time", label = "Initial time:", placeholder = "hh:mm", value = "00:00")
                      ),
                      column(
                        offset = 2,
                        width = 2,
                        textInput(inputId = "prun", label = "Number of parallel simulations (when not using the visualisation):", value = "10")
                      )
                    ),
                    fluidRow(
                      column(offset = 1,
                             width = 2,
                             selectInput("step", div(class = "icon-container", style="margin-top:20px",
                                                     h5(tags$b("Step: "), icon("info-circle")),
                                                     div(class = "icon-text", "Duration of a FLAME GPU 2 step in seconds.")
                             ),
                             choices = c(1, 2, 3, 4, 5, 6, 10, 12, 15, 20, 30, 60),
                             selected = 10)
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
                              3,
                              offset = 1,
                              selectInput("run_type", "Select run type:", choices=c("Without Docker (with visualisation)", "Without Docker (without visualisation)", "Docker", "Slurm"), selected = "Docker")
                            )
                          ),
                          fluidRow(
                            column(2,
                                   offset = 1,
                                   actionButton("run", "Run")
                            )
                          )
                    )
                  )
              )
      ),
      tabItem(tabName = "post_process",
              title = h3("Post Processing of the simulation"),
              fluidRow(
                box(width = 12,
                    title = div(class = "icon-container", style="margin-top:20px",
                                h3("Uploading simulation ", icon("info-circle")),
                                div(class = "icon-text", "Must be the folder containing all the simulations obtained throught FLAMEGPU2.")
                    ),
                    fluidRow(
                      column(
                        width = 4,
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
                        width = 6,
                        style = "margin-top: 20px;",
                        verbatimTextOutput("dirPath")
                        # actionButton(
                        #   label = "Load",
                        #   icon = shiny::icon("upload"),
                        #   inputId = "LoadCSVsimul_Button"
                        # )
                      )
                    )
                )
              ),
              fluidRow(
                box(width = 12,collapsed = T,collapsible = T,
                    title = div(class = "icon-container", style="margin-top:20px",
                                h3("Query on Disease Status", icon("info-circle")),
                                div(class = "icon-text", "Find the simulations with defined specification on the disease.")
                    ),
                    fluidRow(
                      column(width = 6,
                             uiOutput("PostProc_filters")
                      ),
                      column(width = 5,
                             div(class = "icon-container", style="margin-top:20px",
                                 h4("Resulting Simulations", icon("info-circle")),
                                 div(class = "icon-text", "Click on the table to visualise the corresponding disease dynamics.")
                             ),
                             DT::dataTableOutput("PostProc_table")
                      )
                    ),
                    fluidRow(
                      column(10,
                             plotOutput("EvolutionPlot")
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
                             plotOutput("CountersPlot")
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
                             selectInput("Room_Counters_A_C_selectize",choices = "",label = "Choice the room:")
                      )
                    ),
                    fluidRow(
                      column(10,
                             plotOutput("A_C_CountersPlot")
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
                box(width = 12,collapsed = F,collapsible = T,
                    title = div(class = "icon-container", style="margin-top:20px",
                                h3("2D Visualisation", icon("info-circle")),
                                div(class = "icon-text", "2D visulisation of the agents moving in the modeled system")
                    ),
                    fluidRow(
                      column(2,offset = 1,
                             uiOutput("subfolderUI")
                      )
                    ),
                    fluidRow(
                      box(width = 10, status = "primary",
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
                                   selectizeInput("visualColor_select","Select colour room:", choices = c("Name", "Type", "Area","Contact","Aerosol","Comulative Aerosol"))
                            ),
                            column(2,
                                   radioButtons("visualLabel_select","Show in the room:",
                                                selected = "None",
                                                choices = c("None","Name", "Type", "Area","Agent ID"))
                            )

                          )
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
                             actionButton("next_step_visual", label = HTML("<i class='fa fa-forward'></i> Next"),
                                          class = "btn-primary")
                      ),
                      column(2,
                             numericInput("animationStep",label = "Set the animation step (sec):", value = 1, min = 1)
                      )
                    ),
                    fluidRow(
                      column(12,
                             uiOutput("TwoDMapPlots")
                      )
                    )
                )
              )
      )
      #### END tabs ####
    )
  ),
  skin = "purple"
)
