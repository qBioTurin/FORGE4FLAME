theme_fancy <- function() {
  theme_minimal() +
    theme(
      plot.background = element_rect(fill = "#2b2b2b", color = NA),
      panel.background = element_rect(fill = "#3c3c3c", color = NA),
      panel.grid.major = element_line(color = "#666666", size = 0.3),
      panel.grid.minor = element_line(color = "#444444", size = 0.2),
      axis.text = element_text(color = "white", size = 16),
      axis.title = element_text(color = "white", size = 20, face = "bold"),
      plot.title = element_text(color = "white", size = 22, face = "bold", hjust = 0.5),
      legend.background = element_rect(fill = "#2b2b2b"),
      legend.text = element_text(color = "white", size = 18),
      legend.key.size = unit(1.5, 'cm'),
      legend.key = element_rect(fill = "white"),
      legend.title = element_text(color = "white", face = "bold", size = 18),
      legend.position = "bottom",
      strip.background = element_rect(fill = "white",color = "black"),
      strip.text = element_text(color = "black", size = 18, face = "bold")
    )
}

generate_obj <- function(temp_directory){
  fileConn = file(file.path(temp_directory, 'room.obj'), 'w+')

  length = 1
  width = 1
  height = 1

  # Generate vertices
  vertices = list(
    c(0, 0, 0),
    c(length, 0, 0),
    c(0, height, 0),
    c(0, 0, width),
    c(0, height, width),
    c(length, 0, width),
    c(length, height, 0),
    c(length, height, width)
  )

  # Generate triangles
  faces = list(
    c(1, 2, 3),
    c(2, 7, 3),
    c(1, 4, 6),
    c(6, 2, 1),
    c(1, 3, 4),
    c(3, 5, 4),
    c(2, 7, 6),
    c(7, 8, 6)
  )

  for (vertex in vertices)
    writeLines(paste0("v ", vertex[1], " ", vertex[2], " ", vertex[3]), fileConn)

  for (face in faces)
    writeLines(paste0("f ", face[1], " ", face[2], " ", face[3]), fileConn)

  close(fileConn)

  fileConn = file(file.path(temp_directory, 'fillingroom.obj'), 'w+')

  # Generate vertices
  vertices = list(
    c(0, 0, 0),
    c(length, 0, 0),
    c(0, height, 0),
    c(0, 0, width),
    c(0, height, width),
    c(length, 0, width),
    c(length, height, 0),
    c(length, height, width)
  )

  # Generate triangles
  faces = list(
    c(1, 2, 3),
    c(2, 7, 3),
    c(1, 4, 6),
    c(6, 2, 1),
    c(1, 3, 4),
    c(3, 5, 4),
    c(2, 7, 6),
    c(7, 8, 6),
    c(4, 6, 5),
    c(6, 8, 5)
  )

  for (vertex in vertices)
    writeLines(paste0("v ", vertex[1], " ", vertex[2], " ", vertex[3]), fileConn)

  for (face in faces)
    writeLines(paste0("f ", face[1], " ", face[2], " ", face[3]), fileConn)

  close(fileConn)
}

find_ones_submatrix_coordinates <- function(mat, target_rows, target_cols) {
  # plus two since we have to consider the borders
  target_rows= 2 + target_rows
  target_cols= 2 + target_cols

  for (start_row in 1:(nrow(mat)-target_rows+1)) {
    for (start_col in 1:(ncol(mat)-target_cols+1)) {
      end_row <- start_row + target_rows - 1
      end_col <- start_col + target_cols - 1

      submatrix <- mat[start_row:end_row, start_col:end_col]

      if (all(submatrix == 1)) {
        return(c(start_row-1, start_col-1))
      }
    }
  }
  return(NULL)
}

CanvasToMatrix = function(canvasObjects,FullRoom = F,canvas){
  matrixCanvas = canvasObjects$matrixCanvas
  roomNames = canvasObjects$rooms


  ## wall and room id defnition
  if(!is.null(canvasObjects$roomsINcanvas)){
    rooms = canvasObjects$roomsINcanvas %>% filter(CanvasID == canvas)
    for(i in rooms$ID){
      r = rooms %>% filter(ID == i)

      x = r$x
      y = r$y

      ## wall definition as 0
      matrixCanvas[y, x + 0:(r$l+1)] = 0
      matrixCanvas[y + r$w + 1, x + 0:(r$l+1)] = 0
      matrixCanvas[y + 0:(r$w+1), x] = 0
      matrixCanvas[y + 0:(r$w+1), x+ r$l + 1] = 0

      ## inside the walls the matrix with 1
      if(FullRoom)
        matrixCanvas[y + 1:(r$w), x + 1:(r$l)] = i
      else
        matrixCanvas[y + 1:(r$w), x + 1:(r$l)] = 1

      ## door position definition as 2
      if(r$door == "top"){
        r$door_x = canvasObjects$roomsINcanvas$door_x[which(canvasObjects$roomsINcanvas$ID == i)] = r$x + floor((r$l+1)/2)
        r$door_y = canvasObjects$roomsINcanvas$door_y[which(canvasObjects$roomsINcanvas$ID == i)] = r$y
        r$center_y = canvasObjects$roomsINcanvas$center_y[which(canvasObjects$roomsINcanvas$ID == i)] = r$y + ceiling((r$w + 1) / 2)
        r$center_x = canvasObjects$roomsINcanvas$center_x[which(canvasObjects$roomsINcanvas$ID == i)] = r$x + floor((r$l+1)/2)
      }
      else if(r$door == "bottom"){
        r$door_x = canvasObjects$roomsINcanvas$door_x[which(canvasObjects$roomsINcanvas$ID == i)] = r$x + ceiling((r$l+1)/2)
        r$door_y = canvasObjects$roomsINcanvas$door_y[which(canvasObjects$roomsINcanvas$ID == i)] = r$y + r$w + 1
        r$center_y = canvasObjects$roomsINcanvas$center_y[which(canvasObjects$roomsINcanvas$ID == i)] = r$y + floor((r$w + 1) / 2)
        r$center_x = canvasObjects$roomsINcanvas$center_x[which(canvasObjects$roomsINcanvas$ID == i)] = r$x + round((r$l+1)/2)
      }
      else if(r$door == "left"){
        r$door_x = canvasObjects$roomsINcanvas$door_x[which(canvasObjects$roomsINcanvas$ID == i)] = r$x
        r$door_y = canvasObjects$roomsINcanvas$door_y[which(canvasObjects$roomsINcanvas$ID == i)] = r$y + round((r$w+1)/2)
        r$center_y = canvasObjects$roomsINcanvas$center_y[which(canvasObjects$roomsINcanvas$ID == i)] = r$y + round((r$w+1)/2)
        r$center_x = canvasObjects$roomsINcanvas$center_x[which(canvasObjects$roomsINcanvas$ID == i)] = r$x + ceiling((r$l + 1) / 2)
      }
      else if(r$door == "right"){
        r$door_x = canvasObjects$roomsINcanvas$door_x[which(canvasObjects$roomsINcanvas$ID == i)] = r$x+ r$l + 1
        r$door_y = canvasObjects$roomsINcanvas$door_y[which(canvasObjects$roomsINcanvas$ID == i)] = r$y + floor((r$w+1)/2)
        r$center_y = canvasObjects$roomsINcanvas$center_y[which(canvasObjects$roomsINcanvas$ID == i)] = r$y + floor((r$w+1)/2)
        r$center_x = canvasObjects$roomsINcanvas$center_x[which(canvasObjects$roomsINcanvas$ID == i)] = r$x + floor((r$l + 1) / 2)
      }

      matrixCanvas[r$door_y, r$door_x] = if(r$door != "none") 2 else 0
      if(r$type != "Fillingroom")
        matrixCanvas[r$center_y, r$center_x] = roomNames$ID[roomNames$Name == r$Name]
    }
  }

  ## movement node definition as 3
  if(!is.null(canvasObjects$nodesINcanvas)){
    nodes = canvasObjects$nodesINcanvas %>% filter(CanvasID == canvas)
    for(i in nodes$ID){
      r = nodes %>% filter(ID == i)
      matrixCanvas[r$y, r$x] = 3
    }
  }

  return(matrixCanvas)
}

command_addRoomObject = function(newroom){
  txt = paste0("// Crea un nuovo oggetto Square con le proprietà desiderate
                const newRoom = new Room(",newroom$ID,",",
               newroom$x*10," , ",newroom$y*10," ,",
               newroom$center_x*10,",",
               newroom$center_y*10,",",
               newroom$door_x*10,",",
               newroom$door_y*10,",",
               newroom$l*10,",",
               newroom$w*10,",",
               newroom$h,",",
               newroom$colorFill,",",
               newroom$colorBorder,", \" ",
               newroom$Name,"\" , \"",newroom$door,"\");")
  paste0(txt,"
          // Aggiungi il nuovo oggetto Square all'array arrayObject
         FloorArray[\"",newroom$CanvasID,"\"].arrayObject.push(newRoom);"
  )
}

UpdatingData = function(input,output,canvasObjects, mess,areasColor, session){
  messNames = names(mess)
  for(i in messNames)
    canvasObjects[[i]] = mess[[i]]

  ### UPDATING THE CANVAS ####
  # deleting everything from canvas
  js$clearCanvas()
  # update the canvas dimension
  js$canvasDimension(canvasObjects$canvasDimension$canvasWidth, canvasObjects$canvasDimension$canvasHeight)

  for(floor in canvasObjects$floors$Name){
    runjs(paste0("
                 FloorArray[\"", floor, "\"] = new FloorManager(\"", floor, "\");"))
  }

  selected = ""
  if(nrow(canvasObjects$floors) != 0){
    selected = canvasObjects$floors$Name[1]
  }

  if(length(canvasObjects$floors$Name)>1){
    output$FloorRank <- renderUI({
      div(
        rank_list(text = "Drag the floors in the desired order",
                  labels =  canvasObjects$floors$Name,
                  input_id = paste("list_floors")
        )
      )
    })
  }else{
    output$FloorRank <- renderUI({ NULL })
  }

  updateSelectizeInput(inputId = "canvas_selector",
                       selected = selected,
                       choices = c("", canvasObjects$floors$Name) )

  if(!is.null(canvasObjects$rooms)){
    output$length <- renderText({
      "Length of selected room (length refers to the wall with the door): "
    })

    output$width <- renderText({
      "Width of selected room: "
    })

    output$height <- renderText({
      "Height of selected room: "
    })
  }

  # draw rooms
  if(!is.null(canvasObjects$roomsINcanvas)){
    for(r_id in canvasObjects$roomsINcanvas$ID){
      newroom = canvasObjects$roomsINcanvas %>% filter(ID == r_id)
      runjs( command_addRoomObject( newroom) )
    }

    # update types
    updateSelectizeInput(inputId = "select_type",choices = unique(canvasObjects$types$Name) )
    updateSelectInput(inputId = "selectInput_color_type",
                      choices = unique(canvasObjects$types$Name))
    # update areas
    updateSelectInput(inputId = "selectInput_color_area",
                      choices = unique(canvasObjects$areas$Name))
    updateSelectizeInput(inputId = "select_area",
                         choices = unique(canvasObjects$areas$Name) )
  }
  # draw points
  if(!is.null(canvasObjects$nodesINcanvas)){
    for(r_id in canvasObjects$nodesINcanvas$ID){
      newpoint = canvasObjects$nodesINcanvas %>% filter(ID == r_id)
      runjs(paste0("// Crea un nuovo oggetto Square con le proprietà desiderate
                const newPoint = new Circle(", newpoint$ID,",", newpoint$x*10," , ", newpoint$y*10,", 5, rgba(0, 127, 255, 1));
                // Aggiungi il nuovo oggetto Square all'array arrayObject
                FloorArray[\"",newpoint$CanvasID,"\"].arrayObject.push(newPoint);"))
    }
  }

  updateSelectizeInput(inputId = "id_new_agent", choices = if(!is.null(canvasObjects$agents)) unique(names(canvasObjects$agents)) else "", selected = "")
  updateSelectizeInput(inputId = "id_agents_to_copy", choices = if(!is.null(canvasObjects$agents)) unique(names(canvasObjects$agents)) else "", selected = "")

  classes <- c()
  for(i in 1:length(canvasObjects$agents)){
    classes <- c(canvasObjects$agents[[i]]$Class, classes)
  }

  updateSelectizeInput(inputId = "id_class_agent", choices = if(length(classes) > 0) unique(classes) else "")

  selected = "SIR"
  if(!is.null(canvasObjects$disease)){
    selected = canvasObjects$disease$Name

    updateTextInput(session, inputId = "beta_aerosol", value=canvasObjects$disease$beta_aerosol)
    updateTextInput(session, inputId = "beta_contact", value=canvasObjects$disease$beta_contact)

    params <- parse_distribution(canvasObjects$disease$gamma_time, canvasObjects$disease$gamma_dist)
    gamma_dist <- canvasObjects$disease$gamma_dist
    gamma_a <- params[[1]]
    gamma_b <- params[[2]]
    tab <- if(gamma_dist == "Deterministic") "DetTime_tab" else "StocTime_tab"

    update_distribution("gamma", gamma_dist, gamma_a, gamma_b, tab)


    if(grepl("E", selected)){
      params <- parse_distribution(canvasObjects$disease$alpha_time, canvasObjects$disease$alpha_dist)
      alpha_dist <- canvasObjects$disease$alpha_dist
      alpha_a <- params[[1]]
      alpha_b <- params[[2]]
      tab <- if(alpha_dist == "Deterministic") "DetTime_tab" else "StocTime_tab"


      update_distribution("alpha", alpha_dist, alpha_a, alpha_b, tab)
    }


    if(grepl("D", selected)){
      params <- parse_distribution(canvasObjects$disease$lambda_time, canvasObjects$disease$lambda_dist)
      lambda_dist <- canvasObjects$disease$lambda_dist
      lambda_a <- params[[1]]
      lambda_b <- params[[2]]
      tab <- if(lambda_dist == "Deterministic") "DetTime_tab" else "StocTime_tab"

      update_distribution("lambda", lambda_dist, lambda_a, lambda_b, tab)
    }


    if(grepl("^([^S]*S[^S]*S[^S]*)$", selected[length(selected)])){
      params <- parse_distribution(canvasObjects$disease$nu_time, canvasObjects$disease$nu_dist)
      nu_dist <- canvasObjects$disease$nu_dist
      nu_a <- params[[1]]
      nu_b <- params[[2]]
      tab <- if(nu_dist == "Deterministic") "DetTime_tab" else "StocTime_tab"

      update_distribution("nu", nu_dist, nu_a, nu_b, tab)
    }
  }

  updateSelectizeInput(inputId = "disease_model",
                       selected = selected)

  updateTextInput(session, inputId = "seed", value = canvasObjects$starting$seed)
  updateRadioButtons(session, inputId = "initial_day", selected = canvasObjects$starting$day)
  updateTextInput(session, inputId = "nrun", value = canvasObjects$starting$nrun)
  updateTextInput(session, inputId = "initial_time", value = canvasObjects$starting$time)
  updateTextInput(session, inputId = "simulation_days", value = canvasObjects$starting$simulation_days)
  updateSelectizeInput(session, inputId = "step", choices = c(1, 2, 3, 4, 5, 6, 10, 12, 15, 20, 30, 60), selected = as.numeric(canvasObjects$starting$step))


  rooms = canvasObjects$roomsINcanvas %>% filter(type != "Fillingroom", type != "Stair")
  roomsAvailable = c("", unique(paste0( rooms$type,"-", rooms$area) ) )
  updateSelectizeInput(session = session, "room_ventilation",
                       choices = roomsAvailable, selected = "")

  hideElement("outside_contagion_plot")

  if(!is.null(canvasObjects$outside_contagion)){
    output$outside_contagion_plot <- renderPlot({
      ggplot(canvasObjects$outside_contagion) +
        geom_line(aes(x=day, y=percentage_infected), color="green") +
        ylim(0, NA) +
        labs(title = "Outside contagion", x = "Day", y = "Percentage") +
        theme(title = element_text(size = 34), axis.title = element_text(size = 26), axis.text = element_text(size = 22)) +
        theme_fancy()
    })

    showElement("outside_contagion_plot")
  }
  else{
    hideElement("outside_contagion_plot")
  }

  # Resources
  if(!is.null(canvasObjects$agents)){
    allResRooms <- do.call(rbind,
              lapply(names(canvasObjects$agents), function(agent) {
                rooms = unique(c(canvasObjects$agents[[agent]]$DeterFlow$Room,
                                 canvasObjects$agents[[agent]]$RandFlow$Room))
                rooms <- rooms[rooms != "Do nothing"]
                if(length(rooms)>0)
                  data.frame(Agent = agent , Room =  rooms)
                else NULL
              })
      )

    updateSelectizeInput(session = session, "selectInput_alternative_resources_global", choices = if(!is.null(allResRooms)) allResRooms$Room else "")

    choices <- unique( allResRooms$Room )
    choices <- choices[!grepl(paste0("Spawnroom", collapse = "|"), choices)]
    choices <- choices[!grepl(paste0("Stair", collapse = "|"), choices)]

    updateSelectizeInput(session, "selectInput_resources_type", choices = choices, selected= "", server = TRUE)
  }
  else{
    updateSelectizeInput(session, "selectInput_resources_type", choices = "", selected= "", server = TRUE)
  }

  "The file has been uploaded with success!"
}

UpdatingTimeSlots_tabs = function(input,output,canvasObjects, InfoApp, session, ckbox_entranceFlow){
  Agent = input$id_new_agent
  EntryExitTime= canvasObjects$agents[[Agent]]$EntryExitTime
  FlowID = canvasObjects$agents[[Agent]]$DeterFlow$FlowID
  entry_type = canvasObjects$agents[[Agent]]$entry_type

  NumTabs = InfoApp$NumTabsTimeSlot
  #if i change type from one agent to another I have to remove all tabs type
  if(length(NumTabs) > 0){
    #if it's the first agent ever we click on we remove the default void slot
    if(InfoApp$oldAgentType == ""){
      removeTab(inputId = "Rate_tabs", target = "1 slot")
      removeTab(inputId = "Time_tabs", target = "1 slot")

    }
    if(InfoApp$oldAgentType == "Time window"){
      for( i in NumTabs) {
        removeTab(inputId = "Time_tabs", target = paste0(i, " slot"))
      }
    }
    else if(InfoApp$oldAgentType == "Daily Rate"){
      for( i in NumTabs) {
        removeTab(inputId = "Rate_tabs", target = paste0(i, " slot"))
      }
    }
  }

  InfoApp$NumTabsTimeSlot = numeric(0)

  if((is.null(EntryExitTime) || nrow(EntryExitTime) == 0) && ckbox_entranceFlow == "Daily Rate"){
    appendTab(inputId = "Rate_tabs",
              tabPanel(paste0(1," slot"),
                       value = paste0(1," slot"),
                       tags$b("Entrance rate:"),
                       get_distribution_panel(paste0("daily_rate_", 1)),
                       column(7,
                              textInput(inputId = "EntryTimeRate_1", label = "Initial generation time:", placeholder = "hh:mm"),
                              textInput(inputId = "ExitTimeRate_1", label = "Final generation time:", placeholder = "hh:mm"),
                       ),
                       column(5,
                              checkboxGroupInput("selectedDaysRate_1", "Select Days of the Week",
                                                 choices = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                                                 selected = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
                              )

                       )
              )
    )
    InfoApp$NumTabsTimeSlot = 1
    showTab(inputId = "Rate_tabs", target = paste0(1, " slot"), select = T)

    updateTextInput(inputId = "num_agent", value = 0)
    disable("num_agent")
  }else if((is.null(EntryExitTime) || nrow(EntryExitTime) == 0) && ckbox_entranceFlow == "Time window"){
    appendTab(inputId = "Time_tabs",
              tabPanel(paste0(1," slot"),
                       value = paste0(1," slot"),
                       column(7,
                              textInput(inputId = "EntryTime_1", label = "Entry time:", placeholder = "hh:mm"),
                              if(length(FlowID)>0){
                                selectInput(inputId = paste0("Select_TimeDetFlow_",length(FlowID)),
                                            label = "Associate with a determined flow:",
                                            choices = sort(unique(FlowID)) )
                              }else{
                                selectInput(inputId = paste0("Select_TimeDetFlow_",1),
                                            label = "Associate with a determined flow:",
                                            choices = "1 flow")
                              }
                       ),
                       column(5,
                              checkboxGroupInput("selectedDays_1", "Select Days of the Week",
                                                 choices = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                                                 selected = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
                              )

                       )
              )
    )
    InfoApp$NumTabsTimeSlot = 1
    showTab(inputId = "Time_tabs", target = paste0(1, " slot"), select = T)

    enable("num_agent")
  }else if((!is.null(EntryExitTime) || nrow(EntryExitTime) > 0) && ckbox_entranceFlow == "Time window"){
      updateRadioButtons(session, "ckbox_entranceFlow", selected = "Time window")

      slots = sort(unique(gsub(pattern = " slot", replacement = "", x = EntryExitTime$Name)))
      for(i in (slots) ){
        InfoApp$NumTabsTimeSlot = c(InfoApp$NumTabsTimeSlot,i)
        df = EntryExitTime %>% filter(Name ==paste0(i, " slot"))

        appendTab(inputId = "Time_tabs",
                  tabPanel(paste0(i," slot"),
                           value = paste0(i," slot"),
                           column(7,
                                  textInput(inputId = paste0("EntryTime_",i), label = "Entry time:", value = unique(df$EntryTime), placeholder = "hh:mm"),
                                  selectInput(inputId = paste0("Select_TimeDetFlow_",i),
                                              label = "Associate with a determined flow:",
                                              selected = unique(df$FlowID),
                                              choices = sort(unique(FlowID)))
                           ),
                           column(5,
                                  checkboxGroupInput(paste0("selectedDays_",i), "Select Days of the Week",
                                                     choices = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                                                     selected = df$Days
                                  )
                           )
                  )
        )
      }
      showTab(inputId = "Time_tabs", target = paste0(slots[1], " slot"), select = T)
      enable("num_agent")
    } else if((!is.null(EntryExitTime) || nrow(EntryExitTime) > 0) && ckbox_entranceFlow == "Daily Rate"){
      updateRadioButtons(session, "ckbox_entranceFlow", selected = "Daily Rate")

      slots = sort(unique(gsub(pattern = " slot", replacement = "", x = EntryExitTime$Name)))
      tab <- "DetTime_tab"
      for(i in (slots) ){
        InfoApp$NumTabsTimeSlot = c(InfoApp$NumTabsTimeSlot,i)
        df = EntryExitTime %>% filter(Name ==paste0(i, " slot"))


        params <- parse_distribution(unique(df$RateTime), unique(df$RateDist))
        rate_dist <- unique(df$RateDist)
        rate_a <- params[[1]]
        rate_b <- params[[2]]
        if(i == min(slots))
          tab <- if(rate_dist == "Deterministic") "DetTime_tab" else "StocTime_tab"

        appendTab(inputId = "Rate_tabs",
                  tabPanel(paste0(i," slot"),
                           value = paste0(i," slot"),
                           tags$b("Entrance rate:"),
                           get_distribution_panel(paste0("daily_rate_", i), a=rate_a, b=rate_b, selected_dist = rate_dist),
                           column(7,
                                  textInput(inputId = paste0("EntryTimeRate_",i), label = "Initial generation time:", value = unique(df$EntryTime), placeholder = "hh:mm"),
                                  textInput(inputId = paste0("ExitTimeRate_",i), label = "Final generation time:", value = unique(df$ExitTime), placeholder = "hh:mm"),
                           ),
                           column(5,
                                  checkboxGroupInput(paste0("selectedDaysRate_",i), "Select Days of the Week",
                                                     choices = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                                                     selected = df$Days
                                  )
                           )
                  )
        )

        # update_distribution(paste0("daily_rate_", i), rate_dist, rate_a, rate_b, tab)
      }
      showTab(inputId = "Rate_tabs", target = paste0(1, " slot"), select = T)
      showTab(inputId = paste0("DistTime_tabs_daily_rate_", slots[1]), target = tab, select = T)
      # if(tab == "StocTime_tab")
      #   updateSelectInput(inputId = paste0("DistStoc_id_daily_rate_", slots[1]), selected = rate_dist)

      updateTextInput(inputId = "num_agent", value = 0)
      disable("num_agent")
    }
}

get_distribution_panel = function(id, a = "", b = "", selected_dist = ""){
  dist_panel <-  tagList(
    div(style = "height:20px"),
    tabsetPanel(id = paste0("DistTime_tabs_", id),
                             tabPanel("Deterministic",
                                      value = "DetTime_tab",
                                      textInput(inputId = paste0("DetTime_", id), label = HTML("<i>Fixed deterministic value:</i>"),placeholder = "Value", value = a)
                             ),
                             tabPanel("Stochastic",
                                      value = "StocTime_tab",
                                      selectizeInput(inputId = paste0("DistStoc_id_", id),
                                                     label = HTML("<i>Distribution:</i>"),
                                                     choices = c("Exponential","Uniform","Truncated Positive Normal"),
                                                     selected = selected_dist),
                                      conditionalPanel(
                                        condition = paste0("input.DistStoc_id_", id, " == 'Exponential'"),
                                        textInput(inputId = paste0("DistStoc_ExpRate_", id),
                                                 label = HTML("<i>Value:</i>"),
                                                 placeholder = "Value",
                                                 value = a)

                                      ),
                                      conditionalPanel(
                                        condition = paste0("input.DistStoc_id_", id, " == 'Uniform'"),
                                        fluidRow(
                                          column(width = 4,
                                                 textInput(inputId = paste0("DistStoc_UnifRate_a_", id), label = "a:", placeholder = "Value", value = a)
                                          ),
                                          column(width = 4,
                                                 textInput(inputId = paste0("DistStoc_UnifRate_b_", id), label = "b:", placeholder = "Value", value = b)

                                          )
                                        )
                                      ),
                                      conditionalPanel(
                                        condition = paste0("input.DistStoc_id_", id, " == 'Truncated Positive Normal'"),
                                        fluidRow(
                                          column(width = 4,
                                                 textInput(inputId = paste0("DistStoc_NormRate_m_", id), label = "Mean:", placeholder = "Value", value = a)
                                          ),
                                          column(width = 4,
                                                 textInput(inputId = paste0("DistStoc_NormRate_sd_", id), label = "Sd:", placeholder = "Value", value = b)

                                          )
                                         )
                                       )
                             )
                 ),
    div(style = "height:10px")
)
  return(dist_panel)
}

check_distribution_parameters <- function(input, suffix){
  if(input[[paste0("DistTime_tabs_", suffix)]] == "DetTime_tab"){
    if(input[[paste0("DetTime_", suffix)]] == "")
      return(list(NULL, NULL))

    if(is.na(as.numeric(gsub(",", "\\.", input[[paste0("DetTime_", suffix)]]))) || as.numeric(gsub(",", "\\.", input[[paste0("DetTime_", suffix)]])) <= 0){
      print(as.numeric(gsub(",", "\\.", input[[paste0("DetTime_", suffix)]])))
      shinyalert("You must specify a time > 0 (in minutessssss).")
      return(list(NULL, NULL))
    }
    new_time = input[[paste0("DetTime_", suffix)]]
    new_dist = "Deterministic"
  }else if(input[[paste0("DistTime_tabs_", suffix)]] == "StocTime_tab"){
    new_dist = input[[paste0("DistStoc_id_", suffix)]]

    if(input[[paste0("DistStoc_id_", suffix)]] == 'Exponential'){
      if(input[[paste0("DistStoc_ExpRate_", suffix)]] == "")
        return(list(NULL, NULL))

      if(is.na(as.numeric(gsub(",", "\\.", input[[paste0("DistStoc_ExpRate_", suffix)]]))) || as.numeric(gsub(",", "\\.", input[[paste0("DistStoc_ExpRate_", suffix)]])) <= 0 ){
        shinyalert("You must specify a time > 0 (in minutes).")
        return(list(NULL, NULL))
      }
      new_time = input[[paste0("DistStoc_ExpRate_", suffix)]]
    }else if(input[[paste0("DistStoc_id_", suffix)]]== 'Uniform'){
      if(input[[paste0("DistStoc_UnifRate_a_", suffix)]] == "" || input[[paste0("DistStoc_UnifRate_b_", suffix)]] == "")
        return(list(NULL, NULL))

      if( is.na(as.numeric(gsub(",", "\\.", input[[paste0("DistStoc_UnifRate_a_", suffix)]]))) ||
          is.na(as.numeric(gsub(",", "\\.", input[[paste0("DistStoc_UnifRate_b_", suffix)]]))) ||
          as.numeric(gsub(",", "\\.", input[[paste0("DistStoc_UnifRate_a_", suffix)]])) >= as.numeric(gsub(",", "\\.", input[[paste0("DistStoc_UnifRate_b_", suffix)]])) ||
          as.numeric(input[[paste0("DistStoc_UnifRate_a_", suffix)]]) <= 0 || as.numeric(input[[paste0("DistStoc_UnifRate_b_", suffix)]]) <= 0){
        shinyalert("You must specify a and b as numeric, a < b (in minutes and both > 0).")
        return(list(NULL, NULL))
      }
      new_time = paste0("a = ",input[[paste0("DistStoc_UnifRate_a_", suffix)]] ,"; b = ",input[[paste0("DistStoc_UnifRate_b_", suffix)]])
    }else if(input[[paste0("DistStoc_id_", suffix)]] == 'Truncated Positive Normal'){
      if(input[[paste0("DistStoc_NormRate_m_", suffix)]] == "" || input[[paste0("DistStoc_NormRate_sd_", suffix)]] == "")
        return(list(NULL, NULL))

      if( is.na(as.numeric(gsub(",", "\\.", input[[paste0("DistStoc_NormRate_m_", suffix)]]))) ||
          is.na(as.numeric(gsub(",", "\\.", input[[paste0("DistStoc_NormRate_m_", suffix)]]))) ||
          as.numeric(input[[paste0("DistStoc_NormRate_m_", suffix)]]) <= 0 || as.numeric(input[[paste0("DistStoc_NormRate_sd_", suffix)]]) < 0){
        shinyalert("You must specify the mean and standard deviation as numeric (in minutes, with mean > 0 and std >= 0).")
        return(list(NULL, NULL))
      }
      new_time = paste0("Mean = ",input[[paste0("DistStoc_NormRate_m_", suffix)]] ,"; Sd = ",input[[paste0("DistStoc_NormRate_sd_", suffix)]])
    }
  }

  return(list(new_dist, new_time))
}

parse_distribution <- function(time, dist){
  # Deterministic or exponential: n
  a <- time
  b <- 0.0

  # Uniform: a = n; b = m
  # Truncated Positive Normal: Mean = n; Sd = m
  if(dist == 'Uniform' || dist == 'Truncated Positive Normal'){
    params <- str_split(time, ";")
    a <- params[[1]][1]
    b <- params[[1]][2]

    a = str_split(a, "=")[[1]][2]
    b = str_split(b, "=")[[1]][2]
  }

  return(list(as.double(gsub(",", "\\.", a)), as.double(gsub(",", "\\.", b))))
}

update_distribution <- function(id, dist, a, b, tab){
  showTab(inputId = paste0("DistTime_tabs_", id), target = tab, select = T)
  if(tab == "StocTime_tab")
    updateSelectInput(inputId = paste0("DistStoc_id_", id), selected = dist)

  if(dist == "Deterministic"){
    updateTextInput(inputId = paste0("DetTime_", id), value = a)
  }
  else if(dist == "Exponential"){
    updateSelectizeInput(inputId = paste0("DistStoc_id_", id), selected = "Exponential")
    updateTextInput(inputId = paste0("DistStoc_ExpRate_", id), value = a)
  }
  else if(dist == "Uniform"){
    updateSelectizeInput(inputId = paste0("DistStoc_id_", id), selected = "Uniform")
    updateTextInput(inputId = paste0("DistStoc_UnifRate_a_", id), value = a)
    updateTextInput(inputId = paste0("DistStoc_UnifRate_b_", id), value = b)
  }
  else if(dist == "Truncated Positive Normal"){
    updateSelectizeInput(inputId = paste0("DistStoc_id_", id), selected = "Truncated Positive Normal")
    updateTextInput(inputId = paste0("DistStoc_NormRate_m_", id), value = a)
    updateTextInput(inputId = paste0("DistStoc_NormRate_sd_", id), value = b)
  }
}

FromToMatrices.generation = function(WHOLEmodel){
  maxN = as.numeric(WHOLEmodel$starting$simulation_days)

  ## defualt values
  default_params = data.frame(
    Measure = "Ventilation",
    Type = "Global",
    Parameters = "0",
    From = 1,
    To = maxN,
    stringsAsFactors = FALSE
  )

  WHOLEmodel$rooms_whatif = rbind(default_params, WHOLEmodel$rooms_whatif)
  # ## rooms_whatif as in the OLD version
  # rooms_whatif = WHOLEmodel$rooms_whatif  %>% distinct() %>%
  #   tidyr::spread(value = "Parameters", key = "Measure") %>% select(-From, -To)
  # if(length(unique(rooms_whatif$Type)) > 1){
  #   split = str_split(rooms_whatif$Type,pattern = "-")%>%
  #     as.data.frame() %>%
  #     t %>%
  #     data.frame(stringsAsFactors = F)
  #   rooms_whatif$Type= split %>% pull(1)
  #   rooms_whatif$Area= split %>% pull(2)
  # }

  MeasuresFromTo <- NULL
  ## From_to matrix generation rooms
  if(!is.null(WHOLEmodel$roomsINcanvas)){
    rooms = WHOLEmodel$roomsINcanvas %>% mutate(Type= paste0(type,"-",area)) %>% select(Type) %>% distinct() %>% pull()
    rooms_fromto= matrix(0,ncol = maxN, nrow = length(rooms), dimnames = list(rooms = rooms, days= 1:maxN))

    MeasuresFromTo = lapply( unique(WHOLEmodel$rooms_whatif$Measure),function(m,fromto){
      rooms_whatif = WHOLEmodel$rooms_whatif %>% filter(Measure == m)

      global = rooms_whatif %>% filter(Type == "Global")
      if(dim(global)[1] >0){
        for(i in seq_along(global[,1])){
          glob_specific = global[i,]
          fromto[,glob_specific$From:glob_specific$To] = glob_specific$Parameters
        }
      }

      room_specific = rooms_whatif %>% filter(Type != "Global")
      if(dim(room_specific)[1] >0){
        for(i in seq_along(room_specific[,1])){
          r_specific = room_specific[i,]
          fromto[r_specific$Type,r_specific$From:r_specific$To] = r_specific$Parameters
        }
      }
      fromto = cbind(rooms,fromto) # put rooms name as first column
      return(fromto)
    },fromto = rooms_fromto)
    names(MeasuresFromTo) = unique(WHOLEmodel$rooms_whatif$Measure)
  }

  AgentMeasuresFromTo <- NULL
  initial_infected <- NULL
  ## From_to matrix generation Agents
  if(!is.null(WHOLEmodel$agents)){
    agents = names( WHOLEmodel$agents )
    agents_fromto= matrix(0,ncol = maxN, nrow = length(agents), dimnames = list(agents = agents, days= 1:maxN))

    agent_default <- data.frame(
      Measure = c("Mask","Vaccination","Swab","Quarantine","External screening"),
      Type = "Global",
      Parameters = c( "Type: No mask; Fraction: 0",
                      "Efficacy: 1; Fraction: 0; Coverage Dist.Days: Deterministic, 0, 0",
                      "Sensitivity: 1; Specificity: 1; Dist: No swab, 0, 0 ",
                      "Dist.Days: No quarantine, 0, 0; Q.Room: Spawnroom-None; Sensitivity: 1; Specificity: 1; Dist: No swab, 0, 0 ",
                      "First: 0; Second: 0" ),
      From = 1,
      To = WHOLEmodel$starting$simulation_days,
      stringsAsFactors = FALSE
    )

    WHOLEmodel$agents_whatif = rbind(agent_default,WHOLEmodel$agents_whatif)
    AgentMeasuresFromTo = lapply( unique(WHOLEmodel$agents_whatif$Measure),function(m,fromto){
      agents_whatif = WHOLEmodel$agents_whatif %>% filter(Measure == m) %>% rename(Name = Type)

      # parsing the parameters
      params = str_split(agents_whatif[,"Parameters"],pattern = "; ")%>%
        as.data.frame() %>%
        t %>%
        data.frame(stringsAsFactors = F)

      colnames(params)= str_split(params[1,],pattern = ": ")%>%
        as.data.frame() %>%
        t %>%
        data.frame(stringsAsFactors = F) %>% pull(1)
      rownames(params) = NULL

      for(j in 1:nrow(params))
        params[j,] = gsub(x = params[j,],replacement = "",pattern = paste0(paste0(colnames(params),": "),collapse = "||"))

      agents_whatif = cbind(agents_whatif %>% select(-Parameters), params)

      fromto = lapply(names(params),function(i,fromto_p){
        a_specific = agents_whatif[,c("Name","From","To",i) ]

        global = a_specific %>% filter(Name == "Global")
        if(dim(global)[1] >0){
          for(ii in seq_along(global[,1])){
            glob_specific = global[ii,]
            fromto_p[,glob_specific$From:glob_specific$To] = glob_specific[,i]
          }
        }

        agent_specific = a_specific %>% filter(Name != "Global")
        if(dim(agent_specific)[1] >0){
          for(ii in seq_along(agent_specific[,1])){
            specific = agent_specific[ii,]
            fromto_p[specific$Name,specific$From:specific$To] = specific[,i]
          }
        }
        fromto_p = cbind(agents,fromto_p) # put agents name as first column
        return(fromto_p)
      },fromto_p = fromto)
      names(fromto)= names(params)

      return(fromto)
    },fromto = agents_fromto)

    names(AgentMeasuresFromTo) = unique(WHOLEmodel$agents_whatif$Measure)

    # set initial infected agents as default zero
    initial_infected <- data.frame(Agent = c(agents, "Random"), Number = c(rep(0, length(agents)), 0))

    # Process "Global" infection values
    global <- WHOLEmodel$initial_infected %>% filter(Type == "Global")
    if (nrow(global) > 0) {
      global <- global[1,]  # Ensure single row
      initial_infected$Number <- global$Number  # Apply globally
    }

    # Process specific agent types
    agent_specific <- WHOLEmodel$initial_infected %>% filter(!Type %in% c("Random", "Global"))
    if (nrow(agent_specific) > 0) {
      for (ii in seq_len(nrow(agent_specific))) {
        agent_name <- agent_specific$Type[ii]
        index <- which(initial_infected$Agent == agent_name)
        if (length(index) > 0) {
          initial_infected$Number[index] <- agent_specific$Number[ii]
        }
      }
    }

    # Process "Random" infection values
    random <- WHOLEmodel$initial_infected %>% filter(Type == "Random")
    if (nrow(random) > 0) {
      random <- random[1,]  # Ensure single row
      index <- which(initial_infected$Agent == "Random")
      initial_infected$Number[index] <- random$Number
    }

    initial_infected <- as.matrix(initial_infected)
  }

  ####
  return(list(AgentMeasuresFromTo = AgentMeasuresFromTo,
              RoomsMeasuresFromTo = MeasuresFromTo,
              initial_infected = initial_infected))
}

check_overlaps <- function(entry_exit_df, deter_flow_df) {
  # Function to calculate mean time based on distribution
  get_mean_time <- function(dist_type, time_value) {
    if (dist_type == "Deterministic") {
      return(as.numeric(time_value))  # Exact time
    } else if (dist_type == "Exponential") {
      return(as.numeric(time_value))  # Mean of exponential (1/lambda = time_value)
    } else if (dist_type == "Uniform") {
      values <- as.numeric(str_extract_all(time_value, "\\d+\\.?\\d*")[[1]])
      return((values[1] + values[2]) / 2)  # Uniform mean
    } else if (dist_type == "Truncated Positive Normal") {
      values <- as.numeric(str_extract_all(time_value, "\\d+\\.?\\d*")[[1]])
      return(values[1])  # Mean value
    }
  }

  # Merge datasets on FlowID
  merged_df <- entry_exit_df %>%
    inner_join(deter_flow_df, by = "FlowID") %>%
    mutate(
      EntryTime = as.numeric(str_split(EntryTime, ":")[[1]][1]) * 60 + as.numeric(str_split(EntryTime, ":")[[1]][2]),  # Convert EntryTime to time format
      MeanTime = mapply(get_mean_time, Dist, Time) * 60  # Convert minutes to seconds
    ) %>%
    group_by(Name.x, FlowID, Days) %>%
    mutate(CumulativeMeanTime = cumsum(MeanTime)) %>%
    summarise(
      EntryTime = min(EntryTime),  # Take the earliest entry time for the group
      TotalTime = sum(MeanTime),   # Total time spent in activities
      LastTime = EntryTime + max(CumulativeMeanTime),  # Final time after all activities
      .groups = "drop"
    )

  # Check for overlaps
  overlaps <- merged_df %>%
    group_by(Days) %>%
    filter(EntryTime < lag(LastTime, default = first(EntryTime)))

  if (nrow(overlaps) > 0) {
    return(overlaps)  # Return the overlapping entries
  } else {
    return(NULL)
  }
}

library(parallel)

parallel_search_directory <- function(start_path, dir_name, n_cores = detectCores() - 1) {
  all_dirs <- list.dirs(start_path, recursive = TRUE)

  # Split directories into chunks for parallel processing
  dir_chunks <- split(all_dirs, sort(rep(1:n_cores, length.out = length(all_dirs))))

  # Parallel search using mclapply
  matches <- mclapply(dir_chunks, function(dirs) {
    grep(paste0("/", dir_name, "$"), dirs, value = TRUE)
  }, mc.cores = n_cores)

  return(unlist(matches))
}

F4FgetVolumes=function(exclude){
  osSystem <- Sys.info()["sysname"]
  if (osSystem == "Darwin") {
    volumes <- fs::dir_ls("/Volumes")
    names(volumes) <- basename(volumes)
  }
  else if (osSystem == "Linux") {
    volumes <- c(Computer = "/")
    if (isTRUE(dir_exists("/media"))) {
      media <- dir_ls("/media")
      names(media) <- basename(media)
      volumes <- c(volumes, media)
    }
  }
  else if (osSystem == "Windows") {
    wmic <- paste0(Sys.getenv("SystemRoot"), "\\System32\\Wbem\\WMIC.exe")
    if (!file.exists(wmic)) {
      volumes_info <- system2("powershell", "$dvr=[System.IO.DriveInfo]::GetDrives();Write-Output $dvr.length $dvr.name $dvr.VolumeLabel;",
                              stdout = TRUE)
      num = as.integer(volumes_info[1])
      if (num == 0)
        return(NULL)
      mat <- matrix(volumes_info[-1], nrow = num, ncol = 2)
      mat[, 1] <- gsub(":\\\\$", ":/", mat[, 1])
      sel <- mat[, 2] == ""
      mat[sel, 2] <- mat[sel, 1]
      volumes <- mat[, 1]
      volNames <- mat[, 2]
      volNames <- paste0(volNames, " (", gsub(":/$", ":",
                                              volumes), ")")
    }
    else {
      volumes <- system(paste(wmic, "logicaldisk get Caption"),
                        intern = TRUE, ignore.stderr = TRUE)
      volumes <- sub(" *\\r$", "", volumes)
      keep <- !tolower(volumes) %in% c("caption", "")
      volumes <- volumes[keep]
      volNames <- system(paste(wmic, "/FAILFAST:1000 logicaldisk get VolumeName"),
                         intern = TRUE, ignore.stderr = TRUE)
      volNames <- sub(" *\\r$", "", volNames)
      volNames <- volNames[keep]
      volNames <- paste0(volNames, ifelse(volNames == "",
                                          "", " "))
      volNames <- paste0(volNames, "(", volumes, ")")
    }
    names(volumes) <- volNames
    volumes <- gsub(":$", ":/", volumes)
  }
  else {
    stop("unsupported OS")
  }
  if (!is.null(exclude)) {
    volumes <- volumes[!names(volumes) %in% exclude]
  }
  volumes
}


check <- function(canvasObjects, input, output){
  show_modal_spinner()

  if(is.null(canvasObjects$agents) || length(canvasObjects$agents) == 0){
    shinyalert(paste0("No agent is defined."))
    remove_modal_spinner()
    return()
  }

  if(is.null(canvasObjects$rooms) || length(canvasObjects$rooms) == 0){
    shinyalert(paste0("No room is defined."))
    remove_modal_spinner()
    return()
  }

  if(is.null(canvasObjects$roomsINcanvas) || length(canvasObjects$roomsINcanvas) == 0){
    shinyalert(paste0("No room is drew in the canvas."))
    remove_modal_spinner()
    return()
  }

  spawnroom <- canvasObjects$roomsINcanvas %>%
    filter(type == "Spawnroom")

  if(nrow(spawnroom) != 1){
    shinyalert(paste0("There must be exactly one Spawnroom in the canvas."))
    remove_modal_spinner()
    return()
  }

  rooms <- canvasObjects$roomsINcanvas %>%
    filter(!type %in% c("Spawnroom", "Fillingroom", "Stair", "Waitingroom"))

  if(nrow(rooms) < 1){
    shinyalert(paste0("There must at least one room in the canvas with a type different from Spawnroom, Fillingroom, Stair, and Waitingroom."))
    remove_modal_spinner()
    return()
  }

  for(agent in 1:length(canvasObjects$agents)){
    if(is.null(canvasObjects$agents[[agent]]$DeterFlow) || nrow(canvasObjects$agents[[agent]]$DeterFlow) == 0){
      shinyalert(paste0("No determined flow is defined for the agent ", names(canvasObjects$agents)[[agent]], "."))
      remove_modal_spinner()
      return()
    }

    for(df in 1:length(unique(canvasObjects$agents[[agent]]$DeterFlow$FlowID))){
      df_local <- canvasObjects$agents[[agent]]$DeterFlow %>%
        filter(FlowID == unique(canvasObjects$agents[[agent]]$DeterFlow$FlowID)[df])

      rooms_type <- unique(df_local$Room)

      if(length(rooms_type) <= 1){
        shinyalert(paste0("The flow ", df, " of agent ", names(canvasObjects$agents)[[agent]], " has less then two rooms' types. The first and last rooms must be the Spawnroom with at least another type of room in the middle."))
        remove_modal_spinner()
        return()
      }

      if(!("Spawnroom" == strsplit(df_local$Room[1], "-")[[1]][1]) || !("Spawnroom" == strsplit(df_local$Room[nrow(df_local)], "-")[[1]][1])){
        shinyalert(paste0("The first and/or the last rooms of agent ", names(canvasObjects$agents)[[agent]], ", flow ", df, " are not a Spawnroom."))
        remove_modal_spinner()
        return()
      }

      df_local$Time[nrow(df_local)] <- 0
      label <- strsplit(df_local$Label[nrow(df_local)], "-")[[1]]
      df_local$Label[nrow(df_local)] <- paste0(label[1], " - ", label[2], " - 0 min - ", label[4])
    }

    if(is.null(canvasObjects$agents[[agent]]$EntryExitTime) || nrow(canvasObjects$agents[[agent]]$EntryExitTime) == 0){
      shinyalert(paste0("No entry flow is defined for the agent ", names(canvasObjects$agents)[[agent]], "."))
      remove_modal_spinner()
      return()
    }

    if(canvasObjects$agents[[agent]]$entry_type != "Daily Rate"){
      for(df in 1:length(unique(canvasObjects$agents[[agent]]$EntryExitTime$FlowID))){
        # Sovrapposition check
        overlaps <- check_overlaps(canvasObjects$agents[[agent]]$EntryExitTime, canvasObjects$agents[[agent]]$DeterFlow)
        if(!is.null(overlaps)){
          shinyalert(paste0("There is a sovrapposition in the definition of the entry flow for the agent ", names(canvasObjects$agents)[[agent]], "."))
          remove_modal_spinner()
          return()
        }
      }
    }
  }

  if(is.null(canvasObjects$disease$beta_contact)){
    shinyalert("You must insert the beta contact parameter (in the Infection page).")
    remove_modal_spinner()
    return()
  }

  if(is.null(canvasObjects$disease$beta_aerosol)){
    shinyalert("You must insert the beta aerosol parameter (in the Infection page).")
    remove_modal_spinner()
    return()
  }

  if(is.null(canvasObjects$disease$gamma_time)){
    shinyalert("You must insert the gamma parameter (in the Infection page).")
    remove_modal_spinner()
    return()
  }

  if(grepl("E", canvasObjects$disease$Name)){
    if(is.null(canvasObjects$disease$alpha_time)){
      shinyalert("You must insert the alpha parameter (in the Infection page).")
      remove_modal_spinner()
      return()
    }
  }


  if(grepl("D", canvasObjects$disease$Name)){
    if(is.null(canvasObjects$disease$lambda_time)){
      shinyalert("You must insert the lambda parameter (in the Infection page).")
      remove_modal_spinner()
      return()
    }
  }


  if(canvasObjects$disease$Name[length(canvasObjects$disease$Name)] == "S"){
    if(is.null(canvasObjects$disease$nu_time)){
      shinyalert("You must insert the nu parameter (in the Infection page).")
      remove_modal_spinner()
      return()
    }
  }

  if (!(grepl("^([01]?[0-9]|2[0-3]):[0-5][0-9]$", input$initial_time) || grepl("^\\d{1,2}$", input$initial_time))){
    shinyalert("The format of the initial time (in the Configuration page) should be: hh:mm (e.g. 06:15, or 20).")
    remove_modal_spinner()
    return()
  }

  if(input$seed == "" || !grepl("(^[0-9]+).*", input$seed) || input$seed < 0){
    shinyalert("You must specify a number greater or equals than 0 (>= 0) as seed (in the Configuration tab).")
    remove_modal_spinner()
    return()
  }

  if(input$simulation_days == "" || !grepl("(^[0-9]+).*", input$simulation_days) || input$simulation_days <= 0){
    shinyalert("You must specify a number greater than 0 (> 0) as number of days to simulate (in the Configuration tab).")
    remove_modal_spinner()
    return()
  }

  if(input$nrun == "" || !grepl("(^[0-9]+).*", input$nrun) || input$nrun <= 0){
    shinyalert("You must specify a number greater than 0 (> 0) as number of run to execute (in the Configuration tab).")
    remove_modal_spinner()
    return()
  }

  enable("rds_generation")

  if(is.null(canvasObjects$flame_dirs)){
    flame_dirs <- parallel_search_directory("~", "FLAMEGPU-FORGE4FLAME/resources/f4f")

    canvasObjects$flame_dirs <- flame_dirs

    if(length(flame_dirs) == 0){
      output$flame_link <- renderText({
        paste0("The directory FORGE4FLAME/inst/FLAMEGPU-FORGE4FLAME/resources/f4f/ does not exist anywhere. Check you are in the correct directory.")
      })
      remove_modal_spinner()
      return()
    }
  }

  remove_modal_spinner()
}
