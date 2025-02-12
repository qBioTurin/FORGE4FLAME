theme_fancy <- function() {
  theme_minimal() +
    theme(
      plot.background = element_rect(fill = "#2b2b2b", color = NA),
      panel.background = element_rect(fill = "#3c3c3c", color = NA),
      panel.grid.major = element_line(color = "#666666", size = 0.3),
      panel.grid.minor = element_line(color = "#444444", size = 0.2),
      axis.text = element_text(color = "white", size = 12),
      axis.title = element_text(color = "white", size = 14, face = "bold"),
      plot.title = element_text(color = "white", size = 18, face = "bold", hjust = 0.5),
      legend.background = element_rect(fill = "#2b2b2b"),
      legend.text = element_text(color = "white"),
      legend.key = element_rect(fill = "white" ),
      legend.title = element_text(color = "white", face = "bold"),
      legend.position = "bottom",
      strip.background = element_rect(fill = "white",color = "black"),
      strip.text = element_text(color = "black", size = 14, face = "bold")
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

  updateSelectizeInput(inputId = "id_new_agent",choices = unique(names(canvasObjects$agents)), selected = "")
  updateSelectizeInput(inputId = "id_agents_to_copy",choices = unique(names(canvasObjects$agents)), selected = "")

  classes <- c()
  for(i in 1:length(canvasObjects$agents)){
    classes <- c(canvasObjects$agents[[i]]$Class, classes)
  }

  updateSelectizeInput(inputId = "id_class_agent",choices = unique(classes))

  updateSelectizeInput(session, "selectInput_resources_type", choices = c(), selected= "", server = TRUE)

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


    if(selected[length(selected)] == "S"){
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
        geom_line(aes(x=day, y=percentage_infected)) +
        ylim(0, NA) +
        labs(title = "Outside contagion", x = "Day", y = "Percentage") +
        theme(title = element_text(size = 34), axis.title = element_text(size = 26), axis.text = element_text(size = 22))
    })

    showElement("outside_contagion_plot")
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
  dist_panel <-  tabsetPanel(id = paste0("DistTime_tabs_", id),
                             tabPanel("Deterministic",
                                      value = "DetTime_tab",
                                      textInput(inputId = paste0("DetTime_", id), label = "Fixed deterministic value:",placeholder = "value", value = a)
                             ),
                             tabPanel("Stochastic",
                                      value = "StocTime_tab",
                                      selectizeInput(inputId = paste0("DistStoc_id_", id),
                                                     label = "Distribution:",
                                                     choices = c("Exponential","Uniform","Truncated Positive Normal"),
                                                     selected = selected_dist),
                                      conditionalPanel(
                                        condition = paste0("input.DistStoc_id_", id, " == 'Exponential'"),
                                        textInput(inputId = paste0("DistStoc_ExpRate_", id),
                                                 label = "Value:",
                                                 placeholder = "value",
                                                 value = a)

                                      ),
                                      conditionalPanel(
                                        condition = paste0("input.DistStoc_id_", id, " == 'Uniform'"),
                                        fluidRow(
                                          column(width = 4,
                                                 textInput(inputId = paste0("DistStoc_UnifRate_a_", id), label = "a:", placeholder = "value", value = a)
                                          ),
                                          column(width = 4,
                                                 textInput(inputId = paste0("DistStoc_UnifRate_b_", id), label = "b:", placeholder = "value", value = b)

                                          )
                                        )
                                      ),
                                      conditionalPanel(
                                        condition = paste0("input.DistStoc_id_", id, " == 'Truncated Positive Normal'"),
                                        fluidRow(
                                          column(width = 4,
                                                 textInput(inputId = paste0("DistStoc_NormRate_m_", id), label = "Mean:", placeholder = "value", value = a)
                                          ),
                                          column(width = 4,
                                                 textInput(inputId = paste0("DistStoc_NormRate_sd_", id), label = "Sd:", placeholder = "value", value = b)

                                          )
                                         )
                                       )
                             )
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
                      "Efficacy: 1; Fraction: 0; Coverage Dist. Days: Deterministic, 0, 0",
                      "Sensitivity: 1; Specificity: 1; Dist: No swab, 0, 0 ",
                      "Dist. Days: No quarantine, 0, 0; Q. Room: Spawnroom-None; Dist: No swab, 0, 0 ",
                      "First: 0; Second: 0" ),
      From = 1,
      To = 10,
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

          for(ii in seq_along(a_specific[,1])){
            specific = a_specific[ii,]
            fromto_p[,specific$From:specific$To] = specific[,i]
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
    initial_infected= matrix(0,ncol = 2, nrow = length(agents)+1, dimnames = list(agents = c(agents, "Random")))

    global = WHOLEmodel$initial_infected %>% filter(Type == "Global")
    if(dim(global)[1] >0){
      global = global[1,] # just in case, but the saving should block multi global definitions
      initial_infected[,1] = global$Number
    }

    agent_specific =  WHOLEmodel$initial_infected %>% filter(! Type %in% c("Random","Global") )
    if(dim(agent_specific)[1] >0){
      for(ii in seq_along(agent_specific[,1])){
        initial_infected[ agent_specific[ii,]$Type,1] = agent_specific[ii,]$Number
      }
    }

    random = WHOLEmodel$initial_infected %>% filter(Type == "Random")
    if(dim(random)[1] >0){
      random = random[1,] # just in case, but the saving should block multi random definitions
      initial_infected[random[1,]$Type,1] = random[1,]$Number
    }

    initial_infected = cbind(c(agents, "Random"), initial_infected)
  }

  ####
  return(list(AgentMeasuresFromTo = AgentMeasuresFromTo,
              RoomsMeasuresFromTo = MeasuresFromTo,
              initial_infected = initial_infected))
}
