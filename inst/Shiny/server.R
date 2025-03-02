
source(system.file("Shiny","Rfunctions.R", package = "FORGE4FLAME"))

options(shiny.maxRequestSize=2^30)

server <- function(input, output,session) {

  canvasObjects = reactiveValues(rooms = NULL,
                                 roomsINcanvas = NULL,
                                 nodesINcanvas = NULL,
                                 pathINcanvas = NULL,
                                 types = data.frame(Name=c("Normal","Stair","Spawnroom","Fillingroom","Waitingroom"),
                                                    ID=c(4, 5, 6, 7, 8),
                                                    Color=c(
                                                      "rgba(255, 0, 0, 1)", #Red
                                                      "rgba(0, 255, 0, 1)", #Green
                                                      "rgba(0, 0, 255, 1)",#Blue
                                                      "rgba(0, 0, 0, 1)", #Black
                                                      "rgba(0, 100, 30, 1)"
                                                    )
                                 ),
                                 canvasDimension = data.frame(canvasWidth = 1000,
                                                              canvasHeight = 800),
                                 matrixCanvas = matrix(1, nrow = 80,ncol = 100),
                                 selectedId = 1,
                                 floors = NULL,
                                 areas = data.frame(Name=c("None"),
                                                    ID=c(0),
                                                    Color=c(
                                                      "rgba(0, 0, 0, 1)")
                                 ),
                                 agents = NULL,
                                 disease = NULL,
                                 resources = NULL,
                                 color = "Room",
                                 matricesCanvas = NULL,
                                 starting = data.frame(seed=NA, simulation_days=10, day="Monday", time="00:00", step=10, nrun=100, prun=10),
                                 rooms_whatif = data.frame(
                                   Measure = character(),
                                   Type = character(),
                                   Parameters = character(),
                                   From = numeric(),
                                   To = numeric(),
                                   stringsAsFactors = FALSE
                                 ),
                                 agents_whatif = data.frame(
                                   Measure = character(),
                                   Type = character(),
                                   Parameters = character(),
                                   From = numeric(),
                                   To = numeric(),
                                   stringsAsFactors = FALSE
                                 ),
                                 initial_infected = data.frame(
                                   Type = character(),
                                   Number = numeric(),
                                   stringsAsFactors = FALSE
                                 ),
                                 outside_contagion=NULL,
                                 virus_variant = 1,
                                 virus_severity = 0,
                                 cancel_button_selected = FALSE,
                                 TwoDVisual = NULL,
                                 width = NULL,
                                 length = NULL,
                                 height = NULL,
  )

  InfoApp = reactiveValues(NumTabsFlow = 0, NumTabsTimeSlot = 1, tabs_ids = c(), oldAgentType = "")

  canvasObjectsSTART = canvasObjects

  hideElement("outside_contagion_plot")

  observeEvent(input$set_canvas,{
    disable("rds_generation")
    disable("flamegpu_connection")
    canvasWidth = canvasObjects$canvasDimension$canvasWidth
    canvasHeight = canvasObjects$canvasDimension$canvasHeight

    if(input$canvasWidth != "")
      newCanvasWidth = round(as.numeric(gsub(" ", "", input$canvasWidth)))*10 # 10 pixel = 1 meter

    if(input$canvasHeight != "")
      newCanvasHeight = round(as.numeric(gsub(" ", "", input$canvasHeight)))*10 # 10 pixel = 1 meter

    roomOutsideCanvas = FALSE
    if(!is.null(canvasObjects$roomsINcanvas)){
      for(i in 1:nrow(canvasObjects$roomsINcanvas)){
        if(canvasObjects$roomsINcanvas$door[i] == "bottom" || canvasObjects$roomsINcanvas$door[i] == "top"){
          length = canvasObjects$roomsINcanvas$w[i]
          width = canvasObjects$roomsINcanvas$l[i]
        }
        else{
          length = canvasObjects$roomsINcanvas$l[i]
          width = canvasObjects$roomsINcanvas$w[i]
        }

        if((canvasObjects$roomsINcanvas$x[i] + length + 1)*10 >= newCanvasWidth || (canvasObjects$roomsINcanvas$y[i] + width + 1)*10 >= newCanvasHeight){
          shinyalert("The new canvas dimension is too small. There will be at least one room outside the canvas.")
          return()
        }
      }
    }

    if(input$canvasWidth != "")
      canvasObjects$canvasDimension$canvasWidth = newCanvasWidth

    if(input$canvasHeight != "")
      canvasObjects$canvasDimension$canvasHeight = newCanvasHeight

    # Passa i valori al canvas in JavaScript
    js$canvasDimension(canvasObjects$canvasDimension$canvasWidth, canvasObjects$canvasDimension$canvasHeight)

    # we add two rows and columns to ensure that the walls are inside the canvas
    canvasObjects$matrixCanvas = matrix(1,
                                        nrow = canvasObjects$canvasDimension$canvasHeight/10+2 ,
                                        ncol = canvasObjects$canvasDimension$canvasWidth/10+2)

  })

  observeEvent(input$delete_floor, {
    disable("rds_generation")
    disable("flamegpu_connection")
    if(input$canvas_selector != ""){
      canvasObjects$floors <- canvasObjects$floors %>%
        filter(Name != input$canvas_selector)

      if(!is.null(canvasObjects$roomsINcanvas)){
        canvasObjects$roomsINcanvas <- canvasObjects$roomsINcanvas %>%
          filter(CanvasID != input$canvas_selector)
      }

      if(!is.null(canvasObjects$nodesINcanvas)){
        canvasObjects$nodesINcanvas <- canvasObjects$nodesINcanvas %>%
          filter(CanvasID != input$canvas_selector)
      }

      runjs(paste0("
        delete FloorArray[\"", input$canvas_selector, "\"];"))

      selected = ""
      if(nrow(canvasObjects$floors) != 0){
        selected = canvasObjects$floors$Name[1]
      }
      else{
        runjs("$('#canvas_selector').trigger('change');")
      }

      updateSelectizeInput(inputId = "canvas_selector",
                           selected = selected,
                           choices = c("", canvasObjects$floors$Name) )
    }
  })

  #### update floor  ####
  observeEvent(input$canvas_selector,{
    disable("rds_generation")
    disable("flamegpu_connection")
    if(input$canvas_selector != "" && ! input$canvas_selector %in% canvasObjects$floors$Name  ){
      Name = gsub(" ", "", input$canvas_selector)
      if(Name != ""){
        if(!grepl("^[a-zA-Z0-9_]+$", Name)){
          shinyalert("Floor name cannot contain special charachters.")
          updateSelectizeInput(inputId = "canvas_selector",
                               selected = "",
                               choices = c("", canvasObjects$floors$Name) )
          return()
        }

        if(!is.null(canvasObjects$floors) && nrow(canvasObjects$floors) != 0){
          if(nrow(canvasObjects$floors) > 1000){
            shinyalert("The maximum permitted number of floors is 1000.")
            return()
          }

          canvasObjects$floors = rbind(canvasObjects$floors,
                                       data.frame(ID = max(canvasObjects$floors$ID)+1, Name = Name, Order = max(canvasObjects$floors$Order)+1))
        }
        else{
          canvasObjects$floors = data.frame(ID = 1, Name = Name, Order = 1)
        }
      }
    }

    if(!is.null(canvasObjects$roomsINcanvas)){
      roomsINcanvasFloor <- canvasObjects$roomsINcanvas %>%
        filter(CanvasID == input$canvas_selector)

      if(nrow(roomsINcanvasFloor) > 0){
        updateSelectizeInput(inputId = "select_RemoveRoom",
                             selected = "",
                             choices = c("", paste0( roomsINcanvasFloor$Name," #",roomsINcanvasFloor$ID ) ) )
      }
      else{
        updateSelectizeInput(inputId = "select_RemoveRoom",
                             selected = "",
                             choices = "" )
      }
    }
  })

  #### ordering floors
  observeEvent(input$canvas_selector,{
    disable("rds_generation")
    disable("flamegpu_connection")
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
  })

  ## record the floors order
  observeEvent(input$list_floors,{
    disable("rds_generation")
    disable("flamegpu_connection")
    if(length(canvasObjects$floors$Name)>1){
      canvasObjects$floors= canvasObjects$floors %>% arrange(factor(Name,levels = input$list_floors))
      canvasObjects$floors$Order = 1:length(canvasObjects$floors$Name)
    }
  })

  #### save new room  ####
  observeEvent(input$save_room,{
    disable("rds_generation")
    disable("flamegpu_connection")
    Name = gsub(" ", "", tolower(input$id_new_room))

    length_new_room = as.numeric(gsub(" ", "", gsub(",", "\\.", input$length_new_room)))
    width_new_room = as.numeric(gsub(" ", "", gsub(",", "\\.", input$width_new_room)))
    height_new_room = as.numeric(gsub(" ", "", gsub(",", "\\.", input$height_new_room)))

    if(is.na(length_new_room) || is.na(width_new_room) || is.na(height_new_room) ){
      shinyalert(paste0("The height, the lenght and the width must be numbers."))
      return()
    }


    if(Name != "" && width_new_room != "" && length_new_room != "" && height_new_room != ""){

      if(Name %in% canvasObjects$rooms$Name){
        shinyalert(paste0("There already exist a room with name: ", Name, "."))
        return()
      }

      if(input$select_type == ""){
        shinyalert("You must select a type.")
        return()
      }

      if(height_new_room > 10){
        shinyalert("The maximum permitted height for a room is 10 meters.")
        return()
      }

      if(width_new_room < 2 ||  length_new_room < 2 ||  height_new_room < 2){
        shinyalert("The dimension of the room can not be smaller than 2x2x2.")
        return()
      }

      if(!grepl("(^[A-Za-z]+).*", Name)){
        shinyalert("Room name must start with a letter (a-z).")
        return()
      }

      if(!grepl("^[a-zA-Z0-9_]+$", Name)){
        shinyalert("Room name cannot contain special charachters.")
        return()
      }


      samp = runif(3, 0, 1)

      typeID = canvasObjects$types$ID[which(input$select_type == canvasObjects$types)]

      newRoom <- data.frame(Name = Name, ID = typeID,
                            type=input$select_type, w = width_new_room, l = length_new_room, h = height_new_room,
                            colorFill = paste0("rgba(", round(255*samp[1]), ", ", round(255*samp[2]), ", ", round(255*samp[3]),", 1)"))

      if(is.null(canvasObjects$rooms)) {
        canvasObjects$rooms <- newRoom
      }else{
        if(Name %in% canvasObjects$rooms$Name){
          shinyalert(paste0("There already exists a room named ", Name, " (case insensitive). "))
          return()
        }

        canvasObjects$rooms <- rbind(
          canvasObjects$rooms,
          newRoom)
      }
    }else{
      shinyalert("All the dimensions must be defined.")
      return()
    }

    shinyalert("Success", paste0("The room named ", Name, " is added with success."), "success", 1000)
  })

  ## save new area   ####
  observeEvent(input$select_area,{
    disable("rds_generation")
    disable("flamegpu_connection")

    if(! input$select_area %in%canvasObjects$areas$Name  ){
      Name = gsub(" ", "", input$select_area)
      if(Name != ""){
        if(!grepl("^[a-zA-Z0-9_]+$", Name)){
          shinyalert("Area name cannot contain special charachters.")
          updateSelectizeInput(inputId = "select_area",
                               selected = "None",
                               choices = c("", unique(canvasObjects$areas$Name)) )
          return()
        }

        samp = runif(3, 0, 1)
        if(is.null(canvasObjects$areas)) {
          canvasObjects$areas <- data.frame(Name=Name, ID=1, Color=paste0('rgba(', round(255*samp[1]), ', ', round(255*samp[2]), ', ', round(255*samp[3]), ', 1)'))
        }else{
          newID = max(canvasObjects$areas$ID)+1
          newarea = data.frame(Name=Name, ID=newID, Color=paste0('rgba(', round(255*samp[1]), ', ', round(255*samp[2]), ', ', round(255*samp[3]), ', 1)'))
          canvasObjects$areas = rbind(canvasObjects$areas, newarea)
        }
      }
    }

    if(input$select_area != "" && !is.null(canvasObjects$areas)){
      # update the area color list
      updateSelectInput(inputId = "selectInput_color_area",
                        choices = unique(canvasObjects$areas$Name))
    }
  })

  ## update rooms list to choose
  observeEvent(canvasObjects$rooms, {
    disable("rds_generation")
    disable("flamegpu_connection")
    updateSelectizeInput(inputId = "select_room",
                         selected = "",
                         choices = c("", unique(canvasObjects$rooms$Name)) )
    if(input$selectInput_color_room ==""){
      updateSelectInput(inputId = "selectInput_color_room", choices = unique(canvasObjects$rooms$Name))

    }else {
      selected_room <- input$selectInput_color_room
      updateSelectInput(inputId = "selectInput_color_room",selected = selected_room, choices = unique(canvasObjects$rooms$Name))
    }})

  observeEvent(canvasObjects$roomsINcanvas, {
    disable("rds_generation")
    disable("flamegpu_connection")
    rooms = canvasObjects$roomsINcanvas %>% filter(type != "Fillingroom", type != "Stair", type != "Waitingroom")

    roomsAvailable = c("", unique(paste0( rooms$type,"-", rooms$area) ) )
    updateSelectizeInput(session = session, "Det_select_room_flow",
                         choices = roomsAvailable)
    updateSelectizeInput(session = session, "Rand_select_room_flow",
                         choices = roomsAvailable)
  })

  # when a user use DetActivity he can choose a number form 1 to 5
  # observeEvent(input$Det_select_room_flow, {
  #   disable("rds_generation")
  #   disable("flamegpu_connection")
  #   if(input$Det_select_room_flow != ""){
  #
  #     updateSelectizeInput(session = session, "DetActivity",
  #                          choices = c("", "Very Light - e.g. resting", "Light - e.g. speak while resting", "Quite Hard - e.g. speak/walk while standing", "Hard - e.g. loudly speaking"))
  #
  #   }
  # })

  observeEvent(canvasObjects$roomsINcanvas, {
    disable("rds_generation")
    disable("flamegpu_connection")
    roomsINcanvasFloor <- canvasObjects$roomsINcanvas %>%
      filter(CanvasID == input$canvas_selector)

    if(nrow(roomsINcanvasFloor) > 0){
      updateSelectizeInput(inputId = "select_RemoveRoom",
                           selected = "",
                           choices = c("", paste0( roomsINcanvasFloor$Name," #",roomsINcanvasFloor$ID ) ) )
    }
    else{
      updateSelectizeInput(inputId = "select_RemoveRoom",
                           selected = "",
                           choices = "" )
    }
  })

  observeEvent(input$select_type, {
    disable("rds_generation")
    disable("flamegpu_connection")
    if(input$select_type != "" && !is.null(canvasObjects$types)){
      # update the color type list
      updateSelectInput(inputId = "selectInput_color_type",
                        choices = unique(canvasObjects$types$Name))
    }

    if(! input$select_type %in%canvasObjects$types$Name  ){
      Name = gsub(" ", "", input$select_type)

      if(Name != ""){
        if(!grepl("(^[A-Za-z]+).*", Name)){
          shinyalert("Room name must start with a letter (a-z).")
          return()
        }

        if(is.null(canvasObjects$types)) {
          canvasObjects$types <- data.frame(Name=Name, ID=4, Color = "rgba(0, 0, 0, 1)" )
        }else{
          newID = max(canvasObjects$types$ID)+1

          newtype = data.frame(Name=Name, ID=newID,
                               Color = paste0("rgba(",round(255*runif(1, 0, 1)),", ",round(255*runif(1, 0, 1)),", ",round(255*runif(1, 0, 1)),", ",round(255*runif(1, 0, 1)),")") )
          canvasObjects$types = rbind(canvasObjects$types, newtype)
        }
      }

    }

    if(input$select_type == "Fillingroom"){
      updateSelectizeInput(inputId = "door_new_room", choices = c("right","left","top","bottom","none"),selected = "none")
      disable("door_new_room")
    }
    else{
      updateSelectizeInput(inputId = "door_new_room", choices = c("right","left","top","bottom","none"),selected = "right")
      enable("door_new_room")
    }
  })

  observeEvent(input$select_room, {
    disable("rds_generation")
    disable("flamegpu_connection")
    if(!is.null(canvasObjects$rooms) && input$select_room != ""){
      selectedRoom = canvasObjects$rooms %>% filter(Name == input$select_room)
      if(selectedRoom$type == "Fillingroom"){
        updateSelectizeInput(inputId = "door_new_room", choices = c("right","left","top","bottom","none"),selected = "none")
        disable("door_new_room")
      }
      else{
        updateSelectizeInput(inputId = "door_new_room", choices = c("right","left","top","bottom","none"),selected = "right")
        enable("door_new_room")
      }

      if(selectedRoom$type == "Spawnroom"){
        updateSelectizeInput(inputId = "select_area",selected = "None")
        disable("select_area")
      }
      else{
        enable("select_area")
      }
    }
  })

  observeEvent(input$select_room, {
    disable("rds_generation")
    disable("flamegpu_connection")
    canvasObjects$width = canvasObjects$rooms$w[which(canvasObjects$rooms$Name == input$select_room)]
    canvasObjects$length = canvasObjects$rooms$l[which(canvasObjects$rooms$Name == input$select_room)]
    canvasObjects$height = canvasObjects$rooms$h[which(canvasObjects$rooms$Name == input$select_room)]

    output$length <- renderText({
      paste0("Length of selected room (length refers to the wall with the door): ", canvasObjects$length)
    })

    output$width <- renderText({
      paste0("Width of selected room: ", canvasObjects$width)
    })

    output$height <- renderText({
      paste0("Height of selected room: ", canvasObjects$height)
    })
  })

  #### DRAW rooms: ####
  ## add in canvas a new selected room
  observeEvent(input$add_room,{
    disable("rds_generation")
    disable("flamegpu_connection")
    #Se non sono presenti piani non è possibile aggiungere stanze
    if(input$canvas_selector == ""){
      shinyalert("You must select a floor.")
      return()
    }
    if(input$select_room != ""){

      roomSelected = canvasObjects$rooms %>% filter(Name == input$select_room)

      if(roomSelected$type == "Spawnroom" && !is.null(canvasObjects$roomsINcanvas)){
        exist = canvasObjects$roomsINcanvas %>% filter(type == "Spawnroom")

        if(nrow(exist) > 0){
          shinyalert(paste0("There already exists a Spawnroom. It is possible to have only one room of this type."))
          return()
        }
      }

      width = roomSelected$w
      length = roomSelected$l
      height = roomSelected$h
      if(input$door_new_room == "left" || input$door_new_room == "right"){
        width = roomSelected$l
        length = roomSelected$w
      }

      # FullRoom is a flag to set TRUE if inside the matrix representing
      # the room we want the ID of the room
      matrix = CanvasToMatrix(canvasObjects,FullRoom = T,canvas = input$canvas_selector)
      # Check if there is still space for the new room
      result <- find_ones_submatrix_coordinates(matrix, target_rows = width, target_cols = length)
      xnew = result[2]
      ynew = result[1]

      if(is.null(xnew) || is.null(ynew)){
        # There no space available!
        output$Text_SpaceAvailable <- renderUI({
          # Generate the message based on your logic or input values
          message <-  paste0("No space available in the floor for a new ",input$select_room , " room.")

          # Apply custom styling using HTML tags
          styled_message <- paste0("<div style='color: red; background-color: white;'>", message, "</div>")

          # Return the HTML content
          return(HTML(styled_message))
        })
      }else{
        newroom = data.frame(ID = 1,
                             typeID = roomSelected$ID,
                             type=roomSelected$type,
                             x = round(xnew)+1, y = round(ynew)+1,
                             center_x = 0, center_y = 0,
                             door_x = 0, door_y = 0,
                             w = width, l = length, h = height,
                             Name = roomSelected$Name,
                             door = input$door_new_room,
                             colorFill = roomSelected$colorFill,
                             colorBorder = "rgba(0, 0, 0, 1)",
                             area = input$select_area,
                             CanvasID = input$canvas_selector
        )

        if(is.null(canvasObjects$roomsINcanvas)){
          canvasObjects$roomsINcanvas = newroom
        }
        else{
          newroom$ID = max(canvasObjects$roomsINcanvas$ID, 1) + 1
          canvasObjects$roomsINcanvas = rbind(canvasObjects$roomsINcanvas, newroom)
        }

        canvasObjects$selectedId = newroom$ID

        runjs( command_addRoomObject( newroom) )

        rooms = canvasObjects$roomsINcanvas %>% filter(type != "Fillingroom", type != "Stair", type != "Spawnroom")
        roomsAvailable = c("", unique(paste0( rooms$type,"-", rooms$area) ) )
        updateSelectizeInput(session = session, "room_ventilation",
                             choices = roomsAvailable)
        updateSelectizeInput(session = session, "room_quarantine",
                             choices = roomsAvailable)
      }

    }

  })

  deletingRoomFromCanvas = function(session,objectDelete,canvasObjects){
    runjs(paste0("
          FloorArray[\"",objectDelete$CanvasID,"\"].arrayObject.forEach(e => {
            if(e.type === \'rectangle\' && e.id === ", objectDelete$ID, "){
              const indexToRemove = FloorArray[\"",objectDelete$CanvasID,"\"].arrayObject.indexOf(e);
                  // Verifica se l'oggetto è stato trovato
                  if (indexToRemove !== -1) {
                    // Rimuovi l'oggetto dall'array
                    FloorArray[\"",objectDelete$CanvasID,"\"].arrayObject.splice(indexToRemove, 1);
                  }
            }
          })"))


    canvasObjects$roomsINcanvas <- canvasObjects$roomsINcanvas %>%
      filter(ID != objectDelete$ID)

    if(nrow(canvasObjects$roomsINcanvas %>% filter(type == objectDelete$type, area == objectDelete$area)) == 0)
      canvasObjects$rooms_whatif <- canvasObjects$rooms_whatif %>% filter(Type != paste0( objectDelete$type,"-", objectDelete$area) )

    ## if the room is present in Agents Flow then we have to remove them
    ## when there are this type of room anymore
    if(!is.null(canvasObjects$agents)){
      for(a in 1:length(canvasObjects$agents))
        if(!is.null(canvasObjects$agents[[a]]$DeterFlow))
          canvasObjects$agents[[a]]$DeterFlow = canvasObjects$agents[[a]]$DeterFlow %>%
            filter(Room  %in% c("Spawnroom-None", paste0(canvasObjects$roomsINcanvas$type, "-", canvasObjects$roomsINcanvas$area)))
    }

    if(!is.null(canvasObjects$pathINcanvas)){
      pathsINcanvasFloor <- canvasObjects$pathINcanvas %>%
        filter(CanvasID == input$canvas_selector)

      if(!is.null(pathsINcanvasFloor)){
        pIc = pathsINcanvasFloor
        objectDelete$door_x = objectDelete$door_x*10
        objectDelete$door_y = objectDelete$door_y*10
        pIc = pIc %>% filter((fromX == objectDelete$door_x + pIc$offset_x_n1*10 & fromY == objectDelete$door_y + pIc$offset_y_n1*10) |
                               (toX == objectDelete$door_x + pIc$offset_x_n2*10 & toY == objectDelete$door_y + pIc$offset_y_n2*10) )

        for(i in pIc$id)
          runjs(
            paste0("
            const indexToRemove = FloorArray[\"",objectDelete$CanvasID,"\"].arrayObject.findIndex(obj => obj.type === \'segment\' &&  obj.id === ",i,");
            if (indexToRemove !== -1) {
              FloorArray[\"",objectDelete$CanvasID,"\"].arrayObject.splice(indexToRemove, 1);
            }
            ")
          )
      }
    }

    rooms = canvasObjects$roomsINcanvas %>% filter(type != "Fillingroom", type != "Stair")
    roomsAvailable = c("", unique(paste0( rooms$type,"-", rooms$area) ) )
    updateSelectizeInput(session = session, "room_ventilation",
                         choices = roomsAvailable)
    updateSelectizeInput(session = session, "room_quarantine",
                         choices = roomsAvailable)
  }

  observeEvent(input$remove_room,{
    disable("rds_generation")
    disable("flamegpu_connection")
    if(input$select_RemoveRoom != "" && !is.null(canvasObjects$roomsINcanvas) && dim(canvasObjects$roomsINcanvas)[1] > 0) {

      objectDelete = canvasObjects$roomsINcanvas %>%
        mutate(NewID = paste0( Name," #", ID ) ) %>%
        filter(NewID == input$select_RemoveRoom)

      roomSameAreaType = canvasObjects$roomsINcanvas %>% filter(area == objectDelete$area, type == objectDelete$type)

      if(dim(roomSameAreaType)[1] == 1){
        # The room that we want delete is the last one in the area and type,
        # so we have to check that if it is present in the flows than we have to ask if the user want to delete it

        agents_with_room_type <- c()
        #crea un warning che impedisce di proseguire se la stanza da eliminare è presente in un flusso di un agente
        if(!is.null(canvasObjects$agents)){

          agents_with_room_type1 <- do.call(rbind, lapply(canvasObjects$agents,"[[","DeterFlow") ) %>%
            select(Name,Room) %>%
            distinct() %>%
            filter(Room == paste0(objectDelete$type, "-", objectDelete$area)) %>%
            pull(Name)

          agents_with_room_type2 <- do.call(rbind, lapply(canvasObjects$agents,"[[","DeterFlow") ) %>%
            select(Name,Room) %>%
            distinct() %>%
            filter(Room == paste0(objectDelete$type, "-", objectDelete$area)) %>%
            pull(Name)

          agents_with_room_type = unique(agents_with_room_type1,agents_with_room_type2)

          if(length(agents_with_room_type) > 0){

            shinyalert(
              title = "Confirmation",
              text = paste0("Impossible to delete the room: ", objectDelete$Name,
                            " as it is the last room available for the flow of the following agents: ",
                            paste(unique(agents_with_room_type), collapse = ", "), "."),
              type = "warning",
              showCancelButton = TRUE,
              confirmButtonText = "OK",
              cancelButtonText = "Cancel",
              callbackR = function(x) {
                if (x) {
                  for(a in  agents_with_room_type){
                    if(!is.null(canvasObjects$agents[[a]]$DeterFlow)){
                      canvasObjects$agents[[a]]$DeterFlow = canvasObjects$agents[[a]]$DeterFlow %>% filter(Room != paste0(objectDelete$type, "-", objectDelete$area) )
                    }
                    if(!is.null(canvasObjects$agents[[a]]$RandFlow)){
                      canvasObjects$agents[[a]]$RandFlow = canvasObjects$agents[[a]]$RandFlow %>% filter(Room != paste0(objectDelete$type, "-", objectDelete$area) )
                    }
                  }

                  deletingRoomFromCanvas(session,objectDelete,canvasObjects)
                }
              }
            )
            return()
          }
        }

        ### Feleting rooms from whatif tables
        RoomToDelete =  paste0(objectDelete$type, "-", objectDelete$area)
        if(nrow(canvasObjects$rooms_whatif) > 0)
          canvasObjects$rooms_whatif <- canvasObjects$rooms_whatif %>% filter(Type != RoomToDelete)
      }

      deletingRoomFromCanvas(session,objectDelete,canvasObjects)

    }})

  #### Color legend: ####

  observeEvent(input$select_fillColor,{

    if(!is.null(canvasObjects$roomsINcanvas) &&
       dim(canvasObjects$roomsINcanvas)[1]>0 ){ # some colors are changed
      canvasObjects$color <- input$select_fillColor

      # First all the rooms of the changed color are removed
      if(input$select_fillColor == "Area")
        colors = canvasObjects$areas %>% rename(area = Name)
      else if(input$select_fillColor == "Type")
        colors = canvasObjects$types %>% rename(type = Name)
      else
        colors = canvasObjects$rooms %>% select(ID,Name,colorFill) %>% rename(Color = colorFill)

      colors = merge(colors %>% select(-ID),canvasObjects$roomsINcanvas)

      for(canvasID in unique(canvasObjects$roomsINcanvas$CanvasID)){
        for( id in unique(canvasObjects$roomsINcanvas$ID)){
          runjs(paste0("
          FloorArray[\"",canvasID,"\"].arrayObject.forEach(e => {
            if(e.type === \'rectangle\' && e.id === ", id, "){
              const indexToRemove = FloorArray[\"",canvasID,"\"].arrayObject.indexOf(e);
              console.log('indexToRemove:', indexToRemove);
                  // Verifica se l'oggetto è stato trovato
                  if (indexToRemove !== -1) {
                  // Rimuovi l'oggetto dall'array
                  FloorArray[\"",canvasID,"\"].arrayObject.splice(indexToRemove, 1);
                  }
            }
          })"))

          # Second all the removed rooms are added with the new colors
          canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$ID == id,"colorFill"] <- colors[colors$ID == id, "Color"]
          runjs( command_addRoomObject(canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$ID == id,]) )

        }
      }
    }
  })

  # room
  output$RoomColors <- renderUI({
    if(!is.null(canvasObjects$rooms) && input$selectInput_color_room!= ""){
      col_output_list <-  lapply(input$selectInput_color_room,function(name)
      {
        room = canvasObjects$rooms %>% filter(Name == name)
        colourpicker::colourInput(paste0("col_",room$Name),
                                  paste0("Select colour for " , room$Name),
                                  gsub(pattern = ", 1\\)",replacement = "\\)",
                                       gsub(pattern = "rgba",replacement = "rgb",room$colorFill)
                                  ),
                                  allowTransparent = T)
      })
      do.call(tagList, col_output_list)
    }
  })
  toListen <- reactive({
    if(!is.null(canvasObjects$rooms)){
      ListCol = lapply(canvasObjects$rooms$Name, function(i){
        if(!is.null(input[[paste0("col_",i)]]))
          data.frame(Name = i, Col = input[[paste0("col_",i)]])
      }
      )
      ListCol<-ListCol[!sapply(ListCol,is.null)]
    }else{
      ListCol = list()
    }

    return(ListCol)
  })
  observeEvent(toListen(),{
    disable("rds_generation")
    disable("flamegpu_connection")
    if(length(toListen()) > 0 ){
      ColDF = do.call(rbind,
                      lapply(canvasObjects$rooms$Name,function(i)
                        if(!is.null(input[[paste0("col_",i)]]))
                          data.frame(Name = i,
                                     ColNew = paste0("rgba(",paste(col2rgb(input[[paste0("col_",i)]]),collapse = ", "),", 1)")
                          )
                      )
      )

      ## Check which color has changed for updating the room color

      ColDFmerged = merge(ColDF,canvasObjects$rooms)
      ColDFmergedFiltered = ColDFmerged %>% filter( ColNew != colorFill  )

      if(dim(ColDFmergedFiltered)[1] >0){
        if(!is.null(canvasObjects$roomsINcanvas) &&
           dim(canvasObjects$roomsINcanvas)[1]>0 ){ # some colors are changed

          # First all the rooms of the changed color are removed
          objectDelete = canvasObjects$roomsINcanvas %>%
            filter(Name %in% ColDFmergedFiltered$Name)

          if(input$select_fillColor == "Room"){
            runjs(paste0("
          FloorArray[\"",objectDelete$CanvasID,"\"].arrayObject.forEach(e => {
            if(e.type === \'rectangle\' && e.id === ", objectDelete$ID, "){
              const indexToRemove = FloorArray[\"",objectDelete$CanvasID,"\"].arrayObject.indexOf(e);
              console.log('indexToRemove:', indexToRemove);
                  // Verifica se l'oggetto è stato trovato
                  if (indexToRemove !== -1) {
                  // Rimuovi l'oggetto dall'array
                  FloorArray[\"",objectDelete$CanvasID,"\"].arrayObject.splice(indexToRemove, 1);
                  }
            }
          })"
            )
            )

            # Second all the removed rooms are added with the new colors
            for(i in objectDelete$ID){
              canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$ID == i,"colorFill"] <- ColDFmergedFiltered[ColDFmergedFiltered$Name == canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$ID == i,"Name"] ,"ColNew"]
              runjs( command_addRoomObject(canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$ID == i,]) )
            }
          }
        }

        for(j in ColDFmergedFiltered$Name)
          canvasObjects$rooms[canvasObjects$rooms$Name == j,"colorFill"] <-   ColDFmergedFiltered[ColDFmergedFiltered$Name == j ,"ColNew"]
      }
    }
  })

  # areas
  output$AreaColors <- renderUI({
    if(!is.null(canvasObjects$areas) && input$selectInput_color_area!= ""){
      name = input$selectInput_color_area
      canvasObjects$areas$Color[canvasObjects$areas$Name == name] -> color
      div(
        colourpicker::colourInput(paste0("col_area_",name),
                                  paste0("Select colour for " , name),
                                  gsub(pattern = ", 1\\)",replacement = "\\)",
                                       gsub(pattern = "rgba",replacement = "rgb",color)
                                  ),
                                  allowTransparent = T)
      )
    }
  })
  toListen_color_area <- reactive({
    if(!is.null(canvasObjects$areas)){
      ListCol = lapply(canvasObjects$areas$Name, function(i){
        if(!is.null(input[[paste0("col_area_",i)]]))
          data.frame(Name = i, Col = input[[paste0("col_area_",i)]])
      }
      )
      ListCol<-ListCol[!sapply(ListCol,is.null)]
    }else{
      ListCol = list()
    }

    return(ListCol)
  })
  observeEvent(toListen_color_area(),{
    disable("rds_generation")
    disable("flamegpu_connection")
    if(length(toListen_color_area()) > 0 ){
      ColDF = do.call(rbind,
                      lapply(canvasObjects$areas$Name,function(i)
                        if(!is.null(input[[paste0("col_area_",i)]]))
                          data.frame(Name = i,
                                     ColNew = paste0("rgba(",paste(col2rgb(input[[paste0("col_area_",i)]]),collapse = ", "),", 1)")
                          )
                      )
      )

      ## Check which color has changed for updating the room color

      ColDFmerged = merge(ColDF, canvasObjects$areas)
      ColDFmergedFiltered = ColDFmerged %>% filter( ColNew != Color  )

      if(dim(ColDFmergedFiltered)[1] >0){
        if(!is.null(canvasObjects$roomsINcanvas) &&
           dim(canvasObjects$roomsINcanvas)[1]>0 ){ # some colors are changed

          # First all the rooms of the changed color are removed
          objectDelete = canvasObjects$roomsINcanvas %>%
            filter(area %in% ColDFmergedFiltered$Name)

          if(input$select_fillColor == "Area"){
            runjs(paste0("
          FloorArray[\"",objectDelete$CanvasID,"\"].arrayObject.forEach(e => {
            if(e.type === \'rectangle\' && e.id === ", objectDelete$ID, "){
              const indexToRemove = FloorArray[\"",objectDelete$CanvasID,"\"].arrayObject.indexOf(e);
              console.log('indexToRemove:', indexToRemove);
                  // Verifica se l'oggetto è stato trovato
                  if (indexToRemove !== -1) {
                  // Rimuovi l'oggetto dall'array
                  FloorArray[\"",objectDelete$CanvasID,"\"].arrayObject.splice(indexToRemove, 1);
                  }
            }
          })"
            )
            )

            # Second all the removed rooms are added with the new colors
            for(i in ColDFmergedFiltered$Name){
              canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$area == i,"colorFill"] <- ColDFmergedFiltered[ColDFmergedFiltered$Name == i ,"ColNew"]
              for(j in canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$area == i,"ID"])
                runjs( command_addRoomObject(canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$ID == j,]) )
            }
          }
        }

        for(j in ColDFmergedFiltered$Name)
          canvasObjects$areas[canvasObjects$areas$Name == j,"Color"] <-   ColDFmergedFiltered[ColDFmergedFiltered$Name == j ,"ColNew"]

      }
    }
  })

  # type
  output$TypeColors <- renderUI({
    if(!is.null(canvasObjects$types) && input$selectInput_color_type!= ""){
      name = input$selectInput_color_type
      canvasObjects$types$Color[canvasObjects$types$Name == name] -> color
      div(
        colourpicker::colourInput(paste0("col_type_",name),
                                  paste0("Select colour for " , name),
                                  gsub(pattern = ", 1\\)",replacement = "\\)",
                                       gsub(pattern = "rgba",replacement = "rgb",color)
                                  ),
                                  allowTransparent = T)
      )
    }
  })
  toListen_color_type <- reactive({
    if(!is.null(canvasObjects$types)){
      ListCol = lapply(canvasObjects$types$Name, function(i){
        if(!is.null(input[[paste0("col_type_",i)]]))
          data.frame(Name = i, Col = input[[paste0("col_type_",i)]])
      }
      )
      ListCol<-ListCol[!sapply(ListCol,is.null)]
    }else{
      ListCol = list()
    }

    return(ListCol)
  })

  observeEvent(toListen_color_type(),{
    disable("rds_generation")
    disable("flamegpu_connection")
    if(length(toListen_color_type()) > 0 ){
      ColDF = do.call(rbind,
                      lapply(canvasObjects$types$Name,function(i)
                        if(!is.null(input[[paste0("col_type_",i)]]))
                          data.frame(Name = i,
                                     ColNew = paste0("rgba(",paste(col2rgb(input[[paste0("col_type_",i)]]),collapse = ", "),", 1)")
                          )
                      )
      )

      ## Check which color has changed for updating the room color

      ColDFmerged = merge(ColDF, canvasObjects$types)
      ColDFmergedFiltered = ColDFmerged %>% filter( ColNew != Color  )


      if(input$select_fillColor == "Type"){

        if(dim(ColDFmergedFiltered)[1] >0){

          # First all the rooms of the changed color are removed
          objectDelete = canvasObjects$roomsINcanvas %>%
            filter(type %in% ColDFmergedFiltered$Name)

          runjs(paste0("
          FloorArray[\"",objectDelete$CanvasID,"\"].arrayObject.forEach(e => {
            if(e.type === \'rectangle\' && e.id === ", objectDelete$ID, "){
              const indexToRemove = FloorArray[\"",objectDelete$CanvasID,"\"].arrayObject.indexOf(e);
              console.log('indexToRemove:', indexToRemove);
                  // Verifica se l'oggetto è stato trovato
                  if (indexToRemove !== -1) {
                  // Rimuovi l'oggetto dall'array
                  FloorArray[\"",objectDelete$CanvasID,"\"].arrayObject.splice(indexToRemove, 1);
                  }
            }
          })"
          )
          )

          # Second all the removed rooms are added with the new colors
          for(i in ColDFmergedFiltered$Name){
            canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$type == i,"colorFill"] <- ColDFmergedFiltered[ColDFmergedFiltered$Name == i ,"ColNew"]
            for(j in canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$type == i,"ID"])
              runjs( command_addRoomObject(canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$ID == j,]) )
          }
        }
      }

      for(j in ColDFmergedFiltered$Name)
        canvasObjects$types[canvasObjects$types$Name == j,"Color"] <-   ColDFmergedFiltered[ColDFmergedFiltered$Name == j ,"ColNew"]

    }
  })

  ##### DRAW points: ####
  observeEvent(input$add_point,{
    disable("rds_generation")
    disable("flamegpu_connection")
    if(!is.null(canvasObjects$roomsINcanvas)){
      roomsINcanvasFloor <- canvasObjects$roomsINcanvas %>%
        filter(CanvasID == input$canvas_selector)

      matrix = CanvasToMatrix(canvasObjects,canvas = input$canvas_selector)
      #check if there is still space for the new room
      result <-  which(matrix == 1, arr.ind = TRUE)
      if(dim(result)[1] == 0) result = NULL
      else result = result[1,]
      xnew = result[2]
      ynew = result[1]
    }else{
      xnew =runif(1,min = 1,max = canvasObjects$canvasDimension$canvasWidth/10-1)
      ynew =runif(1,min = 1,max = canvasObjects$canvasDimension$canvasHeight/10-1)
    }

    newpoint = data.frame(ID = 1 , x = round(xnew), y = round(ynew), CanvasID = input$canvas_selector )

    if(is.null(canvasObjects$nodesINcanvas))
      canvasObjects$nodesINcanvas = newpoint
    else{
      newpoint$ID = max(canvasObjects$nodesINcanvas$ID) + 1
      canvasObjects$nodesINcanvas = rbind(canvasObjects$nodesINcanvas, newpoint)
    }

    runjs(paste0("// Crea un nuovo oggetto Circle con le proprietà desiderate
                const newPoint = new Circle(", newpoint$ID,",", newpoint$x*10," , ", newpoint$y*10,", 5, rgba(0, 127, 255, 1));
                // Aggiungi il nuovo oggetto Circle all'array arrayObject
                FloorArray[\"",newpoint$CanvasID,"\"].arrayObject.push(newPoint);") )

  })

  observeEvent(input$remove_point,{
    disable("rds_generation")
    disable("flamegpu_connection")
    if(!is.null(canvasObjects$nodesINcanvas) && dim(canvasObjects$nodesINcanvas)[1]>0) {
      nodesINcanvasFloor <- canvasObjects$nodesINcanvas %>%
        filter(CanvasID == input$canvas_selector)

      deletedPoint = nodesINcanvasFloor[length(nodesINcanvasFloor$ID),]

      runjs(paste0("
        const indexToRemove = FloorArray[\"",deletedPoint$CanvasID,"\"].arrayObject.findIndex(obj => obj.type === \'circle\' &&  obj.id === ",deletedPoint$ID,");
        if (indexToRemove !== -1) {
          FloorArray[\"",deletedPoint$CanvasID,"\"].arrayObject.splice(indexToRemove, 1);
        }
        ") )

      if(!is.null(canvasObjects$pathINcanvas)){
        pathsINcanvasFloor <- canvasObjects$pathINcanvas %>%
          filter(CanvasID == input$canvas_selector)

        if(!is.null(pathsINcanvasFloor)){
          pIc = pathsINcanvasFloor
          deletedPoint$x = deletedPoint$x*10
          deletedPoint$y = deletedPoint$y*10
          pIc = pIc %>% filter((fromX == deletedPoint$x & fromY == deletedPoint$y) |
                                 (toX == deletedPoint$x & toY == deletedPoint$y) )

          for(i in pIc$id)
            runjs(
              paste0("
            const indexToRemove = FloorArray[\"",deletedPoint$CanvasID,"\"].arrayObject.findIndex(obj => obj.type === \'segment\' &&  obj.id === ",i,");
            if (indexToRemove !== -1) {
              FloorArray[\"",deletedPoint$CanvasID,"\"].arrayObject.splice(indexToRemove, 1);
            }
            ")
            )
        }
      }

      canvasObjects$nodesINcanvas <- canvasObjects$nodesINcanvas %>%
        filter(ID != deletedPoint$ID)
    }



  })

  observeEvent(input$clear_all,{
    disable("rds_generation")
    disable("flamegpu_connection")
    if(!is.null(canvasObjects$roomsINcanvas)){

      canvasObjects$roomsINcanvas <- canvasObjects$roomsINcanvas %>%
        filter(CanvasID != input$canvas_selector)
      if(!is.null(canvasObjects$agents)){
        for(a in 1:length(canvasObjects$agents)){
          if(!is.null(canvasObjects$agents[[a]]$DeterFlow)){
            roomparts <- strsplit(canvasObjects$agents[[a]]$DeterFlow$Room, "-")
            if(roomparts[[1]][1]!="Do nothing")
            {for(i in 1:length(roomparts)){
              if(nrow(canvasObjects$roomsINcanvas %>% filter(type == roomparts[[i]][1], area == roomparts[[i]][2]))==0){
                canvasObjects$agents[[a]]$DeterFlow <- canvasObjects$agents[[a]]$DeterFlow %>% filter(Room != canvasObjects$agents[[a]]$DeterFlow$Room[i])
              }
            }}
            roomparts<-strsplit(canvasObjects$agents[[a]]$RandFlow$Room, "-")
            if(roomparts[[1]][1]!="Do nothing")
            {for(i in 1:length(roomparts)){
              if(nrow(canvasObjects$roomsINcanvas %>% filter(type == roomparts[[i]][1], area == roomparts[[i]][2]))==0){
                canvasObjects$agents[[a]]$RandFlow <- canvasObjects$agents[[a]]$RandFlow %>% filter(Room != canvasObjects$agents[[a]]$RandFlow$Room[i])
              }
            }}
          }

        }
      }
    }

    if(!is.null(canvasObjects$nodesINcanvas)){
      canvasObjects$nodesINcanvas <- canvasObjects$nodesINcanvas %>%
        filter(CanvasID != input$canvas_selector)
    }

    runjs(paste0("
        FloorArray[\"",input$canvas_selector,"\"].arrayObject = new Array(0)"))
  })

  observeEvent(input$path_generation,{
    disable("rds_generation")
    disable("flamegpu_connection")
    nodes = NULL

    if(!is.null(canvasObjects$nodesINcanvas)){
      nodesINcanvasFloor <- canvasObjects$nodesINcanvas %>%
        filter(CanvasID == input$canvas_selector) %>%
        mutate(offset_x = 0, offset_y = 0, door = "none")

      nodesINcanvasFloor <- unique(nodesINcanvasFloor)

      if(nrow(nodesINcanvasFloor) >= 1){
        nodes = nodesINcanvasFloor
      }
    }

    CanvasToMatrix(canvasObjects, canvas = input$canvas_selector)


    if(!is.null(canvasObjects$roomsINcanvas)){
      if(is.null(nodes)){
        maxID <- 0
      }
      else{
        maxID <- max(nodes$ID)
      }

      roomsINcanvasFloor <- canvasObjects$roomsINcanvas %>%
        filter(CanvasID == input$canvas_selector, door != "none") %>%
        mutate(ID=ID+maxID, x=door_x, y=door_y, CanvasID=CanvasID) %>%
        select(ID, x, y, CanvasID, door)

      offsets_x = c()
      offsets_y = c()
      for(i in 1:nrow(roomsINcanvasFloor)){
        if(roomsINcanvasFloor$door[i] == "bottom"){
          roomsINcanvasFloor$y[i] = roomsINcanvasFloor$y[i] + 1
          offsets_x <- c(offsets_x, 0)
          offsets_y <- c(offsets_y, 1)
        }
        else if(roomsINcanvasFloor$door[i] == "left"){
          roomsINcanvasFloor$x[i] = roomsINcanvasFloor$x[i] - 1
          offsets_x <- c(offsets_x, 0)
          offsets_y <- c(offsets_y, 0)
        }
        else if(roomsINcanvasFloor$door[i] == "top"){
          roomsINcanvasFloor$y[i] = roomsINcanvasFloor$y[i] - 1
          offsets_x <- c(offsets_x, 0)
          offsets_y <- c(offsets_y, 0)
        }
        else{
          roomsINcanvasFloor$x[i] = roomsINcanvasFloor$x[i] + 1
          offsets_x <- c(offsets_x, 1)
          offsets_y <- c(offsets_y, 0)
        }
      }

      roomsINcanvasFloor <- roomsINcanvasFloor %>%
        mutate(offset_x = offsets_x, offset_y = offsets_y)

      if(!is.null(nodes)){
        nodes <- rbind(nodes, roomsINcanvasFloor)
      }
      else{
        nodes <- roomsINcanvasFloor
      }
    }

    ######
    # Let's generate the dataframe in which we save all the possible paths
    pathINcanvasLIST = list()
    k = 1
    for( id in nodes$ID){
      n1 = nodes %>% filter(ID == id)
      for(id2 in nodes$ID[nodes$ID>id]){
        n2 = nodes %>% filter(ID == id2)
        if((n1$door == "none"   || n2$door == "none") ||
           (n1$door == "right"  && ((n2$door == "right"  && n2$x == n1$x) || (n2$door == "left"   && n2$x > n1$x) || (n2$door == "top"  && n2$x > n1$x && n2$y > n1$y) || (n2$door == "bottom" && n2$x > n1$x && n2$y < n1$y))) ||
           (n1$door == "left"   && ((n2$door == "left"   && n2$x == n1$x) || (n2$door == "right"  && n2$x < n1$x) || (n2$door == "top"  && n2$x < n1$x && n2$y > n1$y) || (n2$door == "bottom" && n2$x < n1$x && n2$y < n1$y))) ||
           (n1$door == "top"    && ((n2$door == "top"    && n2$y == n1$y) || (n2$door == "bottom" && n2$y < n1$y) || (n2$door == "left" && n2$y < n1$y && n2$x > n1$x) || (n2$door == "right"  && n2$y < n1$y && n2$x < n1$x))) ||
           (n1$door == "bottom" && ((n2$door == "bottom" && n2$y == n1$y) || (n2$door == "top"    && n2$y > n1$y) || (n2$door == "left" && n2$y > n1$y && n2$x > n1$x) || (n2$door == "right"  && n2$y > n1$y && n2$x < n1$x)))){
          pathINcanvasLIST[[k]] = data.frame(id = k,
                                             fromX = n1$x*10, fromY = n1$y*10,
                                             toX = n2$x*10, toY = n2$y*10, CanvasID = input$canvas_selector,
                                             offset_x_n1 = n1$offset_x, offset_y_n1 = n1$offset_y,
                                             offset_x_n2 = n2$offset_x, offset_y_n2 = n2$offset_y)
          k = k+1
        }
      }
    }

    pIc <- NULL

    if(!is.null(canvasObjects$pathINcanvas)){
      pIc <- canvasObjects$pathINcanvas %>%
        filter(CanvasID == input$canvas_selector)

      canvasObjects$pathINcanvas <- canvasObjects$pathINcanvas %>%
        filter(CanvasID != input$canvas_selector)
    }

    pathINcanvasLIST <- do.call(rbind,pathINcanvasLIST)

    canvasObjects$pathINcanvas = rbind(canvasObjects$pathINcanvas, pathINcanvasLIST)
    ######

    if(!is.null(pIc)){
      for(i in pIc$id)
        runjs(
          paste0("
          const indexToRemove = FloorArray[\"",input$canvas_selector,"\"].arrayObject.findIndex(obj => obj.type === \'segment\' &&  obj.id === ",i,");
          if (indexToRemove !== -1) {
            FloorArray[\"",input$canvas_selector,"\"].arrayObject.splice(indexToRemove, 1);
          }
          ")
        )
    }


    for(i in pathINcanvasLIST$id){
      pIc = pathINcanvasLIST %>% filter(id==i)
      path = bresenham(c(pIc$fromX/10, pIc$toX/10), c(pIc$fromY/10, pIc$toY/10))
      matrixCanvas = CanvasToMatrix(canvasObjects,canvas = input$canvas_selector)
      sum = 0
      for(j in 1:length(path$x)){
        if(matrixCanvas[path$y[j], path$x[j]] == 0)
          sum = sum + 1
      }
      if(sum == 0){
        runjs(paste0("// Crea un nuovo oggetto path
                const newPath = new Segment(", pIc$id,",",
                     pIc$fromX - pIc$offset_x_n1*10," , ", pIc$fromY - pIc$offset_y_n1*10,
                     " , ",pIc$toX - pIc$offset_x_n2*10," , ",pIc$toY - pIc$offset_y_n2*10,
                     ");
                // Aggiungi il nuovo oggetto Segment all'array arrayObject
                FloorArray[\"",input$canvas_selector,"\"].arrayObject.push(newPath);") )
      }
    }
  })

  ####

  observeEvent(input$selected, {
    disable("rds_generation")
    disable("flamegpu_connection")
    if(!is.null(input$id)){
      x = round(input$x/10)
      y = round(input$y/10)

      length = canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$ID == input$id, "l"]
      width = canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$ID == input$id, "w"]

      if(input$type == "circle")
        canvasObjects$nodesINcanvas[canvasObjects$nodesINcanvas$ID == input$id,c("x","y")] = c(x, y)
      else{
        canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$ID == input$id,c("x","y")] = c(x, y)
      }

      canvasObjects$selectedId = input$id
    }
  })

  observeEvent(input$check, {
    disable("rds_generation")
    disable("flamegpu_connection")

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

    enable("flamegpu_connection")
  })

  output$rds_generation <- downloadHandler(
    filename = function() {
      paste0('WHOLEmodel', Sys.Date(), '.zip')
    },
    content = function(file) {
      canvasObjects$TwoDVisual <- NULL
      canvasObjects$plot_2D <- NULL
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)
      dir.create(paste0(temp_directory, "/obj"))
      model = reactiveValuesToList(canvasObjects)
      model$flame_dirs <- NULL

      matricesCanvas <- list()
      for(cID in unique(canvasObjects$roomsINcanvas$CanvasID)){
        matricesCanvas[[cID]] = CanvasToMatrix(canvasObjects, canvas = cID)
      }
      canvasObjects$matricesCanvas <- matricesCanvas
      file_name <- glue("WHOLEmodel.RDs")
      saveRDS(model, file=file.path(temp_directory, file_name))

      out = FromToMatrices.generation(model)
      model$rooms_whatif = out$RoomsMeasuresFromTo
      model$agents_whatif = out$AgentMeasuresFromTo
      model$initial_infected = out$initial_infected
      model$outside_contagion$percentage_infected <- as.character(model$outside_contagion$percentage_infected)
      write_json(x = model, path = file.path(temp_directory, gsub(".RDs", ".json", file_name)))

      generate_obj(paste0(temp_directory, "/obj"))

      zip::zip(
        zipfile = file,
        files = dir(temp_directory),
        root = temp_directory
      )
    },
    contentType = "application/zip"
  )

  observeEvent(input$flamegpu_connection, {
    showModal(
      modalDialog(
        title = "Insert a directory name to identify uniquely this model",
        textInput("popup_text", "Directory name:", ""),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("save_text", "Save")
        )
      )
    )
  })

  observeEvent(input$save_text, {
    removeModal()

    flame_dirs <- canvasObjects$flame_dirs

    for(flame_dir in flame_dirs){
      if(!dir.exists(paste0(flame_dir, input$popup_text))){
        system(paste0("mkdir ", flame_dir, "/", input$popup_text))
      }
    }

    matricesCanvas <- list()
    for(cID in unique(canvasObjects$roomsINcanvas$CanvasID)){
      matricesCanvas[[cID]] = CanvasToMatrix(canvasObjects, canvas = cID)
    }
    canvasObjects$matricesCanvas <- matricesCanvas

    canvasObjects$TwoDVisual <- NULL
    canvasObjects$plot_2D <- NULL

    model = reactiveValuesToList(canvasObjects)
    model$flame_dirs <- NULL

    file_name <- glue("WHOLEmodel.RDs")
    for(flame_dir in flame_dirs){
      saveRDS(model, file=file.path(paste0(flame_dir, "/", input$popup_text), file_name))
    }

    out = FromToMatrices.generation(model)
    model$rooms_whatif = out$RoomsMeasuresFromTo
    model$agents_whatif = out$AgentMeasuresFromTo
    model$initial_infected = out$initial_infected
    model$outside_contagion$percentage_infected <- as.character(model$outside_contagion$percentage_infected)
    file_name <- glue("WHOLEmodel.json")
    for(flame_dir in flame_dirs){
      write_json(x = model, path = file.path(paste0(flame_dir, "/", input$popup_text), file_name))
    }

    success_text <- "Model linked to FLAME GPU 2 in "
    i <- 0
    for(flame_dir in flame_dirs){
      success_text <- paste0(success_text, flame_dir, "/", input$popup_text)
      if(i < length(flame_dirs) - 1)
        success_text <- paste0(success_text, ", ")

      i <- i + 1
    }

    shinyalert("Success", success_text, "success", 1000)
  })

  ### Load: ####

  # general upload in the app
  observeEvent(input$LoadRDs_Button,{
    disable("rds_generation")
    disable("flamegpu_connection")
    if( !is.null(canvasObjects$roomsINcanvas) )
    { ### alert!!! if it is already present!
      showModal(modalDialog(
        title = "Important message",
        "Do you want to update the rooms by clearing the floor?",
        easyClose = TRUE,
        footer= tagList(actionButton("confirmUpload", "Update"),
                        modalButton("Cancel")
        )
      ))
    }
    else{
      isolate({
        if(is.null(input$RDsImport) || !file.exists(input$RDsImport$datapath) || !grepl(".RDs", input$RDsImport$datapath)){
          shinyalert("Error","Please select one RDs file.", "error", 5000)
          return()
        }

        mess = readRDS(input$RDsImport$datapath)
        messNames = names(mess)

        if(!all(messNames[-length(messNames)] %in% names(canvasObjectsSTART)) ){
          shinyalert("Error",
                     paste(mess[["message"]],"\n The file must be RDs saved throught this application." ),
                     "error", 5000)
          return()
        }

        textSucc = UpdatingData(input,output,canvasObjects,mess,areasColor, session)
        shinyalert("Success", textSucc, "success", 1000)
        updateTabsetPanel(session, "SideTabs", selected = "canvas_tab")
      })
    }
    postprocObjects$FLAGmodelLoaded = TRUE
  })

  observeEvent(input$confirmUpload,{
    disable("rds_generation")
    disable("flamegpu_connection")
    # clear the object
    for(i in names(canvasObjects))
      canvasObjects[[i]] = canvasObjectsSTART[[i]]

    # output$LoadingError_RDs <- renderText(
    isolate({
      if(is.null(input$RDsImport) || !file.exists(input$RDsImport$datapath) || !grepl(".RDs", input$RDsImport$datapath)){
        shinyalert("Error","Please select one RDs file.", "error", 5000)
        return()
      }

      mess = readRDS(input$RDsImport$datapath)
      messNames = names(mess)

      if(!all(messNames[-length(messNames)] %in% names(canvasObjectsSTART)) ){
        shinyalert("Error",
                   paste(mess[["message"]],"\n The file must be RDs saved throught this application." ),
                   "error", 5000)
        return()
      }

      textSucc = UpdatingData(input,output,canvasObjects,mess,areasColor, session)
      shinyalert("Success", textSucc, "success", 1000)
      updateTabsetPanel(session, "SideTabs", selected = "canvas_tab")

      UpdatingData(input,output,canvasObjects,mess,areasColor, session)
      postprocObjects$FLAGmodelLoaded = TRUE
    })
    # )
    removeModal()
  })

  ### AGENTS definition ####
  observeEvent(input$id_new_agent,{
    disable("rds_generation")
    disable("flamegpu_connection")
    Agent = input$id_new_agent

    if(Agent != ""){
      if(!grepl("^[a-zA-Z0-9_]+$", Agent)){
        shinyalert("Agent name cannot contain special charachters.")
        updateSelectizeInput(inputId = "id_new_agent",
                             selected = "",
                             choices = c("", names(canvasObjects$agents)) )
        return()
      }

      if(stringr::str_to_lower(Agent) %in% c("global","random")){
        shinyalert("Agent name cannot be 'global' or 'random'.")
        updateSelectizeInput(inputId = "id_new_agent",
                             selected = "",
                             choices = c("", names(canvasObjects$agents)) )
        return()
      }
      new_agent = list(
        DeterFlow = data.frame(Name=character(0), Room=character(0), Time=numeric(0), Flow =numeric(0), Acticity = numeric(0),
                               Label = character(0), FlowID = character(0) ),
        RandFlow  = data.frame(Name=Agent, Room="Do nothing", Dist="Deterministic", Activity=1, ActivityLabel="Light", Time=0, Weight =1),
        Class = "",
        EntryExitTime = NULL,
        NumAgent = "1"
      )


      if(is.null(canvasObjects$agents)){
        canvasObjects$agents[[1]] = new_agent
        names(canvasObjects$agents) = Agent
        canvasObjects$agents[[Agent]]$entry_type <- "Time window"
      }
      else if(! Agent %in% names(canvasObjects$agents) ){
        canvasObjects$agents[[Agent]] = new_agent
        canvasObjects$agents[[Agent]]$entry_type <- "Time window"
      }

      updateSelectizeInput(session = session,"id_class_agent",selected = canvasObjects$agents[[Agent]]$Class )
      updateTextInput(session, "num_agent", value = canvasObjects$agents[[Agent]]$NumAgent)

      if(length(names(canvasObjects$agents)) > 1){
        agents <- names(canvasObjects$agents)[which(names(canvasObjects$agents) != Agent)]

        updateSelectizeInput(session = session,inputId =  "id_agents_to_copy",
                             choices = agents, selected = "")
      }

      ##### updating all the agents tabs
      ## update table of entrance time ##
      # first remove all tabs
      updateCheckboxInput(session, inputId = "ckbox_entranceFlow", value = canvasObjects$agents[[Agent]]$entry_type)
      UpdatingTimeSlots_tabs(input,output,canvasObjects,InfoApp,session,canvasObjects$agents[[Agent]]$entry_type)

      ## Updating the flows tabs ##
      # first we have to remove all the tabs
      if(length(InfoApp$tabs_ids) >0){
        for( i in InfoApp$tabs_ids){
          removeTab(inputId = "DetFlow_tabs", target = i )
        }
        InfoApp$tabs_ids <- c()
      }

      InfoApp$NumTabsFlow = 0
      input$DetFlow_tabs

      #order the remaining flow of the agent and show in the correct order
      FlowTabs = canvasObjects$agents[[Agent]]$DeterFlow$FlowID
      if(length(FlowTabs)>0){
        for(NumFlow in order(unique(FlowTabs))){
          InfoApp$NumTabsFlow = InfoApp$NumTabsFlow +1
          appendTab(inputId = "DetFlow_tabs",
                    tabPanel(
                      paste0(substring(unique(FlowTabs)[NumFlow], 1, 1), " flow"),
                      uiOutput( paste0("UIDetFlows",Agent,"_",substring(unique(FlowTabs)[NumFlow], 1, 1), " flow") )
                    )
          )
          InfoApp$tabs_ids <- append(InfoApp$tabs_ids, unique(FlowTabs)[NumFlow])
        }
        showTab(inputId = "DetFlow_tabs", target = FlowTabs[order(FlowTabs)[1]])
      }else{
        appendTab(inputId = "DetFlow_tabs",
                  tabPanel(
                    paste0(1, " flow"),
                    uiOutput( paste0("UIDetFlows",Agent,"_",1, " flow") )
                  )
        )
        InfoApp$tabs_ids <- append(InfoApp$tabs_ids, "1 flow")
        rank_list_drag = rank_list(text = "Drag the rooms in the desired order",
                                   labels =  NULL,
                                   input_id = paste("list_detflow",Agent,paste0(1, " flow"),sep = "_")
        )
        output[[paste0("UIDetFlows",Agent,"_",1, " flow")]] <- renderUI({ rank_list_drag })

        showTab(inputId = "DetFlow_tabs", target = "1 flow")
        InfoApp$NumTabsFlow = 1
      }

      # InfoApp$NumTabsTimeSlot <- 0
      # if(!is.null(canvasObjects$agents[[Agent]]$EntryExitTime))
      #   InfoApp$NumTabsTimeSlot <- length(unique(canvasObjects$agents[[Agent]]$EntryExitTime$Name))

      ### END updating

      shinyjs::show(id = "rand_description")
      InfoApp$oldAgentType = canvasObjects$agents[[Agent]]$entry_type
    }
  })

  observeEvent(input$button_rm_agent,{
    disable("rds_generation")
    disable("flamegpu_connection")

    Agent <- input$id_new_agent
    if(Agent != "" && Agent %in% names(canvasObjects$agents)){
      if(InfoApp$NumTabsFlow > 0){
        flows = unique(canvasObjects$agents[[Agent]]$DeterFlow$FlowID)
        for(i in flows){
          removeTab(inputId = "DetFlow_tabs", target = i)
        }
      }
      InfoApp$NumTabsFlow = 0

      canvasObjects$agents[[Agent]]$EntryExitTime <- NULL
      UpdatingTimeSlots_tabs(input,output,canvasObjects,InfoApp,session,canvasObjects$agents[[Agent]]$entry_type)

      output$RandomEvents_table = DT::renderDataTable(
        DT::datatable(data.frame(Name=Agent, Room="Do nothing", Dist="Deterministic", Activity=1, ActivityLabel="Light", Time=0, Weight =1) %>% select(-c(Name, Activity)),
                      options = list(
                        columnDefs = list(list(className = 'dt-left', targets=0),
                                          list(className = 'dt-left', targets=1),
                                          list(className = 'dt-left', targets=2),
                                          list(className = 'dt-left', targets=3),
                                          list(className = 'dt-left', targets=4)),
                        pageLength = 5
                      ),
                      selection = 'single',
                      rownames = F,
                      colnames = c("Room", "Distribution", "Activity", "Time", "Weight")
        )
      )

      canvasObjects$agents <- canvasObjects$agents[-which(names(canvasObjects$agents) ==Agent)]
      canvasObjects$agents_whatif <- canvasObjects$agents_whatif %>%
        filter(Type != Agent)

      if(length(names(canvasObjects$agents)) == 0){
        canvasObjects$agents <- NULL
        canvasObjects$agents_whatif <- data.frame(
                                         Measure = character(),
                                         Type = character(),
                                         Parameters = character(),
                                         From = numeric(),
                                         To = numeric(),
                                         stringsAsFactors = FALSE
                                       )
        updateSelectizeInput(session, inputId = "id_new_agent", choices = "", selected = "")

        updateSelectizeInput(session = session, "agent_mask",
                             choices = "")

        updateSelectizeInput(session = session, "agent_vaccination",
                             choices = "")

        updateSelectizeInput(session = session, "agent_swab",
                             choices = "")

        updateSelectizeInput(session = session, "agent_quarantine",
                             choices = "")

        updateSelectizeInput(session = session, "agent_external_screening",
                             choices = "")

        updateSelectizeInput(session = session, "agent_initial_infected",
                             choices = "")
      }
      else{
        updateSelectizeInput(session, inputId = "id_new_agent", choices = names(canvasObjects$agents), selected = "")

        updateSelectizeInput(session = session, "agent_mask",
                             choices = c("", names(canvasObjects$agents)))

        updateSelectizeInput(session = session, "agent_vaccination",
                             choices = c("", names(canvasObjects$agents)))

        updateSelectizeInput(session = session, "agent_swab",
                             choices = c("", names(canvasObjects$agents)))

        updateSelectizeInput(session = session, "agent_quarantine",
                             choices = c("", names(canvasObjects$agents)))

        updateSelectizeInput(session = session, "agent_external_screening",
                             choices = c("", names(canvasObjects$agents)))

        updateSelectizeInput(session = session, "agent_initial_infected",
                             choices = c("", names(canvasObjects$agents)))
      }

      for(i in 1:length(canvasObjects$resources)){
        canvasObjects$resources[[i]]$roomResource <- canvasObjects$resources[[i]]$roomResource[, which(!names(canvasObjects$resources[[i]]$roomResource) == Agent)]
        canvasObjects$resources[[i]]$waitingRoomsRand[which(!canvasObjects$resources[[i]]$waitingRoomsRand$Agent == Agent),]
        canvasObjects$resources[[i]]$waitingRoomsDeter[which(!canvasObjects$resources[[i]]$waitingRoomsDeter$Agent == Agent),]
      }

      if(nrow(canvasObjects$agents_whatif) > 0)
        canvasObjects$agents_whatif <- canvasObjects$agents_whatif %>% filter(Type != Agent)

      if(nrow(canvasObjects$initial_infected) > 0)
        canvasObjects$initial_infected <- canvasObjects$initial_infected %>% filter(Type != Agent)
    }
  })

  input_num_agent <- debounce(reactive({input$num_agent}), 1000L)

  observeEvent(input_num_agent(),{
    disable("rds_generation")
    disable("flamegpu_connection")
    NumAgent = gsub(" ", "", input$num_agent)

    if(input$id_new_agent != ""){
      if(NumAgent == "" || !grepl("(^[0-9]+).*", NumAgent) || is.na(as.integer(NumAgent)) || as.integer(NumAgent) < 0){
        shinyalert("You must insert a positive integer value.")
        return()
      }

      if(!is.null(canvasObjects$agents)){
        canvasObjects$agents[[input$id_new_agent]]$NumAgent = NumAgent
      }
    }
  })

  observeEvent(input$button_copy_agent,{
    disable("rds_generation")
    disable("flamegpu_connection")
    if(input$id_agents_to_copy == ""){
      shinyalert("You must select an agent to copy.")
      return()
    }
    if(canvasObjects$agents[[input$id_agents_to_copy]]$Class== ""){
      shinyalert("You must select a valid class for the new agent.")
      return()
    }
    Agent <- input$id_new_agent
    canvasObjects$agents[[Agent]] = canvasObjects$agents[[input$id_agents_to_copy]]
    if(nrow(canvasObjects$agents[[Agent]]$DeterFlow) > 0)
      canvasObjects$agents[[Agent]]$DeterFlow$Name = Agent
    if(nrow(canvasObjects$agents[[Agent]]$RandFlow) > 0)
      canvasObjects$agents[[Agent]]$RandFlow$Name = Agent

    new_agent_whatif <- canvasObjects$agents_whatif %>%
      filter(Type == input$id_agents_to_copy)

    if(nrow(new_agent_whatif) > 0){
      new_agent_whatif$Type <- Agent

      canvasObjects$agents_whatif <- canvasObjects$agents_whatif %>%
        filter(Type != Agent)
      canvasObjects$agents_whatif <- rbind(canvasObjects$agents_whatif, new_agent_whatif)
    }

    updateSelectizeInput(session = session,inputId ="id_class_agent",
                         selected = canvasObjects$agents[[Agent]]$Class)
    updateTextInput(session, "num_agent", value = canvasObjects$agents[[Agent]]$NumAgent )

    ##### updating all the agents tabs
    ## update table of entrance time ##
    # first remove all tabs

    UpdatingTimeSlots_tabs(input,output,canvasObjects,InfoApp,session,canvasObjects$agents[[Agent]]$entry_type)

    ## Updating the flows tabs ##
    # first we have to remove all the tabs (keeping the first one)
    if(length(InfoApp$tabs_ids) >0){
      for( i in InfoApp$tabs_ids) removeTab(inputId = "DetFlow_tabs", target = i )
      InfoApp$tabs_ids <- c()
    }
    InfoApp$NumTabsFlow = 0

    FlowTabs = canvasObjects$agents[[Agent]]$DeterFlow$FlowID
    if(length(FlowTabs)>0){
      for(NumFlow in 1:length(unique(FlowTabs))){
        InfoApp$NumTabsFlow = InfoApp$NumTabsFlow +1
        appendTab(inputId = "DetFlow_tabs",
                  tabPanel(
                    paste0(NumFlow, " flow"),
                    uiOutput( paste0("UIDetFlows",Agent,"_",NumFlow, " flow") )
                  )
        )
        InfoApp$tabs_ids <- append(InfoApp$tabs_ids, unique(FlowTabs)[NumFlow])
      }
      showTab(inputId = "DetFlow_tabs", target =  FlowTabs[order(FlowTabs)[1]])
    }else{
      appendTab(inputId = "DetFlow_tabs",
                tabPanel(
                  paste0(1, " flow"),
                  uiOutput( paste0("UIDetFlows",Agent,"_",1, " flow") )
                )
      )
      InfoApp$tabs_ids <- append(InfoApp$tabs_ids, "1 flow")
      rank_list_drag = rank_list(text = "Drag the rooms in the desired order",
                                 labels =  NULL,
                                 input_id = paste("list_detflow",Agent,paste0(1, " flow"),sep = "_")
      )
      output[[paste0("UIDetFlows",Agent,"_",1, " flow")]] <- renderUI({ rank_list_drag })

      showTab(inputId = "DetFlow_tabs", target = "1 flow")
      InfoApp$NumTabsFlow = 1

    }
    InfoApp$oldAgentType = canvasObjects$agents[[Agent]]$entry_type

    ### END updating
  })

  observeEvent(input$id_class_agent,{
    disable("rds_generation")
    disable("flamegpu_connection")
    if(input$id_new_agent != "")
      canvasObjects$agents[[input$id_new_agent]]$Class = input$id_class_agent
  })

  #### Determined flow ####
  observeEvent(input$add_room_to_det_flow,{
    disable("rds_generation")
    disable("flamegpu_connection")
    if(!is.null(canvasObjects$agents)){
      name = input$id_new_agent
      new_room = input$Det_select_room_flow

      det_flow <- check_distribution_parameters(input, "det_flow")
      new_dist <- det_flow[[1]]
      new_time <- det_flow[[2]]

      if(is.null(new_dist) || is.null(new_time))
        return()

      activity = switch(input$DetActivity,
                        "Very Light - e.g. resting" = 1,
                        "Light - e.g. speak while resting" = 1.7777,
                        "Quite Hard - e.g. speak/walk while standing" = 2.5556,
                        "Hard - e.g. loudly speaking"= 6.1111)
      activityLabel = switch(input$DetActivity,
                             "Very Light - e.g. resting" = "Very Light",
                             "Light - e.g. speak while resting" = "Light",
                             "Quite Hard - e.g. speak/walk while standing" = "Quite Hard",
                             "Hard - e.g. loudly speaking"= "Hard")
      FlowID = input$DetFlow_tabs

      if(is.null(FlowID)){
        shinyalert("You must select a flow.")
        return()
      }

      if(input$DetActivity == ""){
        shinyalert("You must specify an activity.")
        return()
      }

      if(new_room != "" && new_time != ""){
        agentsOLD = canvasObjects$agents[[name]]$DeterFlow
        agentsOLD_filter = agentsOLD[agentsOLD$FlowID == FlowID,]
        agent = data.frame(Name = name,
                           Room = new_room,
                           Dist = new_dist,
                           Time = new_time,
                           Flow = length(agentsOLD_filter[,"Flow"])+1,
                           Activity = activity,
                           Label = paste0(new_room, " - ",new_dist, " ", new_time, " min", " - ",activityLabel),
                           FlowID = FlowID )
        if(agent$Label %in% agentsOLD_filter[,"Label"])
        {
          agent$Label <- paste0("(",length(grep(x= agentsOLD_filter[,"Label"],pattern = agent$Label))+1,") ", agent$Label)
        }
        canvasObjects$agents[[name]]$DeterFlow = rbind(agentsOLD, agent)
      }
    }
  })

  #updating the list of rooms in determined flow
  observe({
    input$id_new_agent -> agentID
    input$DetFlow_tabs -> IDDetFlow_tabs

    if(!grepl("^[a-zA-Z0-9_]+$", agentID)){
      return()
    }

    if(!is.null(canvasObjects$agents) && agentID != "" && !is.null(canvasObjects$agents[[input$id_new_agent]]$DeterFlow) && nrow(canvasObjects$agents[[input$id_new_agent]]$DeterFlow) >= 0 && !is.null(IDDetFlow_tabs)){
      agent <- canvasObjects$agents[[agentID]]$DeterFlow %>% filter( FlowID == IDDetFlow_tabs)

      if(length(agent$Room) != 0){
        rank_list_drag = rank_list(text = "Drag the rooms in the desired order",
                                   labels =  agent$Label[agent$Flow],
                                   input_id = paste("list_detflow",agentID,IDDetFlow_tabs,sep = "_")
        )
      }
      else{
        rank_list_drag = rank_list(text = "Drag the rooms in the desired order",
                                   labels =  NULL,
                                   input_id = paste("list_detflow",agentID,IDDetFlow_tabs,sep = "_")
        )
      }

      output[[paste0("UIDetFlows",agentID,"_",input$DetFlow_tabs)]] <- renderUI({ rank_list_drag })
    }
  })

  observeEvent(input$add_det_flow,{
    disable("rds_generation")
    disable("flamegpu_connection")
    input$id_new_agent -> agentID


    if(!is.null(canvasObjects$agents) && agentID != ""){
      #if the agent has already det flow the new flow will be greatest flow + 1
      if(nrow(canvasObjects$agents[[agentID]]$DeterFlow) > 0  && !is.null(canvasObjects$agents[[agentID]]$DeterFlow)){
        FlowTabs = canvasObjects$agents[[agentID]]$DeterFlow$FlowID
        NumFlow = as.numeric(substring(FlowTabs[order(FlowTabs, decreasing = TRUE)[1]], 1, 1))
      }
      #else just add one on the tab number
      else {
        NumFlow = InfoApp$NumTabsFlow
      }

      InfoApp$tabs_ids <- append(InfoApp$tabs_ids, paste0(NumFlow+1, " flow"))

      if(NumFlow > 0){
        NumFlow = NumFlow + 1
        appendTab(inputId = "DetFlow_tabs",
                  tabPanel(
                    paste0(NumFlow, " flow"),
                    uiOutput( paste0("UIDetFlows",agentID,"_",NumFlow, " flow") )
                  )
        )

        rank_list_drag = rank_list(text = "Drag the rooms in the desired order",
                                   labels =  NULL,
                                   input_id = paste("list_detflow",agentID,paste0(NumFlow, " flow"),sep = "_")
        )

        output[[paste0("UIDetFlows",agentID,"_",NumFlow, " flow")]] <- renderUI({ rank_list_drag })

        showTab(inputId = "DetFlow_tabs", target = paste0(NumFlow, " flow") )
        InfoApp$NumTabsFlow = InfoApp$NumTabsFlow + 1

        selectToUpdate = grep(pattern = "Select_TimeDetFlow_",x = names(input),value = T)
        for(i in selectToUpdate){
          selected <- input[[i]]
          updateSelectInput(session = session,inputId = i, selected=selected, choices = InfoApp$tabs_ids)
        }
      }

    }
  })

  observeEvent(input$rm_det_flow, {
    disable("rds_generation")
    disable("flamegpu_connection")
    if(InfoApp$NumTabsFlow >= 1){
      if(InfoApp$NumTabsFlow > 1){
        removeTab( inputId = "DetFlow_tabs", target =  input$DetFlow_tabs, session = session)
        InfoApp$tabs_ids <- InfoApp$tabs_ids[!InfoApp$tabs_ids%in%c(input$DetFlow_tabs)]
      }

      flowrm = gsub(pattern = " flow", replacement = "", x = input$DetFlow_tabs)
      InfoApp$NumTabsFlow = InfoApp$NumTabsFlow - 1

      Agent <- input$id_new_agent
      if(Agent != ""){
        AgentInfo <- canvasObjects$agents[[Agent]]

        AgentInfo$DeterFlow <- AgentInfo$DeterFlow[which(!AgentInfo$DeterFlow$FlowID == paste0(flowrm, " flow")),]
        AgentInfo$EntryExitTime <- AgentInfo$EntryExitTime[which(!AgentInfo$EntryExitTime$FlowID == paste0(flowrm, " flow")),]

        canvasObjects$agents[[Agent]] <- AgentInfo

        selectToUpdate = grep(pattern = "Select_TimeDetFlow_",x = names(input),value = T)
        UpdatingTimeSlots_tabs(input,output,canvasObjects,InfoApp,session,canvasObjects$agents[[Agent]]$entry_type)
        for(i in selectToUpdate) updateSelectInput(session = session,inputId = i, choices = InfoApp$tabs_ids)
      }
    }
  })

  observeEvent(input$remove_room_to_det_flow,{
    disable("rds_generation")
    disable("flamegpu_connection")
    if(!is.null(canvasObjects$agents) && input$id_new_agent != ""){
      agent <- canvasObjects$agents[[input$id_new_agent]]$DeterFlow
      input[[paste("list_detflow", input$id_new_agent,input$DetFlow_tabs,sep = "_") ]] -> list_detflow
      if(length(list_detflow) > 0 &&
         length(agent$Room) > 0){
        # find the last room in the selected flow id
        max(which(canvasObjects$agents[[ input$id_new_agent ]]$DeterFlow$FlowID == input$DetFlow_tabs )) -> nrow
        if(nrow>1){
          canvasObjects$agents[[ input$id_new_agent ]]$DeterFlow <- canvasObjects$agents[[ input$id_new_agent ]]$DeterFlow[-nrow,]
        }else{
          canvasObjects$agents[[ input$id_new_agent ]]$DeterFlow <- data.frame(Name=character(0), Room=character(0), Time=numeric(0), Flow =numeric(0), Activity = numeric(0),
                                                                               Label = character(0), FlowID = character(0) )
        }
      }
    }
  })

  observe({
    namesDetFlows = paste("list_detflow", input$id_new_agent,input$DetFlow_tabs,sep = "_")
    input[[namesDetFlows]]

    if(!grepl("^[a-zA-Z0-9_]+$", input$id_new_agent)){
      return()
    }

    if(!is.null(canvasObjects$agents) && input$id_new_agent != "" && !is.null(canvasObjects$agents[[input$id_new_agent]]$DeterFlow) && nrow(canvasObjects$agents[[input$id_new_agent]]$DeterFlow) >= 0 && !is.null(input$DetFlow_tabs)){
      isolate({
        agent <- canvasObjects$agents[[input$id_new_agent]]$DeterFlow %>% filter(FlowID == input$DetFlow_tabs)
        DeterFlow_tmp = canvasObjects$agents[[input$id_new_agent]]$DeterFlow %>% filter(FlowID != input$DetFlow_tabs)
        input[[namesDetFlows]] -> list_detflow
        if(!is.null(list_detflow) &&
           length(agent$Room) > 0 &&
           length(list_detflow) == length(agent$Label) ){
          newOrder = data.frame(Name = input$id_new_agent,
                                Label = list_detflow,
                                Flow = 1:length(list_detflow) )
          DeterFlow = merge(agent %>% select(-Flow), newOrder) %>%
            select(Name,Room,Dist, Time, Flow, Activity,  Label,FlowID) %>% arrange(Flow)
          canvasObjects$agents[[ input$id_new_agent ]]$DeterFlow = rbind(DeterFlow_tmp,DeterFlow)
        }
      })
    }
  })

  #### Random flow ####

  observeEvent(input$add_room_to_rand_flow,{
    disable("rds_generation")
    disable("flamegpu_connection")
    name = input$id_new_agent
    agent = canvasObjects$agents[[name]]$RandFlow

    activity = switch(input$RandActivity,
                      "Very Light - e.g. resting" = 1,
                      "Light - e.g. speak while resting" = 1.7777,
                      "Quite Hard - e.g. speak/walk while standing" = 2.5556,
                      "Hard - e.g. loudly speaking"= 6.1111)
    activityLabel = switch(input$RandActivity,
                           "Very Light - e.g. resting" = "Very Light",
                           "Light - e.g. speak while resting" = "Light",
                           "Quite Hard - e.g. speak/walk while standing" = "Quite Hard",
                           "Hard - e.g. loudly speaking"= "Hard")

    if(is.null(canvasObjects$agents[[name]])){
      shinyalert("You should define an agent.")
      return()
    }

    if(input$RandActivity == ""){
      shinyalert("You must specify an activity.")
      return()
    }

    if(input$RandWeight == "" ||
       (as.double(as.numeric(gsub(",", "\\.", input$RandWeight)))<=0 ||
        as.double((as.numeric(gsub(",", "\\.", input$RandWeight))))>=1) ){
      shinyalert("You must specify a weight between 0 and 1.")
      return()
    }

    rand_flow <- check_distribution_parameters(input, "rand_flow")
    new_dist <- rand_flow[[1]]
    new_time <- rand_flow[[2]]

    if(is.null(new_dist) || is.null(new_time))
      return()

    sumweights = as.numeric(gsub(",", "\\.", input$RandWeight)) + sum(as.numeric(canvasObjects$agents[[name]]$RandFlow$Weight)) - as.numeric(canvasObjects$agents[[name]]$RandFlow[canvasObjects$agents[[name]]$RandFlow$Room == "Do nothing","Weight"])

    if(sumweights <= 0 && sumweights > 1 ){
      shinyalert("The sum of weight of going anywhere must not greater (>=) than 1.")
      return()
    }

    canvasObjects$agents[[name]]$RandFlow[canvasObjects$agents[[name]]$RandFlow$Room == "Do nothing","Weight"] = 1 - sumweights #round(1-sumweights,digits = 4)


    if(input$Rand_select_room_flow != "" ){

      newOrder = data.frame(Name = name,
                            Room= input$Rand_select_room_flow,
                            Dist = new_dist,
                            Time = new_time,
                            Activity = activity,
                            ActivityLabel = activityLabel,
                            Weight = gsub(",", "\\.", as.numeric(input$RandWeight))
      )
      canvasObjects$agents[[name]]$RandFlow = rbind(canvasObjects$agents[[name]]$RandFlow,newOrder)
    }

    output$RandomEvents_table = DT::renderDataTable(
      DT::datatable(canvasObjects$agents[[name]]$RandFlow %>% select(-c(Name, Activity)),
                    options = list(
                      columnDefs = list(list(className = 'dt-left', targets=0),
                                        list(className = 'dt-left', targets=1),
                                        list(className = 'dt-left', targets=2),
                                        list(className = 'dt-left', targets=3),
                                        list(className = 'dt-left', targets=4)),
                      pageLength = 5
                    ),
                    selection = 'single',
                    rownames = F,
                    colnames = c("Room", "Distribution", "Activity", "Time", "Weight")
      )
    )

  })

  #aggiorna la visualizzazione di RandomEvents_table quando cambia l'agent
  observe({
    if(!is.null(canvasObjects$agents) && input$id_new_agent != ""){
      agent <- canvasObjects$agents[[input$id_new_agent]]$RandFlow
      if(length(agent$Room) != 0){
        output$RandomEvents_table = DT::renderDataTable(
          DT::datatable(agent %>% select(-c(Name, Activity)),
                        options = list(
                          columnDefs = list(list(className = 'dt-left', targets=0),
                                            list(className = 'dt-left', targets=1),
                                            list(className = 'dt-left', targets=2),
                                            list(className = 'dt-left', targets=3),
                                            list(className = 'dt-left', targets=4)),
                          pageLength = 5
                        ),
                        selection = 'single',
                        rownames = F,
                        colnames = c("Room", "Distribution", "Activity", "Time", "Weight")
          )
        )
      }
    }
  })

  observeEvent(input$RandomEvents_table_cell_clicked, {
    info <- input$RandomEvents_table_cell_clicked
    req(input$id_new_agent!= "")

    if (!is.null(info$row)) {
      if(info$row %in% which(canvasObjects$agents[[input$id_new_agent]]$RandFlow$Room == "Do nothing")){
        shinyalert(" 'Do nothing' event cannot be removed. ",type = "error")
        return()
      }else{
        shinyalert(
          title = "Delete Entry?",
          text = "Are you sure you want to delete this row?",
          type = "warning",
          showCancelButton = TRUE,
          confirmButtonText = "Yes, delete it!",
          callbackR = function(x) {
            if (x) {
              canvasObjects$agents[[input$id_new_agent]]$RandFlow$Weight[[which(canvasObjects$agents[[input$id_new_agent]]$RandFlow$Room == "Do nothing" )]] <-as.numeric(gsub(",", "\\.", canvasObjects$agents[[input$id_new_agent]]$RandFlow$Weight[[which(canvasObjects$agents[[input$id_new_agent]]$RandFlow$Room == "Do nothing" )]])) + as.numeric(gsub(",", "\\.", canvasObjects$agents[[input$id_new_agent]]$RandFlow$Weight[[info$row]]))
              canvasObjects$agents[[input$id_new_agent]]$RandFlow <- canvasObjects$agents[[input$id_new_agent]]$RandFlow[-info$row,]
            }
          }
        )
      }
    }
  })

  #### entry/exit flow ####

  observeEvent(input$ckbox_entranceFlow,{
    disable("rds_generation")
    disable("flamegpu_connection")

    if(!is.null(canvasObjects$agents) && is.null(canvasObjects$agents[[input$id_new_agent]]$EntryExitTime) && InfoApp$NumTabsFlow == 1){
      Agent <- input$id_new_agent

      InfoApp$oldAgentType = canvasObjects$agents[[Agent]]$entry_type
      canvasObjects$agents[[Agent]]$entry_type <- input$ckbox_entranceFlow


      selectToUpdate = grep(pattern = "Select_TimeDetFlow_",x = names(input),value = T)
      UpdatingTimeSlots_tabs(input,output,canvasObjects,InfoApp,session,canvasObjects$agents[[Agent]]$entry_type)
      for(i in selectToUpdate) updateSelectInput(session = session,inputId = i, choices = InfoApp$tabs_ids)

      return()
    }

    if(!canvasObjects$cancel_button_selected && input$id_new_agent != "" && input$ckbox_entranceFlow != canvasObjects$agents[[input$id_new_agent]]$entry_type){
      showModal(modalDialog(
        title = "Important message",
        "Do you want to update all the agent's time slot information? Existing data will be overwritten, and if you select 'Daily Rate,' only the first flow will be retained.",
        easyClose = TRUE,
        footer= tagList(actionButton("confirmUpdates", "Update"),
                        actionButton("cancelAction", "Cancel")
        )
      ))
    }

    if(canvasObjects$cancel_button_selected)
      canvasObjects$cancel_button_selected = FALSE
  })

  observeEvent(input$cancelAction,{
    disable("rds_generation")
    disable("flamegpu_connection")
    canvasObjects$cancel_button_selected = TRUE
    updateCheckboxInput(session, inputId = "ckbox_entranceFlow",value = canvasObjects$agents[[input$id_new_agent]]$entry_type)
    removeModal()
  })

  observeEvent(input$confirmUpdates,{
    disable("rds_generation")
    disable("flamegpu_connection")
    input$id_new_agent-> Agent
    InfoApp$oldAgentType = canvasObjects$agents[[Agent]]$entry_type
    canvasObjects$agents[[Agent]]$entry_type <- input$ckbox_entranceFlow
    canvasObjects$agents[[Agent]]$EntryExitTime <- NULL

    FlowIDs <- 2:InfoApp$NumTabsFlow
    if(InfoApp$NumTabsFlow > 1){
      removeTab(inputId = "DetFlow_tabs", target=paste0(FlowIDs, " flow"), session = session)
      InfoApp$tabs_ids <- InfoApp$tabs_ids[!InfoApp$tabs_ids %in% FlowIDs]

      InfoApp$NumTabsFlow = 1
    }

    Agent <- input$id_new_agent
    if(Agent != ""){
      AgentInfo <- canvasObjects$agents[[Agent]]

      AgentInfo$DeterFlow <- AgentInfo$DeterFlow[which(!AgentInfo$DeterFlow$FlowID != "1 flow"),]

      canvasObjects$agents[[Agent]] <- AgentInfo

      selectToUpdate = grep(pattern = "Select_TimeDetFlow_",x = names(input),value = T)
      UpdatingTimeSlots_tabs(input,output,canvasObjects,InfoApp,session,canvasObjects$agents[[Agent]]$entry_type)
      for(i in selectToUpdate) updateSelectInput(session = session,inputId = i, choices = InfoApp$tabs_ids)
    }

    removeModal()
  })

  observeEvent(input$add_slot, {
    disable("rds_generation")
    disable("flamegpu_connection")

    NumTabs = as.numeric(max(c(0, InfoApp$NumTabsTimeSlot)))+1
    InfoApp$NumTabsTimeSlot = c(InfoApp$NumTabsTimeSlot,NumTabs)
    appendTab(inputId = "Time_tabs",
              tabPanel(paste0(NumTabs," slot"),
                       value = paste0(NumTabs," slot"),
                       column(7,
                              textInput(inputId = paste0("EntryTime_",NumTabs), label = "Entry time:", placeholder = "hh:mm"),
                              if(length(canvasObjects$agents[[input$id_new_agent]]$DeterFlow$FlowID)>0){
                                selectInput(inputId = paste0("Select_TimeDetFlow_",NumTabs),
                                            label = "Associate with a determined flow:",
                                            choices = sort(unique(canvasObjects$agents[[input$id_new_agent]]$DeterFlow$FlowID)) )
                              }else{
                                selectInput(inputId = paste0("Select_TimeDetFlow_",NumTabs),
                                            label = "Associate with a determined flow:",
                                            choices = "1 flow")
                              }
                       ),
                       column(5,
                              checkboxGroupInput(paste0("selectedDays_",NumTabs), "Select Days of the Week",
                                                 choices = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                                                 selected = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
                              )

                       )
              )
    )
  })

  observeEvent(input$add_slot_rate, {
    disable("rds_generation")
    disable("flamegpu_connection")
    NumTabs = as.numeric(max(c(0, InfoApp$NumTabsTimeSlot)))+1
    InfoApp$NumTabsTimeSlot = c(InfoApp$NumTabsTimeSlot,NumTabs)
    appendTab(inputId = "Rate_tabs",
              tabPanel(paste0(NumTabs," slot"),
                       value = paste0(NumTabs," slot"),
                       tags$b("Entrance rate:"),
                       get_distribution_panel(paste0("daily_rate_", NumTabs)),
                       #textInput(inputId = paste0("EntranceRate_", NumTabs), label = "Entrance rate:", placeholder = "Daily entrance rate", value = ""),
                       column(7,
                              textInput(inputId = paste0("EntryTimeRate_",NumTabs), label = "Entry time:", placeholder = "hh:mm"),
                              textInput(inputId = paste0("ExitTimeRate_",NumTabs), label = "Exit time:", placeholder = "hh:mm"),

                       ),
                       column(5,
                              checkboxGroupInput(paste0("selectedDaysRate_",NumTabs), "Select Days of the Week",
                                                 choices = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                                                 selected = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
                              )

                       )
              )
    )
  })

  observeEvent(input$rm_slot, {
    disable("rds_generation")
    disable("flamegpu_connection")
    if(length(InfoApp$NumTabsTimeSlot)>1){
      removeTab( inputId = "Time_tabs", target =  input$Time_tabs, session = session)
      slotrm = gsub(pattern = " slot", replacement = "", x = input$Time_tabs)
      InfoApp$NumTabsTimeSlot = InfoApp$NumTabsTimeSlot[which(InfoApp$NumTabsTimeSlot!=slotrm)]

      Agent <- input$id_new_agent
      if(Agent != ""){
        AgentInfo <- canvasObjects$agents[[Agent]]

        AgentInfo$EntryExitTime <- AgentInfo$EntryExitTime[which(!AgentInfo$EntryExitTime$Name == paste0(slotrm, " slot")),]

        canvasObjects$agents[[Agent]] <- AgentInfo
      }
    }
  })

  observeEvent(input$rm_slot_rate, {
    disable("rds_generation")
    disable("flamegpu_connection")
    if(length(InfoApp$NumTabsTimeSlot)>1){
      removeTab( inputId = "Rate_tabs", target =  input$Rate_tabs, session = session)
      slotrm = gsub(pattern = " slot", replacement = "", x = input$Rate_tabs)
      InfoApp$NumTabsTimeSlot = InfoApp$NumTabsTimeSlot[which(InfoApp$NumTabsTimeSlot!=slotrm)]

      Agent <- input$id_new_agent
      if(Agent != ""){
        AgentInfo <- canvasObjects$agents[[Agent]]

        AgentInfo$EntryExitTime <- AgentInfo$EntryExitTime[which(!AgentInfo$EntryExitTime$Name == paste0(slotrm, " slot")),]

        canvasObjects$agents[[Agent]] <- AgentInfo
      }
    }
  })



  observeEvent(input$set_timeslot,{
    disable("rds_generation")
    disable("flamegpu_connection")
    if(is.null(canvasObjects$agents)){
      shinyalert("You should define an agent.")
      return()
    }

    if(input$ckbox_entranceFlow == "Daily Rate"){
      indexes =  InfoApp$NumTabsTimeSlot

      for(index in indexes){
        daily_rate <- check_distribution_parameters(input, paste0("daily_rate_", index))
        new_dist <- daily_rate[[1]]
        new_time <- daily_rate[[2]]

        if(is.null(new_dist) || is.null(new_time))
          return()


        EntryTimeRate <- input[[paste0("EntryTimeRate_",index)]]
        ExitTimeRate <- input[[paste0("ExitTimeRate_",index)]]
        if(!any(sapply(list(EntryTimeRate,
                            ExitTimeRate,
                            input[[paste0("selectedDaysRate_",index)]]), is.null))){
          if(EntryTimeRate == "" || ExitTimeRate == ""){
            shinyalert("You should define the Entry and the Exit time.")
            return()
          }
          if(EntryTimeRate != ""){
            if (! (grepl("^([01]?[0-9]|2[0-3]):[0-5][0-9]$", EntryTimeRate) || grepl("^\\d{1,2}$", EntryTimeRate)) )
            {
              shinyalert("The format of the time should be: hh:mm (e.g. 06:15, or 20).")
              return()
            }
          }
          if(grepl("^\\d{1,2}$", EntryTimeRate)){
            EntryTimeRate <- paste0(EntryTimeRate,":00")
          }

          if(ExitTimeRate != ""){
            if (! (grepl("^([01]?[0-9]|2[0-3]):[0-5][0-9]$",ExitTimeRate) || grepl("^\\d{1,2}$",ExitTimeRate)) )
            {
              shinyalert("The format of the time should be: hh:mm (e.g. 06:15, or 20:30)ò")
              return()
            }
          }
          if(grepl("^\\d{1,2}$", ExitTimeRate)){
            ExitTimeRate <- paste0(ExitTimeRate,":00")
          }
          #check if the number before : in EntryTime is lower than number before : in ExitTime
          if(as.numeric(strsplit(input[[paste0("EntryTimeRate_", index)]], ":")[[1]][1]) > as.numeric(strsplit(input[[paste0("ExitTimeRate_", index)]], ":")[[1]][1])) {
            shinyalert("The Entry time should be lower than the Exit timeò")
            return()
          }
          if (as.numeric(strsplit(input[[paste0("EntryTimeRate_", index)]], ":")[[1]][1]) == as.numeric(strsplit(input[[paste0("ExitTimeRate_", index)]], ":")[[1]][1]) &&
              as.numeric(strsplit(input[[paste0("EntryTimeRate_", index)]], ":")[[1]][2]) > as.numeric(strsplit(input[[paste0("ExitTimeRate_", index)]], ":")[[1]][2])) {
            shinyalert("The Entry time should be lower than the Exit time.")
            return()
          }
          #check if

          if(EntryTimeRate  != "" && ExitTimeRate != ""){
            df = data.frame(Name = paste0(index, " slot"),
                            EntryTime = EntryTimeRate ,
                            ExitTime = ExitTimeRate,
                            RateDist = new_dist,
                            RateTime = new_time,
                            Days = input[[paste0("selectedDaysRate_",index)]])
          }else{
            df = data.frame(Name = paste0(index, " slot"),
                            EntryTime = NA ,
                            ExitTime = NA,
                            RateDist = NA,
                            RateTime = NA,
                            Days = NA)
          }

          new_entry_time = unique(df$EntryTime)
          new_exit_time = unique(df$ExitTime)

          if(!is.null(canvasObjects$agents[[input$id_new_agent]]$EntryExitTime) && is.data.frame(canvasObjects$agents[[input$id_new_agent]]$EntryExitTime))
            canvasObjects$agents[[input$id_new_agent]]$EntryExitTime = rbind(canvasObjects$agents[[input$id_new_agent]]$EntryExitTime %>% filter(Name !=  paste0(index, " slot")),df)
          else
            canvasObjects$agents[[input$id_new_agent]]$EntryExitTime =  df

          canvasObjects$agents[[input$id_new_agent]]$EntryExitTime -> EntryExitTime
          #check if df$Name is present in EntryExitTime$Name
          if(!is.null(EntryExitTime) && is.data.frame(EntryExitTime)){
            #check if df$Days is present in EntryExitTime$Days
            if(nrow(EntryExitTime %>% filter(Name!= paste0(index, " slot")) %>% filter(Days %in%  df$Days)) > 0){
              #check if in the same day there is a time slot that collides with the new one
              if(nrow(EntryExitTime %>% filter(Name!= paste0(index, " slot")) %>% filter(Days %in%  df$Days) %>% filter(EntryTime < new_exit_time & ExitTime > new_entry_time)) > 0){
                shinyalert("The time slot you are trying to add collides with another time slot.")
                return()
              }

            }
          }
        }
      }

    }else{
      indexes =  InfoApp$NumTabsTimeSlot

      for(index in indexes){
        EntryTime <- input[[paste0("EntryTime_",index)]]
        if(!any(sapply(list(EntryTime,
                            input[[paste0("selectedDays_",index)]]), is.null))){
          if(EntryTime == ""){
            shinyalert("You should define the entry time.")
            return()
          }
          if(EntryTime != ""){
            if (! (grepl("^([01]?[0-9]|2[0-3]):[0-5][0-9]$", EntryTime) || grepl("^\\d{1,2}$", EntryTime)) )
            {
              shinyalert("The format of the time should be: hh:mm (e.g. 06:15, or 20).")
              return()
            }
          }
          if(grepl("^\\d{1,2}$", EntryTime)){
            EntryTime <- paste0(EntryTime,":00")
          }

          if(EntryTime  != ""){
            df = data.frame(Name = paste0(index, " slot"),
                            EntryTime = EntryTime ,
                            Days = input[[paste0("selectedDays_",index)]],
                            FlowID = input[[paste0("Select_TimeDetFlow_",index)]])
          }else{
            df = data.frame(Name = paste0(index, " slot"),
                            EntryTime = NA ,
                            Days = NA,
                            FlowID = NA)
          }


          if(!is.null(canvasObjects$agents[[input$id_new_agent]]$EntryExitTime) && is.data.frame(canvasObjects$agents[[input$id_new_agent]]$EntryExitTime))
            canvasObjects$agents[[input$id_new_agent]]$EntryExitTime = rbind(canvasObjects$agents[[input$id_new_agent]]$EntryExitTime %>% filter(Name !=  paste0(index, " slot")), df)
          else
            canvasObjects$agents[[input$id_new_agent]]$EntryExitTime =  df
        }
      }
    }

    print(canvasObjects$agents[[input$id_new_agent]]$EntryExitTime)
    removeModal()
  })

  #### Resources ####
  #Show resources and change value

  get_agents_with_room_type <- function(room_type) {
    agents_with_room_type <- c()
    for (agent_name in names(canvasObjects$agents)) {
      if (check_room_type_in_agent_flow(agent_name, room_type)) {
        agents_with_room_type <- c(agents_with_room_type, agent_name)
      }
    }
    return(agents_with_room_type)
  }

  # Updating the resources_value inside the resource dataframe
  # Reactive expression to gather all rooms from Flow$Room

  allResRooms <- reactive({
    do.call(rbind,
            lapply(names(canvasObjects$agents), function(agent) {
              rooms = unique(canvasObjects$agents[[agent]]$DeterFlow$Room,
                             canvasObjects$agents[[agent]]$RandFlow$Room)
              if(length(rooms)>0){
                df_Rand <- canvasObjects$agents[[agent]]$RandFlow %>%
                  filter(Room != "Do nothing")
                if(dim(df_Rand)[1] > 0){
                  rbind(
                    data.frame(Agent = agent , Room = canvasObjects$agents[[agent]]$DeterFlow$Room, Flow = "Deter"),
                    data.frame(Agent = agent , Room = df_Rand$Room, Flow = "Rand")
                  )
                }else{
                  NULL
                }
              }
              else NULL
            })
    )
  })

  output$selectInput_alternative_resources_global <- renderUI({
    # Generate selectizeInput for each relevant agent
    choicesRoom = c("Same room", "Skip room")

    if(!is.null(canvasObjects$roomsINcanvas)){
      rooms = canvasObjects$roomsINcanvas %>%
        select(type, Name, area) %>%
        filter(! type %in% c("Spawnroom", "Fillingroom", "Stair") ) %>%
        mutate(NameTypeArea = paste0(type," - ",area)) %>%
        distinct()

      # Generate selectizeInput for each relevant agent
      choicesRoom = c("Same room","Skip room",unique(rooms$NameTypeArea) )
    }

    selectizeInput(
      inputId = "selectInput_alternative_resources_global",
      label = "Select second choice for each agent:",
      choices = choicesRoom,
      selected = "Same room"
    )
  })

  observeEvent(input$set_resources, {
    show_modal_spinner()
    if(!is.null(canvasObjects$roomsINcanvas)){
      if(input$textInput_resources_global != "" &&
         !is.null(input$textInput_resources_global) &&
         !grepl("^[0-9]+$", input$textInput_resources_global) &&
         input$textInput_resources_global >= 0){
        shinyalert("You must specify a numeric value greater or equals than 0 (>= 0) for the global number of resources.")
        return()
      }

      all_res_rooms <- canvasObjects$roomsINcanvas
      canvasObjects$resources <- NULL
      for(i in unique(paste0(all_res_rooms$type, "-", all_res_rooms$area))){
        if(is.null(canvasObjects$resources[[i]])){
          rooms_names <- unique((all_res_rooms %>% filter(type == str_split(i, "-")[[1]][1], area == str_split(i, "-")[[1]][2]))$Name)
          canvasObjects$resources[[i]]$roomResource <- data.frame(room=rooms_names, MAX=rep(input$textInput_resources_global, length(rooms_names)))
          canvasObjects$resources[[i]]$waitingRoomsDeter <- data.frame(Agent=NULL, Room=NULL)
          canvasObjects$resources[[i]]$waitingRoomsRand <- data.frame(Agent=NULL, Room=NULL)
        }


        for(Agent in names(canvasObjects$agents)){
          canvasObjects$resources[[i]]$waitingRoomsDeter <- rbind(canvasObjects$resources[[i]]$waitingRoomsDeter, data.frame(Agent=Agent, Room=input$selectInput_alternative_resources_global))
          canvasObjects$resources[[i]]$waitingRoomsRand <- rbind(canvasObjects$resources[[i]]$waitingRoomsRand, data.frame(Agent=Agent, Room=input$selectInput_alternative_resources_global))

          canvasObjects$resources[[i]]$roomResource[, Agent] <- input$textInput_resources_global
        }
      }
    }
    remove_modal_spinner()
  })

  # Generate dynamic selectizeInput based on the selected room
  output$dynamicSelectizeInputs_waitingRoomsDeter <- renderUI({

    resources_type = req(input$selectInput_resources_type)

    ResRoomsDF <- req( allResRooms() ) %>% filter(Room == resources_type) %>% filter(Flow == "Deter")

    rooms = canvasObjects$roomsINcanvas %>%
      select(type, Name, area ) %>%
      filter(! type %in% c("Spawnroom", "Fillingroom", "Stair") ) %>%
      mutate(NameTypeArea = paste0(type," - ",area)) %>%
      distinct()
    relevantAgents <- unique(ResRoomsDF$Agent)

    # Generate selectizeInput for each relevant agent
    if(!is.null(rooms) && dim(rooms)[1]>1 ){
      ListSel = lapply(relevantAgents, function(agent) {
        # aggionrare i selectize dei waiting se esiste già una selezione!
        waitingRooms = canvasObjects$resources[[resources_type]]$waitingRoomsDeter

        if(!is.null(waitingRooms))
          waitingRooms = waitingRooms %>% filter(Agent == agent)

        choicesRoom = c("Same room","Skip room",unique( rooms$NameTypeArea ) )

        if(!is.null(waitingRooms) && dim(waitingRooms)[1] > 0  )
          roomSelected = waitingRooms$Room
        else
          roomSelected = choicesRoom[1]

        selectizeInput(
          inputId = paste0("selectInput_WaitingRoomDeterSelect_", agent),
          label = paste0("Select second choice room in Determined Flow for ", agent, ":"),
          choices = choicesRoom,
          selected = roomSelected
        )

      })
    }else
      ListSel = NULL

    return(ListSel)
  })
  output$dynamicSelectizeInputs_waitingRoomsRand <- renderUI({

    resources_type = req(input$selectInput_resources_type)

    ResRoomsDF <- req( allResRooms() ) %>% filter(Room == resources_type) %>% filter(Flow == "Rand")

    rooms = canvasObjects$roomsINcanvas %>%
      select(type, Name, area ) %>%
      filter(! type %in% c("Spawnroom", "Fillingroom", "Stair") ) %>%
      mutate(NameTypeArea = paste0(type," - ",area)) %>%
      distinct()
    relevantAgents <- unique(ResRoomsDF$Agent)

    # Generate selectizeInput for each relevant agent
    if(!is.null(rooms) && dim(rooms)[1]>1 ){
      ListSel = lapply(relevantAgents, function(agent) {
        # aggionrare i selectize dei waiting se esiste già una selezione!
        waitingRooms = canvasObjects$resources[[resources_type]]$waitingRoomsRand

        if(!is.null(waitingRooms))
          waitingRooms = waitingRooms %>% filter(Agent == agent)

        choicesRoom = c("Same room","Skip room",unique( rooms$NameTypeArea ) )

        if(!is.null(waitingRooms) && dim(waitingRooms)[1] > 0  )
          roomSelected = waitingRooms$Room
        else
          roomSelected = choicesRoom[1]

        selectizeInput(
          inputId = paste0("selectInput_WaitingRoomRandSelect_", agent),
          label = paste0("Select second choice room in Random Flow for ", agent, ":"),
          choices = choicesRoom,
          selected = roomSelected
        )

      })
    }else
      ListSel = NULL

    return(ListSel)
  })
  observe({
    selectW = grep(x = names(input),pattern = "selectInput_WaitingRoomDeterSelect_",value = T)

    isolate({
      resources_type = input$selectInput_resources_type
      waitingRooms = canvasObjects$resources[[resources_type]]$waitingRoomsDeter
    })

    if(length(selectW) > 0 ){
      waitingRooms = do.call(rbind,
                             lapply(selectW, function(W)
                               data.frame(Agent = gsub(pattern = "selectInput_WaitingRoomDeterSelect_",replacement = "",x = W),
                                          Room = input[[W]] )
                             )
      )
    }

    isolate({
      waitingRooms -> canvasObjects$resources[[resources_type]]$waitingRoomsDeter
    })

  })
  observe({
    selectW = grep(x = names(input),pattern = "selectInput_WaitingRoomRandSelect_",value = T)

    isolate({
      resources_type = input$selectInput_resources_type
      waitingRooms = canvasObjects$resources[[resources_type]]$waitingRoomsRand

    })

    if(length(selectW) > 0 ){
      waitingRooms = do.call(rbind,
                             lapply(selectW, function(W)
                               data.frame(Agent = gsub(pattern = "selectInput_WaitingRoomRandSelect_",replacement = "",x = W),
                                          Room = input[[W]] )
                             )
      )
    }

    isolate({
      waitingRooms -> canvasObjects$resources[[resources_type]]$waitingRoomsRand
    })

  })
  observe({
    if(!is.null(allResRooms()) ){
      choices <- unique( allResRooms()$Room )
      choices <- choices[!grepl(paste0("Spawnroom", collapse = "|"), choices)]
      #choices <- choices[!grepl(paste0("Fillingroom", collapse = "|"), choices)]
      choices <- choices[!grepl(paste0("Stair", collapse = "|"), choices)]

      updateSelectizeInput(session, "selectInput_resources_type", choices = choices, selected= "", server = TRUE)
    }
  })

  observe({
    #give a default to resources and waitingrooms
    resources_type = req(input$selectInput_resources_type)
    ResRoomsDF <- req( allResRooms() ) %>% filter(Room == resources_type)

    rooms = canvasObjects$roomsINcanvas %>%
      select(type, Name, area ) %>%
      mutate(TypeArea = paste0(type,"-",area)) %>%
      filter(TypeArea == resources_type) %>%
      distinct()

    isolate({
      if(dim(rooms)[1]==0){
        data = data.frame()
      }else if(is.null(canvasObjects$resources[[resources_type]]$roomResource)){
        data = data.frame(room = rooms$Name, MAX = 0 )
        for(a in unique(ResRoomsDF$Agent))
          data[,a] = 0
      }else{
        # If there exist already the dataset, then it is used and we have to check that there is already the agents
        dataOLD = canvasObjects$resources[[resources_type]]$roomResource

        data = dataOLD[,c("room","MAX")]
        for(a in unique(ResRoomsDF$Agent)){
          if(a %in% colnames(dataOLD))
            data[,a] = dataOLD[,a]
          else
            data[,a] = 0
        }
        # filter the rooms already present to keep only the new added in the canvas
        dataNEW = rooms %>% filter(!Name %in% dataOLD$room)

        if(dim(dataNEW)[1]> 0 ){
          dataNew = setNames(data.frame(matrix(0, ncol = length(colnames(dataOLD)), nrow = dim(dataNEW)[1])), colnames(dataOLD))
          dataNew$room = dataNEW$Name
          data = rbind(data,dataNew )
        }
      }

      canvasObjects$resources[[resources_type]]$roomResource <- data
    })

    isolate({

      ### E' da sistemare in maniera che si ricrodi cosa avevo inserito sia in rand che determi
      data_waiting = data.frame()

      data_waitingOLD = canvasObjects$resources[[resources_type]]$waitingRoomsDeter
      if(is.null(data_waitingOLD)){
        agents = unique(ResRoomsDF[ResRoomsDF$Flow == "Deter", "Agent"])
        if(length(agents) > 0 ){
          data_waiting = do.call(rbind,
                                 lapply(agents, function(W)
                                   data.frame(Agent = W,
                                              Room = "Same room")
                                 )
          )
        }

      }else{
        # If there exist already the dataset, then it is used and we have to check that there is already the agents

        data_waiting = data_waitingOLD[,c("Agent","Room")]
        for(a in unique(ResRoomsDF$Agent)){
          if(a %in% data_waitingOLD$Agent)
            data_waiting[data_waiting$Agent == a, "Room"] = data_waitingOLD[data_waiting$Agent == a, "Room"]
          else
            data_waiting <- rbind(data_waiting, data.frame(Agent = a, Room = "Same room"))
        }

        agent_eliminated = data_waitingOLD$Agent[!(data_waitingOLD$Agent %in% ResRoomsDF$Agent)]

        if(length(agent_eliminated) != 0){
          data_waiting <- data_waiting %>% filter(!Agent %in% agent_eliminated)
        }
      }

      canvasObjects$resources[[resources_type]]$waitingRoomsDeter <- data_waiting
    })
    isolate({

      ### E' da sistemare in maniera che si ricrodi cosa avevo inserito sia in rand che determi
      data_waiting = data.frame()

      data_waitingOLD = canvasObjects$resources[[resources_type]]$waitingRoomsRand
      if(is.null(data_waitingOLD)){
        agents = unique(ResRoomsDF[ResRoomsDF$Flow == "Rand", "Agent"])
        if(length(agents) > 0 ){
          data_waiting = do.call(rbind,
                                 lapply(agents, function(W)
                                   data.frame(Agent = W,
                                              Room = "Same room")
                                 )
          )
        }

      }else{
        # If there exist already the dataset, then it is used and we have to check that there is already the agents

        if(nrow(data_waitingOLD) >0){
          data_waiting = data_waitingOLD[,c("Agent","Room")]
        }

        for(a in unique(ResRoomsDF$Agent)){
          if(a %in% data_waitingOLD$Agent)
            data_waiting[data_waiting$Agent == a, "Room"] = data_waitingOLD[data_waiting$Agent == a, "Room"]
          else
            data_waiting <- rbind(data_waiting, data.frame(Agent = a, Room = "Same room"))
        }

        agent_eliminated = data_waitingOLD$Agent[!(data_waitingOLD$Agent %in% ResRoomsDF$Agent)]

        if(length(agent_eliminated) != 0){
          data_waiting <- data_waiting %>% filter(!Agent %in% agent_eliminated)
        }
      }

      canvasObjects$resources[[resources_type]]$waitingRoomsRand <- data_waiting
    })

  })

  observe({
    # Render the editable table
    output$RoomAgentResTable <- DT::renderDataTable(
      DT::datatable(canvasObjects$resources[[input$selectInput_resources_type]]$roomResource,
                    options = list(
                      dom = 't',  # Display only the table, not the default elements (e.g., search bar, length menu)
                      scrollX = TRUE
                    ),
                    editable = list(target = 'cell', disable = list(columns = c(0))),
                    selection = 'single',
                    rownames = F,
                    colnames = c("Room", "Maximum", colnames(canvasObjects$resources[[input$selectInput_resources_type]]$roomResource)[-c(1, 2)])
      )
    )
  })



  # Observe table edit and validate input
  observeEvent(input$RoomAgentResTable_cell_edit, {
    info <- input$RoomAgentResTable_cell_edit
    str(info)

    newValue <- as.numeric(info$value)
    canvasObjects$resources[[input$selectInput_resources_type]]$roomResource -> data
    oldValue <- data[info$row, info$col + 1]
    canvasObjects$resources[[input$selectInput_resources_type]]$roomResource[info$row, info$col + 1] <- newValue


    if (is.na(newValue) || newValue < 0) {
      showNotification("Please enter a positive numeric value.", type = "error")
      isolate({
        canvasObjects$resources[[input$selectInput_resources_type]]$roomResource[info$row, info$col + 1] <- oldValue
      })
    }

  })

  #### Flow
  # Funzione per verificare se un tipo di stanza è presente nel flusso di un agente
  check_room_type_in_agent_flow <- function(agent_name, room_type) {
    # Verifica se il flusso dell'agente contiene il tipo di stanza
    if (!is.null(canvasObjects$agents[[agent_name]]$DeterFlow)|| !is.null(canvasObjects$agents[[agent_name]]$RandFlow)) {
      deter_flow_rooms <- canvasObjects$agents[[agent_name]]$DeterFlow$Room
      rand_flow_rooms <- canvasObjects$agents[[agent_name]]$RandFlow$Room
      return(room_type %in% c(deter_flow_rooms, rand_flow_rooms))
    } else {
      return(FALSE)
    }
  }

  #######################
  #### Disease Model ####

  output$description <- renderText({
    disease_model <- input$disease_model

    file_path <- paste0(system.file("Shiny","Descriptions", package = "FORGE4FLAME"),
                        "/", disease_model, "_description.txt")

    # Leggi il testo dal file corrispondente
    if (file.exists(file_path)) {
      description_text <- readLines(file_path, warn = FALSE)
      return(description_text)
    } else {
      return("Description not available for this model.")
    }
  })

  # Save values for the selected disease model #
  observeEvent(input$save_values_disease_model,{
    disable("rds_generation")
    disable("flamegpu_connection")
    Name=input$disease_model
    beta_contact=NULL
    beta_aerosol=NULL
    gamma_time=NULL
    gamma_dist=NULL
    alpha_time=NULL
    alpha_dist=NULL
    lambda_time=NULL
    lambda_dist=NULL
    nu_time=NULL
    nu_dist=NULL

    if(is.na(gsub(",", "\\.", input$beta_contact)) || is.na(gsub(",", "\\.", as.numeric(input$beta_contact))) || is.na(gsub(",", "\\.", input$beta_aerosol)) || is.na(as.numeric(gsub(",", "\\.", input$beta_aerosol)))){
      shinyalert("You must specify a numeric value for beta (contact and aerosol).")
      return()
    }

    beta_contact=gsub(",", "\\.", input$beta_contact)
    beta_aerosol=gsub(",", "\\.", input$beta_aerosol)


    gamma <- check_distribution_parameters(input, "gamma")

    gamma_dist=gamma[[1]]
    gamma_time=gamma[[2]]

    if(is.null(gamma_dist) || is.null(gamma_time))
      return()

    if(grepl("E", Name)){
      alpha <- check_distribution_parameters(input, "alpha")

      alpha_dist=alpha[[1]]
      alpha_time=alpha[[2]]

      if(is.null(alpha_dist) || is.null(alpha_time))
        return()
    }

    if(grepl("D", Name)){
      lambda <- check_distribution_parameters(input, "lambda")

      lambda_dist=lambda[[1]]
      lambda_time=lambda[[2]]

      if(is.null(lambda_dist) || is.null(lambda_time))
        return()
    }

    if(grepl("^([^S]*S[^S]*S[^S]*)$", Name)){
      nu <- check_distribution_parameters(input, "nu")

      nu_dist=nu[[1]]
      nu_time=nu[[2]]

      if(is.null(nu_dist) || is.null(nu_time))
        return()
    }

    canvasObjects$disease = list(Name=Name,beta_contact=beta_contact,beta_aerosol=beta_aerosol,gamma_time=gamma_time,gamma_dist=gamma_dist,alpha_time=alpha_time, alpha_dist=alpha_dist,lambda_time=lambda_time,lambda_dist=lambda_dist,nu_time=nu_time,nu_dist=nu_dist)
  })

  output$disease_model_value <- renderText({
    if(!is.null(canvasObjects$disease)){
      text <- paste0("Disease model: ", canvasObjects$disease$Name, ". Beta (contact): ", canvasObjects$disease$beta_contact, ", Beta (aerosol): ", canvasObjects$disease$beta_aerosol, ", Gamma: ", canvasObjects$disease$gamma_time, " (", canvasObjects$disease$gamma_dist, ")")
      if(!is.null(canvasObjects$disease$alpha_time)){
        text <- paste0(text, ", Alpha: ", canvasObjects$disease$alpha_time, " (", canvasObjects$disease$alpha_dist, ")")}
      if(!is.null(canvasObjects$disease$lambda_time)){
        text <- paste0(text, ", Lambda: ", canvasObjects$disease$lambda_time, " (", canvasObjects$disease$lambda_dist, ")")
      }
      if(!is.null(canvasObjects$disease$nu_time)){
        text <- paste0(text, ", Nu: ", canvasObjects$disease$nu_time, " (", canvasObjects$disease$nu_dist, ")")
      }
      text
    }})

  ####  Save what-if #####
  add_data <- function(measure, parameters, type, from, to,data) {

    # Check if the exact row already exists
    duplicate_row <- subset(data, Measure == measure & Parameters == parameters & Type == type & From == from & To == to)
    if (nrow(duplicate_row) > 0) {
      shinyalert::shinyalert("This entry already exists!", type = "error")
      return(NULL)
    }

    # Check for overlapping time ranges
    if(!is.na(to)){
      overlap_row <- subset(data, Measure == measure & Type == type &
                              ((From <= to & To >= from) | (to >= From & from <= To)))
      if (nrow(overlap_row) > 0) {
        shinyalert::shinyalert("Time range overlaps with an existing entry!", type = "error")
        return(NULL)
      }
    }

    # If no duplicate or overlap, add new row
    new_row <- data.frame(
      Measure = measure,
      Type = type,
      Parameters = parameters,
      From = from,
      To = to,
      stringsAsFactors = FALSE
    )

    return(rbind(data, new_row))
  }


  observeEvent(input$save_ventilation, {
    rooms_whatif = canvasObjects$rooms_whatif

    if(as.integer(input$ventilation_time_to) < as.integer(input$ventilation_time_from) ||
       as.integer(input$ventilation_time_to) > as.numeric(canvasObjects$starting$simulation_days) ||
       as.integer(input$ventilation_time_from) <= 0){
      shinyalert(paste0("The timing should be greater than 0, less than the simulation days (",canvasObjects$starting$simulation_days,"), and 'to'>'from'. ") )
      return()
    }

    ventilation = switch(input$ventilation_params,
                         "0 (no ventilation)" = 0,
                         "0.3 (poorly ventilated)" = 0.3,
                         "1 (domestic)" = 1,
                         "3 (offices/schools)" = 3,
                         "5 (well ventilated)" = 5,
                         "10 (typical maximum)" = 10,
                         "20 (hospital setting)" = 20)

    new_data = add_data(measure = "Ventilation",
                        parameters = paste(ventilation),
                        type = ifelse(input$ventilation_type != "Global", input$room_ventilation, "Global"),
                        from = input$ventilation_time_from,
                        to = input$ventilation_time_to,
                        data = rooms_whatif )

    if( !is.null(new_data) ){
      canvasObjects$rooms_whatif = new_data
    }

  })

  observeEvent(input$save_masks, {
    req(input$mask_fraction)
    req(input$mask_params)

    agents_whatif = canvasObjects$agents_whatif

    if(input$mask_fraction > 1 ||input$mask_fraction < 0){
      shinyalert("Mask fraction must be  in [0,1] ")
      return()
    }
    if(as.integer(input$mask_time_to) < as.integer(input$mask_time_from) ||
       as.integer(input$mask_time_to) > as.numeric(canvasObjects$starting$simulation_days) ||
       as.integer(input$mask_time_from) <= 0){
      shinyalert(paste0("The timing should be greater than 0, less than the simulation days (",canvasObjects$starting$simulation_days,"), and 'to'>'from'. ") )
      return()
    }

    params = paste0("Type: ",input$mask_params,"; Fraction: ",input$mask_fraction)

    new_data = add_data(measure = "Mask",
                        parameters = params,
                        type = ifelse(input$mask_type != "Global", input$agent_mask, "Global"),
                        from = input$mask_time_from,
                        to = input$mask_time_to,
                        data = agents_whatif )

    if( !is.null(new_data) ){
      canvasObjects$agents_whatif = new_data
    }
  })
  observeEvent(input$save_vaccination, {
    agents_whatif = canvasObjects$agents_whatif
    req(input$vaccination_fraction)
    req(input$vaccination_efficacy)

    if((input$vaccination_efficacy) > 1 ||
       (input$vaccination_efficacy) < 0){
      shinyalert(paste0("The efficacy should be in [0,1]") )
      return()
    }
    if((input$vaccination_fraction) > 1 ||
       (input$vaccination_fraction) < 0){
      shinyalert(paste0("The fraction should be in [0,1]") )
      return()
    }

    vaccination_coverage <- check_distribution_parameters(input, "vaccination_coverage")
    new_dist <- vaccination_coverage[[1]]
    new_time <- vaccination_coverage[[2]]

    if(is.null(new_time) && is.null(new_dist))
      return()

    if(new_dist == "Deterministic"){
      if(as.numeric(new_time) < 1){
        shinyalert("The number of vaccine coverage days must be greater or equal (>=) 1.")
        return()
      }
      paramstext = paste0("Dist.Days: ", new_dist,", ",new_time,", 0")

    }else{
      params <- parse_distribution(new_time, new_dist)
      a <- params[[1]]
      b <- params[[2]]

      if(a < 1){
        shinyalert("The number of vaccine coverage days must be greater or equal (>=) 1.")
        return()
      }

      paramstext = paste0( "Dist.Days: ", new_dist,", ",a,", ",b)
    }

    params = paste0("Efficacy: ",input$vaccination_efficacy,"; Fraction: ",input$vaccination_fraction,"; Coverage ",paramstext)

    new_data = add_data(measure = "Vaccination",
                        parameters = params,
                        type = ifelse(input$vaccination_type != "Global", input$agent_vaccination, "Global"),
                        from = input$vaccination_time_from,
                        to = input$vaccination_time_from,
                        data = agents_whatif )

    if( !is.null(new_data) ){
      canvasObjects$agents_whatif = new_data
    }
  })
  observeEvent(input$save_swab, {
    agents_whatif = canvasObjects$agents_whatif

    if(as.integer(input$swab_time_to) < as.integer(input$swab_time_from) ||
       as.integer(input$swab_time_to) > as.numeric(canvasObjects$starting$simulation_days) ||
       as.integer(input$swab_time_from) <= 0){
      shinyalert(paste0("The timing should be greater than 0, less than the simulation days (",canvasObjects$starting$simulation_days,"), and 'to'>'from'. ") )
      return()
    }

    paramstext = paste0("Sensitivity: ",input$swab_sensitivity,"; Specificity: ",input$swab_specificity)

    new_dist <- "No swab"
    new_time <- 0
    if(input$swab_type_specific != "No swab"){
      swab_global <- check_distribution_parameters(input, "swab_days")
      new_dist <- swab_global[[1]]
      new_time <- swab_global[[2]]
    }

    if(is.null(new_time) && is.null(new_dist))
      return()

    if(new_dist == "Deterministic" || new_dist == "No swab"){
      paramstext = paste0(paramstext, "; Dist: ", new_dist,", ",new_time,", 0")
    }else{
      params <- parse_distribution(new_time, new_dist)
      a <- params[[1]]
      b <- params[[2]]

      paramstext = paste0(paramstext, "; Dist: ", new_dist,", ",a,", ",b)
    }


    new_data = add_data(measure = "Swab",
                        parameters = paramstext,
                        type = ifelse(input$swab_type != "Global", input$agent_swab, "Global"),
                        from = input$swab_time_from,
                        to = input$swab_time_to,
                        data = agents_whatif )

    if( !is.null(new_data) ){
      canvasObjects$agents_whatif = new_data
    }
  })
  observeEvent(input$save_quarantine, {
    agents_whatif = canvasObjects$agents_whatif

    req(input$quarantine_type != "No quarantine")

    if(!(input$quarantine_type == "Different for each agent" && input$quarantine_type_agent == "No quarantine") ){

      if(as.integer(input$quarantine_time_to) < as.integer(input$quarantine_time_from) ||
         as.integer(input$quarantine_time_to) > as.numeric(canvasObjects$starting$simulation_days) ||
         as.integer(input$quarantine_time_from) <= 0){
        shinyalert(paste0("The timing should be greater than 0, less than the simulation days (",canvasObjects$starting$simulation_days,"), and 'to'>'from'. ") )
        return()
      }

      quarantine_global <- check_distribution_parameters(input, "quarantine_global")
      new_dist <- quarantine_global[[1]]
      new_time <- quarantine_global[[2]]

      if(is.null(new_time) && is.null(new_dist))
        return()

      if(new_dist == "Deterministic"){
        if(as.numeric(new_time) < 1){
          shinyalert("The number of quarantine days must be greater or equal (>=) 1.")
          return()
        }

        paramstext = paste0("Dist.Days: ", new_dist,", ",new_time,", 0")

      }else{
        params <- parse_distribution(new_time, new_dist)
        a <- params[[1]]
        b <- params[[2]]

        if(a < 1){
          shinyalert("The number of quarantine days must be greater or equal (>=) 1.")
          return()
        }

        paramstext = paste0( "Dist.Days: ", new_dist,", ",a,", ",b)
      }

      paramstext = paste0(paramstext, "; Q.Room: ", input$room_quarantine)
      paramstext =  paste0(paramstext,"; Sensitivity: ",input$quarantine_swab_sensitivity,"; Specificity: ",input$quarantine_swab_specificity)

      new_dist <- "No swab"
      new_time <- 0

      if(input$quarantine_swab_type_global != "No swab"){
        #paramstext =  paste0(paramstext,"; Sensitivity: ",input$quarantine_swab_sensitivity,"; Specificity: ",input$quarantine_swab_specificity)

        quarantine_swab_global <- check_distribution_parameters(input, "quarantine_swab_global")
        new_dist <- quarantine_swab_global[[1]]
        new_time <- quarantine_swab_global[[2]]

        if(is.null(new_time) && is.null(new_dist))
          return()
      }

      if(new_dist == "Deterministic" || new_dist == "No swab"){
        paramstext = paste0(paramstext, "; Dist: ", new_dist,", ",new_time,", 0")
      }else{
        params <- parse_distribution(new_time, new_dist)
        a <- params[[1]]
        b <- params[[2]]

        paramstext = paste0(paramstext, "; Dist: ", new_dist,", ",a,", ",b)
      }

    }else{
      paramstext = "No quarantine, 0, 0"
    }

    new_data = add_data(measure = "Quarantine",
                        parameters = paramstext,
                        type = ifelse(input$quarantine_type != "Global", input$agent_quarantine, "Global"),
                        from = input$quarantine_time_from,
                        to = input$quarantine_time_to,
                        data = agents_whatif )

    if( !is.null(new_data) ){
      canvasObjects$agents_whatif = new_data
    }

    updateSelectizeInput(inputId = "room_quarantine_global", selected = "")

  })
  observeEvent(input$save_external_screening, {
    agents_whatif = canvasObjects$agents_whatif

    if((input$external_screening_second_global) > 1 || (input$external_screening_second_global) < 0){
      shinyalert("External screening must be  in [0,1] ")
      return()
    }
    if((input$external_screening_first_global) > 1 || (input$external_screening_first_global) < 0){
      shinyalert("External screening must be  in [0,1] ")
      return()
    }

    if(as.integer(input$external_screening_time_to) < as.integer(input$external_screening_time_from) ||
       as.integer(input$external_screening_time_to) > as.numeric(canvasObjects$starting$simulation_days) ||
       as.integer(input$external_screening_time_from) <= 0){
      shinyalert(paste0("The timing should be greater than 0, less than the simulation days (",canvasObjects$starting$simulation_days,"), and 'to'>'from'. ") )
      return()
    }

    params = paste0("First: ",input$external_screening_first_global,"; Second: ",input$external_screening_second_global)

    new_data = add_data(measure = "External screening",
                        parameters = params,
                        type = ifelse(input$external_screening_type != "Global", input$agent_external_screening, "Global"),
                        from = input$external_screening_time_from,
                        to = input$external_screening_time_to,
                        data = agents_whatif )

    if( !is.null(new_data) ){
      canvasObjects$agents_whatif = new_data
    }
  })
  observeEvent(input$save_virus,{

    req(input$virus_variant)
    req(input$virus_severity)

    if((input$virus_severity) > 1 || (input$virus_severity) < 0){
      shinyalert("Virus severity must be  in [0,1] ")
      return()
    }
    if((input$virus_variant) < 0){
      shinyalert("Virus variant must be > 0 ")
      return()
    }

    canvasObjects$virus_variant <-  input$virus_variant
    canvasObjects$virus_severity <-  input$virus_severity
  })
  observeEvent(input$save_initial_infected,{
    canvasObjects$initial_infected -> initial_infected
    req(input$virus_variant)
    req(input$virus_severity)

    if(is.na(as.integer(input$initial_infected_global)) || as.integer(input$initial_infected_global) < 0){
      shinyalert("Initial infected must be a number greater or equal (>=) 0.")
      return()
    }

    if(input$initial_infected_type == "Global"){
      if("Global" %in% initial_infected$Type){
        shinyalert(paste0("A 'Global' Initial infected is already defined. Please delete it by click on its row in the table. ") )
        return()
      }
      total_agents <- 0
      for(a in 1:length(canvasObjects$agents)){
        if(canvasObjects$agents[[a]]$entry_type == "Time window"){
          if(as.integer(input$initial_infected_global) > as.numeric(canvasObjects$agents[[a]]$NumAgent)){
            shinyalert(paste0("Initial infected must be a number smaller or equal (<=) the number of agents (for the agent ", names(canvasObjects$agents)[a], " there are ", canvasObjects$agents[[a]]$NumAgent, " agents)."))
            return()
          }
        }
      }
    }else if(input$initial_infected_type == "Random"){
      if("Random" %in% initial_infected$Type){
        shinyalert(paste0("A 'Random' Initial infected is already defined. Please delete it by click on its row in the table. ") )
        return()
      }
      total_agents <- 0
      for(a in 1:length(canvasObjects$agents)){
        if(canvasObjects$agents[[a]]$entry_type == "Time window"){
          total_agents <- total_agents + as.numeric(canvasObjects$agents[[a]]$NumAgent)
        }
      }

      if(as.integer(input$initial_infected_global) > total_agents){
        shinyalert(paste0("Initial infected must be a number smaller or equal (<=) the number of agents (", total_agents, ")."))
        return()
      }
    }else{
      a = input$agent_initial_infected
      if(canvasObjects$agents[[a]]$entry_type == "Time window"){
        if(as.integer(input$initial_infected_global) > as.numeric(canvasObjects$agents[[a]]$NumAgent)){
          shinyalert(paste0("Initial infected must be a number smaller or equal (<=) the number of agents (for the agent ", names(canvasObjects$agents)[a], " there are ", canvasObjects$agents[[a]]$NumAgent, " agents)."))
          return()
        }
      }
    }

    new_row <- data.frame(
      Type = ifelse(input$initial_infected_type != "Different for each agent", input$initial_infected_type, input$agent_initial_infected),
      Number = input$initial_infected_global,
      stringsAsFactors = FALSE
    )

    canvasObjects$initial_infected = rbind(initial_infected, new_row)
  })

  observe({
    disable("rds_generation")
    disable("flamegpu_connection")
    req(!is.null(canvasObjects$agents) && length(canvasObjects$agents)>0 )

    INITagents<- c()

    for(a in 1:length(canvasObjects$agents)){
      if(canvasObjects$agents[[a]]$entry_type == "Time window")
        INITagents <- c(INITagents, names(canvasObjects$agents)[a])
    }

    updateSelectizeInput(session, inputId = "agent_initial_infected", choices = c("", INITagents))

    updateSelectizeInput(session = session, "agent_mask",
                         choices = c("", names(canvasObjects$agents)))

    updateSelectizeInput(session = session, "agent_vaccination",
                         choices = c("", names(canvasObjects$agents)))

    updateSelectizeInput(session = session, "agent_swab",
                         choices = c("", names(canvasObjects$agents)))

    updateSelectizeInput(session = session, "agent_quarantine",
                         choices = c("", names(canvasObjects$agents)))

    updateSelectizeInput(session = session, "agent_external_screening",
                         choices = c("", names(canvasObjects$agents)))


    if(length(canvasObjects$roomsINcanvas) > 0){
      rooms = canvasObjects$roomsINcanvas %>% filter(type != "Fillingroom", type != "Stair")
      roomsAvailable = c("", unique(paste0( rooms$type,"-", rooms$area) ) )

      updateSelectizeInput(session = session, "room_quarantine",
                           choices = roomsAvailable)
    }

  })

  ########### Render the saved data table   ##########
  output$agents_whatif <- renderDT({
    if(!is.null(canvasObjects$agents_whatif)){
      datatable(canvasObjects$agents_whatif %>% mutate(Measure = as.factor(Measure),
                                                       Type = as.factor(Type),
                                                       Parameters= as.factor(Parameters) ),
                filter = 'top', selection = "single", rownames = FALSE, editable = TRUE,
                options = list(searching = TRUE, info = FALSE,paging = FALSE,
                               sort = TRUE, scrollX = TRUE, scrollY = TRUE) )
    }
  })
  output$rooms_whatif <- renderDT({
    if(!is.null(canvasObjects$rooms_whatif)){
      datatable(canvasObjects$rooms_whatif %>% mutate(Measure = as.factor(Measure),
                                                      Type = as.factor(Type),
                                                      Parameters= as.factor(Parameters) ),
                filter = 'top', selection = "single", rownames = FALSE, editable = TRUE,
                options = list(searching = TRUE, info = FALSE,paging = FALSE,
                               sort = TRUE, scrollX = TRUE, scrollY = TRUE) )
    }
  })

  output$virus_info <- renderDT({
    datatable( data.frame(Variant = canvasObjects$virus_variant,
                          Severity = canvasObjects$virus_severity),
               options = list(searching = FALSE, info = FALSE,paging = FALSE,
                              sort = TRUE, scrollX = TRUE, scrollY = TRUE))
  })
  output$initialI_info <- renderDT({
    datatable(canvasObjects$initial_infected,
              options = list(searching = FALSE, info = FALSE,paging = FALSE,
                             sort = TRUE, scrollX = TRUE, scrollY = TRUE) )
  })

  # Double Click to Delete Row with Confirmation

  observeEvent(input$agents_whatif_cell_clicked, {
    info <- input$agents_whatif_cell_clicked
    if (!is.null(info$row)) {
      shinyalert(
        title = "Delete Entry?",
        text = "Are you sure you want to delete this row?",
        type = "warning",
        showCancelButton = TRUE,
        confirmButtonText = "Yes, delete it!",
        callbackR = function(x) {
          if (x) {
            data <- canvasObjects$agents_whatif
            canvasObjects$agents_whatif <- data[-info$row, ]
          }
        }
      )
    }
  })

  observeEvent(input$rooms_whatif_cell_clicked, {
    info <- input$rooms_whatif_cell_clicked
    if (!is.null(info$row)) {
      shinyalert(
        title = "Delete Entry?",
        text = "Are you sure you want to delete this row?",
        type = "warning",
        showCancelButton = TRUE,
        confirmButtonText = "Yes, delete it!",
        callbackR = function(x) {
          if (x) {
            data <- canvasObjects$rooms_whatif
            canvasObjects$rooms_whatif <- data[-info$row, ]
          }
        }
      )
    }
  })

  observeEvent(input$initialI_info_cell_clicked, {
    info <- input$initialI_info_cell_clicked
    if (!is.null(info$row)) {
      shinyalert(
        title = "Delete Entry?",
        text = "Are you sure you want to delete this row?",
        type = "warning",
        showCancelButton = TRUE,
        confirmButtonText = "Yes, delete it!",
        callbackR = function(x) {
          if (x) {
            data <- canvasObjects$initial_infected
            canvasObjects$initial_infected <- data[-info$row, ]
          }
        }
      )
    }
  })

  ##########

  ### Load csv: ####
  observeEvent(input$LoadCSV_Button_OutsideContagion,{
    disable("rds_generation")
    disable("flamegpu_connection")

    isolate({
      if(is.null(input$OutsideContagionImport) || !file.exists(input$OutsideContagionImport$datapath) || !grepl(".csv", input$OutsideContagionImport$datapath)){
        shinyalert("Error","Please select one csv file.", "error", 5000)
        return()
      }

      dataframe <- read_csv(input$OutsideContagionImport$datapath)
      if(!"day" %in% names(dataframe) || !"percentage_infected" %in% names(dataframe)){
        shinyalert("Error", "The csv mush have two columns: day and percentage_infected", "error", 5000)
        return()
      }

      if(any(is.na(as.numeric(dataframe$day))) || any(is.na(as.numeric(dataframe$percentage_infected)))){
        shinyalert("Error", "The two columns (day and percentage_infected) must contain only numbers", "error", 5000)
        return()
      }

      if(input$population == "" || is.na(as.numeric(input$population))){
        shinyalert("Error", "Population must be a number", "error", 5000)
        return()
      }

      dataframe$day <- as.numeric(dataframe$day)
      dataframe$percentage_infected <- as.numeric(dataframe$percentage_infected)

      dataframe$percentage_infected <- dataframe$percentage_infected / as.numeric(input$population)

      if(any(dataframe$percentage_infected < 0) || any(dataframe$percentage_infected > 1)){
        shinyalert("Error", "The percentage_infected column must contain numbers in [0, 1]", "error", 5000)
        return()
      }

      if(any(dataframe$day <= 0) || dataframe$day[nrow(dataframe)] < as.numeric(canvasObjects$starting$simulation_days)){
        shinyalert("Error", "The number of days to simulate is bigger then the number of days in the file", "error", 5000)
        return()
      }

      canvasObjects$outside_contagion <- dataframe %>%
        select(day, percentage_infected)

      output$outside_contagion_plot <- renderPlot({
        ggplot(dataframe) +
          geom_line(aes(x=day, y=percentage_infected), color="green", linewidth=1.5) +
          ylim(0, NA) +
          labs(title = "Outside contagion", x = "Day", y = "Percentage") +
          theme(title = element_text(size = 34), axis.title = element_text(size = 26), axis.text = element_text(size = 22)) +
          theme_fancy()
      })

      showElement("outside_contagion_plot")

      shinyalert("Success", "File loaded", "success", 1000)
    })
  })

  observeEvent(input$initial_day,{
    canvasObjects$starting$day <- input$initial_day
  })

  initial_time <- debounce(reactive({input$initial_time}), 1000L)

  observeEvent(initial_time(),{
    disable("rds_generation")
    disable("flamegpu_connection")
    new_time <- input$initial_time

    if (!(grepl("^([01]?[0-9]|2[0-3]):[0-5][0-9]$", new_time) || grepl("^\\d{1,2}$", new_time))){
      shinyalert("The format of the time should be: hh:mm (e.g. 06:15, or 20).")
      return()
    }

    canvasObjects$starting$time <- new_time
  })

  simulation_days <- debounce(reactive({input$simulation_days}), 1000L)

  observeEvent(simulation_days(),{
    disable("rds_generation")
    disable("flamegpu_connection")
    simulation_days <- input$simulation_days

    if(simulation_days == "" || !grepl("(^[0-9]+).*", simulation_days) || simulation_days < 0){
      shinyalert("You must specify a number greater than 0 (>= 0).")
      return()
    }

    old_simulation_days <- canvasObjects$starting$simulation_days
    canvasObjects$starting$simulation_days <- simulation_days

    if(nrow(canvasObjects$agents_whatif) > 0){
      for(i in 1:nrow(canvasObjects$agents_whatif)){
        if(canvasObjects$agents_whatif[i, "To"] == old_simulation_days)
          canvasObjects$agents_whatif[i, "To"] <- simulation_days
      }
    }

    if(nrow(canvasObjects$rooms_whatif) > 0){
      for(i in 1:nrow(canvasObjects$rooms_whatif)){
        if(canvasObjects$rooms_whatif[i, "To"] == old_simulation_days)
          canvasObjects$rooms_whatif[i, "To"] <- simulation_days
      }
    }

    updateNumericInput(session = session, inputId = "ventilation_time_to", value = simulation_days)
    updateNumericInput(session = session, inputId = "mask_time_to", value = simulation_days)
    updateNumericInput(session = session, inputId = "swab_time_to", value = simulation_days)
    updateNumericInput(session = session, inputId = "quarantine_time_to", value = simulation_days)
    updateNumericInput(session = session, inputId = "external_screening_time_to", value = simulation_days)
  })

  seed <- debounce(reactive({input$seed}), 1000L)

  observeEvent(seed(),{
    disable("rds_generation")
    disable("flamegpu_connection")
    seed <- input$seed

    if(seed == "" || !grepl("(^[0-9]+).*", seed) || seed < 0){
      shinyalert("You must specify a number greater then or equal to 0 (>= 0).")
      return()
    }

    canvasObjects$starting$seed <- seed
  })

  observeEvent(input$step,{
    disable("rds_generation")
    disable("flamegpu_connection")
    canvasObjects$starting$step <- input$step
  })

  nrun <- debounce(reactive({input$nrun}), 1000L)

  observeEvent(nrun(),{
    disable("rds_generation")
    disable("flamegpu_connection")
    nrun <- input$nrun

    if(nrun == "" || !grepl("(^[0-9]+).*", nrun) || nrun <= 0){
      shinyalert("You must specify a number greater than 0 (> 0).")
      return()
    }

    canvasObjects$starting$nrun <- nrun
  })

  prun <- debounce(reactive({input$prun}), 1000L)

  observeEvent(nrun(),{
    disable("rds_generation")
    disable("flamegpu_connection")
    prun <- input$prun

    if(prun == "" || !grepl("(^[0-9]+).*", prun) || prun <= 0){
      shinyalert("You must specify a number greater than 0 (> 0).")
      return()
    }

    if(prun > canvasObjects$starting$nrun){
      prun <- nrun
    }

    canvasObjects$starting$prun <- prun
  })


  #### START post processing #####

  postprocObjects = reactiveValues(evolutionCSV = NULL,
                                   Filter_evolutionCSV = NULL,
                                   CONTACTcsv = NULL,
                                   CONTACTmatrix = NULL,
                                   AEROSOLcsv = NULL,
                                   COUNTERScsv = NULL,
                                   A_C_COUNTERS = NULL,
                                   Mapping = NULL,
                                   FLAGmodelLoaded = FALSE,
                                   MappingID_room = FALSE,
  )

  required_files <- c("AEROSOL.csv","AGENT_POSITION_AND_STATUS.csv", "CONTACT.csv","counters.csv",
                      "evolution.csv" )
  # Allow user to select a folder
  wdFolders = str_split(string = dirname(getwd()),pattern = "/")
  wdFolders= paste0(wdFolders[[1]][1:3],collapse = "/")
  shinyDirChoose(input, "dir", roots = c(wd = wdFolders), filetypes = c('', 'csv','txt'))

  # Get the selected folder path
  dirPath <- reactive({
    req(input$dir)
    parseDirPath(roots = c(wd = wdFolders), input$dir)
  })

  # Display the selected folder path
  output$dirPath <- renderText({
    dirPath()
  })

  # Check for required files in subfolders
  valid_subfolders <- reactive({
    dir = req(dirPath())
    subfolders <- list.dirs(dir, recursive = FALSE)
    valid <- sapply(subfolders, function(subfolder) {
      all(file.exists(file.path(subfolder, required_files)))
    })
    if(length(subfolders) != 0){
      subfolders[valid]
    }
  })

  # Create a dropdown to select a subfolder
  # output$subfolderUI <- renderUI({
  #   if (req(length(valid_subfolders()) != 0)) {
  #     selectInput("selectedSubfolder", "Select Subfolder", choices = c("",basename(valid_subfolders())),selected = "" )
  #   }
  #   else{
  #     selectInput("selectedSubfolder", "Select Subfolder", choices = c("" ,selected = "" ))
  #   }
  # })

  observe({
    dir = req(dirPath())
    show_modal_spinner()

    # Evolution
    subfolders <- list.dirs(dir, recursive = FALSE)
    rooms_file = paste0(dir,"/rooms_mapping.txt")
    if(!file.exists(rooms_file)){
      shinyalert("Error", "The file rooms_mapping doesn't exists in the directory", "error")
      remove_modal_spinner()
      return()
    }

    isolate({
      G <- read_table(rooms_file,col_names = FALSE)
      colnames(G) = c("ID","x","y","z")

      roomsINcanvas = req(canvasObjects$roomsINcanvas)
      floors = req(canvasObjects$floors) %>%
        mutate(y = (Order - 1) * 10, CanvasID = Name)

      roomsINcanvas <- roomsINcanvas %>%
        filter(type == "Fillingroom") %>%
        mutate(z = y) %>%
        select(x, z, CanvasID, w, h) %>%
        left_join(floors, by = "CanvasID") %>%
        select(x, y, z, w, h) %>%
        mutate(x=x+ceiling(w/2), z=z+ceiling(h/2), ID = NA) %>%
        select(ID, x, y, z)

      G <- rbind(G, roomsINcanvas)

      postprocObjects$Mapping = G
      ####

      csv_files <- file.path(subfolders, "evolution.csv")
      data_list <- lapply(csv_files, function(file) {
        if (file.exists(file)) {
          f = read_csv(file)
          f$Folder= basename(dirname(file))
          f
        } else {
          NULL
        }
      })
      data_list <- Filter(Negate(is.null), data_list)  # Remove NULL entries
      if (length(data_list) == 0) return(NULL)
      postprocObjects$evolutionCSV = do.call(rbind, data_list)

      csv_files <- file.path(subfolders, "counters.csv")
      data_list <- lapply(csv_files, function(file) {
        if (file.exists(file)) {
          f = read_csv(file)
          colnames(f) = c("Day",
                          "Agents birth", "Agents deaths", "Agents in quarantine",
                          "Number of swabs", "Number of agents infected \noutside the environment")

          f$Folder= basename(dirname(file))
          f
        } else {
          NULL
        }
      })
      COUNTERSdata_list <- Filter(Negate(is.null), data_list)  # Remove NULL entries
      if (length(COUNTERSdata_list) == 0) return(NULL)
      postprocObjects$COUNTERScsv = do.call(rbind, COUNTERSdata_list) %>% distinct()

      csv_files <- file.path(subfolders, "AEROSOL.csv")
      data_list <- lapply(csv_files, function(file) {
        if (file.exists(file)) {
          f = read_csv(file,
                       col_names = c("time", "virus_concentration", "room_id"))
          f$Folder= basename(dirname(file))
          f
        } else {
          NULL
        }
      })
      AEROSOLdata_list <- Filter(Negate(is.null), data_list)  # Remove NULL entries
      if (length(AEROSOLdata_list) == 0) return(NULL)
      postprocObjects$AEROSOLcsv = do.call(rbind, AEROSOLdata_list) %>% distinct()

      csv_files <- file.path(subfolders, "CONTACT.csv")
      data_list <- lapply(csv_files, function(file) {
        if (file.exists(file)) {
          f = read_csv(file,col_names = c("time", "agent_id1", "agent_id2", "room_id"))
          f$Folder= basename(dirname(file))
          f
        } else {
          NULL
        }
      })
      CONTACTdata_list <- Filter(Negate(is.null), data_list)  # Remove NULL entries
      if (length(CONTACTdata_list) == 0) return(NULL)
      postprocObjects$CONTACTcsv = do.call(rbind, CONTACTdata_list) %>% distinct()

      csv_files <- file.path(subfolders, "CONTACTS_MATRIX.csv")
      data_list <- lapply(csv_files, function(file) {
        if (file.exists(file)) {
          f = read_csv(file,
                       col_names = c("time", "type1", "type2", "contacts"))
          f$Folder= basename(dirname(file))
          f
        } else {
          NULL
        }
      })
      CONTACTdata_list <- Filter(Negate(is.null), data_list)  # Remove NULL entries
      if (length(CONTACTdata_list) == 0) return(NULL)
      postprocObjects$CONTACTmatrix = do.call(rbind, CONTACTdata_list) %>% distinct()
    })
    remove_modal_spinner()
    shinyalert("Everything is loaded!")
  })

  #### query ####
  observe({

    CONTACTcsv = req(postprocObjects$CONTACTcsv)
    CONTACTmatrix = req(postprocObjects$CONTACTmatrix)
    AEROSOLcsv = req(postprocObjects$AEROSOLcsv)
    req(postprocObjects$FLAGmodelLoaded )

    isolate({
      dir = req(dirPath())
      roomsINcanvas = req(canvasObjects$roomsINcanvas)
      #### read all the areosol and contact ####
      subfolders <- list.dirs(dir, recursive = FALSE)
      MinTime = min( postprocObjects$evolutionCSV$Day)
      MaxTime = max( postprocObjects$evolutionCSV$Day)
      step = as.numeric(canvasObjects$starting$step)

      AEROSOLcsv$time <- as.numeric(AEROSOLcsv$time)

      if(!(step %in% names(table(diff(AEROSOLcsv$time)))) ) {
        shinyalert("The time step of the simulation does not correspond to the step defined in settings.",type = "error")
        return()
      }

      roomsINcanvas = roomsINcanvas %>% mutate( coord = ifelse(type == "Fillingroom", paste0(x+ceiling(w/2),"-", y+ceiling(h/2),"-", CanvasID), paste0(center_x,"-", center_y,"-", CanvasID)))
      rooms_id = roomsINcanvas$Name
      names(rooms_id) = roomsINcanvas$coord

      Mapping = postprocObjects$Mapping %>% mutate( CanvasID = unique(roomsINcanvas$CanvasID)[( y / 10 )+1] ,
                                                    coord = paste0(x,"-", z ,"-",CanvasID),
                                                    Name = rooms_id[coord] )

      Mapping = merge(Mapping,roomsINcanvas %>% select(coord, type, area, Name)) %>% select(-coord,-x,-y,-z)

      postprocObjects$MappingID_room = Mapping
      postprocObjects$AEROSOLcsv =  merge(Mapping , AEROSOLcsv, by.x = "ID", by.y = "room_id" )

      CONTACTcsv =  merge(Mapping , CONTACTcsv, by.x = "ID", by.y = "room_id" )
      agents = names(canvasObjects$agents)
      CONTACTcsv$agent_id1 = agents[CONTACTcsv$agent_id1+1]
      CONTACTcsv$agent_id2 = agents[CONTACTcsv$agent_id2+1]

      postprocObjects$CONTACTcsv = CONTACTcsv  %>%
        arrange(CanvasID,Folder, area, type, agent_id1, agent_id2, time) %>%
        group_by(CanvasID,Folder, area, type, agent_id1, agent_id2) %>%
        mutate(time_diff = time - lag(time, default = first(time))) %>%
        filter( time_diff != 1) %>%
        ungroup() %>%
        select(-time_diff)

      CONTACTmatrix$type1 = agents[CONTACTmatrix$type1+1]
      CONTACTmatrix$type2 = agents[CONTACTmatrix$type2+1]


      postprocObjects$CONTACTmatrix = CONTACTmatrix %>%
        group_by(type2,type1, Folder) %>%
        summarise(Mean = mean(contacts),
                  Sd = sd(contacts) )

      # Count the number of unique meetings per hour
      C_COUNTERS <-  postprocObjects$CONTACTcsv %>%
        mutate(hour = ceiling((time*step)/(60*60)) ) %>%  # Convert time to hourly bins
        group_by(CanvasID,Name,area,type,Folder,hour) %>%
        summarise(contact_counts = n())

      A_COUNTERS =postprocObjects$AEROSOLcsv   %>%
        mutate( hour = ceiling((time*step)/(60*60)) ) %>%
        group_by(CanvasID,Name,area,type,Folder,hour) %>%
        summarize(virus_concentration = mean(virus_concentration) )

      A_C_COUNTERS = merge(C_COUNTERS , A_COUNTERS, all = T)

      A_C_COUNTERS[is.na(A_C_COUNTERS)] = 0
      postprocObjects$A_C_COUNTERS = A_C_COUNTERS

      rooms = unique( paste(A_C_COUNTERS$CanvasID, " ; ", A_C_COUNTERS$area, " ; ", A_C_COUNTERS$Name) )
      updateSelectInput(session = session, inputId = "Room_Counters_A_C_selectize",
                        choices = c("",rooms),selected = "")

      #####
      postprocObjects$FLAGmodelLoaded = FALSE
    })

  })

  observe({
    pl = NULL
    info <- input$PostProc_table_cell_clicked
    folderselected = req(info$value)

    isolate({
      CONTACTmatrix = req(postprocObjects$CONTACTmatrix)
      c = CONTACTmatrix %>% filter(Folder == folderselected)
      agents = names(canvasObjects$agents)

      # Ensure type1 and type2 factors include all agents
      c$type1 <- factor(c$type1, levels = agents)
      c$type2 <- factor(c$type2, levels = agents)


      pl = ggplot(c, aes(x = type1, y = type2, fill = Mean)) +
        geom_tile() +
        scale_fill_gradient(low = "blue", high = "red") +
        theme_bw() +
        labs(title = "",
             x = "",
             y = "",
             fill = "Mean number of contact\n per hour") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              axis.text = element_text(size = 16),
              axis.title = element_text(size = 20, face = "bold"),
              plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
              legend.text = element_text(size = 18),
              legend.key.size = unit(1.5, 'cm'),
              legend.title = element_text(face = "bold", size = 18),
              legend.position = "bottom",
              strip.text = element_text(size = 18, face = "bold"))
    })
    output$ContactMatrix_plot = renderPlot({pl })

  })

  output$PostProc_filters <- renderUI({
    df <- req(postprocObjects$evolutionCSV )
    show_modal_spinner()

    name_cols <- colnames(df%>% select(-Folder))
    sliders = lapply(name_cols, function(col) {
      values = unique(df[[col]])
      if(col == "Day") values <- values[-c(length(values))]
      sliderInput(
        inputId = paste0("filter_", col),
        label = paste("Select range for", col),
        min = min(values, na.rm = TRUE),
        max = max(values, na.rm = TRUE),
        value = range(values, na.rm = TRUE)
      )
    })
    remove_modal_spinner()
    sliders
  })

  observe({
    df <-req(postprocObjects$evolutionCSV )
    name_cols <- colnames(df%>% select(-Folder))

    for (col in name_cols) {
      input_id <- paste0("filter_", col)
      if (!is.null(input[[input_id]])) {
        df <- df[df[[col]] >= input[[input_id]][1] & df[[col]] <= input[[input_id]][2], ]
      }
    }
    postprocObjects$Filter_evolutionCSV = df
  })

  observe({
    df = req(postprocObjects$Filter_evolutionCSV)
    folders = unique(df$Folder)

    output$PostProc_table <- DT::renderDataTable({
      DT::datatable(
        data.frame( FolderNames = paste(folders)) ,
        options = list(
          pageLength = 5
        ),
        editable = list(target = 'cell'),
        selection = 'single',
        rownames = F
      )
    })

  })


  # Observe table edit and validate input
  observe( {
    info <- input$PostProc_table_cell_clicked
    folder = req(info$value)

    EvolutionDisease_radioButt = input$EvolutionDisease_radioButt
    df <- req(postprocObjects$evolutionCSV )

    df = df %>% filter(Folder == folder) %>% select(-Folder) %>%
      tidyr::gather(-Day, value =  "Number", key = "Compartments")

    fixed_colors <- c("Susceptible" = "green", "Exposed" = "blue", "Infected" = "red", "Recovered" = "purple", "Died" = "black")

    pl = ggplot()
    if(!is.null(EvolutionDisease_radioButt)){
      DfStat = postprocObjects$evolutionCSV %>%
        tidyr::gather(-Day,-Folder, value =  "Number", key = "Compartments") %>%
        group_by( Day,Compartments ) %>%
        summarise(Mean = mean(Number),
                  MinV = min(Number),
                  MaxV = max(Number) )

      if("Area from all simulations" %in% EvolutionDisease_radioButt){
        pl = pl +
          geom_ribbon(data = DfStat,
                      aes(x = Day, ymin = MinV,ymax = MaxV, group= Compartments, fill = Compartments),alpha = 0.4)+
          scale_fill_manual(values = fixed_colors,
                            limits = names(fixed_colors),
                            labels = names(fixed_colors),
                            drop = FALSE)
      }

      if("Mean curves" %in% EvolutionDisease_radioButt){
        pl = pl + geom_line(data = DfStat,
                            aes(x = Day, y = Mean, group= Compartments, col = Compartments, linetype = "Mean Curves"))+
          scale_linetype_manual(values = c("Simulation" = "solid","Mean Curves" = "dashed"))
      }

    }
    pl = pl +
      geom_line(data = df, aes(x = Day, y = Number,col = Compartments, linetype = "Simulation" ), linewidth=1.5)+
      labs(y="Cumulative number of individuals",col="Compartments", linetype="Type")+
      scale_color_manual(values = fixed_colors,
                         limits = names(fixed_colors),
                         labels = names(fixed_colors),
                         drop = FALSE) +
      theme_fancy()


    output$EvolutionPlot <- renderPlot({
      pl
    })
  })

  output$EvolutionPlot <-  output$CountersPlot<- renderPlot({
    ggplot()+labs(title = "Please select from the table which simulation to plot")
  })
  output$A_C_CountersPlot<- renderPlot({
    ggplot()+labs(title = "Please select a room")
  })
  counters_colorsNames <- c( "Agents birth", "Agents deaths", "Agents in quarantine",
                             "Number of swabs", "Number of agents infected \noutside the environment")
  counters_colors = viridisLite::turbo(n = length(counters_colorsNames))
  names(counters_colors) = counters_colorsNames

  observe( {
    info <- input$PostProc_table_cell_clicked
    folder = req(info$value)

    CountersDisease_radioButt = input$CountersDisease_radioButt
    df <- req(postprocObjects$COUNTERScsv )

    df = df %>% filter(Folder == folder) %>% select(-Folder) %>%
      tidyr::gather(-Day, value =  "Number", key = "Counters")

    pl = ggplot()
    if(!is.null(CountersDisease_radioButt)){
      DfStat = postprocObjects$COUNTERScsv %>%
        tidyr::gather(-Day,-Folder, value =  "Number", key = "Counters") %>%
        group_by( Day,Counters ) %>%
        summarise(Mean = mean(Number),
                  MinV = min(Number),
                  MaxV = max(Number) )

      if("Area from all simulations" %in% CountersDisease_radioButt){
        pl = pl +
          geom_ribbon(data = DfStat,
                      aes(x = Day, ymin = MinV,ymax = MaxV, group= Counters, fill = Counters),alpha = 0.4)+
          scale_fill_manual(values = counters_colors,
                            limits = names(counters_colors),
                            labels = names(counters_colors),
                            drop = FALSE)
      }

      if("Mean curves" %in% CountersDisease_radioButt){
        pl = pl + geom_line(data = DfStat,
                            aes(x = Day, y = Mean, group= Counters, col = Counters, linetype = "Mean Curves"))+
          scale_linetype_manual(values = c("Simulation" = "solid","Mean Curves" = "dashed"))
      }

    }
    pl = pl +
      geom_line(data = df, aes(x = Day, y = Number,col = Counters, linetype = "Simulation" ), linewidth=1.5)+
      labs(y="",col="Counters", linetype="Type")+
      scale_color_manual(values = counters_colors,
                         limits = names(counters_colors),
                         labels = names(counters_colors),
                         drop = FALSE)+
      theme_fancy()+facet_wrap(~Counters,scales = "free")


    output$CountersPlot <- renderPlot({
      pl
    })

  })
  observe( {
    info <- input$PostProc_table_cell_clicked
    folder = req(info$value)

    CountersDisease_radioButt = input$A_C_CountersDisease_radioButt
    req(input$Room_Counters_A_C_selectize != "")
    df <- req(postprocObjects$A_C_COUNTERS )


    Room <- input$Room_Counters_A_C_selectize
    Room = str_split(string = Room, pattern = "\\s ; \\s")[[1]]

    counters_colorsNames <- c("Contacts", "Virus concentration")
    names(df)[which(names(df) %in% c("contact_counts", "virus_concentration"))] = counters_colorsNames

    # Define the maximum hour value
    MAX_HOUR <- as.numeric(input$simulation_days) * 24

    df = df %>%
      filter(Folder == folder, CanvasID == Room[1],area == Room[2], Name == Room[3] ) %>%
      select(-CanvasID,-Name,-area,-type) %>%
      select(-Folder) %>%
      complete(hour = full_seq(0:MAX_HOUR, 1), fill = list(Contacts = 0, `Virus concentration` = 0)) %>%
      tidyr::gather(-hour, value =  "Number", key = "Counters")
    A_C_counters_colors = c("#E5D05AFF","#DEF5E5FF")

    pl = ggplot()
    if(!is.null(CountersDisease_radioButt)){
      names(postprocObjects$A_C_COUNTERS)[which(names(postprocObjects$A_C_COUNTERS) %in% c("contact_counts", "virus_concentration"))] = counters_colorsNames

      # Create a complete dataframe with all hours from 0 to MAX_HOUR for each Folder
      DfStat = postprocObjects$A_C_COUNTERS %>%
        filter( CanvasID == Room[1],area == Room[2], Name == Room[3] ) %>%
        select(-CanvasID,-Name,-area,-type,-Folder) %>%
        group_by(hour) %>%
        summarise(Mean_contacts = mean(Contacts),
                  MinV_contacts = min(Contacts),
                  MaxV_contacts = max(Contacts),
                  Mean_aerosol = mean(`Virus concentration`),
                  MinV_aerosol = min(`Virus concentration`),
                  MaxV_aerosol = max(`Virus concentration`)) %>%
        complete(hour = full_seq(0:MAX_HOUR, 1), fill = list(Mean_contacts = 0, MinV_contacts = 0, MaxV_contacts = 0, Mean_aerosol = 0, MinV_aerosol = 0, MaxV_aerosol = 0)) %>%
        pivot_longer(cols = c(MinV_contacts, MaxV_contacts, MinV_aerosol, MaxV_aerosol),
                     names_to = c("Variable", "Counters"),
                     names_pattern = "(MinV|MaxV)_(.*)",
                     values_to = "Value") %>%
        pivot_wider(names_from = Variable, values_from = Value) %>%
        mutate(Counters = if_else(Counters == "contacts", "Contacts","Virus concentration"))

      average_trajectory <- postprocObjects$A_C_COUNTERS %>%
        filter( CanvasID == Room[1],area == Room[2], Name == Room[3] ) %>%
        select(-CanvasID,-Name,-area,-type,-Folder) %>%
        group_by(hour) %>%
        summarise(
          Contacts = mean(Contacts, na.rm = TRUE),
          `Virus concentration` = mean(`Virus concentration`, na.rm = TRUE)
        ) %>%
        ungroup() %>%
        complete(hour = full_seq(0:MAX_HOUR, 1), fill = list(Contacts = 0, `Virus concentration` = 0))

      average_trajectory <- average_trajectory %>%
        pivot_longer(cols = c(Contacts, `Virus concentration`),
                     names_to = "Counters",
                     values_to = "Value")

      if("Area from all simulations" %in% CountersDisease_radioButt){
        pl = pl +
          geom_ribbon(data = DfStat,
                      aes(x = hour, ymin = MinV, ymax = MaxV, group= Counters, fill = Counters),alpha = 0.4)+
          scale_fill_manual(values = A_C_counters_colors,
                            limits = names(A_C_counters_colors),
                            labels = names(A_C_counters_colors),
                            drop = FALSE)
      }

      if("Mean curves" %in% CountersDisease_radioButt){
        pl = pl + geom_line(data = average_trajectory,
                            aes(x = hour, y = Value, group= Counters, col = Counters, linetype = "Mean Curves"))+
          scale_linetype_manual(values = c("Simulation" = "solid","Mean Curves" = "dashed"))
      }

    }
    pl = pl +
      geom_line(data = df, aes(x = hour, y = Number,col = Counters, linetype = "Simulation" ), linewidth=1.5)+
      labs(y="",col="Variable",fill="Variable", x = "Hours", linetype="Type")+
      scale_color_manual(values = A_C_counters_colors,
                         limits = names(A_C_counters_colors),
                         labels = names(A_C_counters_colors),
                         drop = FALSE)+
      theme_fancy()+ facet_wrap(~Counters,scales = "free")


    output$A_C_CountersPlot <- renderPlot({
      pl
    })

  })

  #### end query post processing ####

  #### 2D visualisation ####

  observeEvent(input$PostProc_table_cell_clicked,{
    disable("rds_generation")
    disable("flamegpu_connection")
    info <- input$PostProc_table_cell_clicked
    folder = req(info$value)

    isolate({
      if(is.null(canvasObjects$roomsINcanvas)){
        shinyalert("Error", "A F4F model must loaded", "error", 5000)
        return()
      }

        show_modal_spinner()

        CSVdatapath = paste0(dirPath(), "/" , folder,"/AGENT_POSITION_AND_STATUS.csv")

        dataframe <- read_csv(CSVdatapath)
        colnames(dataframe) <- c( "time", "id", "agent_type", "x", "y", "z",
                                  "disease_state")


        floors = canvasObjects$floors %>% arrange(Order) %>% rename(CanvasID = Name)

        Nfloors = length(floors$CanvasID)
        simulation_log = dataframe %>%
          select(time, id, agent_type, x, y, z, disease_state) %>%
          filter(y %in% seq(0,10*(Nfloors-1),by = 10) | y == 10000)

        floors$y = seq(0,10*(Nfloors-1),by = 10)
        #simulation_log %>% filter(y != 10000) %>% select(y)  %>% distinct() %>% arrange()

        simulation_log = merge(simulation_log, floors %>% select(-ID), all.x = TRUE) %>%
          mutate(time = as.numeric(time)) %>%
          filter(!is.na(time))

        simulation_log = simulation_log %>%
          group_by(id) %>%
          arrange(time) %>%
          #tidyr::complete(time = tidyr::full_seq(time, 1)) %>%
          tidyr::fill(agent_type, x, y, z, CanvasID, Order,disease_state, .direction = "down") %>%
          ungroup() #%>%
        #filter(y != 10000)

        # add agent names to the simulation log!
        if(!is.null(names(canvasObjects$agents))){
          simulation_log = simulation_log %>% mutate(agent_type = names(canvasObjects$agents)[agent_type+1])
        }

        canvasObjects$TwoDVisual <- simulation_log

        simulation_log = simulation_log %>%
          filter(y != 10000)

        remove_modal_spinner()

        ## updating slider and selectize
        step = as.numeric(canvasObjects$starting$step)
        updateNumericInput("animationStep",session = session, value = step, max = max(simulation_log$time)*step)
        updateSliderInput("animation", session = session,
                          max = max(simulation_log$time)*step, min = min(simulation_log$time)*step,
                          value = min(simulation_log$time)*step, step = step )
        updateSelectInput("visualFloor_select", session = session,
                          choices = c("All",unique(floors$CanvasID)))
        updateSelectInput("visualAgent_select", session = session,
                          choices = c("All",sort(unique(simulation_log$agent_type))))
        ##

        shinyalert("Success", paste0("File loaded "), "success", 1000)
    })
  })

  animationStep <- debounce(reactive({input$animationStep}), 1000L)

  observeEvent(animationStep(),{
    req(canvasObjects$TwoDVisual)

    if(is.na(input$animationStep) || input$animationStep == "") {
      shinyalert("The time step cannot be less than 1 sec.", type = "error")
      return()
    }

    if( input$animationStep < 1 ) {
      shinyalert("The time step cannot be less than 1 sec.",type = "error")
      return()
    }

    if( input$animationStep > max(canvasObjects$TwoDVisual$time)* as.numeric(canvasObjects$starting$step) ) {
      shinyalert("The time step cannot be greater than the maximum time of the simulation",type = "error")
      return()
    }

    updateSliderInput("animation", session = session,value = input$animation, step =  input$animationStep)
  })
  observeEvent(input$next_step_visual, {
    req(canvasObjects$TwoDVisual)

    new_val <- min(input$animation +  input$animationStep,
                   max(canvasObjects$TwoDVisual$time)* as.numeric(canvasObjects$starting$step) )

    updateSliderInput(session, "animation", value = new_val)
  })
  # Get unique CanvasIDs
  canvas_ids <- reactive({
    simulation_log = req(canvasObjects$TwoDVisual)
    unique(simulation_log$CanvasID)
  })

  output$TwoDMapPlots <- renderUI({
    simulation_log = req(canvasObjects$TwoDVisual)

    H = length(canvas_ids())*300
    plot_output_list <- plotOutput(outputId = paste0("plot_map"), height = paste0(H,"px") )

    (plot_output_list)
  })

  # Render each plot individually
  observeEvent(input$visualAgent_select,{
    simulation_log = req(canvasObjects$TwoDVisual)

    if(input$visualAgent_select != "All"){
      idAgents = simulation_log %>% filter(agent_type == input$visualAgent_select) %>% select(id) %>% distinct() %>% pull()
      updateSelectInput(session = session, "visualAgentID_select", choices = c("All",sort(idAgents)),selected = "All")
    }
  })

  observe({
    info <- input$PostProc_table_cell_clicked
    folder = req(info$value)

    roomsINcanvas = req(canvasObjects$roomsINcanvas)
    floorSelected = input$visualFloor_select
    colorFeat = input$visualColor_select
    Label = input$visualLabel_select

    isolate({
      step = as.numeric(canvasObjects$starting$step)
      timeIn <- input$animation/step
      timeGrid = seq(0,timeIn,1) # number of steps to reach the seconds selected

      disease = strsplit( isolate(req("SEIRD")), "" )[[1]]

      # Define the fixed colors and shapes
      fixed_colors <- c("S" = "green", "E" = "blue", "I" = "red", "R" = "purple", "D" = "black")
      other_chars <- setdiff(unique(disease), names(fixed_colors))
      random_colors <- sample(colors(), length(other_chars))
      all_colors <- c(fixed_colors, setNames(random_colors, other_chars))

      colorDisease = data.frame(State = names(all_colors), Col = (all_colors),  stringsAsFactors = F)
      colorDisease$State = factor(x = colorDisease$State, levels = disease)

      #####
      Mapping = postprocObjects$MappingID_room

      if(colorFeat == "Area"){
        roomsINcanvas = merge( roomsINcanvas %>% select(-colorFill),
                               canvasObjects$areas %>% select(-ID) ,
                               by.x = "area", by.y = "Name" ) %>% rename(colorFill = Color)
        roomsINcanvas$IDtoColor = roomsINcanvas$area
      }else if(colorFeat == "Type"){
        roomsINcanvas = merge( roomsINcanvas %>% select(-colorFill),
                               canvasObjects$types %>% select(-ID) ,
                               by.x = "type", by.y = "Name" ) %>%
          rename(colorFill = Color)
        roomsINcanvas$IDtoColor = roomsINcanvas$type
      }else if(colorFeat == "Name"){
        roomsINcanvas = merge( roomsINcanvas %>% select(-colorFill),
                               canvasObjects$rooms %>% select(Name,colorFill) ,
                               by.x = "Name", by.y = "Name" )
        roomsINcanvas$IDtoColor = roomsINcanvas$Name
      }else if(colorFeat == "CumulContact"){
        CONTACTcsv = postprocObjects$CONTACTcsv   %>%
          filter(Folder == folder , time <= timeIn) %>%
          select(-Folder)

        if(dim(CONTACTcsv)[1] == 0){
          roomsINcanvas$IDtoColor = 0
        }else{
          CONTACTcsv = CONTACTcsv %>% group_by(CanvasID,Name,area,type) %>%
            summarize(counts = n()) %>%
            rename(IDtoColor = counts)

          CONTACTcsv = Mapping %>% select(Name, CanvasID,type,area) %>% distinct() %>%
            full_join(CONTACTcsv, by = c("Name", "CanvasID","type","area")) %>%
            mutate(IDtoColor = ifelse(is.na(IDtoColor), 0, IDtoColor))

          if("IDtoColor" %in% colnames(roomsINcanvas))
            roomsINcanvas = roomsINcanvas%>% select(- IDtoColor )
          roomsINcanvas = merge(roomsINcanvas,CONTACTcsv)
        }

      }else if(colorFeat == "Aerosol"){
        AEROSOLcsv = postprocObjects$AEROSOLcsv %>%
          filter(Folder == folder , time <= timeIn) %>%
          select(-Folder)

        ### Check if it has all the data for each time step

        if(dim(AEROSOLcsv)[1] == 0){
          roomsINcanvas$IDtoColor = 0
        }else{

          AEROSOLcsv= AEROSOLcsv %>% mutate(difftime = (time-timeIn) ) %>%
            filter(difftime <= 0,  difftime == max(difftime)) %>% select(virus_concentration,type,area,Name,CanvasID) %>%
            rename(IDtoColor = virus_concentration)
          # here i give to each room for each step a virus concetration = 0 when is not present
          AEROSOLcsv <- Mapping %>% select(Name, CanvasID,type,area) %>% distinct() %>%
            left_join(AEROSOLcsv, by = c( "Name", "CanvasID","type","area")) %>%
            mutate(IDtoColor = ifelse(is.na(IDtoColor), 0, IDtoColor))

          if("IDtoColor" %in% colnames(roomsINcanvas))
            roomsINcanvas = roomsINcanvas%>% select(- IDtoColor )
          roomsINcanvas = merge(roomsINcanvas,AEROSOLcsv)
        }
      }else if(colorFeat == "CumulAerosol"){
        AEROSOLcsv = postprocObjects$AEROSOLcsv %>%
          filter(Folder == folder , time <= timeIn)%>%
          group_by(type,area,Name,CanvasID) %>%
          mutate(virus_concentration = cumsum(virus_concentration))

        if(dim(AEROSOLcsv)[1] == 0){
          roomsINcanvas$IDtoColor = 0
        }else{
          AEROSOLcsv= AEROSOLcsv %>% mutate(difftime = (time-timeIn) ) %>%
            filter(difftime <= 0,  difftime == max(difftime)) %>% select(virus_concentration,type,area,Name,CanvasID) %>%
            rename(IDtoColor = virus_concentration)

          # here i give to each room for each step a virus concetration = 0 when is not present
          AEROSOLcsv <- Mapping %>% select(Name, CanvasID,type,area) %>% distinct() %>%
            left_join(AEROSOLcsv, by = c("Name", "CanvasID","type","area")) %>%
            mutate(IDtoColor = ifelse(is.na(IDtoColor), 0, IDtoColor))

          if("IDtoColor" %in% colnames(roomsINcanvas))
            roomsINcanvas = roomsINcanvas%>% select(- IDtoColor )
          roomsINcanvas = merge(roomsINcanvas,AEROSOLcsv)
        }
      }

      df <- roomsINcanvas %>%
        mutate(xmin = x + l,
               xmax = x,
               ymin = y + w,
               ymax = y)

      floors = canvasObjects$floors

      if(floorSelected != "All"){
        df = df %>% filter(CanvasID == floorSelected)
      }else{
        df$CanvasID = factor(df$CanvasID, levels = floors$Name)
      }

      if( colorFeat %in% c("CumulContact","Aerosol","CumulAerosol") ){
        MinCol =0
        if(colorFeat == "Aerosol"){
          MaxCol = max(postprocObjects$AEROSOLcsv %>%
                         filter(Folder == folder) %>% pull(virus_concentration))
        }else if(colorFeat == "CumulContact"){
          MaxCol = max(postprocObjects$CONTACTcsv %>%
                         filter(Folder == folder) %>%
                         group_by(type,area,Name,CanvasID)   %>%
                         count() %>%
                         pull(n) )
        }else if(colorFeat == "CumulAerosol"){
          MaxCol = max(postprocObjects$AEROSOLcsv %>%
                         filter(Folder == folder) %>%
                         group_by(type,area,Name,CanvasID) %>%
                         mutate(virus_concentration = cumsum(virus_concentration)) %>%
                         pull(virus_concentration))
        }

        sc_fill <- scale_fill_gradient(low = "blue", high = "red",
                                       limits=c(MinCol,MaxCol),
                                       guide = "colourbar")
        guide_fill = labs(fill = colorFeat)
      }else{
        df$colorFillParsed = gsub(pattern = "rgba",replacement = "rgb",x = df$colorFill)
        df$colorFillParsed = gsub(pattern = ",",replacement = "/255,",x = df$colorFillParsed)
        df$colorFillParsed = gsub(pattern = ")",replacement = "/255)",x = df$colorFillParsed)

        df$colorFillParsed =sapply(df$colorFillParsed, function(x) eval(parse(text=x)))
        dfcolor = df %>% select(colorFillParsed,IDtoColor) %>% distinct()
        dfcolor$colorFillParsed <- gsub(pattern = "#([A-Fa-f0-9]{6})[A-Fa-f0-9]{2}", replacement = "#\\1", x = dfcolor$colorFillParsed)
        sc_fill = scale_fill_manual( values = dfcolor$colorFillParsed,
                                     breaks = dfcolor$IDtoColor,
                                     drop = FALSE )
        guide_fill = guides(fill = "none" )

      }

      #df = df %>% mutate(ymin = -ymin + max(ymax), ymax = -ymax + max(ymax) )
      # simulation_log = simulation_log  %>% mutate(z = z + min(df$y) )

      pl = ggplot() +
        scale_y_reverse() +
        geom_rect(data = df,
                  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = IDtoColor),
                  color = "black") +
        sc_fill +guide_fill+
        scale_color_manual(values = colorDisease$Col,
                           limits = (colorDisease$State),
                           labels = (colorDisease$State),
                           drop = FALSE) +
        coord_fixed() +
        facet_wrap(~CanvasID,ncol = 2) +
        theme_bw() +
        theme(legend.position = "bottom",
              axis.text = element_text(size = 16),
              axis.title = element_text(size = 20, face = "bold"),
              plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
              legend.text = element_text(size = 14),
              legend.key.size = unit(1.5, 'cm'),
              legend.title = element_text(face = "bold", size = 18),
              strip.text = element_text(size = 18, face = "bold"))

      canvasObjects$plot_2D <- pl

      })

    # ### HeatMap plot
    #
    # plHeatmap = ggplot(simulation_log %>% filter(time == 7)) +
    #   scale_y_reverse() +
    #   facet_wrap(~CanvasID) +
    #   geom_rect(data = df,
    #             aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill= "white")+
    #   stat_density_2d(geom = "polygon",
    #                   aes(x = x, y = z, group = disease_stateString, fill = disease_stateString, alpha = ..level..),
    #                   bins = 4)  +
    #   scale_fill_manual(values = colorDisease$Col,
    #                     limits = (colorDisease$State),
    #                     labels = (colorDisease$State)) +
    #   labs(title = "Agent Concentration Heatmap",
    #        x = "",
    #        y = "",
    #        fill = "Disease State") +
    #   theme_dark()

  })

  observe({
    info <- input$PostProc_table_cell_clicked
    folder = req(info$value)

    pl = req( canvasObjects$plot_2D)
    simulation_log = req(canvasObjects$TwoDVisual)
    timeIn <- req(input$animation)
    colorFeat = input$visualColor_select
    visualAgent = input$visualAgent_select
    visualAgentID = input$visualAgentID_select

    isolate({
      Mapping = postprocObjects$MappingID_room
      step = as.numeric(canvasObjects$starting$step)
      timeIn <- input$animation/step

      Label = input$visualLabel_select
      floorSelected = input$visualFloor_select
      floors = canvasObjects$floors

      df = pl$layers[[1]]$data
      disease = strsplit( isolate(req("SEIRD")), "" )[[1]]
      simulation_log$disease_stateString = disease[simulation_log$disease_state+1]

      shapeAgents = data.frame(Agents = (unique(simulation_log$agent_type)),
                               Shape = 0:(length(unique(simulation_log$agent_type)) -1) ,  stringsAsFactors = F)

      if(floorSelected != "All"){
        simulation_log = simulation_log %>% filter(CanvasID == floorSelected)
      }else{
        simulation_log$CanvasID = factor(simulation_log$CanvasID, levels = floors$Name)
      }

      if(visualAgent != "All"){
        simulation_log = simulation_log %>% filter(agent_type == visualAgent)
        if(visualAgentID != "All"){
          simulation_log = simulation_log %>% filter(id == visualAgentID)
        }
      }

      simulation_log$agent_type = factor(x = simulation_log$agent_type , levels = unique(simulation_log$agent_type))

      simulation_log <- simulation_log %>%
        filter(time <= timeIn) %>%
        group_by(id) %>%
        filter(time == max(time)) %>%
        filter(y != 10000)


      if(colorFeat %in% c("CumulAerosol", "Aerosol") ){
        AEROSOLcsv = postprocObjects$AEROSOLcsv %>%
          filter(Folder == folder , time <= timeIn)

        if(colorFeat == "CumulAerosol")
          AEROSOLcsv = AEROSOLcsv %>%
            group_by(type,area,Name,CanvasID) %>%
            mutate(virus_concentration = cumsum(virus_concentration))

        if(dim(AEROSOLcsv)[1] == 0){
          df$IDtoColor = 0
        }else{
          AEROSOLcsv = AEROSOLcsv %>% mutate(difftime = (timeIn-time) ) %>%
            filter(difftime >= 0,  difftime == min(difftime)) %>%
            select(virus_concentration,type,area,Name,CanvasID) %>%
            rename(IDtoColor = virus_concentration)

          # here i give to each room for each step a virus concetration = 0 when is not present
          AEROSOLcsv <- Mapping %>% select(Name, CanvasID,type,area) %>% distinct() %>%
            left_join(AEROSOLcsv, by = c("Name", "CanvasID","type","area")) %>%
            mutate(IDtoColor = ifelse(is.na(IDtoColor), 0, IDtoColor))

          if("IDtoColor" %in% colnames(df))
            df = df %>% select(-IDtoColor )

          df = merge(df,AEROSOLcsv)
        }
        pl$layers[[1]]$data = df
      }else if(colorFeat == "CumulContact"){
        CONTACTcsv = postprocObjects$CONTACTcsv   %>%
          filter(Folder == folder , time <= timeIn)

        if(dim(CONTACTcsv)[1] == 0){
          df$IDtoColor = 0
        }else{
          CONTACTcsv = CONTACTcsv %>%
            group_by(CanvasID,Name,area,type) %>%
            count() %>%
            rename(IDtoColor = n)
          CONTACTcsv <- Mapping %>% select(Name, CanvasID,type,area) %>% distinct() %>%
            left_join(CONTACTcsv, by = c("Name", "CanvasID","type","area")) %>%
            mutate(IDtoColor = ifelse(is.na(IDtoColor), 0, IDtoColor))

          if("IDtoColor" %in% colnames(df))
            df = df %>% select(-IDtoColor )
          df = merge(df,CONTACTcsv)
        }
        pl$layers[[1]]$data = df
      }

      pl <-pl +
        geom_point(data = simulation_log,
                   aes(x = x, y = z, group = id, shape = agent_type,
                       color = disease_stateString ), size = 5, stroke = 2) +
        scale_shape_manual(values = shapeAgents$Shape,
                           #limits = shapeAgents$Agents,
                           breaks = shapeAgents$Agents) +
                           #drop = FALSE)
        guides(shape = guide_legend(ncol=10, order=1))


      if(! Label  %in% c("None","Agent ID")){
        df = df %>% rename(name = Name)
        pl = pl + geom_label(data = df,
                             aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2,
                                 label = get(tolower(Label)) ),
                             color = "black", size = 4)
      }else if(Label == "Agent ID"){
        dfSim = simulation_log %>% filter(time == timeIn)
        pl = pl + geom_label(data = dfSim,
                             aes(x = x, y = z,
                                 label = id, col = disease_stateString ),
                             size = 4)
      }

      total_seconds = timeIn*step + as.numeric(strsplit(input$initial_time, ":")[[1]][1]) * 60 * 60 + as.numeric(strsplit(input$initial_time, ":")[[1]][2]) * 60
      days <- total_seconds %/% (24 * 3600)  # Number of days
      remaining_seconds <- total_seconds %% (24 * 3600)
      hours <- remaining_seconds %/% 3600  # Number of hours
      remaining_seconds <- remaining_seconds %% 3600
      minutes <- remaining_seconds %/% 60  # Number of minutes
      seconds <- remaining_seconds %% 60   # Remaining seconds
      title = labs(title = paste0(days+1, "d:", hours, "h:",minutes,"m:",seconds,"s (# steps: ", timeIn,")"), x = "", y = "", color = "Disease state", shape = "Agent type")

      output[["plot_map"]] <- renderPlot({ pl + title })

    })
  })

  #### END 2D visualisation ####
  observeEvent(input$save_text_run, {
    removeModal()

    output_text <- system("docker exec flamegpu2-container echo 'Hello from container'", intern = TRUE)
    print(output_text)
  })

}
