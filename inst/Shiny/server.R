source(system.file("Shiny", "Rfunctions.R", package = "FORGE4FLAME"))

options(shiny.maxRequestSize = 2^30)

server <- function(input, output, session) {
  canvasObjects <- reactiveValues(
    rooms = NULL,
    roomsINcanvas = NULL,
    nodesINcanvas = NULL,
    pathINcanvas = NULL,
    types = data.frame(
      Name = c("Normal", "Stair", "Spawnroom", "Fillingroom", "Waitingroom"),
      ID = c(4, 5, 6, 7, 8),
      Color = c(
        "rgba(255, 0, 0, 1)", # Red
        "rgba(0, 255, 0, 1)", # Green
        "rgba(0, 0, 255, 1)", # Blue
        "rgba(0, 0, 0, 1)", # Black
        "rgba(0, 100, 30, 1)"
      )
    ),
    canvasDimension = data.frame(
      canvasWidth = 1000,
      canvasHeight = 800
    ),
    matrixCanvas = matrix(0, nrow = 80, ncol = 100),
    selectedId = 1,
    floors = NULL,
    floorsBG = list(),
    areas = data.frame(
      Name = c("None"),
      ID = c(0),
      Color = c(
        "rgba(0, 0, 0, 1)"
      )
    ),
    agents = NULL,
    disease = NULL,
    resources = NULL,
    color = "Room",
    matricesCanvas = NULL,
    starting = data.frame(seed = NA, simulation_days = 10, day = "Monday", time = "00:00", step = 60, nrun = 100, prun = 10),
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
    outside_contagion = NULL,
    virus_parameters = data.frame(virus_variant = 1, ngen_base = 0.589, vl = 9, decay_rate = 0.636, gravitational_settling_rate = 0.39, inhalation_rate_pure = 0.521),
    cancel_button_selected = FALSE,
    TwoDVisual = NULL,
    width = NULL,
    length = NULL,
    height = NULL,
  )

  InfoApp <- reactiveValues(NumTabsFlow = 0, NumTabsTimeSlot = 1, NumTabsTimeShift = list("shift_1" = 1), tabs_ids = c(), oldAgentType = "", invalidRooms = c())

  canvasObjectsSTART <- canvasObjects

  hideElement("outside_contagion_plot")
  hideElement("DownloadPostProc_Button")

  observeEvent(input$link_canvas_tab, {
    updateTabItems(session, "SideTabs", "canvas_tab")
  })
  observeEvent(input$link_rooms, {
    updateTabItems(session, "SideTabs", "rooms")
  })
  observeEvent(input$link_agents, {
    updateTabItems(session, "SideTabs", "agents")
  })
  observeEvent(input$link_resources, {
    updateTabItems(session, "SideTabs", "resources")
  })
  observeEvent(input$link_infection, {
    updateTabItems(session, "SideTabs", "infection")
  })
  observeEvent(input$link_whatif, {
    updateTabItems(session, "SideTabs", "whatif")
  })
  observeEvent(input$link_settings, {
    updateTabItems(session, "SideTabs", "settings")
  })
  observeEvent(input$link_configuration, {
    updateTabItems(session, "SideTabs", "configuration")
  })
  observeEvent(input$link_run, {
    updateTabItems(session, "SideTabs", "run")
  })
  observeEvent(input$link_post_process, {
    updateTabItems(session, "SideTabs", "post_process")
  })

  observeEvent(input$LoadBG_image, {
    # 1. Ensure a file is present
    req(input$BGfile)

    if (is.null(canvasObjects$floors)) {
      shinyalert("To set the background it is necessary to define a floor.")
      return()
    }

    # 2. Validate extension
    ext <- tolower(tools::file_ext(input$BGfile$name))
    if (!(ext %in% c("dxf", "png"))) {
      showNotification("Please upload a .dxf or .png file.", type = "error", duration = 5)
      return()
    }

    # Handle PNG files
    if (ext == "png") {
      # Read PNG image
      img <- tryCatch(
        png::readPNG(input$BGfile$datapath),
        error = function(e) {
          showNotification(paste("Error reading PNG:", e$message), type = "error", duration = 5)
          return(NULL)
        }
      )
      if (is.null(img)) {
        return()
      }

      # Get image dimensions (in pixels)
      img_height <- dim(img)[1]
      img_width <- dim(img)[2]

      # Get pixels per meter from user input (default to 10 if not set)
      pixels_per_meter <- if (!is.null(input$png_pixels_per_meter) && input$png_pixels_per_meter > 0) {
        input$png_pixels_per_meter
      } else {
        10
      }
      canvas_w <- ceiling(img_width / pixels_per_meter)
      canvas_h <- ceiling(img_height / pixels_per_meter)

      # Update canvas dimensions
      if (length(canvasObjects$floors) > 0) {
        canvas_w <- max(canvas_w, max(canvasObjects$floors$canvasWidth, na.rm = TRUE))
        canvas_h <- max(canvas_h, max(canvasObjects$floors$canvasHeight, na.rm = TRUE))
      }

      canvasObjects$canvasDimension$canvasWidth <- max(canvasObjects$canvasDimension$canvasWidth, canvas_w * 10)
      canvasObjects$canvasDimension$canvasHeight <- max(canvasObjects$canvasDimension$canvasHeight, canvas_h * 10)
      runjs(paste0("shinyjs.canvasDimension({w:", canvasObjects$canvasDimension$canvasWidth, ", h:", canvasObjects$canvasDimension$canvasHeight, "})"))

      canvasObjects$matrixCanvas <- matrix(
        data = 1,
        nrow = canvas_h + 2,
        ncol = canvas_w + 2
      )

      # Create ggplot with PNG as background using annotation_raster
      gg <- ggplot() +
        annotation_raster(img, xmin = 0, xmax = img_width, ymin = 0, ymax = img_height) +
        coord_fixed(ratio = 1, xlim = c(0, img_width), ylim = c(0, img_height), expand = FALSE) +
        theme_void()

      gg -> canvasObjects$floorsBG[[input$canvas_selector]]$plot
      canvasObjects$floorsBG[[input$canvas_selector]]$imWidth <- canvas_w * 10
      canvasObjects$floorsBG[[input$canvas_selector]]$imHeight <- canvas_h * 10

      sendBG(gg, canvas_w * 10, canvas_h * 10, session, input$canvas_selector)
      return()
    }
    # 3. Inspect available layers
    layers_info <- tryCatch(
      sf::st_layers(input$BGfile$datapath),
      error = function(e) {
        showNotification(paste("Cannot read layers:", e$message), type = "error", duration = 5)
        return(NULL)
      }
    )
    if (is.null(layers_info)) {
      return()
    }

    # 4. Read the DXF, catch errors
    # Require a specific layer (e.g. "entities")
    needed_layer <- "entities"
    if (!(needed_layer %in% layers_info$name)) {
      showModal(modalDialog(
        title = "Required Layer Not Found",
        paste("Your DXF file contains these layers:", paste(layers_info$name, collapse = ", ")),
        "Expected layer:", needed_layer,
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
    plan_raw <- tryCatch(
      sf::st_read(dsn = input$BGfile$datapath, layer = needed_layer, quiet = TRUE),
      error = function(e) {
        showNotification(paste("Error reading DXF:", e$message), type = "error", duration = 5)
        return(NULL)
      }
    )
    if (is.null(plan_raw)) {
      return()
    }

    # 5. Check for empty geometry
    if (nrow(plan_raw) == 0 || all(is.na(st_geometry(plan_raw)))) {
      showNotification("No geometries found in the uploaded DXF.", type = "warning", duration = 5)
      return()
    }

    units_info <- read_dxf_units(input$BGfile$datapath)
    if (units_info["unit"] == "unitless") {
      units_info <- detect_units_by_bbox(plan_raw)
    }

    # 2. Scale geometries to meters
    plan <- plan_raw
    sf::st_geometry(plan) <- sf::st_geometry(plan) * as.numeric(units_info["factor"])

    # 3. Compute bounding box & canvas dimensions
    bbox <- sf::st_bbox(plan)
    canvas_w <- as.numeric(ceiling(bbox["xmax"] - bbox["xmin"]))
    canvas_h <- as.numeric(ceiling(bbox["ymax"] - bbox["ymin"]))

    # 4. Store dimensions and notify front-end
    ## Check also the other dimensions of the other floors, and use the max
    if (length(canvasObjects$floors) > 0) {
      canvas_w <- max(canvas_w, max(canvasObjects$floors$canvasWidth, na.rm = TRUE))
      canvas_h <- max(canvas_h, max(canvasObjects$floors$canvasHeight, na.rm = TRUE))
    }

    canvasObjects$canvasDimension$canvasWidth <- max(canvasObjects$canvasDimension$canvasWidth, canvas_w * 10)
    canvasObjects$canvasDimension$canvasHeight <- max(canvasObjects$canvasDimension$canvasHeight, canvas_h * 10)
    runjs(paste0("shinyjs.canvasDimension({w:", canvasObjects$canvasDimension$canvasWidth, ", h:", canvasObjects$canvasDimension$canvasHeight, "})"))

    canvasObjects$matrixCanvas <- matrix(
      data = 1,
      nrow = canvas_h + 2,
      ncol = canvas_w + 2
    )

    # 6. Create ggplot of your floorplan
    gg <- ggplot(plan) +
      geom_sf(color = "black", size = 0.2) +
      theme_void() +
      coord_sf(expand = FALSE)
    gg -> canvasObjects$floorsBG[[input$canvas_selector]]$plot

    canvasObjects$floorsBG[[input$canvas_selector]]$imWidth <- canvas_w * 10
    canvasObjects$floorsBG[[input$canvas_selector]]$imHeight <- canvas_h * 10

    sendBG(gg, canvas_w * 10, canvas_h * 10, session, input$canvas_selector)
  })

  observeEvent(input$set_canvas, {
    disable("rds_generation")
    disable("flamegpu_connection")
    canvasWidth <- canvasObjects$canvasDimension$canvasWidth
    canvasHeight <- canvasObjects$canvasDimension$canvasHeight

    if (input$canvasWidth != "") {
      newCanvasWidth <- round(as.numeric(gsub(" ", "", input$canvasWidth))) * 10
    } # 10 pixel = 1 meter

    if (input$canvasHeight != "") {
      newCanvasHeight <- round(as.numeric(gsub(" ", "", input$canvasHeight))) * 10
    } # 10 pixel = 1 meter

    roomOutsideCanvas <- FALSE
    if (!is.null(canvasObjects$roomsINcanvas)) {
      for (i in 1:nrow(canvasObjects$roomsINcanvas)) {
        if (canvasObjects$roomsINcanvas$door[i] == "bottom" || canvasObjects$roomsINcanvas$door[i] == "top") {
          length <- canvasObjects$roomsINcanvas$w[i]
          width <- canvasObjects$roomsINcanvas$l[i]
        } else {
          length <- canvasObjects$roomsINcanvas$l[i]
          width <- canvasObjects$roomsINcanvas$w[i]
        }

        if ((canvasObjects$roomsINcanvas$x[i] + length + 1) * 10 >= newCanvasWidth || (canvasObjects$roomsINcanvas$y[i] + width + 1) * 10 >= newCanvasHeight) {
          shinyalert("The new canvas dimension is too small. There will be at least one room outside the canvas.")
          return()
        }
      }
    }

    if (input$canvasWidth != "") {
      canvasObjects$canvasDimension$canvasWidth <- newCanvasWidth
    }

    if (input$canvasHeight != "") {
      canvasObjects$canvasDimension$canvasHeight <- newCanvasHeight
    }

    # Passa i valori al canvas in JavaScript
    runjs(paste0("shinyjs.canvasDimension({w:", canvasObjects$canvasDimension$canvasWidth, ", h:", canvasObjects$canvasDimension$canvasHeight, "})"))

    # we add two rows and columns to ensure that the walls are inside the canvas
    canvasObjects$matrixCanvas <- matrix(0,
      nrow = canvasObjects$canvasDimension$canvasHeight / 10,
      ncol = canvasObjects$canvasDimension$canvasWidth / 10
    )
  })


  observeEvent(input$delete_floor, {
    disable("rds_generation")
    disable("flamegpu_connection")
    if (input$canvas_selector != "") {
      canvasObjects$floors <- canvasObjects$floors %>%
        filter(Name != input$canvas_selector)

      if (!is.null(canvasObjects$roomsINcanvas)) {
        canvasObjects$roomsINcanvas <- canvasObjects$roomsINcanvas %>%
          filter(CanvasID != input$canvas_selector)
      }

      if (!is.null(canvasObjects$nodesINcanvas)) {
        canvasObjects$nodesINcanvas <- canvasObjects$nodesINcanvas %>%
          filter(CanvasID != input$canvas_selector)
      }

      runjs(paste0("
        delete FloorArray[\"", input$canvas_selector, "\"];"))

      selected <- ""
      if (nrow(canvasObjects$floors) != 0) {
        selected <- canvasObjects$floors$Name[1]
      } else {
        runjs("$('#canvas_selector').trigger('change');")
      }

      updateSelectizeInput(
        inputId = "canvas_selector",
        selected = selected,
        choices = c("", canvasObjects$floors$Name)
      )

      if (!is.null(canvasObjects$floorsBG[[input$canvas_selector]])) {
        canvasObjects$floorsBG <- canvasObjects$floorsBG[-which(input$canvas_selector == names(canvasObjects$floorsBG))]
      }
    }
  })


  #### update floor  ####
  observeEvent(input$canvas_selector, {
    disable("rds_generation")
    disable("flamegpu_connection")
    if (input$canvas_selector != "" && !input$canvas_selector %in% canvasObjects$floors$Name) {
      Name <- gsub(" ", "", input$canvas_selector)
      if (Name != "") {
        if (!grepl("^[a-zA-Z0-9_]+$", Name)) {
          shinyalert("Error", "Floor name cannot contain special charachters.", type = "error")
          updateSelectizeInput(
            inputId = "canvas_selector",
            selected = "",
            choices = c("", canvasObjects$floors$Name)
          )
          return()
        }
        if (!is.null(canvasObjects$floors) && nrow(canvasObjects$floors) != 0) {
          if (nrow(canvasObjects$floors) > 1000) {
            shinyalert("Error", "The maximum permitted number of floors is 1000.", type = "error")
            return()
          }
          canvasObjects$floors <- rbind(
            canvasObjects$floors,
            data.frame(
              ID = max(canvasObjects$floors$ID) + 1,
              Name = Name, Order = max(canvasObjects$floors$Order) + 1
            )
          )
        } else {
          canvasObjects$floors <- data.frame(ID = 1, Name = Name, Order = 1)
        }
      }
    }
    if (!is.null(canvasObjects$roomsINcanvas)) {
      roomsINcanvasFloor <- canvasObjects$roomsINcanvas %>%
        filter(CanvasID == input$canvas_selector)
      if (nrow(roomsINcanvasFloor) > 0) {
        updateSelectizeInput(
          inputId = "select_RemoveRoom",
          selected = "",
          choices = c("", paste0(roomsINcanvasFloor$Name, " #", roomsINcanvasFloor$ID))
        )
      } else {
        updateSelectizeInput(
          inputId = "select_RemoveRoom",
          selected = "",
          choices = ""
        )
      }
    }
    if (length(canvasObjects$floors) > 0) {
      # removing BG image when change floor
      sendBG(
        ggplot() +
          theme_void() +
          coord_sf(expand = FALSE),
        canvas_w = canvasObjects$canvasDimension$canvasWidth,
        canvas_h = canvasObjects$canvasDimension$canvasHeight,
        session, "empty"
      )
    }
    if (!canvasObjects$floorsBG[[input$canvas_selector]] %>% is.null()) {
      sendBG(canvasObjects$floorsBG[[input$canvas_selector]]$plot,
        canvas_w = canvasObjects$floorsBG[[input$canvas_selector]]$imWidth,
        canvas_h = canvasObjects$floorsBG[[input$canvas_selector]]$imHeight,
        session, input$canvas_selector
      )
    }
  })

  observeEvent(input$HideBG_image, {
    disable("rds_generation")
    disable("flamegpu_connection")
    canvas_selector <- req(input$canvas_selector)
    req(canvasObjects$floorsBG[[canvas_selector]])
    if (input$HideBG_image) {
      sendBG(
        ggplot() +
          theme_void() +
          coord_sf(expand = FALSE),
        canvas_w = canvasObjects$canvasDimension$canvasWidth,
        canvas_h = canvasObjects$canvasDimension$canvasHeight,
        session, "empty"
      )
    } else {
      sendBG(canvasObjects$floorsBG[[input$canvas_selector]]$plot,
        canvas_w = canvasObjects$floorsBG[[input$canvas_selector]]$imWidth,
        canvas_h = canvasObjects$floorsBG[[input$canvas_selector]]$imHeight,
        session, input$canvas_selector
      )
    }
  })

  #### ordering floors
  observeEvent(input$canvas_selector, {
    disable("rds_generation")
    disable("flamegpu_connection")
    if (length(canvasObjects$floors$Name) > 1) {
      output$FloorRank <- renderUI({
        div(
          rank_list(
            text = "Drag the floors in the desired order",
            labels = canvasObjects$floors$Name,
            input_id = paste("list_floors")
          )
        )
      })
    } else {
      output$FloorRank <- renderUI({
        NULL
      })
    }
  })

  ## record the floors order
  observeEvent(input$list_floors, {
    disable("rds_generation")
    disable("flamegpu_connection")
    if (length(canvasObjects$floors$Name) > 1) {
      canvasObjects$floors <- canvasObjects$floors %>% arrange(factor(Name, levels = input$list_floors))
      canvasObjects$floors$Order <- 1:length(canvasObjects$floors$Name)
    }
  })

  #### save new room  ####
  observeEvent(input$save_room, {
    disable("rds_generation")
    disable("flamegpu_connection")
    Name <- gsub(" ", "", tolower(input$id_new_room))

    length_new_room <- as.numeric(gsub(" ", "", gsub(",", "\\.", input$length_new_room)))
    width_new_room <- as.numeric(gsub(" ", "", gsub(",", "\\.", input$width_new_room)))
    height_new_room <- as.numeric(gsub(" ", "", gsub(",", "\\.", input$height_new_room)))

    if (is.na(length_new_room) || is.na(width_new_room) || is.na(height_new_room)) {
      shinyalert("Error", "The height, the lenght and the width must be numbers.", type = "error")
      return()
    }


    if (Name != "" && width_new_room != "" && length_new_room != "" && height_new_room != "") {
      if (Name %in% canvasObjects$rooms$Name) {
        shinyalert("Error", paste0("There already exist a room with name: ", Name, "."), type = "error")
        return()
      }

      if (input$select_type == "") {
        shinyalert("Error", "You must select a type.", type = "error")
        return()
      }

      if (input$select_type %in% names(canvasObjects$agents)) {
        shinyalert("Error", "You can not define a room type using the same name assigned to an agent.", type = "error")
        return()
      }

      if (height_new_room > 10) {
        shinyalert("Error", "The maximum permitted height for a room is 10 meters.", type = "error")
        return()
      }

      if (width_new_room < 2 || length_new_room < 2 || height_new_room < 2) {
        shinyalert("Error", "The dimension of the room can not be smaller than 2x2x2.", type = "error")
        return()
      }

      if (!grepl("(^[A-Za-z]+).*", Name)) {
        shinyalert("Error", "Room name must start with a letter (a-z).", type = "error")
        return()
      }

      if (!grepl("^[a-zA-Z0-9_]+$", Name)) {
        shinyalert("Error", "Room name cannot contain special charachters.", type = "error")
        return()
      }


      samp <- runif(3, 0, 1)

      typeID <- canvasObjects$types$ID[which(input$select_type == canvasObjects$types)]

      newRoom <- data.frame(
        Name = Name, ID = typeID,
        type = input$select_type, w = width_new_room, l = length_new_room, h = height_new_room,
        colorFill = paste0("rgba(", round(255 * samp[1]), ", ", round(255 * samp[2]), ", ", round(255 * samp[3]), ", 1)")
      )

      if (is.null(canvasObjects$rooms)) {
        canvasObjects$rooms <- newRoom
      } else {
        if (Name %in% canvasObjects$rooms$Name) {
          shinyalert("Error", paste0("There already exists a room named ", Name, " (case insensitive). "), type = "error")
          return()
        }

        canvasObjects$rooms <- rbind(
          canvasObjects$rooms,
          newRoom
        )
      }
    } else {
      shinyalert("Error", "All the dimensions must be defined.", type = "error")
      return()
    }

    shinyalert("Success", paste0("The room named ", Name, " is added with success."), "success", 1000)
  })

  ## save new area   ####
  observeEvent(input$select_area, {
    disable("rds_generation")
    disable("flamegpu_connection")

    if (!input$select_area %in% canvasObjects$areas$Name) {
      Name <- gsub(" ", "", input$select_area)
      if (Name != "") {
        if (!grepl("^[a-zA-Z0-9_]+$", Name)) {
          shinyalert("Error", "Area name cannot contain special charachters.", type = "error")
          updateSelectizeInput(
            inputId = "select_area",
            selected = "None",
            choices = c("", unique(canvasObjects$areas$Name))
          )
          return()
        }

        samp <- runif(3, 0, 1)
        if (is.null(canvasObjects$areas)) {
          canvasObjects$areas <- data.frame(Name = Name, ID = 1, Color = paste0("rgba(", round(255 * samp[1]), ", ", round(255 * samp[2]), ", ", round(255 * samp[3]), ", 1)"))
        } else {
          newID <- max(canvasObjects$areas$ID) + 1
          newarea <- data.frame(Name = Name, ID = newID, Color = paste0("rgba(", round(255 * samp[1]), ", ", round(255 * samp[2]), ", ", round(255 * samp[3]), ", 1)"))
          canvasObjects$areas <- rbind(canvasObjects$areas, newarea)
        }
      }
    }

    if (input$select_area != "" && !is.null(canvasObjects$areas)) {
      # update the area color list
      updateSelectInput(
        inputId = "selectInput_color_area",
        choices = unique(canvasObjects$areas$Name)
      )
    }
  })

  ## update rooms list to choose
  observeEvent(canvasObjects$rooms, {
    disable("rds_generation")
    disable("flamegpu_connection")
    updateSelectizeInput(
      inputId = "select_room",
      selected = "",
      choices = c("", unique(canvasObjects$rooms$Name))
    )
    if (input$selectInput_color_room == "") {
      updateSelectInput(inputId = "selectInput_color_room", choices = unique(canvasObjects$rooms$Name))
    } else {
      selected_room <- input$selectInput_color_room
      updateSelectInput(inputId = "selectInput_color_room", selected = selected_room, choices = unique(canvasObjects$rooms$Name))
    }
  })

  observeEvent(canvasObjects$roomsINcanvas, {
    disable("rds_generation")
    disable("flamegpu_connection")
    rooms <- canvasObjects$roomsINcanvas %>% filter(type != "Fillingroom", type != "Stair", type != "Waitingroom")

    roomsAvailable <- c("", unique(paste0(rooms$type, "-", rooms$area)))
    updateSelectizeInput(
      session = session, "Det_select_room_flow",
      choices = roomsAvailable
    )
    updateSelectizeInput(
      session = session, "Rand_select_room_flow",
      choices = roomsAvailable
    )
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

    if (nrow(roomsINcanvasFloor) > 0) {
      updateSelectizeInput(
        inputId = "select_RemoveRoom",
        selected = "",
        choices = c("", paste0(roomsINcanvasFloor$Name, " #", roomsINcanvasFloor$ID))
      )
    } else {
      updateSelectizeInput(
        inputId = "select_RemoveRoom",
        selected = "",
        choices = ""
      )
    }
  })

  observeEvent(input$select_type, {
    disable("rds_generation")
    disable("flamegpu_connection")

    Name <- gsub(" ", "", input$select_type)
    if (Name != "") {
      if (!tolower(Name) %in% tolower(canvasObjects$types$Name)) {
        if (!grepl("(^[A-Za-z]+).*", Name)) {
          shinyalert("Error", "Room name must start with a letter (a-z).", type = "error")
          return()
        }

        if (Name %in% names(canvasObjects$agents)) {
          shinyalert("Error", "You can not define a room type using the same name assigned to an agent.", type = "error")
          updateSelectizeInput(
            inputId = "select_type",
            selected = "",
            choices = c("", canvasObjects$types$Name)
          )
          return()
        }

        if (grepl("-", Name)) {
          shinyalert("Error", "The type cannot contain special charachters.", type = "error")
          updateSelectizeInput(
            inputId = "select_type",
            selected = "",
            choices = c("", canvasObjects$types$Name)
          )
          return()
        }

        if (is.null(canvasObjects$types)) {
          canvasObjects$types <- data.frame(Name = Name, ID = 4, Color = "rgba(0, 0, 0, 1)")
        } else {
          newID <- max(canvasObjects$types$ID) + 1

          newtype <- data.frame(
            Name = Name, ID = newID,
            Color = paste0("rgba(", round(255 * runif(1, 0, 1)), ", ", round(255 * runif(1, 0, 1)), ", ", round(255 * runif(1, 0, 1)), ", ", round(255 * runif(1, 0, 1)), ")")
          )
          canvasObjects$types <- rbind(canvasObjects$types, newtype)
        }
      } else {
        updateSelectizeInput(
          inputId = "select_type",
          selected = canvasObjects$types$Name[which(tolower(Name) == tolower(canvasObjects$types$Name))],
          choices = canvasObjects$types$Name
        )
        return()
      }
    }

    if (input$select_type != "" && !is.null(canvasObjects$types)) {
      # update the color type list
      updateSelectInput(
        inputId = "selectInput_color_type",
        choices = unique(canvasObjects$types$Name)
      )
    }

    if (input$select_type == "Fillingroom") {
      updateSelectizeInput(inputId = "door_new_room", choices = c("right", "left", "top", "bottom", "none"), selected = "none")
      disable("door_new_room")
    } else {
      updateSelectizeInput(inputId = "door_new_room", choices = c("right", "left", "top", "bottom", "none"), selected = "right")
      enable("door_new_room")
    }
  })

  observeEvent(input$select_room, {
    disable("rds_generation")
    disable("flamegpu_connection")
    if (!is.null(canvasObjects$rooms) && input$select_room != "") {
      selectedRoom <- canvasObjects$rooms %>% filter(Name == input$select_room)
      if (selectedRoom$type == "Fillingroom") {
        updateSelectizeInput(inputId = "door_new_room", choices = c("right", "left", "top", "bottom", "none"), selected = "none")
        disable("door_new_room")
      } else {
        updateSelectizeInput(inputId = "door_new_room", choices = c("right", "left", "top", "bottom", "none"), selected = "right")
        enable("door_new_room")
      }
    }
  })

  observeEvent(input$select_room, {
    disable("rds_generation")
    disable("flamegpu_connection")
    canvasObjects$width <- canvasObjects$rooms$w[which(canvasObjects$rooms$Name == input$select_room)]
    canvasObjects$length <- canvasObjects$rooms$l[which(canvasObjects$rooms$Name == input$select_room)]
    canvasObjects$height <- canvasObjects$rooms$h[which(canvasObjects$rooms$Name == input$select_room)]

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
  observeEvent(input$add_room, {
    disable("rds_generation")
    disable("flamegpu_connection")
    # Se non sono presenti piani non è possibile aggiungere stanze
    if (input$canvas_selector == "") {
      shinyalert("Error", "You must select a floor.", type = "error")
      return()
    }
    if (input$select_room != "") {
      roomSelected <- canvasObjects$rooms %>% filter(Name == input$select_room)

      width <- roomSelected$w
      length <- roomSelected$l
      height <- roomSelected$h
      if (input$door_new_room == "left" || input$door_new_room == "right") {
        width <- roomSelected$l
        length <- roomSelected$w
      }

      # FullRoom is a flag to set TRUE if inside the matrix representing
      # the room we want the ID of the room
      matrix <- CanvasToMatrix(canvasObjects, FullRoom = T, canvas = input$canvas_selector)
      # Check if there is still space for the new room
      result <- find_ones_submatrix_coordinates(matrix, target_rows = ceiling(width), target_cols = ceiling(length))
      xnew <- result[2]
      ynew <- result[1]

      if (is.null(xnew) || is.null(ynew)) {
        shinyalert("Error", paste0("No space available in the floor '", input$canvas_selector, "' for a new ", input$select_room, " room."), "error")
        return()
      } else {
        color_type <- canvasObjects$color
        room_color_base <- roomSelected$colorFill
        if (color_type == "Type") {
          room_color_base <- (canvasObjects$types %>% filter(Name == roomSelected$type))$Color
        }

        if (color_type == "Area") {
          room_color_base <- (canvasObjects$areas %>% filter(Name == input$select_area))$Color
        }

        # Ensure base color has alpha = 1
        rgb_match <- regmatches(room_color_base, regexec("rgba?\\(([0-9]+),\\s*([0-9]+),\\s*([0-9]+)", room_color_base))
        if (length(rgb_match[[1]]) >= 4) {
          r <- rgb_match[[1]][2]
          g <- rgb_match[[1]][3]
          b <- rgb_match[[1]][4]
          room_color_base <- paste0("rgba(", r, ", ", g, ", ", b, ", 1)")
        }

        # Apply alpha from slider to the room color for display
        alpha_value <- input$room_fill_alpha
        if (!is.null(alpha_value)) {
          room_color_display <- paste0("rgba(", r, ", ", g, ", ", b, ", ", alpha_value, ")")
        } else {
          room_color_display <- room_color_base
        }

        newroom <- data.frame(
          ID = 1,
          typeID = roomSelected$ID,
          type = roomSelected$type,
          x = xnew, y = ynew,
          center_x = 0, center_y = 0,
          door_x = 0, door_y = 0,
          w = width, l = length, h = height,
          Name = roomSelected$Name,
          door = input$door_new_room,
          colorFill = room_color_display,
          colorFillBase = room_color_base,
          colorBorder = "rgba(0, 0, 0, 1)",
          area = input$select_area,
          CanvasID = input$canvas_selector
        )

        length <- ceiling(length)
        width <- ceiling(width)

        if (input$door_new_room == "top") {
          newroom$door_x <- newroom$x + floor(length / 2) + 1
          newroom$door_y <- newroom$y
          newroom$center_y <- newroom$y + ceiling((width + 1) / 2)
          newroom$center_x <- newroom$x + floor(length / 2) + 1
        } else if (input$door_new_room == "bottom") {
          newroom$door_x <- newroom$x + floor(length / 2) + 1
          newroom$door_y <- newroom$y + width + 1
          newroom$center_y <- newroom$y + floor((width + 1) / 2)
          newroom$center_x <- newroom$x + floor(length / 2) + 1
        } else if (input$door_new_room == "left") {
          newroom$door_x <- newroom$x
          newroom$door_y <- newroom$y + round(width / 2) + 1
          newroom$center_y <- newroom$y + round(width / 2) + 1
          newroom$center_x <- newroom$x + ceiling((length + 1) / 2)
        } else if (input$door_new_room == "right") {
          newroom$door_x <- newroom$x + length + 1
          newroom$door_y <- newroom$y + floor(width / 2) + 1
          newroom$center_y <- newroom$y + floor(width / 2) + 1
          newroom$center_x <- newroom$x + floor((length + 1) / 2)
        }

        if (is.null(canvasObjects$roomsINcanvas)) {
          canvasObjects$roomsINcanvas <- newroom
        } else {
          newroom$ID <- max(canvasObjects$roomsINcanvas$ID, 1) + 1
          canvasObjects$roomsINcanvas <- rbind(canvasObjects$roomsINcanvas, newroom)
        }

        canvasObjects$selectedId <- newroom$ID

        runjs(command_addRoomObject(newroom))

        rooms <- canvasObjects$roomsINcanvas %>% filter(type != "Fillingroom", type != "Stair", type != "Spawnroom")
        roomsAvailable <- c("", unique(paste0(rooms$type, "-", rooms$area)))
        updateSelectizeInput(
          session = session, "room_ventilation",
          choices = roomsAvailable
        )
        updateSelectizeInput(
          session = session, "room_quarantine",
          choices = roomsAvailable
        )

        # If the new room is of type Stair, add it also in the other floors, if any and if there space
        if (roomSelected$type == "Stair" && nrow(canvasObjects$floors) > 1) {
          shinyalert(
            title = "Stairs",
            text = "Do you want to add a Stair room to the other floors as well?",
            type = "info",
            showCancelButton = TRUE,
            confirmButtonText = "Yes",
            cancelButtonText = "No",
            callbackR = function(x) {
              if (x) {
                floor_without_space <- c()
                for (floor in canvasObjects$floors$Name) {
                  if (floor == input$canvas_selector) next

                  matrix <- CanvasToMatrix(canvasObjects, FullRoom = T, canvas = floor)

                  # Check if there is still space for the new room
                  result <- find_ones_submatrix_coordinates(matrix, target_rows = ceiling(width), target_cols = ceiling(length))
                  xnew <- result[2]
                  ynew <- result[1]

                  if (is.null(xnew) || is.null(ynew)) {
                    floor_without_space <- c(floor_without_space, floor)
                  } else {
                    # Add the room
                    newroom$ID <- max(canvasObjects$roomsINcanvas$ID, 1) + 1
                    newroom$CanvasID <- floor

                    canvasObjects$roomsINcanvas <- rbind(canvasObjects$roomsINcanvas, newroom)

                    runjs(command_addRoomObject(newroom))

                    rooms <- canvasObjects$roomsINcanvas %>% filter(type != "Fillingroom", type != "Stair", type != "Spawnroom")
                    roomsAvailable <- c("", unique(paste0(rooms$type, "-", rooms$area)))
                    updateSelectizeInput(
                      session = session, "room_ventilation",
                      choices = roomsAvailable
                    )
                    updateSelectizeInput(
                      session = session, "room_quarantine",
                      choices = roomsAvailable
                    )
                  }
                }

                if (length(floor_without_space) > 0) {
                  showNotification(paste0("A Stair room has been added to each floor, except in ", paste(floor_without_space, collapse = ", "), " because there is no space available (consider adding them if you need them). Adjust its position as needed."), duration = 5)
                } else {
                  showNotification(paste0("A Stair room has been added to each floor. Adjust its position as needed."), duration = 5)
                }
              }
            }
          )
        }
      }
    }
  })

  # Observer for room fill alpha slider - updates all rooms when alpha changes
  observeEvent(input$room_fill_alpha,
    {
      req(canvasObjects$roomsINcanvas)

      alpha_value <- input$room_fill_alpha
      if (is.null(alpha_value)) {
        return()
      }

      # Update colorFill for all rooms based on their base color and new alpha
      for (i in seq_len(nrow(canvasObjects$roomsINcanvas))) {
        room <- canvasObjects$roomsINcanvas[i, ]

        # Get base color (or use colorFill if colorFillBase doesn't exist)
        base_color <- if (!is.null(room$colorFillBase) && !is.na(room$colorFillBase)) {
          room$colorFillBase
        } else {
          room$colorFill
        }

        # Extract RGB values and apply new alpha
        rgb_match <- regmatches(base_color, regexec("rgba?\\(([0-9]+),\\s*([0-9]+),\\s*([0-9]+)", base_color))
        if (length(rgb_match[[1]]) >= 4) {
          r <- rgb_match[[1]][1]
          g <- rgb_match[[1]][2]
          b <- rgb_match[[1]][3]
          new_color <- paste0("rgba(", r, ", ", g, ", ", b, ", ", alpha_value, ")")

          canvasObjects$roomsINcanvas[i, "colorFill"] <- new_color

          # First remove the existing room from the canvas
          runjs(paste0("
          FloorArray[\"", room$CanvasID, "\"].arrayObject.forEach((e, index) => {
            if(e.type === 'rectangle' && e.id === ", room$ID, "){
              FloorArray[\"", room$CanvasID, "\"].arrayObject.splice(index, 1);
            }
          });
        "))

          # Then add it back with the new color
          runjs(command_addRoomObject(canvasObjects$roomsINcanvas[i, ]))
        }
      }
    },
    ignoreInit = TRUE
  )

  deletingRoomFromCanvas <- function(session, objectDelete, canvasObjects) {
    runjs(paste0("
          FloorArray[\"", objectDelete$CanvasID, "\"].arrayObject.forEach(e => {
            if(e.type === \'rectangle\' && e.id === ", objectDelete$ID, "){
              const indexToRemove = FloorArray[\"", objectDelete$CanvasID, "\"].arrayObject.indexOf(e);
                  // Verifica se l'oggetto è stato trovato
                  if (indexToRemove !== -1) {
                    // Rimuovi l'oggetto dall'array
                    FloorArray[\"", objectDelete$CanvasID, "\"].arrayObject.splice(indexToRemove, 1);
                  }
            }
          })"))


    canvasObjects$roomsINcanvas <- canvasObjects$roomsINcanvas %>%
      filter(ID != objectDelete$ID)

    if (nrow(canvasObjects$roomsINcanvas %>% filter(type == objectDelete$type, area == objectDelete$area)) == 0) {
      canvasObjects$rooms_whatif <- canvasObjects$rooms_whatif %>% filter(Type != paste0(objectDelete$type, "-", objectDelete$area))
    }

    if (!is.null(canvasObjects$pathINcanvas)) {
      pathsINcanvasFloor <- canvasObjects$pathINcanvas %>%
        filter(CanvasID == input$canvas_selector)

      if (!is.null(pathsINcanvasFloor)) {
        pIc <- pathsINcanvasFloor
        objectDelete$door_x <- objectDelete$door_x * 10
        objectDelete$door_y <- objectDelete$door_y * 10
        pIc <- pIc %>% filter((fromX == objectDelete$door_x + pIc$offset_x_n1 * 10 & fromY == objectDelete$door_y + pIc$offset_y_n1 * 10) |
          (toX == objectDelete$door_x + pIc$offset_x_n2 * 10 & toY == objectDelete$door_y + pIc$offset_y_n2 * 10))

        for (i in pIc$id) {
          runjs(
            paste0("
            const indexToRemove = FloorArray[\"", objectDelete$CanvasID, "\"].arrayObject.findIndex(obj => obj.type === \'segment\' &&  obj.id === ", i, ");
            if (indexToRemove !== -1) {
              FloorArray[\"", objectDelete$CanvasID, "\"].arrayObject.splice(indexToRemove, 1);
            }
            ")
          )
        }
      }
    }

    rooms <- canvasObjects$roomsINcanvas %>% filter(type != "Fillingroom", type != "Stair")
    roomsAvailable <- c("", unique(paste0(rooms$type, "-", rooms$area)))
    updateSelectizeInput(
      session = session, "room_ventilation",
      choices = roomsAvailable
    )
    updateSelectizeInput(
      session = session, "room_quarantine",
      choices = roomsAvailable
    )
  }

  observeEvent(input$remove_room, {
    disable("rds_generation")
    disable("flamegpu_connection")

    if (input$select_RemoveRoom != "" && !is.null(canvasObjects$roomsINcanvas) && dim(canvasObjects$roomsINcanvas)[1] > 0) {
      objectDelete <- canvasObjects$roomsINcanvas %>%
        mutate(NewID = paste0(Name, " #", ID)) %>%
        filter(NewID == input$select_RemoveRoom)

      roomSameAreaType <- canvasObjects$roomsINcanvas %>% filter(area == objectDelete$area, type == objectDelete$type)

      if (dim(roomSameAreaType)[1] == 1) {
        # The room that we want delete is the last one in the area and type,
        # so we have to check that if it is present in the flows than we have to ask if the user want to delete it

        agents_with_room_type <- c()
        # crea un warning che impedisce di proseguire se la stanza da eliminare è presente in un flusso di un agente
        if (!is.null(canvasObjects$agents)) {
          agents_with_room_type1 <- do.call(rbind, lapply(canvasObjects$agents, "[[", "DeterFlow")) %>%
            select(Name, Room) %>%
            distinct() %>%
            filter(Room == paste0(objectDelete$type, "-", objectDelete$area)) %>%
            pull(Name)

          agents_with_room_type2 <- do.call(rbind, lapply(canvasObjects$agents, "[[", "RandFlow")) %>%
            select(Name, Room) %>%
            distinct() %>%
            filter(Room == paste0(objectDelete$type, "-", objectDelete$area)) %>%
            pull(Name)

          agents_with_room_type <- c(agents_with_room_type1, agents_with_room_type2)

          if (length(agents_with_room_type) > 0) {
            shinyalert(
              title = "Confirmation",
              text = paste0(
                "Pay attention to delete the room '", objectDelete$Name,
                "' as it is the last room available for the flow of the following agents: ",
                paste(unique(agents_with_room_type), collapse = ", "), "."
              ),
              type = "warning",
              showCancelButton = TRUE,
              confirmButtonText = "OK",
              cancelButtonText = "Cancel",
              callbackR = function(x) {
                if (x) {
                  for (a in agents_with_room_type) {
                    if (!is.null(canvasObjects$agents[[a]]$DeterFlow)) {
                      canvasObjects$agents[[a]]$DeterFlow <- canvasObjects$agents[[a]]$DeterFlow %>% filter(Room != paste0(objectDelete$type, "-", objectDelete$area))
                    }
                    if (!is.null(canvasObjects$agents[[a]]$RandFlow)) {
                      canvasObjects$agents[[a]]$RandFlow <- canvasObjects$agents[[a]]$RandFlow %>% filter(Room != paste0(objectDelete$type, "-", objectDelete$area))
                    }
                  }

                  deletingRoomFromCanvas(session, objectDelete, canvasObjects)
                }
              }
            )
            return()
          }
        }

        ### Deleting rooms from what-if tables
        RoomToDelete <- paste0(objectDelete$type, "-", objectDelete$area)
        if (nrow(canvasObjects$rooms_whatif) > 0) {
          canvasObjects$rooms_whatif <- canvasObjects$rooms_whatif %>% filter(Type != RoomToDelete)
        }
      }

      deletingRoomFromCanvas(session, objectDelete, canvasObjects)
    }
  })

  #### Color legend: ####

  observeEvent(input$select_fillColor, {
    if (!is.null(canvasObjects$roomsINcanvas) &&
      dim(canvasObjects$roomsINcanvas)[1] > 0) { # some colors are changed
      canvasObjects$color <- input$select_fillColor

      # First all the rooms of the changed color are removed
      if (input$select_fillColor == "Area") {
        colors <- canvasObjects$areas %>% rename(area = Name)
      } else if (input$select_fillColor == "Type") {
        colors <- canvasObjects$types %>% rename(type = Name)
      } else {
        colors <- canvasObjects$rooms %>%
          select(ID, Name, colorFill) %>%
          rename(Color = colorFill)
      }

      colors <- merge(colors %>% select(-ID), canvasObjects$roomsINcanvas)

      for (canvasID in unique(canvasObjects$roomsINcanvas$CanvasID)) {
        for (id in unique(canvasObjects$roomsINcanvas$ID)) {
          runjs(paste0("
          FloorArray[\"", canvasID, "\"].arrayObject.forEach(e => {
            if(e.type === \'rectangle\' && e.id === ", id, "){
              const indexToRemove = FloorArray[\"", canvasID, "\"].arrayObject.indexOf(e);
              console.log('indexToRemove:', indexToRemove);
                  // Verifica se l'oggetto è stato trovato
                  if (indexToRemove !== -1) {
                  // Rimuovi l'oggetto dall'array
                  FloorArray[\"", canvasID, "\"].arrayObject.splice(indexToRemove, 1);
                  }
            }
          })"))

          # Second all the removed rooms are added with the new colors
          canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$ID == id, "colorFill"] <- colors[colors$ID == id, "Color"]
          runjs(command_addRoomObject(canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$ID == id, ]))
        }
      }
    }
  })

  # room
  output$RoomColors <- renderUI({
    if (!is.null(canvasObjects$rooms) && input$selectInput_color_room != "") {
      col_output_list <- lapply(input$selectInput_color_room, function(name) {
        room <- canvasObjects$rooms %>% filter(Name == name)
        colourpicker::colourInput(paste0("col_", room$Name),
          paste0("Select colour for ", room$Name),
          gsub(
            pattern = ", 1\\)", replacement = "\\)",
            gsub(pattern = "rgba", replacement = "rgb", room$colorFill)
          ),
          allowTransparent = T
        )
      })
      do.call(tagList, col_output_list)
    }
  })
  toListen <- reactive({
    if (!is.null(canvasObjects$rooms)) {
      ListCol <- lapply(canvasObjects$rooms$Name, function(i) {
        if (!is.null(input[[paste0("col_", i)]])) {
          data.frame(Name = i, Col = input[[paste0("col_", i)]])
        }
      })
      ListCol <- ListCol[!sapply(ListCol, is.null)]
    } else {
      ListCol <- list()
    }

    return(ListCol)
  })
  observeEvent(toListen(), {
    disable("rds_generation")
    disable("flamegpu_connection")
    if (length(toListen()) > 0) {
      ColDF <- do.call(
        rbind,
        lapply(canvasObjects$rooms$Name, function(i) {
          if (!is.null(input[[paste0("col_", i)]])) {
            data.frame(
              Name = i,
              ColNew = paste0("rgba(", paste(col2rgb(input[[paste0("col_", i)]]), collapse = ", "), ", 1)")
            )
          }
        })
      )

      ## Check which color has changed for updating the room color

      ColDFmerged <- merge(ColDF, canvasObjects$rooms)
      ColDFmergedFiltered <- ColDFmerged %>% filter(ColNew != colorFill)

      if (dim(ColDFmergedFiltered)[1] > 0) {
        if (!is.null(canvasObjects$roomsINcanvas) &&
          dim(canvasObjects$roomsINcanvas)[1] > 0) { # some colors are changed

          # First all the rooms of the changed color are removed
          objectDelete <- canvasObjects$roomsINcanvas %>%
            filter(Name %in% ColDFmergedFiltered$Name)

          if (input$select_fillColor == "Room") {
            runjs(paste0("
          FloorArray[\"", objectDelete$CanvasID, "\"].arrayObject.forEach(e => {
            if(e.type === \'rectangle\' && e.id === ", objectDelete$ID, "){
              const indexToRemove = FloorArray[\"", objectDelete$CanvasID, "\"].arrayObject.indexOf(e);
              console.log('indexToRemove:', indexToRemove);
                  // Verifica se l'oggetto è stato trovato
                  if (indexToRemove !== -1) {
                  // Rimuovi l'oggetto dall'array
                  FloorArray[\"", objectDelete$CanvasID, "\"].arrayObject.splice(indexToRemove, 1);
                  }
            }
          })"))

            # Second all the removed rooms are added with the new colors
            for (i in objectDelete$ID) {
              canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$ID == i, "colorFill"] <- ColDFmergedFiltered[ColDFmergedFiltered$Name == canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$ID == i, "Name"], "ColNew"]
              runjs(command_addRoomObject(canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$ID == i, ]))
            }
          }
        }

        for (j in ColDFmergedFiltered$Name) {
          canvasObjects$rooms[canvasObjects$rooms$Name == j, "colorFill"] <- ColDFmergedFiltered[ColDFmergedFiltered$Name == j, "ColNew"]
        }
      }
    }
  })

  # areas
  output$AreaColors <- renderUI({
    if (!is.null(canvasObjects$areas) && input$selectInput_color_area != "") {
      name <- input$selectInput_color_area
      canvasObjects$areas$Color[canvasObjects$areas$Name == name] -> color
      div(
        colourpicker::colourInput(paste0("col_area_", name),
          paste0("Select colour for ", name),
          gsub(
            pattern = ", 1\\)", replacement = "\\)",
            gsub(pattern = "rgba", replacement = "rgb", color)
          ),
          allowTransparent = T
        )
      )
    }
  })
  toListen_color_area <- reactive({
    if (!is.null(canvasObjects$areas)) {
      ListCol <- lapply(canvasObjects$areas$Name, function(i) {
        if (!is.null(input[[paste0("col_area_", i)]])) {
          data.frame(Name = i, Col = input[[paste0("col_area_", i)]])
        }
      })
      ListCol <- ListCol[!sapply(ListCol, is.null)]
    } else {
      ListCol <- list()
    }

    return(ListCol)
  })
  observeEvent(toListen_color_area(), {
    disable("rds_generation")
    disable("flamegpu_connection")
    if (length(toListen_color_area()) > 0) {
      ColDF <- do.call(
        rbind,
        lapply(canvasObjects$areas$Name, function(i) {
          if (!is.null(input[[paste0("col_area_", i)]])) {
            data.frame(
              Name = i,
              ColNew = paste0("rgba(", paste(col2rgb(input[[paste0("col_area_", i)]]), collapse = ", "), ", 1)")
            )
          }
        })
      )

      ## Check which color has changed for updating the room color

      ColDFmerged <- merge(ColDF, canvasObjects$areas)
      ColDFmergedFiltered <- ColDFmerged %>% filter(ColNew != Color)

      if (dim(ColDFmergedFiltered)[1] > 0) {
        if (!is.null(canvasObjects$roomsINcanvas) &&
          dim(canvasObjects$roomsINcanvas)[1] > 0) { # some colors are changed

          # First all the rooms of the changed color are removed
          objectDelete <- canvasObjects$roomsINcanvas %>%
            filter(area %in% ColDFmergedFiltered$Name)

          if (input$select_fillColor == "Area") {
            runjs(paste0("
          FloorArray[\"", objectDelete$CanvasID, "\"].arrayObject.forEach(e => {
            if(e.type === \'rectangle\' && e.id === ", objectDelete$ID, "){
              const indexToRemove = FloorArray[\"", objectDelete$CanvasID, "\"].arrayObject.indexOf(e);
              console.log('indexToRemove:', indexToRemove);
                  // Verifica se l'oggetto è stato trovato
                  if (indexToRemove !== -1) {
                  // Rimuovi l'oggetto dall'array
                  FloorArray[\"", objectDelete$CanvasID, "\"].arrayObject.splice(indexToRemove, 1);
                  }
            }
          })"))

            # Second all the removed rooms are added with the new colors
            for (i in ColDFmergedFiltered$Name) {
              canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$area == i, "colorFill"] <- ColDFmergedFiltered[ColDFmergedFiltered$Name == i, "ColNew"]
              for (j in canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$area == i, "ID"]) {
                runjs(command_addRoomObject(canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$ID == j, ]))
              }
            }
          }
        }

        for (j in ColDFmergedFiltered$Name) {
          canvasObjects$areas[canvasObjects$areas$Name == j, "Color"] <- ColDFmergedFiltered[ColDFmergedFiltered$Name == j, "ColNew"]
        }
      }
    }
  })

  # type
  output$TypeColors <- renderUI({
    if (!is.null(canvasObjects$types) && input$selectInput_color_type != "") {
      name <- input$selectInput_color_type
      canvasObjects$types$Color[canvasObjects$types$Name == name] -> color
      div(
        colourpicker::colourInput(paste0("col_type_", name),
          paste0("Select colour for ", name),
          gsub(
            pattern = ", 1\\)", replacement = "\\)",
            gsub(pattern = "rgba", replacement = "rgb", color)
          ),
          allowTransparent = T
        )
      )
    }
  })
  toListen_color_type <- reactive({
    if (!is.null(canvasObjects$types)) {
      ListCol <- lapply(canvasObjects$types$Name, function(i) {
        if (!is.null(input[[paste0("col_type_", i)]])) {
          data.frame(Name = i, Col = input[[paste0("col_type_", i)]])
        }
      })
      ListCol <- ListCol[!sapply(ListCol, is.null)]
    } else {
      ListCol <- list()
    }

    return(ListCol)
  })

  observeEvent(toListen_color_type(), {
    disable("rds_generation")
    disable("flamegpu_connection")
    if (length(toListen_color_type()) > 0) {
      ColDF <- do.call(
        rbind,
        lapply(canvasObjects$types$Name, function(i) {
          if (!is.null(input[[paste0("col_type_", i)]])) {
            data.frame(
              Name = i,
              ColNew = paste0("rgba(", paste(col2rgb(input[[paste0("col_type_", i)]]), collapse = ", "), ", 1)")
            )
          }
        })
      )

      ## Check which color has changed for updating the room color

      ColDFmerged <- merge(ColDF, canvasObjects$types)
      ColDFmergedFiltered <- ColDFmerged %>% filter(ColNew != Color)


      if (input$select_fillColor == "Type") {
        if (dim(ColDFmergedFiltered)[1] > 0) {
          # First all the rooms of the changed color are removed
          objectDelete <- canvasObjects$roomsINcanvas %>%
            filter(type %in% ColDFmergedFiltered$Name)

          runjs(paste0("
          FloorArray[\"", objectDelete$CanvasID, "\"].arrayObject.forEach(e => {
            if(e.type === \'rectangle\' && e.id === ", objectDelete$ID, "){
              const indexToRemove = FloorArray[\"", objectDelete$CanvasID, "\"].arrayObject.indexOf(e);
              console.log('indexToRemove:', indexToRemove);
                  // Verifica se l'oggetto è stato trovato
                  if (indexToRemove !== -1) {
                  // Rimuovi l'oggetto dall'array
                  FloorArray[\"", objectDelete$CanvasID, "\"].arrayObject.splice(indexToRemove, 1);
                  }
            }
          })"))

          # Second all the removed rooms are added with the new colors
          for (i in ColDFmergedFiltered$Name) {
            canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$type == i, "colorFill"] <- ColDFmergedFiltered[ColDFmergedFiltered$Name == i, "ColNew"]
            for (j in canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$type == i, "ID"]) {
              runjs(command_addRoomObject(canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$ID == j, ]))
            }
          }
        }
      }

      for (j in ColDFmergedFiltered$Name) {
        canvasObjects$types[canvasObjects$types$Name == j, "Color"] <- ColDFmergedFiltered[ColDFmergedFiltered$Name == j, "ColNew"]
      }
    }
  })

  ##### DRAW points: ####
  observeEvent(input$add_point, {
    disable("rds_generation")
    disable("flamegpu_connection")
    if (!is.null(canvasObjects$roomsINcanvas)) {
      roomsINcanvasFloor <- canvasObjects$roomsINcanvas %>%
        filter(CanvasID == input$canvas_selector)

      matrix <- CanvasToMatrix(canvasObjects, canvas = input$canvas_selector)
      # check if there is still space for the new room
      result <- which(matrix == 0, arr.ind = TRUE)
      result <- result[which(!result[, 1] %in% c(1, nrow(matrix))), ]
      result <- result[which(!result[, 2] %in% c(1, nrow(matrix))), ]
      if (dim(result)[1] == 0) {
        result <- NULL
      } else {
        result <- result[1, ]
      }
      xnew <- result[2] - 1
      ynew <- result[1] - 1
    } else {
      xnew <- runif(1, min = 1, max = canvasObjects$canvasDimension$canvasWidth / 10 - 1)
      ynew <- runif(1, min = 1, max = canvasObjects$canvasDimension$canvasHeight / 10 - 1)
    }

    newpoint <- data.frame(ID = 1, x = xnew, y = ynew, CanvasID = input$canvas_selector)

    if (is.null(canvasObjects$nodesINcanvas)) {
      canvasObjects$nodesINcanvas <- newpoint
    } else {
      newpoint$ID <- max(canvasObjects$nodesINcanvas$ID) + 1
      canvasObjects$nodesINcanvas <- rbind(canvasObjects$nodesINcanvas, newpoint)
    }

    runjs(paste0("// Crea un nuovo oggetto Circle con le proprietà desiderate
                const newPoint = new Circle(", newpoint$ID, ",", newpoint$x * 10 + 5, " , ", newpoint$y * 10 + 5, ", 5, rgba(0, 127, 255, 1));
                // Aggiungi il nuovo oggetto Circle all'array arrayObject
                FloorArray[\"", newpoint$CanvasID, "\"].arrayObject.push(newPoint);"))
  })

  observeEvent(input$remove_point, {
    disable("rds_generation")
    disable("flamegpu_connection")
    if (!is.null(canvasObjects$nodesINcanvas) && dim(canvasObjects$nodesINcanvas)[1] > 0) {
      nodesINcanvasFloor <- canvasObjects$nodesINcanvas %>%
        filter(CanvasID == input$canvas_selector)

      deletedPoint <- nodesINcanvasFloor[length(nodesINcanvasFloor$ID), ]

      runjs(paste0("
        const indexToRemove = FloorArray[\"", deletedPoint$CanvasID, "\"].arrayObject.findIndex(obj => obj.type === \'circle\' &&  obj.id === ", deletedPoint$ID, ");
        if (indexToRemove !== -1) {
          FloorArray[\"", deletedPoint$CanvasID, "\"].arrayObject.splice(indexToRemove, 1);
        }
        "))

      if (!is.null(canvasObjects$pathINcanvas)) {
        pathsINcanvasFloor <- canvasObjects$pathINcanvas %>%
          filter(CanvasID == input$canvas_selector)

        if (!is.null(pathsINcanvasFloor)) {
          pIc <- pathsINcanvasFloor
          deletedPoint$x <- deletedPoint$x * 10
          deletedPoint$y <- deletedPoint$y * 10
          pIc <- pIc %>% filter((fromX == deletedPoint$x & fromY == deletedPoint$y) |
            (toX == deletedPoint$x & toY == deletedPoint$y))

          for (i in pIc$id) {
            runjs(
              paste0("
            const indexToRemove = FloorArray[\"", deletedPoint$CanvasID, "\"].arrayObject.findIndex(obj => obj.type === \'segment\' &&  obj.id === ", i, ");
            if (indexToRemove !== -1) {
              FloorArray[\"", deletedPoint$CanvasID, "\"].arrayObject.splice(indexToRemove, 1);
            }
            ")
            )
          }
        }
      }

      canvasObjects$nodesINcanvas <- canvasObjects$nodesINcanvas %>%
        filter(ID != deletedPoint$ID)

      if (nrow(canvasObjects$nodesINcanvas) == 0) {
        canvasObjects$nodesINcanvas <- NULL
      }
    }
  })

  observeEvent(input$clear_all, {
    disable("rds_generation")
    disable("flamegpu_connection")
    if (!is.null(canvasObjects$roomsINcanvas)) {
      canvasObjects$roomsINcanvas <- canvasObjects$roomsINcanvas %>%
        filter(CanvasID != input$canvas_selector)
      if (!is.null(canvasObjects$agents)) {
        for (a in 1:length(canvasObjects$agents)) {
          if (!is.null(canvasObjects$agents[[a]]$DeterFlow)) {
            roomparts <- strsplit(canvasObjects$agents[[a]]$DeterFlow$Room, "-")
            {
              for (i in 1:length(roomparts)) {
                if (nrow(canvasObjects$roomsINcanvas %>% filter(type == roomparts[[i]][1], area == roomparts[[i]][2])) == 0) {
                  canvasObjects$agents[[a]]$DeterFlow <- canvasObjects$agents[[a]]$DeterFlow %>% filter(Room != canvasObjects$agents[[a]]$DeterFlow$Room[i])
                }
              }
            }
            roomparts <- strsplit(canvasObjects$agents[[a]]$RandFlow$Room, "-")
            {
              for (i in 1:length(roomparts)) {
                if (nrow(canvasObjects$roomsINcanvas %>% filter(type == roomparts[[i]][1], area == roomparts[[i]][2])) == 0) {
                  canvasObjects$agents[[a]]$RandFlow <- canvasObjects$agents[[a]]$RandFlow %>% filter(Room != canvasObjects$agents[[a]]$RandFlow$Room[i])
                }
              }
            }
          }
        }
      }
    }

    if (!is.null(canvasObjects$nodesINcanvas)) {
      canvasObjects$nodesINcanvas <- canvasObjects$nodesINcanvas %>%
        filter(CanvasID != input$canvas_selector)
    }

    runjs(paste0("
        FloorArray[\"", input$canvas_selector, "\"].arrayObject = new Array(0)"))
  })

  observeEvent(input$path_generation, {
    disable("rds_generation")
    disable("flamegpu_connection")
    nodes <- NULL

    if (!is.null(canvasObjects$nodesINcanvas)) {
      nodesINcanvasFloor <- canvasObjects$nodesINcanvas %>%
        filter(CanvasID == input$canvas_selector) %>%
        mutate(offset_x = 0, offset_y = 0, door = "none")

      nodesINcanvasFloor <- unique(nodesINcanvasFloor)

      if (nrow(nodesINcanvasFloor) >= 1) {
        nodes <- nodesINcanvasFloor
      }
    }

    # CanvasToMatrix(canvasObjects, canvas = input$canvas_selector)


    if (!is.null(canvasObjects$roomsINcanvas)) {
      if (is.null(nodes)) {
        maxID <- 0
      } else {
        maxID <- max(nodes$ID)
      }

      roomsINcanvasFloor <- canvasObjects$roomsINcanvas %>%
        filter(CanvasID == input$canvas_selector, door != "none") %>%
        mutate(ID = ID + maxID, x = door_x, y = door_y, CanvasID = CanvasID) %>%
        select(ID, x, y, CanvasID, door)

      offsets_x <- c()
      offsets_y <- c()
      for (i in 1:nrow(roomsINcanvasFloor)) {
        if (roomsINcanvasFloor$door[i] == "bottom") {
          roomsINcanvasFloor$y[i] <- roomsINcanvasFloor$y[i] + 1
          offsets_x <- c(offsets_x, 0)
          offsets_y <- c(offsets_y, 1)
        } else if (roomsINcanvasFloor$door[i] == "left") {
          roomsINcanvasFloor$x[i] <- roomsINcanvasFloor$x[i] - 1
          offsets_x <- c(offsets_x, 0)
          offsets_y <- c(offsets_y, 0)
        } else if (roomsINcanvasFloor$door[i] == "top") {
          roomsINcanvasFloor$y[i] <- roomsINcanvasFloor$y[i] - 1
          offsets_x <- c(offsets_x, 0)
          offsets_y <- c(offsets_y, 0)
        } else {
          roomsINcanvasFloor$x[i] <- roomsINcanvasFloor$x[i] + 1
          offsets_x <- c(offsets_x, 1)
          offsets_y <- c(offsets_y, 0)
        }
      }

      roomsINcanvasFloor <- roomsINcanvasFloor %>%
        mutate(offset_x = offsets_x, offset_y = offsets_y)

      if (!is.null(nodes)) {
        nodes <- rbind(nodes, roomsINcanvasFloor)
      } else {
        nodes <- roomsINcanvasFloor
      }
    }

    ######
    # Let's generate the dataframe in which we save all the possible paths
    pathINcanvasLIST <- list()
    k <- 1
    for (id in nodes$ID) {
      n1 <- nodes %>% filter(ID == id)
      for (id2 in nodes$ID[nodes$ID > id]) {
        n2 <- nodes %>% filter(ID == id2)
        if ((n1$door == "none" || n2$door == "none") ||
          (n1$door == "right" && ((n2$door == "right" && n2$x == n1$x) || (n2$door == "left" && n2$x > n1$x) || (n2$door == "top" && n2$x > n1$x && n2$y > n1$y) || (n2$door == "bottom" && n2$x > n1$x && n2$y < n1$y))) ||
          (n1$door == "left" && ((n2$door == "left" && n2$x == n1$x) || (n2$door == "right" && n2$x < n1$x) || (n2$door == "top" && n2$x < n1$x && n2$y > n1$y) || (n2$door == "bottom" && n2$x < n1$x && n2$y < n1$y))) ||
          (n1$door == "top" && ((n2$door == "top" && n2$y == n1$y) || (n2$door == "bottom" && n2$y < n1$y) || (n2$door == "left" && n2$y < n1$y && n2$x > n1$x) || (n2$door == "right" && n2$y < n1$y && n2$x < n1$x))) ||
          (n1$door == "bottom" && ((n2$door == "bottom" && n2$y == n1$y) || (n2$door == "top" && n2$y > n1$y) || (n2$door == "left" && n2$y > n1$y && n2$x > n1$x) || (n2$door == "right" && n2$y > n1$y && n2$x < n1$x)))) {
          pathINcanvasLIST[[k]] <- data.frame(
            id = k,
            fromX = n1$x * 10, fromY = n1$y * 10,
            toX = n2$x * 10, toY = n2$y * 10, CanvasID = input$canvas_selector,
            offset_x_n1 = n1$offset_x, offset_y_n1 = n1$offset_y,
            offset_x_n2 = n2$offset_x, offset_y_n2 = n2$offset_y
          )
          k <- k + 1
        }
      }
    }

    pIc <- NULL

    if (!is.null(canvasObjects$pathINcanvas)) {
      pIc <- canvasObjects$pathINcanvas %>%
        filter(CanvasID == input$canvas_selector)

      canvasObjects$pathINcanvas <- canvasObjects$pathINcanvas %>%
        filter(CanvasID != input$canvas_selector)
    }

    pathINcanvasLIST <- do.call(rbind, pathINcanvasLIST)

    canvasObjects$pathINcanvas <- rbind(canvasObjects$pathINcanvas, pathINcanvasLIST)
    ######

    if (!is.null(pIc)) {
      for (i in pIc$id) {
        runjs(
          paste0("
          const indexToRemove = FloorArray[\"", input$canvas_selector, "\"].arrayObject.findIndex(obj => obj.type === \'segment\' &&  obj.id === ", i, ");
          if (indexToRemove !== -1) {
            FloorArray[\"", input$canvas_selector, "\"].arrayObject.splice(indexToRemove, 1);
          }
          ")
        )
      }
    }


    for (i in pathINcanvasLIST$id) {
      pIc <- pathINcanvasLIST %>% filter(id == i)
      path <- bresenham(c(pIc$fromX / 10, pIc$toX / 10), c(pIc$fromY / 10, pIc$toY / 10))
      matrixCanvas <- CanvasToMatrix(canvasObjects, canvas = input$canvas_selector)
      sum <- 0
      for (j in 1:length(path$x)) {
        if (matrixCanvas[path$y[j], path$x[j]] == 1) {
          sum <- sum + 1
        }
      }
      if (sum == 0) {
        runjs(paste0(
          "// Crea un nuovo oggetto path
                const newPath = new Segment(", pIc$id, ",",
          pIc$fromX - pIc$offset_x_n1 * 10, " , ", pIc$fromY - pIc$offset_y_n1 * 10,
          " , ", pIc$toX - pIc$offset_x_n2 * 10, " , ", pIc$toY - pIc$offset_y_n2 * 10,
          ");
                // Aggiungi il nuovo oggetto Segment all'array arrayObject
                FloorArray[\"", input$canvas_selector, "\"].arrayObject.push(newPath);"
        ))
      }
    }
  })

  ####

  observeEvent(input$selected, {
    disable("rds_generation")
    disable("flamegpu_connection")
    if (!is.null(input$id)) {
      if (input$type == "circle") {
        x <- floor(input$x / 10)
        y <- floor(input$y / 10)
      }
      # else{
      #   x = input$x/10
      #   y = input$y/10
      # }

      # length = ceiling(canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$ID == input$id, "l"])
      # width = ceiling(canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$ID == input$id, "w"])

      if (input$type == "circle") {
        canvasObjects$nodesINcanvas[canvasObjects$nodesINcanvas$ID == input$id, c("x", "y")] <- c(x, y)
      }
      # else{
      #   canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$ID == input$id,c("x","y")] = c(x, y)
      #
      #   if(canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$ID == input$id, "door"] == "top"){
      #     canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$ID == input$id,"door_x"] = x + floor(length/2) + 1
      #     canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$ID == input$id,"door_y"] = y
      #     canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$ID == input$id,"center_y"] = y + ceiling((width + 1) / 2)
      #     canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$ID == input$id,"center_x"] = x + floor(length/2) + 1
      #   }
      #   else if(canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$ID == input$id, "door"] == "bottom"){
      #     canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$ID == input$id,"door_x"] = x + floor(length/2) + 1
      #     canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$ID == input$id,"door_y"] = y + width + 1
      #     canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$ID == input$id,"center_y"] = y + floor((width + 1) / 2)
      #     canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$ID == input$id,"center_x"] = x + floor(length/2) + 1
      #   }
      #   else if(canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$ID == input$id, "door"] == "left"){
      #     canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$ID == input$id,"door_x"] = x
      #     canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$ID == input$id,"door_y"] = y + round(width/2) + 1
      #     canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$ID == input$id,"center_y"] = y + round(width/2) + 1
      #     canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$ID == input$id,"center_x"] = x + ceiling((length + 1) / 2)
      #   }
      #   else if(canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$ID == input$id, "door"] == "right"){
      #     canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$ID == input$id,"door_x"] = x + length + 1
      #     canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$ID == input$id,"door_y"] = y + floor(width/2) + 1
      #     canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$ID == input$id,"center_y"] = y + floor(width/2) + 1
      #     canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$ID == input$id,"center_x"] = x + floor((length + 1) / 2)
      #   }
      # }

      canvasObjects$selectedId <- input$id
    }
  })

  observeEvent(input$movement_completed, {
    room <- input$movement_completed

    room$x <- room$x / 10
    room$y <- room$y / 10
    room$length <- ceiling(room$length / 10)
    room$width <- ceiling(room$width / 10)

    if (room$side == "top") {
      room$door_x <- room$x + floor(room$length / 2) + 1
      room$door_y <- room$y
      room$center_y <- room$y + ceiling((room$width + 1) / 2)
      room$center_x <- room$x + floor(room$length / 2) + 1
    } else if (room$side == "bottom") {
      room$door_x <- room$x + floor(room$length / 2) + 1
      room$door_y <- room$y + room$width + 1
      room$center_y <- room$y + floor((room$width + 1) / 2)
      room$center_x <- room$x + floor(room$length / 2) + 1
    } else if (room$side == "left") {
      room$door_x <- room$x
      room$door_y <- room$y + round(room$width / 2) + 1
      room$center_y <- room$y + round(room$width / 2) + 1
      room$center_x <- room$x + ceiling((room$length + 1) / 2)
    } else if (room$side == "right") {
      room$door_x <- room$x + room$length + 1
      room$door_y <- room$y + floor(room$width / 2) + 1
      room$center_y <- room$y + floor(room$width / 2) + 1
      room$center_x <- room$x + floor((room$length + 1) / 2)
    }

    canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$ID == room$id, c("x", "y")] <- c(room$x, room$y)
    canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$ID == room$id, "door_x"] <- room$door_x
    canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$ID == room$id, "door_y"] <- room$door_y
    canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$ID == room$id, "center_y"] <- room$center_y
    canvasObjects$roomsINcanvas[canvasObjects$roomsINcanvas$ID == room$id, "center_x"] <- room$center_x

    matrix <- CanvasToMatrix(canvasObjects, canvas = input$canvas_selector)

    if (!room$movement_completed || nrow(canvasObjects$roomsINcanvas) <= 1 || room$type == "circle" || (room$center_x == 0 && room$center_y == 0)) {
      return()
    }

    valid_rooms <- is_room_connected(matrix, room, canvasObjects$roomsINcanvas %>% filter(CanvasID == input$canvas_selector), if (!is.null(canvasObjects$nodesINcanvas)) canvasObjects$nodesINcanvas %>% filter(CanvasID == input$canvas_selector) else NULL)

    if (!valid_rooms) {
      showNotification("The room you just placed is not connected to any other room or graph point on the canvas. Please, move it in a different position.", duration = 5, type = "warning")
      if (length(InfoApp$invalidRooms[InfoApp$invalidRooms == room$id]) == 0) {
        InfoApp$invalidRooms <- c(InfoApp$invalidRooms, room$id)
      }
    } else {
      InfoApp$invalidRooms <- InfoApp$invalidRooms[InfoApp$invalidRooms != room$id]
    }
  })

  observeEvent(input$check, {
    disable("rds_generation")
    disable("flamegpu_connection")

    output <- check(canvasObjects, input, output, InfoApp)

    is_docker <- file.exists("/.dockerenv")
    is_docker_compose <- Sys.getenv("DOCKER_COMPOSE") == "ON"
    if (!is.null(output) && (!is_docker || is_docker_compose)) {
      enable("flamegpu_connection")
    }

    if (!is.null(output)) {
      enable("rds_generation")
    }

    enable("run")
  })

  observeEvent(input$check_run, {
    disable("rds_generation")
    disable("flamegpu_connection")

    output <- check(canvasObjects, input, output, InfoApp)

    is_docker <- file.exists("/.dockerenv")
    is_docker_compose <- Sys.getenv("DOCKER_COMPOSE") == "ON"
    if (!is.null(output) && (!is_docker || is_docker_compose)) {
      enable("flamegpu_connection")
    }

    if (!is.null(output)) {
      enable("rds_generation")
    }

    enable("run")
  })

  output$rds_generation <- downloadHandler(
    filename = function() {
      paste0("model", Sys.Date(), ".zip")
    },
    content = function(file) {
      postprocObjects$simulation_log_folder <- NULL
      postprocObjects$simulation_log <- NULL
      postprocObjects$plot_2D <- NULL
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)

      matricesCanvas <- list()
      for (cID in unique(canvasObjects$roomsINcanvas$CanvasID)) {
        matricesCanvas[[cID]]$floor <- CanvasToMatrix(canvasObjects, canvas = cID)
        matricesCanvas[[cID]]$rooms <- CanvasRoomToMatrix(canvasObjects, canvas = cID)
      }
      canvasObjects$matricesCanvas <- matricesCanvas

      model <- reactiveValuesToList(canvasObjects)

      file_name <- glue("model.RDs")
      saveRDS(model, file = file.path(temp_directory, file_name))

      out <- FromToMatrices.generation(model)
      model$rooms_whatif <- out$RoomsMeasuresFromTo
      model$agents_whatif <- out$AgentMeasuresFromTo
      model$initial_infected <- out$initial_infected
      model$outside_contagion$percentage_infected <- as.character(model$outside_contagion$percentage_infected)
      model$floorsBG <- NULL
      write_json(x = model, path = file.path(temp_directory, gsub(".RDs", ".json", file_name)))

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

    matricesCanvas <- list()
    for (cID in unique(canvasObjects$roomsINcanvas$CanvasID)) {
      matricesCanvas[[cID]]$floor <- CanvasToMatrix(canvasObjects, canvas = cID)
      matricesCanvas[[cID]]$rooms <- CanvasRoomToMatrix(canvasObjects, canvas = cID)
    }
    canvasObjects$matricesCanvas <- matricesCanvas

    postprocObjects$simulation_log <- NULL
    postprocObjects$simulation_log_folder <- NULL
    postprocObjects$plot_2D <- NULL

    model <- reactiveValuesToList(canvasObjects)

    file_name <- glue("model.RDs")
    saveRDS(model, file = file.path(paste0("FLAMEGPU-FORGE4FLAME/resources/f4f/", input$popup_text), file_name))

    out <- FromToMatrices.generation(model)
    model$rooms_whatif <- out$RoomsMeasuresFromTo
    model$agents_whatif <- out$AgentMeasuresFromTo
    model$initial_infected <- out$initial_infected
    model$outside_contagion$percentage_infected <- as.character(model$outside_contagion$percentage_infected)
    model$floorsBG <- NULL
    file_name <- glue("model.json")
    write_json(x = model, path = file.path(paste0("FLAMEGPU-FORGE4FLAME/resources/f4f/", input$popup_text), file_name))

    success_text <- "Model linked to FLAME GPU 2 in FLAMEGPU-FORGE4FLAME/resources/f4f/."

    shinyalert("Success", success_text, "success", 1000)
  })

  ### Load: ####

  # general upload in the app
  observeEvent(input$LoadRDs_Button, {
    disable("rds_generation")
    disable("flamegpu_connection")
    if (!is.null(canvasObjects$roomsINcanvas)) { ### alert!!! if it is already present!
      showModal(modalDialog(
        title = "Important message",
        "Do you want to update the rooms by clearing the floor?",
        easyClose = TRUE,
        footer = tagList(
          actionButton("confirmUpload", "Update"),
          modalButton("Cancel")
        )
      ))
    } else {
      isolate({
        postprocObjects$FLAGmodelLoaded <- FALSE
        postprocObjects$DirPath <- NULL
        postprocObjects$Filter_evolutionCSV <- NULL
        postprocObjects$CONTACTcsv <- NULL
        postprocObjects$CONTACT_std <- NULL
        postprocObjects$CONTACTmatrix <- NULL
        postprocObjects$AEROSOL_std <- NULL
        postprocObjects$AEROSOLcsv <- NULL
        postprocObjects$COUNTERScsv <- NULL
        postprocObjects$A_C_COUNTERS <- NULL
        postprocObjects$Mapping <- NULL
        postprocObjects$MappingID_room <- FALSE
        postprocObjects$Model <- NULL
        if (is.null(input$RDsImport) || !file.exists(input$RDsImport$datapath) || !grepl(".RDs", input$RDsImport$datapath)) {
          shinyalert("Error", "Please select one RDs file.", "error")
          return()
        }

        mess <- readRDS(input$RDsImport$datapath)
        messNames <- names(mess)

        if (!all(messNames[-length(messNames)] %in% names(canvasObjectsSTART))) {
          shinyalert(
            "Error",
            paste(mess[["message"]], "\n The file must be RDs saved throught this application."),
            "error"
          )
          return()
        }

        textSucc <- UpdatingData(input, output, canvasObjects, mess, areasColor, session)
        shinyalert("Success", textSucc, "success", 1000)
        updateTabsetPanel(session, "SideTabs", selected = "canvas_tab")
      })
    }
    postprocObjects$FLAGmodelLoaded <- TRUE
  })

  observeEvent(input$confirmUpload, {
    disable("rds_generation")
    disable("flamegpu_connection")
    postprocObjects$FLAGmodelLoaded <- FALSE
    # clear the object
    for (i in names(canvasObjects)) {
      canvasObjects[[i]] <- canvasObjectsSTART[[i]]
    }

    # output$LoadingError_RDs <- renderText(
    isolate({
      if (is.null(input$RDsImport) || !file.exists(input$RDsImport$datapath) || !grepl(".RDs", input$RDsImport$datapath)) {
        shinyalert("Error", "Please select one RDs file.", "error")
        return()
      }

      mess <- readRDS(input$RDsImport$datapath)
      messNames <- names(mess)

      if (!all(messNames[-length(messNames)] %in% names(canvasObjectsSTART))) {
        shinyalert(
          "Error",
          paste(mess[["message"]], "\n The file must be RDs saved throught this application."),
          "error"
        )
        return()
      }

      textSucc <- UpdatingData(input, output, canvasObjects, mess, areasColor, session)
      shinyalert("Success", textSucc, "success", 1000)
      updateTabsetPanel(session, "SideTabs", selected = "canvas_tab")

      UpdatingData(input, output, canvasObjects, mess, areasColor, session)
      postprocObjects$FLAGmodelLoaded <- TRUE
      postprocObjects$DirPath <- NULL
      postprocObjects$Filter_evolutionCSV <- NULL
      postprocObjects$CONTACTcsv <- NULL
      postprocObjects$CONTACT_std <- NULL
      postprocObjects$CONTACTmatrix <- NULL
      postprocObjects$AEROSOL_std <- NULL
      postprocObjects$AEROSOLcsv <- NULL
      postprocObjects$COUNTERScsv <- NULL
      postprocObjects$A_C_COUNTERS <- NULL
      postprocObjects$Mapping <- NULL
      postprocObjects$MappingID_room <- FALSE
      postprocObjects$Model <- NULL
    })
    # )
    removeModal()
  })

  ### AGENTS definition ####
  observeEvent(input$id_new_agent, {
    disable("rds_generation")
    disable("flamegpu_connection")
    Agent <- input$id_new_agent

    if (Agent != "") {
      if (tolower(Agent) %in% tolower(names(canvasObjects$agents))) {
        Agent <- names(canvasObjects$agents)[which(tolower(Agent) == tolower(names(canvasObjects$agents)))]
        updateSelectizeInput(
          inputId = "id_new_agent",
          selected = Agent,
          choices = unique(names(canvasObjects$agents))
        )
      }

      if (Agent %in% canvasObjects$types$Name) {
        shinyalert("Error", "You can not define an agent using the same name assigned to a room type.", type = "error")
        return()
      }

      if (!grepl("^[a-zA-Z0-9_]+$", Agent)) {
        shinyalert("Error", "Agent name cannot contain special charachters.", type = "error")
        updateSelectizeInput(
          inputId = "id_new_agent",
          selected = "",
          choices = c("", names(canvasObjects$agents))
        )
        return()
      }

      if (stringr::str_to_lower(Agent) %in% c("global", "random")) {
        shinyalert("Error", "Agent name cannot be 'global' or 'random'.", type = "error")
        updateSelectizeInput(
          inputId = "id_new_agent",
          selected = "",
          choices = c("", names(canvasObjects$agents))
        )
        return()
      }
      new_agent <- list(
        DeterFlow = data.frame(
          Name = character(0), Room = character(0), Time = numeric(0), Flow = numeric(0), Acticity = numeric(0),
          Label = character(0), FlowID = character(0), AgentLinked = character(0), AgentLinkedType = character(0)
        ),
        RandFlow = data.frame(
          Name = character(0), Room = character(0), Dist = character(0), Activity = numeric(0), ActivityLabel = character(0), Time = numeric(0),
          Weight = numeric(0), TimeSlot = character(0), AgentLinked = character(0), AgentLinkedType = character(0)
        ),
        EntryExitTime = NULL,
        NumAgent = "1"
      )


      if (is.null(canvasObjects$agents)) {
        canvasObjects$agents[[1]] <- new_agent
        names(canvasObjects$agents) <- Agent
        canvasObjects$agents[[Agent]]$entry_type <- "Time window"
      } else if (!Agent %in% names(canvasObjects$agents)) {
        canvasObjects$agents[[Agent]] <- new_agent
        canvasObjects$agents[[Agent]]$entry_type <- "Time window"
      }

      if (length(names(canvasObjects$agents)) > 1) {
        agents <- names(canvasObjects$agents)[which(names(canvasObjects$agents) != Agent)]

        updateSelectizeInput(
          session = session, inputId = "id_agents_to_copy",
          choices = agents, selected = ""
        )
      }

      ## update table of entrance time ##
      # first remove all tabs
      updateRadioButtons(session, inputId = "ckbox_entranceFlow", selected = canvasObjects$agents[[Agent]]$entry_type)
      UpdatingTimeSlots_tabs(input, output, canvasObjects, InfoApp, session, canvasObjects$agents[[Agent]]$entry_type)

      ## Updating the flows tabs ##
      # first we have to remove all the tabs
      if (length(InfoApp$tabs_ids) > 0) {
        for (i in InfoApp$tabs_ids) {
          removeTab(inputId = "DetFlow_tabs", target = i)
        }
        InfoApp$tabs_ids <- c()
      }

      InfoApp$NumTabsFlow <- 0

      # order the remaining flow of the agent and show in the correct order
      FlowTabs <- canvasObjects$agents[[Agent]]$DeterFlow$FlowID
      if (length(FlowTabs) > 0) {
        for (NumFlow in order(unique(FlowTabs))) {
          InfoApp$NumTabsFlow <- InfoApp$NumTabsFlow + 1
          appendTab(
            inputId = "DetFlow_tabs",
            tabPanel(
              paste0(substring(unique(FlowTabs)[NumFlow], 1, 1), " flow"),
              uiOutput(paste0("UIDetFlows", Agent, "_", substring(unique(FlowTabs)[NumFlow], 1, 1), " flow"))
            )
          )
          InfoApp$tabs_ids <- append(InfoApp$tabs_ids, unique(FlowTabs)[NumFlow])
        }
        showTab(inputId = "DetFlow_tabs", target = FlowTabs[order(FlowTabs)[1]])
      } else {
        appendTab(
          inputId = "DetFlow_tabs",
          tabPanel(
            paste0(1, " flow"),
            uiOutput(paste0("UIDetFlows", Agent, "_", 1, " flow"))
          )
        )
        InfoApp$tabs_ids <- append(InfoApp$tabs_ids, "1 flow")
        rank_list_drag <- rank_list(
          text = "Drag the rooms in the desired order",
          labels = NULL,
          input_id = paste("list_detflow", Agent, paste0(1, " flow"), sep = "_")
        )
        output[[paste0("UIDetFlows", Agent, "_", 1, " flow")]] <- renderUI({
          rank_list_drag
        })

        showTab(inputId = "DetFlow_tabs", target = "1 flow")
        InfoApp$NumTabsFlow <- 1
      }

      if (is.null(canvasObjects$agents[[Agent]]$EntryExitTime)) {
        InfoApp$NumTabsTimeSlot <- 1
        InfoApp$NumTabsTimeShift <- list("shift_1" = 1)
      } else {
        shifts <- as.numeric(sort(gsub(" shift", "", unique(canvasObjects$agents[[Agent]]$EntryExitTime$Shift))))

        for (shift in shifts) {
          InfoApp$NumTabsTimeShift[[paste0("shift_", shift)]] <- as.numeric(sort(gsub(" slot", "", unique((canvasObjects$agents[[Agent]]$EntryExitTime %>% filter(Shift == paste0(shift, " shift")))$Name))))
        }
      }


      ### END updating

      if (nrow(canvasObjects$agents[[Agent]]$RandFlow) > 0) {
        shinyjs::show(id = "rand_description")
      }

      InfoApp$oldAgentType <- canvasObjects$agents[[Agent]]$entry_type
    }
  })

  observeEvent(input$button_rm_agent, {
    disable("rds_generation")
    disable("flamegpu_connection")

    Agent <- input$id_new_agent
    if (Agent != "" && Agent %in% names(canvasObjects$agents)) {
      if (InfoApp$NumTabsFlow > 0) {
        flows <- unique(canvasObjects$agents[[Agent]]$DeterFlow$FlowID)
        for (i in flows) {
          removeTab(inputId = "DetFlow_tabs", target = i)
        }
      }
      InfoApp$NumTabsFlow <- 0

      canvasObjects$agents[[Agent]]$EntryExitTime <- NULL
      UpdatingTimeSlots_tabs(input, output, canvasObjects, InfoApp, session, canvasObjects$agents[[Agent]]$entry_type)

      output$RandomEvents_table <- DT::renderDataTable(
        DT::datatable(
          data.frame(
            Name = character(0), Room = character(0), Dist = character(0), Activity = numeric(0), ActivityLabel = character(0), Time = numeric(0),
            Weight = numeric(0), TimeSlot = character(0), AgentLinked = character(0), AgentLinkedType = character(0)
          ) %>% select(-c(Name, Activity)),
          options = list(
            columnDefs = list(
              list(className = "dt-left", targets = 0),
              list(className = "dt-left", targets = 1),
              list(className = "dt-left", targets = 2),
              list(className = "dt-left", targets = 3),
              list(className = "dt-left", targets = 4),
              list(className = "dt-left", targets = 5),
              list(className = "dt-left", targets = 6),
              list(className = "dt-left", targets = 7)
            ),
            pageLength = 5
          ),
          selection = "single",
          rownames = F,
          colnames = c("Room", "Distribution", "Activity", "Time", "Weight", "Time Slot", "Agent Linked", "Agent Linked Type")
        )
      )

      canvasObjects$agents <- canvasObjects$agents[-which(names(canvasObjects$agents) == Agent)]
      canvasObjects$agents_whatif <- canvasObjects$agents_whatif %>%
        filter(Type != Agent)

      if (length(names(canvasObjects$agents)) == 0) {
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

        updateSelectizeInput(
          session = session, "agent_mask",
          choices = ""
        )

        updateSelectizeInput(
          session = session, "agent_vaccination",
          choices = ""
        )

        updateSelectizeInput(
          session = session, "agent_swab",
          choices = ""
        )

        updateSelectizeInput(
          session = session, "agent_quarantine",
          choices = ""
        )

        updateSelectizeInput(
          session = session, "agent_external_screening",
          choices = ""
        )

        updateSelectizeInput(
          session = session, "agent_initial_infected",
          choices = ""
        )
      } else {
        updateSelectizeInput(session, inputId = "id_new_agent", choices = names(canvasObjects$agents), selected = "")

        updateSelectizeInput(
          session = session, "agent_mask",
          choices = c("", names(canvasObjects$agents))
        )

        updateSelectizeInput(
          session = session, "agent_vaccination",
          choices = c("", names(canvasObjects$agents))
        )

        updateSelectizeInput(
          session = session, "agent_swab",
          choices = c("", names(canvasObjects$agents))
        )

        updateSelectizeInput(
          session = session, "agent_quarantine",
          choices = c("", names(canvasObjects$agents))
        )

        updateSelectizeInput(
          session = session, "agent_external_screening",
          choices = c("", names(canvasObjects$agents))
        )

        updateSelectizeInput(
          session = session, "agent_initial_infected",
          choices = c("", names(canvasObjects$agents))
        )
      }

      for (i in 1:length(canvasObjects$resources)) {
        canvasObjects$resources[[i]]$roomResource <- canvasObjects$resources[[i]]$roomResource[, which(!names(canvasObjects$resources[[i]]$roomResource) == Agent)]
        canvasObjects$resources[[i]]$waitingRoomsRand[which(!canvasObjects$resources[[i]]$waitingRoomsRand$Agent == Agent), ]
        canvasObjects$resources[[i]]$waitingRoomsDeter[which(!canvasObjects$resources[[i]]$waitingRoomsDeter$Agent == Agent), ]
      }

      if (nrow(canvasObjects$agents_whatif) > 0) {
        canvasObjects$agents_whatif <- canvasObjects$agents_whatif %>% filter(Type != Agent)
      }

      if (nrow(canvasObjects$initial_infected) > 0) {
        canvasObjects$initial_infected <- canvasObjects$initial_infected %>% filter(Type != Agent)
      }
    }
  })

  # input_num_agent <- debounce(reactive({input$num_agent}), 1000L)
  #
  # observeEvent(input_num_agent(),{
  #   disable("rds_generation")
  #   disable("flamegpu_connection")
  #   NumAgent = gsub(" ", "", input$num_agent)
  #
  #   if(input$id_new_agent != ""){
  #     if(NumAgent == "" || !grepl("(^[0-9]+).*", NumAgent) || is.na(as.integer(NumAgent)) || as.integer(NumAgent) < 0){
  #       shinyalert("Error", "You must insert a positive integer value.", type = "error")
  #       return()
  #     }
  #
  #     if(!is.null(canvasObjects$agents)){
  #       canvasObjects$agents[[input$id_new_agent]]$NumAgent = NumAgent
  #     }
  #   }
  # })

  observeEvent(input$button_copy_agent, {
    disable("rds_generation")
    disable("flamegpu_connection")
    if (input$id_agents_to_copy == "") {
      shinyalert("Error", "You must select an agent to copy.", type = "error")
      return()
    }

    Agent <- input$id_new_agent
    canvasObjects$agents[[Agent]] <- canvasObjects$agents[[input$id_agents_to_copy]]
    if (nrow(canvasObjects$agents[[Agent]]$DeterFlow) > 0) {
      canvasObjects$agents[[Agent]]$DeterFlow$Name <- Agent
    }
    if (nrow(canvasObjects$agents[[Agent]]$RandFlow) > 0) {
      canvasObjects$agents[[Agent]]$RandFlow$Name <- Agent
    }

    new_agent_whatif <- canvasObjects$agents_whatif %>%
      filter(Type == input$id_agents_to_copy)

    if (nrow(new_agent_whatif) > 0) {
      new_agent_whatif$Type <- Agent

      canvasObjects$agents_whatif <- canvasObjects$agents_whatif %>%
        filter(Type != Agent)
      canvasObjects$agents_whatif <- rbind(canvasObjects$agents_whatif, new_agent_whatif)
    }

    ##### updating all the agents tabs
    ## update table of entrance time ##
    # first remove all tabs

    UpdatingTimeSlots_tabs(input, output, canvasObjects, InfoApp, session, canvasObjects$agents[[Agent]]$entry_type)

    ## Updating the flows tabs ##
    # first we have to remove all the tabs (keeping the first one)
    if (length(InfoApp$tabs_ids) > 0) {
      for (i in InfoApp$tabs_ids) removeTab(inputId = "DetFlow_tabs", target = i)
      InfoApp$tabs_ids <- c()
    }
    InfoApp$NumTabsFlow <- 0

    FlowTabs <- canvasObjects$agents[[Agent]]$DeterFlow$FlowID
    if (length(FlowTabs) > 0) {
      for (NumFlow in 1:length(unique(FlowTabs))) {
        InfoApp$NumTabsFlow <- InfoApp$NumTabsFlow + 1
        appendTab(
          inputId = "DetFlow_tabs",
          tabPanel(
            paste0(NumFlow, " flow"),
            uiOutput(paste0("UIDetFlows", Agent, "_", NumFlow, " flow"))
          )
        )
        InfoApp$tabs_ids <- append(InfoApp$tabs_ids, unique(FlowTabs)[NumFlow])
      }
      showTab(inputId = "DetFlow_tabs", target = FlowTabs[order(FlowTabs)[1]])
    } else {
      appendTab(
        inputId = "DetFlow_tabs",
        tabPanel(
          paste0(1, " flow"),
          uiOutput(paste0("UIDetFlows", Agent, "_", 1, " flow"))
        )
      )
      InfoApp$tabs_ids <- append(InfoApp$tabs_ids, "1 flow")
      rank_list_drag <- rank_list(
        text = "Drag the rooms in the desired order",
        labels = NULL,
        input_id = paste("list_detflow", Agent, paste0(1, " flow"), sep = "_")
      )
      output[[paste0("UIDetFlows", Agent, "_", 1, " flow")]] <- renderUI({
        rank_list_drag
      })

      showTab(inputId = "DetFlow_tabs", target = "1 flow")
      InfoApp$NumTabsFlow <- 1
    }
    InfoApp$oldAgentType <- canvasObjects$agents[[Agent]]$entry_type

    ### END updating
  })

  #### Determined flow ####
  observeEvent(input$add_room_to_det_flow, {
    disable("rds_generation")
    disable("flamegpu_connection")
    if (!is.null(canvasObjects$agents)) {
      name <- input$id_new_agent
      new_room <- input$Det_select_room_flow

      if (new_room == "") {
        shinyalert("Error", "Please, select a room for the determined flow.", "error")
        return()
      }

      det_flow <- check_distribution_parameters(input, "det_flow")
      new_dist <- det_flow[[1]]
      new_time <- det_flow[[2]]

      if (is.null(new_dist) || is.null(new_time)) {
        shinyalert("Error", "Please, specify a time for the determined flow.", "error")
        return()
      }

      activity <- switch(input$DetActivity,
        "Very Light - e.g. resting" = 1,
        "Light - e.g. speak while resting" = 1.7777,
        "Quite Hard - e.g. speak/walk while standing" = 2.5556,
        "Hard - e.g. loudly speaking" = 6.1111
      )
      activityLabel <- switch(input$DetActivity,
        "Very Light - e.g. resting" = "Very Light",
        "Light - e.g. speak while resting" = "Light",
        "Quite Hard - e.g. speak/walk while standing" = "Quite Hard",
        "Hard - e.g. loudly speaking" = "Hard"
      )
      FlowID <- input$DetFlow_tabs

      if (is.null(FlowID)) {
        shinyalert("Error", "You must select a flow.", type = "error")
        return()
      }

      if (input$DetActivity == "") {
        shinyalert("Error", "You must specify an activity.", type = "error")
        return()
      }

      agentlinked <- ifelse(input$agentLink_det_flow == "", "None", input$agentLink_det_flow)
      agentlinkedtype <- ifelse(input$agentLink_det_flow == "", "None", input$ckbox_agentLink_det_flow)

      if (new_room != "" && new_time != "") {
        agentsOLD <- canvasObjects$agents[[name]]$DeterFlow
        agentsOLD_filter <- agentsOLD[agentsOLD$FlowID == FlowID, ]
        agent <- data.frame(
          Name = name,
          Room = new_room,
          Dist = new_dist,
          Time = new_time,
          Flow = length(agentsOLD_filter[, "Flow"]) + 1,
          Activity = activity,
          Label = paste0(new_room, " - ", new_dist, " ", new_time, " min", " - ", activityLabel, " - ", agentlinked),
          FlowID = FlowID,
          AgentLinked = agentlinked,
          AgentLinkedType = agentlinkedtype
        )

        if (agent$Label %in% agentsOLD_filter[, "Label"]) {
          agent$Label <- paste0("(", length(grep(x = agentsOLD_filter[, "Label"], pattern = agent$Label)) + 1, ") ", agent$Label)
        }
        canvasObjects$agents[[name]]$DeterFlow <- rbind(agentsOLD, agent)
      }
    }
  })

  # updating the list of rooms in determined flow
  observe({
    req(input$id_new_agent) -> agentID
    req(input$DetFlow_tabs) -> IDDetFlow_tabs
    req(canvasObjects$agents[[input$id_new_agent]]$DeterFlow) -> DeterFlow

    if (!grepl("^[a-zA-Z0-9_]+$", agentID)) {
      return()
    }

    isolate({
      if (!is.null(canvasObjects$agents) && agentID != "" && !is.null(DeterFlow) &&
        nrow(DeterFlow) >= 0 && !is.null(IDDetFlow_tabs)) {
        agent <- canvasObjects$agents[[agentID]]$DeterFlow %>% filter(FlowID == IDDetFlow_tabs)

        if (length(agent$Room) != 0) {
          rank_list_drag <- rank_list(
            text = "Drag the rooms in the desired order",
            labels = agent$Label[agent$Flow],
            input_id = paste("list_detflow", agentID, IDDetFlow_tabs, sep = "_")
          )
        } else {
          rank_list_drag <- rank_list(
            text = "Drag the rooms in the desired order",
            labels = NULL,
            input_id = paste("list_detflow", agentID, IDDetFlow_tabs, sep = "_")
          )
        }

        output[[paste0("UIDetFlows", agentID, "_", input$DetFlow_tabs)]] <- renderUI({
          rank_list_drag
        })
      }
    })
  })

  observeEvent(input$add_det_flow, {
    disable("rds_generation")
    disable("flamegpu_connection")
    input$id_new_agent -> agentID


    if (!is.null(canvasObjects$agents) && agentID != "") {
      # if the agent has already det flow the new flow will be greatest flow + 1
      if (nrow(canvasObjects$agents[[agentID]]$DeterFlow) > 0 && !is.null(canvasObjects$agents[[agentID]]$DeterFlow)) {
        FlowTabs <- canvasObjects$agents[[agentID]]$DeterFlow$FlowID
        NumFlow <- as.numeric(substring(FlowTabs[order(FlowTabs, decreasing = TRUE)[1]], 1, 1))
      }
      # else just add one on the tab number
      else {
        NumFlow <- InfoApp$NumTabsFlow
      }

      InfoApp$tabs_ids <- append(InfoApp$tabs_ids, paste0(NumFlow + 1, " flow"))

      if (NumFlow > 0) {
        NumFlow <- NumFlow + 1
        appendTab(
          inputId = "DetFlow_tabs",
          tabPanel(
            paste0(NumFlow, " flow"),
            uiOutput(paste0("UIDetFlows", agentID, "_", NumFlow, " flow"))
          )
        )

        rank_list_drag <- rank_list(
          text = "Drag the rooms in the desired order",
          labels = NULL,
          input_id = paste("list_detflow", agentID, paste0(NumFlow, " flow"), sep = "_")
        )

        output[[paste0("UIDetFlows", agentID, "_", NumFlow, " flow")]] <- renderUI({
          rank_list_drag
        })

        showTab(inputId = "DetFlow_tabs", target = paste0(NumFlow, " flow"))
        InfoApp$NumTabsFlow <- InfoApp$NumTabsFlow + 1

        selectToUpdate <- grep(pattern = "Select_TimeDetFlow_", x = names(input), value = T)
        for (i in selectToUpdate) {
          selected <- input[[i]]
          updateSelectInput(session = session, inputId = i, selected = selected, choices = InfoApp$tabs_ids)
        }
      }
    }
  })

  observeEvent(input$rm_det_flow, {
    disable("rds_generation")
    disable("flamegpu_connection")
    if (InfoApp$NumTabsFlow >= 1) {
      if (InfoApp$NumTabsFlow > 1) {
        removeTab(inputId = "DetFlow_tabs", target = input$DetFlow_tabs, session = session)
        InfoApp$tabs_ids <- InfoApp$tabs_ids[!InfoApp$tabs_ids %in% c(input$DetFlow_tabs)]
      }

      flowrm <- gsub(pattern = " flow", replacement = "", x = input$DetFlow_tabs)
      InfoApp$NumTabsFlow <- InfoApp$NumTabsFlow - 1

      Agent <- input$id_new_agent
      if (Agent != "") {
        AgentInfo <- canvasObjects$agents[[Agent]]

        AgentInfo$DeterFlow <- AgentInfo$DeterFlow[which(!AgentInfo$DeterFlow$FlowID == paste0(flowrm, " flow")), ]
        AgentInfo$EntryExitTime <- AgentInfo$EntryExitTime[which(!AgentInfo$EntryExitTime$FlowID == paste0(flowrm, " flow")), ]

        canvasObjects$agents[[Agent]] <- AgentInfo

        selectToUpdate <- grep(pattern = "Select_TimeDetFlow_", x = names(input), value = T)
        UpdatingTimeSlots_tabs(input, output, canvasObjects, InfoApp, session, canvasObjects$agents[[Agent]]$entry_type)
        for (i in selectToUpdate) updateSelectInput(session = session, inputId = i, choices = InfoApp$tabs_ids)
      }
    }
  })

  observeEvent(input$remove_room_to_det_flow, {
    disable("rds_generation")
    disable("flamegpu_connection")
    if (!is.null(canvasObjects$agents) && input$id_new_agent != "") {
      agent <- canvasObjects$agents[[input$id_new_agent]]$DeterFlow
      input[[paste("list_detflow", input$id_new_agent, input$DetFlow_tabs, sep = "_")]] -> list_detflow
      if (length(list_detflow) > 0 &&
        length(agent$Room) > 0) {
        # find the last room in the selected flow id
        max(which(canvasObjects$agents[[input$id_new_agent]]$DeterFlow$FlowID == input$DetFlow_tabs)) -> nrow
        if (nrow > 1) {
          canvasObjects$agents[[input$id_new_agent]]$DeterFlow <- canvasObjects$agents[[input$id_new_agent]]$DeterFlow[-nrow, ]
        } else {
          canvasObjects$agents[[input$id_new_agent]]$DeterFlow <- data.frame(
            Name = character(0), Room = character(0), Time = numeric(0), Flow = numeric(0), Activity = numeric(0),
            Label = character(0), FlowID = character(0), AgentLinked = character(0), AgentLinkedType = character(0)
          )
        }
      }
    }
  })

  observe({
    namesDetFlows <- paste("list_detflow", input$id_new_agent, input$DetFlow_tabs, sep = "_")
    req(input[[namesDetFlows]]) -> list_detflow

    if (!grepl("^[a-zA-Z0-9_]+$", input$id_new_agent)) {
      return()
    }

    if (!is.null(canvasObjects$agents) && input$id_new_agent != "" && !is.null(canvasObjects$agents[[input$id_new_agent]]$DeterFlow) && nrow(canvasObjects$agents[[input$id_new_agent]]$DeterFlow) >= 0 && !is.null(input$DetFlow_tabs)) {
      isolate({
        agent <- canvasObjects$agents[[input$id_new_agent]]$DeterFlow %>% filter(FlowID == input$DetFlow_tabs)
        DeterFlow_tmp <- canvasObjects$agents[[input$id_new_agent]]$DeterFlow %>% filter(FlowID != input$DetFlow_tabs)

        if (!is.null(list_detflow) &&
          length(agent$Room) > 0 &&
          length(list_detflow) == length(agent$Label)) {
          newOrder <- data.frame(
            Name = input$id_new_agent,
            Label = list_detflow,
            Flow = 1:length(list_detflow)
          )
          DeterFlow <- merge(agent %>% select(-Flow), newOrder, by = c("Name", "Label")) %>%
            select(Name, Room, Dist, Time, Flow, Activity, Label, FlowID, AgentLinked, AgentLinkedType) %>%
            arrange(Flow)
          canvasObjects$agents[[input$id_new_agent]]$DeterFlow <- rbind(DeterFlow_tmp, DeterFlow)
        }
      })
    }
  })

  #### Random flow ####

  observeEvent(input$add_room_to_rand_flow, {
    disable("rds_generation")
    disable("flamegpu_connection")
    name <- input$id_new_agent
    agent <- canvasObjects$agents[[name]]$RandFlow

    if (input$Rand_select_room_flow == "") {
      shinyalert("Error", "Please, select a room for the random flow.", "error")
      return()
    }

    EntryTime <- input[["EntryTimeRate_rand_flow"]]
    ExitTime <- input[["ExitTimeRate_rand_flow"]]

    activity <- switch(input$RandActivity,
      "Very Light - e.g. resting" = 1,
      "Light - e.g. speak while resting" = 1.7777,
      "Quite Hard - e.g. speak/walk while standing" = 2.5556,
      "Hard - e.g. loudly speaking" = 6.1111
    )
    activityLabel <- switch(input$RandActivity,
      "Very Light - e.g. resting" = "Very Light",
      "Light - e.g. speak while resting" = "Light",
      "Quite Hard - e.g. speak/walk while standing" = "Quite Hard",
      "Hard - e.g. loudly speaking" = "Hard"
    )

    if (is.null(canvasObjects$agents[[name]])) {
      shinyalert("Error", "You should define an agent.", type = "error")
      return()
    }

    if (input$RandActivity == "") {
      shinyalert("Error", "You must specify an activity.", type = "error")
      return()
    }

    if (input$RandWeight == "" ||
      (as.double(as.numeric(gsub(",", "\\.", input$RandWeight))) <= 0 ||
        as.double((as.numeric(gsub(",", "\\.", input$RandWeight)))) >= 1)) {
      shinyalert("Error", "You must specify a weight between 0 and 1.", type = "error")
      return()
    }

    rand_flow <- check_distribution_parameters(input, "rand_flow")
    new_dist <- rand_flow[[1]]
    new_time <- rand_flow[[2]]

    if (is.null(new_dist) || is.null(new_time)) {
      shinyalert("Error", "Please, specify a time for the random flow.", "error")
      return()
    }

    listTimes <- canvasObjects$agents[[name]]$RandFlow %>%
      filter(
        Name == name,
        Room == input$Rand_select_room_flow
      ) %>%
      pull(TimeSlot)

    if (length(listTimes) == 0) listTimes <- NULL
    times <- CheckEntryExit(EntryTime, ExitTime, listTimes)

    if (times[1] == "Error") {
      shinyalert("Error", times[2], type = "error")
      return()
    }

    agentlinked <- agentlinkedtype <- ifelse(input$agentLink_rand_flow == "", "None", input$agentLink_rand_flow)
    agentlinkedtype <- ifelse(input$agentLink_rand_flow == "", "None", input$ckbox_agentLink_rand_flow)

    if (input$Rand_select_room_flow != "") {
      newOrder <- data.frame(
        Name = name,
        Room = input$Rand_select_room_flow,
        Dist = new_dist,
        Time = new_time,
        Activity = activity,
        ActivityLabel = activityLabel,
        Weight = gsub(",", "\\.", as.numeric(input$RandWeight)),
        TimeSlot = times[1],
        AgentLinked = agentlinked,
        AgentLinkedType = agentlinkedtype
      )
      canvasObjects$agents[[name]]$RandFlow <- rbind(canvasObjects$agents[[name]]$RandFlow, newOrder)
    }

    output$RandomEvents_table <- DT::renderDataTable(
      DT::datatable(canvasObjects$agents[[name]]$RandFlow %>% select(-c(Name, Activity)),
        options = list(
          columnDefs = list(
            list(className = "dt-left", targets = 0),
            list(className = "dt-left", targets = 1),
            list(className = "dt-left", targets = 2),
            list(className = "dt-left", targets = 3),
            list(className = "dt-left", targets = 4),
            list(className = "dt-left", targets = 5),
            list(className = "dt-left", targets = 6),
            list(className = "dt-left", targets = 7)
          ),
          pageLength = 5
        ),
        selection = "single",
        rownames = F,
        colnames = c("Room", "Distribution", "Activity", "Time", "Weight", "Time Slot", "Agent Linked", "Agent Linked Type")
      )
    )

    shinyjs::show(id = "rand_description")
  })

  # aggiorna la visualizzazione di RandomEvents_table quando cambia l'agent
  observe({
    if (!is.null(canvasObjects$agents) && input$id_new_agent != "") {
      agent <- canvasObjects$agents[[input$id_new_agent]]$RandFlow
      if (length(agent$Room) != 0) {
        output$RandomEvents_table <- DT::renderDataTable(
          DT::datatable(agent %>% select(-c(Name, Activity)),
            options = list(
              columnDefs = list(
                list(className = "dt-left", targets = 0),
                list(className = "dt-left", targets = 1),
                list(className = "dt-left", targets = 2),
                list(className = "dt-left", targets = 3),
                list(className = "dt-left", targets = 4),
                list(className = "dt-left", targets = 5),
                list(className = "dt-left", targets = 6),
                list(className = "dt-left", targets = 7)
              ),
              pageLength = 5
            ),
            selection = "single",
            rownames = F,
            colnames = c("Room", "Distribution", "Activity", "Time", "Weight", "Time Slot", "Agent Linked", "Agent Linked Type")
          )
        )
      } else {
        output$RandomEvents_table <- DT::renderDataTable(
          DT::datatable(
            data.frame(
              Name = character(0), Room = character(0), Dist = character(0), Activity = numeric(0), ActivityLabel = character(0), Time = numeric(0),
              Weight = numeric(0), TimeSlot = character(0), AgentLinked = character(0), AgentLinkedType = character(0)
            ) %>% select(-c(Name, Activity)),
            options = list(
              columnDefs = list(
                list(className = "dt-left", targets = 0),
                list(className = "dt-left", targets = 1),
                list(className = "dt-left", targets = 2),
                list(className = "dt-left", targets = 3),
                list(className = "dt-left", targets = 4),
                list(className = "dt-left", targets = 5),
                list(className = "dt-left", targets = 6),
                list(className = "dt-left", targets = 7)
              ),
              pageLength = 5
            ),
            selection = "single",
            rownames = F,
            colnames = c("Room", "Distribution", "Activity", "Time", "Weight", "Time Slot", "Agent Linked", "Agent Linked Type")
          )
        )
      }
    }
  })

  observeEvent(input$RandomEvents_table_cell_clicked, {
    info <- input$RandomEvents_table_cell_clicked
    req(input$id_new_agent != "")

    if (!is.null(info$row)) {
      shinyalert(
        title = "Delete Entry?",
        text = "Are you sure you want to delete this row?",
        type = "warning",
        showCancelButton = TRUE,
        confirmButtonText = "Yes, delete it!",
        callbackR = function(x) {
          if (x) {
            if (nrow(canvasObjects$agents[[input$id_new_agent]]$RandFlow) == 1) {
              canvasObjects$agents[[input$id_new_agent]]$RandFlow <- data.frame(
                Name = character(0), Room = character(0), Dist = character(0), Activity = numeric(0), ActivityLabel = character(0), Time = numeric(0),
                Weight = numeric(0), TimeSlot = character(0), AgentLinked = character(0), AgentLinkedType = character(0)
              )

              output$RandomEvents_table <- DT::renderDataTable(
                DT::datatable(
                  data.frame(
                    Name = character(0), Room = character(0), Dist = character(0), Activity = numeric(0), ActivityLabel = character(0), Time = numeric(0),
                    Weight = numeric(0), TimeSlot = character(0), AgentLinked = character(0), AgentLinkedType = character(0)
                  ) %>% select(-c(Name, Activity)),
                  options = list(
                    columnDefs = list(
                      list(className = "dt-left", targets = 0),
                      list(className = "dt-left", targets = 1),
                      list(className = "dt-left", targets = 2),
                      list(className = "dt-left", targets = 3),
                      list(className = "dt-left", targets = 4),
                      list(className = "dt-left", targets = 5),
                      list(className = "dt-left", targets = 6),
                      list(className = "dt-left", targets = 7)
                    ),
                    pageLength = 5
                  ),
                  selection = "single",
                  rownames = F,
                  colnames = c("Room", "Distribution", "Activity", "Time", "Weight", "Time Slot", "Agent Linked", "Agent Linked Type")
                )
              )
            } else {
              canvasObjects$agents[[input$id_new_agent]]$RandFlow <- canvasObjects$agents[[input$id_new_agent]]$RandFlow[-info$row, ]
            }
          }
        }
      )
    }
  })

  #### entry/exit flow ####

  observeEvent(input$ckbox_entranceFlow, {
    disable("rds_generation")
    disable("flamegpu_connection")

    if (!is.null(canvasObjects$agents) && is.null(canvasObjects$agents[[input$id_new_agent]]$EntryExitTime) && InfoApp$NumTabsFlow == 1) {
      Agent <- input$id_new_agent

      InfoApp$oldAgentType <- canvasObjects$agents[[Agent]]$entry_type
      canvasObjects$agents[[Agent]]$entry_type <- input$ckbox_entranceFlow


      selectToUpdate <- grep(pattern = "Select_TimeDetFlow_", x = names(input), value = T)
      UpdatingTimeSlots_tabs(input, output, canvasObjects, InfoApp, session, canvasObjects$agents[[Agent]]$entry_type)
      for (i in selectToUpdate) updateSelectInput(session = session, inputId = i, choices = InfoApp$tabs_ids)

      return()
    }

    if (!canvasObjects$cancel_button_selected && input$id_new_agent != "" && input$ckbox_entranceFlow != canvasObjects$agents[[input$id_new_agent]]$entry_type) {
      showModal(modalDialog(
        title = "Important message",
        "Do you want to update all the agent's time slot information? Existing data will be overwritten, and if you select 'Daily Rate,' only the first flow will be retained.",
        easyClose = TRUE,
        footer = tagList(
          actionButton("confirmUpdates", "Update"),
          actionButton("cancelAction", "Cancel")
        )
      ))
    }

    if (canvasObjects$cancel_button_selected) {
      canvasObjects$cancel_button_selected <- FALSE
    }
  })

  observeEvent(input$cancelAction, {
    disable("rds_generation")
    disable("flamegpu_connection")
    canvasObjects$cancel_button_selected <- TRUE
    updateRadioButtons(session, inputId = "ckbox_entranceFlow", selected = canvasObjects$agents[[input$id_new_agent]]$entry_type)
    removeModal()
  })

  observeEvent(input$confirmUpdates, {
    disable("rds_generation")
    disable("flamegpu_connection")
    input$id_new_agent -> Agent
    InfoApp$oldAgentType <- canvasObjects$agents[[Agent]]$entry_type
    canvasObjects$agents[[Agent]]$entry_type <- input$ckbox_entranceFlow
    canvasObjects$agents[[Agent]]$EntryExitTime <- NULL

    FlowIDs <- 2:InfoApp$NumTabsFlow
    if (InfoApp$NumTabsFlow > 1) {
      removeTab(inputId = "DetFlow_tabs", target = paste0(FlowIDs, " flow"), session = session)
      InfoApp$tabs_ids <- InfoApp$tabs_ids[!InfoApp$tabs_ids %in% FlowIDs]

      InfoApp$NumTabsFlow <- 1
    }

    Agent <- input$id_new_agent
    if (Agent != "") {
      AgentInfo <- canvasObjects$agents[[Agent]]

      AgentInfo$DeterFlow <- AgentInfo$DeterFlow[which(!AgentInfo$DeterFlow$FlowID != "1 flow"), ]

      canvasObjects$agents[[Agent]] <- AgentInfo

      selectToUpdate <- grep(pattern = "Select_TimeDetFlow_", x = names(input), value = T)
      UpdatingTimeSlots_tabs(input, output, canvasObjects, InfoApp, session, canvasObjects$agents[[Agent]]$entry_type)
      for (i in selectToUpdate) updateSelectInput(session = session, inputId = i, choices = InfoApp$tabs_ids)
    }

    removeModal()
  })

  observeEvent(input$add_slot, {
    disable("rds_generation")
    disable("flamegpu_connection")

    showed_shift <- gsub("shift_", "", input$Shift_tabs)

    NumTabs <- first_missing_number(InfoApp$NumTabsTimeShift[[paste0("shift_", showed_shift)]])
    InfoApp$NumTabsTimeShift[[paste0("shift_", showed_shift)]] <- sort(c(InfoApp$NumTabsTimeShift[[paste0("shift_", showed_shift)]], NumTabs))
    appendTab(
      inputId = paste0("Time_tabs_", showed_shift),
      tabPanel(paste0(NumTabs, " slot"),
        value = paste0("slot_", showed_shift, "_", NumTabs),
        column(
          7,
          textInput(inputId = paste0("EntryTime_", showed_shift, "_", NumTabs), label = "Entry time:", placeholder = "hh:mm"),
          if (length(canvasObjects$agents[[input$id_new_agent]]$DeterFlow$FlowID) > 0) {
            selectInput(
              inputId = paste0("Select_TimeDetFlow_", showed_shift, "_", NumTabs),
              label = "Associate with a determined flow:",
              choices = sort(unique(canvasObjects$agents[[input$id_new_agent]]$DeterFlow$FlowID))
            )
          } else {
            selectInput(
              inputId = paste0("Select_TimeDetFlow_", showed_shift, "_", NumTabs),
              label = "Associate with a determined flow:",
              choices = "1 flow"
            )
          }
        ),
        column(
          5,
          checkboxGroupInput(paste0("selectedDays_", showed_shift, "_", NumTabs), "Select Days of the Week",
            choices = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
            selected = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
          )
        )
      )
    )

    showTab(inputId = paste0("Time_tabs_", showed_shift), target = paste0("slot_", showed_shift, "_", NumTabs), select = T)
  })

  observeEvent(input$add_shift, {
    disable("rds_generation")
    disable("flamegpu_connection")

    shifts <- as.numeric(gsub("shift_", "", names(InfoApp$NumTabsTimeShift)))

    NumShifts <- min(setdiff(seq_len(max(shifts) + 1), shifts))
    appendTab(
      inputId = "Shift_tabs",
      tabPanel(paste0(NumShifts, " shift"),
        value = paste0("shift_", NumShifts),
        fluidRow(
          column(4,
            offset = 1,
            textInput(
              inputId = paste0("num_agent_", NumShifts), label = "Number of agents:",
              placeholder = "The number must be a positive integer"
            )
          )
        ),
        fluidRow(
          column(11,
            offset = 1,
            tabsetPanel(
              id = paste0("Time_tabs_", NumShifts),
              tabPanel("1 slot",
                value = paste0("slot_", NumShifts, "_1"),
                column(
                  7,
                  textInput(inputId = paste0("EntryTime_", NumShifts, "_1"), label = "Entry time:", placeholder = "hh:mm"),
                  if (length(canvasObjects$agents[[input$id_new_agent]]$DeterFlow$FlowID) > 0) {
                    selectInput(
                      inputId = paste0("Select_TimeDetFlow_", NumShifts, "_1"),
                      label = "Associate with a determined flow:",
                      choices = sort(unique(canvasObjects$agents[[input$id_new_agent]]$DeterFlow$FlowID))
                    )
                  } else {
                    selectInput(
                      inputId = paste0("Select_TimeDetFlow_", NumShifts, "_1"),
                      label = "Associate with a determined flow:",
                      choices = "1 flow"
                    )
                  }
                ),
                column(
                  5,
                  checkboxGroupInput(paste0("selectedDays_", NumShifts, "_1"), "Select Days of the Week",
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

    showTab(inputId = "Shift_tabs", target = paste0("shift_", NumShifts), select = T)
    InfoApp$NumTabsTimeShift[[paste0("shift_", NumShifts)]] <- 1
  })

  observeEvent(input$add_slot_rate, {
    disable("rds_generation")
    disable("flamegpu_connection")

    NumTabs <- first_missing_number(InfoApp$NumTabsTimeSlot)
    InfoApp$NumTabsTimeSlot <- sort(c(InfoApp$NumTabsTimeSlot, NumTabs))
    appendTab(
      inputId = "Rate_tabs",
      tabPanel(paste0(NumTabs, " slot"),
        value = paste0("slot_", NumTabs),
        tags$b("Entrance rate:"),
        get_distribution_panel(paste0("daily_rate_", NumTabs)),
        # textInput(inputId = paste0("EntranceRate_", NumTabs), label = "Entrance rate:", placeholder = "Daily entrance rate", value = ""),
        column(
          7,
          textInput(inputId = paste0("EntryTimeRate_", NumTabs), label = "Initial generation time:", placeholder = "hh:mm"),
          textInput(inputId = paste0("ExitTimeRate_", NumTabs), label = "Final generation time:", placeholder = "hh:mm"),
        ),
        column(
          5,
          checkboxGroupInput(paste0("selectedDaysRate_", NumTabs), "Select Days of the Week",
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

    showed_shift <- gsub("shift_", "", input$Shift_tabs)

    NumTabs <- as.numeric(InfoApp$NumTabsTimeShift[[paste0("shift_", showed_shift)]])
    if (length(NumTabs) > 1) {
      removeTab(inputId = paste0("Time_tabs_", showed_shift), target = input[[paste0("Time_tabs_", showed_shift)]], session = session)

      slotrm <- gsub(pattern = paste0("slot_", showed_shift, "_"), replacement = "", x = input[[paste0("Time_tabs_", showed_shift)]])
      InfoApp$NumTabsTimeShift[[paste0("shift_", showed_shift)]] <- InfoApp$NumTabsTimeShift[[paste0("shift_", showed_shift)]][which(InfoApp$NumTabsTimeShift[[paste0("shift_", showed_shift)]] != slotrm)]

      Agent <- input$id_new_agent
      if (Agent != "") {
        AgentInfo <- canvasObjects$agents[[Agent]]

        if (!is.null(AgentInfo$EntryExitTime)) {
          AgentInfo$EntryExitTime <- AgentInfo$EntryExitTime[which(!(AgentInfo$EntryExitTime$Name == paste0(slotrm, " slot") & AgentInfo$EntryExitTime$Shift == showed_shift)), ]
        }

        canvasObjects$agents[[Agent]] <- AgentInfo
      }
    }
  })

  observeEvent(input$rm_shift, {
    disable("rds_generation")
    disable("flamegpu_connection")

    NumShifts <- InfoApp$NumTabsTimeShift
    showed_shift <- gsub("shift_", "", input$Shift_tabs)
    if (length(NumShifts) > 1) {
      removeTab(inputId = "Shift_tabs", target = paste0("shift_", showed_shift), session = session)

      shiftrm <- gsub(pattern = "shift_", replacement = "", x = input$Shift_tabs)
      InfoApp$NumTabsTimeShift <- InfoApp$NumTabsTimeShift[which(InfoApp$NumTabsTimeShift != shiftrm)]
      InfoApp$NumTabsTimeShift[[paste0("shift_", showed_shift)]] <- NULL

      Agent <- input$id_new_agent
      if (Agent != "") {
        AgentInfo <- canvasObjects$agents[[Agent]]

        if (!is.null(AgentInfo$EntryExitTime)) {
          AgentInfo$EntryExitTime <- AgentInfo$EntryExitTime[which(!(AgentInfo$EntryExitTime$Shift == paste0(shiftrm, " shift"))), ]
        }

        canvasObjects$agents[[Agent]] <- AgentInfo
      }
    }
  })

  observeEvent(input$rm_slot_rate, {
    disable("rds_generation")
    disable("flamegpu_connection")
    if (length(InfoApp$NumTabsTimeSlot) > 1) {
      removeTab(inputId = "Rate_tabs", target = input$Rate_tabs, session = session)
      slotrm <- gsub(pattern = "slot_", replacement = "", x = input$Rate_tabs)
      InfoApp$NumTabsTimeSlot <- InfoApp$NumTabsTimeSlot[which(InfoApp$NumTabsTimeSlot != slotrm)]

      Agent <- input$id_new_agent
      if (Agent != "") {
        AgentInfo <- canvasObjects$agents[[Agent]]

        AgentInfo$EntryExitTime <- AgentInfo$EntryExitTime[which(!AgentInfo$EntryExitTime$Name == paste0(slotrm, " slot")), ]

        canvasObjects$agents[[Agent]] <- AgentInfo
      }
    }
  })


  observeEvent(input$set_timeslot, {
    disable("rds_generation")
    disable("flamegpu_connection")
    if (is.null(canvasObjects$agents)) {
      shinyalert("Error", "You should define an agent.", type = "error")
      return()
    }

    if (input$ckbox_entranceFlow == "Daily Rate") {
      indexes <- InfoApp$NumTabsTimeSlot

      for (index in indexes) {
        daily_rate <- check_distribution_parameters(input, paste0("daily_rate_", index))
        new_dist <- daily_rate[[1]]
        new_time <- daily_rate[[2]]

        if (is.null(new_dist) || is.null(new_time)) {
          shinyalert("Error", "Please, specify a time for the time slot.", "error")
          return()
        }


        EntryTimeRate <- input[[paste0("EntryTimeRate_", index)]]
        ExitTimeRate <- input[[paste0("ExitTimeRate_", index)]]
        if (!any(sapply(list(
          EntryTimeRate,
          ExitTimeRate,
          input[[paste0("selectedDaysRate_", index)]]
        ), is.null))) {
          if (EntryTimeRate == "" || ExitTimeRate == "") {
            shinyalert("Error", "You should define the Entry and the Exit time.", type = "error")
            return()
          }
          if (EntryTimeRate != "") {
            if (!(grepl("^([01]?[0-9]|2[0-3]):[0-5][0-9]$", EntryTimeRate) || grepl("^\\d{1,2}$", EntryTimeRate))) {
              shinyalert("Error", "The format of the time should be: hh:mm (e.g. 06:15, or 20).", type = "error")
              return()
            }
          }
          if (grepl("^\\d{1,2}$", EntryTimeRate)) {
            EntryTimeRate <- paste0(EntryTimeRate, ":00")
          }

          if (ExitTimeRate != "") {
            if (!(grepl("^([01]?[0-9]|2[0-3]):[0-5][0-9]$", ExitTimeRate) || grepl("^\\d{1,2}$", ExitTimeRate))) {
              shinyalert("Error", "The format of the time should be: hh:mm (e.g. 06:15, or 20:30).", type = "error")
              return()
            }
          }
          if (grepl("^\\d{1,2}$", ExitTimeRate)) {
            ExitTimeRate <- paste0(ExitTimeRate, ":00")
          }
          # check if the number before : in EntryTime is lower than number before : in ExitTime
          if (as.numeric(strsplit(input[[paste0("EntryTimeRate_", index)]], ":")[[1]][1]) > as.numeric(strsplit(input[[paste0("ExitTimeRate_", index)]], ":")[[1]][1])) {
            shinyalert("Error", "The Entry time should be lower than the Exit time.", type = "error")
            return()
          }
          if (as.numeric(strsplit(input[[paste0("EntryTimeRate_", index)]], ":")[[1]][1]) == as.numeric(strsplit(input[[paste0("ExitTimeRate_", index)]], ":")[[1]][1]) &&
            as.numeric(strsplit(input[[paste0("EntryTimeRate_", index)]], ":")[[1]][2]) > as.numeric(strsplit(input[[paste0("ExitTimeRate_", index)]], ":")[[1]][2])) {
            shinyalert("Error", "The Entry time should be lower than the Exit time.", type = "error")
            return()
          }
          # check if

          if (EntryTimeRate != "" && ExitTimeRate != "") {
            df <- data.frame(
              Name = paste0(index, " slot"),
              EntryTime = EntryTimeRate,
              ExitTime = ExitTimeRate,
              RateDist = new_dist,
              RateTime = new_time,
              Days = input[[paste0("selectedDaysRate_", index)]],
              Shift = "1 shift",
              NumAgent = 0
            )
          } else {
            df <- data.frame(
              Name = paste0(index, " slot"),
              EntryTime = NA,
              ExitTime = NA,
              RateDist = NA,
              RateTime = NA,
              Days = NA,
              Shift = NA,
              NumAgent = NA
            )
          }

          new_entry_time <- unique(df$EntryTime)
          new_exit_time <- unique(df$ExitTime)

          if (!is.null(canvasObjects$agents[[input$id_new_agent]]$EntryExitTime) && is.data.frame(canvasObjects$agents[[input$id_new_agent]]$EntryExitTime)) {
            canvasObjects$agents[[input$id_new_agent]]$EntryExitTime <- rbind(canvasObjects$agents[[input$id_new_agent]]$EntryExitTime %>% filter(Name != paste0(index, " slot")), df)
          } else {
            canvasObjects$agents[[input$id_new_agent]]$EntryExitTime <- df
          }

          canvasObjects$agents[[input$id_new_agent]]$EntryExitTime -> EntryExitTime
          # check if df$Name is present in EntryExitTime$Name
          if (!is.null(EntryExitTime) && is.data.frame(EntryExitTime)) {
            # check if df$Days is present in EntryExitTime$Days
            if (nrow(EntryExitTime %>% filter(Name != paste0(index, " slot")) %>% filter(Days %in% df$Days)) > 0) {
              # check if in the same day there is a time slot that collides with the new one
              if (nrow(EntryExitTime %>% filter(Name != paste0(index, " slot")) %>% filter(Days %in% df$Days) %>% filter(EntryTime < new_exit_time & ExitTime > new_entry_time)) > 0) {
                shinyalert("Error", "The time slot you are trying to add collides with another time slot.", type = "error")
                return()
              }
            }
          }
        }
      }
    } else {
      canvasObjects$agents[[input$id_new_agent]]$EntryExitTime <- NULL

      for (shift in names(InfoApp$NumTabsTimeShift)) {
        indexes <- InfoApp$NumTabsTimeShift[[shift]]
        num_shift <- as.numeric(gsub("shift_", "", shift))

        for (index in indexes) {
          EntryTime <- input[[paste0("EntryTime_", num_shift, "_", index)]]
          if (!any(sapply(list(
            EntryTime,
            input[[paste0("selectedDays_", num_shift, "_", index)]]
          ), is.null))) {
            if (EntryTime == "") {
              shinyalert("Error", "You should define the entry time.", type = "error")
              return()
            }
            if (EntryTime != "") {
              if (!(grepl("^([01]?[0-9]|2[0-3]):[0-5][0-9]$", EntryTime) || grepl("^\\d{1,2}$", EntryTime))) {
                shinyalert("Error", "The format of the time should be: hh:mm (e.g. 06:15, or 20).", type = "error")
                return()
              }
            }
            if (grepl("^\\d{1,2}$", EntryTime)) {
              EntryTime <- paste0(EntryTime, ":00")
            }

            if (is.na(input[[paste0("num_agent_", num_shift)]]) || !is.numeric(input[[paste0("num_agent_", num_shift)]]) ||
              as.numeric(input[[paste0("num_agent_", num_shift)]]) <= 0) {
              shinyalert("Error", "The number of agents must be a number > 0 for each shift.", type = "error")
              return()
            }

            if (EntryTime != "") {
              df <- data.frame(
                Shift = paste0(num_shift, " shift"),
                Name = paste0(index, " slot"),
                EntryTime = EntryTime,
                Days = input[[paste0("selectedDays_", num_shift, "_", index)]],
                FlowID = input[[paste0("Select_TimeDetFlow_", num_shift, "_", index)]],
                NumAgent = input[[paste0("num_agent_", num_shift)]]
              )
            } else {
              df <- data.frame(
                Shift = paste0(num_shift, " shift"),
                Name = paste0(index, " slot"),
                EntryTime = NA,
                Days = NA,
                FlowID = NA,
                NumAgent = input[[paste0("num_agent_", num_shift)]]
              )
            }


            if (!is.null(canvasObjects$agents[[input$id_new_agent]]$EntryExitTime) && is.data.frame(canvasObjects$agents[[input$id_new_agent]]$EntryExitTime)) {
              canvasObjects$agents[[input$id_new_agent]]$EntryExitTime <- rbind(canvasObjects$agents[[input$id_new_agent]]$EntryExitTime, df)
            } else {
              canvasObjects$agents[[input$id_new_agent]]$EntryExitTime <- df
            }
          }
        }
      }
    }

    print(canvasObjects$agents[[input$id_new_agent]]$EntryExitTime)
    removeModal()
  })

  #### Resources ####
  # Show resources and change value

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
    do.call(
      rbind,
      lapply(names(canvasObjects$agents), function(agent) {
        rooms <- unique(
          canvasObjects$agents[[agent]]$DeterFlow$Room,
          canvasObjects$agents[[agent]]$RandFlow$Room
        )
        if (length(rooms) > 0) {
          df_Rand <- canvasObjects$agents[[agent]]$RandFlow
          if (!is.null(df_Rand) && dim(df_Rand)[1] > 0) {
            rbind(
              data.frame(Agent = agent, Room = canvasObjects$agents[[agent]]$DeterFlow$Room, Flow = "Deter"),
              data.frame(Agent = agent, Room = df_Rand$Room, Flow = "Rand")
            )
          } else {
            data.frame(Agent = agent, Room = canvasObjects$agents[[agent]]$DeterFlow$Room, Flow = "Deter")
          }
        } else {
          NULL
        }
      })
    )
  })

  objectsINcanvas <- reactive({
    if (!is.null(canvasObjects$roomObjects)) {
      objs_list <- lapply(names(canvasObjects$roomObjects), function(room_name) {
        objs <- canvasObjects$roomObjects[[room_name]]
        if (length(objs) > 0) {
          do.call(rbind, lapply(objs, function(obj) {
            if (is.null(obj$isObstacle) || !obj$isObstacle) {
              data.frame(
                ID = obj$id,
                Name = obj$name,
                Room = room_name,
                Area = paste0(obj$name, " - ", room_name),
                Capacity = ifelse(is.null(obj$capacity), NA, obj$capacity),
                stringsAsFactors = FALSE
              )
            } else {
              NULL
            }
          }))
        } else {
          NULL
        }
      })
      do.call(rbind, objs_list)
    }
  })

  allResObjects <- reactive({
    objectsINcanvas()
  })

  selected_object_key <- reactive({
    unique(paste0(input$selectInput_object_resources_name, " - ", input$selectInput_object_resources_room))
  })

  output$selectInput_alternative_resources_global <- renderUI({
    # Generate selectizeInput for each relevant agent
    choicesRoom <- c("Same room", "Skip room")

    if (!is.null(canvasObjects$roomsINcanvas)) {
      rooms <- canvasObjects$roomsINcanvas %>%
        select(type, Name, area) %>%
        filter(!type %in% c("Spawnroom", "Fillingroom", "Stair")) %>%
        mutate(NameTypeArea = paste0(type, " - ", area)) %>%
        distinct()

      # Generate selectizeInput for each relevant agent
      choicesRoom <- c("Same room", "Skip room", unique(rooms$NameTypeArea))
    }

    selectizeInput(
      inputId = "selectInput_alternative_resources_global",
      label = "Select second choice for each agent:",
      choices = choicesRoom,
      selected = "Same room"
    )
  })

  output$selectInput_alternative_object_resources_global <- renderUI({
    choicesRoom <- c("Same object", "Skip object")

    if (!is.null(canvasObjects$roomsINcanvas)) {
      rooms <- canvasObjects$roomsINcanvas %>%
        select(type, Name, area) %>%
        filter(!type %in% c("Spawnroom", "Fillingroom", "Stair")) %>%
        mutate(NameTypeArea = paste0(type, " - ", area)) %>%
        distinct()

      choicesRoom <- c("Same object", "Skip object", unique(rooms$NameTypeArea))
    }

    # Add objects to choices
    if (!is.null(objectsINcanvas())) {
      choicesRoom <- c(choicesRoom, unique(objectsINcanvas()$Area))
    }

    selectizeInput(
      inputId = "selectInput_alternative_object_resources_global",
      label = "Select second choice for each agent:",
      choices = choicesRoom,
      selected = "Same object"
    )
  })

  observeEvent(input$set_resources, {
    show_modal_spinner()
    if (!is.null(canvasObjects$roomsINcanvas)) {
      if (input$textInput_resources_global != "" &&
        !is.null(input$textInput_resources_global) &&
        !grepl("^[0-9]+$", input$textInput_resources_global) &&
        input$textInput_resources_global >= 0) {
        shinyalert("Error", "You must specify a numeric value greater or equals than 0 (>= 0) for the global number of resources.", type = "error")
        return()
      }

      all_res_rooms <- canvasObjects$roomsINcanvas
      canvasObjects$resources <- NULL
      for (i in unique(paste0(all_res_rooms$type, "-", all_res_rooms$area))) {
        if (is.null(canvasObjects$resources[[i]])) {
          rooms_names <- unique((all_res_rooms %>% filter(type == str_split(i, "-")[[1]][1], area == str_split(i, "-")[[1]][2]))$Name)
          canvasObjects$resources[[i]]$roomResource <- data.frame(
            room = rooms_names,
            MAX = rep(input$textInput_resources_global, length(rooms_names)),
            Policy = rep(input$selectInput_obj_policy_global, length(rooms_names))
          )
          canvasObjects$resources[[i]]$waitingRoomsDeter <- data.frame(Agent = NULL, Room = NULL)
          canvasObjects$resources[[i]]$waitingRoomsRand <- data.frame(Agent = NULL, Room = NULL)
        }


        for (Agent in names(canvasObjects$agents)) {
          canvasObjects$resources[[i]]$waitingRoomsDeter <- rbind(canvasObjects$resources[[i]]$waitingRoomsDeter, data.frame(Agent = Agent, Room = input$selectInput_alternative_resources_global))
          canvasObjects$resources[[i]]$waitingRoomsRand <- rbind(canvasObjects$resources[[i]]$waitingRoomsRand, data.frame(Agent = Agent, Room = input$selectInput_alternative_resources_global))

          if (!is.null(input$textInput_resources_global) &&
            nzchar(input$textInput_resources_global) &&
            nrow(canvasObjects$resources[[i]]$roomResource) > 0) {
            canvasObjects$resources[[i]]$roomResource[[Agent]] <- rep(
              input$textInput_resources_global,
              nrow(canvasObjects$resources[[i]]$roomResource)
            )
          }
        }
      }
    }
    remove_modal_spinner()
  })

  observeEvent(input$set_object_resources, {
    show_modal_spinner()
    if (!is.null(objectsINcanvas())) {
      all_objs <- objectsINcanvas()
      canvasObjects$objectResources <- NULL
      for (i in unique(all_objs$Area)) {
        if (is.null(canvasObjects$objectResources[[i]])) {
          obj_names <- all_objs %>% filter(Area == i)
          canvasObjects$objectResources[[i]]$objectResource <- obj_names %>% select(Name, Capacity)
          canvasObjects$objectResources[[i]]$waitingRoomsDeter <- data.frame(Agent = NULL, Room = NULL)
          canvasObjects$objectResources[[i]]$waitingRoomsRand <- data.frame(Agent = NULL, Room = NULL)
        }

        for (Agent in names(canvasObjects$agents)) {
          canvasObjects$objectResources[[i]]$waitingRoomsDeter <- rbind(canvasObjects$objectResources[[i]]$waitingRoomsDeter, data.frame(Agent = Agent, Room = input$selectInput_alternative_object_resources_global))
          canvasObjects$objectResources[[i]]$waitingRoomsRand <- rbind(canvasObjects$objectResources[[i]]$waitingRoomsRand, data.frame(Agent = Agent, Room = input$selectInput_alternative_object_resources_global))
        }
      }
    }
    remove_modal_spinner()
  })

  # Generate dynamic selectizeInput based on the selected room
  output$dynamicSelectizeInputs_waitingRoomsDeter <- renderUI({
    resources_type <- req(input$selectInput_resources_type)

    ResRoomsDF <- req(allResRooms()) %>%
      filter(Room == resources_type) %>%
      filter(Flow == "Deter")

    rooms <- canvasObjects$roomsINcanvas %>%
      select(type, Name, area) %>%
      filter(!type %in% c("Spawnroom", "Fillingroom", "Stair")) %>%
      mutate(NameTypeArea = paste0(type, " - ", area)) %>%
      distinct()
    relevantAgents <- unique(ResRoomsDF$Agent)

    # Generate selectizeInput for each relevant agent
    if (!is.null(rooms) && dim(rooms)[1] > 1) {
      ListSel <- lapply(relevantAgents, function(agent) {
        # aggionrare i selectize dei waiting se esiste già una selezione!
        waitingRooms <- canvasObjects$resources[[resources_type]]$waitingRoomsDeter

        if (!is.null(waitingRooms)) {
          waitingRooms <- waitingRooms %>% filter(Agent == agent)
        }

        choicesRoom <- c("Same room", "Skip room", unique(rooms$NameTypeArea))

        if (!is.null(waitingRooms) && dim(waitingRooms)[1] > 0) {
          roomSelected <- waitingRooms$Room
        } else {
          roomSelected <- choicesRoom[1]
        }

        selectizeInput(
          inputId = paste0("selectInput_WaitingRoomDeterSelect_", agent),
          label = paste0("Select second choice room in Determined Flow for ", agent, ":"),
          choices = choicesRoom,
          selected = roomSelected
        )
      })
    } else {
      ListSel <- NULL
    }

    return(ListSel)
  })
  output$dynamicSelectizeInputs_waitingRoomsRand <- renderUI({
    resources_type <- req(input$selectInput_resources_type)

    ResRoomsDF <- req(allResRooms()) %>%
      filter(Room == resources_type) %>%
      filter(Flow == "Rand")

    rooms <- canvasObjects$roomsINcanvas %>%
      select(type, Name, area) %>%
      filter(!type %in% c("Spawnroom", "Fillingroom", "Stair")) %>%
      mutate(NameTypeArea = paste0(type, " - ", area)) %>%
      distinct()
    relevantAgents <- unique(ResRoomsDF$Agent)

    # Generate selectizeInput for each relevant agent
    if (!is.null(rooms) && dim(rooms)[1] > 1) {
      ListSel <- lapply(relevantAgents, function(agent) {
        # aggionrare i selectize dei waiting se esiste già una selezione!
        waitingRooms <- canvasObjects$resources[[resources_type]]$waitingRoomsRand

        if (!is.null(waitingRooms)) {
          waitingRooms <- waitingRooms %>% filter(Agent == agent)
        }

        choicesRoom <- c("Same room", "Skip room", unique(rooms$NameTypeArea))

        if (!is.null(waitingRooms) && dim(waitingRooms)[1] > 0) {
          roomSelected <- waitingRooms$Room
        } else {
          roomSelected <- choicesRoom[1]
        }

        selectizeInput(
          inputId = paste0("selectInput_WaitingRoomRandSelect_", agent),
          label = paste0("Select second choice room in Random Flow for ", agent, ":"),
          choices = choicesRoom,
          selected = roomSelected
        )
      })
    } else {
      ListSel <- NULL
    }

    return(ListSel)
  })

  observe({
    selectW <- grep(x = names(input), pattern = "selectInput_WaitingRoomDeterSelect_", value = T)

    isolate({
      resources_type <- input$selectInput_resources_type
      waitingRooms <- canvasObjects$resources[[resources_type]]$waitingRoomsDeter
    })

    if (length(selectW) > 0) {
      waitingRooms <- do.call(
        rbind,
        lapply(selectW, function(W) {
          data.frame(
            Agent = gsub(pattern = "selectInput_WaitingRoomDeterSelect_", replacement = "", x = W),
            Room = input[[W]]
          )
        })
      )
    }

    isolate({
      waitingRooms -> canvasObjects$resources[[resources_type]]$waitingRoomsDeter
    })
  })

  observe({
    selectW <- grep(x = names(input), pattern = "selectInput_WaitingRoomRandSelect_", value = T)

    isolate({
      resources_type <- input$selectInput_resources_type
      waitingRooms <- canvasObjects$resources[[resources_type]]$waitingRoomsRand
    })

    if (length(selectW) > 0) {
      waitingRooms <- do.call(
        rbind,
        lapply(selectW, function(W) {
          data.frame(
            Agent = gsub(pattern = "selectInput_WaitingRoomRandSelect_", replacement = "", x = W),
            Room = input[[W]]
          )
        })
      )
    }

    isolate({
      waitingRooms -> canvasObjects$resources[[resources_type]]$waitingRoomsRand
    })
  })

  # Dynamic selectors for Objects (Unified)
  output$dynamicSelectizeInputs_waitingObjects <- renderUI({
    resources_type <- req(selected_object_key())
    if (resources_type == " - ") {
      return()
    }
    relevantAgents <- names(canvasObjects$agents)

    rooms <- canvasObjects$roomsINcanvas %>%
      select(type, Name, area) %>%
      filter(!type %in% c("Spawnroom", "Fillingroom", "Stair")) %>%
      mutate(NameTypeArea = paste0(type, " - ", area)) %>%
      distinct()

    choicesRoom <- c("Same object", "Skip object", unique(rooms$NameTypeArea))
    if (!is.null(objectsINcanvas())) {
      choicesRoom <- c(choicesRoom, unique(objectsINcanvas()$Area))
    }

    ListSel <- lapply(relevantAgents, function(agent) {
      # Use Deter as primary source of truth for the selector, they should be synced
      waitingRooms <- canvasObjects$objectResources[[resources_type]]$waitingRoomsDeter
      if (!is.null(waitingRooms)) {
        waitingRooms <- waitingRooms %>% filter(Agent == agent)
      }

      if (!is.null(waitingRooms) && dim(waitingRooms)[1] > 0) {
        roomSelected <- waitingRooms$Room
      } else {
        roomSelected <- "Same object"
      }

      selectizeInput(
        inputId = paste0("selectInput_WaitingObjectSelect_", agent),
        label = paste0("Select second choice for ", agent, " at ", resources_type, ":"),
        choices = choicesRoom,
        selected = roomSelected
      )
    })
    return(ListSel)
  })

  observe({
    selectW <- grep(x = names(input), pattern = "selectInput_WaitingObjectSelect_", value = T)
    isolate({
      resources_type <- selected_object_key()
      if (is.null(resources_type) || resources_type == "" || resources_type == " - ") {
        return()
      }
    })

    if (length(selectW) > 0) {
      waitingRooms <- do.call(
        rbind,
        lapply(selectW, function(W) {
          data.frame(
            Agent = gsub(pattern = "selectInput_WaitingObjectSelect_", replacement = "", x = W),
            Room = input[[W]]
          )
        })
      )

      isolate({
        if (!is.null(resources_type) && resources_type != "") {
          canvasObjects$objectResources[[resources_type]]$waitingRoomsDeter <- waitingRooms
          canvasObjects$objectResources[[resources_type]]$waitingRoomsRand <- waitingRooms
        }
      })
    }
  })
  observe({
    if (!is.null(allResRooms())) {
      choices <- unique(allResRooms()$Room)
      choices <- choices[!grepl(paste0("Spawnroom", collapse = "|"), choices)]
      # choices <- choices[!grepl(paste0("Fillingroom", collapse = "|"), choices)]
      choices <- choices[!grepl(paste0("Stair", collapse = "|"), choices)]

      updateSelectizeInput(session, "selectInput_resources_type", choices = choices, selected = "", server = TRUE)
    }
  })

  observe({
    if (!is.null(allResObjects())) {
      choices <- unique(allResObjects()$Room)
      updateSelectizeInput(session, "selectInput_object_resources_room", choices = choices, selected = "", server = TRUE)
    }
  })

  # Sync room policy selector when room AND object change OR when the underlying data changes
  observe({
    res_type <- req(selected_object_key())
    if (res_type == " - ") {
      return()
    }
    obj_name <- req(input$selectInput_object_resources_name)

    # Accessing objectResource here makes this observer reactive to changes in it
    res_data <- canvasObjects$objectResources[[res_type]]$objectResource

    if (!is.null(res_data) && nrow(res_data) > 0) {
      row_idx <- which(res_data$object == obj_name)
      if (length(row_idx) > 0) {
        current_policy <- res_data$Policy[row_idx]
        if (!is.null(input$selectInput_obj_policy_room) && input$selectInput_obj_policy_room != current_policy) {
          updateSelectInput(session, "selectInput_obj_policy_room", selected = current_policy)
        }
      }
    }
  })

  # Update object-specific policy only when save button is clicked
  observeEvent(input$save_room_policy_btn, {
    res_type <- req(selected_object_key())
    obj_name <- req(input$selectInput_object_resources_name)
    policy <- input$selectInput_obj_policy_room

    isolate({
      if (!is.null(canvasObjects$objectResources[[res_type]]$objectResource)) {
        data <- canvasObjects$objectResources[[res_type]]$objectResource
        row_idx <- which(data$object == obj_name)
        if (length(row_idx) > 0) {
          data$Policy[row_idx] <- policy
          canvasObjects$objectResources[[res_type]]$objectResource <- data
          shinyalert("Success", paste0("Selection policy for ", obj_name, " in ", res_type, " updated to: ", policy), type = "success")
        }
      }
    })
  })

  # Apply global policy to all objects in all rooms
  observeEvent(input$set_object_resources, {
    global_policy <- input$selectInput_obj_policy_global
    isolate({
      for (res_type in names(canvasObjects$objectResources)) {
        data <- canvasObjects$objectResources[[res_type]]$objectResource
        if (!is.null(data)) {
          data$Policy <- global_policy
          canvasObjects$objectResources[[res_type]]$objectResource <- data
        }
      }
      shinyalert("Success", paste0("Global policy applied: all objects set to '", global_policy, "'"), type = "success")
    })
  })

  observe({
    req(input$selectInput_object_resources_room)
    if (!is.null(allResObjects())) {
      choices <- allResObjects() %>%
        filter(Room == input$selectInput_object_resources_room) %>%
        pull(Name) %>%
        unique()
      updateSelectizeInput(session, "selectInput_object_resources_name", choices = choices, selected = "", server = TRUE)
    }
  })

  observe({
    # give a default to resources and waitingrooms
    resources_type <- req(input$selectInput_resources_type)
    ResRoomsDF <- req(allResRooms()) %>% filter(Room == resources_type)

    rooms <- canvasObjects$roomsINcanvas %>%
      select(type, Name, area) %>%
      mutate(TypeArea = paste0(type, "-", area)) %>%
      filter(TypeArea == resources_type) %>%
      distinct()

    isolate({
      if (dim(rooms)[1] == 0) {
        data <- data.frame()
      } else if (is.null(canvasObjects$resources[[resources_type]]$roomResource)) {
        data <- data.frame(room = rooms$Name, MAX = 0)
        for (a in unique(ResRoomsDF$Agent)) {
          data[, a] <- 0
        }
      } else {
        # If there exist already the dataset, then it is used and we have to check that there is already the agents
        dataOLD <- canvasObjects$resources[[resources_type]]$roomResource

        data <- dataOLD[, c("room", "MAX")]
        for (a in unique(ResRoomsDF$Agent)) {
          if (a %in% colnames(dataOLD)) {
            data[, a] <- dataOLD[, a]
          } else {
            data[, a] <- 0
          }
        }
        # filter the rooms already present to keep only the new added in the canvas
        dataNEW <- rooms %>% filter(!Name %in% dataOLD$room)

        if (dim(dataNEW)[1] > 0) {
          dataNew <- setNames(data.frame(matrix(0, ncol = length(colnames(dataOLD)), nrow = dim(dataNEW)[1])), colnames(dataOLD))
          dataNew$room <- dataNEW$Name
          data <- rbind(data, dataNew)
        }
      }

      canvasObjects$resources[[resources_type]]$roomResource <- data
    })

    isolate({
      ### E' da sistemare in maniera che si ricrodi cosa avevo inserito sia in rand che determi
      data_waiting <- data.frame()

      data_waitingOLD <- canvasObjects$resources[[resources_type]]$waitingRoomsDeter
      if (is.null(data_waitingOLD) || nrow(data_waitingOLD) == 0) {
        agents <- unique(ResRoomsDF[ResRoomsDF$Flow == "Deter", "Agent"])
        if (length(agents) > 0) {
          data_waiting <- do.call(
            rbind,
            lapply(agents, function(W) {
              data.frame(
                Agent = W,
                Room = "Same room"
              )
            })
          )
        }
      } else {
        # If there exist already the dataset, then it is used and we have to check that there is already the agents

        data_waiting <- data_waitingOLD[, c("Agent", "Room")]
        for (a in unique(ResRoomsDF$Agent)) {
          if (a %in% data_waitingOLD$Agent) {
            data_waiting[data_waiting$Agent == a, "Room"] <- data_waitingOLD[data_waiting$Agent == a, "Room"]
          } else {
            data_waiting <- rbind(data_waiting, data.frame(Agent = a, Room = "Same room"))
          }
        }

        agent_eliminated <- data_waitingOLD$Agent[!(data_waitingOLD$Agent %in% ResRoomsDF$Agent)]

        if (length(agent_eliminated) != 0) {
          data_waiting <- data_waiting %>% filter(!Agent %in% agent_eliminated)
        }
      }

      canvasObjects$resources[[resources_type]]$waitingRoomsDeter <- data_waiting
    })
    isolate({
      ### E' da sistemare in maniera che si ricrodi cosa avevo inserito sia in rand che determi
      data_waiting <- data.frame()

      data_waitingOLD <- canvasObjects$resources[[resources_type]]$waitingRoomsRand
      if (is.null(data_waitingOLD)) {
        agents <- unique(ResRoomsDF[ResRoomsDF$Flow == "Rand", "Agent"])
        if (length(agents) > 0) {
          data_waiting <- do.call(
            rbind,
            lapply(agents, function(W) {
              data.frame(
                Agent = W,
                Room = "Same room"
              )
            })
          )
        }
      } else {
        # If there exist already the dataset, then it is used and we have to check that there is already the agents

        if (nrow(data_waitingOLD) > 0) {
          data_waiting <- data_waitingOLD[, c("Agent", "Room")]
        }

        for (a in unique(ResRoomsDF$Agent)) {
          if (a %in% data_waitingOLD$Agent) {
            data_waiting[data_waiting$Agent == a, "Room"] <- data_waitingOLD[data_waiting$Agent == a, "Room"]
          } else {
            data_waiting <- rbind(data_waiting, data.frame(Agent = a, Room = "Same room"))
          }
        }

        agent_eliminated <- data_waitingOLD$Agent[!(data_waitingOLD$Agent %in% ResRoomsDF$Agent)]

        if (length(agent_eliminated) != 0) {
          data_waiting <- data_waiting %>% filter(!Agent %in% agent_eliminated)
        }
      }

      canvasObjects$resources[[resources_type]]$waitingRoomsRand <- data_waiting
    })
  })

  observe({
    # give a default to resources and waitingrooms for objects
    resources_type <- req(selected_object_key())
    if (resources_type == " - ") {
      return()
    }
    relevantAgents <- names(canvasObjects$agents)

    objs <- objectsINcanvas() %>%
      filter(Area == resources_type) %>%
      distinct()

    isolate({
      if (dim(objs)[1] == 0) {
        data <- data.frame()
      } else if (is.null(canvasObjects$objectResources[[resources_type]]$objectResource)) {
        data <- data.frame(object = objs$Name, MAX = objs$Capacity, Policy = "Random")
        for (a in relevantAgents) {
          data[, a] <- 0
        }
      } else {
        # If there exist already the dataset, then it is used and we have to check that there is already the agents
        dataOLD <- canvasObjects$objectResources[[resources_type]]$objectResource

        data <- dataOLD[, c("object", "MAX", "Policy")]
        for (a in relevantAgents) {
          if (a %in% colnames(dataOLD)) {
            data[, a] <- dataOLD[, a]
          } else {
            data[, a] <- 0
          }
        }
        # filter the objects already present to keep only the new added in the canvas
        dataNEW <- objs %>% filter(!Name %in% dataOLD$object)

        if (dim(dataNEW)[1] > 0) {
          dataNew <- setNames(data.frame(matrix(0, ncol = length(colnames(dataOLD)), nrow = dim(dataNEW)[1])), colnames(dataOLD))
          dataNew$object <- dataNEW$Name
          dataNew$Policy <- "Random"
          data <- rbind(data, dataNew)
        }
      }

      if (dim(data)[1] > 0) {
        canvasObjects$objectResources[[resources_type]]$objectResource <- data
      }
    })

    isolate({
      ### E' da sistemare in maniera che si ricrodi cosa avevo inserito sia in rand che determi
      data_waiting <- data.frame()

      data_waitingOLD <- canvasObjects$objectResources[[resources_type]]$waitingRoomsDeter
      if (is.null(data_waitingOLD) || nrow(data_waitingOLD) == 0) {
        if (length(relevantAgents) > 0) {
          data_waiting <- data.frame(Agent = relevantAgents, Room = "Same object")
        }
      } else {
        # If there exist already the dataset, then it is used and we have to check that there is already the agents

        data_waiting <- data_waitingOLD[, c("Agent", "Room")]
        for (a in relevantAgents) {
          if (a %in% data_waitingOLD$Agent) {
            data_waiting[data_waiting$Agent == a, "Room"] <- data_waitingOLD[data_waiting$Agent == a, "Room"]
          } else {
            data_waiting <- rbind(data_waiting, data.frame(Agent = a, Room = "Same object"))
          }
        }

        agent_eliminated <- data_waitingOLD$Agent[!(data_waitingOLD$Agent %in% relevantAgents)]

        if (length(agent_eliminated) != 0) {
          data_waiting <- data_waiting %>% filter(!Agent %in% agent_eliminated)
        }
      }

      canvasObjects$objectResources[[resources_type]]$waitingRoomsDeter <- data_waiting
    })

    isolate({
      ### E' da sistemare in maniera che si ricrodi cosa avevo inserito sia in rand che determi
      data_waiting <- data.frame()

      data_waitingOLD <- canvasObjects$objectResources[[resources_type]]$waitingRoomsRand
      if (is.null(data_waitingOLD) || nrow(data_waitingOLD) == 0) {
        if (length(relevantAgents) > 0) {
          data_waiting <- data.frame(Agent = relevantAgents, Room = "Same object")
        }
      } else {
        # If there exist already the dataset, then it is used and we have to check that there is already the agents

        data_waiting <- data_waitingOLD[, c("Agent", "Room")]
        for (a in relevantAgents) {
          if (a %in% data_waitingOLD$Agent) {
            data_waiting[data_waiting$Agent == a, "Room"] <- data_waitingOLD[data_waiting$Agent == a, "Room"]
          } else {
            data_waiting <- rbind(data_waiting, data.frame(Agent = a, Room = "Same object"))
          }
        }

        agent_eliminated <- data_waitingOLD$Agent[!(data_waitingOLD$Agent %in% relevantAgents)]

        if (length(agent_eliminated) != 0) {
          data_waiting <- data_waiting %>% filter(!Agent %in% agent_eliminated)
        }
      }

      canvasObjects$objectResources[[resources_type]]$waitingRoomsRand <- data_waiting
    })
  })

  # Summary dataframe of all object selection policies (room, object, policy)
  projectObjectPolicies <- reactive({
    res_list <- canvasObjects$objectResources
    if (length(res_list) == 0) {
      return(data.frame(room = character(), object = character(), policy = character()))
    }

    do.call(rbind, lapply(names(res_list), function(r_type) {
      data <- res_list[[r_type]]$objectResource
      if (is.null(data) || !"Policy" %in% colnames(data)) {
        return(NULL)
      }
      data.frame(room = r_type, object = data$object, policy = data$Policy)
    }))
  })

  output$RoomAgentResTable <- DT::renderDataTable(
    {
      resource_data <- canvasObjects$resources[[input$selectInput_resources_type]]$roomResource
      if (is.null(resource_data)) {
        return()
      }

      DT::datatable(resource_data,
        editable = list(target = "cell", disable = list(columns = c(0))),
        rownames = FALSE,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = "t",
          columnDefs = list(list(className = "dt-center", targets = "_all"))
        ),
        colnames = c("Room", "Maximum Capacity", colnames(resource_data)[-c(1, 2)])
      )
    },
    server = T
  )

  observe({
    output$ObjectAgentResTable <- DT::renderDataTable(
      {
        resources_type <- req(selected_object_key())
        if (resources_type == " - ") {
          return()
        }
        DT::datatable(canvasObjects$objectResources[[resources_type]]$objectResource,
          editable = list(target = "cell", disable = list(columns = c(0))),
          rownames = FALSE,
          options = list(
            pageLength = 10,
            scrollX = TRUE,
            dom = "t",
            columnDefs = list(list(className = "dt-center", targets = "_all"))
          ),
          colnames = c("Object", "Maximum", "Selection Policy", colnames(canvasObjects$objectResources[[resources_type]]$objectResource)[-c(1, 2, 3)])
        )
      },
      server = T
    )
  })

  observeEvent(input$RoomAgentResTable_cell_edit, {
    info <- input$RoomAgentResTable_cell_edit
    resources_type <- input$selectInput_resources_type
    data <- canvasObjects$resources[[resources_type]]$roomResource

    # R uses 1-based indexing, info$col is 0-based
    col_idx <- info$col + 1
    row_idx <- info$row

    if (col_idx == 1) {
      # Room name (read-only)
      return()
    } else if (col_idx == 3) {
      # Policy column (string)
      newValue <- info$value
      if (!(newValue %in% c("Closest to door", "Random"))) {
        shinyalert("Error", "Object selection policy must be either 'Closest to door' or 'Random'.", type = "error")
        return()
      }
      canvasObjects$resources[[resources_type]]$roomResource[row_idx, col_idx] <- newValue
    } else {
      # Capacity columns (numeric)
      oldValue <- data[row_idx, col_idx]
      newValue <- as.numeric(info$value)
      if (is.na(newValue) || newValue < 0) {
        shinyalert("Error", "You must specify a numeric value >= 0.", type = "error")
        return()
      }
      canvasObjects$resources[[resources_type]]$roomResource[row_idx, col_idx] <- newValue
    }
  })

  observeEvent(input$ObjectAgentResTable_cell_edit, {
    info <- input$ObjectAgentResTable_cell_edit
    resources_type <- req(selected_object_key())
    if (resources_type == " - ") {
      return()
    }
    canvasObjects$objectResources[[resources_type]]$objectResource -> data
    oldValue <- data[info$row, info$col + 1]
    canvasObjects$objectResources[[resources_type]]$objectResource[info$row, info$col + 1] <- newValue <- as.numeric(info$value)
    if (is.na(newValue) || newValue < 0) {
      shinyalert("Error", "You must specify a numeric value greater or equals than 0 (>= 0) for the number of resources.", type = "error")
      canvasObjects$objectResources[[resources_type]]$objectResource[info$row, info$col + 1] <- oldValue
    }
  })

  #### Flow
  # Funzione per verificare se un tipo di stanza è presente nel flusso di un agente
  check_room_type_in_agent_flow <- function(agent_name, room_type) {
    # Verifica se il flusso dell'agente contiene il tipo di stanza
    if (!is.null(canvasObjects$agents[[agent_name]]$DeterFlow) || !is.null(canvasObjects$agents[[agent_name]]$RandFlow)) {
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

    file_path <- paste0(
      system.file("Shiny", "Descriptions", package = "FORGE4FLAME"),
      "/", disease_model, "_description.txt"
    )

    # Leggi il testo dal file corrispondente
    if (file.exists(file_path)) {
      description_text <- readLines(file_path, warn = FALSE)
      return(description_text)
    } else {
      return("Description not available for this model.")
    }
  })

  # Save values for the selected disease model #
  observeEvent(input$save_values_disease_model, {
    disable("rds_generation")
    disable("flamegpu_connection")
    Name <- input$disease_model
    virus_severity <- NULL
    beta_contact <- NULL
    beta_aerosol <- NULL
    gamma_time <- NULL
    gamma_dist <- NULL
    alpha_time <- NULL
    alpha_dist <- NULL
    lambda_time <- NULL
    lambda_dist <- NULL
    nu_time <- NULL
    nu_dist <- NULL

    num_classes <- if (isTRUE(input$enable_risk_classes)) input$num_risk_classes else 1

    if (is.null(num_classes)) {
      shinyalert("Error", "Please specify the number of risk classes.", "error")
      return()
    }

    risk_classes <- list()
    total_prop <- 0

    for (i in 1:num_classes) {
      suffix <- if (isTRUE(input$enable_risk_classes)) paste0("_class_", i) else ""
      risk_name <- if (isTRUE(input$enable_risk_classes)) input[[paste0("risk_class_name_", i)]] else "Risk class 1"
      disease_model_name <- Name

      virus_severity <- gsub(",", ".", input[[paste0("virus_severity", suffix)]])

      if ((virus_severity) > 1 || (virus_severity) < 0) {
        shinyalert("Error", "Virus severity must be  in [0, 1].", type = "error")
        return()
      }

      beta_contact <- gsub(",", ".", input[[paste0("beta_contact", suffix)]])
      beta_aerosol <- gsub(",", ".", input[[paste0("beta_aerosol", suffix)]])

      if (is.na(as.numeric(beta_contact)) || is.na(as.numeric(beta_aerosol))) {
        shinyalert("You must specify numeric values for beta (contact and aerosol).", "error")
        return()
      }

      if (isTRUE(input$enable_risk_classes)) {
        proportion <- input[[paste0("risk_class_proportion_", i)]]
        if (is.null(proportion) || proportion < 0 || proportion > 1) {
          shinyalert("Error", paste0("Invalid proportion for ", risk_name, " (must be 0-1)."), "error")
          return()
        }
        total_prop <- total_prop + proportion
      } else {
        proportion <- total_prop <- 1
      }

      # Check gamma for all models
      gamma_params <- check_distribution_parameters(input, paste0("gamma", suffix))
      gamma_dist <- gamma_params[[1]]
      gamma_time <- gamma_params[[2]]

      if (is.null(gamma_dist) || is.null(gamma_time)) {
        shinyalert("Error", paste0("Specify a value for gamma for ", risk_name), "error")
        return()
      }

      # Base list for this class
      cls <- list(
        name = risk_name,
        disease_model_name = disease_model_name,
        virus_severity = virus_severity,
        beta_contact = beta_contact,
        beta_aerosol = beta_aerosol,
        gamma_dist = gamma_dist,
        gamma_time = gamma_time,
        proportion = proportion
      )

      # Check alpha if model has E (exposed)
      if (grepl("E", disease_model_name)) {
        alpha_params <- check_distribution_parameters(input, paste0("alpha", suffix))
        if (any(sapply(alpha_params, is.null))) {
          shinyalert("Error", paste0("Please specify alpha for ", risk_name), "error")
          return()
        }
        cls$alpha_dist <- alpha_params[[1]]
        cls$alpha_time <- alpha_params[[2]]
      }

      # Check lambda if model has D (deaths)
      if (grepl("D", disease_model_name)) {
        lambda_params <- check_distribution_parameters(input, paste0("lambda", suffix))
        if (any(sapply(lambda_params, is.null))) {
          shinyalert("Error", paste0("Please specify lambda for ", risk_name), "error")
          return()
        }
        cls$lambda_dist <- lambda_params[[1]]
        cls$lambda_time <- lambda_params[[2]]
      }

      # Check nu if double S (end of immunity)
      if (grepl("^([^S]*S[^S]*S[^S]*)$", disease_model_name)) {
        nu_params <- check_distribution_parameters(input, paste0("nu", suffix))
        if (any(sapply(nu_params, is.null))) {
          shinyalert("Error", paste0("Please specify nu for ", risk_name), "error")
          return()
        }
        cls$nu_dist <- nu_params[[1]]
        cls$nu_time <- nu_params[[2]]
      }

      risk_classes[[i]] <- cls
    }

    # Check if proportions sum to ~1.0
    if (isTRUE(input$enable_risk_classes) && abs(total_prop - 1.0) > 0.01) {
      shinyalert("Error", paste0("The sum of all proportions must equal 1.0 (current sum: ", round(total_prop, 2), ")"), "error")
      return()
    }

    ListParamsDisease <- risk_classes
    shinyalert("Success", paste0(num_classes, " risk classes saved successfully!"), "success", timer = 3000)

    canvasObjects$disease <- ListParamsDisease
  })

  observeEvent(input$save_values_virus_parameters, {
    disable("rds_generation")
    disable("flamegpu_connection")

    ngen_base <- input$ngen_base
    vl <- input$vl
    decay_rate <- input$decay_rate
    gravitational_settling_rate <- input$gravitational_settling_rate
    inhalation_rate_pure <- input$inhalation_rate_pure

    if (is.null(ngen_base) || !is.numeric(ngen_base) || ngen_base < 0) {
      shinyalert("Error", "The exhalation rate pure must be a number greater than 0.", "error")
      return()
    }

    if (is.null(vl) || !is.numeric(vl) || vl < 0) {
      shinyalert("Error", "The viral load must be a number greater than 0.", "error")
      return()
    }

    if (is.null(decay_rate) || !is.numeric(decay_rate) || decay_rate < 0) {
      shinyalert("Error", "The decay rate must be a number greater than 0.", "error")
      return()
    }

    if (is.null(gravitational_settling_rate) || !is.numeric(gravitational_settling_rate) || gravitational_settling_rate < 0) {
      shinyalert("Error", "The gravitational settling rate must be a number greater than 0.", "error")
      return()
    }

    if (is.null(inhalation_rate_pure) || !is.numeric(inhalation_rate_pure) || inhalation_rate_pure < 0) {
      shinyalert("Error", "The inhalation rate pure must be a number greater than 0.", "error")
      return()
    }

    canvasObjects$virus_parameters$ngen_base <- ngen_base
    canvasObjects$virus_parameters$vl <- vl
    canvasObjects$virus_parameters$decay_rate <- decay_rate
    canvasObjects$virus_parameters$gravitational_settling_rate <- gravitational_settling_rate
    canvasObjects$virus_parameters$inhalation_rate_pure <- inhalation_rate_pure
  })

  output$disease_model_value <- renderText({
    if (!is.null(canvasObjects$disease)) {
      text <- paste0("Risk classes: ", length(canvasObjects$disease), "; Disease model: ", canvasObjects$disease[[1]]$disease_model_name, ".\n\n")
      for (i in 1:length(canvasObjects$disease)) {
        disease_risk_class <- canvasObjects$disease[[i]]

        proportion <- disease_risk_class$proportion

        text <- paste0(text, "Risk class ", i, ": Proportion: ", proportion, ", Beta (contact): ", disease_risk_class$beta_contact, ", Beta (aerosol): ", disease_risk_class$beta_aerosol, ", Gamma: ", disease_risk_class$gamma_time, " (", disease_risk_class$gamma_dist, ")")
        if (!is.null(disease_risk_class$alpha_time)) {
          text <- paste0(text, ", Alpha: ", disease_risk_class$alpha_time, " (", disease_risk_class$alpha_dist, ")")
        }
        if (!is.null(disease_risk_class$lambda_time)) {
          text <- paste0(text, ", Lambda: ", disease_risk_class$lambda_time, " (", disease_risk_class$lambda_dist, ")")
        }
        if (!is.null(disease_risk_class$nu_time)) {
          text <- paste0(text, ", Nu: ", disease_risk_class$nu_time, " (", disease_risk_class$nu_dist, ")")
        }
        text <- paste0(text, "\n")
      }
      text
    }
  })

  #####  Risk Classes for Infectious States #####

  # Reactive value to store risk classes data
  risk_classes_data <- reactiveVal(list())

  observe({
    req(input$enable_risk_classes)

    # Generate UI for risk classes dynamically
    output$risk_classes_ui <- renderUI({
      num_classes <- as.integer(input$num_risk_classes)
      if (is.null(num_classes) || num_classes < 2) num_classes <- 2

      disease_model <- input$disease_model
      if (is.null(disease_model)) disease_model <- "SIR"

      existing_data <- risk_classes_data()
      if (is.null(existing_data) || length(existing_data) != num_classes) {
        return()
      }
      # Create UI for each risk class
      class_uis <- lapply(1:num_classes, function(i) {
        # Pre-fill with existing data if available
        class_name <- if (!is.null(existing_data[[i]]$name)) existing_data[[i]]$name else paste0("Risk Class ", i)
        beta_contact_val <- if (!is.null(existing_data[[i]]$beta_contact)) existing_data[[i]]$beta_contact else ifelse(i == 1, "0.024", "0.024")
        beta_aerosol_val <- if (!is.null(existing_data[[i]]$beta_aerosol)) existing_data[[i]]$beta_aerosol else ifelse(i == 1, "410", "410")
        proportion_val <- if (!is.null(existing_data[[i]]$proportion)) existing_data[[i]]$proportion else round(1 / num_classes, 2)

        div(
          style = paste0("border-left: 4px solid ", c("#e74c3c", "#3498db", "#2ecc71", "#f39c12", "#9b59b6")[i], "; padding: 15px; margin-bottom: 15px; background-color: #f9f9f9; border-radius: 8px;"),
          fluidRow(
            column(
              12,
              tags$h4(
                style = paste0("color: ", c("#e74c3c", "#3498db", "#2ecc71", "#f39c12", "#9b59b6")[i], "; font-weight: 600;"),
                icon("layer-group"), " ", class_name
              )
            )
          ),
          fluidRow(
            column(
              6,
              textInput(
                inputId = paste0("risk_class_name_", i),
                label = "Class Name:",
                value = class_name,
                placeholder = paste0("e.g., High Risk, Low Risk")
              )
            ),
            column(
              6,
              numericInput(
                inputId = paste0("risk_class_proportion_", i),
                label = HTML("Proportion of Infected <i>(0-1)</i>:"),
                value = proportion_val,
                min = 0,
                max = 1,
                step = 0.01
              )
            )
          ),
          fluidRow(
            column(
              width = 6,
              div(
                class = "icon-container",
                h5(tags$b("Virus severity: "), icon("info-circle")),
                div(class = "icon-text", "Probability to show sever symptoms. In [4] you can find an example for the Covid-19.")
              ),
              numericInput(
                inputId = paste0("virus_severity_class_", i),
                label = NULL,
                value = 0.22, max = 1, min = 0
              )
            )
          ),
          fluidRow(
            column(
              6,
              div(
                class = "icon-container",
                tags$label(icon("info-circle"), " β", tags$sub("contact"), " (Contact Risk):"),
                tags$div(
                  class = "icon-text",
                  style = "width: 250px;",
                  "Contamination risk for this class due to close-range contacts. Higher values indicate more infectious individuals."
                )
              ),
              textInput(
                inputId = paste0("beta_contact_class_", i),
                label = NULL,
                value = beta_contact_val,
                placeholder = "e.g., 0.024"
              )
            ),
            column(
              6,
              div(
                class = "icon-container",
                tags$label(icon("info-circle"), " β", tags$sub("aerosol"), " (Aerosol Risk):"),
                tags$div(
                  class = "icon-text",
                  style = "width: 250px;",
                  "Risk constant for this class due to aerosol transmission. Higher values indicate more infectious individuals."
                )
              ),
              textInput(
                inputId = paste0("beta_aerosol_class_", i),
                label = NULL,
                value = beta_aerosol_val,
                placeholder = "e.g., 410"
              )
            )
          ),
          fluidRow(
            column(
              12,
              div(
                class = "icon-container",
                h5(icon("info-circle"), " γ (Recovery Rate):"),
                div(class = "icon-text", "γ represents the recovery rate for this risk class.")
              ),
              get_distribution_panel(paste0("gamma_class_", i))
            )
          ),
          if (disease_model %in% c("SEIR", "SEIRS", "SEIRD", "SEIRDS")) {
            fluidRow(
              column(
                12,
                div(
                  class = "icon-container",
                  h5(icon("info-circle"), " α (Incubation Rate):"),
                  div(class = "icon-text", "α represents the incubation rate for this risk class.")
                ),
                get_distribution_panel(paste0("alpha_class_", i))
              )
            )
          },
          if (disease_model %in% c("SIRD", "SEIRD", "SEIRDS", "SIRDS")) {
            fluidRow(
              column(
                12,
                div(
                  class = "icon-container",
                  h5(icon("info-circle"), " λ (Fatality Rate):"),
                  div(class = "icon-text", "λ represents the fatality rate for this risk class.")
                ),
                get_distribution_panel(paste0("lambda_class_", i))
              )
            )
          },
          if (disease_model %in% c("SIRS", "SEIRS", "SIRDS", "SEIRDS")) {
            fluidRow(
              column(
                12,
                div(
                  class = "icon-container",
                  h5(icon("info-circle"), " ν (End-of-Immunization Rate):"),
                  div(class = "icon-text", "ν represents the end-of-immunization rate for this risk class.")
                ),
                get_distribution_panel(paste0("nu_class_", i))
              )
            )
          },
          tags$hr(style = "border-top: 1px dashed #ddd;")
        )
      })

      # Add save button and total proportion display
      tagList(
        class_uis,
        fluidRow(
          column(
            6,
            tags$div(
              style = "padding: 10px; background-color: #ecf0f1; border-radius: 5px; margin-top: 10px;",
              tags$strong("Total Proportion: "),
              textOutput("total_proportion_display", inline = TRUE),
              tags$span(
                id = "proportion_warning",
                style = "color: #e74c3c; margin-left: 10px;",
                uiOutput("proportion_warning_text", inline = TRUE)
              )
            )
          )
        )
      )
    })
  })

  observe({
    req(input$num_risk_classes)
    req(input$enable_risk_classes)

    # Calculate and display total proportion
    output$total_proportion_display <- renderText({
      req(input$enable_risk_classes)
      num_classes <- input$num_risk_classes
      if (is.null(num_classes)) {
        return("0.00")
      }

      total <- 0
      for (i in 1:num_classes) {
        prop_val <- input[[paste0("risk_class_proportion_", i)]]
        if (!is.null(prop_val) && !is.na(prop_val)) {
          total <- total + prop_val
        }
      }
      sprintf("%.2f", total)
    })
    # Warning if total proportion != 1
    output$proportion_warning_text <- renderUI({
      req(input$enable_risk_classes)
      num_classes <- input$num_risk_classes
      if (is.null(num_classes)) {
        return(NULL)
      }

      total <- 0
      for (i in 1:num_classes) {
        prop_val <- input[[paste0("risk_class_proportion_", i)]]
        if (!is.null(prop_val) && !is.na(prop_val)) {
          total <- total + prop_val
        }
      }

      if (abs(total - 1.0) > 0.001) {
        HTML(paste0("<i class='fa fa-exclamation-triangle'></i> Warning: Should sum to 1.0"))
      } else {
        HTML("<i class='fa fa-check' style='color: #2ecc71;'></i> OK")
      }
    })
  })

  observe({
    # Store current data before updating
    req(input$num_risk_classes)
    req(input$enable_risk_classes)

    num_classes <- input$num_risk_classes

    current_data <- list()
    for (i in 1:num_classes) {
      current_data[[i]] <- list(
        name = input[[paste0("risk_class_name_", i)]],
        beta_contact = input[[paste0("risk_class_beta_contact_", i)]],
        beta_aerosol = input[[paste0("risk_class_beta_aerosol_", i)]],
        proportion = input[[paste0("risk_class_proportion_", i)]]
      )
    }
    risk_classes_data(current_data)
  })


  # Reset risk classes when checkbox is unchecked
  observeEvent(input$enable_risk_classes, {
    if (!input$enable_risk_classes) {
      canvasObjects$risk_classes <- NULL
      canvasObjects$risk_classes_enabled <- FALSE
      risk_classes_data(list())
    }
  })

  ####  Save what-if #####
  add_data <- function(measure, parameters, type, from, to, data) {
    # Check if the exact row already exists
    duplicate_row <- subset(data, Measure == measure & Parameters == parameters & Type == type & From == from & To == to)
    if (nrow(duplicate_row) > 0) {
      shinyalert("Error", "This entry already exists!", type = "error")
      return(NULL)
    }

    # Check for overlapping time ranges
    if (!is.na(to)) {
      overlap_row <- subset(data, Measure == measure & Type == type &
        ((From <= to & To >= from) | (to >= From & from <= To)))
      if (nrow(overlap_row) > 0) {
        shinyalert("Error", "Time range overlaps with an existing entry!", type = "error")
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
    rooms_whatif <- canvasObjects$rooms_whatif

    if (as.integer(input$ventilation_time_to) < as.integer(input$ventilation_time_from) ||
      as.integer(input$ventilation_time_to) > as.numeric(canvasObjects$starting$simulation_days) ||
      as.integer(input$ventilation_time_from) <= 0) {
      shinyalert("Error", paste0("The timing should be greater than 0, less than the simulation days (", canvasObjects$starting$simulation_days, "), and 'to'>'from'. "), type = "error")
      return()
    }

    if (input$ventilation_params == "Custom value") {
      if (is.null(input$ventilation_params_custom) || !is.numeric(input$ventilation_params_custom) || input$ventilation_params_custom < 0) {
        shinyalert("Error", paste0("The custom ventilation value must be a number greater than 0."), type = "error")
        return()
      }
    }

    if (is.null(input$ventilation_air) || !is.numeric(input$ventilation_air) || input$ventilation_air < 0 || input$ventilation_air > 100) {
      shinyalert("Error", paste0("The fraction of air supplied from outside must be a number in [0, 100]."), type = "error")
      return()
    }

    if (is.null(input$sterilisation_params) || !is.numeric(input$sterilisation_params) || input$sterilisation_params < 0 || input$sterilisation_params > 100) {
      shinyalert("Error", paste0("The sterilisation filtration efficacy must be a number in [0, 100]."), type = "error")
      return()
    }

    if (input$sterilisation_params == 0 && input$ventilation_air < 100) {
      shinyalert("Error", paste0("The fraction of air supplied from outside must be equals to 100 if no air filter is active."), type = "error")
      return()
    }

    ventilation <- switch(input$ventilation_params,
      "0 (no ventilation)" = 0,
      "0.3 (poorly ventilated)" = 0.3,
      "1 (domestic)" = 1,
      "3 (offices/schools)" = 3,
      "5 (well ventilated)" = 5,
      "10 (typical maximum)" = 10,
      "20 (hospital setting)" = 20,
      "Custom value" = input$ventilation_params_custom
    )

    new_data <- add_data(
      measure = "Ventilation",
      parameters = paste0("Ventilation: ", ventilation, "; Sterilisation: ", input$sterilisation_params, "; Air: ", input$ventilation_air),
      type = ifelse(input$ventilation_type != "Global", input$room_ventilation, "Global"),
      from = input$ventilation_time_from,
      to = input$ventilation_time_to,
      data = rooms_whatif
    )

    if (!is.null(new_data)) {
      canvasObjects$rooms_whatif <- new_data
    }
  })

  observeEvent(input$save_masks, {
    req(input$mask_fraction)
    req(input$mask_params)

    agents_whatif <- canvasObjects$agents_whatif

    if (input$mask_fraction > 1 || input$mask_fraction < 0) {
      shinyalert("Error", "Mask fraction must be  in [0, 1.]", type = "error")
      return()
    }
    if (as.integer(input$mask_time_to) < as.integer(input$mask_time_from) ||
      as.integer(input$mask_time_to) > as.numeric(canvasObjects$starting$simulation_days) ||
      as.integer(input$mask_time_from) <= 0) {
      shinyalert("Error", paste0("The timing should be greater than 0, less than the simulation days (", canvasObjects$starting$simulation_days, "), and 'to'>'from'. "), type = "error")
      return()
    }

    params <- paste0("Type: ", input$mask_params, "; Fraction: ", input$mask_fraction)

    new_data <- add_data(
      measure = "Mask",
      parameters = params,
      type = ifelse(input$mask_type != "Global", input$agent_mask, "Global"),
      from = input$mask_time_from,
      to = input$mask_time_to,
      data = agents_whatif
    )

    if (!is.null(new_data)) {
      canvasObjects$agents_whatif <- new_data
    }
  })
  observeEvent(input$save_vaccination, {
    agents_whatif <- canvasObjects$agents_whatif
    req(input$vaccination_fraction)
    req(input$vaccination_efficacy)

    if ((input$vaccination_efficacy) > 1 ||
      (input$vaccination_efficacy) < 0) {
      shinyalert("Error", "The efficacy should be in [0, 1].", type = "error")
      return()
    }
    if ((input$vaccination_fraction) > 1 ||
      (input$vaccination_fraction) < 0) {
      shinyalert("Error", "The fraction should be in [0, 1].", type = "error")
      return()
    }

    vaccination_coverage <- check_distribution_parameters(input, "vaccination_coverage")
    new_dist <- vaccination_coverage[[1]]
    new_time <- vaccination_coverage[[2]]

    if (is.null(new_time) && is.null(new_dist)) {
      shinyalert("Error", "Please, specify a value for the vaccination coverage.", "error")
      return()
    }

    if (new_dist == "Deterministic") {
      if (as.numeric(new_time) < 1) {
        shinyalert("Error", "The number of vaccine coverage days must be greater or equal (>=) 1.", type = "error")
        return()
      }
      paramstext <- paste0("Dist.Days: ", new_dist, ", ", new_time, ", 0")
    } else {
      params <- parse_distribution(new_time, new_dist)
      a <- params[[1]]
      b <- params[[2]]

      if (a < 1) {
        shinyalert("Error", "The number of vaccine coverage days must be greater or equal (>=) 1.", type = "error")
        return()
      }

      paramstext <- paste0("Dist.Days: ", new_dist, ", ", a, ", ", b)
    }

    params <- paste0("Efficacy: ", input$vaccination_efficacy, "; Fraction: ", input$vaccination_fraction, "; Coverage ", paramstext)

    new_data <- add_data(
      measure = "Vaccination",
      parameters = params,
      type = ifelse(input$vaccination_type != "Global", input$agent_vaccination, "Global"),
      from = input$vaccination_time_from,
      to = input$vaccination_time_from,
      data = agents_whatif
    )

    if (!is.null(new_data)) {
      canvasObjects$agents_whatif <- new_data
    }
  })
  observeEvent(input$save_swab, {
    agents_whatif <- canvasObjects$agents_whatif

    if (as.integer(input$swab_time_to) < as.integer(input$swab_time_from) ||
      as.integer(input$swab_time_to) > as.numeric(canvasObjects$starting$simulation_days) ||
      as.integer(input$swab_time_from) <= 0) {
      shinyalert("Error", paste0("The timing should be greater than 0, less than the simulation days (", canvasObjects$starting$simulation_days, "), and 'To' > 'From'."), type = "error")
      return()
    }

    paramstext <- paste0("Sensitivity: ", input$swab_sensitivity, "; Specificity: ", input$swab_specificity)

    new_dist <- "No swab"
    new_time <- 0
    if (input$swab_type_specific != "No swab") {
      swab_global <- check_distribution_parameters(input, "swab_days")
      new_dist <- swab_global[[1]]
      new_time <- swab_global[[2]]
    }

    if (is.null(new_time) && is.null(new_dist)) {
      shinyalert("Error", "Please, specify a value as the number of days.", "error")
      return()
    }

    if (new_dist == "Deterministic" || new_dist == "No swab") {
      paramstext <- paste0(paramstext, "; Dist: ", new_dist, ", ", new_time, ", 0")
    } else {
      params <- parse_distribution(new_time, new_dist)
      a <- params[[1]]
      b <- params[[2]]

      paramstext <- paste0(paramstext, "; Dist: ", new_dist, ", ", a, ", ", b)
    }


    new_data <- add_data(
      measure = "Swab",
      parameters = paramstext,
      type = ifelse(input$swab_type != "Global", input$agent_swab, "Global"),
      from = input$swab_time_from,
      to = input$swab_time_to,
      data = agents_whatif
    )

    if (!is.null(new_data)) {
      canvasObjects$agents_whatif <- new_data
    }
  })
  observeEvent(input$save_quarantine, {
    agents_whatif <- canvasObjects$agents_whatif

    req(input$quarantine_type != "No quarantine")

    if (!(input$quarantine_type == "Different for each agent" && input$quarantine_type_agent == "No quarantine")) {
      if (as.integer(input$quarantine_time_to) < as.integer(input$quarantine_time_from) ||
        as.integer(input$quarantine_time_to) > as.numeric(canvasObjects$starting$simulation_days) ||
        as.integer(input$quarantine_time_from) <= 0) {
        shinyalert("Error", paste0("The timing should be greater than 0, less than the simulation days (", canvasObjects$starting$simulation_days, "), and 'To' > 'From'."), type = "error")
        return()
      }

      quarantine_global <- check_distribution_parameters(input, "quarantine_global")
      new_dist <- quarantine_global[[1]]
      new_time <- quarantine_global[[2]]

      if (is.null(new_time) && is.null(new_dist)) {
        shinyalert("Error", "Please, specify a value as the number of days.", "error")
        return()
      }

      if (new_dist == "Deterministic") {
        if (as.numeric(new_time) < 1) {
          shinyalert("Error", "The number of quarantine days must be greater or equal (>=) 1.", type = "error")
          return()
        }

        paramstext <- paste0("Dist.Days: ", new_dist, ", ", new_time, ", 0")
      } else {
        params <- parse_distribution(new_time, new_dist)
        a <- params[[1]]
        b <- params[[2]]

        if (a < 1) {
          shinyalert("Error", "The number of quarantine days must be greater or equal (>=) 1.", type = "error")
          return()
        }

        paramstext <- paste0("Dist.Days: ", new_dist, ", ", a, ", ", b)
      }

      paramstext <- paste0(paramstext, "; Q.Room: ", input$room_quarantine)
      paramstext <- paste0(paramstext, "; Sensitivity: ", input$quarantine_swab_sensitivity, "; Specificity: ", input$quarantine_swab_specificity)

      new_dist <- "No swab"
      new_time <- 0

      if (input$quarantine_swab_type_global != "No swab") {
        # paramstext =  paste0(paramstext,"; Sensitivity: ",input$quarantine_swab_sensitivity,"; Specificity: ",input$quarantine_swab_specificity)

        quarantine_swab_global <- check_distribution_parameters(input, "quarantine_swab_global")
        new_dist <- quarantine_swab_global[[1]]
        new_time <- quarantine_swab_global[[2]]

        if (is.null(new_time) && is.null(new_dist)) {
          shinyalert("Error", "Please, specify a value as the number of days.", "error")
          return()
        }
      }

      if (new_dist == "Deterministic" || new_dist == "No swab") {
        paramstext <- paste0(paramstext, "; Dist: ", new_dist, ", ", new_time, ", 0")
      } else {
        params <- parse_distribution(new_time, new_dist)
        a <- params[[1]]
        b <- params[[2]]

        paramstext <- paste0(paramstext, "; Dist: ", new_dist, ", ", a, ", ", b)
      }
    } else {
      paramstext <- "No quarantine, 0, 0"
    }

    new_data <- add_data(
      measure = "Quarantine",
      parameters = paramstext,
      type = ifelse(input$quarantine_type != "Global", input$agent_quarantine, "Global"),
      from = input$quarantine_time_from,
      to = input$quarantine_time_to,
      data = agents_whatif
    )

    if (!is.null(new_data)) {
      canvasObjects$agents_whatif <- new_data
    }

    updateSelectizeInput(inputId = "room_quarantine_global", selected = "")
  })
  observeEvent(input$save_external_screening, {
    agents_whatif <- canvasObjects$agents_whatif

    if ((input$external_screening_second_global) > 1 || (input$external_screening_second_global) < 0) {
      shinyalert("Error", "External screening must be  in [0, 1].", type = "error")
      return()
    }
    if ((input$external_screening_first_global) > 1 || (input$external_screening_first_global) < 0) {
      shinyalert("Error", "External screening must be  in [0, 1].", type = "error")
      return()
    }

    if (as.integer(input$external_screening_time_to) < as.integer(input$external_screening_time_from) ||
      as.integer(input$external_screening_time_to) > as.numeric(canvasObjects$starting$simulation_days) ||
      as.integer(input$external_screening_time_from) <= 0) {
      shinyalert("Error", paste0("The timing should be greater than 0, less than the simulation days (", canvasObjects$starting$simulation_days, "), and 'To' > 'From'."), type = "error")
      return()
    }

    params <- paste0("First: ", input$external_screening_first_global, "; Second: ", input$external_screening_second_global)

    new_data <- add_data(
      measure = "External screening",
      parameters = params,
      type = ifelse(input$external_screening_type != "Global", input$agent_external_screening, "Global"),
      from = input$external_screening_time_from,
      to = input$external_screening_time_to,
      data = agents_whatif
    )

    if (!is.null(new_data)) {
      canvasObjects$agents_whatif <- new_data
    }
  })

  observeEvent(input$save_virus, {
    req(input$virus_variant)

    if ((input$virus_variant) < 0) {
      shinyalert("Error", "Virus variant must be > 0.", type = "error")
      return()
    }

    canvasObjects$virus_parameters$virus_variant <- input$virus_variant
  })
  observeEvent(input$save_initial_infected, {
    canvasObjects$initial_infected -> initial_infected

    if (is.na(as.integer(input$initial_infected_global)) || as.integer(input$initial_infected_global) < 0) {
      shinyalert("Error", "Initial infected must be a number greater or equal (>=) 0.", type = "error")
      return()
    }

    if (input$initial_infected_type == "Global") {
      if ("Global" %in% initial_infected$Type) {
        shinyalert("Error", "A 'Global' Initial infected is already defined. Please delete it by click on its row in the table.", type = "error")
        return()
      }
      total_agents <- 0
      for (a in 1:length(names(canvasObjects$agents))) {
        if (canvasObjects$agents[[a]]$entry_type == "Time window") {
          eet <- canvasObjects$agents[[a]]$EntryExitTime %>%
            select(Shift, NumAgent) %>%
            distinct()
          NumAgent <- sum(as.numeric(eet$NumAgent))
          if (as.integer(input$initial_infected_global) > NumAgent) {
            shinyalert("Error", paste0("Initial infected must be a number smaller or equal (<=) the number of agents (for the agent ", names(canvasObjects$agents)[a], " there are ", NumAgent, " agents)."), type = "error")
            return()
          }
        }
      }
    } else if (input$initial_infected_type == "Random") {
      if ("Random" %in% initial_infected$Type) {
        shinyalert("Error", "A 'Random' Initial infected is already defined. Please delete it by click on its row in the table.", type = "error")
        return()
      }
      total_agents <- 0
      for (a in 1:length(canvasObjects$agents)) {
        if (canvasObjects$agents[[a]]$entry_type == "Time window") {
          eet <- canvasObjects$agents[[a]]$EntryExitTime %>%
            select(Shift, NumAgent) %>%
            distinct()
          NumAgent <- sum(as.numeric(eet$NumAgent))
          total_agents <- total_agents + NumAgent
        }
      }

      if (as.integer(input$initial_infected_global) > total_agents) {
        shinyalert("Error", paste0("Initial infected must be a number smaller or equal (<=) the number of agents (", total_agents, ")."), type = "error")
        return()
      }
    } else {
      a <- input$agent_initial_infected
      if (canvasObjects$agents[[a]]$entry_type == "Time window") {
        eet <- canvasObjects$agents[[a]]$EntryExitTime %>%
          select(Shift, NumAgent) %>%
          distinct()
        NumAgent <- sum(as.numeric(eet$NumAgent))
        if (as.integer(input$initial_infected_global) > NumAgent) {
          shinyalert("Error", paste0("Initial infected must be a number smaller or equal (<=) the number of agents (for the agent ", names(canvasObjects$agents)[a], " there are ", NumAgent, " agents)."), type = "error")
          return()
        }
      }
    }

    new_row <- data.frame(
      Type = ifelse(input$initial_infected_type != "Different for each agent", input$initial_infected_type, input$agent_initial_infected),
      Number = input$initial_infected_global,
      stringsAsFactors = FALSE
    )

    canvasObjects$initial_infected <- rbind(initial_infected, new_row)
  })

  observe({
    disable("rds_generation")
    disable("flamegpu_connection")
    req(!is.null(canvasObjects$agents) && length(canvasObjects$agents) > 0)

    INITagents <- c()

    for (a in 1:length(canvasObjects$agents)) {
      if (!is.null(canvasObjects$agents[[a]]$entry_type)) {
        if (canvasObjects$agents[[a]]$entry_type == "Time window") {
          INITagents <- c(INITagents, names(canvasObjects$agents)[a])
        }
      }
    }

    updateSelectizeInput(session, inputId = "agent_initial_infected", choices = c("", INITagents))

    updateSelectizeInput(
      session = session, "agent_mask",
      choices = c("", names(canvasObjects$agents))
    )

    updateSelectizeInput(
      session = session, "agent_vaccination",
      choices = c("", names(canvasObjects$agents))
    )

    updateSelectizeInput(
      session = session, "agent_swab",
      choices = c("", names(canvasObjects$agents))
    )

    updateSelectizeInput(
      session = session, "agent_quarantine",
      choices = c("", names(canvasObjects$agents))
    )

    updateSelectizeInput(
      session = session, "agent_external_screening",
      choices = c("", names(canvasObjects$agents))
    )


    if (length(canvasObjects$roomsINcanvas) > 0) {
      rooms <- canvasObjects$roomsINcanvas %>% filter(type != "Fillingroom", type != "Stair")
      roomsAvailable <- c("", unique(paste0(rooms$type, "-", rooms$area)))

      updateSelectizeInput(
        session = session, "room_quarantine",
        choices = roomsAvailable
      )
    }
  })

  ########### Render the saved data table   ##########
  output$agents_whatif <- renderDT({
    if (!is.null(canvasObjects$agents_whatif)) {
      datatable(
        canvasObjects$agents_whatif %>% mutate(
          Measure = as.factor(Measure),
          Type = as.factor(Type),
          Parameters = as.factor(Parameters)
        ),
        filter = "top", selection = "single", rownames = FALSE, editable = TRUE,
        options = list(
          searching = TRUE, info = FALSE, paging = FALSE,
          sort = TRUE, scrollX = TRUE, scrollY = TRUE
        )
      )
    }
  })
  output$rooms_whatif <- renderDT({
    if (!is.null(canvasObjects$rooms_whatif)) {
      datatable(
        canvasObjects$rooms_whatif %>% mutate(
          Measure = as.factor(Measure),
          Type = as.factor(Type),
          Parameters = as.factor(Parameters)
        ),
        filter = "top", selection = "single", rownames = FALSE, editable = TRUE,
        options = list(
          searching = TRUE, info = FALSE, paging = FALSE,
          sort = TRUE, scrollX = TRUE, scrollY = TRUE
        )
      )
    }
  })

  output$virus_info <- renderDT({
    datatable(data.frame(Variant = canvasObjects$virus_parameters$virus_variant),
      options = list(
        searching = FALSE, info = FALSE, paging = FALSE,
        sort = TRUE, scrollX = TRUE, scrollY = TRUE
      )
    )
  })
  output$initialI_info <- renderDT({
    datatable(canvasObjects$initial_infected,
      options = list(
        searching = FALSE, info = FALSE, paging = FALSE,
        sort = TRUE, scrollX = TRUE, scrollY = TRUE
      )
    )
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
  observeEvent(input$LoadCSV_Button_OutsideContagion, {
    disable("rds_generation")
    disable("flamegpu_connection")

    isolate({
      if (is.null(input$OutsideContagionImport) || !file.exists(input$OutsideContagionImport$datapath) || !grepl(".csv", input$OutsideContagionImport$datapath)) {
        shinyalert("Error", "Please select one csv file.", "error")
        return()
      }

      dataframe <- read_csv(input$OutsideContagionImport$datapath)
      if (!"day" %in% names(dataframe) || !"percentage_infected" %in% names(dataframe)) {
        shinyalert("Error", "The csv mush have two columns: day and percentage_infected.", "error")
        return()
      }

      if (any(is.na(as.numeric(dataframe$day))) || any(is.na(as.numeric(dataframe$percentage_infected)))) {
        shinyalert("Error", "The two columns (day and percentage_infected) must contain only numbers.", "error")
        return()
      }

      if (input$population == "" || is.na(as.numeric(input$population))) {
        shinyalert("Error", "Population must be a number.", "error")
        return()
      }

      dataframe$day <- as.numeric(dataframe$day)
      dataframe$percentage_infected <- as.numeric(dataframe$percentage_infected)

      dataframe$percentage_infected <- dataframe$percentage_infected / as.numeric(input$population)

      if (any(dataframe$percentage_infected < 0) || any(dataframe$percentage_infected > 1)) {
        shinyalert("Error", "The percentage_infected column must contain numbers in [0, 1].", "error")
        return()
      }

      # Create a full sequence of days
      all_days <- data.frame(day = 1:canvasObjects$starting$simulation_days)

      dataframe_full <- merge(all_days, dataframe, by = "day", all.x = TRUE)
      dataframe_full$percentage_infected[is.na(dataframe_full$percentage_infected)] <- 0

      dataframe <- dataframe_full %>%
        group_by(day) %>%
        filter(percentage_infected == max(percentage_infected)) %>%
        ungroup() %>%
        filter(day >= 1)

      canvasObjects$outside_contagion <- dataframe %>%
        select(day, percentage_infected)

      output$outside_contagion_plot <- renderPlot({
        ggplot(dataframe) +
          geom_line(aes(x = day, y = percentage_infected), color = "green", linewidth = 1.5) +
          ylim(0, NA) +
          labs(title = "Outside contagion", x = "Day", y = "Percentage") +
          theme(title = element_text(size = 34), axis.title = element_text(size = 26), axis.text = element_text(size = 22)) +
          theme_fancy()
      })

      showElement("outside_contagion_plot")

      shinyalert("Success", "File loaded.", "success", 1000)
    })
  })

  observeEvent(input$initial_day, {
    canvasObjects$starting$day <- input$initial_day
  })

  initial_time <- debounce(reactive({
    input$initial_time
  }), 1000L)

  observeEvent(initial_time(), {
    disable("rds_generation")
    disable("flamegpu_connection")
    new_time <- input$initial_time

    if (!(grepl("^([01]?[0-9]|2[0-3]):[0-5][0-9]$", new_time) || grepl("^\\d{1,2}$", new_time))) {
      shinyalert("Error", "The format of the time should be: hh:mm (e.g. 06:15, or 20).", type = "error")
      return()
    }

    canvasObjects$starting$time <- new_time
  })

  simulation_days <- debounce(reactive({
    input$simulation_days
  }), 1000L)

  observeEvent(simulation_days(), {
    disable("rds_generation")
    disable("flamegpu_connection")
    simulation_days <- input$simulation_days

    if (simulation_days == "" || !grepl("(^[0-9]+).*", simulation_days) || simulation_days < 0) {
      shinyalert("Error", "You must specify a number greater than 0 (>= 0).", type = "error")
      return()
    }

    old_simulation_days <- canvasObjects$starting$simulation_days
    canvasObjects$starting$simulation_days <- simulation_days

    if (nrow(canvasObjects$agents_whatif) > 0) {
      for (i in 1:nrow(canvasObjects$agents_whatif)) {
        if (canvasObjects$agents_whatif[i, "To"] == old_simulation_days) {
          canvasObjects$agents_whatif[i, "To"] <- simulation_days
        }

        if (canvasObjects$agents_whatif[i, "From"] > simulation_days) {
          canvasObjects$agents_whatif[i, "From"] <- simulation_days
        }
      }
    }

    if (nrow(canvasObjects$rooms_whatif) > 0) {
      for (i in 1:nrow(canvasObjects$rooms_whatif)) {
        if (canvasObjects$rooms_whatif[i, "To"] == old_simulation_days) {
          canvasObjects$rooms_whatif[i, "To"] <- simulation_days
        }

        if (canvasObjects$rooms_whatif[i, "From"] == simulation_days) {
          canvasObjects$rooms_whatif[i, "From"] <- simulation_days
        }
      }
    }

    updateNumericInput(session = session, inputId = "ventilation_time_to", value = simulation_days)
    updateNumericInput(session = session, inputId = "mask_time_to", value = simulation_days)
    updateNumericInput(session = session, inputId = "swab_time_to", value = simulation_days)
    updateNumericInput(session = session, inputId = "quarantine_time_to", value = simulation_days)
    updateNumericInput(session = session, inputId = "external_screening_time_to", value = simulation_days)


    all_days <- data.frame(day = 1:canvasObjects$starting$simulation_days)

    if (!is.null(canvasObjects$outside_contagion)) {
      canvasObjects$outside_contagion <- merge(all_days, canvasObjects$outside_contagion, by = "day", all.x = TRUE)
      canvasObjects$outside_contagion$percentage_infected[is.na(canvasObjects$outside_contagion$percentage_infected)] <- 0

      output$outside_contagion_plot <- renderPlot({
        ggplot(canvasObjects$outside_contagion) +
          geom_line(aes(x = day, y = percentage_infected), color = "green", linewidth = 1.5) +
          ylim(0, NA) +
          labs(title = "Outside contagion", x = "Day", y = "Percentage") +
          theme(title = element_text(size = 34), axis.title = element_text(size = 26), axis.text = element_text(size = 22)) +
          theme_fancy()
      })
    }
  })

  seed <- debounce(reactive({
    input$seed
  }), 1000L)

  observeEvent(seed(), {
    disable("rds_generation")
    disable("flamegpu_connection")
    seed <- input$seed

    if (seed == "" || !grepl("(^[0-9]+).*", seed) || seed < 0) {
      shinyalert("Error", "You must specify a number greater then or equal to 0 (>= 0).", type = "error")
      return()
    }

    canvasObjects$starting$seed <- seed
  })

  observeEvent(input$step, {
    disable("rds_generation")
    disable("flamegpu_connection")
    canvasObjects$starting$step <- input$step
  })

  nrun <- debounce(reactive({
    input$nrun
  }), 1000L)

  observeEvent(nrun(), {
    disable("rds_generation")
    disable("flamegpu_connection")
    nrun <- input$nrun

    if (nrun == "" || !grepl("(^[0-9]+).*", nrun) || nrun <= 0) {
      shinyalert("Error", "You must specify a number greater than 0 (> 0).", type = "error")
      return()
    }

    canvasObjects$starting$nrun <- nrun
  })

  prun <- debounce(reactive({
    input$prun
  }), 1000L)

  observeEvent(nrun(), {
    disable("rds_generation")
    disable("flamegpu_connection")
    prun <- input$prun

    if (prun == "" || !grepl("(^[0-9]+).*", prun) || prun <= 0) {
      shinyalert("Error", "You must specify a number greater than 0 (> 0).", type = "error")
      return()
    }

    if (prun > canvasObjects$starting$nrun) {
      prun <- nrun
    }

    canvasObjects$starting$prun <- prun
  })


  #### START post processing #####

  postprocObjects <- reactiveValues(
    DirPath = NULL,
    Filter_evolutionCSV = NULL,
    AGENT_POSITION_AND_STATUS = NULL,
    CONTACTcsv = NULL,
    CONTACTmatrix = NULL,
    AEROSOLcsv = NULL,
    AEROSOL_std = NULL,
    COUNTERScsv = NULL,
    A_C_COUNTERS = NULL,
    Mapping = NULL,
    FLAGmodelLoaded = FALSE,
    MappingID_room = FALSE,
    Model = NULL,
    animation_bg = NULL
  )

  required_files <- c("AEROSOL.csv", "AGENT_POSITION_AND_STATUS.csv", "CONTACT.csv", "counters.csv")
  # Allow user to select a folder

  vols <- F4FgetVolumes(exclude = "")
  shinyDirChoose(input, "dir",
    roots = vols,
    session = session
  )

  # Get the selected folder path
  observeEvent(input$dir,
    {
      req(input$dir) # Ensure input$dir is not NULL
      if (!is.list(input$dir)) {
        return()
      } # Avoid accessing $path on an atomic vector

      # Ensure the user clicked "Select" and the path is not empty or NA
      dirPath <- parseDirPath(vols, input$dir)
      if (is.null(dirPath) || dirPath == "" || length(dirPath) == 0) {
        return() # Exit the event if no valid directory path is selected
      }

      postprocObjects$dirPath <- dirPath

      # check if any of the required file is missing, if yes stop
      missing_files <- sapply(required_files, function(f) {
        length(list.files(postprocObjects$dirPath,
          pattern = paste0("^", f, "$"),
          recursive = TRUE, full.names = TRUE
        )) == 0
      })

      if (any(missing_files)) {
        shinyalert(
          title = "Error",
          text = paste(
            "The following required files are missing (even in subfolders):\n",
            paste(names(missing_files)[missing_files], collapse = "\n")
          ),
          type = "error"
        )
        postprocObjects$dirPath <- NULL
        return()
      }


      output$dirPath <- renderText({
        dirPath
      })
    },
    ignoreInit = TRUE
  )

  observeEvent(input$LoadFolderPostProc_Button, {
    is_docker_compose <- Sys.getenv("DOCKER_COMPOSE") == "ON"
    if (is_docker_compose) {
      req(input$Folder_Selection_Compose_cell_clicked$value)
      dirname <- input$Folder_Selection_Compose_cell_clicked$value
    } else {
      dirname <- req(input$dir)
    }

    missing_files <- sapply(required_files, function(f) {
      length(list.files(postprocObjects$dirPath,
        pattern = paste0("^", f, "$"),
        recursive = TRUE, full.names = TRUE
      )) == 0
    })

    # check if any of the required file is missing, if yes stop
    if (any(missing_files)) {
      shinyalert(
        title = "Error",
        text = paste(
          "The following required files are missing (even in subfolders):\n",
          paste(names(missing_files)[missing_files], collapse = "\n")
        ),
        type = "error"
      )
      postprocObjects$dirPath <- NULL
      return()
    }

    if (is.null(canvasObjects$roomsINcanvas)) {
      shinyalert("Error", "The corresponding F4F model must loaded before inspecting the simulations.", "error")
      return()
    }

    if (!is.null(postprocObjects$dirPath)) {
      # to fix
      postprocObjects$FLAGmodelLoaded <- FALSE
    }

    if (is_docker_compose) {
      postprocObjects$dirPath <- paste0("/usr/local/lib/R/site-library/FORGE4FLAME/FLAMEGPU-FORGE4FLAME/results/", dirname)
    } else {
      postprocObjects$dirPath <- parseDirPath(roots = vols, dirname)
    }
  })


  # Check for required files in subfolders
  valid_subfolders <- reactive({
    dir <- req(postprocObjects$dirPath)
    subfolders <- list.dirs(dir, recursive = FALSE)
    valid <- sapply(subfolders, function(subfolder) {
      all(file.exists(file.path(subfolder, required_files)))
    })
    if (length(subfolders) != 0) {
      subfolders[valid]
    }
  })

  observe({
    dir <- req(postprocObjects$dirPath)
    show_modal_progress_line()

    # Evolution
    subfolders <- list.dirs(dir, recursive = FALSE)
    rooms_file <- paste0(dir, "/rooms_mapping.txt")
    if (!file.exists(rooms_file)) {
      shinyalert("Error", "The file rooms_mapping doesn't exists in the directory.", "error")
      postprocObjects$dirPath <- NULL
      remove_modal_progress()
      return()
    }

    model_file <- list.files(path = dir, pattern = "\\.RDs$", full.names = TRUE)
    if (length(model_file) > 0) {
      model_file <- model_file[1]
    } else {
      shinyalert("Error", "The RDs file of the model doesn't exists in the directory.", "error")
      postprocObjects$dirPath <- NULL
      remove_modal_progress()
      return()
    }
    postprocObjects$Model <- readRDS(model_file)

    isolate({
      G <- read_table(rooms_file, col_names = FALSE)
      colnames(G) <- c("ID", "x", "y", "z")

      roomsINcanvas <- req(canvasObjects$roomsINcanvas)
      floors <- req(canvasObjects$floors) %>%
        mutate(y = (Order - 1) * 10, CanvasID = Name)

      fillroomsINcanvas <- roomsINcanvas %>%
        filter(type == "Fillingroom") %>%
        mutate(z = y) %>%
        select(x, z, CanvasID, w, h) %>%
        left_join(floors, by = "CanvasID") %>%
        select(x, y, z, w, h) %>%
        mutate(x = x + ceiling(w / 2), z = z + ceiling(h / 2), ID = -1) %>%
        select(ID, x, y, z)

      G <- rbind(G, fillroomsINcanvas)

      postprocObjects$Mapping <- G

      #### read all the files
      read_and_process_csv <- function(file, col_names = NULL) {
        if (file.exists(file)) {
          f <- read_csv(file)
          if (!is.null(col_names)) {
            colnames(f) <- col_names
          }

          f$Folder <- basename(dirname(file))
          return(f)
        } else {
          shinyalert("Warning", sprintf("File '%s' not found. Skipping.", file), type = "warning")
          return()
        }
        return(NULL)
      }

      # List of files and column names
      file_info <- list(
        list(name = "AGENT_POSITION_AND_STATUS", file = "AGENT_POSITION_AND_STATUS.csv", cols = c("time", "id", "agent_type", "x", "y", "z", "disease_state", "room_id")),
        list(name = "COUNTERScsv", file = "counters.csv", cols = c("Day", "Agents births", "Agents deaths", "Agents in quarantine", "Number of swabs", "Number of agents infected \noutside the environment")),
        list(name = "AEROSOLcsv", file = "AEROSOL.csv", cols = c("time", "virus_concentration", "room_id")),
        list(name = "CONTACTcsv", file = "CONTACT.csv", cols = c("time", "agent_id1", "agent_id2", "room_id")),
        list(name = "CONTACTmatrix", file = "CONTACTS_MATRIX.csv", cols = c("time", "type1", "type2", "contacts"))
      )

      # Process files in parallel
      for (i in seq_along(file_info)) {
        csv_files <- file.path(subfolders, file_info[[i]]$file)

        data_list <- lapply(csv_files, read_and_process_csv, col_names = file_info[[i]]$cols)
        data_list <- Filter(Negate(is.null), data_list) # Remove NULLs

        if (length(data_list) == 0) {
          postprocObjects[[file_info[[i]]$name]] <- data.frame()
          update_modal_progress(i / length(file_info))
          next
        }

        postprocObjects[[file_info[[i]]$name]] <- bind_rows(data_list) %>% distinct()
        update_modal_progress(i / length(file_info))
      }
    })
    remove_modal_progress()
    shinyalert("Success", "Everything is loaded!", type = "success", 1000)
  })

  observe({
    req(postprocObjects$FLAGmodelLoaded)
    dir <- req(postprocObjects$dirPath)
    Mapping <- req(postprocObjects$Mapping)
    isolate({
      roomsINcanvas <- req(canvasObjects$roomsINcanvas)
      roomsINcanvas <- roomsINcanvas %>% mutate(coord = ifelse(type == "Fillingroom", paste0(x + ceiling(w / 2), "-", y + ceiling(h / 2), "-", CanvasID), paste0(center_x, "-", center_y, "-", CanvasID)))
      rooms_id <- roomsINcanvas$Name
      names(rooms_id) <- roomsINcanvas$coord

      Mapping <- Mapping %>% mutate(
        CanvasID = canvasObjects$floors$Name[(y / 10) + 1],
        coord = paste0(x, "-", z, "-", CanvasID),
        Name = rooms_id[coord]
      )

      Mapping <- merge(Mapping, roomsINcanvas %>% select(coord, type, area, Name))

      postprocObjects$MappingID_room <- merge(roomsINcanvas %>% select(-ID, -typeID),
        Mapping %>% select(-y, -coord) %>% rename(center_x = x, center_y = z),
        all.x = T
      )

      postprocObjects$Mapping <- Mapping %>% select(-coord, -x, -y, -z)
    })
  })

  #### query ####
  observe({
    CONTACTcsv <- req(postprocObjects$CONTACTcsv)
    CONTACTmatrix <- req(postprocObjects$CONTACTmatrix)
    AEROSOLcsv <- req(postprocObjects$AEROSOLcsv)
    req(postprocObjects$FLAGmodelLoaded)
    req(postprocObjects$MappingID_room)
    show_modal_spinner(text = "We are preparing everything.")

    isolate({
      dir <- req(postprocObjects$dirPath)
      roomsINcanvas <- req(canvasObjects$roomsINcanvas)
      #### read all the areosol and contact ####
      subfolders <- list.dirs(dir, recursive = FALSE)
      step <- as.numeric(postprocObjects$Model$starting$step)

      AEROSOLcsv$time <- as.numeric(AEROSOLcsv$time)

      Mapping <- req(postprocObjects$Mapping)

      postprocObjects$AEROSOL_std <- merge(Mapping, AEROSOLcsv, by.x = "ID", by.y = "room_id")

      postprocObjects$CONTACT_std <- merge(Mapping, CONTACTcsv, by.x = "ID", by.y = "room_id")

      # CONTACTcsv =  merge(Mapping , CONTACTcsv, by.x = "ID", by.y = "room_id" )


      agent_with_time_window <- Filter(function(x) x$entry_type == "Time window", canvasObjects$agents)
      agent_with_daily_rate <- Filter(function(x) x$entry_type == "Daily Rate", canvasObjects$agents)
      canvasObjects$agents <- c(agent_with_time_window, agent_with_daily_rate)
      agents <- names(canvasObjects$agents)
      CONTACTcsv$agent_id1 <- agents[CONTACTcsv$agent_id1 + 1]
      CONTACTcsv$agent_id2 <- agents[CONTACTcsv$agent_id2 + 1]

      postprocObjects$CONTACT_std <- postprocObjects$CONTACT_std %>%
        arrange(CanvasID, Folder, area, type, agent_id1, agent_id2, time) %>%
        group_by(CanvasID, Folder, area, type, agent_id1, agent_id2) %>%
        mutate(time_diff = time - lag(time, default = first(time))) %>%
        filter(time_diff != 1) %>%
        ungroup() %>%
        select(-time_diff)

      CONTACTmatrix$type1 <- agents[CONTACTmatrix$type1 + 1]
      CONTACTmatrix$type2 <- agents[CONTACTmatrix$type2 + 1]


      postprocObjects$CONTACTmatrix <- CONTACTmatrix %>%
        group_by(type2, type1, Folder) %>%
        summarise(
          Mean = mean(contacts),
          Sd = sd(contacts)
        )

      # Count the number of unique meetings per hour
      C_COUNTERS <- postprocObjects$CONTACT_std %>%
        mutate(hour = ceiling((time * step) / (60 * 60))) %>% # Convert time to hourly bins
        group_by(CanvasID, Name, area, type, Folder, hour, ID) %>%
        summarise(contact_counts = n())

      A_COUNTERS <- postprocObjects$AEROSOL_std %>%
        mutate(hour = ceiling((time * step) / (60 * 60))) %>%
        group_by(CanvasID, Name, area, type, Folder, hour, ID) %>%
        summarize(virus_concentration = mean(virus_concentration))

      A_C_COUNTERS <- merge(C_COUNTERS, A_COUNTERS, all = T)

      A_C_COUNTERS[is.na(A_C_COUNTERS)] <- 0
      postprocObjects$A_C_COUNTERS <- A_C_COUNTERS

      rooms <- unique(paste(A_C_COUNTERS$CanvasID, " ; ", A_C_COUNTERS$area, " ; ", A_C_COUNTERS$Name, " ;  ID ", A_C_COUNTERS$ID))
      updateSelectInput(
        session = session, inputId = "Room_Counters_A_C_selectize",
        choices = c("", rooms), selected = ""
      )

      #####
      postprocObjects$FLAGmodelLoaded <- FALSE

      # Set default values for 2D visualization: show average cumulative aerosol
      updateSelectInput(session, "visualColor_select", selected = "CumulAerosol")
      updateCheckboxInput(session, "visualShowAverage", value = TRUE)

      # Initialize slider with data max time
      if (!is.null(postprocObjects$AEROSOL_std) && nrow(postprocObjects$AEROSOL_std) > 0) {
        maxTime <- max(postprocObjects$AEROSOL_std$time, na.rm = TRUE)
        if (maxTime > 0) {
          updateNumericInput("animationStep", session = session, value = step, max = maxTime * step)
          updateSliderInput("animation",
            session = session,
            max = maxTime * step, min = 0,
            value = 0, step = step
          )

          floors <- canvasObjects$floors
          if (!is.null(floors)) {
            updateSelectInput("visualFloor_select",
              session = session,
              choices = c("All", unique(floors$Name))
            )
          }
        }
      }
    })

    remove_modal_spinner()
    showElement("DownloadPostProc_Button")
  })

  observe({
    pl <- NULL
    info <- input$PostProc_table_cell_clicked
    folderselected <- req(info$value)

    isolate({
      CONTACTmatrix <- req(postprocObjects$CONTACTmatrix)
      c <- CONTACTmatrix %>% filter(Folder == folderselected)
      agent_with_time_window <- Filter(function(x) x$entry_type == "Time window", canvasObjects$agents)
      agent_with_daily_rate <- Filter(function(x) x$entry_type == "Daily Rate", canvasObjects$agents)
      canvasObjects$agents <- c(agent_with_time_window, agent_with_daily_rate)
      agents <- names(canvasObjects$agents)

      c$type1 <- factor(c$type1, levels = agents)
      c$type2 <- factor(c$type2, levels = agents)

      pl <- ggplot(c, aes(x = type1, y = type2, fill = Mean)) +
        geom_tile() +
        scale_fill_gradient(low = "green", high = "red") +
        theme_bw() +
        labs(
          title = "",
          x = "",
          y = "",
          fill = "Mean number of contact\n per hour"
        ) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text = element_text(size = 16),
          axis.title = element_text(size = 20, face = "bold"),
          plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
          legend.text = element_text(size = 18),
          legend.key.size = unit(1.5, "cm"),
          legend.title = element_text(face = "bold", size = 18),
          legend.position = "bottom",
          strip.text = element_text(size = 18, face = "bold")
        )
    })
    output$ContactMatrix_plot <- renderPlot({
      pl
    })
  })

  output$PostProc_filters <- renderUI({
    df <- req(postprocObjects$evolutionCSV)
    show_modal_spinner()

    # i want to add the slider about the agent
    name_cols <- colnames(df %>% select(-Folder, -agent_type))
    sliders <- lapply(name_cols, function(col) {
      values <- unique(df[[col]])
      if (col == "Day") values <- values[-c(length(values))]
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
    df <- req(postprocObjects$evolutionCSV)
    name_cols <- colnames(df %>% select(-Folder))

    for (col in name_cols) {
      input_id <- paste0("filter_", col)
      if (!is.null(input[[input_id]])) {
        df <- df[df[[col]] >= input[[input_id]][1] & df[[col]] <= input[[input_id]][2], ]
      }
    }
    postprocObjects$Filter_evolutionCSV <- df
  })

  observe({
    df <- req(postprocObjects$Filter_evolutionCSV)
    folders <- unique(df$Folder)

    output$PostProc_table <- DT::renderDataTable({
      DT::datatable(
        data.frame(FolderNames = paste(folders)),
        options = list(
          pageLength = 5
        ),
        editable = list(target = "cell"),
        selection = "single",
        rownames = F
      )
    })
  })

  ##### Processing AGENT_POSITION_AND_STATUS

  observe({
    AGENT_POSITION_AND_STATUS <- req(postprocObjects$AGENT_POSITION_AND_STATUS)
    canvasObjects <- req(canvasObjects)

    isolate({
      floors <- canvasObjects$floors %>%
        arrange(Order) %>%
        rename(CanvasID = Name)

      Nfloors <- length(floors$CanvasID)
      simulation_log <- AGENT_POSITION_AND_STATUS %>%
        select(time, id, agent_type, x, y, z, room_id, disease_state, Folder) %>%
        filter(y %in% seq(0, 10 * (Nfloors - 1), by = 10) | y == 10000)

      floors$y <- seq(0, 10 * (Nfloors - 1), by = 10)
      # simulation_log %>% filter(y != 10000) %>% select(y)  %>% distinct() %>% arrange()

      simulation_log <- merge(simulation_log, floors %>% select(-ID), all.x = TRUE) %>%
        mutate(time = as.numeric(time)) %>%
        filter(!is.na(time))

      simulation_log <- simulation_log %>%
        group_by(id, Folder) %>%
        arrange(time) %>%
        # tidyr::complete(time = tidyr::full_seq(time, 1)) %>%
        tidyr::fill(agent_type, x, y, z, room_id, CanvasID, Order, disease_state, Folder, .direction = "down") %>%
        ungroup()

      # add agent names to the simulation log!
      if (!is.null(names(canvasObjects$agents))) {
        agent_with_time_window <- Filter(function(x) x$entry_type == "Time window", canvasObjects$agents)
        agent_with_daily_rate <- Filter(function(x) x$entry_type == "Daily Rate", canvasObjects$agents)
        canvasObjects$agents <- c(agent_with_time_window, agent_with_daily_rate)
        simulation_log <- simulation_log %>% mutate(agent_type = names(canvasObjects$agents)[agent_type + 1])
      }

      simulation_log <- simulation_log %>%
        mutate(
          disease_state = c("S", "E", "I", "R", "D")[disease_state + 1],
          CanvasID = ifelse(y == 10000, "Outside", CanvasID)
        )


      # Create evolutionCSV from simulation_log for filtering and download
      postprocObjects$evolutionCSV <- simulation_log %>%
        select(time, disease_state, Folder, agent_type) %>%
        distinct() %>%
        group_by(Folder, time, disease_state, agent_type) %>%
        summarise(Number = n(), .groups = "drop") %>%
        tidyr::pivot_wider(names_from = disease_state, values_from = Number, values_fill = 0) %>%
        ungroup() %>%
        arrange(Folder, time) %>%
        rename(Day = time)

      # Store full simulation log for enhanced disease evolution plot
      postprocObjects$simulation_log_full <- simulation_log
    })
  })

  ##### Disease State Evolution - Enhanced Visualization #####

  # Update filter choices when simulation_log is available
  observe({
    simulation_log <- req(postprocObjects$simulation_log_full)
    Mapping <- req(postprocObjects$Mapping)

    isolate({
      # Get unique agent types
      agent_types <- unique(simulation_log$agent_type)
      agent_types <- agent_types[!is.na(agent_types)]
      updateSelectizeInput(session, "diseaseEvol_agentType",
        choices = c("All", sort(agent_types)),
        selected = "All"
      )

      # Create composite room identifier (Name - type - area) for unique room selection
      Mapping_with_id <- Mapping %>%
        mutate(RoomID = paste0(Name, " (", type, " - ", area, ")"))

      # Get unique rooms with composite identifier
      room_choices <- unique(Mapping_with_id$RoomID)
      room_choices <- room_choices[!is.na(room_choices)]
      updateSelectizeInput(session, "diseaseEvol_room",
        choices = c("All", sort(room_choices)),
        selected = "All"
      )

      # Get unique floors
      floors_list <- unique(simulation_log$CanvasID)
      floors_list <- floors_list[!is.na(floors_list)]
      updateSelectizeInput(session, "diseaseEvol_floor",
        choices = c("All", sort(floors_list)),
        selected = "All"
      )

      # Get disease states based on model
      disease_model <- canvasObjects$disease[[1]]$disease_model_name
      if (is.null(disease_model)) disease_model <- "SIR"
      states <- strsplit(disease_model, "")[[1]]
      state_names <- c("S" = "Susceptible", "E" = "Exposed", "I" = "Infected", "R" = "Recovered", "D" = "Died")
      state_choices <- setNames(states, state_names[states])
      updateSelectizeInput(session, "diseaseEvol_states",
        choices = c("All", state_choices),
        selected = "All"
      )

      # Get unique simulations (folders)
      folders <- unique(simulation_log$Folder)
      folders <- folders[!is.na(folders)]
      n_sims <- length(folders)
      updateSelectizeInput(session, "diseaseEvol_simulation",
        choices = c("All (Aggregate)" = "All", setNames(folders, paste0("Sim: ", folders))),
        selected = "All"
      )
    })
  })

  # Reset filters button
  observeEvent(input$diseaseEvol_reset, {
    updateSelectizeInput(session, "diseaseEvol_agentType", selected = "All")
    updateSelectizeInput(session, "diseaseEvol_room", selected = "All")
    updateSelectizeInput(session, "diseaseEvol_floor", selected = "All")
    updateSelectizeInput(session, "diseaseEvol_states", selected = "All")
    updateSelectizeInput(session, "diseaseEvol_simulation", selected = "All")
    updateSelectInput(session, "diseaseEvol_granularity", selected = "day")
    updateSelectInput(session, "diseaseEvol_measureType", selected = "all_states")
    updateRadioButtons(session, "diseaseEvol_plotType", selected = "line")
    updateRadioButtons(session, "diseaseEvol_aggregateMode", selected = "mean_sd")
    updateCheckboxGroupInput(session, "diseaseEvol_options", selected = "legend")
    updateCheckboxInput(session, "diseaseEvol_normalize", value = FALSE)
    updateCheckboxInput(session, "diseaseEvol_facetAgent", value = FALSE)
    updateCheckboxInput(session, "diseaseEvol_facetState", value = FALSE)
    updateCheckboxInput(session, "diseaseEvol_showRibbon", value = TRUE)
    updateSliderInput(session, "diseaseEvol_alpha", value = 0.3)
  })

  # Observer to enable/disable aggregation mode based on number of simulations
  observe({
    simulation_log <- postprocObjects$simulation_log_full
    if (is.null(simulation_log)) {
      return()
    }

    # Count unique folders (simulations)
    folders <- unique(simulation_log$Folder)
    n_sims <- length(folders[!is.na(folders)])

    # If only one simulation, disable aggregation options (force individual mode)
    if (n_sims <= 1) {
      shinyjs::disable("diseaseEvol_aggregateMode")
      shinyjs::disable("diseaseEvol_showRibbon")
      shinyjs::disable("diseaseEvol_alpha")
      updateRadioButtons(session, "diseaseEvol_aggregateMode", selected = "individual")
    } else {
      shinyjs::enable("diseaseEvol_aggregateMode")
      shinyjs::enable("diseaseEvol_showRibbon")
      shinyjs::enable("diseaseEvol_alpha")
    }
  })

  # Reactive to prepare filtered data for disease evolution plot
  diseaseEvol_data <- reactive({
    simulation_log <- req(postprocObjects$simulation_log_full)
    Mapping <- req(postprocObjects$Mapping)

    # Get the step size in seconds from starting settings
    step_seconds <- as.numeric(canvasObjects$starting$step)
    if (is.null(step_seconds) || is.na(step_seconds)) step_seconds <- 60 # default to 60 seconds

    # Get simulation days to calculate the complete time range
    simulation_days <- as.numeric(canvasObjects$starting$simulation_days)
    if (is.null(simulation_days) || is.na(simulation_days)) simulation_days <- 30 # default to 30 days

    # Calculate total simulation time in seconds
    total_simulation_seconds <- simulation_days * 86400 # days to seconds

    # Parse starting time from canvasObjects$starting$time (format "HH:MM" or "HH")
    starting_time_str <- canvasObjects$starting$time
    if (is.null(starting_time_str) || is.na(starting_time_str)) starting_time_str <- "00:00"

    # Parse starting time to get offset in seconds from midnight
    if (grepl(":", starting_time_str)) {
      time_parts <- strsplit(starting_time_str, ":")[[1]]
      starting_hour <- as.numeric(time_parts[1])
      starting_minute <- as.numeric(time_parts[2])
    } else {
      starting_hour <- as.numeric(starting_time_str)
      starting_minute <- 0
    }
    starting_offset_seconds <- starting_hour * 3600 + starting_minute * 60

    # Check if we need a specific folder or all
    selected_sims <- input$diseaseEvol_simulation

    # Use simulation_log_full which has the raw time column
    if ("All" %in% selected_sims || is.null(selected_sims) || length(selected_sims) == 0) {
      sim_data <- simulation_log
    } else {
      sim_data <- simulation_log %>%
        filter(Folder %in% selected_sims)
    }

    # If room filter is applied, we need to join with room names
    # Track if multiple rooms are selected for faceting
    multiple_rooms_selected <- FALSE

    if (!"All" %in% input$diseaseEvol_room && length(input$diseaseEvol_room) > 0) {
      multiple_rooms_selected <- length(input$diseaseEvol_room) > 1

      # Create composite room identifier in Mapping to match the selected values
      Mapping_with_id <- Mapping %>%
        mutate(RoomID = paste0(Name, " (", type, " - ", area, ")"))

      # Filter Mapping to get the selected rooms
      selected_rooms <- Mapping_with_id %>%
        filter(RoomID %in% input$diseaseEvol_room)

      # Join simulation data with Mapping to get room info
      # First merge to get room_id match, keeping RoomID for faceting
      sim_data <- sim_data %>%
        inner_join(
          selected_rooms %>% select(ID, RoomID, Name, type, area),
          by = c("room_id" = "ID")
        )
    }

    # Apply agent type filter
    if (!"All" %in% input$diseaseEvol_agentType && length(input$diseaseEvol_agentType) > 0) {
      sim_data <- sim_data %>% filter(agent_type %in% input$diseaseEvol_agentType)
    }

    # Apply floor filter
    if (!"All" %in% input$diseaseEvol_floor && length(input$diseaseEvol_floor) > 0) {
      sim_data <- sim_data %>% filter(CanvasID %in% input$diseaseEvol_floor)
    }

    # Apply disease state filter
    if (!"All" %in% input$diseaseEvol_states && length(input$diseaseEvol_states) > 0) {
      sim_data <- sim_data %>% filter(disease_state %in% input$diseaseEvol_states)
    }

    # Get the selected granularity
    granularity <- input$diseaseEvol_granularity

    # Calculate time column based on selected granularity
    # time column represents the step number, so we need to convert:
    # actual_seconds = time * step_seconds
    sim_data <- sim_data %>%
      mutate(
        time_granular = case_when(
          granularity == "step" ~ time,
          granularity == "minute" ~ floor((time * step_seconds) / 60),
          granularity == "hour" ~ floor((time * step_seconds) / 3600),
          granularity == "day" ~ floor((time * step_seconds) / 86400),
          granularity == "week" ~ floor((time * step_seconds) / 604800),
          granularity == "month" ~ floor((time * step_seconds) / 2592000), # ~30 days
          TRUE ~ time
        )
      )

    # Calculate max_time_granular based on simulation_days and granularity
    max_time_granular <- switch(granularity,
      "step"   = floor(total_simulation_seconds / step_seconds),
      "minute" = floor(total_simulation_seconds / 60),
      "hour"   = floor(total_simulation_seconds / 3600),
      "day"    = simulation_days,
      "week"   = floor(simulation_days / 7),
      "month"  = floor(simulation_days / 30),
      simulation_days
    )

    # Calculate min_time_granular based on starting time for step/minute/hour granularities
    min_time_granular <- switch(granularity,
      "step"   = floor(starting_offset_seconds / step_seconds),
      "minute" = floor(starting_offset_seconds / 60),
      "hour"   = floor(starting_offset_seconds / 3600),
      "day"    = 0,
      "week"   = 0,
      "month"  = 0,
      0
    )

    # Get measure type selection
    measure_type <- input$diseaseEvol_measureType
    if (is.null(measure_type)) measure_type <- "all_states"

    # Check if RoomID column exists (multiple rooms selected)
    has_room <- "RoomID" %in% names(sim_data) && multiple_rooms_selected

    # Get all disease states for complete expansion
    disease_model <- canvasObjects$disease[[1]]$disease_model_name
    if (is.null(disease_model)) disease_model <- "SIR"
    all_disease_states <- strsplit(disease_model, "")[[1]]

    # Aggregate based on granularity, measure type, and room
    if (input$diseaseEvol_facetAgent && !"All" %in% input$diseaseEvol_agentType) {
      # With agent type faceting
      if (measure_type == "final_state") {
        # Final State: Take only the last time point within each granular period for each agent
        if (has_room) {
          agg_data <- sim_data %>%
            select(Folder, time, time_granular, id, disease_state, agent_type, RoomID) %>%
            group_by(Folder, time_granular, id, RoomID) %>%
            filter(time == max(time)) %>%
            ungroup() %>%
            select(-time) %>%
            group_by(Folder, time_granular, disease_state, agent_type, RoomID) %>%
            summarise(Count = n(), .groups = "drop")
        } else {
          agg_data <- sim_data %>%
            select(Folder, time, time_granular, id, disease_state, agent_type) %>%
            group_by(Folder, time_granular, id) %>%
            filter(time == max(time)) %>%
            ungroup() %>%
            select(-time) %>%
            group_by(Folder, time_granular, disease_state, agent_type) %>%
            summarise(Count = n(), .groups = "drop")
        }
      } else if (measure_type == "state_changes") {
        # State Changes: Count only agents that changed their disease state within each period
        # First, identify agents whose state changed within each time_granular period
        if (has_room) {
          state_changes <- sim_data %>%
            select(Folder, time, time_granular, id, disease_state, agent_type, RoomID) %>%
            arrange(Folder, id, time) %>%
            group_by(Folder, time_granular, id, RoomID) %>%
            mutate(prev_state = lag(disease_state, default = first(disease_state))) %>%
            filter(disease_state != prev_state) %>%
            ungroup()

          # Count transitions TO each state (the new state after the change)
          agg_data <- state_changes %>%
            group_by(Folder, time_granular, disease_state, agent_type, RoomID) %>%
            summarise(Count = n(), .groups = "drop")
        } else {
          state_changes <- sim_data %>%
            select(Folder, time, time_granular, id, disease_state, agent_type) %>%
            arrange(Folder, id, time) %>%
            group_by(Folder, time_granular, id) %>%
            mutate(prev_state = lag(disease_state, default = first(disease_state))) %>%
            filter(disease_state != prev_state) %>%
            ungroup()

          # Count transitions TO each state (the new state after the change)
          agg_data <- state_changes %>%
            group_by(Folder, time_granular, disease_state, agent_type) %>%
            summarise(Count = n(), .groups = "drop")
        }
      } else {
        # All States: Count all unique agent-state combinations within each granular period
        if (has_room) {
          agg_data <- sim_data %>%
            select(Folder, time_granular, id, disease_state, agent_type, RoomID) %>%
            distinct() %>%
            group_by(Folder, time_granular, disease_state, agent_type, RoomID) %>%
            summarise(Count = n(), .groups = "drop")
        } else {
          agg_data <- sim_data %>%
            select(Folder, time_granular, id, disease_state, agent_type) %>%
            distinct() %>%
            group_by(Folder, time_granular, disease_state, agent_type) %>%
            summarise(Count = n(), .groups = "drop")
        }
      }

      # Complete the time range from 0 to max_time_granular for all combinations
      # Get unique folders and agent types
      folders <- unique(agg_data$Folder)
      agent_types_in_data <- unique(agg_data$agent_type)

      if (has_room) {
        rooms_in_data <- unique(agg_data$RoomID)
        complete_grid <- expand.grid(
          Folder = folders,
          time_granular = min_time_granular:max_time_granular,
          disease_state = all_disease_states,
          agent_type = agent_types_in_data,
          RoomID = rooms_in_data,
          stringsAsFactors = FALSE
        )
      } else {
        complete_grid <- expand.grid(
          Folder = folders,
          time_granular = min_time_granular:max_time_granular,
          disease_state = all_disease_states,
          agent_type = agent_types_in_data,
          stringsAsFactors = FALSE
        )
      }

      agg_data <- complete_grid %>%
        left_join(agg_data, by = names(complete_grid)) %>%
        mutate(Count = ifelse(is.na(Count), 0, Count))
    } else {
      # Without agent type faceting
      if (measure_type == "final_state") {
        # Final State: Take only the last time point within each granular period for each agent
        if (has_room) {
          agg_data <- sim_data %>%
            select(Folder, time, time_granular, id, disease_state, RoomID) %>%
            group_by(Folder, time_granular, id, RoomID) %>%
            filter(time == max(time)) %>%
            ungroup() %>%
            select(-time) %>%
            group_by(Folder, time_granular, disease_state, RoomID) %>%
            summarise(Count = n(), .groups = "drop")
        } else {
          agg_data <- sim_data %>%
            select(Folder, time, time_granular, id, disease_state) %>%
            group_by(Folder, time_granular, id) %>%
            filter(time == max(time)) %>%
            ungroup() %>%
            select(-time) %>%
            group_by(Folder, time_granular, disease_state) %>%
            summarise(Count = n(), .groups = "drop")
        }
      } else if (measure_type == "state_changes") {
        # State Changes: Count only agents that changed their disease state within each period
        if (has_room) {
          state_changes <- sim_data %>%
            select(Folder, time, time_granular, id, disease_state, RoomID) %>%
            arrange(Folder, id, time) %>%
            group_by(Folder, time_granular, id, RoomID) %>%
            mutate(prev_state = lag(disease_state, default = first(disease_state))) %>%
            filter(disease_state != prev_state) %>%
            ungroup()

          # Count transitions TO each state (the new state after the change)
          agg_data <- state_changes %>%
            group_by(Folder, time_granular, disease_state, RoomID) %>%
            summarise(Count = n(), .groups = "drop")
        } else {
          state_changes <- sim_data %>%
            select(Folder, time, time_granular, id, disease_state) %>%
            arrange(Folder, id, time) %>%
            group_by(Folder, time_granular, id) %>%
            mutate(prev_state = lag(disease_state, default = first(disease_state))) %>%
            filter(disease_state != prev_state) %>%
            ungroup()

          # Count transitions TO each state (the new state after the change)
          agg_data <- state_changes %>%
            group_by(Folder, time_granular, disease_state) %>%
            summarise(Count = n(), .groups = "drop")
        }
      } else {
        # All States: Count all unique agent-state combinations within each granular period
        if (has_room) {
          agg_data <- sim_data %>%
            select(Folder, time_granular, id, disease_state, RoomID) %>%
            distinct() %>%
            group_by(Folder, time_granular, disease_state, RoomID) %>%
            summarise(Count = n(), .groups = "drop")
        } else {
          agg_data <- sim_data %>%
            select(Folder, time_granular, id, disease_state) %>%
            distinct() %>%
            group_by(Folder, time_granular, disease_state) %>%
            summarise(Count = n(), .groups = "drop")
        }
      }

      # Complete the time range from 0 to max_time_granular for all combinations
      # Get unique folders
      folders <- unique(agg_data$Folder)

      if (has_room) {
        rooms_in_data <- unique(agg_data$RoomID)
        complete_grid <- expand.grid(
          Folder = folders,
          time_granular = min_time_granular:max_time_granular,
          disease_state = all_disease_states,
          RoomID = rooms_in_data,
          stringsAsFactors = FALSE
        )
      } else {
        complete_grid <- expand.grid(
          Folder = folders,
          time_granular = min_time_granular:max_time_granular,
          disease_state = all_disease_states,
          stringsAsFactors = FALSE
        )
      }

      agg_data <- complete_grid %>%
        left_join(agg_data, by = names(complete_grid)) %>%
        mutate(Count = ifelse(is.na(Count), 0, Count))
    }


    # Normalize if requested (per simulation and room if applicable)
    if (input$diseaseEvol_normalize) {
      if (input$diseaseEvol_facetAgent && !"All" %in% input$diseaseEvol_agentType) {
        if (has_room) {
          agg_data <- agg_data %>%
            group_by(Folder, time_granular, agent_type, RoomID) %>%
            mutate(Count = Count / sum(Count) * 100) %>%
            ungroup()
        } else {
          agg_data <- agg_data %>%
            group_by(Folder, time_granular, agent_type) %>%
            mutate(Count = Count / sum(Count) * 100) %>%
            ungroup()
        }
      } else {
        if (has_room) {
          agg_data <- agg_data %>%
            group_by(Folder, time_granular, RoomID) %>%
            mutate(Count = Count / sum(Count) * 100) %>%
            ungroup()
        } else {
          agg_data <- agg_data %>%
            group_by(Folder, time_granular) %>%
            mutate(Count = Count / sum(Count) * 100) %>%
            ungroup()
        }
      }
    }

    # Store has_room flag in the data for later use in faceting
    attr(agg_data, "has_room") <- has_room

    # Convert disease_state to factor with proper ordering based on disease model
    disease_model <- canvasObjects$disease[[1]]$disease_model_name
    if (is.null(disease_model)) disease_model <- "SIR"

    # Define the canonical order of disease states
    all_states_order <- c("S", "E", "I", "R", "D")
    # Get the states present in the disease model (in order)
    model_states <- strsplit(disease_model, "")[[1]]
    # Keep only states that are in the canonical order
    state_levels <- all_states_order[all_states_order %in% model_states]

    # Convert disease_state to factor with proper levels
    agg_data <- agg_data %>%
      mutate(disease_state = factor(disease_state, levels = state_levels))

    agg_data
  })

  # Helper function to render contacts/aerosol plot
  renderContactsAerosolPlot <- function(metric_type) {
    granularity <- input$diseaseEvol_granularity
    aggregate_mode <- input$diseaseEvol_aggregateMode
    show_ribbon <- input$diseaseEvol_showRibbon
    ribbon_alpha <- input$diseaseEvol_alpha
    plot_type <- input$diseaseEvol_plotType
    options <- input$diseaseEvol_options
    facet_room <- if (metric_type == "aerosol") input$diseaseEvol_facetRoom else FALSE
    if (is.null(facet_room)) facet_room <- FALSE
    cumulative <- input$diseaseEvol_cumulative
    if (is.null(cumulative)) cumulative <- FALSE

    # Get the appropriate data
    if (metric_type == "contacts") {
      df_raw <- postprocObjects$CONTACT_std
      y_label <- if (cumulative) "Cumulative Number of Contacts" else "Number of Contacts"
      title_text <- if (cumulative) "Cumulative Contacts Evolution" else "Contacts Evolution"
      metric_color <- "#E5D05AFF"
    } else {
      df_raw <- postprocObjects$AEROSOL_std
      y_label <- if (cumulative) expression(paste("Cumulative Virus Concentration (", PFU / m^3, ")")) else expression(paste("Virus Concentration (", PFU / m^3, ")"))
      title_text <- if (cumulative) "Cumulative Aerosol Concentration Evolution" else "Aerosol Concentration Evolution"
      metric_color <- "#3498db"
    }

    if (is.null(df_raw) || nrow(df_raw) == 0) {
      return(
        ggplot() +
          annotate("text",
            x = 0.5, y = 0.5, label = paste0("No ", metric_type, " data available.\nPlease load simulation data first."),
            size = 6, color = "white"
          ) +
          theme_void() +
          theme(plot.background = element_rect(fill = "#2b2b2b", color = NA))
      )
    }

    # Create RoomID column for aerosol data
    if (metric_type == "aerosol") {
      df_raw <- df_raw %>%
        mutate(RoomID = paste0(Name, " (", type, " - ", area, ")"))
    }

    # Get simulation filter
    sim_filter <- input$diseaseEvol_simulation
    if (!"All" %in% sim_filter && length(sim_filter) > 0) {
      df_raw <- df_raw %>% filter(Folder %in% sim_filter)
    }

    # Get room/floor filters
    room_filter <- input$diseaseEvol_room
    floor_filter <- input$diseaseEvol_floor

    if (!is.null(room_filter) && !"All" %in% room_filter) {
      if (metric_type == "contacts") {
        df_raw <- df_raw %>%
          mutate(RoomID = paste0(Name, " (", type, " - ", area, ")")) %>%
          filter(RoomID %in% room_filter)
      } else {
        df_raw <- df_raw %>% filter(RoomID %in% room_filter)
      }
    }

    if (!is.null(floor_filter) && !"All" %in% floor_filter) {
      df_raw <- df_raw %>% filter(CanvasID %in% floor_filter)
    }

    if (nrow(df_raw) == 0) {
      return(
        ggplot() +
          annotate("text",
            x = 0.5, y = 0.5, label = "No data matches the current filters.",
            size = 6, color = "white"
          ) +
          theme_void() +
          theme(plot.background = element_rect(fill = "#2b2b2b", color = NA))
      )
    }

    # Calculate time granularity
    step <- as.numeric(postprocObjects$Model$starting$step)
    if (is.null(step) || is.na(step)) step <- 60

    # Get simulation days to calculate the complete time range
    simulation_days <- as.numeric(postprocObjects$Model$starting$simulation_days)
    if (is.null(simulation_days) || is.na(simulation_days)) simulation_days <- 30

    # Calculate total simulation time in seconds
    total_simulation_seconds <- simulation_days * 86400

    # Parse starting time from canvasObjects$starting$time (format "HH:MM" or "HH")
    starting_time_str <- postprocObjects$Model$starting$time
    if (is.null(starting_time_str) || is.na(starting_time_str)) starting_time_str <- "00:00"

    # Parse starting time to get offset in seconds from midnight
    if (grepl(":", starting_time_str)) {
      time_parts <- strsplit(starting_time_str, ":")[[1]]
      starting_hour <- as.numeric(time_parts[1])
      starting_minute <- as.numeric(time_parts[2])
    } else {
      starting_hour <- as.numeric(starting_time_str)
      starting_minute <- 0
    }
    starting_offset_seconds <- starting_hour * 3600 + starting_minute * 60

    # Calculate min and max time_granular based on starting time and granularity
    min_time_granular <- switch(granularity,
      "step"   = floor(starting_offset_seconds / step),
      "minute" = floor(starting_offset_seconds / 60),
      "hour"   = floor(starting_offset_seconds / 3600),
      "day"    = 0,
      "week"   = 0,
      "month"  = 0,
      0
    )

    max_time_granular <- switch(granularity,
      "step"   = floor(total_simulation_seconds / step),
      "minute" = floor(total_simulation_seconds / 60),
      "hour"   = floor(total_simulation_seconds / 3600),
      "day"    = simulation_days,
      "week"   = floor(simulation_days / 7),
      "month"  = floor(simulation_days / 30),
      simulation_days
    )

    # Aggregate data based on metric type
    if (metric_type == "contacts") {
      # Count contacts per time unit
      df <- df_raw %>%
        mutate(time_granular = case_when(
          granularity == "step" ~ time,
          granularity == "minute" ~ floor(time * step / 60),
          granularity == "hour" ~ floor(time * step / 3600),
          granularity == "day" ~ floor(time * step / 86400),
          granularity == "week" ~ floor(time * step / 604800),
          granularity == "month" ~ floor(time * step / 2592000),
          TRUE ~ time
        )) %>%
        group_by(Folder, time_granular) %>%
        summarise(Value = n(), .groups = "drop")

      # Complete the time range from min_time_granular to max_time_granular
      folders <- unique(df$Folder)
      complete_grid <- expand.grid(
        Folder = folders,
        time_granular = min_time_granular:max_time_granular,
        stringsAsFactors = FALSE
      )
      df <- complete_grid %>%
        left_join(df, by = c("Folder", "time_granular")) %>%
        mutate(Value = ifelse(is.na(Value), 0, Value))

      # Apply cumulative calculation if enabled
      if (cumulative) {
        df <- df %>%
          arrange(Folder, time_granular) %>%
          group_by(Folder) %>%
          mutate(Value = cumsum(Value)) %>%
          ungroup()
      }
    } else {
      # Aggregate aerosol concentration - group by RoomID if faceting or always to maintain room separation
      df <- df_raw %>%
        mutate(time_granular = case_when(
          granularity == "step" ~ time,
          granularity == "minute" ~ floor(time * step / 60),
          granularity == "hour" ~ floor(time * step / 3600),
          granularity == "day" ~ floor(time * step / 86400),
          granularity == "week" ~ floor(time * step / 604800),
          granularity == "month" ~ floor(time * step / 2592000),
          TRUE ~ time
        ))

      if (facet_room) {
        # Group by RoomID to keep rooms separate
        df <- df %>%
          group_by(Folder, time_granular, RoomID) %>%
          summarise(Value = sum(virus_concentration, na.rm = TRUE), .groups = "drop")

        # Complete the time range from min_time_granular to max_time_granular
        folders <- unique(df$Folder)
        rooms_in_data <- unique(df$RoomID)
        complete_grid <- expand.grid(
          Folder = folders,
          time_granular = min_time_granular:max_time_granular,
          RoomID = rooms_in_data,
          stringsAsFactors = FALSE
        )
        df <- complete_grid %>%
          left_join(df, by = c("Folder", "time_granular", "RoomID")) %>%
          mutate(Value = ifelse(is.na(Value), 0, Value))

        # Apply cumulative calculation if enabled
        if (cumulative) {
          df <- df %>%
            arrange(Folder, RoomID, time_granular) %>%
            group_by(Folder, RoomID) %>%
            mutate(Value = cumsum(Value)) %>%
            ungroup()
        }
      } else {
        # Aggregate across all rooms
        df <- df %>%
          group_by(Folder, time_granular) %>%
          summarise(Value = sum(virus_concentration, na.rm = TRUE), .groups = "drop")

        # Complete the time range from min_time_granular to max_time_granular
        folders <- unique(df$Folder)
        complete_grid <- expand.grid(
          Folder = folders,
          time_granular = min_time_granular:max_time_granular,
          stringsAsFactors = FALSE
        )
        df <- complete_grid %>%
          left_join(df, by = c("Folder", "time_granular")) %>%
          mutate(Value = ifelse(is.na(Value), 0, Value))

        # Apply cumulative calculation if enabled
        if (cumulative) {
          df <- df %>%
            arrange(Folder, time_granular) %>%
            group_by(Folder) %>%
            mutate(Value = cumsum(Value)) %>%
            ungroup()
        }
      }
    }

    n_sims <- length(unique(df$Folder))

    # Create x-axis label based on granularity
    x_label <- switch(granularity,
      "step" = "Time (Steps)",
      "minute" = "Time (Minutes)",
      "hour" = "Time (Hours)",
      "day" = "Time (Days)",
      "week" = "Time (Weeks)",
      "month" = "Time (Months)",
      "Time"
    )

    # Build the plot
    if (n_sims > 1 && aggregate_mode != "individual") {
      # Aggregate statistics across simulations
      if (facet_room && "RoomID" %in% names(df)) {
        agg_stats <- df %>%
          group_by(time_granular, RoomID) %>%
          summarise(
            Mean = mean(Value, na.rm = TRUE),
            SD = sd(Value, na.rm = TRUE),
            SE = SD / sqrt(n()),
            Min = min(Value, na.rm = TRUE),
            Max = max(Value, na.rm = TRUE),
            CI_lower = Mean - 1.96 * SE,
            CI_upper = Mean + 1.96 * SE,
            .groups = "drop"
          )
      } else {
        agg_stats <- df %>%
          group_by(time_granular) %>%
          summarise(
            Mean = mean(Value, na.rm = TRUE),
            SD = sd(Value, na.rm = TRUE),
            SE = SD / sqrt(n()),
            Min = min(Value, na.rm = TRUE),
            Max = max(Value, na.rm = TRUE),
            CI_lower = Mean - 1.96 * SE,
            CI_upper = Mean + 1.96 * SE,
            .groups = "drop"
          )
      }

      if (facet_room && "RoomID" %in% names(agg_stats)) {
        p <- ggplot(agg_stats, aes(x = time_granular, color = RoomID, fill = RoomID))
      } else {
        p <- ggplot(agg_stats, aes(x = time_granular))
      }

      # Add ribbon based on aggregate mode
      if (show_ribbon && plot_type == "line") {
        if (aggregate_mode == "mean_sd") {
          if (facet_room && "RoomID" %in% names(agg_stats)) {
            p <- p + geom_ribbon(aes(ymin = pmax(0, Mean - SD), ymax = Mean + SD, group = RoomID),
              alpha = ribbon_alpha, color = NA
            )
          } else {
            p <- p + geom_ribbon(aes(ymin = pmax(0, Mean - SD), ymax = Mean + SD),
              fill = metric_color, alpha = ribbon_alpha
            )
          }
        } else if (aggregate_mode == "mean_ci") {
          if (facet_room && "RoomID" %in% names(agg_stats)) {
            p <- p + geom_ribbon(aes(ymin = pmax(0, CI_lower), ymax = CI_upper, group = RoomID),
              alpha = ribbon_alpha, color = NA
            )
          } else {
            p <- p + geom_ribbon(aes(ymin = pmax(0, CI_lower), ymax = CI_upper),
              fill = metric_color, alpha = ribbon_alpha
            )
          }
        } else if (aggregate_mode == "minmax") {
          if (facet_room && "RoomID" %in% names(agg_stats)) {
            p <- p + geom_ribbon(aes(ymin = Min, ymax = Max, group = RoomID),
              alpha = ribbon_alpha, color = NA
            )
          } else {
            p <- p + geom_ribbon(aes(ymin = Min, ymax = Max),
              fill = metric_color, alpha = ribbon_alpha
            )
          }
        }
      }

      # Calculate ymin and ymax for bar plot error bars
      if (aggregate_mode == "mean_sd") {
        agg_stats <- agg_stats %>%
          mutate(ymin = pmax(0, Mean - SD), ymax = Mean + SD)
      } else if (aggregate_mode == "mean_ci") {
        agg_stats <- agg_stats %>%
          mutate(ymin = pmax(0, CI_lower), ymax = CI_upper)
      } else if (aggregate_mode == "minmax") {
        agg_stats <- agg_stats %>%
          mutate(ymin = Min, ymax = Max)
      }

      # Add line or bar plot
      if (plot_type == "line") {
        if (facet_room && "RoomID" %in% names(agg_stats)) {
          p <- p + geom_line(aes(y = Mean, group = RoomID), linewidth = 1.2)
          if ("points" %in% options) {
            p <- p + geom_point(aes(y = Mean), size = 2)
          }
        } else {
          p <- p + geom_line(aes(y = Mean), color = metric_color, linewidth = 1.2)
          if ("points" %in% options) {
            p <- p + geom_point(aes(y = Mean), color = metric_color, size = 2)
          }
        }
      } else if (plot_type == "bar") {
        if (facet_room && "RoomID" %in% names(agg_stats)) {
          p <- p + geom_col(aes(y = Mean), position = "dodge", alpha = 0.8)
          if (show_ribbon) {
            p <- p + geom_errorbar(aes(ymin = ymin, ymax = ymax),
              position = position_dodge(width = 0.9),
              width = 0.25, alpha = 0.7
            )
          }
        } else {
          p <- p + geom_col(aes(y = Mean), fill = metric_color, alpha = 0.8)
          if (show_ribbon) {
            p <- p + geom_errorbar(aes(ymin = ymin, ymax = ymax),
              color = metric_color,
              width = 0.25, alpha = 0.7
            )
          }
        }
      }

      title_text <- paste0(title_text, " (", n_sims, " simulations)")
    } else {
      # Individual lines for each simulation
      if (facet_room && "RoomID" %in% names(df)) {
        p <- ggplot(df, aes(x = time_granular, y = Value, color = RoomID, fill = RoomID, group = interaction(Folder, RoomID)))
        if (plot_type == "line") {
          p <- p + geom_line(linewidth = 0.8, alpha = 0.7)
          if ("points" %in% options) {
            p <- p + geom_point(size = 1.5, alpha = 0.7)
          }
        } else if (plot_type == "bar") {
          p <- p + geom_col(position = "dodge", alpha = 0.7)
        }
      } else {
        p <- ggplot(df, aes(x = time_granular, y = Value, group = Folder))
        if (n_sims > 1) {
          if (plot_type == "line") {
            p <- p + geom_line(aes(color = Folder), linewidth = 0.8, alpha = 0.7)
            if ("points" %in% options) {
              p <- p + geom_point(aes(color = Folder), size = 1.5, alpha = 0.7)
            }
          } else if (plot_type == "bar") {
            p <- p + geom_col(aes(fill = Folder), position = "dodge", alpha = 0.7)
          }
        } else {
          if (plot_type == "line") {
            p <- p + geom_line(color = metric_color, linewidth = 1.2)
            if ("points" %in% options) {
              p <- p + geom_point(color = metric_color, size = 2)
            }
          } else if (plot_type == "bar") {
            p <- p + geom_col(fill = metric_color, alpha = 0.8)
          }
          title_text <- paste0(title_text, " (", unique(df$Folder), ")")
        }
      }
    }

    # Apply faceting if enabled
    if (facet_room && "RoomID" %in% names(df)) {
      p <- p + facet_wrap(~RoomID, scales = "free_y")
    }

    # Apply theme and labels
    p <- p +
      labs(title = title_text, x = x_label, y = y_label) +
      theme_fancy()

    # Handle legend visibility
    if (!"legend" %in% options) {
      p <- p + theme(legend.position = "none")
    }

    p
  }

  # Render the Disease State Evolution plot
  output$DiseaseStateEvolutionPlot <- renderPlotly({
    metric <- input$diseaseEvol_metric
    if (is.null(metric)) metric <- "disease_states"

    # Handle different metrics
    if (metric %in% c("contacts", "aerosol")) {
      # Use contacts/aerosol data
      return(plotly::ggplotly(renderContactsAerosolPlot(metric)))
    }

    # Default: disease states
    df <- tryCatch(diseaseEvol_data(), error = function(e) NULL)

    if (is.null(df) || nrow(df) == 0) {
      return(
        plotly::ggplotly(ggplot() +
          annotate("text",
            x = 0.5, y = 0.5, label = "Please wait for simulation data to load.\nFilters will be populated automatically.",
            size = 6, color = "white"
          ) +
          theme_void() +
          theme(plot.background = element_rect(fill = "#2b2b2b", color = NA)))
      )
    }

    granularity <- input$diseaseEvol_granularity
    plot_type <- input$diseaseEvol_plotType
    options <- input$diseaseEvol_options
    aggregate_mode <- input$diseaseEvol_aggregateMode
    show_ribbon <- input$diseaseEvol_showRibbon
    ribbon_alpha <- input$diseaseEvol_alpha

    # Get disease model colors
    disease_model <- canvasObjects$disease[[1]]$disease_model_name
    if (is.null(disease_model)) disease_model <- "SIR"

    # Enhanced color palette for disease states
    state_colors <- c("S" = "#2ecc71", "E" = "#3498db", "I" = "#e74c3c", "R" = "#9b59b6", "D" = "#34495e")
    state_labels <- c("S" = "Susceptible", "E" = "Exposed", "I" = "Infected", "R" = "Recovered", "D" = "Died")

    # Get the factor levels from the data (already properly ordered)
    present_states <- levels(df$disease_state)
    # Filter to only states that have data
    present_states <- present_states[present_states %in% unique(as.character(df$disease_state))]
    state_colors <- state_colors[present_states]
    state_labels <- state_labels[present_states]

    # Create x-axis label based on granularity
    x_label <- switch(granularity,
      "step" = "Time (Steps)",
      "minute" = "Time (Minutes)",
      "hour" = "Time (Hours)",
      "day" = "Time (Days)",
      "week" = "Time (Weeks)",
      "month" = "Time (Months)",
      "Time"
    )

    y_label <- if (input$diseaseEvol_normalize) "Percentage (%)" else "Number of Agents"

    # Check number of simulations
    n_sims <- length(unique(df$Folder))

    # Get measure type for subtitle
    measure_type <- input$diseaseEvol_measureType
    if (is.null(measure_type)) measure_type <- "all_states"
    measure_label <- switch(measure_type,
      "final_state" = "Final State in Period",
      "state_changes" = "State Changes in Period",
      "All States in Period"
    )

    # Determine plot title
    if (n_sims > 1) {
      title_text <- paste0("Disease State Evolution (", n_sims, " simulations)")
    } else {
      title_text <- paste0("Disease State Evolution (", unique(df$Folder), ")")
    }

    # Prepare data based on aggregation mode
    if (n_sims > 1 && aggregate_mode != "individual") {
      # Aggregate across simulations
      # Check if room (RoomID) is in the data
      has_room_in_df <- "RoomID" %in% names(df)

      if (input$diseaseEvol_facetAgent && "agent_type" %in% names(df)) {
        if (has_room_in_df) {
          agg_stats <- df %>%
            group_by(time_granular, disease_state, agent_type, RoomID) %>%
            summarise(
              Mean = mean(Count, na.rm = TRUE),
              SD = sd(Count, na.rm = TRUE),
              SE = SD / sqrt(n()),
              Min = min(Count, na.rm = TRUE),
              Max = max(Count, na.rm = TRUE),
              CI_lower = Mean - 1.96 * SE,
              CI_upper = Mean + 1.96 * SE,
              n_sims = n(),
              .groups = "drop"
            )
        } else {
          agg_stats <- df %>%
            group_by(time_granular, disease_state, agent_type) %>%
            summarise(
              Mean = mean(Count, na.rm = TRUE),
              SD = sd(Count, na.rm = TRUE),
              SE = SD / sqrt(n()),
              Min = min(Count, na.rm = TRUE),
              Max = max(Count, na.rm = TRUE),
              CI_lower = Mean - 1.96 * SE,
              CI_upper = Mean + 1.96 * SE,
              n_sims = n(),
              .groups = "drop"
            )
        }
      } else {
        if (has_room_in_df) {
          agg_stats <- df %>%
            group_by(time_granular, disease_state, RoomID) %>%
            summarise(
              Mean = mean(Count, na.rm = TRUE),
              SD = sd(Count, na.rm = TRUE),
              SE = SD / sqrt(n()),
              Min = min(Count, na.rm = TRUE),
              Max = max(Count, na.rm = TRUE),
              CI_lower = Mean - 1.96 * SE,
              CI_upper = Mean + 1.96 * SE,
              n_sims = n(),
              .groups = "drop"
            )
        } else {
          agg_stats <- df %>%
            group_by(time_granular, disease_state) %>%
            summarise(
              Mean = mean(Count, na.rm = TRUE),
              SD = sd(Count, na.rm = TRUE),
              SE = SD / sqrt(n()),
              Min = min(Count, na.rm = TRUE),
              Max = max(Count, na.rm = TRUE),
              CI_lower = Mean - 1.96 * SE,
              CI_upper = Mean + 1.96 * SE,
              n_sims = n(),
              .groups = "drop"
            )
        }
      }

      # Handle NA values
      agg_stats <- agg_stats %>%
        mutate(
          SD = ifelse(is.na(SD), 0, SD),
          SE = ifelse(is.na(SE), 0, SE),
          CI_lower = ifelse(is.na(CI_lower), Mean, CI_lower),
          CI_upper = ifelse(is.na(CI_upper), Mean, CI_upper)
        )

      # Calculate ribbon bounds based on mode
      if (aggregate_mode == "mean_sd") {
        agg_stats <- agg_stats %>%
          mutate(
            ymin = pmax(Mean - SD, 0),
            ymax = Mean + SD
          )
        subtitle_text <- paste0(measure_label, " | Mean ± Standard Deviation")
      } else if (aggregate_mode == "mean_ci") {
        agg_stats <- agg_stats %>%
          mutate(
            ymin = pmax(CI_lower, 0),
            ymax = CI_upper
          )
        subtitle_text <- paste0(measure_label, " | Mean ± 95% Confidence Interval")
      } else if (aggregate_mode == "minmax") {
        agg_stats <- agg_stats %>%
          mutate(
            ymin = Min,
            ymax = Max
          )
        subtitle_text <- paste0(measure_label, " | Min/Max Range")
      }

      # Base plot with aggregated data
      pl <- ggplot(agg_stats, aes(x = time_granular, color = disease_state, fill = disease_state))

      # Add ribbon for uncertainty
      if (show_ribbon && plot_type == "line") {
        pl <- pl + geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = ribbon_alpha, color = NA)
      }

      # Add plot type
      if (plot_type == "line") {
        pl <- pl + geom_line(aes(y = Mean), linewidth = 1.2)
        if ("points" %in% options) {
          pl <- pl + geom_point(aes(y = Mean), size = 2.5)
        }
      } else if (plot_type == "bar") {
        pl <- pl + geom_col(aes(y = Mean), position = "dodge", alpha = 0.8)
        if (show_ribbon) {
          pl <- pl + geom_errorbar(aes(ymin = ymin, ymax = ymax),
            position = position_dodge(width = 0.9),
            width = 0.25, alpha = 0.7
          )
        }
      }
    } else {
      # Individual simulations or single simulation
      subtitle_text <- if (n_sims > 1) {
        paste0(measure_label, " | Individual Simulation Trajectories")
      } else {
        measure_label
      }

      pl <- ggplot(df, aes(x = time_granular, y = Count, color = disease_state, fill = disease_state))

      if (n_sims > 1) {
        # Multiple individual lines
        pl <- pl + aes(group = interaction(disease_state, Folder))

        if (plot_type == "line") {
          pl <- pl + geom_line(aes(linetype = Folder), linewidth = 0.8, alpha = 0.7)
          if ("points" %in% options) {
            pl <- pl + geom_point(size = 1.5, alpha = 0.7)
          }
        } else if (plot_type == "bar") {
          pl <- pl + geom_col(aes(group = Folder), position = position_dodge2(preserve = "single"), alpha = 0.6)
        }
      } else {
        # Single simulation
        if (plot_type == "line") {
          pl <- pl + geom_line(linewidth = 1.2)
          if ("points" %in% options) {
            pl <- pl + geom_point(size = 2.5)
          }
        } else if (plot_type == "bar") {
          pl <- pl + geom_col(position = "dodge", alpha = 0.8)
        }
      }
    }

    # Apply colors and labels
    pl <- pl +
      scale_color_manual(values = state_colors, labels = state_labels, name = "Disease State") +
      scale_fill_manual(values = state_colors, labels = state_labels, name = "Disease State") +
      labs(x = x_label, y = y_label, title = title_text, subtitle = subtitle_text) +
      theme_fancy()

    # Handle legend visibility
    if (!"legend" %in% options) {
      pl <- pl + theme(legend.position = "none")
    }

    # Faceting options
    facet_agent <- input$diseaseEvol_facetAgent && "agent_type" %in% names(df)
    facet_state <- input$diseaseEvol_facetState
    facet_room <- "RoomID" %in% names(df) # Automatic faceting when multiple rooms selected

    # Build facet formula based on active facets
    if (facet_room) {
      if (facet_agent && facet_state) {
        # Facet by room, agent type, and disease state
        pl <- pl + facet_grid(disease_state ~ RoomID + agent_type, scales = "free_y")
      } else if (facet_agent) {
        # Facet by room and agent type
        pl <- pl + facet_grid(RoomID ~ agent_type, scales = "free_y")
      } else if (facet_state) {
        # Facet by room and disease state
        pl <- pl + facet_grid(disease_state ~ RoomID, scales = "free_y")
      } else {
        # Facet by room only (default when multiple rooms selected)
        pl <- pl + facet_wrap(~RoomID, scales = "free_y")
      }
    } else {
      if (facet_agent && facet_state) {
        # Facet by both agent type and disease state
        pl <- pl + facet_grid(disease_state ~ agent_type, scales = "free_y")
      } else if (facet_agent) {
        # Facet by agent type only
        pl <- pl + facet_wrap(~agent_type, scales = "free_y")
      } else if (facet_state) {
        # Facet by disease state only
        pl <- pl + facet_wrap(~disease_state, scales = "free_y")
      }
    }

    # Add day boundary markers when granularity is "hour"
    if (granularity == "hour") {
      # Get simulation parameters
      simulation_days <- as.numeric(canvasObjects$starting$simulation_days)
      if (is.null(simulation_days) || is.na(simulation_days)) simulation_days <- 30

      # Parse starting time to get the starting hour
      starting_time_str <- canvasObjects$starting$time
      if (is.null(starting_time_str) || is.na(starting_time_str)) starting_time_str <- "00:00"

      if (grepl(":", starting_time_str)) {
        time_parts <- strsplit(starting_time_str, ":")[[1]]
        starting_hour <- as.numeric(time_parts[1])
      } else {
        starting_hour <- as.numeric(starting_time_str)
      }

      # Calculate min and max hours in the data
      min_hour <- min(df$time_granular, na.rm = TRUE)
      max_hour <- max(df$time_granular, na.rm = TRUE)

      # Calculate hour positions where days change (every 24 hours from midnight)
      # First midnight after start: if starting_hour > 0, first midnight is at hour (24 - starting_hour) from start
      # In absolute terms: starting_hour is the first hour, so midnight (hour 0/24) occurs at position:
      # starting_hour + X = 24 => X = 24 - starting_hour (first midnight)
      # Then every 24 hours after that

      first_midnight_hour <- if (starting_hour == 0) 24 else (24 - starting_hour)

      # Generate all midnight positions within the data range
      day_boundaries <- seq(
        from = min_hour + first_midnight_hour,
        to = max_hour,
        by = 24
      )

      # Filter to only include boundaries within the actual data range
      day_boundaries <- day_boundaries[day_boundaries > min_hour & day_boundaries <= max_hour]

      if (length(day_boundaries) > 0) {
        # Create labels for each day boundary
        day_labels <- paste0("Day ", seq_along(day_boundaries) + 1)

        # Add vertical lines at day boundaries
        pl <- pl +
          geom_vline(
            xintercept = day_boundaries,
            linetype = "dashed",
            color = "white",
            alpha = 0.5,
            linewidth = 0.5
          ) +
          annotate("text",
            x = day_boundaries,
            y = Inf,
            label = day_labels,
            vjust = 1.5,
            hjust = 0.5,
            color = "white",
            size = 3,
            alpha = 0.7
          )
      }
    }

    plotly::ggplotly(pl)
  })

  # Render summary statistics table
  output$DiseaseStateSummaryTable <- DT::renderDataTable({
    df <- tryCatch(diseaseEvol_data(), error = function(e) NULL)

    if (is.null(df) || nrow(df) == 0) {
      return(DT::datatable(data.frame(Message = "No data available")))
    }

    granularity <- input$diseaseEvol_granularity
    n_sims <- length(unique(df$Folder))
    aggregate_mode <- input$diseaseEvol_aggregateMode

    # Map state letters to full names
    state_labels <- c("S" = "Susceptible", "E" = "Exposed", "I" = "Infected", "R" = "Recovered", "D" = "Died")

    # Calculate summary statistics
    if (n_sims > 1 && aggregate_mode != "individual") {
      # Multi-simulation summary - aggregate across simulations first, then summarize
      if (input$diseaseEvol_facetAgent && "agent_type" %in% names(df)) {
        # First get per-simulation totals
        sim_totals <- df %>%
          group_by(Folder, disease_state, agent_type) %>%
          summarise(Total = sum(Count, na.rm = TRUE), .groups = "drop")

        summary_df <- sim_totals %>%
          group_by(disease_state, agent_type) %>%
          summarise(
            `N Simulations` = n(),
            `Mean (across sims)` = round(mean(Total, na.rm = TRUE), 2),
            `SD (across sims)` = round(sd(Total, na.rm = TRUE), 2),
            `Min (across sims)` = round(min(Total, na.rm = TRUE), 2),
            `Max (across sims)` = round(max(Total, na.rm = TRUE), 2),
            .groups = "drop"
          ) %>%
          rename(`Disease State` = disease_state, `Agent Type` = agent_type)
      } else {
        # First get per-simulation totals
        sim_totals <- df %>%
          group_by(Folder, disease_state) %>%
          summarise(Total = sum(Count, na.rm = TRUE), .groups = "drop")

        summary_df <- sim_totals %>%
          group_by(disease_state) %>%
          summarise(
            `N Simulations` = n(),
            `Mean (across sims)` = round(mean(Total, na.rm = TRUE), 2),
            `SD (across sims)` = round(sd(Total, na.rm = TRUE), 2),
            `Min (across sims)` = round(min(Total, na.rm = TRUE), 2),
            `Max (across sims)` = round(max(Total, na.rm = TRUE), 2),
            .groups = "drop"
          ) %>%
          rename(`Disease State` = disease_state)
      }
    } else {
      # Single simulation or individual mode - per time point statistics
      if (input$diseaseEvol_facetAgent && "agent_type" %in% names(df)) {
        summary_df <- df %>%
          group_by(disease_state, agent_type) %>%
          summarise(
            `Mean (per time unit)` = round(mean(Count, na.rm = TRUE), 2),
            `SD (per time unit)` = round(sd(Count, na.rm = TRUE), 2),
            `Min` = min(Count, na.rm = TRUE),
            `Max` = max(Count, na.rm = TRUE),
            `Total` = sum(Count, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          rename(`Disease State` = disease_state, `Agent Type` = agent_type)
      } else {
        summary_df <- df %>%
          group_by(disease_state) %>%
          summarise(
            `Mean (per time unit)` = round(mean(Count, na.rm = TRUE), 2),
            `SD (per time unit)` = round(sd(Count, na.rm = TRUE), 2),
            `Min` = min(Count, na.rm = TRUE),
            `Max` = max(Count, na.rm = TRUE),
            `Total` = sum(Count, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          rename(`Disease State` = disease_state)
      }
    }

    # Map state letters to full names
    summary_df$`Disease State` <- state_labels[summary_df$`Disease State`]

    DT::datatable(summary_df,
      options = list(pageLength = 10, dom = "t", scrollX = TRUE),
      rownames = FALSE
    ) %>%
      DT::formatStyle(columns = 1:ncol(summary_df), fontSize = "14px")
  })

  counters_colorsNames <- c(
    "Agents birth", "Agents deaths", "Agents in quarantine",
    "Number of swabs", "Number of agents infected \noutside the environment"
  )
  counters_colors <- viridisLite::turbo(n = length(counters_colorsNames))
  names(counters_colors) <- counters_colorsNames

  observe({
    info <- input$PostProc_table_cell_clicked
    folder <- req(info$value)

    CountersDisease_radioButt <- input$CountersDisease_radioButt
    df <- req(postprocObjects$COUNTERScsv)

    df <- df %>%
      filter(Folder == folder) %>%
      select(-Folder) %>%
      tidyr::gather(-Day, value = "Number", key = "Counters")

    pl <- ggplot()
    if (!is.null(CountersDisease_radioButt)) {
      DfStat <- postprocObjects$COUNTERScsv %>%
        tidyr::gather(-Day, -Folder, value = "Number", key = "Counters") %>%
        group_by(Day, Counters) %>%
        summarise(
          Mean = mean(Number),
          MinV = min(Number),
          MaxV = max(Number)
        )

      if ("Area from all simulations" %in% CountersDisease_radioButt) {
        pl <- pl +
          geom_ribbon(
            data = DfStat,
            aes(x = Day, ymin = MinV, ymax = MaxV, group = Counters, fill = Counters), alpha = 0.4
          ) +
          scale_fill_manual(
            values = counters_colors,
            limits = names(counters_colors),
            labels = names(counters_colors),
            drop = FALSE
          )
      }

      if ("Mean curves" %in% CountersDisease_radioButt) {
        pl <- pl + geom_line(
          data = DfStat,
          aes(x = Day, y = Mean, group = Counters, col = Counters, linetype = "Mean Curves")
        ) +
          scale_linetype_manual(values = c("Simulation" = "solid", "Mean Curves" = "dashed"))
      }
    }
    pl <- pl +
      geom_line(data = df, aes(x = Day, y = Number, col = Counters, linetype = "Simulation"), linewidth = 1.5) +
      labs(y = "", col = "Counters", linetype = "Type") +
      scale_color_manual(
        values = counters_colors,
        limits = names(counters_colors),
        labels = names(counters_colors),
        drop = FALSE
      ) +
      theme_fancy() + facet_wrap(~Counters, scales = "free")


    output$CountersPlot <- renderPlot({
      pl
    })
  })


  output$DownloadPostProc_Button <- downloadHandler(
    filename = function() {
      paste0("PostProcData_filtered_", Sys.Date(), ".zip")
    },
    content = function(file) {
      AEROSOL_std <- postprocObjects$AEROSOL_std
      CONTACT_std <- postprocObjects$CONTACT_std
      CONTACTmatrix <- postprocObjects$CONTACTmatrix
      COUNTERScsv <- postprocObjects$COUNTERScsv
      Mapping <- postprocObjects$Mapping
      simulation_log <- postprocObjects$simulation_log_full

      if (is.null(simulation_log)) {
        showNotification("No simulation data loaded.", type = "error")
        return(NULL)
      }

      show_modal_spinner()

      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)

      # Apply filters based on Disease Evolution settings
      selected_sims <- input$diseaseEvol_simulation
      selected_rooms <- input$diseaseEvol_room
      selected_agents <- input$diseaseEvol_agentType
      selected_floors <- input$diseaseEvol_floor
      selected_states <- input$diseaseEvol_states

      # Filter simulation log
      sim_data_filtered <- simulation_log

      # Filter by simulation/folder
      if (!is.null(selected_sims) && !"All" %in% selected_sims && length(selected_sims) > 0) {
        sim_data_filtered <- sim_data_filtered %>% dplyr::filter(Folder %in% selected_sims)
      }

      # Filter by room
      if (!is.null(selected_rooms) && !"All" %in% selected_rooms && length(selected_rooms) > 0) {
        Mapping_with_id <- Mapping %>%
          dplyr::mutate(RoomID = paste0(Name, " (", type, " - ", area, ")"))
        selected_room_ids <- Mapping_with_id %>%
          dplyr::filter(RoomID %in% selected_rooms) %>%
          dplyr::pull(ID) %>%
          unique()
        sim_data_filtered <- sim_data_filtered %>% dplyr::filter(room_id %in% selected_room_ids)
      }

      # Filter by agent type
      if (!is.null(selected_agents) && !"All" %in% selected_agents && length(selected_agents) > 0) {
        sim_data_filtered <- sim_data_filtered %>% dplyr::filter(agent_type %in% selected_agents)
      }

      # Filter by floor
      if (!is.null(selected_floors) && !"All" %in% selected_floors && length(selected_floors) > 0) {
        sim_data_filtered <- sim_data_filtered %>% dplyr::filter(CanvasID %in% selected_floors)
      }

      # Filter by disease state
      if (!is.null(selected_states) && !"All" %in% selected_states && length(selected_states) > 0) {
        sim_data_filtered <- sim_data_filtered %>% dplyr::filter(disease_state %in% selected_states)
      }

      # Save filtered simulation log
      file_name <- glue("SIMULATION_LOG_filtered.RDs")
      saveRDS(sim_data_filtered, file = file.path(temp_directory, file_name))

      # Also save as CSV for easier access
      file_name <- glue("SIMULATION_LOG_filtered.csv")
      write.csv(sim_data_filtered, file = file.path(temp_directory, file_name), row.names = FALSE)

      # Filter AEROSOL data
      if (!is.null(AEROSOL_std) && nrow(AEROSOL_std) > 0) {
        AEROSOL_filtered <- AEROSOL_std
        if (!is.null(selected_sims) && !"All" %in% selected_sims && length(selected_sims) > 0) {
          AEROSOL_filtered <- AEROSOL_filtered %>% dplyr::filter(Folder %in% selected_sims)
        }
        if (!is.null(selected_floors) && !"All" %in% selected_floors && length(selected_floors) > 0) {
          AEROSOL_filtered <- AEROSOL_filtered %>% dplyr::filter(CanvasID %in% selected_floors)
        }
        if (!is.null(selected_rooms) && !"All" %in% selected_rooms && length(selected_rooms) > 0) {
          Mapping_with_id <- Mapping %>%
            dplyr::mutate(RoomID = paste0(Name, " (", type, " - ", area, ")"))
          selected_room_names <- Mapping_with_id %>%
            dplyr::filter(RoomID %in% selected_rooms) %>%
            dplyr::select(Name, type, area) %>%
            dplyr::distinct()
          AEROSOL_filtered <- AEROSOL_filtered %>%
            dplyr::semi_join(selected_room_names, by = c("Name", "type", "area"))
        }
        saveRDS(AEROSOL_filtered, file = file.path(temp_directory, "AEROSOL_filtered.RDs"))
        write.csv(AEROSOL_filtered, file = file.path(temp_directory, "AEROSOL_filtered.csv"), row.names = FALSE)
      }

      # Filter CONTACT data
      if (!is.null(CONTACT_std) && nrow(CONTACT_std) > 0) {
        CONTACT_filtered <- CONTACT_std
        if (!is.null(selected_sims) && !"All" %in% selected_sims && length(selected_sims) > 0) {
          CONTACT_filtered <- CONTACT_filtered %>% dplyr::filter(Folder %in% selected_sims)
        }
        if (!is.null(selected_floors) && !"All" %in% selected_floors && length(selected_floors) > 0) {
          CONTACT_filtered <- CONTACT_filtered %>% dplyr::filter(CanvasID %in% selected_floors)
        }
        saveRDS(CONTACT_filtered, file = file.path(temp_directory, "CONTACT_filtered.RDs"))
        write.csv(CONTACT_filtered, file = file.path(temp_directory, "CONTACT_filtered.csv"), row.names = FALSE)
      }

      # Filter CONTACT matrix
      if (!is.null(CONTACTmatrix) && nrow(CONTACTmatrix) > 0) {
        CONTACTmatrix_filtered <- CONTACTmatrix
        if (!is.null(selected_sims) && !"All" %in% selected_sims && length(selected_sims) > 0) {
          CONTACTmatrix_filtered <- CONTACTmatrix_filtered %>% dplyr::filter(Folder %in% selected_sims)
        }
        if (!is.null(selected_agents) && !"All" %in% selected_agents && length(selected_agents) > 0) {
          CONTACTmatrix_filtered <- CONTACTmatrix_filtered %>%
            dplyr::filter(agent_type_1 %in% selected_agents | agent_type_2 %in% selected_agents)
        }
        saveRDS(CONTACTmatrix_filtered, file = file.path(temp_directory, "CONTACT_MATRIX_filtered.RDs"))
        write.csv(CONTACTmatrix_filtered, file = file.path(temp_directory, "CONTACT_MATRIX_filtered.csv"), row.names = FALSE)
      }

      # Filter COUNTERS data
      if (!is.null(COUNTERScsv) && nrow(COUNTERScsv) > 0) {
        COUNTERS_filtered <- COUNTERScsv
        if (!is.null(selected_sims) && !"All" %in% selected_sims && length(selected_sims) > 0) {
          COUNTERS_filtered <- COUNTERS_filtered %>% dplyr::filter(Folder %in% selected_sims)
        }
        saveRDS(COUNTERS_filtered, file = file.path(temp_directory, "COUNTERS_filtered.RDs"))
        write.csv(COUNTERS_filtered, file = file.path(temp_directory, "COUNTERS_filtered.csv"), row.names = FALSE)
      }

      # Save filter metadata
      filter_info <- list(
        simulations = selected_sims,
        rooms = selected_rooms,
        agent_types = selected_agents,
        floors = selected_floors,
        disease_states = selected_states,
        download_date = Sys.time()
      )
      saveRDS(filter_info, file = file.path(temp_directory, "FILTER_INFO.RDs"))

      zip::zip(
        zipfile = file,
        files = dir(temp_directory),
        root = temp_directory
      )

      remove_modal_spinner()
    },
    contentType = "application/zip"
  )
  #### end query post processing ####

  #### 2D visualisation ####

  observeEvent(input$PostProc_table_cell_clicked, {
    disable("rds_generation")
    disable("flamegpu_connection")
    info <- input$PostProc_table_cell_clicked
    folder <- req(info$value)
    req(postprocObjects$simulation_log_full) -> simulation_log
    floors <- req(canvasObjects$floors)

    # Reset show average checkbox when a folder is selected
    updateCheckboxInput(session, "visualShowAverage", value = FALSE)

    isolate({
      show_modal_spinner()
      simulation_log <- simulation_log %>%
        filter(Folder == folder) %>%
        select(-Folder)
      # mutate(time = time - min(time))

      simulation_log <- simulation_log %>%
        group_by(id) %>%
        arrange(time) %>%
        # tidyr::complete(time = tidyr::full_seq(time, 1)) %>%
        tidyr::fill(agent_type, x, y, z, CanvasID, Order, disease_state, .direction = "down") %>%
        ungroup()

      postprocObjects$simulation_log_folder <- simulation_log

      remove_modal_spinner()

      simulation_log <- simulation_log %>%
        filter(y != 10000)

      ## updating slider and selectize
      # <<<<<<< HEAD
      #       step = as.numeric(postprocObjects$Model$starting$step)
      #       updateNumericInput("animationStep",session = session, value = step, max = max(simulation_log$time)*step)
      #       updateSliderInput("animation", session = session,
      #                         max = max(simulation_log$time)*step, min = 0,
      #                         value = 0, step = step )
      #       updateSelectInput("visualFloor_select", session = session,
      #                         choices = c("All",unique(floors$Name)))
      #                        # choices = c("All",unique(floors$CanvasID)))
      #       updateSelectInput("visualAgent_select", session = session,
      #                         choices = c("All",sort(unique(simulation_log$agent_type))))
      # =======
      step <- as.numeric(postprocObjects$Model$starting$step)
      updateNumericInput("animationStep", session = session, value = step, max = max(simulation_log$time) * step)
      updateSliderInput("animation",
        session = session,
        max = max(simulation_log$time) * step, min = 0,
        value = 0, step = step
      )
      updateSelectInput("visualFloor_select",
        session = session,
        choices = c("All", unique(floors$Name))
      )
      updateSelectInput("visualAgent_select",
        session = session,
        choices = c("All", sort(unique(simulation_log$agent_type)))
      )

      # Store agent types for shape customization
      postprocObjects$agentTypes <- sort(unique(simulation_log$agent_type))
      ##

      shinyalert("Success", "File loaded.", "success", 1000)
    })
  })

  # Update slider when showing averages (without selecting a specific folder)
  observeEvent(input$visualShowAverage, {
    showAverage <- isTRUE(input$visualShowAverage)
    colorFeat <- input$visualColor_select

    if (showAverage && colorFeat %in% c("CumulContact", "Aerosol", "CumulAerosol")) {
      # Get max time from the data
      maxTime <- 0
      if (colorFeat %in% c("Aerosol", "CumulAerosol") && !is.null(postprocObjects$AEROSOL_std)) {
        maxTime <- max(postprocObjects$AEROSOL_std$time, na.rm = TRUE)
      } else if (colorFeat == "CumulContact" && !is.null(postprocObjects$CONTACT_std)) {
        maxTime <- max(postprocObjects$CONTACT_std$time, na.rm = TRUE)
      }

      if (maxTime > 0) {
        step <- as.numeric(postprocObjects$Model$starting$step)
        updateNumericInput("animationStep", session = session, value = step, max = maxTime * step)
        updateSliderInput("animation",
          session = session,
          max = maxTime * step, min = 0,
          value = 0, step = step
        )

        floors <- canvasObjects$floors
        if (!is.null(floors)) {
          updateSelectInput("visualFloor_select",
            session = session,
            choices = c("All", unique(floors$Name))
          )
        }
      }
    }
  })

  animationStep <- debounce(reactive({
    input$animationStep
  }), 1000L)

  observeEvent(animationStep(), {
    showAverage <- isTRUE(input$visualShowAverage)
    colorFeat <- input$visualColor_select

    # Skip folder requirement when showing averages
    if (!(showAverage && colorFeat %in% c("CumulContact", "Aerosol", "CumulAerosol"))) {
      req(postprocObjects$simulation_log_folder)
    }

    if (is.na(input$animationStep) || input$animationStep == "") {
      shinyalert("Error", "The time step cannot be less than 1 sec.", type = "error")
      return()
    }

    if (input$animationStep < 1) {
      shinyalert("Error", "The time step cannot be less than 1 sec.", type = "error")
      return()
    }

    # Get max time based on mode
    if (showAverage && colorFeat %in% c("CumulContact", "Aerosol", "CumulAerosol")) {
      if (colorFeat %in% c("Aerosol", "CumulAerosol") && !is.null(postprocObjects$AEROSOL_std)) {
        maxTime <- max(postprocObjects$AEROSOL_std$time, na.rm = TRUE)
      } else if (!is.null(postprocObjects$CONTACT_std)) {
        maxTime <- max(postprocObjects$CONTACT_std$time, na.rm = TRUE)
      } else {
        return()
      }
    } else {
      maxTime <- max(postprocObjects$simulation_log_folder$time)
    }

    if (input$animationStep > maxTime * as.numeric(postprocObjects$Model$starting$step)) {
      shinyalert("Error", "The time step cannot be greater than the maximum time of the simulation.", type = "error")
      return()
    }

    updateSliderInput("animation", session = session, value = input$animation, step = input$animationStep)
  })
  observeEvent(input$next_step_visual, {
    showAverage <- isTRUE(input$visualShowAverage)
    colorFeat <- input$visualColor_select

    # Get max time based on mode
    if (showAverage && colorFeat %in% c("CumulContact", "Aerosol", "CumulAerosol")) {
      if (colorFeat %in% c("Aerosol", "CumulAerosol") && !is.null(postprocObjects$AEROSOL_std)) {
        maxTime <- max(postprocObjects$AEROSOL_std$time, na.rm = TRUE)
      } else if (!is.null(postprocObjects$CONTACT_std)) {
        maxTime <- max(postprocObjects$CONTACT_std$time, na.rm = TRUE)
      } else {
        return()
      }
    } else {
      req(postprocObjects$simulation_log_folder)
      maxTime <- max(postprocObjects$simulation_log_folder$time)
    }

    new_val <- min(
      input$animation + input$animationStep,
      maxTime * as.numeric(postprocObjects$Model$starting$step)
    )

    updateSliderInput(session, "animation", value = new_val)
  })

  output$TwoDMapPlots <- renderUI({
    showAverage <- isTRUE(input$visualShowAverage)
    colorFeat <- input$visualColor_select
    floors <- req(canvasObjects$floors)

    # When showing averages with supported color features, use floor info from canvasObjects
    if (showAverage && colorFeat %in% c("CumulContact", "Aerosol", "CumulAerosol")) {
      if (is.null(floors)) {
        return(tags$p("Please load data first."))
      }
      num_floors_in_canvas <- length(unique(floors$Name)) + 1 # +1 to match CanvasID since in this case there is not the "Outside"
    } else {
      simulation_log_folder <- req(postprocObjects$simulation_log_folder)
      num_floors_in_canvas <- length(unique(simulation_log_folder$CanvasID))
    }

    # Increase height per floor to 800 pixels, with minimum of 1000px
    H <- max(400, num_floors_in_canvas * 400)
    plot_output_list <- plotOutput(outputId = "plot_map", height = paste0(H, "px"), width = "100%")

    (plot_output_list)
  })

  ### EMOJI shapes ####
  # Dynamic UI for agent shape and emoji selectors
  # Load emoGG emoji database for emoji mode
  emojiDatabase <- reactive({
    dat.filename <- system.file("emojis.RData", package = "emoGG")
    emojis <- NULL
    load(dat.filename)
    # Get unique emojis with their codes and all keywords
    # Transform codes for Twemoji CDN compatibility:
    # - For ZWJ sequences (containing \u): convert \u to - and add -fe0f at the end
    # - For simple emojis: use raw code as-is
    emojis %>%
      mutate(
        has_zwj = grepl("\\\\u|\\\\U", code),
        # Convert \u and \U to hyphens for ZWJ sequences
        code_transformed = ifelse(
          has_zwj,
          paste0(gsub("\\\\[uU]", "-", code), "-fe0f"), # ZWJ: convert \u to - and add -fe0f
          code # Simple: use as-is
        )
      ) %>%
      group_by(emoji, code_transformed) %>%
      summarise(
        keywords = paste(unique(keyword), collapse = ", "),
        original_code = first(code),
        .groups = "drop"
      ) %>%
      rename(code = code_transformed) %>%
      ungroup()
  })

  # Reactive value to store emoji assignments for each agent type
  emojiAssignments <- reactiveVal(list())

  # Initialize emoji assignments when agent types are loaded
  observe({
    agentTypes <- postprocObjects$agentTypes
    if (!is.null(agentTypes) && length(agentTypes) > 0) {
      current <- emojiAssignments()
      # Default emojis for new agents
      defaultEmojis <- c(
        "1f9d1", "1f468", "1f469", "1f477", "1f9d2", "1f46e", "1f9d3", "1f476",
        "1f3c3", "1f6b6", "1f913", "1f60a", "1f431", "1f436", "1f916", "1f47d"
      )
      for (i in seq_along(agentTypes)) {
        agent <- agentTypes[i]
        if (is.null(current[[agent]])) {
          current[[agent]] <- list(
            code = defaultEmojis[min(i, length(defaultEmojis))],
            emoji = NA
          )
        }
      }
      emojiAssignments(current)
    }
  })

  # Search emojis by keyword
  output$emojiSearchResults <- renderUI({
    req(input$agentVisualMode == "emojis")
    searchTerm <- input$emojiSearchKeyword
    emojiDB <- emojiDatabase()

    if (is.null(searchTerm) || nchar(trimws(searchTerm)) < 2) {
      return(tags$p(
        style = "color: #888; font-style: italic;",
        "Enter at least 2 characters to search..."
      ))
    }

    # Search in emoji names and keywords
    searchTerm <- tolower(trimws(searchTerm))
    matches <- emojiDB %>%
      filter(grepl(searchTerm, tolower(emoji)) | grepl(searchTerm, tolower(keywords))) %>%
      head(30) # Limit results

    if (nrow(matches) == 0) {
      return(tags$p(style = "color: #cc0000;", "No emojis found for '", searchTerm, "'"))
    }

    # Create clickable emoji buttons
    emojiButtons <- lapply(seq_len(nrow(matches)), function(i) {
      row <- matches[i, ]
      actionButton(
        inputId = paste0("selectEmoji_", row$code),
        label = row$emoji,
        style = "font-size: 24px; padding: 8px 12px; margin: 3px; background-color: #fff; border: 1px solid #ddd; border-radius: 8px; cursor: pointer;",
        title = paste0(row$emoji, " (", row$code, ")\nKeywords: ", row$keywords),
        onclick = sprintf(
          "Shiny.setInputValue('selectedEmojiCode', '%s', {priority: 'event'}); Shiny.setInputValue('selectedEmojiName', '%s', {priority: 'event'});",
          row$code, row$emoji
        )
      )
    })

    tagList(
      tags$p(
        style = "margin-bottom: 5px; color: #666;",
        paste0("Found ", nrow(matches), " emojis. Click to select:")
      ),
      tags$div(
        style = "max-height: 200px; overflow-y: auto; padding: 5px; background-color: #fafafa; border-radius: 5px;",
        emojiButtons
      )
    )
  })

  # Handle emoji selection from search
  observeEvent(input$selectedEmojiCode, {
    req(input$selectedEmojiCode)
    req(input$emojiAgentSelector)

    selectedAgent <- input$emojiAgentSelector
    if (selectedAgent != "") {
      current <- emojiAssignments()
      current[[selectedAgent]] <- list(
        code = input$selectedEmojiCode,
        emoji = input$selectedEmojiName
      )
      emojiAssignments(current)

      # Show confirmation
      showNotification(
        paste0("Assigned ", input$selectedEmojiName, " to ", selectedAgent),
        type = "message",
        duration = 2
      )
    }
  })

  # Render emoji assignments table
  output$emojiAssignmentsTable <- renderTable(
    {
      agentTypes <- req(postprocObjects$agentTypes)
      assignments <- emojiAssignments()
      emojiDB <- emojiDatabase()

      data.frame(
        Agent = agentTypes,
        Emoji = sapply(agentTypes, function(a) {
          if (!is.null(assignments[[a]])) {
            code <- assignments[[a]]$code
            # Try to get emoji character from database
            emojiRow <- emojiDB %>%
              filter(code == !!code) %>%
              head(1)
            if (nrow(emojiRow) > 0) {
              emojiRow$emoji
            } else {
              paste0("[", code, "]")
            }
          } else {
            "Not set"
          }
        }),
        Code = sapply(agentTypes, function(a) {
          if (!is.null(assignments[[a]])) assignments[[a]]$code else ""
        }),
        stringsAsFactors = FALSE
      )
    },
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE
  )

  output$agentShapeSelectors <- renderUI({
    agentTypes <- req(postprocObjects$agentTypes)
    visualMode <- input$agentVisualMode
    if (is.null(visualMode)) visualMode <- "shapes"

    if (visualMode == "emojis") {
      # New emoji UI with agent selector and keyword search
      emojiUI <- tagList(
        # Help section
        tags$div(
          style = "margin-bottom: 15px; padding: 12px; background-color: #e8f4fd; border-radius: 8px; border-left: 4px solid #2196F3;",
          tags$p(
            style = "margin: 0;",
            tags$strong(icon("info-circle"), " How to use:"),
            " 1) Select an agent type, 2) Search for an emoji by keyword, 3) Click the emoji to assign it."
          )
        ),

        # Agent selector and search in a row
        fluidRow(
          column(
            4,
            selectInput("emojiAgentSelector",
              label = tags$span(icon("user"), " Select Agent Type:"),
              choices = agentTypes,
              selected = agentTypes[1],
              width = "100%"
            )
          ),
          column(
            8,
            textInput("emojiSearchKeyword",
              label = tags$span(icon("search"), " Search Emoji by Keyword:"),
              placeholder = "e.g., face, happy, doctor, walk, sick...",
              width = "100%"
            )
          )
        ),

        # Search results
        fluidRow(
          column(
            12,
            tags$div(
              style = "min-height: 100px; margin-bottom: 15px;",
              uiOutput("emojiSearchResults")
            )
          )
        ),

        # Current assignments table
        tags$hr(),
        tags$h5(icon("list"), " Current Emoji Assignments:"),
        tags$div(
          style = "max-height: 250px; overflow-y: auto;",
          tableOutput("emojiAssignmentsTable")
        ),

        # Quick suggestions
        tags$hr(),
        tags$details(
          tags$summary(
            style = "cursor: pointer; color: #666; font-size: 12px;",
            icon("lightbulb"), " Suggested keywords for epidemic simulations"
          ),
          tags$div(
            style = "padding: 10px; background-color: #f5f5f5; border-radius: 5px; margin-top: 5px;",
            tags$p(
              style = "margin: 0; font-size: 11px; color: #555;",
              tags$strong("People:"), " person, man, woman, child, baby, old, walk, run, stand", tags$br(),
              tags$strong("Health:"), " sick, mask, face, thermometer, pill, hospital", tags$br(),
              tags$strong("Emotions:"), " happy, sad, smile, cry, angry, fear", tags$br(),
              tags$strong("Professions:"), " doctor, nurse, police, guard, worker", tags$br(),
              tags$strong("Animals:"), " cat, dog, mouse, bird, bat, bug"
            )
          )
        )
      )

      helpSection <- NULL
      header <- NULL
      ui_list <- list(emojiUI)
    } else {
      # Shape mode - use ggplot2 shapes
      shapeChoices <- c(
        "Circle (filled)" = 16, "Square (filled)" = 15, "Triangle (filled)" = 17,
        "Diamond (filled)" = 18, "Circle" = 1, "Square" = 0, "Triangle" = 2,
        "Cross" = 3, "X" = 4, "Diamond" = 5, "Triangle down" = 6,
        "Square cross" = 7, "Star" = 8, "Plus diamond" = 9,
        "Circle plus" = 10, "Triangles up down" = 11, "Square plus" = 12,
        "Circle cross" = 13, "Square triangle" = 14
      )

      ui_list <- lapply(seq_along(agentTypes), function(i) {
        agentType <- agentTypes[i]
        fluidRow(
          column(4, tags$strong(paste0("Agent: ", agentType))),
          column(4, selectInput(
            inputId = paste0("agentShape_", gsub("[^[:alnum:]]", "_", agentType)),
            label = NULL, choices = shapeChoices,
            selected = shapeChoices[min(i, length(shapeChoices))]
          )),
          column(4, numericInput(
            inputId = paste0("agentSize_", gsub("[^[:alnum:]]", "_", agentType)),
            label = NULL, value = 5, min = 1, max = 20, step = 1
          ))
        )
      })

      header <- fluidRow(
        column(4, tags$strong("Agent Type")),
        column(4, tags$strong("Shape")),
        column(4, tags$strong("Size"))
      )
      helpSection <- NULL
    }

    tagList(helpSection, header, ui_list)
  })

  # Render each plot individually
  observeEvent(input$visualAgent_select, {
    simulation_log <- req(postprocObjects$simulation_log_folder)

    if (input$visualAgent_select != "All") {
      idAgents <- simulation_log %>%
        filter(agent_type == input$visualAgent_select) %>%
        select(id) %>%
        distinct() %>%
        pull()
      updateSelectInput(session = session, "visualAgentID_select", choices = c("All", sort(idAgents)), selected = "All")
    }
  })
  ### END EMOJI shapes ####

  # Handle animation background image upload
  observeEvent(input$animation_bg_file, {
    req(input$animation_bg_file)

    ext <- tolower(tools::file_ext(input$animation_bg_file$name))
    if (ext != "png") {
      showNotification("Please upload a PNG file.", type = "error", duration = 5)
      return()
    }

    # Read PNG image
    img <- tryCatch(
      png::readPNG(input$animation_bg_file$datapath),
      error = function(e) {
        showNotification(paste("Error reading PNG:", e$message), type = "error", duration = 5)
        return(NULL)
      }
    )
    if (is.null(img)) {
      return()
    }

    # Store the image in postprocObjects
    postprocObjects$animation_bg <- list(
      img = img,
      width = dim(img)[2],
      height = dim(img)[1]
    )

    updateCheckboxInput(session, "animation_show_bg", value = TRUE)
    showNotification("Background image loaded successfully.", type = "message", duration = 3)
  })

  # Handle clear background button
  observeEvent(input$animation_clear_bg, {
    postprocObjects$animation_bg <- NULL
    updateCheckboxInput(session, "animation_show_bg", value = FALSE)
    showNotification("Background image cleared.", type = "message", duration = 3)
  })

  observe({
    info <- input$PostProc_table_cell_clicked
    showAverage <- isTRUE(input$visualShowAverage)
    colorFeat <- input$visualColor_select

    # When showing averages with CumulContact/Aerosol/CumulAerosol, folder is not required
    if (showAverage && colorFeat %in% c("CumulContact", "Aerosol", "CumulAerosol")) {
      folder <- NULL
      req(postprocObjects$AEROSOL_std) # Require that data is loaded
    } else {
      folder <- req(info$value)
    }

    roomsINcanvas <- req(postprocObjects$MappingID_room)
    floorSelected <- input$visualFloor_select

    # changes from the BG
    animation_bg <- postprocObjects$animation_bg
    input$animation_show_bg
    input$animation_bg_alpha
    room_fill_alpha <- input$room_fill_alpha

    # For averages mode, we need to react to animation slider changes
    animationTime <- input$animation
    Label <- input$visualLabel_select

    isolate({
      step <- as.numeric(postprocObjects$Model$starting$step)
      timeIn <- animationTime / step
      timeGrid <- seq(0, timeIn, 1) # number of steps to reach the seconds selected

      disease <- strsplit(isolate(req("SEIRD")), "")[[1]]

      # Define the fixed colors and shapes
      fixed_colors <- c("S" = "green", "E" = "blue", "I" = "red", "R" = "purple", "D" = "black")
      other_chars <- setdiff(unique(disease), names(fixed_colors))
      random_colors <- sample(colors(), length(other_chars))
      all_colors <- c(fixed_colors, setNames(random_colors, other_chars))

      colorDisease <- data.frame(State = names(all_colors), Col = (all_colors), stringsAsFactors = F)
      colorDisease$State <- factor(x = colorDisease$State, levels = disease)

      ##
      if (colorFeat == "Area") {
        roomsINcanvas <- merge(roomsINcanvas %>% select(-colorFill),
          canvasObjects$areas %>% select(-ID),
          by.x = "area", by.y = "Name"
        ) %>% rename(colorFill = Color)
        roomsINcanvas$IDtoColor <- roomsINcanvas$area
      } else if (colorFeat == "Type") {
        roomsINcanvas <- merge(roomsINcanvas %>% select(-colorFill),
          canvasObjects$types %>% select(-ID),
          by.x = "type", by.y = "Name"
        ) %>%
          rename(colorFill = Color)
        roomsINcanvas$IDtoColor <- roomsINcanvas$type
      } else if (colorFeat == "Name") {
        roomsINcanvas <- merge(roomsINcanvas %>% select(-colorFill),
          canvasObjects$rooms %>% select(Name, colorFill),
          by.x = "Name", by.y = "Name"
        )
        roomsINcanvas$IDtoColor <- roomsINcanvas$Name
      } else if (colorFeat == "CumulContact") {
        if (showAverage) {
          # Average across all folders
          CONTACT_std <- postprocObjects$CONTACT_std %>%
            filter(time <= timeIn)

          if (dim(CONTACT_std)[1] == 0) {
            roomsINcanvas$IDtoColor <- 0
          } else {
            # Count per folder, then average
            CONTACT_std <- CONTACT_std %>%
              group_by(Folder, CanvasID, Name, area, type, ID) %>%
              summarize(counts = n(), .groups = "drop") %>%
              group_by(CanvasID, Name, area, type, ID) %>%
              summarize(IDtoColor = mean(counts), .groups = "drop")

            CONTACT_std <- roomsINcanvas %>%
              select(Name, CanvasID, type, area, ID) %>%
              distinct() %>%
              full_join(CONTACT_std, by = c("Name", "CanvasID", "type", "area", "ID")) %>%
              mutate(IDtoColor = ifelse(is.na(IDtoColor), 0, IDtoColor))

            if ("IDtoColor" %in% colnames(roomsINcanvas)) {
              roomsINcanvas <- roomsINcanvas %>% select(-IDtoColor)
            }
            roomsINcanvas <- merge(roomsINcanvas, CONTACT_std)
          }
        } else {
          CONTACT_std <- postprocObjects$CONTACT_std %>%
            filter(Folder == folder, time <= timeIn) %>%
            select(-Folder)

          if (dim(CONTACT_std)[1] == 0) {
            roomsINcanvas$IDtoColor <- 0
          } else {
            CONTACT_std <- CONTACT_std %>%
              group_by(CanvasID, Name, area, type, ID) %>%
              summarize(counts = n()) %>%
              rename(IDtoColor = counts)

            CONTACT_std <- roomsINcanvas %>%
              select(Name, CanvasID, type, area, ID) %>%
              distinct() %>%
              full_join(CONTACT_std, by = c("Name", "CanvasID", "type", "area", "ID")) %>%
              mutate(IDtoColor = ifelse(is.na(IDtoColor), 0, IDtoColor))

            if ("IDtoColor" %in% colnames(roomsINcanvas)) {
              roomsINcanvas <- roomsINcanvas %>% select(-IDtoColor)
            }
            roomsINcanvas <- merge(roomsINcanvas, CONTACT_std)
          }
        }
      } else if (colorFeat == "Aerosol") {
        if (showAverage) {
          # Average across all folders
          AEROSOL_std <- postprocObjects$AEROSOL_std %>%
            filter(time <= timeIn)

          if (dim(AEROSOL_std)[1] == 0) {
            roomsINcanvas$IDtoColor <- 0
          } else {
            # Get the closest time step per folder, then average across folders
            AEROSOL_std <- AEROSOL_std %>%
              mutate(difftime = (time - timeIn)) %>%
              filter(difftime <= 0) %>%
              group_by(Folder) %>%
              filter(difftime == max(difftime)) %>%
              ungroup() %>%
              group_by(type, area, Name, CanvasID, ID) %>%
              summarize(IDtoColor = mean(virus_concentration), .groups = "drop")

            AEROSOL_std <- roomsINcanvas %>%
              select(Name, CanvasID, type, area, ID) %>%
              distinct() %>%
              left_join(AEROSOL_std, by = c("Name", "CanvasID", "type", "area", "ID")) %>%
              mutate(IDtoColor = ifelse(is.na(IDtoColor), 0, IDtoColor))

            if ("IDtoColor" %in% colnames(roomsINcanvas)) {
              roomsINcanvas <- roomsINcanvas %>% select(-IDtoColor)
            }
            roomsINcanvas <- merge(roomsINcanvas, AEROSOL_std)
          }
        } else {
          AEROSOL_std <- postprocObjects$AEROSOL_std %>%
            filter(Folder == folder, time <= timeIn) %>%
            select(-Folder)

          ### Check if it has all the data for each time step

          if (dim(AEROSOL_std)[1] == 0) {
            roomsINcanvas$IDtoColor <- 0
          } else {
            AEROSOL_std <- AEROSOL_std %>%
              mutate(difftime = (time - timeIn)) %>%
              filter(difftime <= 0, difftime == max(difftime)) %>%
              select(virus_concentration, type, area, Name, CanvasID, ID) %>%
              rename(IDtoColor = virus_concentration)
            # here i give to each room for each step a virus concetration = 0 when is not present
            AEROSOL_std <- roomsINcanvas %>%
              select(Name, CanvasID, type, area, ID) %>%
              distinct() %>%
              left_join(AEROSOL_std, by = c("Name", "CanvasID", "type", "area", "ID")) %>%
              mutate(IDtoColor = ifelse(is.na(IDtoColor), 0, IDtoColor))

            if ("IDtoColor" %in% colnames(roomsINcanvas)) {
              roomsINcanvas <- roomsINcanvas %>% select(-IDtoColor)
            }
            roomsINcanvas <- merge(roomsINcanvas, AEROSOL_std)
          }
        }
      } else if (colorFeat == "CumulAerosol") {
        if (showAverage) {
          # Average across all folders
          AEROSOL_std <- postprocObjects$AEROSOL_std %>%
            filter(time <= timeIn) %>%
            group_by(Folder, ID, type, area, Name, CanvasID) %>%
            summarise(virus_concentration = sum(virus_concentration), .groups = "drop") %>%
            group_by(ID, type, area, Name, CanvasID) %>%
            summarise(IDtoColor = mean(virus_concentration), .groups = "drop")

          if (dim(AEROSOL_std)[1] == 0) {
            roomsINcanvas$IDtoColor <- 0
          } else {
            AEROSOL_std <- roomsINcanvas %>%
              select(Name, CanvasID, type, area, ID) %>%
              distinct() %>%
              left_join(AEROSOL_std, by = c("Name", "CanvasID", "type", "area", "ID")) %>%
              mutate(IDtoColor = ifelse(is.na(IDtoColor), 0, IDtoColor))

            if ("IDtoColor" %in% colnames(roomsINcanvas)) {
              roomsINcanvas <- roomsINcanvas %>% select(-IDtoColor)
            }
            roomsINcanvas <- merge(roomsINcanvas, AEROSOL_std)
          }
        } else {
          AEROSOL_std <- postprocObjects$AEROSOL_std %>%
            filter(Folder == folder, time <= timeIn) %>%
            group_by(ID, type, area, Name, CanvasID) %>%
            summarise(virus_concentration = sum(virus_concentration)) %>%
            mutate(time = timeIn) %>%
            ungroup()

          if (dim(AEROSOL_std)[1] == 0) {
            roomsINcanvas$IDtoColor <- 0
          } else {
            AEROSOL_std <- AEROSOL_std %>%
              mutate(difftime = (time - timeIn)) %>%
              filter(difftime <= 0, difftime == max(difftime)) %>%
              select(virus_concentration, type, area, Name, CanvasID, ID) %>%
              rename(IDtoColor = virus_concentration)

            # here i give to each room for each step a virus concetration = 0 when is not present
            AEROSOL_std <- roomsINcanvas %>%
              select(Name, CanvasID, type, area, ID) %>%
              distinct() %>%
              left_join(AEROSOL_std, by = c("Name", "CanvasID", "type", "area", "ID")) %>%
              mutate(IDtoColor = ifelse(is.na(IDtoColor), 0, IDtoColor))

            if ("IDtoColor" %in% colnames(roomsINcanvas)) {
              roomsINcanvas <- roomsINcanvas %>% select(-IDtoColor)
            }
            roomsINcanvas <- merge(roomsINcanvas, AEROSOL_std)
          }
        }
      }

      df <- roomsINcanvas %>%
        mutate(
          xmin = x + l,
          xmax = x,
          ymin = y + w,
          ymax = y
        )

      floors <- canvasObjects$floors

      if (floorSelected != "All") {
        df <- df %>% filter(CanvasID == floorSelected)
      } else {
        df$CanvasID <- factor(df$CanvasID, levels = floors$Name)
      }

      if (colorFeat %in% c("CumulContact", "Aerosol", "CumulAerosol")) {
        MinCol <- 0

        # Calculate data max - for averages, compute max of averages across all folders
        if (showAverage) {
          if (colorFeat == "Aerosol") {
            # Average virus_concentration across folders at each time, then get max
            dataMaxCol <- max(postprocObjects$AEROSOL_std %>%
              group_by(time, type, area, Name, CanvasID, ID) %>%
              summarise(avg_conc = mean(virus_concentration), .groups = "drop") %>%
              pull(avg_conc), na.rm = TRUE)
          } else if (colorFeat == "CumulContact") {
            # Count per folder, average, then get max
            dataMaxCol <- max(postprocObjects$CONTACT_std %>%
              group_by(Folder, type, area, Name, CanvasID, ID) %>%
              count() %>%
              group_by(type, area, Name, CanvasID, ID) %>%
              summarise(avg_n = mean(n), .groups = "drop") %>%
              pull(avg_n), na.rm = TRUE)
          } else if (colorFeat == "CumulAerosol") {
            # Cumulative sum per folder, then average across folders
            dataMaxCol <- max(postprocObjects$AEROSOL_std %>%
              group_by(Folder, type, area, Name, CanvasID, ID) %>%
              summarise(cumsum_conc = sum(virus_concentration), .groups = "drop") %>%
              group_by(type, area, Name, CanvasID, ID) %>%
              summarise(avg_cumsum = mean(cumsum_conc), .groups = "drop") %>%
              pull(avg_cumsum), na.rm = TRUE)
          }
        } else {
          # Original logic for single folder
          if (colorFeat == "Aerosol") {
            dataMaxCol <- max(postprocObjects$AEROSOL_std %>%
              filter(Folder == folder) %>% pull(virus_concentration))
          } else if (colorFeat == "CumulContact") {
            dataMaxCol <- max(postprocObjects$CONTACT_std %>%
              filter(Folder == folder) %>%
              group_by(type, area, Name, CanvasID, ID) %>%
              count() %>%
              pull(n))
          } else if (colorFeat == "CumulAerosol") {
            dataMaxCol <- max(postprocObjects$AEROSOL_std %>%
              filter(Folder == folder) %>%
              group_by(type, area, Name, CanvasID, ID) %>%
              mutate(virus_concentration = cumsum(virus_concentration)) %>%
              pull(virus_concentration))
          }
        }

        # Check if custom max value is provided
        customMax <- input$visualColor_maxValue
        if (!is.null(customMax) && !is.na(customMax) && customMax > 0) {
          MaxCol <- customMax
        } else {
          MaxCol <- dataMaxCol
        }

        # Get scale type selection
        scaleType <- input$visualScaleType
        if (is.null(scaleType)) scaleType <- "Linear"

        if (scaleType == "Log10") {
          # Log10 scale
          sc_fill <- scale_fill_gradientn(
            colors = c("green", "yellow", "red"),
            limits = c(1e-10, MaxCol + 1e-10),
            trans = "log10",
            guide = "colourbar",
            na.value = "green"
          )
        } else if (scaleType == "Sqrt") {
          # Square root scale - good compromise between linear and log
          sc_fill <- scale_fill_gradientn(
            colors = c("green", "yellow", "red"),
            limits = c(MinCol, MaxCol),
            trans = "sqrt",
            guide = "colourbar",
            na.value = "green"
          )
        } else if (scaleType == "Custom") {
          # Custom breakpoints defined by user (as percentages)
          break1 <- if (!is.null(input$customBreak1) && !is.na(input$customBreak1)) input$customBreak1 / 100 else 0.1
          break2 <- if (!is.null(input$customBreak2) && !is.na(input$customBreak2)) input$customBreak2 / 100 else 0.3
          break3 <- if (!is.null(input$customBreak3) && !is.na(input$customBreak3)) input$customBreak3 / 100 else 0.6

          # Ensure breaks are in order
          breaks <- sort(c(0, break1, break2, break3, 1))

          sc_fill <- scale_fill_gradientn(
            colors = c("green", "yellow", "orange", "orangered", "red"),
            values = breaks,
            limits = c(MinCol, MaxCol),
            guide = "colourbar",
            na.value = "green"
          )
        } else {
          # Linear scale (default)
          sc_fill <- scale_fill_gradient(
            low = "green", high = "red",
            limits = c(MinCol, MaxCol),
            guide = "colourbar"
          )
        }

        # Add unit for aerosol-related color features - indicate scale type
        scale_suffix <- switch(scaleType,
          "Log10" = " (log10)",
          "Sqrt" = " (sqrt)",
          "Custom" = " (custom)",
          ""
        )
        fill_label <- if (colorFeat %in% c("Aerosol", "CumulAerosol")) {
          if (showAverage) {
            if (scaleType != "Linear") bquote(paste("Avg PFU/", m^3, .(scale_suffix))) else expression(paste("Avg PFU/", m^3))
          } else {
            if (scaleType != "Linear") bquote(paste("PFU/", m^3, .(scale_suffix))) else expression(paste("PFU/", m^3))
          }
        } else {
          label_base <- if (showAverage) "Avg Contacts" else colorFeat
          if (scaleType != "Linear") paste0(label_base, scale_suffix) else label_base
        }
        guide_fill <- labs(fill = fill_label)
      } else {
        df$colorFillParsed <- df$colorFillParsed <- gsub(pattern = "rgba", replacement = "rgb", x = df$colorFill)
        df$colorFillParsed <- gsub(pattern = ",", replacement = "/255,", x = df$colorFillParsed)
        df$colorFillParsed <- gsub(pattern = ")", replacement = "/255)", x = df$colorFillParsed)

        df$colorFillParsed <- sapply(df$colorFillParsed, function(x) eval(parse(text = x)))
        dfcolor <- df %>%
          select(colorFillParsed, IDtoColor) %>%
          distinct()
        dfcolor$colorFillParsed <- gsub(pattern = "#([A-Fa-f0-9]{6})[A-Fa-f0-9]{2}", replacement = "#\\1", x = dfcolor$colorFillParsed)
        sc_fill <- scale_fill_manual(
          values = dfcolor$colorFillParsed,
          breaks = dfcolor$IDtoColor,
          drop = FALSE
        )
        guide_fill <- guides(fill = "none")
      }

      # df = df %>% mutate(ymin = -ymin + max(ymax), ymax = -ymax + max(ymax) )
      # simulation_log = simulation_log  %>% mutate(z = z + min(df$y) )

      # Separate special rooms (Spawnroom, Fillingroom) that should always be grey
      df_special <- df %>% dplyr::filter(type %in% c("Spawnroom", "Fillingroom"))
      df_normal <- df %>% dplyr::filter(!type %in% c("Spawnroom", "Fillingroom"))

      # Start building the plot
      pl <- ggplot() +
        scale_y_reverse()

      # Add background image if available and enabled
      if (!is.null(animation_bg) && isTRUE(input$animation_show_bg)) {
        bg_img <- animation_bg$img
        bg_width <- animation_bg$width
        bg_height <- animation_bg$height
        pixels_per_meter <- if (!is.null(input$animation_bg_pixels_per_meter) && input$animation_bg_pixels_per_meter > 0) {
          input$animation_bg_pixels_per_meter
        } else {
          10
        }
        bg_alpha <- if (!is.null(input$animation_bg_alpha)) input$animation_bg_alpha else 0.5

        # Scale the background image dimensions to meters (matching canvas scale: 10 pixels = 1 meter)
        # The rooms are in units where 1 unit = 1 meter, canvas uses 10px/m
        bg_xmax <- (bg_width / pixels_per_meter) * 10 # Convert to canvas units
        bg_ymax <- (bg_height / pixels_per_meter) * 10 # Note: y is reversed

        # Apply alpha to the image if needed
        if (bg_alpha < 1 && length(dim(bg_img)) == 3) {
          if (dim(bg_img)[3] == 3) {
            # Add alpha channel if not present
            bg_img <- abind::abind(bg_img, matrix(bg_alpha, nrow = dim(bg_img)[1], ncol = dim(bg_img)[2]), along = 3)
          } else if (dim(bg_img)[3] == 4) {
            # Multiply existing alpha by user alpha
            bg_img[, , 4] <- bg_img[, , 4] * bg_alpha
          }
        }
        pl <- pl + ggpubr::background_image(bg_img)
        # pl <- pl + annotation_raster(bg_img, xmin = 0, xmax = bg_xmax, ymin = bg_ymax, ymax = 0)
      }

      # Get room fill alpha value (default to 0.5 if not set)
      room_alpha <- if (!is.null(room_fill_alpha)) room_fill_alpha else 0.5

      # Add room layers
      pl <- pl +
        # Draw normal rooms with dynamic color
        geom_rect(
          data = df_normal,
          aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = IDtoColor),
          color = "black", alpha = room_alpha
        ) +
        # Draw special rooms (Spawnroom, Fillingroom) always in grey
        geom_rect(
          data = df_special,
          aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
          fill = "grey80", color = "black", alpha = room_alpha
        ) +
        sc_fill + guide_fill +
        scale_color_manual(
          values = colorDisease$Col,
          limits = (colorDisease$State),
          labels = (colorDisease$State),
          drop = FALSE
        ) +
        coord_fixed() +
        facet_wrap(~CanvasID, ncol = 2) +
        theme_bw() +
        theme(
          legend.position = "bottom",
          legend.direction = "vertical",
          axis.text = element_text(size = 16),
          axis.title = element_text(size = 20, face = "bold"),
          plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
          legend.text = element_text(size = 14),
          legend.key.size = unit(1.5, "cm"),
          legend.title = element_text(face = "bold", size = 18),
          strip.text = element_text(size = 18, face = "bold")
        )


      if (!Label %in% c("None", "Agent ID")) {
        df <- df %>% rename(name = Name, id = ID)
        pl <- pl + geom_label(
          data = df,
          aes(
            x = (xmin + xmax) / 2, y = (ymin + ymax) / 2,
            label = get(tolower(Label))
          ),
          color = "black", size = 4
        )
      }
      # else if(Label == "Agent ID"){
      #   dfSim = simulation_log %>% filter(time == timeIn)
      #   pl = pl + geom_label(data = dfSim,
      #                        aes(x = x, y = z,
      #                            label = id, col = disease_state ),
      #                        size = 4)
      # }

      postprocObjects$plot_2D <- pl +
        theme(
          panel.border = element_rect(
            color = "white",
            fill = NA,
            linewidth = 15
          )
        )
    })
  })

  # Function to generate 2D visualization plot with agents at a specific time
  generate2DPlotWithAgents <- function(pl_base, simulation_log, timeIn, folder, colorFeat, visualAgent,
                                       visualAgentID, Label, floorSelected, visualMode,
                                       emojiAgents, shapeAgents, floors, roomsINcanvas,
                                       AEROSOL_std, CONTACT_std, initial_time, step, customMax = NULL, titleSuffix = "", showAverage = FALSE) {
    pl <- pl_base

    # Find the layer with room data (geom_rect with xmin/xmax/ymin/ymax)
    # This handles cases where a background image layer may be present first
    room_layer_idx <- 1
    for (i in seq_along(pl$layers)) {
      layer_data <- pl$layers[[i]]$data
      if (!is.null(layer_data) && is.data.frame(layer_data) &&
        all(c("xmin", "xmax", "ymin", "ymax") %in% colnames(layer_data))) {
        room_layer_idx <- i
        break
      }
    }
    df <- pl$layers[[room_layer_idx]]$data

    # Filter simulation log for current time
    sim_log <- simulation_log %>%
      dplyr::filter(time <= timeIn) %>%
      dplyr::group_by(id) %>%
      dplyr::filter(time == max(time)) %>%
      dplyr::filter(y != 10000) %>%
      dplyr::ungroup()

    if (visualAgent != "All") {
      sim_log <- sim_log %>% dplyr::filter(agent_type == visualAgent)
      if (visualAgentID != "All") {
        sim_log <- sim_log %>% dplyr::filter(id == visualAgentID)
      }
    }

    sim_log$agent_type <- factor(sim_log$agent_type, levels = unique(simulation_log$agent_type))

    if (floorSelected != "All") {
      sim_log <- sim_log %>% dplyr::filter(CanvasID == floorSelected)
    } else {
      sim_log$CanvasID <- factor(sim_log$CanvasID, levels = floors$Name)
    }

    # Update room colors based on colorFeat
    # Note: Layer 1 is normal rooms, Layer 2 is special rooms (Spawnroom, Fillingroom) which stay grey
    if (colorFeat %in% c("CumulAerosol", "Aerosol") && !is.null(AEROSOL_std)) {
      if (showAverage) {
        # For averages: use data already calculated across all folders
        AEROSOL_data <- AEROSOL_std %>%
          dplyr::filter(time <= timeIn)
      } else {
        # For single folder: filter by the selected folder
        AEROSOL_data <- AEROSOL_std %>%
          dplyr::filter(Folder == folder, time <= timeIn)
      }

      if (colorFeat == "CumulAerosol") {
        if (showAverage) {
          # For averages: data is already averaged
          AEROSOL_data <- AEROSOL_data %>%
            dplyr::group_by(Folder, ID, type, area, Name, CanvasID) %>%
            dplyr::summarise(virus_concentration = sum(virus_concentration), .groups = "drop") %>%
            dplyr::group_by(ID, type, area, Name, CanvasID) %>%
            dplyr::summarise(IDtoColor = mean(virus_concentration), .groups = "drop")
        } else {
          AEROSOL_data <- AEROSOL_data %>%
            dplyr::group_by(ID, type, area, Name, CanvasID) %>%
            dplyr::summarise(virus_concentration = sum(virus_concentration), .groups = "drop") %>%
            dplyr::mutate(time = timeIn)
        }
      }

      if (nrow(AEROSOL_data) == 0) {
        df$IDtoColor <- 0
      } else {
        if (!showAverage) {
          AEROSOL_data <- AEROSOL_data %>%
            dplyr::mutate(difftime = (timeIn - time)) %>%
            dplyr::filter(difftime >= 0, difftime == min(difftime)) %>%
            dplyr::select(virus_concentration, type, area, Name, CanvasID, ID) %>%
            dplyr::rename(IDtoColor = virus_concentration)
        } else if (!("IDtoColor" %in% colnames(AEROSOL_data))) {
          # For averages when CumulAerosol, IDtoColor is already computed above
          AEROSOL_data <- AEROSOL_data %>%
            dplyr::select(type, area, Name, CanvasID, ID, IDtoColor)
        }

        AEROSOL_data <- roomsINcanvas %>%
          dplyr::select(Name, CanvasID, type, area, ID) %>%
          dplyr::distinct() %>%
          dplyr::left_join(AEROSOL_data, by = c("Name", "CanvasID", "type", "area", "ID")) %>%
          dplyr::mutate(IDtoColor = ifelse(is.na(IDtoColor), 0, IDtoColor))

        if ("IDtoColor" %in% colnames(df)) df <- df %>% dplyr::select(-IDtoColor)
        df <- merge(df, AEROSOL_data)
        # Only update normal rooms (exclude Spawnroom, Fillingroom)
        df <- df %>% dplyr::filter(!type %in% c("Spawnroom", "Fillingroom"))
      }
      pl$layers[[room_layer_idx]]$data <- df
    } else if (colorFeat == "CumulContact" && !is.null(CONTACT_std)) {
      if (showAverage) {
        # For averages: use data already calculated across all folders
        CONTACT_data <- CONTACT_std %>%
          dplyr::filter(time <= timeIn)
      } else {
        # For single folder: filter by the selected folder
        CONTACT_data <- CONTACT_std %>%
          dplyr::filter(Folder == folder, time <= timeIn)
      }

      if (nrow(CONTACT_data) == 0) {
        df$IDtoColor <- 0
      } else {
        if (showAverage) {
          # For averages: calculate averages
          CONTACT_data <- CONTACT_data %>%
            dplyr::group_by(Folder, CanvasID, Name, area, type, ID) %>%
            dplyr::summarize(counts = n(), .groups = "drop") %>%
            dplyr::group_by(CanvasID, Name, area, type, ID) %>%
            dplyr::summarize(IDtoColor = mean(counts), .groups = "drop")
        } else {
          CONTACT_data <- CONTACT_data %>%
            dplyr::group_by(CanvasID, Name, area, type, ID) %>%
            dplyr::count() %>%
            dplyr::rename(IDtoColor = n) %>%
            dplyr::ungroup()
        }

        CONTACT_data <- roomsINcanvas %>%
          dplyr::select(Name, CanvasID, type, area, ID) %>%
          dplyr::distinct() %>%
          dplyr::left_join(CONTACT_data, by = c("Name", "CanvasID", "type", "area", "ID")) %>%
          dplyr::mutate(IDtoColor = ifelse(is.na(IDtoColor), 0, IDtoColor))

        if ("IDtoColor" %in% colnames(df)) df <- df %>% dplyr::select(-IDtoColor)
        df <- merge(df, CONTACT_data)
        # Only update normal rooms (exclude Spawnroom, Fillingroom)
        df <- df %>% dplyr::filter(!type %in% c("Spawnroom", "Fillingroom"))
      }
      pl$layers[[room_layer_idx]]$data <- df
    }

    # Add agents to plot
    if (visualMode == "emojis" && !is.null(emojiAgents)) {
      sim_log <- sim_log %>%
        dplyr::mutate(agent_type_char = as.character(agent_type)) %>%
        dplyr::left_join(emojiAgents, by = c("agent_type_char" = "Agents"))

      unique_emoji_codes <- unique(sim_log$EmojiCode)
      for (emoji_code in unique_emoji_codes) {
        sim_subset <- sim_log %>% dplyr::filter(EmojiCode == emoji_code)
        if (nrow(sim_subset) > 0) {
          pl <- pl + emoGG::geom_emoji(data = sim_subset, aes(x = x, y = z), emoji = emoji_code)
        }
      }
      # Position the disease state indicator above the emoji (offset by ~0.8 units)
      # Since scale_y_reverse() is used, we subtract to move visually upward
      pl <- pl + geom_point(
        data = sim_log, aes(x = x, y = z - 0.8, color = disease_state),
        size = 4, alpha = 0.7, shape = 19, stroke = 1
      ) +
        guides(color = guide_legend(override.aes = list(size = 5)))
    } else if (!is.null(shapeAgents)) {
      # Add black contour layer first (slightly larger, behind the colored points)
      pl <- pl + geom_point(
        data = sim_log, aes(x = x, y = z, shape = agent_type), size = 6,
        color = "black", stroke = 2.5, show.legend = FALSE
      ) +
        # Add colored points on top
        geom_point(data = sim_log, aes(
          x = x, y = z, group = id, shape = agent_type,
          color = disease_state
        ), size = 6, stroke = 1.5) +
        scale_shape_manual(values = setNames(shapeAgents$Shape, shapeAgents$Agents)) +
        scale_size_manual(values = setNames(shapeAgents$Size, shapeAgents$Agents), guide = "none") +
        guides(shape = guide_legend(ncol = 8, order = 1))
    }

    if (Label == "Agent ID") {
      pl <- pl + geom_label(data = sim_log, aes(x = x, y = z, label = id, col = disease_state), size = 4)
    }

    # Calculate title
    total_seconds <- timeIn * step + as.numeric(strsplit(initial_time, ":")[[1]][1]) * 60 * 60 +
      as.numeric(strsplit(initial_time, ":")[[1]][2]) * 60
    days <- total_seconds %/% (24 * 3600)
    remaining_seconds <- total_seconds %% (24 * 3600)
    hours <- remaining_seconds %/% 3600
    remaining_seconds <- remaining_seconds %% 3600
    minutes <- remaining_seconds %/% 60
    seconds <- remaining_seconds %% 60

    title_text <- paste0(days + 1, "d:", hours, "h:", minutes, "m:", seconds, "s (# steps: ", round(timeIn), ")", titleSuffix)

    if (visualMode == "emojis") {
      title <- labs(
        title = title_text,
        x = "", y = "", color = "Disease state"
      )
    } else {
      title <- labs(
        title = title_text,
        x = "", y = "", color = "Disease state", shape = "Agent type"
      )
    }

    pl + title
  }

  observe({
    info <- input$PostProc_table_cell_clicked
    showAverage <- isTRUE(input$visualShowAverage)

    timeIn <- req(input$animation)
    colorFeat <- input$visualColor_select
    step <- as.numeric(postprocObjects$Model$starting$step)
    timeIn <- timeIn / step

    # Get folder if available
    folder <- if (!is.null(info) && !is.null(info$value)) info$value else NULL

    # When showing averages without a folder selected, we still need some data loaded
    if (showAverage && colorFeat %in% c("CumulContact", "Aerosol", "CumulAerosol") && is.null(folder)) {
      req(postprocObjects$AEROSOL_std) # Require that data is loaded
    } else if (!showAverage || !(colorFeat %in% c("CumulContact", "Aerosol", "CumulAerosol"))) {
      # For non-average mode, folder is required
      folder <- req(info$value)
    }

    visualAgent <- input$visualAgent_select
    visualAgentID <- input$visualAgentID_select
    Label <- input$visualLabel_select
    floorSelected <- input$visualFloor_select
    visualMode <- input$agentVisualMode
    initial_time <- input$initial_time
    customMax <- input$visualColor_maxValue
    pl <- req(postprocObjects$plot_2D)

    roomsINcanvas <- postprocObjects$MappingID_room
    floors <- canvasObjects$floors
    if (is.null(visualMode)) visualMode <- "shapes"

    if (is.null(initial_time)) initial_time <- "00:00"

    # Calculate title
    total_seconds <- timeIn * step + as.numeric(strsplit(initial_time, ":")[[1]][1]) * 60 * 60 +
      as.numeric(strsplit(initial_time, ":")[[1]][2]) * 60
    days <- total_seconds %/% (24 * 3600)
    remaining_seconds <- total_seconds %% (24 * 3600)
    hours <- remaining_seconds %/% 3600
    remaining_seconds <- remaining_seconds %% 3600
    minutes <- remaining_seconds %/% 60
    seconds <- remaining_seconds %% 60

    title_suffix <- if (showAverage && colorFeat %in% c("CumulContact", "Aerosol", "CumulAerosol")) {
      " - Average across all folders"
    } else {
      ""
    }

    # Check if we have simulation_log for agents
    simulation_log <- postprocObjects$simulation_log_folder
    hasSimulationLog <- !is.null(simulation_log) && nrow(simulation_log) > 0

    # When showing averages WITHOUT a folder AND no simulation log, show just the base plot with title
    if (showAverage && colorFeat %in% c("CumulContact", "Aerosol", "CumulAerosol") && is.null(folder) && !hasSimulationLog) {
      title_text <- paste0(days + 1, "d:", hours, "h:", minutes, "m:", seconds, "s (# steps: ", round(timeIn), ")", title_suffix)
      final_plot <- pl + labs(title = title_text, x = "", y = "")

      output[["plot_map"]] <- renderPlot({
        final_plot
      })
      return()
    }

    # For all other cases (with folder selected or with simulation log), show agents
    req(simulation_log)

    # Get agent types from simulation_log
    agentTypesInLog <- unique(simulation_log$agent_type)

    if (visualMode == "emojis") {
      # Emoji mode: Get emoji codes from emojiAssignments reactive
      defaultEmojiCodes <- c(
        "1f9d1", "1f468", "1f469", "1f477", "1f9d2", "1f46e", "1f9d3", "1f476",
        "1f3c3", "1f6b6", "1f913", "1f60a", "1f431", "1f436", "1f916", "1f47d"
      )

      assignments <- emojiAssignments()

      customEmojiCodes <- sapply(seq_along(agentTypesInLog), function(i) {
        at <- agentTypesInLog[i]
        if (!is.null(assignments[[at]]) && !is.null(assignments[[at]]$code)) {
          return(assignments[[at]]$code)
        }
        return(defaultEmojiCodes[min(i, length(defaultEmojiCodes))])
      })
      names(customEmojiCodes) <- NULL

      emojiAgents <- data.frame(
        Agents = agentTypesInLog,
        EmojiCode = customEmojiCodes,
        stringsAsFactors = F
      )
      shapeAgents <- NULL
    } else {
      # Shape mode: Get custom shapes and sizes from user input
      customShapes <- sapply(agentTypesInLog, function(at) {
        inputId <- paste0("agentShape_", gsub("[^[:alnum:]]", "_", at))
        val <- input[[inputId]]
        if (is.null(val) || is.na(val)) {
          idx <- which(agentTypesInLog == at)
          return(as.numeric(c(16, 15, 17, 18, 1, 0, 2, 3, 4, 5)[min(idx, 10)]))
        }
        as.numeric(val)
      })
      names(customShapes) <- NULL

      customSizes <- sapply(agentTypesInLog, function(at) {
        inputId <- paste0("agentSize_", gsub("[^[:alnum:]]", "_", at))
        val <- input[[inputId]]
        if (is.null(val) || is.na(val)) {
          return(5)
        }
        as.numeric(val)
      })
      names(customSizes) <- NULL

      shapeAgents <- data.frame(
        Agents = agentTypesInLog,
        Shape = customShapes,
        Size = customSizes,
        stringsAsFactors = F
      )
      emojiAgents <- NULL
    }

    # Use the generate2DPlotWithAgents function
    final_plot <- generate2DPlotWithAgents(
      pl_base = pl,
      simulation_log = simulation_log,
      timeIn = timeIn,
      folder = folder,
      colorFeat = colorFeat,
      visualAgent = visualAgent,
      visualAgentID = visualAgentID,
      Label = Label,
      floorSelected = floorSelected,
      visualMode = visualMode,
      emojiAgents = emojiAgents,
      shapeAgents = shapeAgents,
      floors = floors,
      roomsINcanvas = roomsINcanvas,
      AEROSOL_std = postprocObjects$AEROSOL_std,
      CONTACT_std = postprocObjects$CONTACT_std,
      initial_time = initial_time,
      step = step,
      customMax = customMax,
      titleSuffix = title_suffix,
      showAverage = showAverage
    )

    output[["plot_map"]] <- renderPlot({
      final_plot
    })
  })

  # Download animation as MP4
  output$download_animation_mp4 <- downloadHandler(
    filename = function() {
      paste0("animation_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".mp4")
    },
    content = function(file) {
      # Validate required data before proceeding
      simulation_log <- postprocObjects$simulation_log_folder
      if (is.null(simulation_log)) {
        showNotification("Please load simulation data first.", type = "error")
        return(NULL)
      }

      showAverage <- isTRUE(input$visualShowAverage)
      colorFeat <- input$visualColor_select

      folder <- input$PostProc_table_cell_clicked$value

      # Check if folder is required (not when showing averages for aerosol/contact data)
      if (!showAverage || !(colorFeat %in% c("CumulContact", "Aerosol", "CumulAerosol"))) {
        if (is.null(folder) || folder == "") {
          showNotification("Please select a simulation folder from the table.", type = "error")
          return(NULL)
        }
      }

      pl_base <- postprocObjects$plot_2D
      if (is.null(pl_base)) {
        showNotification("Please wait for the plot to be generated.", type = "error")
        return(NULL)
      }

      # Show progress
      withProgress(message = "Generating animation...", value = 0, {
        step <- as.numeric(postprocObjects$Model$starting$step)
        fps <- input$animation_fps
        if (is.null(fps) || is.na(fps)) fps <- 10
        animStep <- input$animationStep
        if (is.null(animStep) || is.na(animStep)) animStep <- 1

        # Get settings
        colorFeat <- input$visualColor_select
        visualAgent <- input$visualAgent_select
        visualAgentID <- input$visualAgentID_select
        Label <- input$visualLabel_select
        floorSelected <- input$visualFloor_select
        visualMode <- input$agentVisualMode
        if (is.null(visualMode)) visualMode <- "shapes"
        showAverage <- isTRUE(input$visualShowAverage)

        # Get shape/emoji mappings
        agentTypesInLog <- unique(simulation_log$agent_type)

        if (visualMode == "emojis") {
          defaultEmojiCodes <- c(
            "1f9d1", "1f468", "1f469", "1f477", "1f9d2", "1f46e", "1f9d3", "1f476",
            "1f3c3", "1f6b6", "1f913", "1f60a", "1f431", "1f436", "1f916", "1f47d"
          )
          assignments <- emojiAssignments()
          customEmojiCodes <- sapply(seq_along(agentTypesInLog), function(i) {
            at <- agentTypesInLog[i]
            if (!is.null(assignments[[at]]) && !is.null(assignments[[at]]$code)) {
              return(assignments[[at]]$code)
            }
            return(defaultEmojiCodes[min(i, length(defaultEmojiCodes))])
          })
          names(customEmojiCodes) <- NULL
          emojiAgents <- data.frame(Agents = agentTypesInLog, EmojiCode = customEmojiCodes, stringsAsFactors = FALSE)
          shapeAgents <- NULL
        } else {
          customShapes <- sapply(agentTypesInLog, function(at) {
            inputId <- paste0("agentShape_", gsub("[^[:alnum:]]", "_", at))
            val <- input[[inputId]]
            if (is.null(val) || is.na(val)) {
              idx <- which(agentTypesInLog == at)
              return(as.numeric(c(16, 15, 17, 18, 1, 0, 2, 3, 4, 5)[min(idx, 10)]))
            }
            as.numeric(val)
          })
          customSizes <- sapply(agentTypesInLog, function(at) {
            inputId <- paste0("agentSize_", gsub("[^[:alnum:]]", "_", at))
            val <- input[[inputId]]
            if (is.null(val) || is.na(val)) {
              return(5)
            }
            as.numeric(val)
          })
          shapeAgents <- data.frame(Agents = agentTypesInLog, Shape = customShapes, Size = customSizes, stringsAsFactors = FALSE)
          emojiAgents <- NULL
        }

        # Calculate time range with from/to bounds and granularity
        maxTime <- max(simulation_log$time)
        fromTime <- input$animation_from
        toTime <- input$animation_to
        granularity <- input$animation_granularity
        if (is.null(granularity)) granularity <- "step"

        # Convert from seconds to steps
        fromSteps <- if (is.null(fromTime) || is.na(fromTime)) 0 else fromTime / step
        toSteps <- if (is.null(toTime) || is.na(toTime)) maxTime else toTime / step

        # Clamp values
        fromSteps <- max(0, min(fromSteps, maxTime))
        toSteps <- max(fromSteps, min(toSteps, maxTime))

        # Calculate step increment based on granularity
        stepIncrement <- switch(granularity,
          "step" = 1,
          "second" = 1 / step,
          "minute" = 60 / step,
          "hour" = 3600 / step,
          1
        )
        stepIncrement <- max(1, stepIncrement)

        timeSteps <- seq(fromSteps, toSteps, by = stepIncrement)
        n_frames <- length(timeSteps)

        # Create temp directory for frames
        tmpDir <- tempdir()
        framesDir <- file.path(tmpDir, paste0("animation_frames_", format(Sys.time(), "%Y%m%d%H%M%S")))
        dir.create(framesDir, showWarnings = FALSE, recursive = TRUE)

        # Prepare floor data
        floors <- canvasObjects$floors
        roomsINcanvas <- postprocObjects$MappingID_room

        # Get initial time
        initial_time <- input$initial_time
        if (is.null(initial_time)) initial_time <- "00:00"

        # Prepare data for parallel processing
        AEROSOL_std_data <- postprocObjects$AEROSOL_std
        CONTACT_std_data <- postprocObjects$CONTACT_std

        # Determine number of cores (use at most n_frames cores, and leave 1 core free)
        n_cores <- min(parallel::detectCores() - 1, n_frames, 8)
        n_cores <- max(1, n_cores)

        # Generate frames in parallel
        if (n_cores > 1 && n_frames > 4) {
          # Use parallel processing for larger frame counts
          incProgress(0.1, detail = paste("Starting parallel generation with", n_cores, "cores..."))

          # Create a function that generates a single frame
          generate_single_frame <- function(frame_info) {
            i <- frame_info$i
            timeIn <- frame_info$timeIn
            framePath <- frame_info$framePath

            # Generate the frame plot
            frame_plot <- generate2DPlotWithAgents(
              pl_base = frame_info$pl_base,
              simulation_log = frame_info$simulation_log,
              timeIn = timeIn,
              folder = frame_info$folder,
              colorFeat = frame_info$colorFeat,
              visualAgent = frame_info$visualAgent,
              visualAgentID = frame_info$visualAgentID,
              Label = frame_info$Label,
              floorSelected = frame_info$floorSelected,
              visualMode = frame_info$visualMode,
              emojiAgents = frame_info$emojiAgents,
              shapeAgents = frame_info$shapeAgents,
              floors = frame_info$floors,
              roomsINcanvas = frame_info$roomsINcanvas,
              AEROSOL_std = frame_info$AEROSOL_std,
              CONTACT_std = frame_info$CONTACT_std,
              initial_time = frame_info$initial_time,
              step = frame_info$step,
              customMax = NULL,
              showAverage = frame_info$showAverage
            )

            # Save frame
            ggplot2::ggsave(framePath, frame_plot, width = 16, height = 12, dpi = 100)
            return(framePath)
          }

          # Prepare frame info list
          frame_info_list <- lapply(seq_along(timeSteps), function(i) {
            list(
              i = i,
              timeIn = timeSteps[i],
              framePath = file.path(framesDir, sprintf("frame_%06d.png", i)),
              pl_base = pl_base,
              simulation_log = simulation_log,
              folder = folder,
              colorFeat = colorFeat,
              visualAgent = visualAgent,
              visualAgentID = visualAgentID,
              Label = Label,
              floorSelected = floorSelected,
              visualMode = visualMode,
              emojiAgents = emojiAgents,
              shapeAgents = shapeAgents,
              floors = floors,
              roomsINcanvas = roomsINcanvas,
              AEROSOL_std = AEROSOL_std_data,
              CONTACT_std = CONTACT_std_data,
              initial_time = initial_time,
              step = step,
              showAverage = showAverage
            )
          })

          # Use mclapply for parallel processing (works on macOS/Linux)
          # On Windows, use parLapply with a cluster
          if (.Platform$OS.type == "unix") {
            results <- parallel::mclapply(frame_info_list, generate_single_frame, mc.cores = n_cores)
          } else {
            # Windows fallback - use PSOCK cluster
            cl <- parallel::makeCluster(n_cores)
            on.exit(parallel::stopCluster(cl), add = TRUE)

            # Export necessary functions and packages to cluster
            parallel::clusterExport(cl, c("generate2DPlotWithAgents"), envir = environment())
            parallel::clusterEvalQ(cl, {
              library(ggplot2)
              library(dplyr)
              if (requireNamespace("emoGG", quietly = TRUE)) library(emoGG)
            })

            results <- parallel::parLapply(cl, frame_info_list, generate_single_frame)
          }

          incProgress(0.8, detail = "Frames generated, creating video...")
        } else {
          # Sequential processing for small frame counts or single core
          for (i in seq_along(timeSteps)) {
            timeIn <- timeSteps[i]

            incProgress(1 / n_frames, detail = paste("Frame", i, "of", n_frames))

            # Use the shared generate2DPlotWithAgents function
            frame_plot <- generate2DPlotWithAgents(
              pl_base = pl_base,
              simulation_log = simulation_log,
              timeIn = timeIn,
              folder = folder,
              colorFeat = colorFeat,
              visualAgent = visualAgent,
              visualAgentID = visualAgentID,
              Label = Label,
              floorSelected = floorSelected,
              visualMode = visualMode,
              emojiAgents = emojiAgents,
              shapeAgents = shapeAgents,
              floors = floors,
              roomsINcanvas = roomsINcanvas,
              AEROSOL_std = AEROSOL_std_data,
              CONTACT_std = CONTACT_std_data,
              initial_time = initial_time,
              step = step,
              customMax = NULL,
              showAverage = showAverage
            )

            # Save frame
            framePath <- file.path(framesDir, sprintf("frame_%06d.png", i))
            ggsave(framePath, frame_plot, width = 16, height = 12, dpi = 100)
          }
        }

        # Use magick package to create video
        outputPath <- file.path(tmpDir, "output_animation.mp4")

        # Get list of frame files
        frameFiles <- list.files(framesDir, pattern = "frame_.*\\.png$", full.names = TRUE)
        frameFiles <- sort(frameFiles)

        if (length(frameFiles) > 0) {
          # Use magick package
          if (requireNamespace("magick", quietly = TRUE)) {
            # Read all frames into magick image stack
            img_stack <- magick::image_read(frameFiles[1])
            if (length(frameFiles) > 1) {
              for (f in frameFiles[-1]) {
                img_stack <- c(img_stack, magick::image_read(f))
              }
            }
            # Set animation delay (100/fps gives centiseconds per frame)
            img_anim <- magick::image_animate(img_stack, fps = fps, dispose = "previous")
            # Write as MP4 video
            magick::image_write_video(img_anim, path = outputPath, framerate = fps)
          } else {
            # Fallback to ffmpeg command
            ffmpegCmd <- sprintf(
              "ffmpeg -y -framerate %d -i '%s/frame_%%06d.png' -c:v libx264 -pix_fmt yuv420p -crf 23 '%s'",
              fps, framesDir, outputPath
            )
            system(ffmpegCmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
          }

          # Copy to output file
          if (file.exists(outputPath)) {
            file.copy(outputPath, file, overwrite = TRUE)
          }
        }

        # Cleanup
        unlink(framesDir, recursive = TRUE)
        if (file.exists(outputPath)) unlink(outputPath)
      }) # end withProgress
    }
  )

  observe({
    is_docker <- file.exists("/.dockerenv")
    if (is_docker) {
      updateSelectInput(session = session, inputId = "run_type", choices = "Docker", selected = "Docker")
    } else {
      updateSelectInput(session = session, inputId = "run_type", choices = c("Local with 3D visualisation", "Local", "Docker"), selected = "Docker")
    }

    output$error_docker <- renderText({
      ""
    })

    is_docker_compose <- Sys.getenv("DOCKER_COMPOSE") == "ON"
    if (is_docker && !is_docker_compose) {
      updateSelectInput(session = session, inputId = "run_type", choices = "", selected = "")
      output$error_docker <- renderText({
        "It is not possible to run a simulation inside the F4F Docker. Use Docker Compose instead."
      })
      output$error_docker_postproc <- renderText({
        "It is not possible to visualise simulation's results using the F4F Docker. Use Docker Compose instead."
      })
      disable("dir")
      disable("LoadFolderPostProc_Button")
    }
  })

  observeEvent(input$SideTabs, {
    is_docker_compose <- Sys.getenv("DOCKER_COMPOSE") == "ON"
    if (is_docker_compose) {
      disable("dir")

      directories <- list.dirs("/usr/local/lib/R/site-library/FORGE4FLAME/FLAMEGPU-FORGE4FLAME/results", recursive = FALSE)
      dir_names <- basename(directories)

      output$Folder_Selection_Compose <- DT::renderDataTable(
        DT::datatable(data.frame(Directory = dir_names),
          options = list(
            columnDefs = list(list(className = "dt-left", targets = 0)),
            pageLength = 5
          ),
          selection = "single",
          rownames = FALSE,
          colnames = c("Directory Name")
        )
      )
    }
  })

  #### END 2D visualisation ####

  vols_dir_results <- F4FgetVolumes(exclude = "")

  shinyDirChoose(input, "dir_results",
    roots = vols_dir_results,
    session = session
  )

  observeEvent(input$run, {
    is_docker_compose <- Sys.getenv("DOCKER_COMPOSE") == "ON"
    if (!is_docker_compose) {
      showModal(
        modalDialog(
          title = "Insert a directory name to identify uniquely this model",
          textInput("popup_text", "Directory name:", ""),
          shinyDirButton("dir_results", "Select Folder", "Upload"),
          verbatimTextOutput("dirResultsPath"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("save_text_run", "Run")
          )
        )
      )
    } else {
      showModal(
        modalDialog(
          title = "Insert a directory name to identify uniquely this model",
          textInput("popup_text", "Directory name:", ""),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("save_text_run", "Run")
          )
        )
      )
    }
  })

  observeEvent(input$dir_results, {
    dirPath <- parseDirPath(vols_dir_results, input$dir_results)
    if (length(dirPath) != 0) {
      output$dirResultsPath <- renderText({
        dirPath
      })
    }
  })

  run_simulation <- reactiveValues(path = "")
  log_active <- reactiveVal(FALSE)

  observeEvent(input$save_text_run, {
    if (input$popup_text == "") {
      shinyalert("Error", "Missing directories name. Please, write one.", type = "error")
      return()
    }

    is_docker_compose <- Sys.getenv("DOCKER_COMPOSE") == "ON"
    if (!is_docker_compose && (is.null(input$dir_results) ||
      (is.numeric(input$dir_results) && input$dir_results <= 1) ||
      (is.list(input$dir_results) && length(input$dir_results$path) > 0 && all(nchar(unlist(input$dir_results$path)) == 0)))) {
      shinyalert("Error", "Missing directories for results. Please, select one.", type = "error")
      return()
    }

    removeModal()

    output$dirResultsPath <- renderText({
      ""
    })

    pathResults <- parseDirPath(vols_dir_results, input$dir_results)

    matricesCanvas <- list()
    for (cID in unique(canvasObjects$roomsINcanvas$CanvasID)) {
      matricesCanvas[[cID]]$floor <- CanvasToMatrix(canvasObjects, canvas = cID)
      matricesCanvas[[cID]]$rooms <- CanvasRoomToMatrix(canvasObjects, canvas = cID)
    }
    canvasObjects$matricesCanvas <- matricesCanvas

    postprocObjects$simulation_log_folder <- NULL
    postprocObjects$simulation_log <- NULL
    postprocObjects$plot_2D <- NULL

    model <- reactiveValuesToList(canvasObjects)
    model_RDS <- model

    out <- FromToMatrices.generation(model)
    model$rooms_whatif <- out$RoomsMeasuresFromTo
    model$agents_whatif <- out$AgentMeasuresFromTo
    model$initial_infected <- out$initial_infected
    model$outside_contagion$percentage_infected <- as.character(model$outside_contagion$percentage_infected)
    model$floorsBG <- NULL

    if (is_docker_compose) {
      system(paste0("mkdir -p FLAMEGPU-FORGE4FLAME/resources/f4f/", input$popup_text))

      file_name <- glue("model.RDs")
      saveRDS(model_RDS, file = file.path(paste0("FLAMEGPU-FORGE4FLAME/resources/f4f/", input$popup_text), file_name))

      file_name <- glue("model.json")
      write_json(x = model, path = file.path(paste0("FLAMEGPU-FORGE4FLAME/resources/f4f/", input$popup_text), file_name))
    } else {
      if (input$run_type == "Docker") {
        system(paste0("mkdir -p Data/", input$popup_text))

        file_name <- glue("model.RDs")
        saveRDS(model_RDS, file = file.path(paste0("Data/", input$popup_text), file_name))

        file_name <- glue("model.json")
        write_json(x = model, path = file.path(paste0("Data/", input$popup_text), file_name))
      } else {
        system(paste0("mkdir -p FLAMEGPU-FORGE4FLAME/resources/f4f/", input$popup_text))

        file_name <- glue("model.RDs")
        saveRDS(model_RDS, file = file.path(paste0("FLAMEGPU-FORGE4FLAME/resources/f4f/", input$popup_text), file_name))

        file_name <- glue("model.json")
        write_json(x = model, path = file.path(paste0("FLAMEGPU-FORGE4FLAME/resources/f4f/", input$popup_text), file_name))
      }
    }

    run_simulation$path <- paste0("FLAMEGPU-FORGE4FLAME/", input$popup_text, "_output.log")
    log_active(TRUE)

    if (is_docker_compose) {
      cmd <- paste0('docker exec -u $UID:$UID flamegpu2-container /usr/bin/bash -c "./abm_ensemble.sh -expdir ', input$popup_text, '" > FLAMEGPU-FORGE4FLAME/', input$popup_text, "_output.log 2>&1")
      system(cmd,
        wait = FALSE, intern = FALSE, ignore.stdout = FALSE,
        ignore.stderr = FALSE, show.output.on.console = TRUE
      )
    } else {
      if (input$run_type == "Docker") {
        cmd <- paste0("docker run --user $UID:$UID --rm --gpus all --runtime nvidia --name FLAMEGPUABM -v ", getwd(), "/Data/", input$popup_text, ":/home/docker/flamegpu2/FLAMEGPU-FORGE4FLAME/resources/f4f/", input$popup_text, " -v ", pathResults, ':/home/docker/flamegpu2/FLAMEGPU-FORGE4FLAME/flamegpu2_results qbioturin/flamegpu2 /usr/bin/bash -c "/home/docker/flamegpu2/FLAMEGPU-FORGE4FLAME/abm_ensemble.sh -expdir ', input$popup_text, '" > FLAMEGPU-FORGE4FLAME/', input$popup_text, "_output.log 2>&1")
        system(cmd,
          wait = FALSE, intern = FALSE, ignore.stdout = FALSE,
          ignore.stderr = FALSE, show.output.on.console = TRUE
        )
      } else if (input$run_type == "Local") {
        cmd <- paste0(
          "cd FLAMEGPU-FORGE4FLAME && nohup ./abm_ensemble.sh -expdir ",
          input$popup_text, " -resdir ", pathResults, " -subdir ON > ", input$popup_text, "_output.log 2>&1"
        )
        system(cmd,
          wait = FALSE, intern = FALSE, ignore.stdout = FALSE,
          ignore.stderr = FALSE, show.output.on.console = TRUE
        )
      } else {
        cmd <- paste0(
          "cd FLAMEGPU-FORGE4FLAME && nohup ./abm.sh -expdir ",
          input$popup_text, " -v ON -resdir ", pathResults, " -subdir ON > ", input$popup_text, "_output.log 2>&1"
        )
        system(cmd,
          wait = FALSE, intern = FALSE, ignore.stdout = FALSE,
          ignore.stderr = FALSE, show.output.on.console = TRUE
        )
      }
    }
  })

  observeEvent(input$stop_run, {
    is_docker_compose <- Sys.getenv("DOCKER_COMPOSE") == "ON"
    if (is_docker_compose) {
      system("docker exec flamegpu2-container pkill -f abm.sh")
      system("docker exec flamegpu2-container pkill -f abm_ensemble.sh")
    } else {
      if (input$run_type == "Docker") {
        system("docker stop FLAMEGPUABM")
      } else {
        system("pkill -f abm.sh")
        system("pkill -f abm_ensemble.sh")
      }
    }
  })

  # Reactive poll that checks for changes in the file every 1 second
  file_data <- reactivePoll(
    intervalMillis = 1000, # Check every 1 second (1000 ms)
    session = session,
    checkFunc = function() {
      if (log_active()) {
        # Check if the file's modification time has changed
        file.info(run_simulation$path)$mtime
      }
    },
    valueFunc = function() {
      if (log_active()) {
        # Return the file content when it changes
        if (file.exists(run_simulation$path)) {
          readLines(run_simulation$path)
        } else {
          "File not found."
        }
      }
    }
  )

  # Output the content of the log file
  output$log_content <- renderText({
    paste(file_data(), collapse = "\n")
  })


  #### Objects in Rooms Section ####

  # Initialize reactive values for objects
  canvasObjects$roomObjects <- list()
  canvasObjects$objectResources <- list()
  canvasObjects$definedObjectTypes <- data.frame(
    ID = numeric(),
    Name = character(),
    Width = numeric(),
    Length = numeric(),
    Color = character(),
    IsObstacle = logical(),
    Capacity = numeric(),
    stringsAsFactors = FALSE
  )

  # Update room selector for objects
  observe({
    if (!is.null(canvasObjects$rooms)) {
      room_choices <- canvasObjects$rooms %>%
        filter(!(type %in% c("Fillingroom", "Stair", "Spawnroom"))) %>%
        pull(Name)
      updateSelectInput(session, "select_room_for_objects",
        choices = c("", room_choices)
      )
      updateSelectInput(session, "copy_objects_from_room",
        choices = c("", room_choices)
      )
    }
  })

  # Update object type selector
  observe({
    if (nrow(canvasObjects$definedObjectTypes) > 0) {
      object_choices <- c("", canvasObjects$definedObjectTypes$Name)
      updateSelectizeInput(session, "select_object_type",
        choices = object_choices
      )
    }
  })

  # Display room dimensions when room is selected
  output$room_dimensions_info <- renderUI({
    req(input$select_room_for_objects)
    if (input$select_room_for_objects == "") {
      return(NULL)
    }

    room_data <- canvasObjects$rooms %>%
      filter(Name == input$select_room_for_objects)

    if (nrow(room_data) > 0) {
      HTML(paste0(
        "<strong>Room: </strong>", room_data$Name, "<br/>",
        "<strong>Width: </strong>", room_data$w, " m<br/>",
        "<strong>Length: </strong>", room_data$l, " m<br/>",
        "<strong>Height: </strong>", room_data$h, " m<br/>",
        "<strong>Type: </strong>", room_data$type
      ))
    }
  })

  # When an object type is selected, populate the form fields
  observeEvent(input$select_object_type, {
    req(input$select_object_type)
    if (input$select_object_type == "") {
      return()
    }

    # Find the object type in defined objects
    obj_type <- canvasObjects$definedObjectTypes %>%
      filter(Name == input$select_object_type)

    if (nrow(obj_type) > 0) {
      obj_type <- obj_type[1, ] # Get the first match (could not be only one due multiple same names)
      updateTextInput(session, "object_name", value = obj_type$Name)
      updateNumericInput(session, "object_width", value = obj_type$Width)
      updateNumericInput(session, "object_length", value = obj_type$Length)
      updateColourInput(session, "object_color", value = obj_type$Color)
      updateCheckboxInput(session, "object_is_obstacle", value = obj_type$IsObstacle)
      if (!obj_type$IsObstacle) {
        updateNumericInput(session, "object_capacity", value = obj_type$Capacity)
      }
    }
  })

  # Update canvas when room is selected
  observeEvent(input$select_room_for_objects, {
    req(input$select_room_for_objects)
    if (input$select_room_for_objects == "") {
      return()
    }

    room_data <- canvasObjects$rooms %>%
      filter(Name == input$select_room_for_objects)

    if (nrow(room_data) > 0) {
      # Get existing objects for this room
      existing_objects <- if (!is.null(canvasObjects$roomObjects[[input$select_room_for_objects]])) {
        canvasObjects$roomObjects[[input$select_room_for_objects]]
      } else {
        list()
      }

      # Send room data to JavaScript
      session$sendCustomMessage("setRoomForObjects", list(
        width = room_data$w,
        length = room_data$l,
        height = room_data$h,
        door = room_data$door,
        objects = existing_objects
      ))
    }
  })

  # Copy objects from another room
  observeEvent(input$copy_objects_btn, {
    req(input$select_room_for_objects)
    req(input$copy_objects_from_room)

    if (input$select_room_for_objects == "") {
      shinyalert("Please select a destination room first.", type = "error")
      return()
    }

    if (input$copy_objects_from_room == "") {
      shinyalert("Please select a room to copy from.", type = "error")
      return()
    }

    if (input$select_room_for_objects == input$copy_objects_from_room) {
      shinyalert("Cannot copy objects from the same room.", type = "error")
      return()
    }

    # Get source room objects
    source_objects <- canvasObjects$roomObjects[[input$copy_objects_from_room]]

    if (is.null(source_objects) || length(source_objects) == 0) {
      shinyalert("The selected room has no objects to copy.", type = "warning")
      return()
    }

    # Get destination room data
    dest_room_data <- canvasObjects$rooms %>%
      filter(Name == input$select_room_for_objects)

    # Check if objects fit in destination room
    valid_objects <- list()
    skipped_objects <- character()

    for (obj in source_objects) {
      if (obj$width <= dest_room_data$w && obj$length <= dest_room_data$l) {
        valid_objects <- append(valid_objects, list(obj))
      } else {
        skipped_objects <- c(skipped_objects, obj$name)
      }
    }

    if (length(valid_objects) == 0) {
      shinyalert("None of the objects from the source room fit in the destination room.", type = "error")
      return()
    }

    # Send all valid objects to JavaScript
    for (obj in valid_objects) {
      session$sendCustomMessage("addObjectToCanvas", list(
        id = obj$id,
        name = obj$name,
        width = obj$width,
        length = obj$length,
        color = obj$color,
        isObstacle = ifelse(is.null(obj$isObstacle), FALSE, obj$isObstacle),
        capacity = obj$capacity,
        x = 1.0,
        y = 1.0
      ))
    }

    # Show success message
    if (length(skipped_objects) > 0) {
      shinyalert(paste0(
        "Copied ", length(valid_objects), " objects successfully. Skipped ",
        length(skipped_objects), " objects that were too large: ",
        paste(skipped_objects, collapse = ", ")
      ), type = "info")
    } else {
      shinyalert(
        paste0(
          "Successfully copied ", length(valid_objects), " objects from ",
          input$copy_objects_from_room, " to ", input$select_room_for_objects
        ),
        type = "success"
      )
    }

    # Clear source selector
    updateSelectInput(session, "copy_objects_from_room", selected = "")
  })

  # Add object to room
  observeEvent(input$add_object_to_room, {
    req(input$select_room_for_objects)
    req(input$object_name)
    req(input$object_width)
    req(input$object_length)

    if (input$select_room_for_objects == "") {
      shinyalert("Please select a room first.", type = "error")
      return()
    }

    if (input$object_name == "") {
      shinyalert("Please enter an object name.", type = "error")
      return()
    }

    if (input$object_width <= 0 || input$object_length <= 0) {
      shinyalert("Object width and length must be greater than 0.", type = "error")
      return()
    }

    # Validate capacity for non-obstacles
    if (!input$object_is_obstacle) {
      if (is.na(input$object_capacity) || input$object_capacity < 1) {
        shinyalert("Agent capacity must be at least 1 for non-obstacle objects.", type = "error")
        return()
      }
    }

    room_data <- canvasObjects$rooms %>%
      filter(Name == input$select_room_for_objects)

    if (input$object_width > room_data$w || input$object_length > room_data$l) {
      shinyalert(paste0(
        "Object dimensions cannot exceed room dimensions (",
        room_data$w, "m × ", room_data$l, "m)."
      ), type = "error")
      return()
    }

    # Add to defined object types if not already present
    obj_name <- input$object_name
    existing_obj <- canvasObjects$definedObjectTypes %>%
      filter(
        Name == obj_name,
        Width == input$object_width,
        Length == input$object_length,
        IsObstacle == input$object_is_obstacle
      )

    if (nrow(existing_obj) == 0) {
      # Add new object type to global list
      new_obj_type <- data.frame(
        ID = ifelse(input$object_is_obstacle, 0, nrow(canvasObjects$definedObjectTypes %>% filter(!IsObstacle)) + 1),
        Name = obj_name,
        Width = input$object_width,
        Length = input$object_length,
        Color = input$object_color,
        IsObstacle = input$object_is_obstacle,
        Capacity = ifelse(input$object_is_obstacle, NA, input$object_capacity),
        stringsAsFactors = FALSE
      )
      canvasObjects$definedObjectTypes <- new_obj_type
    }

    # Send object to JavaScript canvas
    session$sendCustomMessage("addObjectToCanvas", list(
      id = ifelse(input$object_is_obstacle, 0,
        nrow(canvasObjects$definedObjectTypes %>% filter(!IsObstacle))
      ),
      name = input$object_name,
      width = input$object_width,
      length = input$object_length,
      color = input$object_color,
      isObstacle = input$object_is_obstacle,
      capacity = ifelse(input$object_is_obstacle, NA, input$object_capacity),
      x = 1.0, # Default position (snapped to grid)
      y = 1.0 # Default position (snapped to grid)
    ))

    # Clear inputs
    updateTextInput(session, "object_name", value = "")
    updateSelectizeInput(session, "select_object_type", selected = "")
  })


  # Update objects data when canvas changes
  observeEvent(input$objects_updated, {
    req(input$select_room_for_objects)

    if (input$select_room_for_objects != "") {
      canvasObjects$roomObjects[[input$select_room_for_objects]] <- input$objects_updated$objects
    }
  })

  # Display objects table
  output$objects_table <- DT::renderDataTable({
    req(input$select_room_for_objects)

    if (input$select_room_for_objects == "" ||
      is.null(canvasObjects$roomObjects[[input$select_room_for_objects]])) {
      return(data.frame(
        ID = numeric(),
        Name = character(),
        X = numeric(),
        Y = numeric(),
        Width = numeric(),
        Length = numeric(),
        Color = character(),
        Obstacle = logical(),
        Capacity = integer(),
        stringsAsFactors = FALSE
      ))
    }

    objects_list <- canvasObjects$roomObjects[[input$select_room_for_objects]]

    if (length(objects_list) == 0) {
      return(data.frame(
        ID = numeric(),
        Name = character(),
        X = numeric(),
        Y = numeric(),
        Width = numeric(),
        Length = numeric(),
        Color = character(),
        Obstacle = logical(),
        Capacity = integer(),
        stringsAsFactors = FALSE
      ))
    }

    objects_df <- do.call(rbind, lapply(objects_list, function(obj) {
      data.frame(
        Name = obj$name,
        X = round(obj$x, 2),
        Y = round(obj$y, 2),
        Width = obj$width,
        Length = obj$length,
        Color = obj$color,
        Obstacle = ifelse(is.null(obj$isObstacle), FALSE, obj$isObstacle),
        Capacity = ifelse(is.null(obj$capacity) || is.na(obj$capacity), NA, obj$capacity),
        stringsAsFactors = FALSE
      )
    }))

    DT::datatable(
      objects_df,
      selection = "single",
      options = list(
        pageLength = 10,
        searching = TRUE,
        ordering = TRUE
      )
    )
  })

  # Remove selected object
  observeEvent(input$remove_selected_object, {
    req(input$select_room_for_objects)
    req(input$objects_table_rows_selected)

    selected_row <- input$objects_table_rows_selected - 1 # JavaScript uses 0-based indexing

    session$sendCustomMessage("removeObjectFromCanvas", selected_row)
  })

  # Clear all objects
  observeEvent(input$clear_all_objects, {
    req(input$select_room_for_objects)

    shinyalert(
      title = "Clear All Objects",
      text = "Are you sure you want to remove all objects from this room?",
      type = "warning",
      showCancelButton = TRUE,
      confirmButtonText = "Yes, clear all",
      cancelButtonText = "Cancel",
      callbackR = function(value) {
        if (value) {
          session$sendCustomMessage("clearAllObjects", list())
          canvasObjects$roomObjects[[input$select_room_for_objects]] <- list()
        }
      }
    )
  })
}
