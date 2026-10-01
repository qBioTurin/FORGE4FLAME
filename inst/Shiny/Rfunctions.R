CheckEntryExit = function(EntryTime, ExitTime, listTimes){
  if(EntryTime == ""){
    EntryTime = "00:00"
  }
  if(ExitTime == ""){
    ExitTime = "23:59"
  }

  if(EntryTime != ""){
    if (! (grepl("^([01]?[0-9]|2[0-3]):[0-5][0-9]$", EntryTime) || grepl("^\\d{1,2}$", EntryTime)) )
    {
      return(c("Error", "The format of the time should be: hh:mm (e.g. 06:15, or 20)."))
    }
  }
  if(grepl("^\\d{1,2}$", EntryTime)){
    EntryTime <- paste0(EntryTime,":00")
  }

  if(ExitTime != ""){
    if (! (grepl("^([01]?[0-9]|2[0-3]):[0-5][0-9]$",ExitTime) || grepl("^\\d{1,2}$",ExitTime)) )
    {
      return(c("Error","The format of the time should be: hh:mm (e.g. 06:15, or 20:30)"))

    }
  }
  if(grepl("^\\d{1,2}$", ExitTime)){
    ExitTime <- paste0(ExitTime,":00")
  }


  #check if the number before : in EntryTime is lower than number before : in ExitTime
  if(as.numeric(strsplit(EntryTime, ":")[[1]][1]) > as.numeric(strsplit(ExitTime, ":")[[1]][1])) {
    return(c("Error", "The Entry time should be lower than the Exit time."))

  }
  if (as.numeric(strsplit(EntryTime, ":")[[1]][1]) == as.numeric(strsplit(ExitTime, ":")[[1]][1]) &&
      as.numeric(strsplit(EntryTime, ":")[[1]][2]) > as.numeric(strsplit(ExitTime, ":")[[1]][2])) {
    return(c("Error", "The Entry time should be lower than the Exit time."))

  }

  new_slot = paste0(EntryTime," - ", ExitTime)
  # check if it is overlapping
  if(!is.null(listTimes)){
    new_times <- parse_time_slot(new_slot)

    for (slot in listTimes) {
      existing_times <- parse_time_slot(slot)

      if (new_times$start < existing_times$end &&
          new_times$end > existing_times$start) {
        return(c("Error", "The time slot overlaps with the existing slots w.r.t the selected room."))  # Overlap detected
      }
    }
  }

  return(new_slot)
}

parse_time_slot <- function(slot) {
  times <- str_split(slot, " - ", simplify = TRUE)
  list(
    start = lubridate::hm(times[1]),
    end   = lubridate::hm(times[2])
  )
}

theme_fancy <- function() {
  theme_minimal() +
    theme(
      plot.background = element_rect(fill = "#2b2b2b", color = NA),
      panel.background = element_rect(fill = "#3c3c3c", color = NA),
      panel.grid.major = element_line(color = "#666666", size = 0.3),
      panel.grid.minor = element_line(color = "#444444", size = 0.2),
      axis.text = element_text(color = "white", size = 10),
      axis.title = element_text(color = "white", size = 12, face = "bold"),
      plot.title = element_text(color = "white", size = 14, face = "bold", hjust = 0.5),
      legend.background = element_rect(fill = "#2b2b2b"),
      legend.text = element_text(color = "white", size = 10),
      legend.key.size = unit(0.8, "cm"),
      legend.key = element_rect(fill = "white"),
      legend.title = element_text(color = "white", face = "bold", size = 10),
      legend.position = "bottom",
      strip.background = element_rect(fill = "white", color = "white"),
      strip.text = element_text(color = "black", size = 7, face = "bold")
    )
}

find_ones_submatrix_coordinates <- function(mat, target_rows, target_cols) {
  # Dimensions describe the interior. Reserve the complete one-cell wall ring,
  # allowing an existing zero-valued wall to be shared with the new room.
  last_y <- nrow(mat) - target_rows - 2
  last_x <- ncol(mat) - target_cols - 2
  if (last_y < 1 || last_x < 1) return(NULL)
  for (y in seq_len(last_y)) {
    for (x in seq_len(last_x)) {
      if (all(mat[y + 0:(target_rows + 1), x + 0:(target_cols + 1)] == 0)) return(c(y, x))
    }
  }
  NULL
}

read_dxf_units <- function(dxf_path) {
  # Read only first few hundred lines (HEADER usually near top)
  hdr <- readLines(dxf_path, n = 500, warn = FALSE)

  # Find the line containing "$INSUNITS"
  idx <- which(hdr == "$INSUNITS")
  if (length(idx) == 0) {
    warning("No $INSUNITS found; assuming unitless (treat as mm).")
    return(list(code = NA, unit = "unknown", factor = 0.001))
  }

  # Next lines: group code 70 then the integer value
  # hdr[idx+1] should be "70", hdr[idx+2] the numeric code
  code <- as.integer(hdr[idx + 2])

  # Map DXF unit codes → unit names & conversion to meters
  map <- list(
    `0` = list(unit = "unitless", factor = 1),
    `1` = list(unit = "inches",   factor = 0.0254),
    `2` = list(unit = "feet",     factor = 0.3048),
    `3` = list(unit = "miles",    factor = 1609.344),
    `4` = list(unit = "millimeters", factor = 0.001),
    `5` = list(unit = "centimeters", factor = 0.01),
    `6` = list(unit = "meters",      factor = 1),
    `7` = list(unit = "kilometers",  factor = 1000)
    # add more codes if needed
  )

  if (!as.character(code) %in% names(map)) {
    warning("Unrecognized INSUNITS code: ", code,
            "; defaulting to millimeters.")
    return(list(code = code, unit = "millimeters", factor = 0.001))
  }

  unit_info <- map[[as.character(code)]]
  c(code = code, unit = unit_info$unit, factor = unit_info$factor)
}

detect_units_by_bbox <- function(plan_raw) {
  bb <- st_bbox(plan_raw)
  w  <- bb["xmax"] - bb["xmin"]
  h  <- bb["ymax"] - bb["ymin"]
  d  <- sqrt(w^2 + h^2)

  if (d > 5000) {
    c(unit = "millimeters", factor = 0.001)
  } else if (d > 500) {
    c(unit = "centimeters", factor = 0.01)
  } else if (d > 50) {
    c(unit = "meters", factor = 1)
  } else {
    c(unit = "unknown", factor = NA_real_)
  }
}

sendBG <- function(gg,canvas_w,canvas_h,session, canvasSelect){
  # 7. Save the plot in www folder so that Shiny can serve it
  tmpfile <- tempfile(pattern = paste0("floorBG", canvasSelect), fileext = ".png")

  ggsave(tmpfile, gg, width = canvas_w, height = canvas_h, units = "px")

  # Create a public URL that Shiny can serve dynamically
  file_url <- session$fileUrl(basename(tmpfile), tmpfile)

  session$sendCustomMessage("bgImageChanged", list(
    imgFile = file_url,
    wBG = canvas_w,
    hBG = canvas_h
  ))
}

# Room x/y are one-based top/left wall indices. l/w count only interior cells;
# the opposite wall indices are x+l+1 and y+w+1. Canvas pixels = wall index * 10.
canvas_points_outside_rooms <- function(x, y, rooms) {
  outside <- rep(TRUE, length(x))
  if (!is.null(rooms)) for (i in seq_len(nrow(rooms))) {
    r <- rooms[i, ]
    outside <- outside & !(x >= r$x & x <= r$x + ceiling(r$l) + 1 &
                            y >= r$y & y <= r$y + ceiling(r$w) + 1)
  }
  outside
}

# FALSE keeps room IDs, TRUE uses 1 for interiors, and "NoInterior" uses 1
# only where a single room occupies the cell (overlapping interiors become 0).
CanvasToMatrix = function(canvasObjects,FullRoom = FALSE,canvas){
  matrixCanvas = matrix(0,
                        nrow = canvasObjects$canvasDimension$canvasHeight/10,
                        ncol = canvasObjects$canvasDimension$canvasWidth/10)
  masked_cells <- matrix(FALSE, nrow = nrow(matrixCanvas), ncol = ncol(matrixCanvas))
  rooms <- NULL

  doors <- sync_room_doors(canvasObjects$doorsINcanvas, canvasObjects$roomsINcanvas)

  doors <- doors[doors$CanvasID == canvas, , drop = FALSE]

  if(!is.null(canvasObjects$roomsINcanvas)){
    rooms = normalize_canvas_rooms(canvasObjects$roomsINcanvas) %>% filter(CanvasID == canvas)
    rooms <- rooms[order(-ceiling(rooms$l) * ceiling(rooms$w), rooms$ID), , drop = FALSE]
    for(i in rooms$ID){
      r = rooms %>% filter(ID == i)

      x = r$x
      y = r$y

      r$l <- ceiling(r$l)
      r$w <- ceiling(r$w)

      if (FullRoom == "NoInterior") {
        matrixCanvas[y + seq_len(r$w), x + seq_len(r$l)] <-
          matrixCanvas[y + seq_len(r$w), x + seq_len(r$l)] + 1
      } else {
        matrixCanvas[y + seq_len(r$w), x + seq_len(r$l)] <- if (FullRoom) 1 else i
      }

    }

    if (FullRoom == "NoInterior") masked_cells <- matrixCanvas > 1

    # Walls are one grid cell wide. Draw every wall after the interiors so a
    # neighbouring room cannot overwrite a shared wall with traversable cells.
    for (i in seq_len(nrow(rooms))) {
      r <- rooms[i, ]
      xs <- r$x + 0:(ceiling(r$l) + 1)
      ys <- r$y + 0:(ceiling(r$w) + 1)
      matrixCanvas[c(r$y, r$y + ceiling(r$w) + 1), xs] <- 0
      matrixCanvas[ys, c(r$x, r$x + ceiling(r$l) + 1)] <- 0
    }

    # for (i in seq_len(nrow(rooms))) {
    #   r <- rooms[i, ]
    #   if (r$type != "Fillingroom" && any(room_interior_mask(r, rooms))) {
    #     matrixCanvas[r$center_y, r$center_x] <- roomNames$ID[roomNames$Name == r$Name]
    #   }
    # }
  }

  # Both memberships of a shared door have the same matrix coordinates.
  # Open the wall only after all interiors and walls have been drawn.
  if (nrow(doors)){
    valid_doors <- doors$x >= 1 & doors$x <= ncol(matrixCanvas) &
      doors$y >= 1 & doors$y <= nrow(matrixCanvas)
    matrixCanvas[cbind(doors$y[valid_doors], doors$x[valid_doors])] <- 2

    for(d in which(valid_doors)){
      door = doors[d,]
      if(door$side == "top"){
        matrixCanvas[door$y + 1, door$x] = rooms$typeID[rooms$ID == door$roomID]
      } else if(door$side == "bottom"){
        matrixCanvas[door$y - 1, door$x] =  rooms$typeID[rooms$ID == door$roomID]
      }else if(door$side == "left"){
        matrixCanvas[door$y, door$x + 1] =  rooms$typeID[rooms$ID == door$roomID]
      }else if(door$side == "right"){
        matrixCanvas[door$y, door$x - 1] =  rooms$typeID[rooms$ID == door$roomID]
      }
    }
    # A neighbouring wall door can open an adjacent cell occupied by a freely
    # positioned imported door. Door cells have final precedence.
    matrixCanvas[cbind(doors$y[valid_doors], doors$x[valid_doors])] <- 2
    if (identical(FullRoom, "NoInterior") && !is.null(rooms)) {
      internal_rooms <- rooms$ID[rooms$containerID != rooms$ID]
      internal_doors <- valid_doors & doors$ownerRoomID %in% internal_rooms
      if (any(internal_doors)) {
        matrixCanvas[cbind(doors$y[internal_doors], doors$x[internal_doors])] <- 0
      }
    }
  }

  if(!is.null(canvasObjects$nodesINcanvas)){
    nodes = canvasObjects$nodesINcanvas %>% filter(CanvasID == canvas)
    floor_rooms <- canvasObjects$roomsINcanvas
    if (!is.null(floor_rooms)) floor_rooms <- floor_rooms[floor_rooms$CanvasID == canvas, , drop = FALSE]
    # Old graph points must never replace a wall or a door with a 3.
    nodes <- nodes[canvas_points_outside_rooms(nodes$x + 1, nodes$y + 1, floor_rooms), , drop = FALSE]
    for(i in nodes$ID){
      r = nodes %>% filter(ID == i)
      matrixCanvas[r$y + 1, r$x + 1] = 3
    }
  }

  # Door approach markers must not reopen an overlapping room interior.
  matrixCanvas[masked_cells] <- 0
  return(matrixCanvas)
}

# Get rotation angle in degrees based on door position
# Door values: "bottom", "top", "right", "left"
# Bottom: 0 degrees (no rotation)
# Left: 90 degrees clockwise
# up: 180 degrees
# Right: 270 degrees (or -90, counter-clockwise)
get_rotation_angle <- function(door_position) {
  door_position <- tolower(door_position)
  switch(door_position,
    "bottom" = 0,
    "top" = 180,
    "left" = 90,
    "right" = 270,
    0)
}

# Rotate a matrix 90 degrees clockwise
rotate_matrix_90 <- function(mat) {
  # Transpose and reverse rows
  t(apply(mat, 2, rev))
}

# Rotate a matrix by specified angle (90, 180, or 270 degrees)
rotate_matrix <- function(mat, angle) {
  angle <- angle %% 360

  if (angle == 0) {
    return(mat)
  } else if (angle == 90) {
    return(rotate_matrix_90(mat))
  } else if (angle == 180) {
    return(rotate_matrix_90(rotate_matrix_90(mat)))
  } else if (angle == 270) {
    return(rotate_matrix_90(rotate_matrix_90(rotate_matrix_90(mat))))
  } else {
    return(mat)
  }
}

empty_canvas_doors <- function() {
  data.frame(ID = integer(), roomID = integer(), CanvasID = character(),
             side = character(), offset = integer(), x = numeric(), y = numeric(),
             ownerRoomID = integer(), wall_x = numeric(), wall_y = numeric(),
             local_x = numeric(), local_y = numeric())
}

# A physical door is anchored to one room's wall. Other memberships are derived
# from geometry, including a membership inside a surrounding room (side=interior).
canvas_door_anchors <- function(doors) {
  if (is.null(doors) || !nrow(doors)) return(empty_canvas_doors())
  if (!"ownerRoomID" %in% names(doors)) {
    doors <- doors[!duplicated(doors$ID), , drop = FALSE]
    doors$ownerRoomID <- doors$roomID
  } else {
    doors <- doors[doors$roomID == doors$ownerRoomID, , drop = FALSE]
  }
  doors
}

shared_door_peer <- function(room, side, offset, rooms) {
  candidates <- rooms[rooms$ID != room$ID & rooms$CanvasID == room$CanvasID &
                        rooms$type != "Fillingroom", , drop = FALSE]
  if (side == "interior") {
    px <- room$x + offset[1]
    py <- room$y + offset[2]
    valid <- px >= candidates$x & px <= candidates$x + ceiling(candidates$l) + 1 &
      py >= candidates$y & py <= candidates$y + ceiling(candidates$w) + 1
    candidates <- candidates[valid, , drop = FALSE]
    if (!nrow(candidates)) return(data.frame(roomID = integer(), side = character(), offset = integer()))
    peer <- candidates[order(ceiling(candidates$l) * ceiling(candidates$w), candidates$ID)[1], , drop = FALSE]
    return(data.frame(roomID = peer$ID, side = "interior", offset = NA_real_))
  }
  horizontal <- side %in% c("top", "bottom")
  px <- room$x + if (horizontal) offset else if (side == "right") ceiling(room$l) + 1 else 0
  py <- room$y + if (!horizontal) offset else if (side == "bottom") ceiling(room$w) + 1 else 0
  # A door is one wall cell. The next cell beyond it must be inside the peer,
  # not on another wall. This also excludes corner-only contact.
  ex <- px + switch(side, left = -1, right = 1, 0)
  ey <- py + switch(side, top = -1, bottom = 1, 0)
  valid <- ex >= candidates$x + 1 & ex <= candidates$x + ceiling(candidates$l) &
    ey >= candidates$y + 1 & ey <= candidates$y + ceiling(candidates$w)
  candidates <- candidates[valid, , drop = FALSE]
  if (!nrow(candidates)) return(data.frame(roomID = integer(), side = character(), offset = integer()))
  peer <- candidates[order(ceiling(candidates$l) * ceiling(candidates$w), candidates$ID)[1], , drop = FALSE]
  opposite <- c(top = "bottom", bottom = "top", left = "right", right = "left")
  boundary <- switch(side, top = py == peer$y + ceiling(peer$w) + 1, bottom = py == peer$y,
                     left = px == peer$x + ceiling(peer$l) + 1, right = px == peer$x)
  data.frame(roomID = peer$ID, side = if (boundary) unname(opposite[side]) else "interior",
             offset = if (boundary) (if (horizontal) px - peer$x else py - peer$y) else NA_real_)
}

sync_room_doors <- function(doors, rooms) {
  anchors <- canvas_door_anchors(doors)
  if (!nrow(anchors)) return(empty_canvas_doors())
  if (is.null(rooms) || !nrow(rooms)) stop("Doors must belong to a room.")
  if (anyNA(anchors$ID) || anyDuplicated(anchors$ID) || anyDuplicated(rooms$ID)) stop("Duplicate room or door ID.")
  index <- match(anchors$roomID, rooms$ID)
  if (anyNA(index)) stop("A door refers to an unknown room.")
  r <- rooms[index, , drop = FALSE]
  if (!"local_x" %in% names(anchors)) anchors$local_x <- anchors$x - r$x
  if (!"local_y" %in% names(anchors)) anchors$local_y <- anchors$y - r$y
  interior <- anchors$side == "interior"
  horizontal <- anchors$side %in% c("top", "bottom")
  wall_length <- ifelse(horizontal, ceiling(r$l), ceiling(r$w))
  invalid_wall <- !interior & (!is.finite(anchors$offset) | anchors$offset != floor(anchors$offset) |
    anchors$offset < 1 | anchors$offset > wall_length)
  invalid_interior <- interior & (!is.finite(anchors$local_x) | !is.finite(anchors$local_y) |
    anchors$local_x != floor(anchors$local_x) | anchors$local_y != floor(anchors$local_y))
  if (anyNA(anchors$side) || any(!anchors$side %in% c("top", "bottom", "left", "right", "interior")) ||
      any(invalid_wall) || any(invalid_interior) || any(r$type == "Fillingroom")) {
    stop("Doors must occupy a valid wall cell of a non-filling room.")
  }
  anchors$CanvasID <- r$CanvasID
  anchors$ownerRoomID <- r$ID
  anchors$local_x[!interior] <- ifelse(horizontal[!interior], anchors$offset[!interior],
    ifelse(anchors$side[!interior] == "right", ceiling(r$l[!interior]) + 1, 0))
  anchors$local_y[!interior] <- ifelse(!horizontal[!interior], anchors$offset[!interior],
    ifelse(anchors$side[!interior] == "bottom", ceiling(r$w[!interior]) + 1, 0))
  anchors$x <- r$x + anchors$local_x
  anchors$y <- r$y + anchors$local_y
  anchors$wall_x <- anchors$x
  anchors$wall_y <- anchors$y
  # Two clicks on opposite sides of the same physical opening create one door.
  anchors <- anchors[order(anchors$ID), names(empty_canvas_doors()), drop = FALSE]
  anchors <- anchors[!duplicated(anchors[c("CanvasID", "wall_x", "wall_y")]), , drop = FALSE]
  result <- lapply(seq_len(nrow(anchors)), function(i) {
    door <- anchors[i, , drop = FALSE]
    room <- rooms[rooms$ID == door$ownerRoomID, , drop = FALSE]
    peer_offset <- if (door$side == "interior") c(door$local_x, door$local_y) else door$offset
    peer <- shared_door_peer(room, door$side, peer_offset, rooms)
    if (!nrow(peer)) return(door)
    membership <- door
    membership$roomID <- peer$roomID
    membership$side <- peer$side
    membership$offset <- peer$offset
    # One physical opening, one global wall cell, even for an interior membership.
    membership$x <- door$x
    membership$y <- door$y
    rbind(door, membership)
  })
  result <- do.call(rbind, result)

  rownames(result) <- NULL
  result
}

# Distances are in metres (one matrix cell). Use physical door anchors once,
# and wall coordinates rather than the interior dimensions alone. Merely
# adjacent rooms are excluded, so a door on their common wall remains valid.
room_door_clearance_conflicts <- function(rooms, doors, changed_room_ids = rooms$ID,
                                          min_distance = 2) {
  conflicts <- data.frame(doorID = integer(), ownerRoomID = integer(),
                          roomID = integer(), distance = numeric())
  if (is.null(rooms) || nrow(rooms) < 2 || is.null(doors) || !nrow(doors)) return(conflicts)
  anchors <- canvas_door_anchors(sync_room_doors(doors, rooms))
  for (i in seq_len(nrow(anchors))) {
    door <- anchors[i, , drop = FALSE]
    owner <- rooms[rooms$ID == door$ownerRoomID, , drop = FALSE]
    others <- rooms[rooms$CanvasID == owner$CanvasID & rooms$ID != owner$ID &
                      (owner$ID %in% changed_room_ids | rooms$ID %in% changed_room_ids), , drop = FALSE]
    if (!nrow(others)) next
    right <- others$x + ceiling(others$l) + 1
    bottom <- others$y + ceiling(others$w) + 1
    overlap <- pmin(right, owner$x + ceiling(owner$l) + 1) > pmax(others$x, owner$x) &
      pmin(bottom, owner$y + ceiling(owner$w) + 1) > pmax(others$y, owner$y)
    # Point-to-segment distance, including the ends of each wall segment.
    dx <- pmax(others$x - door$x, 0, door$x - right)
    dy <- pmax(others$y - door$y, 0, door$y - bottom)
    distance <- pmin(sqrt((door$x - others$x)^2 + dy^2),
                     sqrt((door$x - right)^2 + dy^2),
                     sqrt(dx^2 + (door$y - others$y)^2),
                     sqrt(dx^2 + (door$y - bottom)^2))
    bad <- which(overlap & distance < min_distance - 1e-9)
    if (length(bad)) conflicts <- rbind(conflicts,
      data.frame(doorID = door$ID, ownerRoomID = owner$ID,
                 roomID = others$ID[bad], distance = distance[bad]))
  }
  conflicts
}

room_door_clearance_message <- function(conflicts) {
  first <- conflicts[1, ]
  sprintf(paste0("Door %s of room %s is %.2f m from the border of room %s. ",
                 "Overlapping rooms must keep at least 2 m between doors and the other room's borders."),
          first$doorID, first$ownerRoomID, first$distance, first$roomID)
}

invalid_shared_door_ids <- function(doors, rooms) {
  updated <- sync_room_doors(doors, rooms)
  ids <- unique(doors$ID[duplicated(doors$ID)])
  ids[vapply(ids, function(id) {
    !setequal(doors$roomID[doors$ID == id], updated$roomID[updated$ID == id])
  }, logical(1))]
}

new_room_door <- function(room, side, offset, id) {
  sync_room_doors(data.frame(ID = id, roomID = room$ID, CanvasID = room$CanvasID,
                            side = side, offset = offset, x = 0, y = 0), room)
}

add_canvas_door <- function(doors, rooms, roomID, side, offset) {
  doors <- sync_room_doors(doors, rooms)
  room <- rooms[rooms$ID == roomID, , drop = FALSE]
  if (nrow(room) != 1) stop("Please select a room.")
  door <- new_room_door(room, side, offset, max(c(0, doors$ID)) + 1L)
  sync_room_doors(rbind(doors, door), rooms)
}

room_doors_for_canvas <- function(doors, roomID) {
  # Only draw the owner's copy; a containing room must not paint ghost doors
  # after its child has been hidden behind another layer.
  doors <- doors[doors$roomID == roomID & doors$ownerRoomID == roomID, , drop = FALSE]
  doors[, c("ID", "side", "offset", "local_x", "local_y"), drop = FALSE]
}

# Geometry ownership is independent of visual layering: smaller rooms carve
# their footprint out of larger rooms; ID resolves equal-area overlaps.
room_interior_mask <- function(room, rooms) {
  mask <- matrix(TRUE, nrow = ceiling(room$w), ncol = ceiling(room$l))
  own_area <- ceiling(room$l) * ceiling(room$w)
  others <- rooms[rooms$CanvasID == room$CanvasID & rooms$ID != room$ID, , drop = FALSE]
  grid_x <- room$x + seq_len(ncol(mask))
  grid_y <- room$y + seq_len(nrow(mask))
  for (i in seq_len(nrow(others))) {
    other <- others[i, ]
    right <- other$x + ceiling(other$l) + 1
    bottom <- other$y + ceiling(other$w) + 1
    xs <- which(grid_x >= other$x & grid_x <= right)
    ys <- which(grid_y >= other$y & grid_y <= bottom)
    other_area <- ceiling(other$l) * ceiling(other$w)
    if (other_area < own_area || (other_area == own_area && other$ID > room$ID)) {
      # Exclude both the child interior and its surrounding wall cells.
      mask[ys, xs] <- FALSE
    } else {
      mask[which(grid_y %in% c(other$y, bottom)), xs] <- FALSE
      mask[ys, which(grid_x %in% c(other$x, right))] <- FALSE
    }
  }
  mask
}

# Warn about possible object coverage without requiring an exact collision.
# Use the same geometry as the exported room matrices, including partial overlaps.
room_object_overlap_message <- function(rooms, room_objects, changed_room_ids) {
  if (is.null(rooms) || nrow(rooms) < 2 || !length(room_objects)) return(NULL)
  affected <- vapply(seq_len(nrow(rooms)), function(i) {
    room <- rooms[i, , drop = FALSE]
    if (!length(room_objects[[room$Name]])) return(FALSE)
    relevant <- if (room$ID %in% changed_room_ids) rooms else {
      rooms[rooms$ID %in% c(room$ID, changed_room_ids), , drop = FALSE]
    }
    any(!room_interior_mask(room, relevant))
  }, logical(1))
  if (!any(affected)) return(NULL)
  labels <- paste0(rooms$Name[affected], " #", rooms$ID[affected],
                   " (", rooms$CanvasID[affected], ")")
  paste0("Overlapping rooms may cover objects in: ", paste(labels, collapse = ", "),
         ". Check the room layouts and reposition any affected objects.")
}

add_room_containment_metadata <- function(rooms) {
  if (is.null(rooms) || !nrow(rooms)) return(rooms)
  rooms$containerID <- as.integer(rooms$ID)
  rooms$containedRooms <- 0L
  for (canvas in unique(rooms$CanvasID)) {
    indices <- which(rooms$CanvasID == canvas)
    left <- rooms$x[indices]
    top <- rooms$y[indices]
    right <- left + ceiling(rooms$l[indices]) + 1
    bottom <- top + ceiling(rooms$w[indices]) + 1
    contains <- outer(left, left, `<=`) & outer(top, top, `<=`) &
      outer(right, right, `>=`) & outer(bottom, bottom, `>=`)
    strict <- outer(left, left, `!=`) | outer(top, top, `!=`) |
      outer(right, right, `!=`) | outer(bottom, bottom, `!=`)
    contains <- contains & strict
    rooms$containedRooms[indices] <- as.integer(rowSums(contains))
    footprint <- (right - left + 1) * (bottom - top + 1)
    for (j in seq_along(indices)) {
      containers <- which(contains[, j])
      if (!length(containers)) next
      outermost <- containers[order(-footprint[containers], rooms$ID[indices][containers])[1]]
      rooms$containerID[indices[j]] <- as.integer(rooms$ID[indices[outermost]])
    }
  }
  rooms
}

normalize_canvas_rooms <- function(rooms) {
  if (is.null(rooms) || !nrow(rooms)) return(rooms)
  if (!"z_index" %in% names(rooms)) {
    rooms$z_index <- rank(-ceiling(rooms$l) * ceiling(rooms$w), ties.method = "first")
  }
  if (any(!is.finite(rooms$z_index))) stop("Invalid room drawing order.")
  for (i in seq_len(nrow(rooms))) {
    r <- rooms[i, ]
    # The room centre describes its geometry and remains fixed even when that
    # cell is covered by another room.
    rooms$center_x[i] <- r$x + floor((ceiling(r$l) + 1) / 2)
    rooms$center_y[i] <- r$y + floor((ceiling(r$w) + 1) / 2)
  }
  add_room_containment_metadata(rooms)
}

normalize_room_doors <- function(model) {
  rooms <- model$roomsINcanvas
  doors <- model$doorsINcanvas
  if (!is.null(rooms) && nrow(rooms) > 0) {
    if (!"object_rotation" %in% names(rooms)) {
      rooms$object_rotation <- if ("door" %in% names(rooms)) {
        vapply(rooms$door, get_rotation_angle, numeric(1))
      } else rep(0, nrow(rooms))
    }
    # Migrate the old single-door schema only when the separate table is absent.
    if (is.null(doors) && "door" %in% names(rooms)) {
      doors <- empty_canvas_doors()
      for (i in seq_len(nrow(rooms))) {
        r <- rooms[i, , drop = FALSE]
        if (r$type == "Fillingroom" || !r$door %in% c("top", "bottom", "left", "right")) next
        horizontal <- r$door %in% c("top", "bottom")
        offset <- if (horizontal) floor(ceiling(r$l) / 2) + 1 else if (r$door == "left") {
          round(ceiling(r$w) / 2) + 1
        } else floor(ceiling(r$w) / 2) + 1
        # The old matrix builder recalculated midpoint doors and centres each
        # time; saved coordinates can be stale after a drag or dimension change.
        rooms$center_x[i] <- r$x + if (horizontal) offset else if (r$door == "left") {
          ceiling((ceiling(r$l) + 1) / 2)
        } else floor((ceiling(r$l) + 1) / 2)
        rooms$center_y[i] <- r$y + if (!horizontal) offset else if (r$door == "top") {
          ceiling((ceiling(r$w) + 1) / 2)
        } else floor((ceiling(r$w) + 1) / 2)
        doors <- rbind(doors, new_room_door(r, r$door, offset, nrow(doors) + 1L))
      }
    }
    for (axis in c("x", "y")) {
      center <- paste0("center_", axis)
      size <- ceiling(rooms[[if (axis == "x") "l" else "w"]])
      if (is.null(rooms[[center]])) rooms[[center]] <- rep(NA_real_, nrow(rooms))
      invalid <- !is.finite(rooms[[center]]) | rooms[[center]] <= rooms[[axis]] |
        rooms[[center]] > rooms[[axis]] + size
      rooms[[center]][invalid] <- (rooms[[axis]] + floor((size + 1) / 2))[invalid]
    }
  }
  if (!is.null(rooms)) rooms <- rooms[, setdiff(names(rooms), c("door", "door_x", "door_y")), drop = FALSE]
  rooms <- normalize_canvas_rooms(rooms)
  model$roomsINcanvas <- rooms
  model$doorsINcanvas <- sync_room_doors(doors, rooms)
  model
}

CanvasRoomToMatrix = function(canvasObjects, FullRoom = FALSE, canvas){
  if (!isFALSE(FullRoom) && !identical(FullRoom, "NoInterior")) {
    stop("FullRoom must be FALSE or 'NoInterior'.")
  }
  rooms <- normalize_canvas_rooms(canvasObjects$roomsINcanvas) %>% filter(CanvasID == canvas)
  doors <- sync_room_doors(canvasObjects$doorsINcanvas, canvasObjects$roomsINcanvas)
  if (!nrow(rooms)) return(list())

  room_matrix <- function(room) {
    n <- room$Name

    objects_list <- canvasObjects$roomObjects[[n]]
    objects_df <- data.frame()

    if (length(objects_list)) {
      objects_df <- do.call(rbind, lapply(objects_list, function(obj) {
        data.frame(
          Name = obj$name,
          ID = obj$id,
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
    }

    rotation <- if (is.null(room$object_rotation)) 0 else room$object_rotation
    # Canvas dimensions already include the placement rotation. Reconstruct
    # the original layout dimensions before drawing and rotating the objects.
    if(!rotation %in% c(90, 270)){
      roomLength = ceiling(room$l)
      roomWidth = ceiling(room$w)
    }
    else{
      roomLength = ceiling(room$w)
      roomWidth = ceiling(room$l)
    }

    matrixCanvas <- matrix(1, nrow = roomWidth + 2, ncol = roomLength + 2)

    matrixCanvas[1, ] <- 0
    matrixCanvas[, 1] <- 0
    matrixCanvas[nrow(matrixCanvas), ] <- 0
    matrixCanvas[, ncol(matrixCanvas)] <- 0

    # Object coordinates refer to the unrotated room layout.
    if (nrow(objects_df)) {
      for (i in seq_len(nrow(objects_df))) {
        r <- objects_df[i, ]

        x <- floor(r$X)
        y <- floor(r$Y)

        obj_width <- ceiling(r$Width)
        obj_length <- ceiling(r$Length)

        matrixCanvas[y + seq_len(obj_width) + 1,
                     x + seq_len(obj_length) + 1] <- -r$ID
      }
    }

    # Rotate every object together, then apply masks and doors in canvas coordinates.
    if (rotation != 0) matrixCanvas <- rotate_matrix(matrixCanvas, rotation)
    matrixCanvas
  }

  base_matrices <- lapply(seq_len(nrow(rooms)), function(i) room_matrix(rooms[i, , drop = FALSE]))

  if (identical(FullRoom, "NoInterior")) {
    roomsMatrix <- lapply(seq_len(nrow(rooms)), function(i) {
      room <- rooms[i, , drop = FALSE]
      matrixCanvas <- base_matrices[[i]]
      occupied <- which(!room_interior_mask(room, rooms), arr.ind = TRUE)
      room_doors <- doors[doors$roomID == room$ID, , drop = FALSE]
      if (nrow(room_doors)) {
        local_y <- room_doors$y - room$y + 1
        local_x <- room_doors$x - room$x + 1
        valid <- local_x >= 1 & local_x <= ncol(matrixCanvas) &
          local_y >= 1 & local_y <= nrow(matrixCanvas)
        matrixCanvas[cbind(local_y[valid], local_x[valid])] <- 2
      }
      # Apply the mask last so doors of nested rooms cannot reopen obstacles.
      if (nrow(occupied)) matrixCanvas[occupied + 1] <- 0
      matrixCanvas
    })
  } else {
    # Paint large rooms first. Smaller rooms (and the higher ID for equal areas)
    # own overlapping cells and therefore retain their IDs and objects.
    canvas_height <- canvasObjects$canvasDimension$canvasHeight / 10
    canvas_width <- canvasObjects$canvasDimension$canvasWidth / 10
    composite <- matrix(NA_real_, nrow = canvas_height, ncol = canvas_width)
    area <- ceiling(rooms$l) * ceiling(rooms$w)
    drawing_order <- order(-area, rooms$ID)
    for (i in drawing_order) {
      room <- rooms[i, , drop = FALSE]
      values <- base_matrices[[i]]
      #values[values == 1] <- room$ID
      rows <- room$y + 0:(nrow(values) - 1)
      cols <- room$x + 0:(ncol(values) - 1)
      composite[rows, cols] <- values
    }
    valid_doors <- doors$x >= 1 & doors$x <= ncol(composite) &
      doors$y >= 1 & doors$y <= nrow(composite)
    if (any(valid_doors)) {
      composite[cbind(doors$y[valid_doors], doors$x[valid_doors])] <- 2
    }

    roomsMatrix <- lapply(seq_len(nrow(rooms)), function(i) {
      room <- rooms[i, , drop = FALSE]
      rows <- room$y + 0:(ceiling(room$w) + 1)
      cols <- room$x + 0:(ceiling(room$l) + 1)
      matrixCanvas <- composite[rows, cols, drop = FALSE]
      # The represented room remains traversable; only nested rooms use IDs.
      matrixCanvas[matrixCanvas == room$ID] <- 1
      matrixCanvas
    })
  }

  names(roomsMatrix) <- paste0(rooms$Name, "_", rooms$ID)
  roomsMatrix
}

# WithoutMask embeds nested room IDs and objects in containing room matrices;
# WithMask represents the same footprints as obstacles.
CanvasMatrices <- function(canvasObjects) {
  matrices <- list(WithoutMask = list(), WithMask = list())
  for (canvas in unique(canvasObjects$roomsINcanvas$CanvasID)) {
    matrices$WithoutMask[[canvas]] <- list(
      floor = CanvasToMatrix(canvasObjects, FullRoom = FALSE, canvas = canvas),
      rooms = CanvasRoomToMatrix(canvasObjects, FullRoom = FALSE, canvas = canvas))
    matrices$WithMask[[canvas]] <- list(
      floor = CanvasToMatrix(canvasObjects, FullRoom = "NoInterior", canvas = canvas),
      rooms = CanvasRoomToMatrix(canvasObjects, FullRoom = "NoInterior", canvas = canvas))
  }
  matrices
}


command_addRoomObject = function(newroom, doors = empty_canvas_doors()){
  doors <- room_doors_for_canvas(doors, newroom$ID)
  # Encode strings as JSON: room names and CSS colours are data, not JavaScript.
  args <- list(newroom$ID, newroom$x * 10, newroom$y * 10,
               newroom$center_x * 10, newroom$center_y * 10,
               (ceiling(newroom$l) + 1) * 10, (ceiling(newroom$w) + 1) * 10, newroom$h,
               newroom$colorFill, newroom$colorBorder, newroom$Name,
               doors, newroom$type, if (is.null(newroom$z_index)) 0 else newroom$z_index)
  encoded <- vapply(args, function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE, dataframe = "rows")), character(1))
  paste0("FloorArray[", jsonlite::toJSON(newroom$CanvasID, auto_unbox = TRUE),
         "].arrayObject.push(new Room(", paste(encoded, collapse = ","), "));" )
}

UpdatingData = function(input,output,canvasObjects, mess,areasColor, session){
  mess <- normalize_room_doors(mess)
  messNames = names(mess)
  for(i in messNames)
    canvasObjects[[i]] = mess[[i]]

  ### UPDATING THE CANVAS ####
  # deleting everything from canvas
  runjs("shinyjs.clearCanvas()")
  # update the canvas dimension
  runjs(paste0("shinyjs.canvasDimension({w:", canvasObjects$canvasDimension$canvasWidth, ", h:", canvasObjects$canvasDimension$canvasHeight, "})"))

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
    # Add colorFillBase column for backward compatibility with old saved models
    if (!"colorFillBase" %in% colnames(canvasObjects$roomsINcanvas)) {
      # Create colorFillBase from colorFill, ensuring alpha=1
      canvasObjects$roomsINcanvas$colorFillBase <- sapply(canvasObjects$roomsINcanvas$colorFill, function(color) {
        rgb_match <- regmatches(color, regexec("rgba?\\(([0-9]+),\\s*([0-9]+),\\s*([0-9]+)", color))
        if (length(rgb_match[[1]]) >= 4) {
          paste0("rgba(", rgb_match[[1]][2], ", ", rgb_match[[1]][3], ", ", rgb_match[[1]][4], ", 1)")
        } else {
          color
        }
      })
    }
    for(r_id in canvasObjects$roomsINcanvas$ID){
      newroom = canvasObjects$roomsINcanvas %>% filter(ID == r_id)
      runjs( command_addRoomObject( newroom, canvasObjects$doorsINcanvas) )
    }

    # update types
    updateSelectizeInput(inputId = "select_type",
                         selected = "",
                         choices = c("", unique(canvasObjects$types$Name)))
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
                const newPoint = new Circle(", newpoint$ID,",", newpoint$x*10+5," , ", newpoint$y*10+5,", 5, rgba(0, 127, 255, 1));
                // Aggiungi il nuovo oggetto Square all'array arrayObject
                FloorArray[\"",newpoint$CanvasID,"\"].arrayObject.push(newPoint);"))
    }
  }

  ### updating the old RDs version with v.2 format ####
  for(a in names(canvasObjects$agents)){
    if(nrow(canvasObjects$agents[[a]]$RandFlow) > 0){
      if(! "TimeSlot" %in% colnames(canvasObjects$agents[[a]]$RandFlow))
        canvasObjects$agents[[a]]$RandFlow = data.frame(canvasObjects$agents[[a]]$RandFlow,TimeSlot = "00:00 - 23:59")

      if(! "AgentLinked" %in% colnames(canvasObjects$agents[[a]]$RandFlow))
        canvasObjects$agents[[a]]$RandFlow = data.frame(canvasObjects$agents[[a]]$RandFlow, AgentLinked = "None")

      if(! "AgentLinkedType" %in% colnames(canvasObjects$agents[[a]]$RandFlow))
        canvasObjects$agents[[a]]$RandFlow = data.frame(canvasObjects$agents[[a]]$RandFlow, AgentLinkedType = "None")

      if(! "AgentLinkedTimeout" %in% colnames(canvasObjects$agents[[a]]$RandFlow))
        canvasObjects$agents[[a]]$RandFlow = data.frame(canvasObjects$agents[[a]]$RandFlow, AgentLinkedTimeout = "None")

      if(! "AgentLinkedTimeoutBehave" %in% colnames(canvasObjects$agents[[a]]$RandFlow))
        canvasObjects$agents[[a]]$RandFlow = data.frame(canvasObjects$agents[[a]]$RandFlow, AgentLinkedTimeoutBehave = "None")
    }

    colnames(canvasObjects$agents[[a]]$RandFlow)[colnames(canvasObjects$agents[[a]]$RandFlow) == 'Weight'] <- 'Times'

    # Get the Weight vector
    times <- canvasObjects$agents[[a]]$RandFlow$Times

    # Find rows where NONE of the time_units appear in the weight string
    # Use grepl to check if any time unit exists in each weight entry
    has_time_unit <- sapply(times, function(w) {
      grepl("minute", w, fixed = TRUE) || grepl("hour", w, fixed = TRUE) || grepl("day", w, fixed = TRUE) || grepl("week", w, fixed = TRUE)
    })

    # Rows without any time unit
    rows_to_add <- which(!has_time_unit)

    # Add " (minute)" to those rows
    canvasObjects$agents[[a]]$RandFlow$Times[rows_to_add] <-
      paste0(canvasObjects$agents[[a]]$RandFlow$Times[rows_to_add], " (minute)")

    if(nrow(canvasObjects$agents[[a]]$DeterFlow) > 0){
      if(! "AgentLinked" %in% colnames(canvasObjects$agents[[a]]$DeterFlow)){
        canvasObjects$agents[[a]]$DeterFlow = data.frame(canvasObjects$agents[[a]]$DeterFlow, AgentLinked = "None")
        canvasObjects$agents[[a]]$DeterFlow$Label = paste0(canvasObjects$agents[[a]]$DeterFlow$Label, " - None" )
      }

      if(! "AgentLinkedType" %in% colnames(canvasObjects$agents[[a]]$DeterFlow))
        canvasObjects$agents[[a]]$DeterFlow = data.frame(canvasObjects$agents[[a]]$DeterFlow,AgentLinkedType = "None")

      if(! "AgentLinkedTimeout" %in% colnames(canvasObjects$agents[[a]]$DeterFlow))
        canvasObjects$agents[[a]]$DeterFlow = data.frame(canvasObjects$agents[[a]]$DeterFlow, AgentLinkedTimeout = "None")

      if(! "AgentLinkedTimeoutBehave" %in% colnames(canvasObjects$agents[[a]]$DeterFlow))
        canvasObjects$agents[[a]]$DeterFlow = data.frame(canvasObjects$agents[[a]]$DeterFlow, AgentLinkedTimeoutBehave = "None")

      for(i in 1:nrow(canvasObjects$agents[[a]]$DeterFlow)){
        activityLabel <- switch(paste(canvasObjects$agents[[a]]$DeterFlow$Activity[i]),
                                "1" = "Very Light",
                                "1.7777" = "Light",
                                "2.5556" ="Quite Hard",
                                "6.1111" = "Hard"
        )

        label <- paste0(canvasObjects$agents[[a]]$DeterFlow$Room[i], " - ", canvasObjects$agents[[a]]$DeterFlow$Dist[i], " ", canvasObjects$agents[[a]]$DeterFlow$Time[i], " min - ", activityLabel, " - ", canvasObjects$agents[[a]]$DeterFlow$AgentLinked[i], " (", canvasObjects$agents[[a]]$DeterFlow$AgentLinkedType[i], ", ", canvasObjects$agents[[a]]$DeterFlow$AgentLinkedTimeout[i], ", ", canvasObjects$agents[[a]]$DeterFlow$AgentLinkedTimeoutBehave[i], ")")
        if(canvasObjects$agents[[a]]$DeterFlow$AgentLinked[i] == "None")
          label <- paste0(canvasObjects$agents[[a]]$DeterFlow$Room[i], " - ", canvasObjects$agents[[a]]$DeterFlow$Dist[i], " ", canvasObjects$agents[[a]]$DeterFlow$Time[i], " min - ", activityLabel)

        canvasObjects$agents[[a]]$DeterFlow$Label[i] <- label
      }
    }

    if(! "Shift" %in% colnames(canvasObjects$agents[[a]]$EntryExitTime)){
      canvasObjects$agents[[a]]$EntryExitTime$Shift <- "1 shift"
    }

    if(! "NumAgent" %in% colnames(canvasObjects$agents[[a]]$EntryExitTime)){
      if("NumAgent" %in% names(canvasObjects$agents[[a]]))
        canvasObjects$agents[[a]]$EntryExitTime$NumAgent <- canvasObjects$agents[[a]]$NumAgent
      else
        canvasObjects$agents[[a]]$EntryExitTime$NumAgent <- "0"
    }




    canvasObjects$agents[[a]]$RandFlow <- canvasObjects$agents[[a]]$RandFlow %>% filter(Room != "Do nothing")

    # Healing: Reset AgentLinked if the linked agent no longer exists
    all_agents_names <- names(canvasObjects$agents)
    if (!is.null(canvasObjects$agents[[a]]$DeterFlow)) {
      mask <- !(canvasObjects$agents[[a]]$DeterFlow$AgentLinked %in% c("None", all_agents_names))
      if (any(mask)) {
        canvasObjects$agents[[a]]$DeterFlow$AgentLinked[mask] <- "None"
        canvasObjects$agents[[a]]$DeterFlow$AgentLinkedType[mask] <- "None"
        # Update labels to remove the invalid agent name
        canvasObjects$agents[[a]]$DeterFlow$Label[mask] <- sapply(
          canvasObjects$agents[[a]]$DeterFlow$Label[mask],
          function(lbl) {
            parts <- strsplit(lbl, " - ")[[1]]
            if (length(parts) >= 4) {
              parts[length(parts)] <- "None"
              return(paste(parts, collapse = " - "))
            }
            return(lbl)
          }
        )
      }
    }
    if (!is.null(canvasObjects$agents[[a]]$RandFlow)) {
      mask <- !(canvasObjects$agents[[a]]$RandFlow$AgentLinked %in% c("None", all_agents_names))
      if (any(mask)) {
        canvasObjects$agents[[a]]$RandFlow$AgentLinked[mask] <- "None"
        canvasObjects$agents[[a]]$RandFlow$AgentLinkedType[mask] <- "None"
      }
    }
  }

  ####

  updateSelectizeInput(session = session, inputId = "id_new_agent", choices = if(!is.null(canvasObjects$agents)) unique(names(canvasObjects$agents)) else "", selected = "")
  updateSelectizeInput(session = session, inputId = "id_agents_to_copy", choices = if(!is.null(canvasObjects$agents)) unique(names(canvasObjects$agents)) else "", selected = "")
  updateSelectizeInput(session = session, inputId ="agentLink_rand_flow", choices = c("", unique(names(canvasObjects$agents))), selected = "" )
  updateSelectizeInput(session = session, inputId ="agentLink_det_flow", choices = c("", unique(names(canvasObjects$agents))), selected = "" )

  selected = "SIR"
  if(!is.null(canvasObjects$disease)){
    if(length(canvasObjects$disease) > 1){
      updateCheckboxInput(session, "enable_risk_classes", value = TRUE)
      updateSelectizeInput(session, "disease_model", selected = canvasObjects$disease[[1]]$disease_model_name)
      updateNumericInput(session, "num_risk_classes", value = length(canvasObjects$disease))

      # Load data for each risk class
      print("TO DO")
    }
    else{
      disease_risk_class <- canvasObjects$disease[[1]]
      selected = disease_risk_class$disease_model_name

      updateTextInput(session, inputId = "beta_aerosol", value=disease_risk_class$beta_aerosol)
      updateTextInput(session, inputId = "beta_contact", value=disease_risk_class$beta_contact)

      params <- parse_distribution(disease_risk_class$gamma_time, disease_risk_class$gamma_dist)
      gamma_dist <- disease_risk_class$gamma_dist
      gamma_a <- params[[1]]
      gamma_b <- params[[2]]
      tab <- if(gamma_dist == "Deterministic") "DetTime_tab" else "StocTime_tab"

      update_distribution("gamma", gamma_dist, gamma_a, gamma_b, tab)


      if(grepl("E", selected)){
        params <- parse_distribution(disease_risk_class$alpha_time, disease_risk_class$alpha_dist)
        alpha_dist <- disease_risk_class$alpha_dist
        alpha_a <- params[[1]]
        alpha_b <- params[[2]]
        tab <- if(alpha_dist == "Deterministic") "DetTime_tab" else "StocTime_tab"


        update_distribution("alpha", alpha_dist, alpha_a, alpha_b, tab)
      }


      if(grepl("D", selected)){
        params <- parse_distribution(disease_risk_class$lambda_time, disease_risk_class$lambda_dist)
        lambda_dist <- disease_risk_class$lambda_dist
        lambda_a <- params[[1]]
        lambda_b <- params[[2]]
        tab <- if(lambda_dist == "Deterministic") "DetTime_tab" else "StocTime_tab"

        update_distribution("lambda", lambda_dist, lambda_a, lambda_b, tab)
      }


      if(grepl("^([^S]*S[^S]*S[^S]*)$", selected[length(selected)])){
        params <- parse_distribution(disease_risk_class$nu_time, disease_risk_class$nu_dist)
        nu_dist <- disease_risk_class$nu_dist
        nu_a <- params[[1]]
        nu_b <- params[[2]]
        tab <- if(nu_dist == "Deterministic") "DetTime_tab" else "StocTime_tab"

        update_distribution("nu", nu_dist, nu_a, nu_b, tab)
      }
    }
  }

  updateSelectizeInput(inputId = "disease_model",
                       selected = selected)

  updateNumericInput(session, "radius", value = canvasObjects$virus_parameters$radius)
  updateNumericInput(session, "ngen_base", value = canvasObjects$virus_parameters$ngen_base)
  updateNumericInput(session, "vl", value = canvasObjects$virus_parameters$vl)
  updateNumericInput(session, "decay_rate", value = canvasObjects$virus_parameters$decay_rate)
  updateNumericInput(session, "gravitational_settling_rate", value = canvasObjects$virus_parameters$gravitational_settling_rate)
  updateNumericInput(session, "inhalation_rate_pure", value = canvasObjects$virus_parameters$inhalation_rate_pure)

  updateTextInput(session, inputId = "seed", value = canvasObjects$starting$seed)
  updateRadioButtons(session, inputId = "initial_day", selected = canvasObjects$starting$day)
  updateTextInput(session, inputId = "nrun", value = canvasObjects$starting$nrun)
  updateTextInput(session, inputId = "prun", value = canvasObjects$starting$prun)
  updateTextInput(session, inputId = "initial_time", value = canvasObjects$starting$time)
  updateTextInput(session, inputId = "simulation_days", value = canvasObjects$starting$simulation_days)
  updateSelectizeInput(session, inputId = "step", choices = c(1, 2, 3, 4, 5, 6, 10, 12, 15, 20, 30, 60), selected = as.numeric(canvasObjects$starting$step))


  rooms = canvasObjects$roomsINcanvas %>% filter(type != "Fillingroom", type != "Stair")
  roomsAvailable = c("", unique(paste0( rooms$type,"-", rooms$area) ) )
  updateSelectizeInput(session = session, "room_ventilation",
                       choices = roomsAvailable, selected = "")

  updateNumericInput(session, inputId = "virus_severity", value = canvasObjects$virus_severity)

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

  # Healing: Remove orphaned agents and invalid keys from resources (Relocated and Hardened)
  if (!is.null(canvasObjects$resources)) {
    all_agents_list <- names(canvasObjects$agents)

    # Keep only resource entries whose keys are valid (Type-Area currently in canvas)
    if (!is.null(canvasObjects$roomsINcanvas)) {
      valid_keys <- unique(paste0(canvasObjects$roomsINcanvas$type, "-", canvasObjects$roomsINcanvas$area))
      current_res_names <- names(canvasObjects$resources)
      if (!is.null(current_res_names)) {
        canvasObjects$resources <- canvasObjects$resources[current_res_names %in% valid_keys]
      }
    }

    for (i in seq_along(canvasObjects$resources)) {
      # Clean roomResource columns
      if (!is.null(canvasObjects$resources[[i]]$roomResource)) {
        # Force conversion to data frame in case it was loaded as a list
        df_res <- as.data.frame(canvasObjects$resources[[i]]$roomResource, stringsAsFactors = FALSE)
        current_cols <- names(df_res)
        # Keep only 'room', 'MAX', and agents that actually exist
        cols_to_keep <- current_cols[current_cols %in% c("room", "MAX", all_agents_list)]
        canvasObjects$resources[[i]]$roomResource <- df_res[, cols_to_keep, drop = FALSE]
      }
      # Clean waiting rooms
      if (!is.null(canvasObjects$resources[[i]]$waitingRoomsRand)) {
        df_rand <- as.data.frame(canvasObjects$resources[[i]]$waitingRoomsRand, stringsAsFactors = FALSE)
        canvasObjects$resources[[i]]$waitingRoomsRand <- df_rand[df_rand$Agent %in% all_agents_list, ]
      }
      if (!is.null(canvasObjects$resources[[i]]$waitingRoomsDeter)) {
        df_deter <- as.data.frame(canvasObjects$resources[[i]]$waitingRoomsDeter, stringsAsFactors = FALSE)
        canvasObjects$resources[[i]]$waitingRoomsDeter <- df_deter[df_deter$Agent %in% all_agents_list, ]
      }
    }
  }

  "The file has been uploaded with success!"
}

UpdatingTimeSlots_tabs = function(input,output,canvasObjects, InfoApp, session, ckbox_entranceFlow){
  Agent = input$id_new_agent
  EntryExitTime= canvasObjects$agents[[Agent]]$EntryExitTime
  FlowID = canvasObjects$agents[[Agent]]$DeterFlow$FlowID
  entry_type = canvasObjects$agents[[Agent]]$entry_type

  NumShifts = InfoApp$NumTabsTimeShift
  NumTabs = InfoApp$NumTabsTimeSlot
  #if i change type from one agent to another I have to remove all tabs type
  if(length(NumShifts) > 0){
    #if it's the first agent ever we click on we remove the default void slot
    if(InfoApp$oldAgentType == ""){
      removeTab(inputId = "Rate_tabs", target = "slot_1", session = session)
      removeTab(inputId = "Shift_tabs", target = "shift_1", session = session)
    }
    #if(InfoApp$oldAgentType == "Time window"){
      for(i in names(NumShifts)) {
        removeTab(inputId = "Shift_tabs", target = i, session = session)
        removeUI(selector = paste0("Time_tabs_", i))
      }
    #}
    #else if(InfoApp$oldAgentType == "Daily Rate"){
      for(j in NumTabs) {
        removeTab(inputId = "Rate_tabs", target = paste0("slot_", j), session = session)
      }
    #}
  }

  if((is.null(EntryExitTime) || nrow(EntryExitTime) == 0) && ckbox_entranceFlow == "Daily Rate"){
    appendTab(inputId = "Rate_tabs",
              tabPanel("1 slot",
                       value = "slot_1",
                       tags$b("Entrance rate:"),
                       get_distribution_panel(paste0("daily_rate_1")),
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
    showTab(inputId = "Rate_tabs", target = "slot_1", select = T)
  }else if((is.null(EntryExitTime) || nrow(EntryExitTime) == 0) && ckbox_entranceFlow == "Time window"){
    appendTab(inputId = "Shift_tabs",
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

    InfoApp$NumTabsTimeShift = list("shift_1"=1)
    showTab(inputId = "Shift_tabs", target = "shift_1", select = T)
    showTab(inputId = "Time_tabs", target = "slot_1_1", select = T)
  }else if((!is.null(EntryExitTime) || nrow(EntryExitTime) > 0) && ckbox_entranceFlow == "Time window"){
    # updateRadioButtons(session, "ckbox_entranceFlow", selected = "Time window")
    shifts = sort(unique(gsub(pattern = " shift", replacement = "", x = EntryExitTime$Shift)))
    InfoApp$NumTabsTimeShift <- list()
    for(j in shifts){
      EntryExitTimeShiftJ <- EntryExitTime %>%
        filter(Shift == paste0(j, " shift"))

      slots = sort(unique(gsub(pattern = " slot", replacement = "", x = EntryExitTimeShiftJ$Name)))

      appendTab(inputId = "Shift_tabs",
                tabPanel(paste0(j, " shift"),
                         value = paste0("shift_", j),
                         fluidRow(
                           column(4,offset=1,
                                  textInput(inputId = paste0("num_agent_", j), label = "Number of agents:",
                                            placeholder = "The number must be a positive integer", value = unique((EntryExitTimeShiftJ %>% filter(Shift == paste0(j, " shift")))$NumAgent))
                           )
                         ),
                         fluidRow(
                           column(11,offset=1,
                                  sortableTabsetPanel(id = paste0("Time_tabs_", j),
                                              !!!lapply(slots, function(i) {
                                                tabPanel(paste0(i, " slot"),
                                                         value = paste0("slot_", j, "_", i),
                                                         column(7,
                                                                textInput(inputId = paste0("EntryTime_", j, "_", i), value = unique((EntryExitTimeShiftJ %>% filter(Name == paste0(i, " slot")))$EntryTime), label = "Entry time:", placeholder = "hh:mm"),
                                                                selectInput(inputId = paste0("Select_TimeDetFlow_", j, "_", i),
                                                                            label = "Associate with a determined flow:",
                                                                            choices = sort(unique(FlowID)),
                                                                            selected = unique((EntryExitTimeShiftJ %>% filter(Name == paste0(i, " slot")))$FlowID))
                                                         ),
                                                         column(5,
                                                                checkboxGroupInput(inputId = paste0("selectedDays_", j, "_", i),
                                                                                   label = "Select Days of the Week",
                                                                                   choices = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                                                                                   selected = (EntryExitTimeShiftJ %>% filter(Name == paste0(i, " slot")))$Days
                                                                )
                                                         )
                                                )
                                              })
                                  )
                           )
                         )
                )
      )

      InfoApp$NumTabsTimeShift[paste0("shift_", j)] = slots
    }

    showTab(inputId = "Shift_tabs", target = paste0("shift_", shifts[1]), select = T)
    showTab(inputId = "Time_tabs", target = paste0("slot_", slots[1]), select = T)
  } else if((!is.null(EntryExitTime) || nrow(EntryExitTime) > 0) && ckbox_entranceFlow == "Daily Rate"){
    # updateRadioButtons(session, "ckbox_entranceFlow", selected = "Daily Rate")
    slots = sort(unique(gsub(pattern = " slot", replacement = "", x = EntryExitTime$Name)))
    tab <- "DetTime_tab"
    InfoApp$NumTabsTimeSlot <- c()
    for(i in slots){
      InfoApp$NumTabsTimeSlot = c(InfoApp$NumTabsTimeSlot, i)
      df = EntryExitTime %>% filter(Name == paste0(i, " slot"))

      params <- parse_distribution(unique(df$RateTime), unique(df$RateDist))
      rate_dist <- unique(df$RateDist)
      rate_a <- params[[1]]
      rate_b <- params[[2]]

      tab <- if(rate_dist == "Deterministic") "DetTime_tab" else "StocTime_tab"

      appendTab(inputId = "Rate_tabs",
                tabPanel(paste0(i," slot"),
                         value = paste0("slot_", i),
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
      showTab(inputId = paste0("DistTime_tabs_daily_rate_", i), target = tab, select = T)
      # update_distribution(paste0("daily_rate_", i), rate_dist, rate_a, rate_b, tab)
    }
    showTab(inputId = "Rate_tabs", target = paste0("slot_", slots[1]), select = T)
    # if(tab == "StocTime_tab")
    #   updateSelectInput(inputId = paste0("DistStoc_id_daily_rate_", slots[1]), selected = rate_dist)
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
  if(grepl("DetTime_tab",input[[paste0("DistTime_tabs_", suffix)]])) {
    if(input[[paste0("DetTime_", suffix)]] == "")
      return(list(NULL, NULL))

    if(is.na(as.numeric(gsub(",", "\\.", input[[paste0("DetTime_", suffix)]]))) || as.numeric(gsub(",", "\\.", input[[paste0("DetTime_", suffix)]])) <= 0){
      print(as.numeric(gsub(",", "\\.", input[[paste0("DetTime_", suffix)]])))
      shinyalert("Error", "You must specify a time greater than 0 (in minutes).", type = "error")
      return(list(NULL, NULL))
    }
    new_time = input[[paste0("DetTime_", suffix)]]
    new_dist = "Deterministic"
  }else if(grepl("StocTime_tab", input[[paste0("DistTime_tabs_", suffix)]])){
    new_dist = input[[paste0("DistStoc_id_", suffix)]]

    if(input[[paste0("DistStoc_id_", suffix)]] == 'Exponential'){
      if(input[[paste0("DistStoc_ExpRate_", suffix)]] == "")
        return(list(NULL, NULL))

      if(is.na(as.numeric(gsub(",", "\\.", input[[paste0("DistStoc_ExpRate_", suffix)]]))) || as.numeric(gsub(",", "\\.", input[[paste0("DistStoc_ExpRate_", suffix)]])) <= 0 ){
        shinyalert("Error", "You must specify a time greater than (in minutes).", type = "error")
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
        shinyalert("Error", "You must specify a and b as numeric (in minutes and both greater than), with a < b.", type = "error")
        return(list(NULL, NULL))
      }
      new_time = paste0("a = ",input[[paste0("DistStoc_UnifRate_a_", suffix)]] ,"; b = ",input[[paste0("DistStoc_UnifRate_b_", suffix)]])
    }else if(input[[paste0("DistStoc_id_", suffix)]] == 'Truncated Positive Normal'){
      if(input[[paste0("DistStoc_NormRate_m_", suffix)]] == "" || input[[paste0("DistStoc_NormRate_sd_", suffix)]] == "")
        return(list(NULL, NULL))

      if( is.na(as.numeric(gsub(",", "\\.", input[[paste0("DistStoc_NormRate_m_", suffix)]]))) ||
          is.na(as.numeric(gsub(",", "\\.", input[[paste0("DistStoc_NormRate_m_", suffix)]]))) ||
          as.numeric(input[[paste0("DistStoc_NormRate_m_", suffix)]]) <= 0 || as.numeric(input[[paste0("DistStoc_NormRate_sd_", suffix)]]) < 0){
        shinyalert("Error", "You must specify the mean and standard deviation as numeric (in minutes, with mean > 0 and std >= 0).", type = "error")
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

  MeasuresFromTo <- NULL
  ## From_to matrix generation rooms
  if(!is.null(WHOLEmodel$roomsINcanvas)){
    rooms = WHOLEmodel$roomsINcanvas %>% mutate(Type= paste0(type,"-",area)) %>% select(Type) %>% distinct() %>% pull()
    rooms_fromto= matrix(0,ncol = maxN, nrow = length(rooms), dimnames = list(rooms = rooms, days= 1:maxN))

    room_default <- data.frame(
      Measure = c("Ventilation"),
      Type = "Global",
      Parameters = c( "Ventilation: 0; Sterilisation: 0; Air: 100"),
      From = 1,
      To = maxN,
      stringsAsFactors = FALSE
    )

    WHOLEmodel$rooms_whatif = rbind(room_default,WHOLEmodel$rooms_whatif)
    MeasuresFromTo = lapply( unique(WHOLEmodel$rooms_whatif$Measure),function(m,fromto){
      rooms_whatif = WHOLEmodel$rooms_whatif %>% filter(Measure == m) %>% rename(Name = Type)

      params = str_split(rooms_whatif[,"Parameters"],pattern = "; ")%>%
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

      rooms_whatif = cbind(rooms_whatif %>% select(-Parameters), params)

      fromto = lapply(names(params),function(i, fromto_p){
        r_specific = rooms_whatif[,c("Name","From","To",i) ]

        global = r_specific %>% filter(Name == "Global")
        if(dim(global)[1] >0){
          for(ii in seq_along(global[,1])){
            glob_specific = global[ii,]
            fromto_p[,glob_specific$From:glob_specific$To] = glob_specific[,i]
          }
        }

        room_specific = r_specific %>% filter(Name != "Global")
        if(dim(room_specific)[1] >0){
          for(ii in seq_along(room_specific[,1])){
            room_specific = room_specific[ii,]
            fromto_p[room_specific$Name,room_specific$From:room_specific$To] = room_specific[,i]
          }
        }
        fromto_p = cbind(rooms,fromto_p) # put rooms name as first column
        return(fromto_p)
      },fromto_p = fromto)
      names(fromto)= names(params)

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
                      paste0("Dist.Days: No quarantine, 0, 0; Q.Room: ", (WHOLEmodel$roomsINcanvas %>% filter(grepl("^Spawnroom", type)))$type[1], "-", (WHOLEmodel$roomsINcanvas %>% filter(grepl("^Spawnroom", type)))$area[1], "; Sensitivity: 1; Specificity: 1; Dist: No swab, 0, 0 "),
                      "First: 0; Second: 0" ),
      From = 1,
      To = maxN,
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

F4FgetVolumes=function(exclude, from="~", custom_name="Home"){
  library(xfun)
  library(fs)

  osSystem <- Sys.info()["sysname"]
  userHome <- path_expand(from)  # Get the user's home directory

  if (osSystem == "Darwin") {
    #volumes <- fs::dir_ls(userHome)
    #names(volumes) <- basename(volumes)
    volumes <- userHome
    names(volumes) <- basename(volumes)
  }
  else if (osSystem == "Linux") {
    volumes <- c(setNames(userHome, custom_name))
    media_path <- file.path(userHome, "media")
    if (isTRUE(dir_exists(media_path))) {
      media <- dir_ls(media_path)
      names(media) <- basename(media)
      volumes <- c(volumes, media)
    }
  }
  else if (osSystem == "Windows") {
    userHome <- gsub("\\\\", "/", userHome)  # Convert Windows path format
    volumes <- c(setNames(userHome, custom_name))

    # Check for mounted drives inside user home (e.g., OneDrive, Network Drives)
    possible_drives <- fs::dir_ls(userHome, type = "directory")
    names(possible_drives) <- basename(possible_drives)
    volumes <- c(volumes, possible_drives)
  }
  else {
    stop("unsupported OS")
  }

  if (!is.null(exclude)) {
    volumes <- volumes[!names(volumes) %in% exclude]
  }

  return(volumes)
}

canvas_graph_nodes <- function(canvasObjects, canvas) {
  doors <- sync_room_doors(canvasObjects$doorsINcanvas, canvasObjects$roomsINcanvas)
  doors <- doors[doors$CanvasID == canvas, , drop = FALSE]
  horizontal <- doors$side %in% c("top", "bottom")
  nodes <- data.frame(ID = seq_len(nrow(doors)), x = doors$x, y = doors$y,
                      CanvasID = doors$CanvasID, door = doors$side, doorID = doors$ID,
                      roomID = doors$roomID,
                      offset_x = doors$x - doors$wall_x,
                      offset_y = doors$y - doors$wall_y)
  points <- canvasObjects$nodesINcanvas
  if (!is.null(points)) {
    points <- points[points$CanvasID == canvas, , drop = FALSE]
    rooms <- canvasObjects$roomsINcanvas
    if (!is.null(rooms)) rooms <- rooms[rooms$CanvasID == canvas, , drop = FALSE]
    points <- points[canvas_points_outside_rooms(points$x + 1, points$y + 1, rooms), , drop = FALSE]
    if (nrow(points)) {
      nodes <- rbind(nodes, data.frame(ID = nrow(nodes) + seq_len(nrow(points)),
                     x = points$x + 1, y = points$y + 1, CanvasID = points$CanvasID,
                     door = "none", doorID = NA_integer_, roomID = NA_integer_,
                     offset_x = 0.5, offset_y = 0.5))
    }
  }
  nodes
}

is_room_connected <- function(matrix, room, roomsINcanvas, nodesINcanvas, doorsINcanvas) {
  doors <- sync_room_doors(doorsINcanvas, roomsINcanvas)
  doors <- doors[doors$CanvasID == room$CanvasID, , drop = FALSE]
  own <- doors[doors$roomID == room$ID, , drop = FALSE]
  others <- doors[doors$roomID != room$ID, c("x", "y"), drop = FALSE]
  if (!is.null(nodesINcanvas)) {
    points <- nodesINcanvas[nodesINcanvas$CanvasID == room$CanvasID, c("x", "y"), drop = FALSE]
    floor_rooms <- roomsINcanvas[roomsINcanvas$CanvasID == room$CanvasID, , drop = FALSE]
    points <- points[canvas_points_outside_rooms(points$x + 1, points$y + 1, floor_rooms), , drop = FALSE]
    others <- rbind(others, points + 1)
  }
  for (i in seq_len(nrow(own))) {
    for (j in seq_len(nrow(others))) {
      path <- bresenham(c(own$x[i], others$x[j]), c(own$y[i], others$y[j]))
      if (all(matrix[cbind(path$y, path$x)] %in% c(0, 2, 3))) return(TRUE)
    }
  }
  FALSE
}


# Helper function to check if an object overlaps with the door area
check_door_collision <- function(canvasObjects, room_name, objects_list) {
  # Get room information
  browser()
  room_info <- canvasObjects$rooms %>%
    filter(Name == room_name) %>%
    mutate(door_x = floor(w/ 2)+1, door_y = l) %>%
    select(door_x, door_y, w, l) %>%
    distinct()

  if (nrow(room_info) == 0) {
    return(list(collision = FALSE, message = ""))
  }

  door_x <- room_info$door_x[1]
  door_y <- room_info$door_y[1]
  room_width <- room_info$w[1]
  room_length <- room_info$l[1]

  # Define door area based on door position (door area is 1 meter wide)
  door_area <- list(x_min = door_x - 0.5, x_max = door_x + 0.5, y_min = door_y - 0.3, y_max = door_y + 0.3)

  # Check if any object overlaps with door area
  for (obj in objects_list) {
    obj_x_min <- obj$x
    obj_x_max <- obj$x + obj$width
    obj_y_min <- obj$y
    obj_y_max <- obj$y + obj$length

    # Check for overlap (AABB - Axis-Aligned Bounding Box collision)
    has_overlap <- !(
      obj_x_max <= door_area$x_min ||  # Object is completely to the left
        obj_x_min >= door_area$x_max ||  # Object is completely to the right
        obj_y_max <= door_area$y_min ||  # Object is completely above
        obj_y_min >= door_area$y_max     # Object is completely below
    )

    if (has_overlap) {
      return(list(
        collision = TRUE,
        message = paste0("Object '", obj$name, "' cannot be placed in front of the door. Please reposition it.")
      ))
    }
  }

  return(list(collision = FALSE, message = ""))
}

check <- function(canvasObjects, input, output, InfoApp){
  show_modal_spinner()


  if(is.null(canvasObjects$agents) || length(canvasObjects$agents) == 0){
    shinyalert("Error", "No agent is defined (Agents page).", type = "error")
    remove_modal_spinner()
    return(NULL)
  }

  if(is.null(canvasObjects$rooms) || length(canvasObjects$rooms) == 0){
    shinyalert("Error", "No room is defined (Rooms page).", type = "error")
    remove_modal_spinner()
    return(NULL)
  }

  if(!is.null(canvasObjects$roomObjects) && length(canvasObjects$roomObjects) > 0){
    for(i in 1:length(canvasObjects$roomObjects)){
      browser()
      collision_check <- check_door_collision(
        canvasObjects,
        names(canvasObjects$roomObjects)[i],
        canvasObjects$roomObjects[[i]]
      )
      if(collision_check$collision){
        shinyalert("Error", paste0("The door of the room ", names(canvasObjects$roomObjects)[i], " is colliding with an object (Canvas page)."), type = "error")
        remove_modal_spinner()
        return(NULL)
      }
    }
  }

  if(is.null(canvasObjects$roomsINcanvas) || length(canvasObjects$roomsINcanvas) == 0){
    shinyalert("Error", "No room is drew in the canvas (Canvas page).", type = "error")
    remove_modal_spinner()
    return(NULL)
  }

  if(is.null(canvasObjects$resources)){
    shinyalert("Error", "No resources are setted.", type = "error")
    remove_modal_spinner()
    return(NULL)
  }

  spawnroom <- canvasObjects$roomsINcanvas %>%
    filter(type == "Spawnroom")

  if(nrow(spawnroom) == 0){
    shinyalert("Error", "There must be at least one Spawnroom in the canvas (Canvas page).", type = "error")
    remove_modal_spinner()
    return(NULL)
  }

  rooms <- canvasObjects$roomsINcanvas %>%
    filter(!type %in% c("Spawnroom", "Fillingroom", "Stair", "Waitingroom"))

  if(nrow(rooms) < 1){
    shinyalert("Error", "There must be at least one room in the canvas with a type different from Spawnroom, Fillingroom, Stair, and Waitingroom (Canvas page).", type = "error")
    remove_modal_spinner()
    return(NULL)
  }

  for(agent in 1:length(canvasObjects$agents)){
    if(is.null(canvasObjects$agents[[agent]]$DeterFlow) || nrow(canvasObjects$agents[[agent]]$DeterFlow) == 0){
      shinyalert("Error", paste0("No determined flow is defined for the agent ", names(canvasObjects$agents)[[agent]], " (Agents page)."), type = "error")
      remove_modal_spinner()
      return(NULL)
    }

    for(df in unique(canvasObjects$agents[[agent]]$DeterFlow$FlowID)){
      df_local <- canvasObjects$agents[[agent]]$DeterFlow %>%
        filter(FlowID == df)

      rooms_type <- unique(df_local$Room)

      if(length(rooms_type) <= 1){
        shinyalert(
          title = "Error",
          text = sprintf(
            "The flow %s of agent %s has less than two rooms' types. The first and last rooms of a determined flow must be the Spawnroom, with at least another type of room in the middle (Agents page).",
            df, names(canvasObjects$agents)[[agent]]
          ),
          type = "error"
        )
        remove_modal_spinner()
        return(NULL)
      }

      if(!("Spawnroom" == strsplit(df_local$Room[1], "-")[[1]][1]) || !("Spawnroom" == strsplit(df_local$Room[nrow(df_local)], "-")[[1]][1])){
        shinyalert("Error", paste0("The first and/or the last room of agent ", names(canvasObjects$agents)[[agent]], ", ", df, " is not a Spawnroom (Agents page)."), type = "error")
        remove_modal_spinner()
        return(NULL)
      }

      df_local$Time[nrow(df_local)] <- 0
      label <- strsplit(df_local$Label[nrow(df_local)], "-")[[1]]
      df_local$Label[nrow(df_local)] <- paste0(label[1], " - ", label[2], " - 0 min - ", label[4])
    }

    if(is.null(canvasObjects$agents[[agent]]$EntryExitTime) || nrow(canvasObjects$agents[[agent]]$EntryExitTime) == 0){
      shinyalert("Error", paste0("No entry flow is defined for the agent ", names(canvasObjects$agents)[[agent]], " (Agents page)."), type = "error")
      remove_modal_spinner()
      return(NULL)
    }

    if(canvasObjects$agents[[agent]]$entry_type != "Daily Rate"){
      for(shift in unique(canvasObjects$agents[[agent]]$EntryExitTime$Shift)){
        EntryExitTimeShift <- canvasObjects$agents[[agent]]$EntryExitTime %>% filter(Shift == shift)

        for(df in unique(EntryExitTimeShift)){
          # Sovrapposition check
          overlaps <- check_overlaps(EntryExitTimeShift, canvasObjects$agents[[agent]]$DeterFlow)
          if(!is.null(overlaps)){
            shinyalert("Error", paste0("There is a sovrapposition in the definition of the entry flow for the agent ", names(canvasObjects$agents)[[agent]], " (Agents page)."), type = "error")
            remove_modal_spinner()
            return(NULL)
          }
        }
      }
    }
  }

  proportion <- 0
  for(i in 1:length(canvasObjects$disease)){
    disease_risk_class <- canvasObjects$disease[[i]]
    disease_model <- canvasObjects$disease[[i]]$disease_model_name

    proportion <- proportion + disease_risk_class$proportion

    if(is.null(disease_risk_class$beta_contact)){
      shinyalert("Error", "You must insert the beta contact parameter (Infection page).", type = "error")
      remove_modal_spinner()
      return(NULL)
    }

    if(is.null(disease_risk_class$beta_aerosol)){
      shinyalert("Error", "You must insert the beta aerosol parameter (Infection page).", type = "error")
      remove_modal_spinner()
      return(NULL)
    }

    if(is.null(disease_risk_class$gamma_time)){
      shinyalert("Error", "You must insert the gamma parameter (Infection page).", type = "error")
      remove_modal_spinner()
      return(NULL)
    }

    if(grepl("E", disease_model)){
      if(is.null(disease_risk_class$alpha_time)){
        shinyalert("Error", "You must insert the alpha parameter (Infection page).", type = "error")
        remove_modal_spinner()
        return(NULL)
      }
    }

    if(grepl("D", disease_model)){
      if(is.null(disease_risk_class$lambda_time)){
        shinyalert("Error", "You must insert the lambda parameter (Infection page).", type = "error")
        remove_modal_spinner()
        return(NULL)
      }
    }

    if(disease_model[length(disease_model)] == "S"){
      if(is.null(disease_risk_class$nu_time)){
        shinyalert("Error", "You must insert the nu parameter (Infection page).", type = "error")
        remove_modal_spinner()
        return(NULL)
      }
    }
  }

  if(abs(proportion - 1.0) > 0.01){
    shinyalert("Error", "The sum of the proportions in each infection risk class must be equals to 1 (Infection page).", type = "error")
    remove_modal_spinner()
    return(NULL)
  }

  if (!(grepl("^([01]?[0-9]|2[0-3]):[0-5][0-9]$", input$initial_time) || grepl("^\\d{1,2}$", input$initial_time))){
    shinyalert("Error", "The format of the initial time (Configuration page) should be: hh:mm (e.g. 06:15, or 20).", type = "error")
    remove_modal_spinner()
    return(NULL)
  }

  if(input$seed == "" || !grepl("(^[0-9]+).*", input$seed) || input$seed < 0){
    shinyalert("Error", "You must specify a number greater or equals than 0 as seed (Configuration page).", type = "error")
    remove_modal_spinner()
    return(NULL)
  }

  if(input$simulation_days == "" || !grepl("(^[0-9]+).*", input$simulation_days) || input$simulation_days <= 0){
    shinyalert("Error", "You must specify a number greater than 0 as number of days to simulate (Configuration page).", type = "error")
    remove_modal_spinner()
    return(NULL)
  }

  if(input$nrun == "" || !grepl("(^[0-9]+).*", input$nrun) || input$nrun <= 0){
    shinyalert("Error", "You must specify a number greater than 0 as number of run to execute (in the Configuration page).", type = "error")
    remove_modal_spinner()
    return(NULL)
  }

  canvasObjects$roomsINcanvas <- normalize_canvas_rooms(canvasObjects$roomsINcanvas)
  canvasObjects$doorsINcanvas <- sync_room_doors(canvasObjects$doorsINcanvas, canvasObjects$roomsINcanvas)
  covered <- vapply(seq_len(nrow(canvasObjects$roomsINcanvas)), function(i) {
    room <- canvasObjects$roomsINcanvas[i, ]
    room$type != "Fillingroom" && !any(room_interior_mask(room, canvasObjects$roomsINcanvas))
  }, logical(1))
  if (any(covered)) {
    shinyalert("Error", paste0("These rooms are entirely occupied by other rooms and have no usable interior: ",
      paste(canvasObjects$roomsINcanvas$Name[covered], collapse = ", "), ". Move or resize them before generating the model."), type = "error")
    remove_modal_spinner()
    return(NULL)
  }
  InfoApp$invalidRooms <- canvasObjects$roomsINcanvas$ID[canvasObjects$roomsINcanvas$type != "Fillingroom"]

  # Check if there are rooms not linked to any other room
  if(length(InfoApp$invalidRooms) > 0){
    for(id in InfoApp$invalidRooms){
      room <- canvasObjects$roomsINcanvas %>%
        filter(ID == id)

      matrix <- CanvasToMatrix(canvasObjects, FullRoom = TRUE, canvas = room$CanvasID)

      valid_rooms <- is_room_connected(matrix, room, canvasObjects$roomsINcanvas, canvasObjects$nodesINcanvas, canvasObjects$doorsINcanvas)

      if(valid_rooms){
        InfoApp$invalidRooms <- InfoApp$invalidRooms[InfoApp$invalidRooms != room$ID]
      }
    }

    if(length(InfoApp$invalidRooms) > 0){
      shinyalert("Error", paste0("There are rooms that are not connected to any other room or graph point on the canvas (Cansa page). Please, move it in a different position. Rooms: ", paste0(canvasObjects$roomsINcanvas$Name[which(canvasObjects$roomsINcanvas$ID %in% InfoApp$invalidRooms)], " #", InfoApp$invalidRooms, collapse = ", "), "."), type = "error")
      remove_modal_spinner()
      return(NULL)
    }
  }

  enable("rds_generation")

  remove_modal_spinner()
  return("OK")
}

first_missing_number <- function(arr) {
  arr <- as.integer(arr)
  arr <- sort(unique(arr))
  for (i in seq_along(arr)) {
    if (arr[i] != i) {
      return(i+1)
    }
  }
  return(length(arr) + 1)
}
