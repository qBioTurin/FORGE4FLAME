#' Convert coordinate metadata into an F4F floor-plan RDS
#'
#' @param json_file Path to a JSON array of rooms containing
#'   `id_stanza_univoca`, `poligono` and optionally `porte_identificate` and
#'   `centro_di_massa`. Coordinates use an image origin: x rightwards, y downwards.
#' @param units_per_metre Number of source coordinate units per metre. Required;
#'   for example, use 10 if ten pixels represent one metre.
#' @param output_file Optional destination for `saveRDS()`. With NULL, only return
#'   the model. The destination directory must already exist.
#' @param floor_name Alphanumeric floor name (underscores allowed).
#' @param room_height Room height in metres, since the JSON only describes 2D geometry.
#' @param margin Positive integer margin in metres around the imported rooms.
#' @param warn Emit a summary of approximations requiring review. The detailed
#'   report is always included in the returned model's `floorplan_import` attribute.
#'
#' @return Invisibly, an F4F model list, also saved if `output_file` is supplied.
#'   `attr(model, "floorplan_import")` contains the source data, scale, room and
#'   door mappings, overlaps and source shared doors that could not remain shared.
#' @details
#' Non-rectangular polygons become their axis-aligned bounding rectangles. A
#' common translation places the plan inside the canvas, and boundaries are
#' rounded to F4F's one-metre grid. Width and length exclude the one-cell wall
#' ring: x/y identify the top-left wall, and the opposite walls are x+l+1/y+w+1.
#' Shared-door memberships use the same global x/y cell.
#' Rooms that collapse at this scale are rejected.
#' Doors of rooms that were rectangular in the source are projected onto the
#' nearest rectangle border. For a room converted from a non-rectangular polygon,
#' each door keeps its original transformed coordinate inside the bounding
#' rectangle. All coordinates are snapped to F4F's one-metre grid. Doors
#' collapsing to the same cell are merged and reported.
#' A room with at least one door is omitted when all of its doors are internal
#' after this conversion. Its doors are omitted as well and recorded in the
#' import report.
#'
#' Identical source door coordinates in two rooms identify a candidate shared
#' door. For rectangular source rooms, its two rows share an ID only if the
#' resulting rectangles have opposing, touching walls at that door cell. A door
#' retained from a non-rectangular room can instead be linked at its unchanged
#' coordinate. Otherwise the entries remain separate and the report identifies
#' the lost connection. Bounding rectangles may overlap;
#' these overlaps are reported, not silently repaired. Review the imported plan
#' before configuring agents, a Spawnroom and the remaining simulation settings.
#' Requires the F4F version with the separate `doorsINcanvas` table.
#' @export
#' @examples
#' \dontrun{
#' model <- f4f_import_floorplan_json(
#'   "metadati_coordinate.json", units_per_metre = 10,
#'   output_file = "planimetria.RDs"
#' )
#' attr(model, "floorplan_import")$overlaps
#' }
f4f_import_floorplan_json <- function(json_file, units_per_metre,
                                      output_file = NULL, floor_name = "Floor1",
                                      room_height = 3, margin = 2, warn = TRUE) {
  fail <- function(message) stop(message, call. = FALSE)
  positive_scalar <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x) && x > 0
  if (missing(units_per_metre) || !positive_scalar(units_per_metre)) {
    fail("units_per_metre must specify how many source units represent one metre (a positive number).")
  }
  if (!positive_scalar(room_height)) fail("room_height must be positive, in metres.")
  if (!positive_scalar(margin) || margin != floor(margin)) fail("margin must be a positive integer, in metres.")
  if (!is.character(floor_name) || length(floor_name) != 1L || is.na(floor_name) ||
      !grepl("^[A-Za-z][A-Za-z0-9_]*$", floor_name)) fail("Use a floor_name starting with a letter, containing only letters, digits and underscores.")
  if (!is.logical(warn) || length(warn) != 1L || is.na(warn)) fail("warn must be TRUE or FALSE.")
  if (!is.character(json_file) || length(json_file) != 1L || is.na(json_file) || !file.exists(json_file)) {
    fail("json_file must identify an existing JSON file.")
  }
  if (!is.null(output_file) && (!is.character(output_file) || length(output_file) != 1L ||
                                is.na(output_file) || !nzchar(output_file) || !dir.exists(dirname(output_file)))) {
    fail("output_file must identify a file in an existing directory.")
  }
  source <- jsonlite::fromJSON(json_file, simplifyVector = FALSE)
  if (!is.list(source) || !length(source) || !is.null(names(source))) fail("Expected a non-empty JSON array of rooms.")
  xy <- function(point, label) {
    if (!is.list(point) || !is.numeric(point$x) || !is.numeric(point$y) ||
        length(point$x) != 1L || length(point$y) != 1L ||
        !is.finite(point$x) || !is.finite(point$y)) fail(paste("Invalid x/y coordinates:", label))
    c(x = point$x, y = point$y)
  }

  ids <- vapply(seq_along(source), function(i) {
    item <- source[[i]]
    if (!is.list(item) || !positive_scalar(item$id_stanza_univoca) ||
        item$id_stanza_univoca != floor(item$id_stanza_univoca) || item$id_stanza_univoca > .Machine$integer.max) {
      fail(paste("Invalid id_stanza_univoca in room", i))
    }
    as.integer(item$id_stanza_univoca)
  }, integer(1))
  types <- vapply(seq_along(source), function(i) {
    item <- source[[i]]
    if (is.null(item$tipo)) "Normal" else as.character(item$tipo)
  }, character(1))
  standard_types <- c("Normal", "Stair", "Spawnroom", "Fillingroom", "Waitingroom")
  custom_types <- unique(types[!types %in% standard_types])
  typesID <- ifelse(types %in% standard_types, match(types, standard_types) + 3L,
                    match(types, custom_types) + 8L)
  areas <- vapply(seq_along(source), function(i) {
    item <- source[[i]]
    if (is.null(item$area)) "None" else as.character(item$area)
  }, character(1))
  namesRoom <- vapply(seq_along(source), function(i) {
    item <- source[[i]]
    if(is.null(item$nome_stanza))
      paste0("Room_", item$id_stanza_univoca)
    else item$nome_stanza
  }, character(1))

  if (anyDuplicated(ids)) fail("id_stanza_univoca must be unique.")
  polygons <- lapply(seq_along(source), function(i) {
    vertices <- source[[i]]$poligono
    if (!is.list(vertices) || length(vertices) < 3L) fail(paste("Room", ids[i], "needs at least three polygon vertices."))
    do.call(rbind, lapply(vertices, xy, label = paste("polygon of room", ids[i])))
  })
  bounds <- t(vapply(polygons, function(p) c(min(p[, 1]), min(p[, 2]), max(p[, 1]), max(p[, 2])), numeric(4)))
  if (any(bounds[, 3] <= bounds[, 1] | bounds[, 4] <= bounds[, 2])) fail("Polygons must have positive width and height.")
  origin <- c(x = min(bounds[, 1]), y = min(bounds[, 2]))
  transform_xy <- function(p) (p - origin) / units_per_metre + margin
  left <- round((bounds[, 1] - origin[1]) / units_per_metre + margin)
  top <- round((bounds[, 2] - origin[2]) / units_per_metre + margin)
  right <- round((bounds[, 3] - origin[1]) / units_per_metre + margin)
  bottom <- round((bounds[, 4] - origin[2]) / units_per_metre + margin)
  collapsed <- ids[right <= left | bottom <= top]
  if (length(collapsed)) fail(paste0("The scale collapses rooms ", paste(collapsed, collapse = ", "),
                                     " on the one-metre grid. Reduce units_per_metre."))
  colours <- grDevices::hcl.colors(length(ids), "Dark 3")
  colours <- apply(grDevices::col2rgb(colours), 2, function(rgb) paste0("rgba(", paste(rgb, collapse = ", "), ", 1)"))
  rooms <- data.frame(ID = ids, typeID = typesID, type = types, x = left, y = top,
                      center_x = left + floor((right - left + 1) / 2),
                      center_y = top + floor((bottom - top + 1) / 2),
                      w = bottom - top, l = right - left, h = room_height,
                      object_rotation = 0, Name = namesRoom,
                      colorFill = colours, colorFillBase = colours,
                      colorBorder = "rgba(0, 0, 0, 1)", area = areas, CanvasID = floor_name,
                      stringsAsFactors = FALSE)
  rownames(rooms) <- NULL
  for (i in seq_along(source)) {
    if (!is.null(source[[i]]$centro_di_massa)) {
      centre <- round(transform_xy(xy(source[[i]]$centro_di_massa, paste("centre of room", ids[i]))))
      rooms$center_x[i] <- max(left[i] + 1, min(right[i], centre[1]))
      rooms$center_y[i] <- max(top[i] + 1, min(bottom[i], centre[2]))
    }
  }
  rectangular <- vapply(seq_along(polygons), function(i) {
    p <- polygons[[i]]
    next_p <- p[c(seq.int(2L, nrow(p)), 1L), , drop = FALSE]
    on_border <- p[, 1] %in% bounds[i, c(1, 3)] | p[, 2] %in% bounds[i, c(2, 4)]
    axis_aligned <- p[, 1] == next_p[, 1] | p[, 2] == next_p[, 2]
    area <- abs(sum(p[, 1] * next_p[, 2] - next_p[, 1] * p[, 2])) / 2
    all(on_border & axis_aligned) && isTRUE(all.equal(area, prod(bounds[i, 3:4] - bounds[i, 1:2])))
  }, logical(1))

  door_map <- data.frame(roomID = integer(), source_x = numeric(), source_y = numeric(),
                         side = character(), offset = integer(), projected_x = numeric(),
                         projected_y = numeric(), distance_m = numeric(), ambiguous_side = logical())
  for (i in seq_along(source)) {
    entries <- source[[i]]$porte_identificate
    if (is.null(entries)) next
    if (!is.list(entries) || !is.null(names(entries))) fail(paste("porte_identificate must be an array in room", ids[i]))

    for (entry in entries) {
      point <- xy(entry, paste("door of room", ids[i]))
      p <- transform_xy(point)
      if (!rectangular[i]) {
        # The room itself is approximated by a bounding rectangle, but moving
        # its doors to that artificial border would alter the source geometry.
        position <- round(p)
        natural_sides <- character()
        natural_offsets <- integer()
        if (position[2] == top[i] && position[1] > left[i] && position[1] <= right[i]) {
          natural_sides <- c(natural_sides, "top")
          natural_offsets <- c(natural_offsets, position[1] - left[i])
        }
        if (position[2] == bottom[i] + 1 && position[1] > left[i] && position[1] <= right[i]) {
          natural_sides <- c(natural_sides, "bottom")
          natural_offsets <- c(natural_offsets, position[1] - left[i])
        }
        if (position[1] == left[i] && position[2] > top[i] && position[2] <= bottom[i]) {
          natural_sides <- c(natural_sides, "left")
          natural_offsets <- c(natural_offsets, position[2] - top[i])
        }
        if (position[1] == right[i] + 1 && position[2] > top[i] && position[2] <= bottom[i]) {
          natural_sides <- c(natural_sides, "right")
          natural_offsets <- c(natural_offsets, position[2] - top[i])
        }
        if (length(natural_sides)) {
          side <- natural_sides[1]
          offset <- natural_offsets[1]
        } else {
          side <- "interior"
          offset <- NA_integer_
        }
        distances <- rep(0, max(1L, length(natural_sides)))
      } else {
        clamped_x <- max(left[i], min(right[i] + 1, p[1]))
        clamped_y <- max(top[i], min(bottom[i] + 1, p[2]))
        candidates <- rbind(top = c(clamped_x, top[i]), bottom = c(clamped_x, bottom[i] + 1),
                            left = c(left[i], clamped_y), right = c(right[i] + 1, clamped_y))
        distances <- rowSums(sweep(candidates, 2, p, "-")^2)
        side <- names(which.min(distances))
        horizontal <- side %in% c("top", "bottom")
        along <- if (horizontal) p[1] - left[i] else p[2] - top[i]
        offset <- max(1, min(if (horizontal) rooms$l[i] else rooms$w[i], floor(along + 0.5)))
        position <- c(if (horizontal) left[i] + offset else if (side == "right") right[i] + 1 else left[i],
                      if (!horizontal) top[i] + offset else if (side == "bottom") bottom[i] + 1 else top[i])
      }
      door_map <- rbind(door_map, data.frame(roomID = ids[i], source_x = point[1], source_y = point[2],
                                             side = side, offset = offset, projected_x = position[1], projected_y = position[2],
                                             distance_m = sqrt(sum((p - position)^2)),
                                             ambiguous_side = if (rectangular[i]) {
                                               sum(abs(distances - min(distances)) < 1e-10) > 1
                                             } else length(natural_sides) > 1,
                                             row.names = NULL))
    }
  }
  removed_room_ids <- ids[vapply(ids, function(id) {
    room_doors <- door_map[door_map$roomID == id, , drop = FALSE]
    nrow(room_doors) > 0 && all(room_doors$side == "interior")
  }, logical(1))]
  discarded_doors <- door_map[door_map$roomID %in% removed_room_ids, , drop = FALSE]
  if (length(removed_room_ids)) {
    door_map <- door_map[!door_map$roomID %in% removed_room_ids, , drop = FALSE]
    rooms <- rooms[!rooms$ID %in% removed_room_ids, , drop = FALSE]
  }
  if (!nrow(rooms)) fail("All rooms were removed because they contain only internal doors.")

  source_keys <- paste(sprintf("%.17g", door_map$source_x), sprintf("%.17g", door_map$source_y), sep = ":")
  door_map$source_door_id <- match(source_keys, unique(source_keys))
  cells <- paste(door_map$roomID, door_map$projected_x, door_map$projected_y, sep = ":")
  door_map$doorID <- match(cells, unique(cells))
  door_map$merged <- duplicated(cells)
  opposite <- c(top = "bottom", bottom = "top", left = "right", right = "left")
  for (key in unique(source_keys)) {
    pair <- which(source_keys == key)
    pair <- pair[!duplicated(cells[pair])]
    if (length(pair) != 2 || length(unique(door_map$roomID[pair])) != 2) next
    a <- door_map[pair[1], ]
    b <- door_map[pair[2], ]
    members <- which(door_map$doorID %in% c(a$doorID, b$doorID))
    compatible <- a$side == "interior" || b$side == "interior" || opposite[a$side] == b$side
    if (length(unique(door_map$roomID[members])) == 2 && compatible &&
        a$projected_x == b$projected_x && a$projected_y == b$projected_y) {
      door_map$doorID[members] <- min(a$doorID, b$doorID)
    }
  }
  doors <- data.frame(ID = door_map$doorID, roomID = door_map$roomID, CanvasID = rep(floor_name, nrow(door_map)),
                      side = door_map$side, offset = door_map$offset,
                      x = door_map$projected_x, y = door_map$projected_y)
  doors <- doors[!duplicated(cells), , drop = FALSE]
  r <- rooms[match(doors$roomID, rooms$ID), , drop = FALSE]
  doors$local_x <- doors$x - r$x
  doors$local_y <- doors$y - r$y
  rownames(doors) <- NULL
  unlinked <- integer()
  for (id in unique(door_map$source_door_id)) {
    group <- door_map[door_map$source_door_id == id, ]
    if (length(unique(group$roomID)) > 1 && length(unique(group$doorID)) > 1) unlinked <- c(unlinked, id)
  }
  overlaps <- data.frame(roomID1 = integer(), roomID2 = integer(), area_m2 = numeric())
  active <- which(!ids %in% removed_room_ids)
  for (i in active) {
    for (j in active[active > i]) {
      area <- max(0, min(right[i], right[j]) - max(left[i], left[j])) *
        max(0, min(bottom[i], bottom[j]) - max(top[i], top[j]))
      if (area > 0) overlaps <- rbind(overlaps, data.frame(roomID1 = ids[i], roomID2 = ids[j], area_m2 = area))
    }
  }
  canvas_w <- max(100L, max(c(right[active] + margin + 1, door_map$projected_x + margin), na.rm = TRUE))
  canvas_h <- max(80L, max(c(bottom[active] + margin + 1, door_map$projected_y + margin), na.rm = TRUE))
  if (!is.finite(canvas_w * canvas_h) || canvas_w * canvas_h > 1e7) {
    fail("The selected scale needs more than 10 million canvas cells. Increase units_per_metre.")
  }
  model <- .f4f_floorplan_defaults()
  model$rooms <- rooms[, c("Name", "typeID", "type", "w", "l", "h", "colorFill")]
  names(model$rooms)[2] <- "ID"
  model$roomsINcanvas <- rooms
  model$doorsINcanvas <- doors
  model$floors <- data.frame(ID = 1L, Name = floor_name, Order = 1L)
  model$canvasDimension <- data.frame(canvasWidth = canvas_w * 10, canvasHeight = canvas_h * 10)
  model$matrixCanvas <- matrix(0, nrow = canvas_h, ncol = canvas_w)
  model$selectedId <- rooms$ID[1]
  issues <- character()
  if (length(removed_room_ids)) issues <- c(issues, paste("Rooms removed because they contain only internal doors:",
                                                          paste(removed_room_ids, collapse = ", ")))
  retained_rectangularized <- ids[!rectangular & !ids %in% removed_room_ids]
  if (length(retained_rectangularized)) issues <- c(issues, paste("Bounding rectangles used for rooms:",
                                                                  paste(retained_rectangularized, collapse = ", ")))
  if (nrow(overlaps)) issues <- c(issues, paste(nrow(overlaps), "overlapping rectangle pairs require review."))
  if (length(unlinked)) issues <- c(issues, paste(length(unlinked), "source shared doors became separate doors because the imported walls do not meet."))
  if (any(door_map$merged)) issues <- c(issues, paste(sum(door_map$merged), "door entries merged into occupied wall cells."))
  if (any(door_map$distance_m > 1)) issues <- c(issues, paste(sum(door_map$distance_m > 1), "door entries moved more than one metre to reach a wall cell."))
  if (any(door_map$ambiguous_side)) issues <- c(issues, "Some door positions are equally close to multiple sides; see ambiguous_side in the door report.")
  attr(model, "floorplan_import") <- list(
    units_per_metre = units_per_metre, origin = origin, margin = margin, source = source,
    rooms = data.frame(roomID = ids, Name = namesRoom, rectangularized = !rectangular,
                       removed = ids %in% removed_room_ids,
                       source_xmin = bounds[, 1], source_ymin = bounds[, 2],
                       source_xmax = bounds[, 3], source_ymax = bounds[, 4]),
    doors = door_map, discarded_doors = discarded_doors,
    removed_rooms = removed_room_ids, overlaps = overlaps,
    unlinked_shared_doors = unlinked, issues = issues)
  if (warn && length(issues)) warning(paste(c("Review the imported floor plan:", issues,
                                              "Details: attr(model, 'floorplan_import')."), collapse = "\n"), call. = FALSE)
  if (!is.null(output_file)) saveRDS(model, output_file)
  invisible(model)
}

.f4f_floorplan_defaults <- function() {
  list(rooms = NULL, roomsINcanvas = NULL, doorsINcanvas = NULL,
       nodesINcanvas = NULL, pathINcanvas = NULL,
       types = data.frame(Name = c("Normal", "Stair", "Spawnroom", "Fillingroom", "Waitingroom"),
                          ID = 4:8, Color = c("rgba(255, 0, 0, 1)", "rgba(0, 255, 0, 1)",
                                              "rgba(0, 0, 255, 1)", "rgba(0, 0, 0, 1)", "rgba(0, 100, 30, 1)")),
       canvasDimension = NULL, matrixCanvas = NULL, selectedId = 1L,
       floors = NULL, floorsBG = list(),
       areas = data.frame(Name = "None", ID = 0L, Color = "rgba(0, 0, 0, 1)"),
       agents = NULL, disease = NULL, resources = NULL,
       agent_resource_links_df = data.frame(agent_id = character(), agent_name = character(),
                                            room = character(), object = character(), has_access = logical(), concurrent_usage = numeric()),
       color = "Room", matricesCanvas = NULL,
       starting = data.frame(seed = NA, simulation_days = 10, day = "Monday", time = "00:00", step = 60, nrun = 100, prun = 10),
       rooms_whatif = data.frame(Measure = character(), Type = character(), Parameters = character(), From = numeric(), To = numeric()),
       agents_whatif = data.frame(Measure = character(), Type = character(), Parameters = character(), From = numeric(), To = numeric()),
       initial_infected = data.frame(Type = character(), Number = numeric()), outside_contagion = NULL,
       virus_parameters = data.frame(radius = 1.05, virus_variant = 1, ngen_base = 0.589, vl = 9,
                                     decay_rate = 0.636, gravitational_settling_rate = 0.39, inhalation_rate_pure = 0.521),
       cancel_button_selected = FALSE, TwoDVisual = NULL, width = NULL, length = NULL, height = NULL,
       roomObjects = list())
}
