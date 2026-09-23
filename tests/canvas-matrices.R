suppressPackageStartupMessages(library(dplyr))
helpers <- c("inst/Shiny/Rfunctions.R", "../inst/Shiny/Rfunctions.R",
             system.file("Shiny", "Rfunctions.R", package = "FORGE4FLAME"))
source(helpers[file.exists(helpers)][1])

make_room <- function(id, name, x, y, l, w, canvas = "floor0", rotation = 0) {
  data.frame(ID = id, Name = name, x = x, y = y, l = l, w = w,
             CanvasID = canvas, type = "Normal", typeID = 4,
             center_x = x + 1, center_y = y + 1, object_rotation = rotation)
}

make_object <- function(id, x = 0, y = 0, length = 1, width = 1) {
  list(id = id, name = paste0("Object", id), x = x, y = y,
       length = length, width = width, color = "black",
       isObstacle = TRUE, capacity = NA_real_)
}

model <- list(
  canvasDimension = data.frame(canvasHeight = 400, canvasWidth = 400),
  roomsINcanvas = rbind(make_room(10, "Outer", 2, 2, 20, 20),
                       make_room(25, "Inner", 7, 7, 6, 6),
                       make_room(99, "Inner", 7, 7, 6, 6, "floor1")),
  roomObjects = list(Outer = list(make_object(101, 1, 1)),
                     Inner = list(make_object(202, 2, 2))),
  doorsINcanvas = empty_canvas_doors(),
  nodesINcanvas = data.frame(ID = 1, x = 30, y = 30, CanvasID = "floor0"))
model$doorsINcanvas <- add_canvas_door(model$doorsINcanvas, model$roomsINcanvas,
                                     roomID = 25, side = "left", offset = 3)
model$matricesCanvas <- CanvasMatrices(model)
stopifnot(identical(names(model$matricesCanvas), c("WithoutMask", "WithMask")))
without <- model$matricesCanvas$WithoutMask
with <- model$matricesCanvas$WithMask
stopifnot(identical(names(without), c("floor0", "floor1")),
          identical(names(without$floor0), c("floor", "rooms")),
          identical(names(without$floor0$rooms), c("Outer_10", "Inner_25")))

# Room IDs survive without a mask; the nested interior becomes an obstacle
# with a mask, including the cell just inside its door.
stopifnot(without$floor0$floor[4, 4] == 10,
          without$floor0$floor[10, 10] == 25,
          without$floor0$floor[7, 7] == 0,
          without$floor0$floor[2, 4] == 0,
          without$floor0$floor[10, 7] == 2,
          with$floor0$floor[4, 4] == 1,
          all(with$floor0$floor[8:13, 8:13] == 0),
          with$floor0$floor[10, 8] == 0,
          with$floor0$floor[10, 7] == 2,
          without$floor0$floor[31, 31] == 3,
          with$floor0$floor[31, 31] == 3)

# WithoutMask embeds the child ID, walls, door and objects in its parent.
# WithMask turns the same footprint into an obstacle. The child's own matrix
# remains usable and retains its objects in both variants.
for (variant in model$matricesCanvas) {
  stopifnot(variant$floor0$rooms$Inner_25[4, 4] == -202,
            variant$floor0$rooms$Outer_10[3, 3] == -101,
            variant$floor0$rooms$Inner_25[4, 1] == 2,
            variant$floor1$rooms$Inner_99[4, 4] == -202)
}
stopifnot(without$floor0$rooms$Outer_10[7, 7] == 25,
          without$floor0$rooms$Outer_10[6, 6] == 0,
          without$floor0$rooms$Outer_10[9, 6] == 2,
          without$floor0$rooms$Outer_10[9, 9] == -202,
          with$floor0$rooms$Outer_10[7, 7] == 0,
          with$floor0$rooms$Outer_10[9, 9] == 0,
          without$floor1$floor[10, 10] == 99,
          with$floor1$floor[10, 10] == 1)

# Room order must not change geometry ownership or masking.
reordered <- model
reordered$roomsINcanvas <- model$roomsINcanvas[3:1, ]
stopifnot(identical(CanvasToMatrix(reordered, canvas = "floor0"), without$floor0$floor),
          identical(CanvasToMatrix(reordered, "NoInterior", "floor0"), with$floor0$floor),
          identical(CanvasRoomToMatrix(reordered, FALSE, "floor0")$Outer_10,
                    without$floor0$rooms$Outer_10),
          identical(CanvasRoomToMatrix(reordered, "NoInterior", "floor0")$Outer_10,
                    with$floor0$rooms$Outer_10))

# Partial overlap masks only the intersection, leaving the exposed interior.
partial <- model
partial$roomsINcanvas$x[2] <- partial$roomsINcanvas$y[2] <- 19
partial$doorsINcanvas <- empty_canvas_doors()
partial <- CanvasMatrices(partial)
stopifnot(partial$WithoutMask$floor0$floor[21, 21] == 25,
          partial$WithMask$floor0$floor[21, 21] == 0,
          partial$WithMask$floor0$floor[25, 25] == 1,
          partial$WithMask$floor0$floor[23, 21] == 0,
          partial$WithoutMask$floor0$rooms$Outer_10[21, 21] == -202,
          partial$WithMask$floor0$rooms$Outer_10[21, 21] == 0)

# A third nesting level preserves the innermost room's own object matrix.
nested <- model
nested$roomsINcanvas <- rbind(nested$roomsINcanvas, make_room(35, "Leaf", 9, 9, 2, 2))
nested$roomObjects$Leaf <- list(make_object(303))
nested <- CanvasMatrices(nested)
for (variant in nested) stopifnot(variant$floor0$rooms$Leaf_35[2, 2] == -303)
stopifnot(nested$WithoutMask$floor0$floor[10, 10] == 35,
          nested$WithMask$floor0$floor[10, 10] == 0,
          nested$WithoutMask$floor0$rooms$Inner_25[4, 4] == -303,
          nested$WithoutMask$floor0$rooms$Outer_10[9, 9] == -303,
          nested$WithMask$floor0$rooms$Inner_25[4, 4] == 0,
          nested$WithMask$floor0$rooms$Outer_10[9, 9] == 0)

# Rotate the same asymmetric 6-by-4 layout, including objects at the edges.
# Expected rectangles are hand-calculated in local matrix (row, column) indices.
local({
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  layouts <- list(
    `0` = list(size = c(6L, 8L), a = list(2, 2:3), b = list(4:5, 6:7), c = list(3:4, 4)),
    `90` = list(size = c(8L, 6L), a = list(2:3, 5), b = list(6:7, 2:3), c = list(4, 3:4)),
    `180` = list(size = c(6L, 8L), a = list(5, 6:7), b = list(2:3, 2:3), c = list(3:4, 5)),
    `270` = list(size = c(8L, 6L), a = list(6:7, 2), b = list(2:3, 4:5), c = list(5, 3:4)))
  for (angle in c(0L, 90L, 180L, 270L)) {
    layout <- layouts[[as.character(angle)]]
    rotated <- model
    rotated$roomsINcanvas[2, c("l", "w", "object_rotation")] <-
      list(layout$size[2] - 2L, layout$size[1] - 2L, angle)
    rotated$roomObjects$Inner <- list(make_object(404, length = 2),
                                     make_object(505, x = 4, y = 2, length = 2, width = 2),
                                     make_object(606, x = 2, y = 1, width = 2))
    rotated$matricesCanvas <- CanvasMatrices(rotated)
    saveRDS(rotated, path)
    restored <- normalize_room_doors(readRDS(path))
    stopifnot(restored$roomsINcanvas$object_rotation[2] == angle,
              identical(restored$matricesCanvas, CanvasMatrices(restored)))

    expected <- matrix(1, nrow = layout$size[1], ncol = layout$size[2])
    expected[c(1, nrow(expected)), ] <- 0
    expected[, c(1, ncol(expected))] <- 0
    expected[layout$a[[1]], layout$a[[2]]] <- -404
    expected[layout$b[[1]], layout$b[[2]]] <- -505
    expected[layout$c[[1]], layout$c[[2]]] <- -606
    expected[4, 1] <- 2 # The selected left door remains in canvas coordinates.
    for (variant in restored$matricesCanvas) {
      stopifnot(identical(variant$floor0$rooms$Inner_25, expected))
    }
    without_outer <- restored$matricesCanvas$WithoutMask$floor0$rooms$Outer_10
    with_outer <- restored$matricesCanvas$WithMask$floor0$rooms$Outer_10
    stopifnot(all(without_outer[layout$a[[1]] + 5, layout$a[[2]] + 5] == -404),
              all(without_outer[layout$b[[1]] + 5, layout$b[[2]] + 5] == -505),
              all(without_outer[layout$c[[1]] + 5, layout$c[[2]] + 5] == -606),
              all(with_outer[layout$a[[1]] + 5, layout$a[[2]] + 5] == 0),
              all(with_outer[layout$b[[1]] + 5, layout$b[[2]] + 5] == 0),
              all(with_outer[layout$c[[1]] + 5, layout$c[[2]] + 5] == 0))
  }
})

# Shared walls/doors and non-overlapping rooms remain usable in both exports.
adjacent <- model
adjacent$roomsINcanvas <- rbind(make_room(10, "Outer", 2, 2, 4, 4),
                              make_room(25, "Inner", 7, 2, 4, 4))
adjacent$doorsINcanvas <- add_canvas_door(empty_canvas_doors(), adjacent$roomsINcanvas,
                                        roomID = 10, side = "right", offset = 2)
adjacent <- CanvasMatrices(adjacent)
for (variant in adjacent) {
  stopifnot(variant$floor0$floor[4, 7] == 2,
            variant$floor0$floor[3, 7] == 0,
            variant$floor0$rooms$Outer_10[3, 6] == 2,
            variant$floor0$rooms$Inner_25[3, 1] == 2)
}
stopifnot(adjacent$WithoutMask$floor0$floor[3, 9] == 25,
          adjacent$WithMask$floor0$floor[3, 9] == 1)

# Binary interiors remain available to placement and graph checks, even for
# room IDs 2 and 3, which otherwise collide with door and graph-point markers.
binary <- model
binary$roomsINcanvas <- make_room(2, "Outer", 2, 2, 4, 4)
binary$doorsINcanvas <- empty_canvas_doors()
stopifnot(CanvasToMatrix(binary, TRUE, "floor0")[3, 3] == 1,
          CanvasToMatrix(binary, FALSE, "floor0")[3, 3] == 2)

empty <- model
empty$roomsINcanvas <- NULL
empty$doorsINcanvas <- empty_canvas_doors()
empty$nodesINcanvas <- NULL
stopifnot(identical(CanvasMatrices(empty), list(WithoutMask = list(), WithMask = list())),
          all(CanvasToMatrix(empty, "NoInterior", "floor0") == 0))

stopifnot(inherits(try(CanvasRoomToMatrix(model, TRUE, "floor0"), silent = TRUE),
                     "try-error"))

# Exercise the serialized model, not just the in-memory matrices.
local({
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  saveRDS(model, path)
  restored <- readRDS(path)
  stopifnot(identical(restored$matricesCanvas, model$matricesCanvas),
            restored$matricesCanvas$WithoutMask$floor0$rooms$Inner_25[4, 4] == -202,
            restored$matricesCanvas$WithMask$floor0$floor[10, 10] == 0)
})
cat("Canvas matrix export checks passed.\n")
