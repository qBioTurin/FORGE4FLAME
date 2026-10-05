source(file.path('..', 'inst', 'Shiny', 'Rfunctions.R'))
room <- data.frame(ID = 1L, Name = 'Room', CanvasID = 'Floor',
                   x = 0, y = 0, l = 6, w = 4, type = 'Normal', object_rotation = 0)
model <- list(roomsINcanvas = room, doorsINcanvas = empty_canvas_doors())
stopifnot(length(room_doors_for_objects(model, 'Room')) == 0)
for (side in c('top', 'bottom', 'left', 'right')) {
  model$doorsINcanvas <- add_canvas_door(model$doorsINcanvas, room, 1, side, 2)
}
d <- room_doors_for_objects(model, 'Room')
stopifnot(length(d) == 4, d[[1]]$x == 1.5, d[[1]]$y == 0,
          d[[2]]$y == 4, d[[3]]$x == 0, d[[4]]$x == 6)
for (door in d) {
  obj <- c(list(name = 'Blocked'), door$clearance)
  stopifnot(check_door_collision(model, 'Room', list(obj))$collision)
}
stopifnot(!check_door_collision(model, 'Room', list(
  list(name = 'Free', x = 2, y = 1, length = 2, width = 2)))$collision)
# Inverse projection must match the matrix rotation used for object export.
for (angle in c(0, 90, 180, 270)) {
  model$roomsINcanvas$object_rotation <- angle
  projected <- room_doors_for_objects(model, 'Room')
  for (i in seq_along(d)) {
    original <- matrix(0, nrow = if (angle %in% c(90, 270)) 6 else 4,
                       ncol = if (angle %in% c(90, 270)) 4 else 6)
    a <- projected[[i]]$clearance
    original[floor(a$y) + 1, floor(a$x) + 1] <- 1
    actual <- rotate_matrix(original, angle)
    b <- d[[i]]$clearance
    stopifnot(actual[floor(b$y) + 1, floor(b$x) + 1] == 1)
  }
}
# Both rooms see an opening on their shared wall.
peer <- room
peer$ID <- 2L
peer$Name <- 'Peer'
peer$x <- 7
model$roomsINcanvas <- rbind(room, peer)
model$doorsINcanvas <- new_room_door(room, 'right', 2, 1)
p <- room_doors_for_objects(model, 'Peer')
stopifnot(length(p) == 1, p[[1]]$x == 0, p[[1]]$y == 1.5)
# Moving a door replaces the old position rather than retaining a phantom dot.
model$doorsINcanvas <- new_room_door(room, 'right', 3, 1)
stopifnot(room_doors_for_objects(model, 'Peer')[[1]]$y == 2.5)
cat('Object door projection and collision checks passed\n')
