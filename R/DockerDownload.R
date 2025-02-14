#' @title Download for the first time all containers embedded in the workflows
#' @description This is a function that prepares the docker environment to be used for the first time the application is installed.
#' @param containers.file, a character string with the name of the file which indicate which are
#'  the initial set of containers to be downloaded. If NULL then the set is given by a
#'   file called "containersNames.txt" located in the folder inst/Containers of F4F package.
#' @author Pernice Simone
#'
#' @examples
#'\dontrun{
##'     #running runDocker
#'      downloadContainers()
#'
#' }
#' @export

downloadContainers <- function(containers.file = NULL, tag = "latest") {
  if(is.null(containers.file)) {
    containers.file <- system.file("Containers", "containersNames.txt", package = "FORGE4FLAME")
  }

  if (!file.exists(containers.file)) {
    stop("The specified containers file does not exist.")
  }

  containers <- read.table(containers.file)

  userid <- system("id -u", intern = TRUE)
  username <- system("id -un", intern = TRUE)

  failed_containers <- c()

  for (i in seq(1, nrow(containers), 1)) {
    status <- system(paste("docker pull", containers[i, 1]))

    if (status != 0) {  # If docker pull fails
      failed_containers <- rbind(failed_containers, containers[i, , drop = FALSE])
    }
  }

  # Save the updated container list if some failed
  if (length(failed_containers) > 0) {
    output_file <- file.path(path.package("FORGE4FLAME"), "Containers", "containersNames.txt")
    write.table(failed_containers, output_file, row.names = TRUE, quote = FALSE)
  }
}
