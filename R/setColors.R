#' Set Colors
#'
#' Returns a vector of 5 hex codes to the global environment.
#'
#' @return blogColours (for now)
#' @export

setColors <- function() {
    # Just saves a vector of colors to the global environment
    .GlobalEnv$blogColours <- c("#619fb5", "#ef8066", "#ffcb90", "#70c793", "#7882c0")

}
