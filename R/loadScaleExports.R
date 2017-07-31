#' Load scale exports file.
#'
#' Just loads the scaleExports and scaleExportsRaw csv files from a local directory. Doesn't do anything else.
#'
#'
#' @return Dataframes called scaleExports and scaleExportsRaw to the global environment
#' @export
#'

loadScaleExports <- function(env = .GlobalEnv) {

    library(tidyverse)

    coffeestats::setDataDir()

    # Assume there's a file called YYYY-MM-DD-usda.tidy.csv; get the most recent
    tryCatch({
        fileName <- dplyr::last(
            list.files(path = coffeestats, pattern = "scaleExports.csv")
        )
        scaleExports <- suppressWarnings(readr::read_csv(
            file = file.path(coffeestats, fileName), col_types = readr::cols())
        )

    }, error = function(e) {
        stop("Error loading the scaleExports.csv, check it exists in the coffeestats directory.")
    })
    tryCatch({
        fileName2 <- dplyr::last(
            list.files(path = coffeestats, pattern = "scaleExportsRaw.csv")
        )
        scaleExportsRaw <- suppressWarnings(readr::read_csv(
            file = file.path(coffeestats, fileName2), col_types = readr::cols()
        ))
    }, error = function(e) {
        stop("Error loading the scaleExportsRaw file, check it exists in the coffeestats directory.")
    })

    env$scaleExports <- scaleExports
    env$scaleExportsRaw <- scaleExportsRaw
}
