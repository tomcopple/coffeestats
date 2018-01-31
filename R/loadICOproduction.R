#' Loads ICO production/consumption
#'
#' Just loads  data from a local file, which should be in the coffeestats data folder, and have the form "2017-01-01-icoFlow.csv".
#'
#'
#' @return Dataframe called icoFlow to the global environment
#'

loadICOproduction <- function(env = .GlobalEnv) {

    library(tidyverse)

    coffeestats::setDataDir()

    # Assume there's a file called YYYY-MM-DD-icoflow.csv; get the most recent
    tryCatch({
        fileName <- dplyr::last(
            list.files(path = coffeestats, pattern = "icoFlow.csv")
        )
        icoFlow <- suppressWarnings(readr::read_csv(
            file = file.path(coffeestats, fileName), col_types = readr::cols())
        )
    }, error = function(e) {
        stop("Error loading the csv file, check it exists in the coffeestats directory. It should be called 'YYYY-MM-DD-icoFlow.csv'")
    }

    )

    env$icoFlow <- icoFlow
}
