#' Load monthly trade statistics
#'
#' Just loads export data from a local file, which should be in the coffeestats data folder, and called export-data.csv.
#'
#'
#' @return Dataframe called mts to the global environment
#' @export
#'

loadMTS <- function(env = .GlobalEnv) {

    library(tidyverse)

    coffeestats::setDataDir()

    # Assume there's a file called YYYY-MM-DD-usda.tidy.csv; get the most recent
    tryCatch({
       mts <- suppressWarnings(readr::read_csv(
            file = file.path(coffeestats, "export-data.csv"), col_types = readr::cols())
        )
    }, error = function(e) {
        stop("Error loading the csv file, check it exists in the coffeestats directory. It should be called export-data.csv")
    }

    )

    env$mts <- mts
}
