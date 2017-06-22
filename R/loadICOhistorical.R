#' Load ICO historical data
#'
#' Just loads ico historical data from a local file, which should be in the coffeestats data folder, and have the form YYYY-MM-DD-ico-historical
#'
#' @return Dataframe called icoData to the global environment
#' @export

loadICOhistorical <- function(env = .GlobalEnv) {

    coffeestats::setDataDir()

    # Assume there's a file called YYYY-MM-DD-ico-historical.csv
    tryCatch({
        fileName <- dplyr::last(
            list.files(path = coffeestats, pattern = "ico-historical.csv")
        )
        icoData <- suppressWarnings(readr::read_csv(
            file = file.path(coffeestats, fileName), col_types = readr::cols())
        )
    }, error = function(e) {
        stop("Error loading the csv file, check it exists in the coffeestats directory. It should have the form YYYY-MM-DD-ico-historical.csv")
    })

    env$icoData <- icoData
}
