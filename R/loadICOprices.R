#' Load ICO prices
#'
#' Just loads data from a local file called icoPrices.csv, which should be in the coffeestats data folder and returns a dataframe called prices.
#'
#' @return Dataframe called icoPrices to the global environment
#' @importFrom readr read_csv
#' @export

loadICOprices <- function() {

    coffeestats::setDataDir()

    # Assume there's a file called YYYY-MM-DD-usda.tidy.csv; get the most recent
    tryCatch({
        icoPrices <- suppressMessages(
            readr::read_csv(
                file = file.path(coffeestats, "icoPrices.csv"),
                col_types = readr::cols())
        )
    }, error = function(e) {
        stop("Error loading the csv file, check it exists in the coffeestats directory.")
    }

    )

    .GlobalEnv$prices <- icoPrices
}
