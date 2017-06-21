#' Load USDA files
#'
#'
#' Just loads usda data from a local file, which should be in the coffeestats data folder, and have the form YYYY-MM-DD-usda-tidy.csv.
#'
#'
#' @return Dataframe called usda to the global environment
#' @export
#'

loadUSDA <- function(env = .GlobalEnv) {

    coffeestats::setDataDir()

    # Assume there's a file called YYYY-MM-DD-usda.tidy.csv; get the most recent
    tryCatch({
        fileName <- dplyr::last(list.files(path = coffeestats, pattern = "usda-tidy.csv"))
        usda <- suppressWarnings(readr::read_csv(file = file.path(coffeestats, fileName), col_types = cols()))
    }, error = function(e) {
        stop("Error loading the csv file, check it exists in the coffeestats directory. It should have the form YYYY-MM-DD-usda-tidy.csv")
    }

    )

    env$usda <- usda
}
