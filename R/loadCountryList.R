#' loadCountryList
#'
#' Just imports the list of countries, continents, and importer/exporter type.
#'
#' @return countries
#' @export

loadCountryList <- function(env = .GlobalEnv) {

    coffeestats::setDataDir()

    # Import the country-list csv as countries
    countries <- readr::read_csv(
        file = file.path(coffeestats, "country-list.csv"),
        na = ""
    )
    env$countries <- countries

}
