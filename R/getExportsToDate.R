#' Get exports to date
#'
#' Just returns a table of cumulative exports to date over the last few years
#'
#' @param countryName Enter a specific country to search for; case sensitive.
#'
#' @return A summary table to the console.
#' @export
#' @import tidyverse

getExportsToDate <- function(countryName) {

    library(tidyverse);library(coffeestats)

    coffeestats::setDataDir()

    # Load mts if not in the workspace
    if(!exists('mts')) { suppressMessages(coffeestats::loadMTS()) }

    # Import crop years and check for countryName
    cy <- suppressMessages(readr::read_csv(
        file = file.path(coffeestats, "cropyears.csv")
    )) %>%
        filter(country == countryName) %>%
        last(.) %>%
        match(., month.abb)

    # Create an ordered factor of 12 months starting with the crop year
    monthFac <- c(month.abb, month.abb)[cy:(cy + 11)]

    # Filter export data and getting cumSum of exports
    result <- mts %>%
        filter(country == countryName) %>%
        mutate(
            monthFac = ordered(month.abb[lubridate::month(month)], levels = monthFac)
        ) %>%
        arrange(monthFac, lubridate::year(month)) %>%
        filter(month >= month[1]) %>%
        arrange(month) %>%
        mutate(
            cropYear = as.factor(c(rep(lubridate::year(min(month)):((lubridate::year(min(month)) + nrow(.) %/% 12) - 1), each = 12),
                                   rep(lubridate::year(min(month)) + nrow(.) %/% 12, each = nrow(.) %% 12))
        )) %>%
        filter(monthFac <= last(monthFac)) %>%
        group_by(cropYear) %>%
        summarise(exports_to_date = sum(value))

    return(result)
}


