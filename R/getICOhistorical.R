#' Get ICO historical data
#'
#' @description Downloads data from the excel files on the ICO historical data website, for production, domestic consumption, crop year exports and opening stocks. Writes a csv file to the coffeestats data folder and returns a dataframe called icoData to the global environment.
#' @return icoData A dataframe containing data back to 1990.
#' @export

getICOhistorical <- function() {

    coffeestats::setDataDir()

    # Download the excel files to a temp file, then import
    # Small function to download and clean files
    getICOfile <- function(filename, series) {
        require(tidyverse)
        baseurl <- "http://www.ico.org/historical/1990%20onwards/Excel/"
        tempfile <- tempfile(fileext = ".xlsx")
        suppressWarnings(download.file(paste0(baseurl, filename),
                                       destfile = tempfile, quiet = TRUE))
        readxl::read_excel(tempfile, skip = 3) %>%
            dplyr::select(-2) %>%
            tidyr::gather(., -1, key = year, value = value) %>%
            dplyr::select(country = 1, year, value) %>%
            dplyr::filter(!(grepl("group", country)),
                          !(grepl("Total", country))) %>%
            dplyr::mutate(country = ifelse(grepl("Ivoire", country), "Cote d'Ivoire", country),
                          year = stringr::str_sub(year, 1, 4),
                          series = series) %>%
            na.omit()
    }

    # Get these four files, might need to double-check links if they ever change.
    icoData <- purrr::map2_df(
        .x = c("1d%20-%20Gross%20Opening%20stocks.xlsx",
               "1a%20-%20Total%20production.xlsx",
               "1b%20-%20Domestic%20consumption.xlsx",
               "1e%20-%20Exports%20-%20crop%20year.xlsx"
        ),
        .y = c("stocks", "production", "consumption", "exports"),
        .f = getICOfile
    )

    readr::write_csv(
        icoData, path = file.path(coffeestats, paste0(lubridate::today(), "-ico-historical.csv"))
        )

    loadICOhistorical()
}
