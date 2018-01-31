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
            dplyr::mutate(
                country = dplyr::case_when(
                    .$country == "Congo, Rep. of" ~ "Congo",
                    .$country == "Congo, Dem. Rep. of" ~ "Congo, DR",
                    grepl("Ivoire", .$country) ~ "Cote d'Ivoire",
                    grepl("Lao", .$country) ~ "Laos",
                    grepl("Trinidad", .$country) ~ "Trinidad and Tobago",
                    TRUE ~ .$country
                ),
                year = stringr::str_sub(year, 1, 4),
                series = series) %>%
            na.omit()
    }

    # Get these four files, might need to double-check links if they ever change.
    icoDataExp <- purrr::map2_df(
        .x = c("1d%20-%20Gross%20Opening%20stocks.xlsx",
               "1a%20-%20Total%20production.xlsx",
               "1b%20-%20Domestic%20consumption.xlsx",
               "1e%20-%20Exports%20-%20crop%20year.xlsx"
        ),
        .y = c("stocks", "production", "consumption", "exports"),
        .f = getICOfile
    )

    # World consumption: Want to get coffee years for all countries
    # Need to download three more files: Member disappearance (consumption) and non-Member imports + re-exports.
    getOthers <- function(filename, series) {
        require(tidyverse)
        baseurl <- "http://www.ico.org/historical/1990%20onwards/Excel/"
        tempfile <- tempfile(fileext = ".xlsx")
        suppressWarnings(download.file(paste0(baseurl, filename),
                                       destfile = tempfile, quiet = TRUE))
        readxl::read_excel(tempfile, skip = 3) %>%
            # dplyr::select(-2) %>%
            # Need to remove duplicatd UAE entry
            dplyr::distinct(`Calendar years`, .keep_all=TRUE) %>%
            tidyr::gather(., -1, key = year, value = value) %>%
            dplyr::select(country = 1, year, value) %>%
            dplyr::filter(!(country %in% c("Africa", "Asia & Oceania", "Caribbean",
                                           "Central America & Mexico", "Europe",
                                           "North America", "South America")),
                          !(country %in% c("China (Mainland)", "Hong Kong", "Macao")),
                          !(country %in% c("Abu Dhabi", "Dubai")),
                          !(grepl("Total", country))) %>%
            dplyr::mutate(
                # More country name fixes
                country = dplyr::case_when(
                    grepl("Belgium", .$country) ~ "Belgium",
                    grepl("South Africa", .$country) ~ "South Africa",
                    grepl("China", .$country) ~ "China",
                    grepl("Iran", .$country) ~ "Iran",
                    .$country == "Korea, Dem. People's Rep. of" ~ "North Korea",
                    .$country == "Korea, Rep. of" ~ "South Korea",
                    grepl("Syrian", .$country) ~ "Syria",
                    TRUE ~ .$country
                ),
                year = stringr::str_sub(year, 1, 4),
                series = series) %>%
            na.omit()
    }
    icoOthers <- purrr::map2_df(
        .x = c("4b%20-%20Disappearance.xlsx",
               "5a%20-%20Non-member%20imports.xlsx",
               "5b%20-%20Non-member%20re-exports.xlsx"
        ),
        .y = c("consumption", "imports", "exports"),
        .f = getOthers
    )

    # Convert imports/exports to net imports
    icoOthers2 <- dplyr::bind_rows(
        icoOthers %>% dplyr::filter(series == "consumption"),
        icoOthers %>% dplyr::filter(series != "consumption") %>%
            tidyr::spread(key = series, value = value) %>%
            dplyr::mutate(consumption = imports - exports) %>%
            dplyr::select(country, year, consumption) %>%
            tidyr::gather(., -country, -year, key = series, value = value)
    )

    # Convert calendar years to coffee years
    icoOthersCY <- icoOthers2 %>%
        dplyr::group_by(country, series) %>%
        dplyr::mutate(valueCY = (0.25*value) + (0.75*dplyr::lead(value))) %>%
        dplyr::select(country, year, series, value = valueCY)

    # Still need to handle European Union; take out for now?
    icoOthersCY <- filter(icoOthersCY, country != "European Union")

    # Merge back with other for now
    icoData <- bind_rows(
        icoDataExp, icoOthersCY
    )

    # Write to csv for easy loading - why was this commented out?
    readr::write_csv(icoData, path = file.path(
        coffeestats,
        paste0(lubridate::today(), "-ico-historical.csv")
    ))

    loadICOhistorical()
}
