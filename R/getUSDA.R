#' Get USDA data
#'
#' @description Downloads a zip file containing a csv from the USDA website, imports it, cleans it and write two new csvs: USDA-tidy.csv containing a tidy dataframe of the data, and USDA-flowsheets.csv containing flowsheet info for the last five years, for importing into Excel.
#' @param url Should be a link to the most recent USDA coffee \strong{zip file}.
#'
#' @return Also returns a dataframe called usda, which is loaded from the usda-tidy.csv using loadUSDA() under the hood.
#' @export
#'
#' @examples getUSDA()

getUSDA <- function(url = "https://apps.fas.usda.gov/psdonline/downloads/psd_coffee_csv.zip") {

    require(tidyverse)
    coffeestats::setDataDir()

    # Download the zip file to coffeestats/data temp file, and import the csv file
    # Shouldn't change
    tryCatch({
        tempfile <- tempfile()
        suppressWarnings(download.file(url, destfile = tempfile, quiet = TRUE))
        usdaRaw <- readr::read_csv(file = unz(tempfile, "psd_coffee.csv"),
                                   col_types = readr::cols())
    },
    error = function(e) {
        stop("Error downloading zip file, check the link address?", call. = FALSE)
    }
    )

    # The raw csv file contains A LOT of data; just going to keep the bare essentials for now.
    # Note to self - maybe come back and see what else if here?
    usda <- usdaRaw %>%
        dplyr::select(
            country = Country_Name, year = Market_Year,
            series = Attribute_Description, value = Value
        ) %>%
        dplyr::mutate(
            ## For some reason the years are always one ahead? Maybe give a reminder to check.
            year = year - 1,
            ## Rename some series to be more consistene
            series = dplyr::case_when(
                .$series == "Bean Exports" ~ "Green Exports",
                .$series == "Bean Imports" ~ "Green Imports",
                .$series == "Beginning Stocks" ~ "Opening Stocks",
                .$series == "Domestic Consumption" ~ "Consumption",
                .$series == "Roast & Ground Exports" ~ "Roasted Exports",
                .$series == "Roast & Ground Imports" ~ "Roasted Imports",
                .$series == "Rst,Ground Dom. Consum" ~ "Roasted Consumption",
                .$series == "Soluble Dom. Cons." ~ "Soluble Consumption",
                TRUE ~ .$series
            ),
            ## Same with country names (think these are all)
            country = dplyr::case_when(
                .$country == "Congo (Brazzaville)" ~ "Congo",
                .$country == "Congo (Kinshasa)" ~ "Congo, DR",
                .$country == "Korea, South" ~ "South Korea",
                .$country == "United States" ~ "USA",
                .$country == "Yemen (Sanaa)" ~ "Yemen",
                TRUE ~ .$country
            )
        ) %>%
        ## Don't bother with other production, distribution and supply
        dplyr::filter(
            !(series %in% c("Other Production", "Total Distribution", "Total Supply"))
        ) %>%
        readr::write_csv(
            path = file.path(coffeestats, paste0(lubridate::today(), "-usda-tidy.csv"))
        )

    print("NB Check to make sure that marketing years are still one year ahead?")

    # Also produce a flowsheet for easy use in country flowsheets
    usda %>%
        dplyr::filter(
            year %in% c(2012:2016),
            series %in% c("Opening Stocks", "Arabica Production", "Robusta Production",
                          "Production", "Consumption", "Exports", "Imports", "Ending Stocks")
        ) %>%
        tidyr::spread(
            key = series, value = value
        ) %>%
        dplyr::select(
            country, year, `Opening Stocks`, `Arabica Production`, `Robusta Production`,
            Production, Consumption, Exports, Imports, `Ending Stocks`
        ) %>%
        readr::write_csv(
            file.path(coffeestats, paste0(lubridate::today(), "-usda-flowsheet.csv"))
            )

    coffeestats::loadUSDA()

}
