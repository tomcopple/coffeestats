#' Get ICO production/consumption figures
#'
#' \code{getICOproduction()} scrapes the ICO production/consumption pdf tables from the website and saves them into a local csv file
#'
#' @return Returns a dataframe called icoFlow to the global environment, and a file with today's date to the data folder
#' @export
#'
#' @import tidyverse
#' @importFrom tabulizer extract_tables
#' @importFrom lubridate today

getICOproduction <- function() {

    coffeestats::setDataDir()

    library(tidyverse)

    prodPDF <- 'http://www.ico.org/prices/po-production.pdf'
    if(httr::HEAD(prodPDF)$status_code != 200) return("Problem downloading production file: check the pdf link in function?")

    # Use the tabulizer package to extract from pdf
    # NB Can't suppress these info messages, just print a reassurance
    print("Don't worry about these info messages, it means it's working")
    icoProdRaw <- tabulizer::extract_tables(prodPDF, method = 'data.frame')[[1]]

    # File is a bit messy - make first row column names and delete final column
    names(icoProdRaw) <- head(icoProdRaw, 1)

    # Then remove extraneous, some spelling correction and gather into tidy.
    icoProd <- icoProdRaw %>%
        select(country = 1, 2:5) %>%
        filter(row_number() > 2) %>%
        filter(!country %in% c("Colombian Milds", "Other Milds", "Brazilian Naturals",
                               "Africa", "Asia & Oceania", "Mexico & Central America",
                               "South America", "Others")) %>%
        mutate(country = case_when(
            grepl("Ivoire", .$country) ~ "Cote d'Ivoire",
            grepl("Congo, Dem. Rep. of", .$country) ~ "Congo, DR",
            grepl("Lao", .$country) ~ "Laos",
            TRUE ~ .$country
        )) %>%
        gather(., -country, key = year, value = production) %>%
        mutate(production = as.numeric(stringr::str_replace_all(production, "[[:space:]]", "")),
               country = ifelse(country == "TOTAL", "World total", country))

    # Might as well also get Consumption while we're here
    conPDF <- "http://www.ico.org/prices/new-consumption-table.pdf"
    if(httr::HEAD(conPDF)$status_code != 200) return("Problem downloading consumption file: check the pdf link in function?")

    icoConRaw <- tabulizer::extract_tables(conPDF, method = 'data.frame')[[1]]
    names(icoConRaw) <- head(icoConRaw, 1)
    icoCon <- icoConRaw %>%
        select(country = 1, 2:5) %>%
        filter(row_number() > 2) %>%
        filter(!country %in% c("Africa", "Asia & Oceania", "Central America & Mexico",
                               "Europe", "North America", "South America", "Others", ""),
               !stringr::str_detect(country, "\\(|\\)|countries")) %>%
        mutate(country = ifelse(stringr::str_detect(country, "Ivoire"), "Cote d'Ivoire", country)) %>%
        gather(., -country, key = year, value = consumption) %>%
        mutate(consumption = as.numeric(stringr::str_replace_all(consumption, "[[:space:]]", "")),
               year = stringr::str_sub(year, 0, 4))

    # Merge together and write a csv?
    icoFlow <- full_join(icoProd, icoCon) %>%
        arrange(country, year) %>%
        readr::write_csv(path = file.path(coffeestats, stringr::str_c(lubridate::today(), "-icoFlow.csv")),
                         na = "")

    coffeestats:::loadICOproduction()

    print("Done!")


}
