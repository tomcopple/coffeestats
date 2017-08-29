#' loadFlowsheetData
#'
#' Import summary data from my-flowsheets into a dataframe called flow.
#'
#' @return flow
#' @export

loadFlowsheetData <- function(env = .GlobalEnv) {

    library(tidyverse);library(readxl)
    coffeestats::setDataDir()
    # Import the summary sheet from my-flowsheets.xlsx and save as dataframe
    flow <- read_excel(
        path = file.path(coffeestats, "my-flowsheets.xlsx"),
        sheet = "flowsheetsSummary"
    ) %>%
        gather(., -series, -source, -country, key = year, value = value) %>%
        mutate(
            country = ifelse(grepl("Ivoire", country), "Cote d'Ivoire", country),
            year = as.integer(year)
            )

    env$flow <- flow

}
