#' Get Monthly Trade Statistics
#'
#' \code{getMTS()} scrapes the ICO website for export data and saves the last six months to a local csv file.
#' @param refresh Do you want to download new data from the ICO website?
#' @param writeFile Do you want to save this to the local data file.
#'
#' @return Returns a dataframe called mts to the global environment.
#' @export
#' @import tidyverse
#' @importFrom tabulizer extract_tables
#' @importFrom lubridate dmy

getMTS <- function(refresh = TRUE, writeFile = TRUE) {

    coffeestats::setDataDir()

    library(tidyverse)

    # Might be necessary to check the link location if there are problems.
    if(refresh == TRUE) {
        # Table location shouldn't change
        pdfTable <- 'http://www.ico.org/prices/m3-exports.pdf'

        # Use the tabulizer package to extract from pdf
        mts <- tabulizer::extract_tables(pdfTable, method = 'data.frame')[[1]] %>%
            gather(., -X, key = month, value = value) %>%
            # Fix some of the results
            mutate(
                # Use case_when (dplyr) to correct some of the names
                X = case_when(
                    grepl("Ivoire", .$X) ~ "Cote d'Ivoire",
                    grepl("Congo, Dem. Rep. of", .$X) ~ "Congo, DR",
                    TRUE ~ .$X
                ),
                # Trim white space,
                X = trimws(X),
                # Create dates
                month = lubridate::dmy(paste0("01.", month)),
                value = as.numeric(gsub("[[:space:]]", "", value))
            ) %>%
            # Remove Colombian, Other and Brazilian; just keep Arabica/Robusta
            filter(!(X %in% c('Colombian Milds', 'Other Milds', 'Brazilian Naturals'))) %>%
            rename(country = X) %>%
            as_data_frame()

        # Update any existing data
        oldMTS <- readr::read_csv(file.path(coffeestats, "export-data.csv"))

        if(class(oldMTS$month) != "Date") {
            oldMTS$month <- lubridate::dmy(oldMTS$month)
        }

        oldMTS <- filter(oldMTS, month < min(mts$month))

        # Bind together
        newMTS <- bind_rows(mts, oldMTS) %>%
            arrange(month)

        # Write to csv
        if(writeFile) {
            readr::write_csv(newMTS, file.path(coffeestats, "export-data.csv"))
        }
    }

    coffeestats::loadMTS()

}
