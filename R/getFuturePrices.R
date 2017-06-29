#' @title Get futures prices
#'
#' @description This function queries Quandl for the second month Arabica/Robusta contracts and updates a local csv file: futurePrices.csv. It will automatically go back far enough to get any missing values from the local file.
#' NB Quandl API key is set in the environmental variables
#'
#' @param writeCSV writes any new data to the local data folder. TRUE by default.
#' @export

getFuturePrices <- function(writeCSV = TRUE, env = .GlobalEnv) {

    library(tidyverse)

    # Set Quandl API Key from system environment
    Quandl::Quandl.api_key(api_key = Sys.getenv('QUANDL_APIKEY'))

    # Set local data directory
    coffeestats::setDataDir()

    # Import the local file and check for most recent daily price
    oldFutures <- readr::read_csv(file.path(coffeestats, 'futurePrices.csv'))
    lastDate <- max(oldFutures$date)


    # Put together Quandl GET request
    quandlRaw <- Quandl::Quandl(c('CHRIS/ICE_KC2', 'CHRIS/LIFFE_RC2'),
                        start_date = lastDate) %>%
        dplyr::select(Date, dplyr::contains('Settle')) %>%
        dplyr::select(date = 1, arabica = 2, robusta = 3) %>%
        dplyr::mutate(robusta = robusta/22.0462) %>%
        tidyr::gather(-date, key = series, value = price) %>%
        dplyr::mutate(date = lubridate::ymd(date),
                      price = as.numeric(price)) %>%
        tidyr::drop_na() %>%
        # Couple of weird values, going to remove manually
        dplyr::filter(price > 8) %>%
        tibble::as_tibble()

    # Join oldFutures and quandlRaw to form newPrices, and write back to csv
    newPrices <- dplyr::bind_rows(
        dplyr::filter(oldFutures, date < min(quandlRaw$date)),
        quandlRaw
    ) %>%
        { if(writeCSV)
            readr::write_csv(path = file.path(coffeestats, 'futurePrices.csv'))
            else .
        }

    # Return future prices to the global environment
    env$futurePrices <- newPrices

    }
