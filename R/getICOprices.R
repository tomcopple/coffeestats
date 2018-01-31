#' Get ICO  prices.
#'
#' Gets daily prices for the ICO composite and 4 groups from the website. Downloads the Excel file for the last two months, and if needs to then goes through the pdfs. Saves in a file called icoPrices.csv in coffeestats/data. Also calls loadICOprices() which returns a dataframe called prices.
#' If writeCSV = FALSE, it just returns the dataframe without updating the csv (I can't remember why you would want to do that, so TRUE by default).
#'
#' @examples
#' getIcoPdfPrices()
#' @importFrom readr cols write_csv
#' @importFrom readxl read_excel
#' @importFrom tabulizer extract_tables
#' @export

getICOprices <- function(writeCSV = TRUE) {

    library(tidyverse)

    # Set local directory
    coffeestats::setDataDir()

    # Download the excel file to a temp file and import
    # Hopefully the link won't change
    url <- "http://www.ico.org/prices/pr-market-prices.xlsx"
    tryCatch({
        tempfile <- tempfile(fileext = ".xlsx")
        suppressWarnings(download.file(url, destfile = tempfile, quiet = TRUE))
        icoRaw <- readxl::read_excel(
            path = tempfile,
            skip = 8,
            col_names = c(
                "date", "ICO.Composite", "Colombian.Milds",
                "Other.Milds", "Brazilian.Naturals", "Robustas"),
            col_types = c("date",
                          "numeric", "skip", "skip", "skip",
                          "numeric", "skip", "skip", "skip",
                          "numeric", "skip", "skip", "skip",
                          "numeric", "skip", "skip", "skip",
                          "numeric")
            )
        icoNew <- gather(icoRaw, -date, key = series, value = price) %>%
            mutate(date = lubridate::as_date(date))
    },
    error = function(e) {
        stop("Error downloading zip file, check the link address?", call. = FALSE)
    }
    )

    # Now load the current price file and see if you've gone back for enough
    icoOld <- readr::read_csv(file.path(coffeestats, "icoPrices.csv"),
                              col_types = readr::cols())

    # If you have, then just rbind back together
    if(max(icoOld$date) >= min(icoNew$date)) {
        icoPrices <- bind_rows(filter(icoOld, date < min(icoNew$date)),
                               icoNew)
    } else {
        # Otherwise have to try the pdfs
        print("Need to go back further, trying the pdf tables. This might take a while")
        baseurl <- "http://www.ico.org/prices/p1-"
        i <- 1
        while(min(icoNew$date) > max(icoOld$date) & i <= 12) {
            m1 <- lubridate::floor_date(lubridate::today() - lubridate::days(1),
                                        unit = "months") - months(i)
            m1String <- stringr::str_c(month.name[lubridate::month(m1)], lubridate::year(m1))
            getUrl   <- stringr::str_c(baseurl, m1String, ".pdf")

            # Check to see if the pdf link words, sometimes needs an extra hyphen
            if(httr::http_error(getUrl)) {
                m1String <- stringr::str_c(month.name[lubridate::month(m1)], lubridate::year(m1),
                                  sep = "-")
                getUrl <- stringr::str_c(baseurl, m1String, ".pdf")
            }

            # Print a status message, just in case
            print(stringr::str_c("Processing: ", m1String))

            # Use tabulizer to extract tables from each one
            suppressMessages(
                getTable <- tabulizer::extract_tables(getUrl, method="data.frame")[[1]] %>%

                    # First two rows are usually rubbish
                    dplyr::filter(row_number() > 2) %>%
                    # And want to give names manually
                    dplyr::select(date = 1, ICO.Composite = 2, Colombian.Milds = 3,
                                  Other.Milds = 4, Brazilian.Naturals = 5, Robustas = 6) %>%
                    # Remove summary from bottom
                    dplyr::filter(!date %in% c('Average', 'High', 'Low')) %>%
                    # Gather, make sure price is a value, and remove NAs
                    tidyr::gather(., -date, key = series, value = price) %>%
                    dplyr::mutate(
                        price = as.numeric(price),
                        date = lubridate::dmy(paste0(date, "-", stringr::str_sub(m1String, -2, -1)))
                    ) %>%
                    na.omit()
            )

            icoNew <- bind_rows(
                filter(getTable, date < min(icoNew$date)),
                icoNew
            )

            i <- i + 1
            }
    }

    # Hopefully now have gone back for enough, do the same thing as earlier.
    if(max(icoOld$date) >= min(icoNew$date)) {
        icoPrices <- bind_rows(filter(icoOld, date < min(icoNew$date)),
                               icoNew)
    } else {
        return("Failed to get enough historical data, probably an error somewhere")
    }

    # Now write csv if TRUE and call loadICOprices.
    if(writeCSV) {
        readr::write_csv(icoPrices, file.path(coffeestats, "icoPrices.csv"))
    }

    coffeestats::loadICOprices()

}
