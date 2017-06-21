#' Get ICO pdf prices.
#'
#' Currently, the ICO provides the last twelve months of daily prices as a pdf on their website.
#' This function extracts data from all 12 pdfs and returns a tidy dataframe called prices
#' If writeCSV = TRUE, then it also checks for the icoPrices.csv file in coffeestats/data and adds any missing values.
#' If writeCSV = FALSE, it just returns the prices from the 12 pdfs.
#'
#' @examples
#' getIcoPdfPrices(writeCSV = TRUE)
#' getIcoPdfPrices()
#' @export

getIcoPdfPrices <- function(writeCSV = FALSE, env = .GlobalEnv) {

    require(tidyverse);require(stringr);require(lubridate)
    setDataDir()


    # Want to get a list of the last twelve months
    # Most recent is always a month before yesterday
    baseurl <- "http://www.ico.org/prices/p1-"
    returnPrices <- data.frame()
    for(i in 1:12) {
        m1 <- lubridate::today() - days(1) - months(i)
        m1String <- paste0(month.name[lubridate::month(m1)], lubridate::year(m1))
        getUrl <- str_c(baseurl, m1String, ".pdf")
        print(paste("Processing:", m1String))

        # One of the urls doesn't work, needs an extra hypeh.
        if(!httr::url_success(getUrl)) {
           m1String <- paste(month.name[lubridate::month(m1)], lubridate::year(m1), sep="-")
           getUrl <- str_c(baseurl, m1String, ".pdf")
        }
        suppressMessages(getTable <- tabulizer::extract_tables(getUrl, method="data.frame")[[1]] %>%
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
            na.omit())
        returnPrices <- rbind(returnPrices, getTable) %>% arrange(date)
    }

    if(writeCSV) {
        oldPrices <- readr::read_csv(file.path(coffeestats, "icoPrices.csv"))
        newPrices <- filter(returnPrices, date > max(oldPrices$date))
        allPrices <- rbind(oldPrices, newPrices) %>%
            write_csv(file.path(coffeestats, "icoPrices.csv"))
        env$icoPrices <- allPrices
    } else {
        env$icoPrices <- allPrices
    }

}
