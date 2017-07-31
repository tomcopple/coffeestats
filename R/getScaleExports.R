#' Get scale exports
#'
#' Either loads or updates monthly trade statistics, then uses that data to predict exports for next month, and total exports for the crop year.
#'
#' @param refresh Set as TRUE to update monthly trade data from ICO website.
#' @param render Set as TRUE to run a R Markdown report on the data.
#'
#' @return Calls loadScaleExports to return scaleExports and scaleExportsRaw
#' @import tidyverse
#' @import plotly
#' @import lubridate
#' @importFrom rmarkdown render
#' @export

getScaleExports <- function(refresh = FALSE, render = FALSE) {

    library(tidyverse);library(plotly);library(lubridate)

    coffeestats::setDataDir()

    # Run getMTS to make sure export data is available.
    # Only refresh MTS if refresh = TRUE
    if(refresh) {
        coffeestats::getMTS(refresh = TRUE, writeFile = TRUE)
    } else {
        coffeestats::loadMTS()
    }

    # Set prediction month (i.e. assume it's one month ahead of max in mts)
    predMonth  <- max(mts$month) + months(1)

    # Import list of crop years
    cyGroup <- readr::read_csv(file.path(coffeestats, "cropyears.csv")) %>%
        rename(country = Country, cyGroup = Crop.year)

    # Merge together, remove Arabica/Robusta/Other/Total and any other NAs
    exports <- full_join(mts, cyGroup) %>%
        na.omit() %>%
        arrange(country, month) %>%
        # Get crop year - if the month is Apr/Jun/Sep, then use that year
        # Otherwise NA
        mutate(cropYear = ifelse(month.abb[lubridate::month(month)] == cyGroup,
                                 lubridate::year(month),
                                 NA)) %>%
        # Then copy down over NAs
        tidyr::fill(cropYear) %>%
        # Some weird ones where the previous country gets copied down
        filter(cropYear <= lubridate::year(month)) %>%
        na.omit()

    # Now create a separate dataframe of complete years (n==12) and get the
    # average share of exports for each month.
    exportsAv <- exports %>%
        # 1.  Get complete years only
        group_by(country, cropYear) %>%
        mutate(n = n()) %>%
        filter(n == 12) %>%
        # 2.  Get total exports for each month, then share by month
        arrange(country, month) %>%
        mutate(totalExports = sum(value),
               share = value/totalExports) %>%
        # 3.  Group by the month number, and get average share
        group_by(country, lubridate::month(month)) %>%
        mutate(avShare = mean(share)) %>%
        ungroup() %>%
        select(country, month = `lubridate::month(month)`, avShare) %>%
        # 4.  Remove duplicates
        distinct() %>%
        # 5.  Get cumulative share at each point in the year
        group_by(country) %>%
        mutate(cumSum = cumsum(avShare))

    # Now get the prediction dataframe.
    prediction <- full_join(
        # 1.  Total exports for the most recent year
        x = exports %>%
            group_by(country, cropYear) %>%
            summarise(total = sum(value)) %>%
            top_n(n = 1, wt = cropYear),
        # 2.  Average exports for this period
        #     (i.e. cumSum for month before prediction)
        y = exportsAv %>%
            filter(month == lubridate::month(predMonth - months(1)))
    ) %>%
        # 3.  scaleExports is the predicted total for the current year
        mutate(scaleExports = total/cumSum) %>%
        select(country, cropYear, scaleExports) %>%
        # 4.  predMonth is the prediction for next month
        full_join(
            x = .,
            y = filter(exportsAv, month == month(predMonth))
        ) %>%
        mutate(predExports = scaleExports * avShare,
               month = predMonth) %>%
        select(country, cropYear, predMonth = month, scaleExports, predExports) %>%
        # Just remove missing data (Benin, Gabon etc)
        na.omit()

    # Also create a dataframe of all raw data, for other predictions etc.
    scaleExportsRaw <- full_join(
        x = exports %>% mutate(mergeMonth = lubridate::month(month)),
        y = exportsAv %>% rename(mergeMonth = month)
    ) %>%
        select(cyGroup, cropYear, country, month, value, avShare, cumSum)

    # And write both as csvs; prediction as scaleExports
    write_csv(prediction,
              file.path(coffeestats, paste0(lubridate::today(), "-scaleExports.csv")))
    write_csv(scaleExportsRaw,
              file.path(coffeestats, paste0(lubridate::today(), "-scaleExportsRaw.csv")))

    # Finally, if render == TRUE, produce a markdown document summarising the results.
    if(render) {
        rmarkdown::render(system.file("rmd/scaleExportsReport.Rmd", package = "coffeestats"))
        browseURL(system.file("rmd/scaleExportsReport.html", package = "coffeestats"))
    }

    coffeestats::loadScaleExports()
}
