#' Exports prediction function
#'
#' Allows you to enter a specific country and explore the scaleExports/MTS data. Useful for flowsheet predictions etc.
#'
#' @param getCountry Enter a specific country to search for; case sensitive.
#' @param table Returns a summary table to the console if true.
#'
#' @return A plotly graph and a table.
#' @export
#' @import tidyverse
#' @import plotly
#' @import lubridate

predExports <- function(getCountry, table = TRUE) {

    library(tidyverse);library(plotly);library(lubridate)

    # Set default data dir, and import scaleExports and scaleExportsRaw
    coffeestats::setDataDir()
    coffeestats::loadScaleExports()

    countryRaw <- full_join(
        x = filter(scaleExportsRaw, country == getCountry) %>%
            arrange(month) %>%
            mutate(cropMonth = ordered(month(month),
                                       levels = unique(month(month)),
                                       labels = unique(month.abb[month(month)]))),
        y = filter(scaleExports, country == getCountry),
        by = "country"
    )

    countrySummary <- full_join(
        x = countryRaw %>%
            filter(cropYear.x == cropYear.y) %>%
            select(country, cropMonth, actual = value, scaleExports),
        y = countryRaw %>%
            filter(cropYear.x != cropYear.y) %>%
            group_by(cropMonth) %>%
            mutate(min = min(value), max = max(value), prediction = avShare * scaleExports) %>%
            select(country, cropMonth, min, max, prediction) %>%
            slice(1)
    ) %>%
        full_join(
            x = .,
            y = countryRaw %>%
                filter(cropYear.x == cropYear.y - 1) %>%
                select(country, cropMonth, lastYear = value)
        )

    p1 <- plot_ly(countrySummary, x = ~cropMonth) %>%
        add_ribbons(name = "Min/max", line = list(width = 0),
                    ymin = ~min, ymax = ~max, hoverinfo = "none",
                    fillcolor = "#ffe090", opacity = 0.4) %>%
        add_lines(name = "Prediction", y = ~prediction,
                  line = list(color = "#cc593d", dash = "dash"),
                  text = ~paste0(cropMonth, " prediction: ",
                                 format(round(prediction, 1), big.mark = ",")),
                  hoverinfo = "text") %>%
        add_lines(name = "Actual", y = ~actual,
                  line = list(color = "#cc593d"),
                  text = ~paste0(cropMonth, " actual: ",
                                 format(round(actual, 1), big.mark = ",")),
                  hoverinfo = "text") %>%
        add_lines(name = "Last year", y = ~lastYear,
                  line = list(color = "#659fb5"),
                  text = ~paste0(cropMonth, " last year: ",
                                 format(round(lastYear, 1), big.mark = ",")),
                  hoverinfo = "text") %>%
        layout(title = paste("Export prediction function for", getCountry),
               xaxis = list(title = ""),
               yaxis = list(title = "", separatethousands = TRUE,
                            ticklen = 20, tickcolor = "#FFF"))

    t1 <- countrySummary %>%
        select(country, cropMonth, actual, prediction, scaleExports)
    if(table) {
        print(t1)
    }

    return(p1)

}
