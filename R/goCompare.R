#' Go Compare
#'
#' Runs a comparison for a specific country of various production estimates, and returns graphs of exports so far and production.
#'
#' @param countryName Case sensitive, needs to be a country in the system.
#'
#' @export
#' @import tidyverse
#' @importFrom readr read_csv
#' @importFrom forcats fct_relevel
#' @importFrom plotly plot_ly layout subplot
#' @importFrom lubridate year month
#' @importFrom magrittr extract2

goCompare <- function(countryName, refresh = FALSE) {

    library(tidyverse);library(coffeestats)

    coffeestats::setDataDir()

    # Using USDA/ICO/FOL data from my-flowsheets and others from production-estimates
    # Only load if either not in global env or if refresh is TRUE
    if(!exists('mts') | refresh) { suppressMessages(coffeestats::loadMTS()) }
    if(!exists('flow') | refresh) { suppressMessages(coffeestats::loadFlowsheetData()) }

    prodCompare <- suppressMessages(
        bind_rows(
            readxl::read_excel(file.path(coffeestats, "production-estimates.xlsx")) %>%
                select(country = Country, source = Source, year = Year, production = Production) %>%
                # Don't worry about Arabica/Robusta split for now, just use total.
                group_by(country, source, year) %>%
                summarise(value = sum(production)) %>%
                ungroup(),
            flow %>%
                filter(series == "Production") %>%
                select(country, source, year, value)
        ) %>%
            filter(year >= max(year) - 5)
        )

    # Import crop year info
    cy <- suppressMessages(readr::read_csv(
        file = file.path(coffeestats, "cropyears.csv")
    ))

    # Put everything together
    compData <- suppressMessages(
        full_join(x = prodCompare, y = cy) %>%
            na.omit() %>%
            filter(year >= max(year) - 5) %>%
            mutate(source = forcats::fct_relevel(source, "ME"))
    )

    # Production graph and table
    tableProd <- filter(compData, country == countryName) %>%
        arrange(source, year, value)
    graphProd <- suppressWarnings(plotly::plot_ly(
        tableProd %>% group_by(source),
        type = "scatter", mode = "lines",
        x = ~year, y = ~value, color = ~source,
        text = ~paste0(source, ": ", round(value, 1)), hoverinfo = "text"
    ) %>%
        plotly::layout(
            title = ~paste("Production by", country),
            xaxis = list(title = "", showgrid = FALSE),
            yaxis = list(title = "", zeroline = FALSE)
        )
    )


    # Some countries are missing in the exports file, so just use production
    if(!countryName %in% mts$country) {
        print(graphProd)
        print(tableProd %>% mutate(value = round(value, 0)) %>% spread(key = year, value = value))
        return("No export data found")
    }

    # Otherwise need to do monthly exports by crop year
    ## First get month number
    cyNum <- compData %>%
        filter(country == countryName) %>%
        slice(1) %>%
        magrittr::extract2('cropYear') %>%
        match(., month.abb)
    if(length(cyNum) == 0) {
        return("Error in crop year lookup")
    }

    ## Create an ordered factor of months starting with the crop year
    monthFac <- c(month.abb, month.abb)[cyNum:(cyNum + 11)]

    ## Now create a new dataframe just for the specific country
    compExp <- mts %>%
        filter(country == countryName) %>%
        mutate(
            monthFac = ordered(month.abb[lubridate::month(month)], levels = monthFac)
        ) %>%
        arrange(monthFac, lubridate::year(month)) %>%
        filter(month >= month[1]) %>%
        arrange(month) %>%
        mutate(cropYear = as.factor(
            c(
                rep(lubridate::year(min(month)):((lubridate::year(min(month)) + nrow(.) %/% 12) -1 ),
                    each = 12),
                rep(lubridate::year(min(month)) + nrow(.) %/% 12,
                    each = nrow(.) %% 12)
            )
        ))

    graphExp <- suppressWarnings(plotly::plot_ly(
        compExp %>% group_by(cropYear),
        x = ~monthFac, y = ~value, color = ~cropYear, type = "scatter", mode = "lines"
    ) %>%
        plotly::layout(
            title = ~paste("Exports by", countryName),
            xaxis = list(title = "", showgrid = FALSE),
            yaxis = list(title = "Exports", zeroline = FALSE)
        )
    )

    tableExp <- coffeestats::getExportsToDate(countryName)

    print(tableExp)
    print(tableProd %>% mutate(value = round(value, 0)) %>% spread(key = year, value = value))

    suppressWarnings(plotly::subplot(
        graphExp, graphProd, nrows = 2
    ))
}

