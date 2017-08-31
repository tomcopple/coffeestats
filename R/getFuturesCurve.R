#' Get coffee futures curve
#'
#' Runs a graph for Arabica & Robusta futures curves, based on yesterday's settle price.
#'
#' @return A plotly graph to the Viewer. That's it.
#' @export

getFuturesCurve <- function() {
    library(tidyverse)
    # Set Quandl API Key from system environment
    Quandl::Quandl.api_key(api_key = Sys.getenv('QUANDL_APIKEY'))
    blogColours <- c("#619fb5", "#ef8066", "#ffcb90", "#70c793", "#7882c0")

    # Set local data directory
    coffeestats::setDataDir()

    # Get data from Quandl
    quandlRaw <- Quandl::Quandl(c('CHRIS/ICE_KC1', 'CHRIS/ICE_KC2', 'CHRIS/ICE_KC3',
                                  'CHRIS/ICE_KC4', 'CHRIS/ICE_KC5', 'CHRIS/ICE_KC6',
                                  'CHRIS/LIFFE_RC1', 'CHRIS/LIFFE_RC2', 'CHRIS/LIFFE_RC3',
                                  'CHRIS/LIFFE_RC4', 'CHRIS/LIFFE_RC5', 'CHRIS/LIFFE_RC6'),
                                start_date = lubridate::today() - lubridate::days(1))

    # Extract the settle price and tidy
    quandlTidy <- quandlRaw %>%
        dplyr::select(Date, dplyr::contains('Settle')) %>%
        tidyr::gather(., -Date, key = series, value = price) %>%
        dplyr::mutate(contract = stringr::str_extract(series, ".C\\d"),
                      date = lubridate::ymd(Date),
                      price = as.numeric(price),
                      market = ifelse(stringr::str_detect(series, "ICE"), "Arabica", "Robusta")) %>%
        dplyr::select(contract, date, price, market) %>%
        tidyr::drop_na()

    # Draw a graph
    p1 <- plotly::plot_ly(
        filter(quandlTidy, market == "Arabica"), name = "Arabica",
        x = ~contract, y = ~price, text = ~paste0("Arabica: ", price), hoverinfo = "text",
        type = "scatter", mode = "lines", line = list(color = blogColours[1])
    )
    p2 <- plotly::plot_ly(
        filter(quandlTidy, market == "Robusta"), name = "Robusta",
        x = ~contract, y = ~price, text = ~paste0("Robusta: ", price), hoverinfo = "text",
        type = "scatter", mode = "lines", line = list(color = blogColours[2])
    )
    plotly::subplot(p1, p2, nrows = 1, margin = 0.1) %>%
        plotly::layout(
            title = "Coffee futures curves",
            xaxis = list(title = "", showgrid = FALSE),
            xaxis2 = list(title = "", showgrid = FALSE),
            yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE),
            yaxis2 = list(title = "", showgrid = FALSE, zeroline = FALSE)
        )

}
