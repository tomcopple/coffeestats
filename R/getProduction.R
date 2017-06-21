#' Import production data from an input file stored on my computer.
#'
#' Doesn't take any arguments, and returns a dataframe called prodAll to the global environment.
#'
#' @examples
#' getProduction()
#' @export

getProduction <- function(env = .GlobalEnv) {

    # Set base data directory
    setDataDir()

    # Import production-all.csv and process into the global environemnt
    env$prodAll <- read_csv(file.path(coffeestats, 'production-all.csv')) %>%
        gather(., -Country, key = year, value = value) %>%
        mutate(year = substr(year, 0, 4)) %>%
        as_data_frame()

}

