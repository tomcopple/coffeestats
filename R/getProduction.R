#' Import production data from an input file stored on my computer.
#'
#' Doesn't take any arguments, and returns a dataframe called prodAll to the global environment.
#'
#' @examples
#' getProduction()

 getProduction <- function(env = .GlobalEnv) {

    library(tidyverse);library(readxl)
    basedir <- '~/Dropbox/Work/coffeestats'

    env$prodAll <- read_csv(file.path(basedir, 'input/production-all.csv')) %>%
        gather(., -Country, key = year, value = value) %>%
        mutate(year = substr(year, 0, 4)) %>%
        as_data_frame()

}

