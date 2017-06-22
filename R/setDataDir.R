#' Set the data folder.
#'
#' I use this to store data in a dropbox folder. All other functions should relate back to this function, so you only need to change it once if the folder moves.
#' Almost definitely not best practice, but just stores the folder location as a string in the global environment. It seems to work for me
#' @examples
#' setDataDir(folderLocation = "~/Dropbox/Work/coffeestats")
#' 
#' @export

setDataDir <- function(folderLocation = "~/Dropbox/Work/coffeestats/data") {

    coffeestats <<- folderLocation

}
