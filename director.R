#' Collect info about director
#'
#' Function \code{director} gets info about actors.
#'
#' @aliases subtitle
#' @param www A link - see full cast from imdb.
#' @return list with info about actors.
#' @import rvest
#'  XML
#' 
#' 

director <- function(www) {
    
  selector <- c(director="//td[@class='name']//a")
  
    tryCatch({
        htmls <- htmls_movie(www, selector)
    }, error = function(e) {
        return(invisible(NULL))
    })
    
  if (length(htmls) == 1 && htmls == "NA") 
    return("NA")
    harvest_people(htmls)
    
} 
