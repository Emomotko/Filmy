#' Collect info about actors
#'
#' Function \code{actors} gets info about actors.
#'
#' @aliases subtitle
#' @param www A link - see full cast from imdb.
#' @return list with info about actors
#' @import rvest
#'  XML
#' 
#' 
actors <- function(www) {
  
  selector<-c(actor="//table[@class='cast_list']//a")
  
    tryCatch({
        htmls <- htmls_movie(www, selector)
    }, error = function(e) {
        return(invisible(NULL))
    })
    
    if (length(htmls) == 1 && htmls == "NA") 
        return("NA")
    harvest_people(htmls)
    
} 
