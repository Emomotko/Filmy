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
    
    selector <- c(actor = "//table[@class='cast_list']//a")
    
    htmls <- tryCatch({
        htmls_movie(www, selector)
    }, error = function(e) {
        "NA"
    })
    
    if (length(htmls) == 1 && is.character(htmls) && htmls == "NA") 
        return("NA")
    lista <- harvest_people(htmls)
    create("Actors.csv", lista)
    return(lista$name)
    
} 
