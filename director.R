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
  
  htmls <- tryCatch({
    htmls_movie(www, selector)
  }, error = function(e) {"NA"})
  
  if (length(htmls) == 1 && is.character(htmls) && htmls == "NA") 
    return("NA")
  lista <- harvest_people(htmls)
  create("Director.csv",lista)
  return(lista$name)   
} 
