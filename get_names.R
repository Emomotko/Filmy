#' Collect names of producers and musicians
#'
#' Function \code{get_names} gets info about actors.
#'
#' @aliases subtitle
#' @param www A link - see full cast from imdb.
#' @return a list: first element - musicians, second - producers
#' @import rvest
#'  XML
#'  stringi
#' 
#' 
get_names <- function(www) {
    
    tryCatch({
        
        url <- html(www)
        
    }, error = function(e) {
        return(invisible(NULL))
    })
    
    # PRODUCENCI
    
    tryCatch({
        prod <- url %>% html_nodes(".simpleCreditsTable:nth-child(9) a") %>% html_text() %>% 
            stri_trim_both()
    }, error = function(e) {
        prod <- "NA"
    })
    
    # MUZYCY
    
    tryCatch({
        music <- url %>% html_nodes(".simpleCreditsTable:nth-child(11) a") %>% html_text() %>% 
            stri_trim_both()
    }, error = function(e) {
        music <- "NA"
    })
    
    
    if (length(prod) == 0) 
        prod <- "NA"
    if (length(music) == 0) 
        music <- "NA"
    return(list(music = music, producers = prod))
} 
