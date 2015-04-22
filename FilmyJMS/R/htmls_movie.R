#' Parse many of HTML pages for actorS AND director
#'
#' Function \code{htmls_movie} gets info about actors.
#'
#' @aliases subtitle
#' @param www A link - see full cast from imdb.
#' @param selector named character vector with selectors which we are going to analyse.
#' @return list of parsed pages or 'NA' if pages don't exist.
#' @import rvest
#'  XML
#'  stringi
#' 
#' 
#' 
htmls_movie <- function(www, selector) {
    
    url <- tryCatch({
        
        html(www)
        
    }, error = function(e) {
        "NA"
    })
    
    if (is.character(url) && url == "NA") 
        return("NA")
    
    
    a <- getNodeSet(url, selector)
    if (is.null(a)) 
        return("NA")
    linki <- xml_attr(a, "href")
    if (length(linki) == 0) 
        return("NA")
    co <- stri_detect_fixed(linki, "name")
    if (all(co == FALSE)) 
        return("NA")
    linki <- linki[co]
    if (names(selector) == "actor") {
        czy <- stri_detect_fixed(linki, "ttfc_fc_cl_t")
    } else if (names(selector) == "director") {
        czy <- stri_detect_fixed(linki, "ttfc_fc_dr")
    }
    if (any(czy == TRUE)) {
        linki <- linki[czy]
        podstawa <- "http://www.imdb.com"
        linki <- stri_paste(podstawa, linki)
        n <- length(linki)
        if (n > 30 && n < 80) {
            k <- floor(n/2)
        } else if (n >= 80) {
            k <- floor(n/3)
        } else k <- n
        
        linki <- linki[1:k]
        
        return(sapply(linki, html))
    } else return("NA")
    
} 
