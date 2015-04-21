#' Create a file from a data.frame and saves it.
#'
#' Function \code{create} gets info about actors.
#'
#' @aliases subtitle
#' @param fname Name of a file where info is to be saved.
#' @param frame a data frame with 6 colums
#' @return invisible(NULL)  - all info is gathered in a particular file.
#' @import stringi
#' 
#' 
#'  

create3 <- function(fname, lista) {
    
    if (!file.exists(fname)) {
        f <- file(fname, open = "a")
        # tworze pierwszy wiersz w pliku:
        writeLines(stri_paste("\"id\"", "\"text\"", sep = ";"), f)
    } else f <- file(fname, open = "a")
    
    if (stri_length(lista$id) > 0) {
        
        for (i in seq_along(lista$text)) {
            # dopisuje do pliku kolejny wiersz
            writeLines(stri_paste(lista$id, lista$text[i], sep = ";"), f)
        }
    }
    close(f)
}
 
