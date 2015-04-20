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
create <- function(fname, frame) {
    
    if (!file.exists(fname)) {
        f <- file(fname, open = "a")
        # tworze pierwszy wiersz w pliku:
        writeLines(stri_paste("\"name\"", "\"jobTitle\"", "\"Place_of_Birth\"", "\"BirthDate\"", 
            "\"DeathDate\"", "\"Film\"", sep = ";"), f)
    } else f <- file(fname, open = "a")
    
    if (nrow(frame) > 0) {
        
        for (i in seq_along(frame[, 1])) {
            # dopisuje do pliku kolejny wiersz
            writeLines(stri_paste(frame[i, 1], frame[i, 2], frame[i, 3], frame[i, 4], 
                frame[i, 5], frame[i, 6], sep = ";"), f)
        }
    }
    close(f)
} 
