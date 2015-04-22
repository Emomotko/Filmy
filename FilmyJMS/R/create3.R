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

create2 <- function(fname, lista) {
    
    if (!file.exists(fname)) {
        f <- file(fname, open = "a")
        # tworze pierwszy wiersz w pliku:
        writeLines(stri_paste("\"id\"", "\"link\"", "\"Title\"", "\"Year\"", "\"Time\"", 
            "\"Relase\"", "\"Genre\"", "\"Storyline\"", "\"MPAA\"", "\"Country\"", 
            "\"Ratings\"", "\"Ratings_max\"", "\"Users\"", "\"Reviews_number\"", 
            "\"Budget\"", "\"Keywords\"", "\"Description\"", "\"Oscar\"", "\"Another_awards\"", 
            "\"Nominations\"", "\"Music\"", "\"Produced\"", "\"Actors\"", "\"Director\"", 
            sep = ";"), f)
    } else f <- file(fname, open = "a")
    
    if (stri_length(lista$id) > 0) {
        
        
        writeLines(stri_paste(lista$id, lista$link, lista$Title, lista$Year, lista$Time, 
            lista$Relase, lista$Genre, lista$Storyline, lista$MPAA, lista$Country, 
            lista$Ratings, lista$Ratings_max, lista$Users, lista$Reviews_number, 
            lista$Budget, lista$Keywords, lista$Description, lista$Oscar, lista$Another_awards, 
            lista$Nominations, lista$Music, lista$Produced, lista$Actors, lista$Director, 
            sep = ";"), f)
        
    }
    close(f)
} 
