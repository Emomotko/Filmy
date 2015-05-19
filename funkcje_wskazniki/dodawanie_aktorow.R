aktorzy <- read.csv2("ActorsSuperClean.csv")
rezyserzy <- read.csv2("DirectorsSuperClean.csv")
filmy <- read.csv2("filmyClean.csv",stringsAsFactors=FALSE)
filmy <- filmy[1:1000,]

dodaj_aktorzy_rezyser <- function(filmy, aktorzy, rezyserzy){
  
  ramka <- cbind(filmy, aktorzy_wiek=0,aktorzy_rozrzut=0,aktorzy_kraje = NA,rezyser_wiek=0,
                 rezyser_kraje=NA)
  n <- nrow(filmy)
  
  for(i in 1:n){
    
    ktorzy <- aktorzy_wyszukaj_wiek_kraj4(filmy[i,],aktorzy)
    ramka[i,24] <- median(ktorzy[[1]],na.rm=TRUE)
    ramka[i,25] <- IQR(ktorzy[[1]],na.rm=TRUE)
    kraje <- ktorzy[[2]][which(ktorzy[[2]]!="NA")]
    kraje <- na.omit(kraje)
    if(length(kraje)>0){
      ramka[i,26] <- stri_flatten(na.omit(ktorzy[[2]]),collapse="@")
    }    
    rez <- rezyser_wyszukaj_wiek_kraj4(filmy[i,],rezyserzy)
    kraje <- rez[[2]][which(rez[[2]]!="NA")]
    kraje <- na.omit(kraje)
    ramka[i,27]<-median(rez[[1]],na.rm=TRUE)
    if(length(kraje)>0){
      ramka[i,28] <- stri_flatten(na.omit(rez[[2]]),collapse="@")
    }    
    
    
  }
  ramka
  
}

##s <- dodaj_aktorzy_rezyser(filmy,aktorzy,rezyserzy) #trzeba jeszcze zapisac bedzie

