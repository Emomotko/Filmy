#' Funkcja zlicza wartosci macierzy podobienstw dla jednego filmu
#'
#' Funkcja \code{wylicz5} zlicza wartosci macierzy podobienstw, gdy mamy dany indeks
#' jednego filmu. Podobienstwa sa zliczanie dla filmu glownego i wszystkich filmow
#' ktore maja wyzsze indeksy (sa dalej w tabeli). Poniewaz macierz jest symetryczna
#' nie ma sensu zliczanie tego samego podobienstwa 2 razy
#' 
#' @usage wylicz5(numer=1, aktorzy, rezyserzy, filmy, ile_wszystkich)
#' @param numer - liczba naturalna >=1 - zaczynamy analize dla filmu o numerze numer z ramki
#' danych filmy
#' @param aktorzy - ramka danych z aktorami
#' @param rezyserzy - ramka danych z rezyserami - niestety te ramki lepiej wczytac do pamieci,
#' wtedy wyszukiwanie bedzie dzialac o wiele szybciej
#' @param filmy - ramka danych z filmami
#' @param ile_wszystkich - wartosc naturalna, mowi nam z iloma filmami porownujemy film
#' glowny
#'
#' @return
#' funkcja zwraca wektor o dlugosci rownej liczbie porownywanych filmow, narazie nie
#' zapisuje bo nei do konca wiem jaka forma powinno miec zapisywanie
#' 
#'
#'@import stringi
#'@import tm


wylicz5 <- function(numer=1, aktorzy, rezyserzy, filmy, ile_wszystkich){
  
  #wczytujemy film glowny
  f_glowny<- filmy[numer,]
  
  #wyznaczamy czas trwania filmu
  czas_glowny <- as.numeric(f_glowny[1,5])
  
  #upraszczamy opis
  opis_gl_upr <- uprosc(f_glowny[1,8])
  
  #upraszczamy fabule
  fabula_gl_upr <- uprosc(f_glowny[1,16])
  
  #wyszukujemy aktorow w bazie i wydobywamy ich wiek oraz kraj pochodzenia
  z_aktorzy <- aktorzy_wyszukaj_wiek_kraj4(f_glowny, aktorzy)
  
  #wyszukujemy aktorow w bazie i wydobywamy ich wiek oraz kraj pochodzenia
  
  z_rezyserzy <- rezyser_wyszukaj_wiek_kraj4(f_glowny, rezyserzy)
  
  #tutaj bedziemy przechowywac wartosci
  wartosci <- numeric(ile_wszystkich-numer) 
  
  #ponizej robimy to samo dla fimlu porownywanego  i zliczamy wskazniki
  for(i in (numer+1):ile_wszystkich){
    
    
    f_por <- filmy[i,]
    czas_por <- as.numeric(f_por[1,5])
    
    cz <- czas(czas_glowny,czas_por)

    
    opisy_por <- uprosc(f_por[1,8])
    
    fabuly_por <- uprosc(f_por[1,16])
    
    frakcje_opis <- frakcja_powtarzanych_slow(opis_gl_upr,opisy_por)
    
    frakcje_fabula <- frakcja_powtarzanych_slow(fabula_gl_upr,fabuly_por)
    
    
    z_a <- list(z_aktorzy,aktorzy_wyszukaj_wiek_kraj4(f_por, aktorzy))

    
    
    r_a <-  rozrzut4(z_a)

    
    m_a <- mediana4(z_a)
    
    kr_a <- kraje(z_a)
    
    #rezyserzy
    
    z_r <- list(z_rezyserzy,rezyser_wyszukaj_wiek_kraj4(f_por, rezyserzy))
    
    m_r <- mediana4(z_r)
    
    kr_r <- kraje(z_r)
    
    wartosci[i-numer] <- sum(c(cz, frakcje_opis,frakcje_fabula, r_a,m_a,kr_a,m_r,kr_r),na.rm=TRUE)
  }
    
  return(wartosci)
  
}

###filmy <- read.csv2("filmyClean.csv",stringsAsFactors=FALSE) - trzeba ustawic zeby
#napisy nie byly faktorami bo sie psuje
