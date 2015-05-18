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
  
  rok_glowny = f_glowny[1,4]
  gat_glowny = f_glowny[1,7]
  kraj_glowny = f_glowny[1,10]
  muz_glowny = f_glowny[1,20]
  prod_glowny = f_glowny[1,21]
  aktorzy_glowny = f_glowny[1,22]
  rez_glowny = f_glowny[1,23]
  
  ocena_glowny = f_glowny[1,11]
  uzyt_glowny = f_glowny[1,12] #liczba uzytkownikow ktora ocenila dany film
  lr_glowny = f_glowny[1,13] # liczba recenzji
  
  oscar_glowny = f_glowny[1,17]
  nagr_glowny = f_glowny[1,18]

  
  key_glowny = f_glowny[1,15]
  key_g = stri_split_fixed(key_glowny,'@')
  
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
    rok_por = f_por[1,4]
    
    #rok powstania
    rok_powstania <- rok(rok_glowny,rok_por)
    
    #print(i)
    #gatunki
    gat_por = f_por[1,7]
    gatunki <- czy_zawiera(gat_glowny,gat_por)
    
    #kraje
    kraj_por = f_por[1,10]
    kraje <- czy_zawiera(kraj_glowny,kraj_por)
    
    #muzyka
    muz_por = f_por[1,20]
    muzyka <- czy_zawiera(muz_glowny,muz_por)
    
    #producenci
    prod_por = f_por[1,21]
    producent <- czy_zawiera(prod_glowny,prod_por)
    
    #aktorzy
    aktorzy_por = f_por[1,22]
    aktorzy_zaw <- czy_zawiera(aktorzy_glowny,aktorzy_por)
    
    #rezyser
    rez_por = f_por[1,23]
    rezyser <- czy_zawiera(rez_glowny,rez_por)
    
    #oceny itd
    ocena_por = f_por[1,11]
    uzyt_por = f_por[1,12]
    lr_por = f_por[1,13]
    oceny <- roznica(ocena_glowny,ocena_por,0.3)
    uzytkownicy <- roznica(uzyt_glowny,uzyt_por,50000)
    recenzje <- roznica(lr_glowny,lr_por,20)
    
    #klucze itd
    key_por = f_por[1,15]
    key_p = stri_split_fixed(key_por,'@')
    key <- frakcja_powtarzanych_slow(key_g,key_p)
    
    #nagrody
    oscar_por = f_por[1,17]
    oscar <- czy_to_samo(oscar_glowny,oscar_por)
    #inne narody:
    nagr_por = f_por[1,18]
    nagrody <- czy_to_samo(nagr_glowny,nagr_por)
    
    
    
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
    
    wartosci[i-numer] <- sum(c(cz, frakcje_opis,frakcje_fabula, r_a,m_a,kr_a,m_r,kr_r,rok_powstania,gatunki,
                               kraje,muzyka,producent,aktorzy_zaw,rezyser,
                               oceny, uzytkownicy, recenzje,key,oscar,nagrody),na.rm=TRUE)/21
  }
    
  return(wartosci)
  
}

###filmy <- read.csv2("filmyClean.csv",stringsAsFactors=FALSE) - trzeba ustawic zeby
#napisy nie byly faktorami bo sie psuje


#s <- wylicz5(numer=1, aktorzy, rezyserzy,filmy,ile_wszystkich=100)
