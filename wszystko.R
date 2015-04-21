#' Collect all info about film,
#'
#' Function \code{wszystko} gets all info about film.
#'
#' @param link A link from imdb.
#' @return List
#' @import rvest
#' @import stringi
#' @import XML
#' 


wszystko = function(link){
  
  
  film= tryCatch({
    html(link)
  }, error = function(e) {
    'NA'})
  if (is.character(film)&&film=="NA") return("NA")
  
  tytul = wyjmij(film,'.header .itemprop')
  rok = wyjmij(film,'.header .nobr')
  rok = stri_sub(rok,2,5) # pozbywam sie zbednych nawiasow
  czas = wyjmij(film,'time')
  czas = czas[2] # na stronie zawsze jest dwa razy podany czas filmu, dlatego biore tylko
  # jeden bo to jest to samo
  if (is.na(czas)){
    czas='NA'  
  }
 
  gatunek = wyjmij(film,'.infobar .itemprop')
  gatunek = stri_paste(gatunek,collapse='@')
  
  
  data_wydania = wyjmij(film, '.infobar .nobr a')
 
  fabula = wyjmij(film,'#titleStoryLine p') # fabula czasami to jest to samo co opis
  fabula = stri_replace_all_regex(fabula,';',',')
  # ale nie zawsze wiec rozroniam
  
  #Motion Picture Rating:
  mpaa = wyjmij(film, '#titleStoryLine .txt-block:nth-child(12) h4+ span')
  
  kraj = wyjmij(film,'.txt-block:nth-child(4) a')
  
  dane_oceny = wyjmij(film,'.star-box-details , .star-box-details span')
  
  nagrody = wyjmij(film,'#titleAwardsRanks')
  if (nagrody!='NA'){
      liczba_nominacji = unlist(stri_extract_all_regex(nagrody,'...nominations'))
      liczba_nominacji = unlist(stri_extract_all_regex(nagrody,'[0-9]+'))
      if (is.na(liczba_nominacji)) liczba_nominacji='NA'
      
      liczba_oscarow = unlist(stri_extract_all_regex(nagrody,'...Oscars'))
      liczba_oscarow = unlist(stri_extract_all_regex(liczba_oscarow,'[0-9]+'))
      if (is.na(liczba_oscarow)) liczba_oscarow='NA'
      
      liczba_innych_nagrod = unlist(stri_extract_all_regex(nagrody,'...wins'))
      liczba_innych_nagrod = unlist(stri_extract_all_regex(liczba_innych_nagrod,'[0-9]+'))
      if (is.na(liczba_innych_nagrod)) liczba_innych_nagrod='NA'
      
  }
 
  
  ocena = dane_oceny[2]
  if (is.na(ocena)) ocena='NA'
  max_ocena = dane_oceny[4]
  if (is.na(max_ocena)) max_ocena='NA'
  liczba_glosujacych = dane_oceny[5]
  if (is.na(liczba_glosujacych)) liczba_glosujacych='NA'
  liczba_recenzji = dane_oceny[6]
  if (is.na(liczba_recenzji)) liczba_recenzji='NA'
  
  link_and_title = html_nodes(film, "a")
  links = html_attr(link_and_title, name="href")
  titles = html_text(link_and_title)
  
  wek_pom = stri_detect_regex(titles,'See full cast')
  ktory = which(wek_pom==TRUE)
  w=links[ktory]
  obsada_link = stri_paste(link,w[2])
  if (length(obsada_link)>0 && !is.na(obsada_link)){
    prod_music = get_names(obsada_link)
    
    muzyka = prod_music$music
    producenci = prod_music$producers
    producenci = stri_paste(producenci,collapse='@')
    rezyser = director(obsada_link)
    aktorzy = actors(obsada_link) 
    aktorzy = stri_paste(aktorzy,collapse='@')
  }
  

  
  #masakryczne wydobycie budzetu:
  kt = stri_locate_all_regex(html_text(film),'Budget')
  kt = unlist(kt)
  budzet = stri_sub(html_text(film),kt[1]+7,kt[2]+28)
  budzet = stri_replace_all_regex(budzet,'\\p{WHITE_SPACE}',' ')
  if (is.na(budzet)) budzet='NA'
  
  opis = summary(links,titles,film)
  if (length(opis)>1){
    opis = opis[2]
  }
  
  klucze = wydobadz(links,titles,film,'Keywords','.sodatext') 
  klucze = stri_trim(klucze,'both')
  klucze = stri_paste(klucze,collapse='@')
  recenzje = wydobadz(links,titles,film,'Reviews','div+ p')
  recenzje = stri_replace_all_regex(recenzje,';',',')
  id_film =stri_sub(link,27,35)
  
  lista2 = list(id=id_film,link=link,Title=tytul,Year=rok,Time=czas,Relase=data_wydania,
       Genre=gatunek,Storyline=fabula,MPAA=mpaa,Country=kraj,Ratings=ocena,
       Ratings_max=max_ocena,Users=liczba_glosujacych,Reviews_number=liczba_recenzji,
       Budget=budzet,Keywords=klucze,Description=opis,Oscar=liczba_oscarow,
       Another_awards=liczba_innych_nagrod,Nominations=liczba_nominacji,Music=muzyka,
       Produced=producenci,Actors=aktorzy )
  
  lista3 = list(id=id_film,text=recenzje)
  create2('filmy.csv',lista2)
  create3('recenzje.csv',lista3)

}
