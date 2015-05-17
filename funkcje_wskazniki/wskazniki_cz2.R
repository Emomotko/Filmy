library(stringi)

#f_glowny = filmy[2]
#f_por = filmy[10]
#f_glowny = unlist(stri_split_regex(f_glowny,';'))
#f_por = unlist(stri_split_regex(f_por,';'))

# funkcja czy lata powstana filmu sa bliskie
rok = function(rok_glowny,rok_por){
  
  if (rok_glowny=="NA"||rok_por=="NA"){
    return(NA)
  }
  rok_glowny = as.numeric(rok_glowny)
  rok_por = as.numeric(rok_por)
  if (rok_glowny < 1920){
    if(rok_por <1920){
      return(1)
    }else {return(0)}
  }else if(rok_glowny>1920 && rok_glowny<2000){
    if (rok_por<2000 && rok_por>1920){
      roznica = abs(rok_glowny-rok_por)
      if (roznica <= 10){
        return(1)
      }else{
        return(0)
      }
    }else{
      return(0)
    }
  }else if(rok_glowny > 2000){
    if(rok_por > 2000){
      roznica = abs(rok_glowny-rok_por)
      if (roznica <= 3){
        return(1)
      }else{
        return(0)
      }
    }else return(0)
  }
  
}

rok_glowny = f_glowny[4]
rok_por = f_por[4]
rok(rok_glowny,rok_por)

# Funkcja sprawdza czy jakies elementy z jednego wektora wystepuja w drugim
czy_zawiera = function(glowny,por){
  
  if (glowny=="NA"||por=="NA"){
    return(NA)
  }
  
  
  glowny = unlist(stri_split_regex(glowny,'@'))
  por = unlist(stri_split_regex(por,'@'))
  czy = vector()
  for ( i in seq_along(glowny)){
    pom = stri_detect_fixed(glowny[i],por)
    wynik = c(czy,pom)
  }
  
  if (any(wynik==TRUE)){
    return(1)
  }else return(0)
  
}

# porownanie gatunkow:
gat_glowny = f_glowny[7]
gat_por = f_por[7]
czy_zawiera(gat_glowny,gat_por)

#porownanie kraju powstania filmu:
kraj_glowny = f_glowny[10]
kraj_por = f_por[10]
czy_zwiera(kraj_glowny,kraj_por)

# porownanie czy ci sami tworcy muzyki:
muz_glowny = f_glowny[20]
muz_por = f_por[20]
czy_zawiera(muz_glowny,muz_por)

# porownanie czy ci sami producenci:
prod_glowny = f_glowny[21]
prod_por = f_por[21]
czy_zawiera(prod_glowny,prod_por)

# porownanie czy sa jacys tacy sami aktorzy 
aktorzy_glowny = f_glowny[22]
aktorzy_por = f_por[22]
czy_zawiera(aktorzy_glowny,aktorzy_por)

#porownanie czy sa jacys tacy sami rezyserzy
rez_glowny = f_glowny[23]
rez_por = f_por[23]
czy_zawiera(rez_glowny,rez_por)



# funkcja do ocen czy dany film ma podobna ocene, liczbe uzytownikow oceniajacych
# oraz liczbe recenzji.
# Parametr param okresla dopuszczalna roznice miedzy danymi wartosciami,
# dla oceny bedzie 0.3, dla liczby uzytkownikow 50000, dla liczby recenzji 20
roznica = function(glowny,por,param){
  
  if(glowny=="NA"||por=="NA") return("NA")
  
  glowny = as.numeric(glowny)
  por = as.numeric(por)
  
  if (abs(glowny-por) <= param){
    return(1)
  }else return(0)
  
}


ocena_glowny = f_glowny[11]
ocena_por = f_por[11]
uzyt_glowny = f_glowny[12] #liczba uzytkownikow ktora ocenila dany film
uzyt_por = f_por[12]
lr_glowny = f_glowny[13] # liczba recenzji
lr_por = f_por[13]

roznica(ocena_glowny,ocena_por,0.3)
roznica(uzyt_glowny,uzyt_por,50000)
roznica(lr_glowny,lr_por,20)



frakcja_powtarzanych_slow <- function(text_gl, text_por){
  
  
  if(text_gl=="na"||text_por=="na"||text_gl=="NA"||text_por=="NA"){
    
    return(NA)
  }
  
  library(tm)
  
  corpus_gl <-Corpus(VectorSource(text_gl))
  corpus_por <-Corpus(VectorSource(text_por))
  
  corpus_gl<-tm_map(corpus_gl,stemDocument)
  corpus_por<-tm_map(corpus_por,stemDocument)
  
  text_gl <- unlist(sapply(corpus_gl, `[`, "content"))
  
  text_por <- unlist(sapply(corpus_por, `[`, "content"))
  
  o1 <- unlist(stri_extract_all_words(text_gl))
  
  o2 <- unlist(stri_extract_all_words(text_por))
  
  
  o1 <- stri_unique(o1)
  o2 <- stri_unique(o2)
  
  2*(sum(stri_duplicated(c(o1,o2)))/length(c(o1,o2)))
  
}
# porownanie podobienstw keywordsow
key_glowny = f_glowny[15]
key_por = f_por[15]
key_g = stri_split_fixed(key_glowny,'@')
key_p = stri_split_fixed(key_por,'@')
frakcja_powtarzanych_slow(key_g,key_p)



czy_to_samo = function(text_gl,text_por){
  
  if(text_gl=="NA"||text_por=="NA"){
    return(NA)
  }
  if (text_gl>0 && text_por>0){
    return(1)
  }else{
    return(0)
  }
  
}# sprawdzam tylko czy sa jakiekolwiek oscary czy nie - chyba bez sensu
# bawic sie w to ile tych oscarow jest bo i tak rzadko ktory film ma jakiekolwiek oscary

#oscary:
oscar_glowny = f_glowny[17]
oscar_por = f_por[17]
czy_to_samo(oscar_glowny,oscar_por)
#inne narody:
nagr_glowny = f_glowny[18]
nagr_por = f_por[18]
czy_to_samo(nagr_glowny,nagr_por)






