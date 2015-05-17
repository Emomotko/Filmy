czas <- function(czas_glowny, czas_por){
  
  if(is.na(czas_glowny)||is.na(czas_por)){
    
    return(NA)
    
  }
  
  if(czas_glowny<60){
    
    if(czas_por<60){
      return(1)
    } else return(0)
  } else if(czas_glowny<90){
    
    if(czas_por>=60&&czas_por<90){
      return(1)
    } else return(0)
    
  } else if(czas_glowny<140){
    
    if(czas_por>=90&&czas_por<140){
      return(1)
    } else return(0)
  } else if(czas_glowny<180){
    if(czas_por>=140&&czas_por<180){
      return(1)
    } else return(0)
    
  } else if(czas_glowny<240){
    
    if(czas_por>=180&&czas_por<240){
      return(1)
    } else return(0)
    
  } else if(czas_glowny>=240&&czas_por>=140) return(1)
  
  return(0)
  
}
frakcja_powtarzanych_slow <- function(text_gl_upr, text_por_upr){
  
  
  if(text_gl_upr=="na"||text_por_upr=="na"){
    
    return(NA)
  } 
  
  pomocniczy <- c(text_gl_upr, text_por_upr)
  2*(sum(stri_duplicated(pomocniczy)))/length(pomocniczy)
  
}

aktorzy_wyszukaj_wiek_kraj4 <- function(f, aktorzy){
  
  aktorzy_f <- unlist(stri_split_fixed(f[1,22],"@"))[1:10]
  
  
  l1 <- aktorzy[aktorzy$name%in%aktorzy_f,]
  l1 <- l1[!duplicated(l1$name),]
  
  
  if(f[4]!="NA"){
    wiek <- 2015-as.numeric(l1[[3]]) - (2015-as.numeric(f[1,4]))
  } else wiek <- NA
  
  kraj <- as.character(l1[[4]])
  
  list(wiek,kraj)
}


rezyser_wyszukaj_wiek_kraj4 <- function(f,rezyserzy){
  
  rezyserzy_f <- unlist(stri_split_fixed(f[1,23],"@"))
  
  l1 <- rezyserzy[rezyserzy$name%in%rezyserzy_f,]
  l1 <- l1[!duplicated(l1$name),]
  
  
  if(f[4]!="NA"){
    wiek <- 2015-as.numeric(l1[[3]]) - (2015-as.numeric(f[1,4]))
  } else wiek <- NA
  
  kraj <- as.character(l1[[4]])
  
  list(wiek,kraj)
  
}


rozrzut4 <- function(z){
  
  r1 <- IQR(z[[1]][[1]],na.rm=TRUE)
  r2 <- IQR(z[[2]][[1]],na.rm=TRUE)
  
  (r1+r2-abs(r1-r2))/(r1+r2)
  
}
mediana4 <- function(z){
  
  m1 <- median(as.numeric(z[[1]][[1]]),na.rm = TRUE)
  m2 <- median(as.numeric(z[[2]][[1]]),na.rm = TRUE)
  
  if(is.na(m1)||is.na(m2)||length(m1)==0||length(m2)==0) return(NA)
  
  if(m1<6){
    
    if(m2<6){
      
      if(abs(m1-m2)<=1) return(1)
    }
    
  } else if(m1<18){
    
    if(m2>=6&&m2<18){
      
      if(abs(m1-m2)<=3) return(1)
    }
  } else if(m1<50){
    
    if(m2>=18&&m2<50){
      
      if(abs(m1-m2)<=4) return(1)
    }
  } else if(m1>=50){
    
    if(m2>=50){
      
      if(abs(m1-m2)<=7) return(1)
    }
  }
  
  return(0)
  
}
kraje <- function(z){
  
  a1<-unlist(z[[1]][[2]])
  a2<-unlist(z[[2]][[2]])
  t1 <- table(a1)
  t1 <- t1[names(t1)!="NA"]
  t2 <- table(a2)
  t2 <- t2[names(t2)!="NA"]
  
  p <- intersect(names(t1),names(t2))
  
  ile <- sapply(p, function(x){
    
    n1 <- which(names(t1)==x)
    n2 <- which(names(t2)==x)
    
    2*min(t1[n1],t2[n2])
    
  })
  
  sum(unlist(ile))/sum(c(t1,t2))
  
}


uprosc <- function(tekst){
  
  corpus <-Corpus(VectorSource(tekst))
  
  corpus<-tm_map(corpus,stemDocument)
  
  tekst <- unlist(sapply(corpus, `[`, "content"))
  
  slowa <- unlist(stri_extract_all_words(tekst))
  
  stri_unique(slowa)
  
}
