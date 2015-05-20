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

czy_zawiera = function(glowny,por){
  
  if (glowny=="NA"||por=="NA"||is.na(glowny)||is.na(por)){
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

roznica = function(glowny,por,param){ 
  
  glowny = as.numeric(glowny)
  por = as.numeric(por)
  if(glowny=="NA"||por=="NA"||is.na(glowny)||is.na(por)) return(NA)
  
  if (abs(glowny-por) <= param){
    return(1)
  }else return(0)
  
}

czy_to_samo = function(text_gl,text_por){
  
  if(text_gl=="NA"||text_por=="NA"||is.na(text_gl)||is.na(text_por)){
    return(NA)
  }
  if (text_gl>0 && text_por>0){
    return(1)
  }else{
    return(0)
  }
  
}