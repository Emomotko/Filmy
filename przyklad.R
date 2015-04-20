### Przyklad wywolania ###

linki = readLines('linki_filmy.txt')
l=length(linki)
# ustawcie recznie l aby komputer przezyl !!!!!!!!
l=3

for (i in 1:l){
  link = stri_sub(linki[i],2,(length(linki[i])-3))
  suppressWarnings(wszystko(link))
}


