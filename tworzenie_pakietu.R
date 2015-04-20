library(devtools)
library(roxygen2)
library(testthat)

#scieżka ustawiona na folder Filmy

#setwd()
session_info()

setwd("FilmyJMS")

create("FilmyJMS")

build()

install()

document()

test()

library(formatR)
tidy_dir("R")