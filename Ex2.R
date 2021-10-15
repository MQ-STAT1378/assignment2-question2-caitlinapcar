install.packages("testthat")
library(devtools)
has_devel()
bank = read.csv("bank.csv", header = TRUE)
library(usethis)
usethis::create_package("Ex2.Multiserver")

