## ---------------------------------------------
## Analysis of the Zwriner infinitive sample 
## Author: Oliver Schallert
## Date Created: 2022-11-25
## ---------------------------------------------

library(tidyr)
library(dplyr)

# You can also use the here-package for relative paths

daten <- read.csv("/Users/uetzelsche/Desktop/braucheninf.csv", sep=";", row.names=,1, na.strings="")

# Discard duplicates

daten_unique <- daten[!duplicated(daten$Beleg), ]

# replace "," by "." 

X <- gsub(",", "\\.", daten_unique$X)
Y <- gsub(",", "\\.", daten_unique$Y)

werte_mod_a <- cbind(daten_unique, X, Y) 
werte_mod_b <- subset(werte_mod_a, select = -c(X,Y)) 

# Rearrange columns

col_abfolge <- c("Aufnahmesigle", "Ort", "X", "Y", "Beleg", "Infinitivrektion", "Kommentar")

daten_final <- werte_mod_b[, col_abfolge] 


# Analysis of ‘status’ government 

rektion <-
  daten_final %>%
  select(X, Y, Ort, Infinitivrektion) %>%
  count(X, Y, Ort, Infinitivrektion) %>%
  spread(Infinitivrektion, n, fill = 0) %>%
  rename(nix = 'NULL', zu = 'ZU') %>%
  rowwise() %>%
  mutate(Gesamt = sum(c(nix, zu))) 

# This can be done more elegantly, but we only want to use R base functions

rektion_final <- rektion[,c(1:4,6,9)]

ges <- sum(rektion$Gesamt)
nozu <- sum(rektion$zu)
nonull <- sum(rektion$nix)

propzu <- nozu/ges * 100
propnull <- nonull/ges * 100

Form <- c("zu", "null")
n <- c(nozu, nonull)
Frequency <- c(propzu, propnull)

rektion_frequenzen <- data.frame(Form, n, Frequency)

# Not really necessary, but we make a X2-test anyway

kontabelle <- rektion_frequenzen[1:2,2:2]
chisq.test(kontabelle)

# Write files

write.csv2(daten_final, file = "/Users/uetzelsche/Desktop/infinitivrektion_rohdaten.csv", row.names = TRUE)

write.csv2(rektion_final, file = "/Users/uetzelsche/Desktop/infinitivrektion_auswertung.csv", row.names = TRUE)
