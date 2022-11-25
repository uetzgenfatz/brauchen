library(tidyr)
library(dplyr)
library(janitor) # Verschiedene Funktionen, um Datensets zu bereinigen

daten <- read.csv("/Users/uetzelsche/Desktop/braucheninf.csv", sep=";", row.names=,1, na.strings="")

# Prüfen, ob es Duplikate gibt

duplikate <- daten %>% get_dupes(Beleg)

# Duplikate aussortieren

daten_unique <- daten[!duplicated(daten$Beleg), ]

# "," durch "." ersetzen

X <- gsub(",", "\\.", daten_unique$X)
Y <- gsub(",", "\\.", daten_unique$Y)

werte_mod_a <- cbind(daten_unique, X, Y) 
werte_mod_b <- subset(werte_mod_a, select = -c(X,Y)) 

# Spalten neu anordnen

col_abfolge <- c("Aufnahmesigle", "Ort", "X", "Y", "Beleg", "Infinitivrektion", "Kommentar")

daten_final <- werte_mod_b[, col_abfolge] 


# Auswertung Infinitivrektion

rektion <-
  daten_final %>%
  select(X, Y, Ort, Infinitivrektion) %>%
  count(X, Y, Ort, Infinitivrektion) %>%
  spread(Infinitivrektion, n, fill = 0) %>%
  rename(nix = 'NULL', zu = 'ZU') %>%
  rowwise() %>%
  mutate(Gesamt = sum(c(nix, zu))) 

# Man kann das auch eleganter machen, aber diese Lösung hat den Vorteil, dass sie nur mit Basisfunktionen von R auskommt.

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

# Eigentlich überflüssig, weil der Unterschied so groß ist, aber für alle Fälle machen wir einen X2-Test

kontabelle <- rektion_frequenzen[1:2,2:2]
chisq.test(kontabelle)

# Dateien schreiben

write.csv2(daten_final, file = "/Users/uetzelsche/Desktop/infinitivrektion_rohdaten.csv", row.names = TRUE)

write.csv2(rektion_final, file = "/Users/uetzelsche/Desktop/infinitivrektion_auswertung.csv", row.names = TRUE)
