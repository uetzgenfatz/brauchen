## ---------------------------------------------
## Analysis of the Zwriner 3SG sample 
## Author: Oliver Schallert
## Date Created: 2022-11-25
## ---------------------------------------------

library(tidyr)
library(dplyr)
library(statsr)

# You can also use the here-package for relative paths

daten <- read.csv("/Users/uetzelsche/Desktop/brauchen3sg.csv", sep=";", row.names=,1, na.strings="")

daten_mod <-
  daten %>%
  select(Auslaut, Verwendung, X, Y, Ort) %>%
  filter(Verwendung == 'MV' | Verwendung == 'VV') %>%
  filter(!is.na(Auslaut)) 

# Morphology

morphologie <-
  daten_mod %>%
  count(Y, X, Ort, Auslaut) %>%
  spread(Auslaut, n, fill = 0) %>%
  rename(nix = 'NULL', t = 'T') %>%
  rowwise() %>%
  mutate(Gesamt = sum(c(nix, t))) 

X <- gsub(",", "\\.", morphologie$X)
Y <- gsub(",", "\\.", morphologie$Y)

coords_mod_a <- cbind(morphologie, X, Y) 
morphologie_mod <- subset(coords_mod_a, select = -c(X, Y)) 

col_abfolge <- c("Ort", "X", "Y", "nix", "t", "Gesamt")
morphologie_final <- morphologie_mod[, col_abfolge] 

# Verb class

verbklasse <-
  daten_mod %>%
  select(Y, X, Ort, Auslaut, Verwendung) %>%
  unite(m, Auslaut, Verwendung) %>%
  count(Y, X, Ort, m) %>%
  spread(m, n, fill = 0) %>%
  rowwise() %>%
  mutate(Gesamt = sum(c(NULL_MV, NULL_VV, T_MV, T_VV))) 

X <- gsub(",", "\\.", verbklasse$X)
Y <- gsub(",", "\\.", verbklasse$Y)

coords_mod <- cbind(verbklasse, X, Y) 
verbklasse_mod <- subset(coords_mod, select = -c(X, Y)) 

col_abfolge <- c("Ort", "X", "Y", "NULL_MV", "NULL_VV", "T_MV", "T_VV", "Gesamt")
verbklasse_final <- verbklasse_mod[, col_abfolge] 

# Frequency table

frequenzen <-
  summarise(group_by(daten_mod, Verwendung, Auslaut), count = n()) %>%
  mutate(freq = count / sum(count) * 100)

hypothesentest <-
  inference(x = Auslaut, y = Verwendung, data = daten_mod, statistic = "proportion", 
            type = "ht", method = "theoretical", success = 'MV', alternative = "greater", null = 0.05)

# Write files

write.csv2(morphologie_final, file = "/Users/uetzelsche/Desktop/morphologie.csv", row.names = TRUE)

write.csv2(verbklasse_final, file = "/Users/uetzelsche/Desktop/verbklasse.csv", row.names = TRUE)
