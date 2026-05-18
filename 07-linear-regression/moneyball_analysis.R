# Moneyball Analyse - Schritt 1
# Zusammenhang zwischen Home Runs (HR) und Runs (R)

library(Lahman)
library(tidyverse)

# Daten vorbereiten: Zeitraum 1961 bis 2001 
# Ich berechne Werte pro Spiel (G), um Teams vergleichbar zu machen
baseball_data <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(HR_per_game = HR / G, 
         R_per_game = R / G)

# Visualisierung
ggplot(baseball_data, aes(x = HR_per_game, y = R_per_game)) +
  geom_point(alpha = 0.5) +
  labs(title = "Home Runs vs. Runs pro Spiel",
       subtitle = "Daten von 1961 bis 2001",
       x = "Home Runs pro Spiel",
       y = "Erzielte Runs pro Spiel")
#--------------------------------------------------------------------

# Vergleich: Wie sieht es mit den Walks (BB) aus?
baseball_data %>% 
  ggplot(aes(x = BB / G, y = R_per_game)) +
  geom_point(alpha = 0.5) +
  labs(title = "Bases on Balls vs. Runs pro Spiel",
       x = "Walks (BB) pro Spiel",
       y = "Erzielte Runs pro Spiel")
#----------------------------------------------------------------------
  # Beobachtung: HR sieht linearer aus als BB. 
  # Grund: HR garantiert Punkte, BB ist nur eine Chance auf Punkte.
  # Aber: BB ist trotzdem ein starker Prädiktor!
#--------------------------------------------------------------------
  # Vergleich: Wie sieht es mit den Stolen Bases (SB) aus?
  
  baseball_data %>% 
    ggplot(aes(x = SB / G, y = R_per_game)) +
    geom_point(alpha = 0.5) +
    labs(title = "Stolen Bases vs. Runs pro Spiel",
       x = "Stolen Basis (SB) pro Spiel",
       y = "Erzielte Runs pro Spiel")

# Fazit der Analyse:
# Während HR und BB eine klare positive Richtung (Korrelation) zeigen, 
# ist die Punktwolke bei den Stolen Bases (SB) viel diffuser.
# Das bestätigt die Theorie: SB sind ein schlechterer Prädiktor für Runs als BB.(historisch überbewertet)
#----------------------------------------------------
#Quiz: ----
#Question: What does the Variable "SOA" stand for in the Teams Table? ------
# strikeouts by pitchers
?Teams

#Make a scatterplot of runs per game versus at bats (AB) per game.-------
baseball_data %>% 
  ggplot(aes(x = AB / G, y = R_per_game)) +
  geom_point(alpha = 0.5) +
  labs(title = "At Bats vs. Runs pro Spiel",
       x = "At Bats (AB) pro Spiel",
       y = "Erzielte Runs pro Spiel")

# Ergebnis & Interpretation (At Bats vs. Runs):
# Es gibt eine positive Korrelation, aber sie ist weniger präzise als bei HR.
# Mehr At Bats bedeuten mehr Chancen auf Runs, aber die Effizienz 
# (wie viele ABs tatsächlich zu Runs führen) variiert stark zwischen den Teams.
# Allein durch mehr At Bats gewinnt man keine Spiele – man muss sie nutzen.

#Use the filtered Teams data frame from Question 6. Make a scatterplot of win rate (number of wins per game) versus number of fielding errors (E) per game.----
baseball_data %>% 
  ggplot(aes(x = E / G, y = W / G)) + 
  geom_point(alpha = 0.5) +
  labs(title = "Einfluss von Fehlern auf die Gewinnrate",
       x = "Errors (Fehler) pro Spiel",
       y = "Gewinnrate (W/G)")
# Ergebnis & Interpretation (Errors vs. Wins):
# Es zeigt sich eine schwache negative Korrelation. 
# Zwar sind Fehler (Errors) schlecht für das Spiel, aber sie sind 
# kein dominanter Prädiktor für den Gesamterfolg einer Saison.
# Das erklärt, warum Bill James (Sabermetrics) argumentierte, dass 
# klassische Fielding-Statistiken oft überbewertet werden.

#Use the filtered Teams data frame from Question 6. Make a scatterplot of triples (X3B) per game versus doubles (X2B) per game.-----
baseball_data %>% 
  ggplot(aes(x = X2B / G, y = X3B / G)) + 
  geom_point(alpha = 0.5) +
  labs(title = "Beziehung zwischen doubles und Tripples",
       x = "Doubles pro Spiel",
       y = "Tripples pro Spiel")
# Ergebnis & Interpretation:
# Die Korrelation ist hier sehr schwach/nicht vorhanden. 
# Das bedeutet: Ein Team, das viele Doubles schlägt, schlägt 
# nicht automatisch auch viele Triples. Unterschiedliche Skills!

# (Q7) - What is the correlation coefficient between number of runs per game and number of at bats per game?
cor_AB_R <- baseball_data %>% mutate(AB_per_game = AB / G) %>% summarise(cor(AB_per_game, R_per_game))

# (Q8) - What is the correlation coefficient between win rate (number of wins per game) and number of errors per game?
cor_WR_E <- baseball_data %>% mutate(WinRate = W/G, Errors_per_game = E/G) %>% summarise (cor(WinRate, Errors_per_game))

# (Q9)S - What is the correlation coefficient between doubles (X2B) per game and triples (X3B) per game?
cor_X2B_X3B <- baseball_data %>% mutate(doubles =X2B/G, tripples = X3B/G) %>% summarise (cor(doubles, tripples))
