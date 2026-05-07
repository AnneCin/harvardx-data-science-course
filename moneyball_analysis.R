# Moneyball Analyse - Schritt 1
# Zusammenhang zwischen Home Runs (HR) und Runs (R)

library(Lahman)
library(tidyverse)

# Daten vorbereiten: Zeitraum 1961 bis 2001 [cite: 1, 4]
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
--------------------------------------------------------------------

# Vergleich: Wie sieht es mit den Walks (BB) aus?
baseball_data %>% 
  ggplot(aes(x = BB / G, y = R_per_game)) +
  geom_point(alpha = 0.5) +
  labs(title = "Bases on Balls vs. Runs pro Spiel",
       x = "Walks (BB) pro Spiel",
       y = "Erzielte Runs pro Spiel")
----------------------------------------------------------------------
  # Beobachtung: HR sieht linearer aus als BB. 
  # Grund: HR garantiert Punkte, BB ist nur eine Chance auf Punkte.
  # Aber: BB ist trotzdem ein starker Prädiktor!
  --------------------------------------------------------------------
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