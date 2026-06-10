# ==============================================================================
# GENERATIVE DESIGN: PARAMETRISCHE FASSADEN-OPTIMIERUNG (MONTE CARLO)
# ==============================================================================
library(tidyverse)

# --- 1. SETUP DER PARAMETER ---
B <- 1000  # Anzahl der simulierten Tage (Szenarien)
N <- 25    # Anzahl der steuerbaren Fassadenlamellen an der Gebäudefront

set.seed(42) # Macht die Simulation reproduzierbar (wichtig für Rhino!)

# --- 2. DIE MONTE-CARLO-SIMULATION ---
# Wir simulieren 1000 verschiedene Wetter- und Winkel-Kombinationen
simulierte_effizienz <- replicate(B, {
  
  # Der Algorithmus testet zufällige Winkel für unsere 25 Lamellen (0 bis 90 Grad)
  lamellen_winkel <- runif(N, min = 0, max = 90)
  
  # Energieeffizienz (Sonnenschutz vs. Tageslicht)
  # ist mathematisch optimal bei einem mittleren Winkel (~45 Grad).
  # "Zufall" für wechselhaftes Wetter --> (rnorm).
  effizienz <- mean(sin(lamellen_winkel * pi / 180)) * 100 + rnorm(1, mean = 0, sd = 5)
  
  return(effizienz)
})

# --- 3. EVALUATION DER PERFORMENS ---
design_daten <- data.frame(Effizienz = simulierte_effizienz)

# Histogramm der simulierten Entwürfe
ggplot(design_daten, aes(x = Effizienz)) +
  geom_histogram(binwidth = 2, color = "black", fill = "darkslategrey", alpha = 0.8) +
  geom_vline(aes(xintercept = mean(Effizienz)), color = "red", linetype = "dashed", size = 1) +
  labs(title = "Generative Design: Effizienz-Verteilung von Fassaden-Layouts",
       subtitle = "Monte-Carlo-Simulation für 1000 Szenarien",
       x = "Berechnete Energieeffizienz (%)",
       y = "Anzahl der optimalen Tage") +
  theme_minimal()

# --- 4. STATISTISCHE KENNZAHLEN FÜR DIE WEITERGABE AN RHINO ---
# Der Erwartungswert zeigt uns die durchschnittliche Performance des Entwurfs
cat("Durchschnittliche Effizienz:", mean(simulierte_effizienz), "%\n")

# Der Standardfehler (sd) zeigt das Risiko: Schwankt die Performance zu stark?
cat("Standardfehler (Risiko):", sd(simulierte_effizienz), "\n")

# --- 5. EXPORT FÜR GRASSHOPPER ---
write_csv(design_daten, "data/rhino_facade_data.csv")
