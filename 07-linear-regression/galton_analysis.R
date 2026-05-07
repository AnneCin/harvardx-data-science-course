#################################################################
# CASE STUDY: IS HEIGHT HEREDITARY? (GALTON FAMILIES)           #
#################################################################

library(tidyverse)
library(HistData)

# 1. DATENVORBEREITUNG ------------------------------------------

data("GaltonFamilies")

# Wir ziehen pro Familie zufällig einen Sohn, um Unabhängigkeit zu garantieren
set.seed(1983) 
# Fixiert den Zufallsgenerator, damit die Stichprobe 
# (sample_n) immer identisch bleibt. Wichtig für die 
# Reproduzierbarkeit der Ergebnisse!

galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)


# 2. EXPLORATIVE ANALYSE (Lagemasse) ----------------------------

# Mittelwert und Standardabweichung (SD)
# SD ist das Maß für die Streuung um den Mittelwert (Einheit: Zoll)
galton_heights %>%
  summarize(mean(father), sd(father), mean(son), sd(son))


# 3. KORRELATION (r) --------------------------------------------

# r beschreibt Richtung und Stärke des linearen Zusammenhangs (-1 bis 1)
galton_heights %>% 
  summarize(r = cor(father, son))
# WICHTIGE MERKREGEL:
# r > 0 (Positiv): 'Je mehr x, desto mehr y' (Beispiel: Vater-Sohn Größe)
# r < 0 (Negativ): 'Je mehr x, desto weniger y' (Beispiel: Errors vs. Wins)
# r = 0 (Null): Kein linearer Trend erkennbar.

# Visualisierung mit Regressionsgerade (blau) und Identitätslinie (rot)
galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + 
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Regression zur Mitte",
       subtitle = "Blau: Regressionsgerade | Rot: 1-zu-1 Erbe-Linie")


# 4. KORRELATION ALS ZUFALLSVARIABLE (Monte Carlo) --------------
# Die Korrelation einer Stichprobe ist eine Schätzung für die Population [cite: 3, 4]

B <- 1000  # Anzahl der Simulationen [cite: 9]
N <- 25    # Kleine Stichprobengröße [cite: 10]

R <- replicate(B, {
  slice_sample(galton_heights, n = N, replace = TRUE) %>% 
    summarize(r = cor(father, son)) %>% 
    pull(r)
})

# Histogramm der Ergebnisse
# Zeigt, dass r bei kleinen Stichproben stark schwanken kann (hoher Standardfehler) [cite: 10, 11]
data.frame(R) %>% 
  ggplot(aes(R)) + 
  geom_histogram(binwidth = 0.05, color = "black", fill = "steelblue") +
  labs(title = "Verteilung der Stichproben-Korrelation (N=25)",
       x = "Berechnete Korrelation (r)")

# Statistischer Check der Simulation
mean(R)  # Sollte nah an der echten Korrelation liegen (~0.5) [cite: 10]
sd(R)    # Der Standardfehler der Korrelation (relativ hoch bei kleinem N) [cite: 10]

# QQ-Plot zur Prüfung auf Normalverteilung (Zentraler Grenzwertsatz)
# Bei N=25 ist die Verteilung oft noch nicht perfekt normal [cite: 13, 15]
data.frame(R) %>%
  ggplot(aes(sample = R)) +
  stat_qq() +
  geom_abline(intercept = mean(R), slope = sqrt((1-mean(R)^2)/(N-2)))