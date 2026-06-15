# --- 1. DOWNLOAD ÜBER 'SF' ---

 install.packages("sf")

library(tidyverse)
library(sf)

# Der offizielle ArcGIS-Pfad zum Spielplatz-Layer (ID 0)
api_url <- "https://geoportal.stadt-koeln.de/arcgis/rest/services/freizeit_natur_sport/spielangebote/MapServer/0/query?where=1=1&outFields=*&f=geojson"

# sf liest das GeoJSON direkt als fertige Tabelle ein!
koeln_spielplaetze_sf <- st_read(api_url)

# Wir machen daraus ein ganz normales, sauberes Data Frame ohne Geo-Ballast
koeln_spielplaetze_final <- as.data.frame(koeln_spielplaetze_sf)

# Let's create an 'equipment_score'
# We count how many play/sport features are available per playground
koeln_spielplaetze_clean <- koeln_spielplaetze_final %>%
  mutate(
    # Convert 'ja' to 1 and NA to 0
    tischtennis = ifelse(is.na(tischtennisplatten), 0, 1),
    fussball = ifelse(ball_sportangebot_vorhanden == "ja", 1, 0),
    basketball = ifelse(!is.na(basketballkoerbe), 1, 0),
    # Sum up the total "feature score"
    equipment_score = tischtennis + fussball + basketball
  )

# Are playgrounds in 'quiet streets' better equipped?
# We use a linear model to test if the location type influences the score
fit_potential <- lm(equipment_score ~ an_verkehrsarmer_strasse, data = koeln_spielplaetze_clean)

# Look at the result
library(broom)
tidy(fit_potential)

ggplot(koeln_spielplaetze_clean, aes(x = an_verkehrsarmer_strasse, y = equipment_score)) +
  geom_boxplot()

# Treat NAs as a separate category instead of removing them
koeln_spielplaetze_clean <- koeln_spielplaetze_clean %>%
  mutate(an_verkehrsarmer_strasse = replace_na(an_verkehrsarmer_strasse, "unknown"))

# Re-run the boxplot
ggplot(koeln_spielplaetze_clean, aes(x = an_verkehrsarmer_strasse, y = equipment_score)) +
  geom_boxplot()

# Update: Calculate score safely by treating NA as 0 immediately
koeln_spielplaetze_clean <- koeln_spielplaetze_clean %>%
  mutate(
    tischtennis = replace_na(tischtennisplatten, 0),
    # Achtung: hier mussten wir kurz prüfen, ob das ein Vektor mit Werten 1/NA ist
    # Wenn tischtennisplatten Zahlenwerte hat (wie 1), dann:
    tischtennis = ifelse(tischtennis > 0, 1, 0),
    
    fussball = ifelse(ball_sportangebot_vorhanden == "ja", 1, 0),
    fussball = replace_na(fussball, 0),
    
    basketball = ifelse(!is.na(basketballkoerbe), 1, 0),
    basketball = replace_na(basketball, 0),
    
    equipment_score = tischtennis + fussball + basketball
  )

# Now run the plot again
ggplot(koeln_spielplaetze_clean, aes(x = an_verkehrsarmer_strasse, y = equipment_score)) +
  geom_boxplot()


# Cleaning up the data pipeline
koeln_spielplaetze_clean <- koeln_spielplaetze_clean %>%
  mutate(
    # Force conversion to numeric first, then handle NAs
    tischtennis = as.numeric(tischtennisplatten),
    tischtennis = replace_na(tischtennis, 0),
    tischtennis = ifelse(tischtennis > 0, 1, 0),
    
    # Ensure basketball is numeric and handle NAs
    basketball = as.numeric(basketballkoerbe),
    basketball = replace_na(basketball, 0),
    basketball = ifelse(basketball > 0, 1, 0),
    
    # Fussball logic remains
    fussball = ifelse(ball_sportangebot_vorhanden == "ja", 1, 0),
    fussball = replace_na(fussball, 0),
    
    # Finally calculate the score
    equipment_score = tischtennis + fussball + basketball
  )

# Check if the plot works now without warnings
ggplot(koeln_spielplaetze_clean, aes(x = an_verkehrsarmer_strasse, y = equipment_score)) +
  geom_boxplot()


koeln_spielplaetze_clean <- koeln_spielplaetze_clean %>%
  mutate(
    # Step 1: Force to text, then try to extract digits, then convert to number
    tischtennis = as.numeric(as.character(tischtennisplatten)),
    tischtennis = ifelse(is.na(tischtennis), 0, 1),
    
    # Step 2: Ensure football is handled
    fussball = ifelse(ball_sportangebot_vorhanden == "ja", 1, 0),
    fussball = replace_na(fussball, 0),
    
    # Step 3: Ensure basketball is handled
    basketball = ifelse(!is.na(basketballkoerbe), 1, 0),
    basketball = replace_na(basketball, 0),
    
    # Final Score
    equipment_score = tischtennis + fussball + basketball
  )

# Now check the structure to see if it worked
str(koeln_spielplaetze_clean$equipment_score)


# Final clean visualization without missing data warnings
ggplot(koeln_spielplaetze_clean, aes(x = an_verkehrsarmer_strasse, y = equipment_score)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  labs(title = "Playground Equipment Quality by Location",
       x = "Quiet Street",
       y = "Equipment Score (0-3)") +
  theme_minimal()


#--- 2. ADVANCED SCORING ---

# 1. Schau dir die ersten 20 Einträge der Spalte an
head(koeln_spielplaetze_clean$besonderheiten, 20)

# 2. Zähle, wie oft "Sand" vorkommt
#sum(grepl("Sand", koeln_spielplaetze_clean$besonderheiten, ignore.case = TRUE), na.rm = TRUE)

# --- BEREINIGTE ANALYSE-LOGIK ---

koeln_spielplaetze_clean <- koeln_spielplaetze_clean %>%
  mutate(
    # Score 1: Kleinkind-Fokus (Alles, was explizit als Spielplatz gelabelt ist)
    suitability_toddler = ifelse(typ == "Spielplatz", 1, 0),
    
    # Score 2: Teenager-Fokus (Alles mit Bolzplatz oder Sportanlage)
    suitability_teen = ifelse(typ %in% c("Bolzplatz", "Sportanlage", "Spiel- und Sportangebot"), 1, 0)
  )

# Kurze Kontrolle, ob die Verteilung Sinn ergibt
#table(koeln_spielplaetze_clean$suitability_toddler, koeln_spielplaetze_clean$typ)

# 3. Zähle, wie oft "Rutsche" vorkommt
#sum(grepl("Rutsche", koeln_spielplaetze_clean$besonderheiten, ignore.case = TRUE), na.rm = TRUE)

# 4. Schau dir an, was in den restlichen 90% steht, die vielleicht NICHT "Sand" enthalten
# Das zeigt dir, welche Wörter stattdessen dort vorkommen
#table(koeln_spielplaetze_clean$besonderheiten)

# Suche nach 'Sand' in den Besonderheiten
#koeln_spielplaetze_clean <- koeln_spielplaetze_clean %>%
  #mutate(
    #sandkasten_vorhanden = ifelse(grepl("Sand", besonderheiten, ignore.case = TRUE), 1, 0),
    # Jetzt kombinieren wir das für den Kleinkind-Score
   # suitability_toddler = ifelse(sandkasten_vorhanden == 1, 1, 0)
 # )

#koeln_spielplaetze_clean <- koeln_spielplaetze_clean %>%
  #mutate(
    # Kleinkind-Eignung durch Text-Suche in 'besonderheiten'
    #suitability_toddler = ifelse(grepl("Sand|Rutsche", besonderheiten, ignore.case = TRUE), 1, 0),
    
    # Teenager-Eignung durch deine bereits existierenden Spalten
    #suitability_teen = ifelse(fussball == 1 | basketball == 1, 1, 0)
    
    #table(koeln_spielplaetze_clean$typ)
  #)