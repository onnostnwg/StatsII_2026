# 0. Laden der Bibliotheken und des Datensatzes
library(haven)
library(ggplot2)
library(dplyr)
library(effsize)
library(broom)
library(pscl)
library(tidyr)
library(car)
library(lme4)
library(performance)
library(reshape2)
library(nnet)
library(effectsize)
library(sjPlot)
library(tidyverse)
library(ggalluvial)
library(mediation)
library(Hmisc)
library(survey) 
 

# Setze das Arbeitsverzeichnis auf den "replication"-Ordner
setwd("C:/Users/onnos/Desktop/TRINITY/PhD/Quantitative Methods II/Replication Paper/Replication") 


# Laden des Datensatzes (relativer Pfad)
 dataaaa <- read_dta("data/PPD_Waves7-9.dta")

 
# 1. Einschluss/Ausschluss von Versuchspersonen

# Ausschluss der Versuchspersonen mit dispcode 22 zu allen Messzeitpunkten
person_ids_dispcode_22 <- unique(dataaaa$id3[dataaaa$dispcode == 22])
dataaa <- dataaaa[!(dataaaa$id3 %in% person_ids_dispcode_22), ]

# Ausschluss der Versuchspersonen mit mindestens einem fehlenden Wert in der Variable partei
person_ids_missing_partei <- unique(dataaa$id3[is.na(dataaa$partei)])
dataa <- dataaa[!(dataaa$id3 %in% person_ids_missing_partei), ]

# Ausschluss der Versuchspersonen mit mindestens einem Wert 16 in der Variable partei
person_ids_with_16 <- unique(dataa$id3[dataa$partei == 16])
data <- dataa[!(dataa$id3 %in% person_ids_with_16), ]

nrow(dataaaa)
nrow(dataaa)
nrow(dataa)
nrow(data)

# Ersetzen von NA in ipf_gewicht durch 1
data$ipf_gewicht[is.na(data$ipf_gewicht)] <- 1

n_distinct(dataaaa$id3)
n_distinct(dataaa$id3)
n_distinct(dataa$id3)
n_distinct(data$id3)




# 2. Vorbereitung der Variablen

# 2.1 Gewicht übertragen

# Extrahiere Gewicht pro id3 aus Umfragewelle 7
gewicht_w7 <- data %>%
  filter(umfragewelle == 7) %>%
  dplyr::select(id3, ipf_gewicht)

# Füge Gewicht über id3 an alle Zeilen im Datensatz an
data <- data %>%
  dplyr::select(-ipf_gewicht) %>% 
  left_join(gewicht_w7, by = "id3")


# 2.2 LiRe Average bilden und transfomireren
data <- data %>%
  filter(umfragewelle %in% c(7, 8, 9)) %>%     
  group_by(id3) %>%
  mutate(LiRe = (mean(linksrechts, na.rm = TRUE) - 1) / 10) %>% 
  ungroup()


# 2.3 Kontrollvariablen

# Region aus Bundesland ableiten
data <- data %>%
  mutate(region = case_when(
    bundesland %in% c(4, 8, 13, 14, 16) ~ "Osten",
    !is.na(bundesland) ~ "Westen",
    TRUE ~ NA_character_
  ))

# Geschlecht als Faktor mit Labels (männlich/weiblich), gleichzeitig numerisch speichern
data$geschlecht[data$geschlecht == 3] <- NA
data$geschlecht <- factor(data$geschlecht, levels = c(1, 2), labels = c("männlich", "weiblich"))
data$geschlecht_num <- as.numeric(data$geschlecht)  # 1 = männlich, 2 = weiblich

# Region: numerisch kodieren für Analysen (Osten = 0, Westen = 1)
data$region <- factor(data$region, levels = c("Osten", "Westen"))
data$region_num <- as.numeric(data$region) - 1  # Osten = 0, Westen = 1

# Haushaltseinkommen: ungültige Werte entfernen, umpolen und als numerisch lassen
data$haushaltseinkommen[data$haushaltseinkommen == 7] <- NA
data$haushaltseinkommen <- 6 - data$haushaltseinkommen
data$haushaltseinkommen <- as.numeric(data$haushaltseinkommen)

# Bildung: ungültige Werte entfernen, als ordinal behandeln (numerisch, aber kein Faktor)
data$bildung[data$bildung %in% c(1, 8, 9, 10)] <- NA
data$bildung <- as.numeric(data$bildung)

# Alter bleibt numerisch
data$alter <- as.numeric(data$alter)


summary(data[, c("geschlecht", "bildung", "haushaltseinkommen", "region", "alter")])



# 2.4 Variable Parteipräferenz bauen

# Umgestaltung der Variable partei und Neubenennung als Parteipräferenz
data <- data %>%
  mutate(Parteipräferenz = case_when(
    partei == 6 ~ 1,
    partei == 4 ~ 2,
    partei == 3 ~ 3,
    partei == 5 ~ 4,
    partei %in% c(2, 8) ~ 5,
    partei == 7 ~ 6,
    partei %in% c(10, 11, 12, 13, 14, 15) ~ 0,
    TRUE ~ partei
  )) %>%
  mutate(Parteipräferenz = factor(Parteipräferenz,
                                  levels = c(0, 1, 2, 3, 4, 5, 6),
                                  labels = c("Andere", "Linke", "Grüne", "SPD", "FDP", "Union", "AfD"),
                                  ordered = TRUE))

# Histogramm zur Häufigkeitsverteilung der neuen Variable Parteipräferenz - ungewichtet
pl1 <- ggplot(data, aes(x = factor(Parteipräferenz))) +
  geom_bar(fill = "purple", color = "black") +
  labs(title = "Häufigkeitsverteilung der Variable Parteipräferenz", 
       x = "Parteipräferenz", 
       y = "Häufigkeit") +
  scale_x_discrete(labels = c("Andere", "Linke", "Grüne", "SPD", "FDP", "Union", "AfD"))

print(pl1)

# Gewichtete Häufigkeiten berechnen
gewichtete_daten <- data %>%
  group_by(Parteipräferenz) %>%
  summarise(Gewicht = sum(ipf_gewicht)) %>%
  mutate(Parteipräferenz = factor(Parteipräferenz,
                                  levels = c("Andere", "Linke", "Grüne", "SPD", "FDP", "Union", "AfD")))

# Histogramm zur Häufigkeitsverteilung der neuen Variable Parteipräferenz - gewichtet
pl2 <- ggplot(gewichtete_daten, aes(x = Parteipräferenz, y = Gewicht)) +
  geom_bar(stat = "identity", fill = "orange", color = "black") +
  labs(title = "Gewichtete Verteilung der Parteipräferenz",
       x = "Parteipräferenz",
       y = "gewichtete Häufigkeit")


# 2.5 Persönlichkeitsvariablen bauen

# Umpolung der angegebenen Variablen
data$v_1280 <- 6 - data$v_1280
data$v_1282 <- 6 - data$v_1282
data$v_1283 <- 6 - data$v_1283
data$v_1284 <- 6 - data$v_1284
data$v_1286 <- 6 - data$v_1286

# Erstellen der neuen Variablen
data$Extraversion <- (data$v_1280 + data$v_1285)/2 
data$Agreeableness <- (data$v_1286 + data$v_1281)/2
data$Conscientiousness <- (data$v_1282 + data$v_1287)/2
data$Neuroticism <- (data$v_1283 + data$v_1288)/2
data$Openness <- (data$v_1284 + data$v_1289)/2

# Sicherstellen, dass die Variablen als intervallskaliert behandelt werden
data <- data %>%
  mutate(Extraversion = as.numeric(Extraversion),
         Agreeableness = as.numeric(Agreeableness),
         Conscientiousness = as.numeric(Conscientiousness),
         Neuroticism = as.numeric(Neuroticism),
         Openness = as.numeric(Openness))

# Berechnung und Skalierung der Big-Five-Mittelwerte pro Person (0 bis 1)
data <- data %>%
  group_by(id3) %>%
  mutate(
    ExtraversionMittelwert = (mean(Extraversion, na.rm = TRUE) - 1) / 4,
    AgreeablenessMittelwert = (mean(Agreeableness, na.rm = TRUE) - 1) / 4,
    ConscientiousnessMittelwert = (mean(Conscientiousness, na.rm = TRUE) - 1) / 4,
    NeuroticismMittelwert = (mean(Neuroticism, na.rm = TRUE) - 1) / 4,
    OpennessMittelwert = (mean(Openness, na.rm = TRUE) - 1) / 4
  ) %>%
  ungroup()





# 3. Deskripitve Stichprobenanalyse - Metrische und Nicht-Metrische Variablen - Kennwerte vor und nach Gewichtung

# Hilfsfunktion: Erstes nicht-NA-Element aus mehreren Wellen
first_non_na <- function(...) {
  vals <- list(...)
  for (val in vals) {
    if (!all(is.na(val))) return(val)
  }
  return(NA)
}

# Finalwert je Person berechnen und Gewicht aus Welle 7 übernehmen
personen_daten <- data %>%
  group_by(id3) %>%
  dplyr::summarise(
    openness       = first_non_na(OpennessMittelwert[umfragewelle == 7], OpennessMittelwert[umfragewelle == 8], OpennessMittelwert[umfragewelle == 9]),
    conscientiousness = first_non_na(ConscientiousnessMittelwert[umfragewelle == 7], ConscientiousnessMittelwert[umfragewelle == 8], ConscientiousnessMittelwert[umfragewelle == 9]),
    extraversion   = first_non_na(ExtraversionMittelwert[umfragewelle == 7], ExtraversionMittelwert[umfragewelle == 8], ExtraversionMittelwert[umfragewelle == 9]),
    agreeableness  = first_non_na(AgreeablenessMittelwert[umfragewelle == 7], AgreeablenessMittelwert[umfragewelle == 8], AgreeablenessMittelwert[umfragewelle == 9]),
    neuroticism    = first_non_na(NeuroticismMittelwert[umfragewelle == 7], NeuroticismMittelwert[umfragewelle == 8], NeuroticismMittelwert[umfragewelle == 9]),
    alter          = first_non_na(alter[umfragewelle == 7], alter[umfragewelle == 8], alter[umfragewelle == 9]),
    einkommen      = first_non_na(haushaltseinkommen[umfragewelle == 7], haushaltseinkommen[umfragewelle == 8], haushaltseinkommen[umfragewelle == 9]),
    geschlecht_final = first_non_na(geschlecht[umfragewelle == 7], geschlecht[umfragewelle == 8], geschlecht[umfragewelle == 9]),
    region_final     = first_non_na(region[umfragewelle == 7], region[umfragewelle == 8], region[umfragewelle == 9]),
    bildung_final    = first_non_na(bildung[umfragewelle == 7], bildung[umfragewelle == 8], bildung[umfragewelle == 9]),
    gewicht        = first(ipf_gewicht[umfragewelle == 7])
  )

# Ungewichtete Mittelwerte und SDs
summary_stats_ungewichtet <- data.frame(
  Variable = c("Openness", "Conscientiousness", "Extraversion", "Agreeableness", "Neuroticism", "Alter", "Haushaltseinkommen"),
  Mean = sapply(personen_daten[, 2:8], mean, na.rm = TRUE),
  SD   = sapply(personen_daten[, 2:8], sd, na.rm = TRUE)
)

print(summary_stats_ungewichtet)

# Gewichtete Mittelwerte und SDs
variablen <- c("openness", "conscientiousness", "extraversion", 
               "agreeableness", "neuroticism", "alter", "einkommen")

summary_stats_gewichtet <- data.frame(
  Variable = c("Openness", "Conscientiousness", "Extraversion", "Agreeableness", "Neuroticism", "Alter", "Haushaltseinkommen"),
  Mean = sapply(variablen, function(v) {
    wtd.mean(personen_daten[[v]], weights = personen_daten$gewicht, na.rm = TRUE)
  }),
  SD = sapply(variablen, function(v) {
    sqrt(wtd.var(personen_daten[[v]], weights = personen_daten$gewicht, na.rm = TRUE))
  })
)

print(summary_stats_gewichtet)


# Funktionen zur Berechnung
## Gewichtete absolute Häufigkeit
gewichtete_abs_haeufigkeit <- function(df, var, gewicht) {
  df %>%
    filter(!is.na(.data[[var]])) %>%
    group_by_at(var) %>%
    dplyr::summarise(Gewichtete_Anzahl = sum(.data[[gewicht]], na.rm = TRUE)) %>%
    arrange(desc(Gewichtete_Anzahl))
}

## Gewichtete relative Häufigkeit
gewichtete_rel_haeufigkeit <- function(abs_df) {
  total <- sum(abs_df$Gewichtete_Anzahl)
  abs_df %>%
    mutate(Relativ = round(Gewichtete_Anzahl / total, 4))
}

## Ungewichtete absolute Häufigkeit
ungewichtete_abs_haeufigkeit <- function(df, var) {
  df %>%
    filter(!is.na(.data[[var]])) %>%
    count(.data[[var]]) %>%
    rename(Wert = 1, Absolute = n)
}

## Ungewichtete relative Häufigkeit
ungewichtete_rel_haeufigkeit <- function(abs_df) {
  total <- sum(abs_df$Absolute)
  abs_df %>%
    mutate(Relativ = round(Absolute / total, 4))
}

# Alle Häufigkeiten berechnen
## Ungewichtet
geschlecht_abs_ung <- ungewichtete_abs_haeufigkeit(personen_daten, "geschlecht_final")
region_abs_ung     <- ungewichtete_abs_haeufigkeit(personen_daten, "region_final")
bildung_abs_ung    <- ungewichtete_abs_haeufigkeit(personen_daten, "bildung_final")

geschlecht_rel_ung <- ungewichtete_rel_haeufigkeit(geschlecht_abs_ung)
region_rel_ung     <- ungewichtete_rel_haeufigkeit(region_abs_ung)
bildung_rel_ung    <- ungewichtete_rel_haeufigkeit(bildung_abs_ung)

## Gewichtet
geschlecht_abs_gew <- gewichtete_abs_haeufigkeit(personen_daten, "geschlecht_final", "gewicht")
region_abs_gew     <- gewichtete_abs_haeufigkeit(personen_daten, "region_final", "gewicht")
bildung_abs_gew    <- gewichtete_abs_haeufigkeit(personen_daten, "bildung_final", "gewicht")

geschlecht_rel_gew <- gewichtete_rel_haeufigkeit(geschlecht_abs_gew)
region_rel_gew     <- gewichtete_rel_haeufigkeit(region_abs_gew)
bildung_rel_gew    <- gewichtete_rel_haeufigkeit(bildung_abs_gew)

# Ausgabe
cat("\n--- UNGEWICHTETE absolute Häufigkeiten ---\n")
print(geschlecht_abs_ung)
print(region_abs_ung)
print(bildung_abs_ung)

cat("\n--- UNGEWICHTETE relative Häufigkeiten ---\n")
print(geschlecht_rel_ung)
print(region_rel_ung)
print(bildung_rel_ung)

cat("\n--- GEWICHTETE absolute Häufigkeiten ---\n")
print(geschlecht_abs_gew)
print(region_abs_gew)
print(bildung_abs_gew)

cat("\n--- GEWICHTETE relative Häufigkeiten ---\n")
print(geschlecht_rel_gew)
print(region_rel_gew)
print(bildung_rel_gew)





# 4. ICC Werte der Persönlichkeitsdimensionen - nicht gewichtet!

# Berechnung der ICC für Openness
Openness_model <- lmer(Openness ~ (1|id3), data = data)
icc_Openness <- icc(Openness_model)$ICC_adjusted
print(icc_Openness)

# Berechnung der ICC für Conscientiousness
Conscientiousness_model <- lmer(Conscientiousness ~ (1|id3), data = data)
icc_Conscientiousness <- icc(Conscientiousness_model)$ICC_adjusted
print(icc_Conscientiousness)

# Berechnung der ICC für Extraversion
extraversion_model <- lmer(Extraversion ~ (1|id3), data = data)
icc_extraversion <- icc(extraversion_model)$ICC_adjusted
print(icc_extraversion)

# Berechnung der ICC für Agreeableness
Agreeableness_model <- lmer(Agreeableness ~ (1|id3), data = data)
icc_Agreeableness <- icc(Agreeableness_model)$ICC_adjusted
print(icc_Agreeableness)

# Berechnung der ICC für Neuroticism
Neuroticism_model <- lmer(Neuroticism ~ (1|id3), data = data)
icc_Neuroticism <- icc(Neuroticism_model)$ICC_adjusted
print(icc_Neuroticism)

# Zusammenfassen der ICCs in einem DataFrame
icc_data <- data.frame(
  Persönlichkeitsvariable = c("Extraversion", "Agreeableness", "Conscientiousness", "Neuroticism", "Openness"),
  ICC = c(icc_extraversion, icc_Agreeableness, icc_Conscientiousness, icc_Neuroticism, icc_Openness)
)




# 5. Voruntersuchungen 

# 5.1 Mittelwerte und Konfindenzintervalle der Big Five nach Parteipräferenz - gewichtet!

weighted_mean_se <- function(x, w) {
  wm <- weighted.mean(x, w, na.rm = TRUE)
  # Weighted variance:
  wvar <- sum(w * (x - wm)^2, na.rm = TRUE) / sum(w, na.rm = TRUE)
  # Standard error:
  wse <- sqrt(wvar / sum(w, na.rm = TRUE))
  return(c(mean = wm, se = wse))
}

big_five_summary_ci_weighted <- data %>%
  filter(!is.na(Parteipräferenz) & Parteipräferenz != "Andere") %>%
  group_by(Parteipräferenz) %>%
  dplyr::summarise(
    Extraversion = list(weighted_mean_se(ExtraversionMittelwert, ipf_gewicht)),
    Conscientiousness = list(weighted_mean_se(ConscientiousnessMittelwert, ipf_gewicht)),
    Neuroticism = list(weighted_mean_se(NeuroticismMittelwert, ipf_gewicht)),
    Openness = list(weighted_mean_se(OpennessMittelwert, ipf_gewicht)),
    Agreeableness = list(weighted_mean_se(AgreeablenessMittelwert, ipf_gewicht))
  ) %>%
  # Extrahiere Mittelwerte und SE in separate Spalten
  mutate(
    Extraversion_mean = map_dbl(Extraversion, "mean"),
    Extraversion_se = map_dbl(Extraversion, "se"),
    Conscientiousness_mean = map_dbl(Conscientiousness, "mean"),
    Conscientiousness_se = map_dbl(Conscientiousness, "se"),
    Neuroticism_mean = map_dbl(Neuroticism, "mean"),
    Neuroticism_se = map_dbl(Neuroticism, "se"),
    Openness_mean = map_dbl(Openness, "mean"),
    Openness_se = map_dbl(Openness, "se"),
    Agreeableness_mean = map_dbl(Agreeableness, "mean"),
    Agreeableness_se = map_dbl(Agreeableness, "se")
  ) %>%
  dplyr::select(Parteipräferenz,
                Extraversion_mean, Extraversion_se,
                Conscientiousness_mean, Conscientiousness_se,
                Neuroticism_mean, Neuroticism_se,
                Openness_mean, Openness_se,
                Agreeableness_mean, Agreeableness_se)

# Anpassen der Big Five Reihenfolge
big_five_levels <- c("Neuroticism", "Agreeableness", "Extraversion", "Conscientiousness", "Openness")

# Umformung der Daten für die Verwendung in ggplot2
big_five_long_ci <- big_five_summary_ci_weighted %>%
  pivot_longer(
    cols = c(Extraversion_mean, Extraversion_se,
             Conscientiousness_mean, Conscientiousness_se,
             Neuroticism_mean, Neuroticism_se,
             Openness_mean, Openness_se,
             Agreeableness_mean, Agreeableness_se),
    names_to = c("trait", "stat"),
    names_sep = "_",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = stat,
    values_from = value
  ) %>%
  mutate(trait = factor(trait, levels = big_five_levels))

# PDF-Gerät öffnen
pdf(file = "figures/Figure 3.pdf", width = 10, height = 7)

# Erstellung des Plots mit Konfidenzintervallen
p2 <- ggplot(big_five_long_ci, aes(x = mean, y = trait, color = Parteipräferenz, shape = Parteipräferenz)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) + 
  geom_errorbarh(aes(xmin = mean - 1.96 * se, xmax = mean + 1.96 * se),  
                 position = position_dodge(width = 0.5), height = 0.2) +
  labs(title = "Mean Values and Confidence Intervals of the Big Five by Party Preference",
       x = "BFI-10 Score", 
       y = "Big Five",
       color = "Party Preference",
       shape = "Party Preference") +
  scale_color_manual(values = c("Linke" = "#FF69B4", 
                                "Grüne" = "#32CD32", 
                                "SPD" = "#FF6347", 
                                "FDP" = "#FFD700", 
                                "Union" = "black", 
                                "AfD" = "#1E90FF", 
                                "Andere" = "lightgrey"),
                     labels = c("Linke" = "Left Party", 
                                "Grüne" = "Greens", 
                                "SPD" = "Social Democrats (SPD)", 
                                "FDP" = "Liberal Democrats (FDP)", 
                                "Union" = "Christian Democrats (CDU/CSU)", 
                                "AfD" = "Right-wing populist Party (AfD)", 
                                "Andere" = "Other")) +
  scale_shape_manual(values = c("Linke" = 16,   # Kreis
                                "Grüne" = 17,   # Dreieck
                                "SPD" = 18,     # Raute
                                "FDP" = 15,     # Quadrat
                                "Union" = 8,    # Stern
                                "AfD" = 3,      # Kreuz
                                "Andere" = 3),  # Plus
                     labels = c("Linke" = "Left Party", 
                                "Grüne" = "Greens", 
                                "SPD" = "Social Democrats (SPD)", 
                                "FDP" = "Liberal Democrats (FDP)", 
                                "Union" = "Christian Democrats (CDU/CSU)", 
                                "AfD" = "Right-wing populist Party (AfD)", 
                                "Andere" = "Other")) +
  theme_minimal(base_family = "Times") +  
  theme(
    plot.title = element_text(hjust = 0.5, family = "Times", size = 18),  
    axis.title = element_text(family = "Times", size = 16), 
    axis.title.x = element_text(vjust = -1),
    axis.text = element_text(family = "Times", size = 14),  
    legend.text = element_text(family = "Times", size = 14),
    legend.title = element_text(family = "Times", size = 14),  
    legend.position = "bottom"  
  ) 

# Plot ausgeben
print(p2)

# PDF schließen
dev.off()




# 5.2 Sankey Diagramm zur Wählerwanderung

# Daten vorbereiten (nur gültige Fälle)
data_clean <- data %>%
  filter(!is.na(Parteipräferenz), !is.na(umfragewelle)) %>%
  dplyr::select(id3, umfragewelle, Parteipräferenz, ipf_gewicht)

# Ins Wide-Format umwandeln
data_wide <- data_clean %>%
  pivot_wider(names_from = umfragewelle, values_from = Parteipräferenz, names_prefix = "welle_")

# Gruppierung für Wechselstatus (mit Faktorlevels für w7_w8 und w7_w8_w9)
data_wide <- data_wide %>%
  mutate(
    w7_w8 = case_when(
      welle_7 == welle_8 ~ "No Switch",
      welle_7 != welle_8 ~ "Switch"
    ),
    w7_w8 = factor(w7_w8, levels = c("No Switch", "Switch")),  
    
    w7_w8_w9 = case_when(
      welle_7 == welle_8 & welle_8 == welle_9 ~ "No Switch",
      welle_7 == welle_8 & welle_8 != welle_9 ~ "Late Switch",
      welle_7 != welle_8 & welle_7 == welle_9 ~ "Switch back",
      welle_7 != welle_8 & welle_8 == welle_9 ~ "Early Switch",
      welle_7 != welle_8 & welle_8 != welle_9 & welle_7 != welle_9 ~ "Double Switch"
    ),
    w7_w8_w9 = factor(w7_w8_w9, levels = c("No Switch", "Late Switch", "Early Switch", "Switch back", "Double Switch"))
  )

# Aggregieren: Wie viele Personen je Kombination (gewichtet)?
sankey_data <- data_wide %>%
  filter(!is.na(welle_7), !is.na(w7_w8), !is.na(w7_w8_w9)) %>%
  group_by(welle_7, w7_w8, w7_w8_w9) %>%
  summarise(Gewicht = sum(ipf_gewicht), .groups = "drop")


# Farben definieren 
party_colors <- c(
  "Left" = "#FF69B4",
  "Greens" = "#32CD32",
  "SPD" = "#FF6347",
  "FDP" = "#FFD700",
  "CDU/CSU" = "black",
  "AfD" = "#1E90FF",
  "Others" = "lightgrey"
)

# Graustufen für die Wechselkategorien
switch_colors <- c(
  "No Switch"    = "grey90",
  "Switch"       = "grey50",
  "Late Switch"  = "grey70",
  "Early Switch" = "grey50",
  "Switch back"  = "grey30",
  "Double Switch"= "grey1"
)

# Parteinamen übersetzen
sankey_data <- sankey_data %>%
  mutate(welle_7 = factor(welle_7,
                          levels = c("Linke", "Grüne", "SPD", "FDP", "Union", "AfD", "Andere"),
                          labels = c("Left", "Greens", "SPD", "FDP",
                                     "CDU/CSU", "AfD", "Others")))


w7_w8_w9_levels <- c("No Switch", "Late Switch", "Early Switch", "Switch back", "Double Switch")



# Labels aus Prozent-Arrays erstellen
sankey_data <- sankey_data %>%
  mutate(
    label_welle_7      = paste0(welle_7, " (", wave1percent, "%)"),
    label_w7_w8        = paste0(w7_w8, " (", wave2percent, "%)"),
    label_w7_w8_w9     = paste0(w7_w8_w9, " (", wave3percent, "%)")
  )

# PDF-Gerät öffnen
pdf(file = "figures/Figure 2.pdf", width = 10, height = 10)


# Sankey-Diagramm erstellen
ggplot(sankey_data,
       aes(axis1 = welle_7, axis2 = w7_w8, axis3 = w7_w8_w9, y = Gewicht)) +
  
  # Flüsse in Parteifarben
  geom_alluvium(aes(fill = welle_7), width = 0.5, knot.pos = 0.3) +   
  
  # Strata: Achse 1 bunt, Achsen 2/3 Grautöne
  geom_stratum(
    aes(fill = ifelse(..x.. == 1, as.character(..stratum..), as.character(..stratum..))),
    width = 0.5, color = "black"
  ) +
  
  # Strata-Beschriftungen mit Prozenten
  geom_text(
    stat = "stratum",
    aes(
      label = ifelse(..x.. == 1,
                     paste0(..stratum.., " (", sprintf("%.1f", 100 * ..prop..), "%)"),
                     paste0(..stratum.., " (", sprintf("%.1f", 100 * ..prop..), "%)")
      ),
      color = ifelse(..stratum.. %in% c("FDP", "Others", "No Switch", "Late Switch"), "black", "white")
    ),
    size = 3.5, family = "sans"
  ) +
  scale_color_identity() +
  
  scale_x_discrete(
    limits = c("July 2022", "January 2023", "October 2023"),
    expand = c(.05, .05)
  ) +
  
  scale_fill_manual(
    values = c(party_colors, switch_colors, "Andere" = "grey80"),
    na.value = "grey50"
  ) +
  
  theme_minimal(base_family = "sans") +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 18, color = "black", margin = margin(t = 10)),
    legend.position = "none"
  )

# PDF schließen
dev.off()





# Prozentwerte für Beschriftung
# strata 1
wave1 <- tapply(data_wide$ipf_gewicht, data_wide$welle_7, sum, na.rm = TRUE)
wave1percent <- 100 * wave1 / sum(wave1)

# strata 2
wave2 <- tapply(data_wide$ipf_gewicht, data_wide$w7_w8, sum, na.rm = TRUE)
wave2percent <- 100 * wave2 / sum(wave2)

# strata 3
wave3 <- tapply(data_wide$ipf_gewicht, data_wide$w7_w8_w9, sum, na.rm = TRUE)
wave3percent <- 100 * wave3 / sum(wave3)

wave1percent
wave2percent
wave3percent


# Switch to AfD in W2
switcherw2 <- data_wide[data_wide$w7_w8 == "Switch", ]
weighted_sum <- tapply(switcherw2$ipf_gewicht, switcherw2$welle_8, sum, na.rm = TRUE)
percentswitchw2 <- 100 * weighted_sum / sum(wave2)
percentswitchw2

# Late Switch to AfD
switcherlate <- data_wide[data_wide$w7_w8_w9 == "Late Switch", ]
weighted_sum <- tapply(switcherlate$ipf_gewicht, switcherlate$welle_9, sum, na.rm = TRUE)
percentswitchlate <- 100 * weighted_sum / sum(wave3)
percentswitchlate

# Early Switch to AfD
switcherearly <- data_wide[data_wide$w7_w8_w9 == "Early Switch", ]
weighted_sum <- tapply(switcherearly$ipf_gewicht, switcherearly$welle_9, sum, na.rm = TRUE)
percentswitchearly <- 100 * weighted_sum / sum(wave3)
percentswitchearly

# Switch back: to AfD and back to wave 1 party
switcherback <- data_wide[data_wide$w7_w8_w9 == "Switch back", ]
weighted_sum <- tapply(switcherback$ipf_gewicht, switcherback$welle_9, sum, na.rm = TRUE)
percentswitchback <- 100 * weighted_sum / sum(wave3)
percentswitchback

# Double Switch: to AfD in W2 and to another party (other than in w1) in w3
switcherdouble <- data_wide[data_wide$w7_w8_w9 == "Double Switch", ]
weighted_sum <- tapply(switcherdouble$ipf_gewicht, switcherdouble$welle_9, sum, na.rm = TRUE)
percentswitchdouble <- 100 * weighted_sum / sum(wave3)
percentswitchdouble

percentswitchw2
percentswitchlate
percentswitchearly
percentswitchback
percentswitchdouble


# Sankey-Diagramm erstellen

ggplot(sankey_data,
       aes(axis1 = welle_7, axis2 = w7_w8, axis3 = w7_w8_w9, y = Gewicht)) +
  # Flüsse in Parteifarben
  geom_alluvium(aes(fill = welle_7), width = 0.5, knot.pos = 0.3) +   
  
  # Strata: Achse 1 bunt, Achsen 2/3 graustufen
  geom_stratum(
    aes(fill = ifelse(..x.. == 1, as.character(..stratum..), as.character(..stratum..))),
    width = 0.5, color = "black"
  ) +
  
  # Strata-Beschriftungen
  geom_text(
    stat = "stratum",
    aes(
      label = after_stat(stratum),
      color = ifelse(after_stat(stratum) %in% c("FDP", "Others", "No Switch", "Late Switch"), "black", "white")
    ),
    size = 5, family = "sans"
  ) +
  scale_color_identity() +
  scale_x_discrete(
    limits = c("July 2022", "January 2023", "October 2023"),
    expand = c(.05, .05)
  ) +
  
  scale_fill_manual(
    values = c(party_colors, switch_colors, "Andere" = "grey80"),
    na.value = "grey50"
  ) +
  
  theme_minimal(base_family = "sans") +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 18, color = "black", margin = margin(t = 10)),
    legend.position = "none"
  )





# 6. Hypothese H1a-H1d

# 6.1 Variable Wahlwechsel bauen

# Detaillierte Parteien Codierung
data <- data %>%
  mutate(Parteipräferenz = case_when(
    partei == 6 ~ 1,
    partei == 4 ~ 2,
    partei == 3 ~ 3,
    partei == 5 ~ 4,
    partei %in% c(2, 8) ~ 5,
    partei == 7 ~ 6,
    partei == 10 ~ 7,
    partei == 11 ~ 8,
    partei == 12 ~ 9,
    partei == 13 ~ 10,
    partei == 14 ~ 11,
    partei == 15 ~ 12,
    TRUE ~ partei
  )) %>%
  mutate(Parteipräferenz = factor(Parteipräferenz,
                                  levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                                  labels = c("Linke", "Grüne", "SPD", "FDP", "Union", "AfD", "Freie Wähler", "die PARTEI", "Tierschutzpartei", "dieBasis", "sonstige Partei", "Nichtwähler"),
                                  ordered = TRUE))



# Berechnung der Anzahl der Wechsel in der Variable Parteipräferenz pro Person über die Messzeitpunkte
data <- data %>%
  group_by(id3) %>%
  mutate(Anzahl_Wechsel = n_distinct(Parteipräferenz) - 1) %>%
  ungroup()

# Erstellung der Variable Wahlwechsel
data <- data %>%
  mutate(Wahlwechsel = factor(ifelse(Anzahl_Wechsel >= 1, 1, 0),
                              levels = c(0, 1),
                              labels = c("Nein", "Ja")))


# 6.1a Ungewichtete Prozentanteile
ungw_prozent <- data %>%
  dplyr::select(id3, Wahlwechsel) %>%
  distinct() %>%
  count(Wahlwechsel) %>%
  mutate(Prozent = round(100 * n / sum(n), 1))
ungw_prozent

# 6.1b Gewichtete Prozentanteile
gew_prozent <- data %>%
  dplyr::select(id3, Wahlwechsel, ipf_gewicht) %>%
  distinct() %>%
  group_by(Wahlwechsel) %>%
  summarise(Gewicht = sum(ipf_gewicht)) %>%
  mutate(Prozent = round(100 * Gewicht / sum(Gewicht), 1))
gew_prozent



# 6.2 t-Tests Wahlwechsel Gruppen

  # Sicherstellen, dass alle nötigen Variablen vorhanden sind
data_filtered <- data %>%
  distinct(id3, .keep_all = TRUE) %>% 
  dplyr::select(Wahlwechsel, OpennessMittelwert, ConscientiousnessMittelwert,
                ExtraversionMittelwert, AgreeablenessMittelwert, NeuroticismMittelwert,
                ipf_gewicht)

# Survey-Design definieren
design <- svydesign(ids = ~1, weights = ~ipf_gewicht, data = data_filtered)

# Gewichteter t-Test (bzw. gewichtete lineare Regression)
ttest_openness <- svyglm(OpennessMittelwert ~ Wahlwechsel, design = design)
ttest_conscientiousness <- svyglm(ConscientiousnessMittelwert ~ Wahlwechsel, design = design)
ttest_extraversion <- svyglm(ExtraversionMittelwert ~ Wahlwechsel, design = design)
ttest_agreeableness <- svyglm(AgreeablenessMittelwert ~ Wahlwechsel, design = design)
ttest_neuroticism <- svyglm(NeuroticismMittelwert ~ Wahlwechsel, design = design)

summary_stats_weighted <- data_filtered %>%
  group_by(Wahlwechsel) %>%
  summarise(
    N = n(),
    M_Openness = weighted.mean(OpennessMittelwert, ipf_gewicht),
    SD_Openness = sqrt(Hmisc::wtd.var(OpennessMittelwert, ipf_gewicht)),
    
    M_Conscientiousness = weighted.mean(ConscientiousnessMittelwert, ipf_gewicht),
    SD_Conscientiousness = sqrt(Hmisc::wtd.var(ConscientiousnessMittelwert, ipf_gewicht)),
    
    M_Extraversion = weighted.mean(ExtraversionMittelwert, ipf_gewicht),
    SD_Extraversion = sqrt(Hmisc::wtd.var(ExtraversionMittelwert, ipf_gewicht)),
    
    M_Agreeableness = weighted.mean(AgreeablenessMittelwert, ipf_gewicht),
    SD_Agreeableness = sqrt(Hmisc::wtd.var(AgreeablenessMittelwert, ipf_gewicht)),
    
    M_Neuroticism = weighted.mean(NeuroticismMittelwert, ipf_gewicht),
    SD_Neuroticism = sqrt(Hmisc::wtd.var(NeuroticismMittelwert, ipf_gewicht))
  )

weighted_cohen_d <- function(x, group, weights) {
  group_levels <- unique(group)
  if (length(group_levels) != 2) stop("Nur zwei Gruppen erlaubt.")
  
  # Gruppierte Daten
  x1 <- x[group == group_levels[1]]
  w1 <- weights[group == group_levels[1]]
  x2 <- x[group == group_levels[2]]
  w2 <- weights[group == group_levels[2]]
  
  # Gewichtete Mittelwerte
  m1 <- weighted.mean(x1, w1)
  m2 <- weighted.mean(x2, w2)
  
  # Gewichtete Varianzen
  s1 <- Hmisc::wtd.var(x1, w1)
  s2 <- Hmisc::wtd.var(x2, w2)
  
  # Pooled SD
  pooled_sd <- sqrt((s1 + s2) / 2)
  
  # Cohen's d
  d <- (m1 - m2) / pooled_sd
  return(d)
}

# Cohen's D
weighted_cohen_d(data_filtered$OpennessMittelwert, data_filtered$Wahlwechsel, data_filtered$ipf_gewicht)
weighted_cohen_d(data_filtered$ConscientiousnessMittelwert, data_filtered$Wahlwechsel, data_filtered$ipf_gewicht)
weighted_cohen_d(data_filtered$ExtraversionMittelwert, data_filtered$Wahlwechsel, data_filtered$ipf_gewicht)
weighted_cohen_d(data_filtered$AgreeablenessMittelwert, data_filtered$Wahlwechsel, data_filtered$ipf_gewicht)
weighted_cohen_d(data_filtered$NeuroticismMittelwert, data_filtered$Wahlwechsel, data_filtered$ipf_gewicht)

summary(ttest_openness)
summary(ttest_conscientiousness)
summary(ttest_extraversion)
summary(ttest_agreeableness)
summary(ttest_neuroticism)

print(summary_stats_weighted, n = Inf, width = Inf)
cat("Anzahl der Fälle in der Berechnung:", nrow(data_filtered), "\n")





# 6.3 gewichtete Mediationsanalyse

#Skalenniveau explizit setzen
data$geschlecht <- as.factor(data$geschlecht)
data$region <- as.factor(data$region)
data$bildung <- factor(data$bildung, ordered = FALSE)
data$haushaltseinkommen <- factor(data$haushaltseinkommen, ordered = FALSE)
data$alter <- as.numeric(data$alter)

data_only_wave_1 <-data
data_only_wave_1 <- data_only_wave_1[data$umfragewelle == 7,]

### save dataset for Stata code to build Figure 4
write_dta(data_only_wave_1, "data/data_wahlwechsel.dta") 


# Regressionen

model.m <- lm(LiRe ~ OpennessMittelwert + ConscientiousnessMittelwert + ExtraversionMittelwert + AgreeablenessMittelwert + NeuroticismMittelwert + alter + geschlecht + bildung + haushaltseinkommen + region,
                data = data_only_wave_1,
                weights = ipf_gewicht)

model.y <- glm(Wahlwechsel ~ OpennessMittelwert + ConscientiousnessMittelwert + ExtraversionMittelwert + AgreeablenessMittelwert + NeuroticismMittelwert + LiRe + alter + geschlecht + bildung + haushaltseinkommen + region,
                 family = binomial(link = "logit"),
                 data = data_only_wave_1,
                 weights = ipf_gewicht)

summary(model.m)
summary(model.y)
tab_model(model.y)


# McFadden's Pseudo-R² mit gewichteten Daten
n_effective <- nobs(model.y)
cat("Anzahl einbezogener Subjekte:", n_effective, "\n")
ll_full <- logLik(model.y)
ll_null <- logLik(update(model.y, . ~ 1))
r2_mcfadden <- 1 - (as.numeric(ll_full) / as.numeric(ll_null))

cat("McFadden's Pseudo-R²:", round(r2_mcfadden, 3), "\n")

# Tjur's -R²
r2(model.y)


# Modell tidy machen und exponentiieren (Odds Ratios)
tidy_model_y <- tidy(model.y, conf.int = TRUE) %>%
  mutate(estimate = exp(estimate),      
         conf.low = exp(conf.low),  
         conf.high = exp(conf.high),       
         model = "Wahlwechsel")            

# Ausgabe der Odds Ratios mit Konfidenzintervallen und p-Werten
print(tidy_model_y %>% dplyr::select(term, estimate, conf.low, conf.high, p.value))


# Mediationsanalysen
set.seed(123)
med.out_openness <- mediate(model.m, model.y,
                              treat = "OpennessMittelwert",
                              mediator = "LiRe",
                              boot = TRUE, sims = 1000,
                              cluster = data_only_wave_1$id3)

summary(med.out_openness)


set.seed(123)
med.out_conscientiousness <- mediate(model.m, model.y,
                                     treat = "ConscientiousnessMittelwert",
                                     mediator = "LiRe",
                                     boot = TRUE, sims = 1000,
                                     cluster = data_only_wave_1$id3)

summary(med.out_conscientiousness)


set.seed(123)
med.out_extraversion <- mediate(model.m, model.y,
                                treat = "ExtraversionMittelwert",
                                mediator = "LiRe",
                                boot = TRUE, sims = 1000,
                                cluster = data_only_wave_1$id3)

summary(med.out_extraversion)


set.seed(123)
med.out_agreeableness <- mediate(model.m, model.y,
                                 treat = "AgreeablenessMittelwert",
                                 mediator = "LiRe",
                                 boot = TRUE, sims = 1000,
                                 cluster = data_only_wave_1$id3)

summary(med.out_agreeableness)


set.seed(123)
med.out_neuroticism <- mediate(model.m, model.y,
                               treat = "NeuroticismMittelwert",
                               mediator = "LiRe",
                               boot = TRUE, sims = 1000,
                               cluster = data_only_wave_1$id3)

summary(med.out_neuroticism)



# Funktion zur Extraktion der Mediationseffekte aus einem mediate-Objekt
extract_mediation_results <- function(med.out, variable_label) {
  data.frame(
    variable = variable_label,
    effect = c("ACME", "ADE", "Total Effect"),
    estimate = c(med.out$d0, med.out$z0, med.out$tau.coef),
    ci.low = c(med.out$d0.ci[1], med.out$z0.ci[1], med.out$tau.ci[1]),
    ci.high = c(med.out$d0.ci[2], med.out$z0.ci[2], med.out$tau.ci[2])
  )
}

# Ergebnisse aus allen fünf Mediationen sammeln
results_list <- list(
  extract_mediation_results(med.out_openness, "Openness"),
  extract_mediation_results(med.out_conscientiousness, "Conscientiousness"),
  extract_mediation_results(med.out_extraversion, "Extraversion"),
  extract_mediation_results(med.out_agreeableness, "Agreeableness"),
  extract_mediation_results(med.out_neuroticism, "Neuroticism")
)

# Zusammenführen der Daten
results_df_plot <- bind_rows(results_list)

# In Odds Ratio umwandeln
results_df_table <- results_df_plot %>%
  mutate(
    estimate = exp(estimate),
    ci.low = exp(ci.low),
    ci.high = exp(ci.high)
  )

# Nur Tabelle in OR
print(results_df_table)

# Richtige Reihenfolge
results_df_plot$variable <- factor(results_df_plot$variable, 
                                   levels = c("Neuroticism", "Agreeableness", "Extraversion", "Conscientiousness", "Openness"))

results_df_plot$effect <- factor(results_df_plot$effect, 
                                 levels = c("ACME", "ADE", "Total Effect"))


# PDF-Gerät öffnen
pdf(file = "figures/Figure 5.pdf", width = 10, height = 7)

# Plot
ggplot(results_df_plot, aes(x = estimate, y = variable, color = effect)) +
  geom_point(position = position_dodge(width = 0.5), size = 2.5) +
  geom_errorbar(aes(xmin = ci.low, xmax = ci.high), 
                position = position_dodge(width = 0.5), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  scale_color_manual(
    values = c("ACME" = "#1b9e77", "ADE" = "#d95f02", "Total Effect" = "#7570b3"),
    labels = c("ACME (indirect effect)", "ADE (direct effect)", "Total effect")
  ) +
  labs(
    x = "Effect Estimate (log odds)",
    y = NULL,
    color = "Effect type"
  ) +
  theme_minimal(base_family = "Times") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 17),
    legend.text = element_text(size = 15),
    axis.title.x = element_text(vjust = -1, size = 15),
    axis.title.y = element_text(hjust = 0.5, size = 15),
    axis.text = element_text(size = 15)
  )

# PDF schließen
dev.off()





# 7. Hypthesen H2a-H2d

# 7.1 Variable "Populismusanfällige" bauen
data <- data %>%
  group_by(id3) %>%
  mutate(
    Populismusanfällige = case_when(
      any(partei[1] == 7) ~ NA_character_,  
      any(partei[2:3] == 7) ~ "Ja",        
      TRUE ~ "Nein"                       
    )
  ) %>%
  ungroup()

# Reduzieren auf eindeutige Personen mit gültigem Populismusstatus
data_populismus <- data %>%
  distinct(id3, .keep_all = TRUE) %>%
  filter(!is.na(Populismusanfällige))

# Proentuale Anteile für Populismusanfällige
# Ungewichtet
data_populismus %>%
  count(Populismusanfällige) %>%
  mutate(Prozent = round(n / sum(n) * 100, 2))
# Gewichtet
data_populismus %>%
  group_by(Populismusanfällige) %>%
  summarise(gewicht_sum = sum(ipf_gewicht)) %>%
  mutate(Prozent = round(gewicht_sum / sum(gewicht_sum) * 100, 2))



# 7.2 t-Test zu Populismusanfälligkeit

# Sicherstellen, dass alle nötigen Variablen vorhanden sind
data_filtered <- data %>%
  distinct(id3, .keep_all = TRUE) %>%  
  dplyr::select(Populismusanfällige, OpennessMittelwert, ConscientiousnessMittelwert,
                ExtraversionMittelwert, AgreeablenessMittelwert, NeuroticismMittelwert,
                ipf_gewicht)

# Bereinigen der Daten (nur zwei Gruppen)
data_filtered <- data_filtered %>%
  filter(Populismusanfällige %in% c("Ja", "Nein")) %>%
  droplevels()

# Survey-Design definieren
design <- svydesign(ids = ~1, weights = ~ipf_gewicht, data = data_filtered)

# Gewichteter t-Test (bzw. gewichtete lineare Regression)
ttest_openness <- svyglm(OpennessMittelwert ~ Populismusanfällige, design = design)
ttest_conscientiousness <- svyglm(ConscientiousnessMittelwert ~ Populismusanfällige, design = design)
ttest_extraversion <- svyglm(ExtraversionMittelwert ~ Populismusanfällige, design = design)
ttest_agreeableness <- svyglm(AgreeablenessMittelwert ~ Populismusanfällige, design = design)
ttest_neuroticism <- svyglm(NeuroticismMittelwert ~ Populismusanfällige, design = design)

summary_stats_weighted <- data_filtered %>%
  group_by(Populismusanfällige) %>%
  summarise(
    N = n(),
    M_Openness = weighted.mean(OpennessMittelwert, ipf_gewicht),
    SD_Openness = sqrt(Hmisc::wtd.var(OpennessMittelwert, ipf_gewicht)),
    
    M_Conscientiousness = weighted.mean(ConscientiousnessMittelwert, ipf_gewicht),
    SD_Conscientiousness = sqrt(Hmisc::wtd.var(ConscientiousnessMittelwert, ipf_gewicht)),
    
    M_Extraversion = weighted.mean(ExtraversionMittelwert, ipf_gewicht),
    SD_Extraversion = sqrt(Hmisc::wtd.var(ExtraversionMittelwert, ipf_gewicht)),
    
    M_Agreeableness = weighted.mean(AgreeablenessMittelwert, ipf_gewicht),
    SD_Agreeableness = sqrt(Hmisc::wtd.var(AgreeablenessMittelwert, ipf_gewicht)),
    
    M_Neuroticism = weighted.mean(NeuroticismMittelwert, ipf_gewicht),
    SD_Neuroticism = sqrt(Hmisc::wtd.var(NeuroticismMittelwert, ipf_gewicht))
  )

weighted_cohen_d <- function(x, group, weights) {
  group_levels <- unique(group)
  if (length(group_levels) != 2) stop("Nur zwei Gruppen erlaubt.")
  
  # Gruppierte Daten
  x1 <- x[group == group_levels[1]]
  w1 <- weights[group == group_levels[1]]
  x2 <- x[group == group_levels[2]]
  w2 <- weights[group == group_levels[2]]
  
  # Gewichtete Mittelwerte
  m1 <- weighted.mean(x1, w1)
  m2 <- weighted.mean(x2, w2)
  
  # Gewichtete Varianzen
  s1 <- Hmisc::wtd.var(x1, w1)
  s2 <- Hmisc::wtd.var(x2, w2)
  
  # Pooled SD
  pooled_sd <- sqrt((s1 + s2) / 2)
  
  # Cohen's d
  d <- (m1 - m2) / pooled_sd
  return(d)
}

# Cohen's D
weighted_cohen_d(data_filtered$OpennessMittelwert, data_filtered$Populismusanfällige, data_filtered$ipf_gewicht)
weighted_cohen_d(data_filtered$ConscientiousnessMittelwert, data_filtered$Populismusanfällige, data_filtered$ipf_gewicht)
weighted_cohen_d(data_filtered$ExtraversionMittelwert, data_filtered$Populismusanfällige, data_filtered$ipf_gewicht)
weighted_cohen_d(data_filtered$AgreeablenessMittelwert, data_filtered$Populismusanfällige, data_filtered$ipf_gewicht)
weighted_cohen_d(data_filtered$NeuroticismMittelwert, data_filtered$Populismusanfällige, data_filtered$ipf_gewicht)

summary(ttest_openness)
summary(ttest_conscientiousness)
summary(ttest_extraversion)
summary(ttest_agreeableness)
summary(ttest_neuroticism)

print(summary_stats_weighted, n = Inf, width = Inf)
cat("Anzahl der Fälle in der Berechnung:", nrow(data_filtered), "\n")





# 7.3 gewichtete Mediationsanalyse

#Skalenniveau explizit setzen
data$geschlecht <- as.factor(data$geschlecht)
data$region <- as.factor(data$region)
data$bildung <- factor(data$bildung, ordered = FALSE)
data$haushaltseinkommen <- factor(data$haushaltseinkommen, ordered = FALSE)
data$alter <- as.numeric(data$alter)

data_only_wave_1 <- data
data_only_wave_1 <- data_only_wave_1[data$umfragewelle == 7,]
data_only_wave_1 <- data_only_wave_1[!is.na(data_only_wave_1$Populismusanfällige), ]
data_only_wave_1$Populismusanfällige <- factor(data_only_wave_1$Populismusanfällige, levels = c("Nein", "Ja"))

### save dataset for Stata code to build Figure 4

write_dta(data_only_wave_1, "data/data_populismus.dta") 

# Regressionen

model.n <- lm(LiRe ~ OpennessMittelwert + ConscientiousnessMittelwert + ExtraversionMittelwert + AgreeablenessMittelwert + NeuroticismMittelwert + alter + geschlecht + bildung + haushaltseinkommen + region,
                data = data_only_wave_1,
                weights = ipf_gewicht)

model.x <- glm(Populismusanfällige ~ OpennessMittelwert + ConscientiousnessMittelwert + ExtraversionMittelwert + AgreeablenessMittelwert + NeuroticismMittelwert + LiRe + alter + geschlecht + bildung + haushaltseinkommen + region,
                 family = binomial(link = "logit"),
                 data = data_only_wave_1,
                 weights = ipf_gewicht)

summary(model.n)
summary(model.x)
tab_model(model.x)


# McFadden's Pseudo-R² mit gewichteten Daten
n_effective <- nobs(model.x)
cat("Anzahl einbezogener Subjekte:", n_effective, "\n")
ll_full <- logLik(model.x)
ll_null <- logLik(update(model.x, . ~ 1))
r2_mcfadden <- 1 - (as.numeric(ll_full) / as.numeric(ll_null))

cat("McFadden's Pseudo-R²:", round(r2_mcfadden, 3), "\n")

# Tjur's -R²
r2(model.x)


# Modell tidy machen und exponentiieren (Odds Ratios)
tidy_model_x <- tidy(model.x, conf.int = TRUE) %>%
  mutate(estimate = exp(estimate),      
         conf.low = exp(conf.low),  
         conf.high = exp(conf.high),       
         model = "Populismusanfälligkeit")            

# Ausgabe der Odds Ratios mit Konfidenzintervallen und p-Werten
print(tidy_model_x %>% dplyr::select(term, estimate, conf.low, conf.high, p.value))

# Mediationsanalysen
set.seed(123)
med.out_openness <- mediate(model.n, model.x,
                            treat = "OpennessMittelwert",
                            mediator = "LiRe",
                            boot = TRUE, sims = 1000,
                            cluster = data_only_wave_1$id3)

summary(med.out_openness)


set.seed(123)
med.out_conscientiousness <- mediate(model.n, model.x,
                                     treat = "ConscientiousnessMittelwert",
                                     mediator = "LiRe",
                                     boot = TRUE, sims = 1000,
                                     cluster = data_only_wave_1$id3)

summary(med.out_conscientiousness)


set.seed(123)
med.out_extraversion <- mediate(model.n, model.x,
                                treat = "ExtraversionMittelwert",
                                mediator = "LiRe",
                                boot = TRUE, sims = 1000,
                                cluster = data_only_wave_1$id3)

summary(med.out_extraversion)


set.seed(123)
med.out_agreeableness <- mediate(model.n, model.x,
                                 treat = "AgreeablenessMittelwert",
                                 mediator = "LiRe",
                                 boot = TRUE, sims = 1000,
                                 cluster = data_only_wave_1$id3)

summary(med.out_agreeableness)


set.seed(123)
med.out_neuroticism <- mediate(model.n, model.x,
                               treat = "NeuroticismMittelwert",
                               mediator = "LiRe",
                               boot = TRUE, sims = 1000,
                               cluster = data_only_wave_1$id3)

summary(med.out_neuroticism)


# Funktion zur Extraktion der Mediationseffekte aus einem mediate-Objekt
extract_mediation_results <- function(med.out, variable_label) {
  data.frame(
    variable = variable_label,
    effect = c("ACME", "ADE", "Total Effect"),
    estimate = c(med.out$d0, med.out$z0, med.out$tau.coef),
    ci.low = c(med.out$d0.ci[1], med.out$z0.ci[1], med.out$tau.ci[1]),
    ci.high = c(med.out$d0.ci[2], med.out$z0.ci[2], med.out$tau.ci[2])
  )
}

# Ergebnisse aus allen fünf Mediationen sammeln
results_list <- list(
  extract_mediation_results(med.out_openness, "Openness"),
  extract_mediation_results(med.out_conscientiousness, "Conscientiousness"),
  extract_mediation_results(med.out_extraversion, "Extraversion"),
  extract_mediation_results(med.out_agreeableness, "Agreeableness"),
  extract_mediation_results(med.out_neuroticism, "Neuroticism")
)

# Zusammenführen der Daten
results_df_plot2 <- bind_rows(results_list)

# In Odds Ratio umwandeln
results_df_table2 <- results_df_plot2 %>%
  mutate(
    estimate = exp(estimate),
    ci.low = exp(ci.low),
    ci.high = exp(ci.high)
  )

# Nur Tabelle in OR
print(results_df_table2)

# Richtige Reihenfolge
results_df_plot2$variable <- factor(results_df_plot2$variable, 
                                   levels = c("Neuroticism", "Agreeableness", "Extraversion", "Conscientiousness", "Openness"))

results_df_plot2$effect <- factor(results_df_plot2$effect, 
                                 levels = c("ACME", "ADE", "Total Effect"))


# PDF-Gerät öffnen
pdf(file = "figures/Figure 6.pdf", width = 10, height = 7)

# Plot
ggplot(results_df_plot2, aes(x = estimate, y = variable, color = effect)) +
  geom_point(position = position_dodge(width = 0.5), size = 2.5) +
  geom_errorbar(aes(xmin = ci.low, xmax = ci.high), 
                position = position_dodge(width = 0.5), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  scale_color_manual(
    values = c("ACME" = "#1b9e77", "ADE" = "#d95f02", "Total Effect" = "#7570b3"),
    labels = c("ACME (indirect effect)", "ADE (direct effect)", "Total effect")
  ) +
  labs(
    x = "Effect Estimate (log odds)",
    y = NULL,
    color = "Effect type"
  ) +
  theme_minimal(base_family = "Times") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 17),
    legend.text = element_text(size = 15),
    axis.title.x = element_text(vjust = -1, size = 15),
    axis.title.y = element_text(hjust = 0.5, size = 15),
    axis.text = element_text(size = 15)
  )

# PDF schließen
dev.off()






# ------------------------------------------------------------------------------

## Robustness check 1 for vote switching in general and susceptibility to populism - Exclusion of potential outliers (left-right)

# 1. Einschluss/Ausschluss von Versuchspersonen

# Ausschluss der Versuchspersonen mit dispcode 22 zu allen Messzeitpunkten
person_ids_dispcode_22 <- unique(dataaaa$id3[dataaaa$dispcode == 22])
dataaa <- dataaaa[!(dataaaa$id3 %in% person_ids_dispcode_22), ]

# Ausschluss der Versuchspersonen mit mindestens einem fehlenden Wert in der Variable partei
person_ids_missing_partei <- unique(dataaa$id3[is.na(dataaa$partei)])
dataa <- dataaa[!(dataaa$id3 %in% person_ids_missing_partei), ]

# Ausschluss der Versuchspersonen mit mindestens einem Wert 16 in der Variable partei
person_ids_with_16 <- unique(dataa$id3[dataa$partei == 16])
data <- dataa[!(dataa$id3 %in% person_ids_with_16), ]

nrow(dataaaa)
nrow(dataaa)
nrow(dataa)
nrow(data)

# Ersetzen von NA in ipf_gewicht durch 1
data$ipf_gewicht[is.na(data$ipf_gewicht)] <- 1

n_distinct(dataaaa$id3)
n_distinct(dataaa$id3)
n_distinct(dataa$id3)
n_distinct(data$id3)



# 2. Vorbereitung der Variablen

# 2.1 Gewicht übertragen

# Extrahiere Gewicht pro id3 aus Umfragewelle 7
gewicht_w7 <- data %>%
  filter(umfragewelle == 7) %>%
  dplyr::select(id3, ipf_gewicht)

# Füge Gewicht über id3 an alle Zeilen im Datensatz an
data <- data %>%
  dplyr::select(-ipf_gewicht) %>% 
  left_join(gewicht_w7, by = "id3")


# 2.2 LiRe Average bilden und transfomireren
data_cleaned <- data %>%
  filter(umfragewelle %in% c(7, 8, 9)) %>%
  dplyr::select(id3, umfragewelle, linksrechts)

# Breite Form: jede Welle in eine eigene Spalte
data_wide <- data_cleaned %>%
  pivot_wider(names_from = umfragewelle, values_from = linksrechts, names_prefix = "w") 

# Differenzen berechnen
data_wide <- data_wide %>%
  mutate(
    diff_78 = abs(w7 - w8),
    diff_89 = abs(w8 - w9),
    diff_79 = abs(w7 - w9),
    max_diff = pmax(diff_78, diff_89, diff_79, na.rm = TRUE)
  )

# id3s mit maximaler Differenz unter 8 behalten
valid_ids <- data_wide %>%
  filter(max_diff < 8) %>%
  pull(id3)

# Ursprünglichen Datensatz auf gültige IDs einschränken
data <- data %>%
  filter(id3 %in% valid_ids)

data <- data %>%
  filter(umfragewelle %in% c(7, 8, 9)) %>%     
  group_by(id3) %>%
  mutate(LiRe = (mean(linksrechts, na.rm = TRUE) - 1) / 10) %>% 
  ungroup()

nrow(data)



# 2.3 Kontrollvariablen

# Region aus Bundesland ableiten
data <- data %>%
  mutate(region = case_when(
    bundesland %in% c(4, 8, 13, 14, 16) ~ "Osten",
    !is.na(bundesland) ~ "Westen",
    TRUE ~ NA_character_
  ))

# Geschlecht als Faktor mit Labels (männlich/weiblich), gleichzeitig numerisch speichern
data$geschlecht[data$geschlecht == 3] <- NA
data$geschlecht <- factor(data$geschlecht, levels = c(1, 2), labels = c("männlich", "weiblich"))
data$geschlecht_num <- as.numeric(data$geschlecht)  # 1 = männlich, 2 = weiblich

# Region: numerisch kodieren für Analysen (Osten = 0, Westen = 1)
data$region <- factor(data$region, levels = c("Osten", "Westen"))
data$region_num <- as.numeric(data$region) - 1  # Osten = 0, Westen = 1

# Haushaltseinkommen: ungültige Werte entfernen, umpolen und als numerisch lassen
data$haushaltseinkommen[data$haushaltseinkommen == 7] <- NA
data$haushaltseinkommen <- 6 - data$haushaltseinkommen
data$haushaltseinkommen <- as.numeric(data$haushaltseinkommen)

# Bildung: ungültige Werte entfernen, als ordinal behandeln (numerisch, aber kein Faktor)
data$bildung[data$bildung %in% c(1, 8, 9, 10)] <- NA
data$bildung <- as.numeric(data$bildung)

# Alter bleibt numerisch
data$alter <- as.numeric(data$alter)


summary(data[, c("geschlecht", "bildung", "haushaltseinkommen", "region", "alter")])



# 2.4 Variable Parteipräferenz bauen

# Umgestaltung der Variable partei und Neubenennung als Parteipräferenz
data <- data %>%
  mutate(Parteipräferenz = case_when(
    partei == 6 ~ 1,
    partei == 4 ~ 2,
    partei == 3 ~ 3,
    partei == 5 ~ 4,
    partei %in% c(2, 8) ~ 5,
    partei == 7 ~ 6,
    partei %in% c(10, 11, 12, 13, 14, 15) ~ 0,
    TRUE ~ partei
  )) %>%
  mutate(Parteipräferenz = factor(Parteipräferenz,
                                  levels = c(0, 1, 2, 3, 4, 5, 6),
                                  labels = c("Andere", "Linke", "Grüne", "SPD", "FDP", "Union", "AfD"),
                                  ordered = TRUE))

# Histogramm zur Häufigkeitsverteilung der neuen Variable Parteipräferenz - ungewichtet
pl1 <- ggplot(data, aes(x = factor(Parteipräferenz))) +
  geom_bar(fill = "purple", color = "black") +
  labs(title = "Häufigkeitsverteilung der Variable Parteipräferenz", 
       x = "Parteipräferenz", 
       y = "Häufigkeit") +
  scale_x_discrete(labels = c("Andere", "Linke", "Grüne", "SPD", "FDP", "Union", "AfD"))

print(pl1)

# Gewichtete Häufigkeiten berechnen
gewichtete_daten <- data %>%
  group_by(Parteipräferenz) %>%
  summarise(Gewicht = sum(ipf_gewicht)) %>%
  mutate(Parteipräferenz = factor(Parteipräferenz,
                                  levels = c("Andere", "Linke", "Grüne", "SPD", "FDP", "Union", "AfD")))

# Histogramm zur Häufigkeitsverteilung der neuen Variable Parteipräferenz - gewichtet
pl2 <- ggplot(gewichtete_daten, aes(x = Parteipräferenz, y = Gewicht)) +
  geom_bar(stat = "identity", fill = "orange", color = "black") +
  labs(title = "Gewichtete Verteilung der Parteipräferenz",
       x = "Parteipräferenz",
       y = "gewichtete Häufigkeit")


# 2.5 Persönlichkeitsvariablen bauen

# Umpolung der angegebenen Variablen
data$v_1280 <- 6 - data$v_1280
data$v_1282 <- 6 - data$v_1282
data$v_1283 <- 6 - data$v_1283
data$v_1284 <- 6 - data$v_1284
data$v_1286 <- 6 - data$v_1286

# Erstellen der neuen Variablen
data$Extraversion <- (data$v_1280 + data$v_1285)/2 
data$Agreeableness <- (data$v_1286 + data$v_1281)/2
data$Conscientiousness <- (data$v_1282 + data$v_1287)/2
data$Neuroticism <- (data$v_1283 + data$v_1288)/2
data$Openness <- (data$v_1284 + data$v_1289)/2

# Sicherstellen, dass die Variablen als intervallskaliert behandelt werden
data <- data %>%
  mutate(Extraversion = as.numeric(Extraversion),
         Agreeableness = as.numeric(Agreeableness),
         Conscientiousness = as.numeric(Conscientiousness),
         Neuroticism = as.numeric(Neuroticism),
         Openness = as.numeric(Openness))

# Berechnung und Skalierung der Big-Five-Mittelwerte pro Person (0 bis 1)
data <- data %>%
  group_by(id3) %>%
  mutate(
    ExtraversionMittelwert = (mean(Extraversion, na.rm = TRUE) - 1) / 4,
    AgreeablenessMittelwert = (mean(Agreeableness, na.rm = TRUE) - 1) / 4,
    ConscientiousnessMittelwert = (mean(Conscientiousness, na.rm = TRUE) - 1) / 4,
    NeuroticismMittelwert = (mean(Neuroticism, na.rm = TRUE) - 1) / 4,
    OpennessMittelwert = (mean(Openness, na.rm = TRUE) - 1) / 4
  ) %>%
  ungroup()



# 6. Hypothese H1a-H1d

# 6.1 Variable Wahlwechsel bauen

# Detaillierte Parteien Codierung
data <- data %>%
  mutate(Parteipräferenz = case_when(
    partei == 6 ~ 1,
    partei == 4 ~ 2,
    partei == 3 ~ 3,
    partei == 5 ~ 4,
    partei %in% c(2, 8) ~ 5,
    partei == 7 ~ 6,
    partei == 10 ~ 7,
    partei == 11 ~ 8,
    partei == 12 ~ 9,
    partei == 13 ~ 10,
    partei == 14 ~ 11,
    partei == 15 ~ 12,
    TRUE ~ partei
  )) %>%
  mutate(Parteipräferenz = factor(Parteipräferenz,
                                  levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                                  labels = c("Linke", "Grüne", "SPD", "FDP", "Union", "AfD", "Freie Wähler", "die PARTEI", "Tierschutzpartei", "dieBasis", "sonstige Partei", "Nichtwähler"),
                                  ordered = TRUE))



# Berechnung der Anzahl der Wechsel in der Variable Parteipräferenz pro Person über die Messzeitpunkte
data <- data %>%
  group_by(id3) %>%
  mutate(Anzahl_Wechsel = n_distinct(Parteipräferenz) - 1) %>%
  ungroup()

# Erstellung der Variable Wahlwechsel
data <- data %>%
  mutate(Wahlwechsel = factor(ifelse(Anzahl_Wechsel >= 1, 1, 0),
                              levels = c(0, 1),
                              labels = c("Nein", "Ja")))


# 6.1a Ungewichtete Prozentanteile
ungw_prozent <- data %>%
  dplyr::select(id3, Wahlwechsel) %>%
  distinct() %>%
  count(Wahlwechsel) %>%
  mutate(Prozent = round(100 * n / sum(n), 1))
ungw_prozent

# 6.1b Gewichtete Prozentanteile
gew_prozent <- data %>%
  dplyr::select(id3, Wahlwechsel, ipf_gewicht) %>%
  distinct() %>%
  group_by(Wahlwechsel) %>%
  summarise(Gewicht = sum(ipf_gewicht)) %>%
  mutate(Prozent = round(100 * Gewicht / sum(Gewicht), 1))
gew_prozent



# 6.2 t-Tests Wahlwechsel Gruppen

# Sicherstellen, dass alle nötigen Variablen vorhanden sind
data_filtered <- data %>%
  distinct(id3, .keep_all = TRUE) %>% 
  dplyr::select(Wahlwechsel, OpennessMittelwert, ConscientiousnessMittelwert,
                ExtraversionMittelwert, AgreeablenessMittelwert, NeuroticismMittelwert,
                ipf_gewicht)

# Survey-Design definieren
design <- svydesign(ids = ~1, weights = ~ipf_gewicht, data = data_filtered)

# Gewichteter t-Test (bzw. gewichtete lineare Regression)
ttest_openness <- svyglm(OpennessMittelwert ~ Wahlwechsel, design = design)
ttest_conscientiousness <- svyglm(ConscientiousnessMittelwert ~ Wahlwechsel, design = design)
ttest_extraversion <- svyglm(ExtraversionMittelwert ~ Wahlwechsel, design = design)
ttest_agreeableness <- svyglm(AgreeablenessMittelwert ~ Wahlwechsel, design = design)
ttest_neuroticism <- svyglm(NeuroticismMittelwert ~ Wahlwechsel, design = design)

summary_stats_weighted <- data_filtered %>%
  group_by(Wahlwechsel) %>%
  summarise(
    N = n(),
    M_Openness = weighted.mean(OpennessMittelwert, ipf_gewicht),
    SD_Openness = sqrt(Hmisc::wtd.var(OpennessMittelwert, ipf_gewicht)),
    
    M_Conscientiousness = weighted.mean(ConscientiousnessMittelwert, ipf_gewicht),
    SD_Conscientiousness = sqrt(Hmisc::wtd.var(ConscientiousnessMittelwert, ipf_gewicht)),
    
    M_Extraversion = weighted.mean(ExtraversionMittelwert, ipf_gewicht),
    SD_Extraversion = sqrt(Hmisc::wtd.var(ExtraversionMittelwert, ipf_gewicht)),
    
    M_Agreeableness = weighted.mean(AgreeablenessMittelwert, ipf_gewicht),
    SD_Agreeableness = sqrt(Hmisc::wtd.var(AgreeablenessMittelwert, ipf_gewicht)),
    
    M_Neuroticism = weighted.mean(NeuroticismMittelwert, ipf_gewicht),
    SD_Neuroticism = sqrt(Hmisc::wtd.var(NeuroticismMittelwert, ipf_gewicht))
  )

weighted_cohen_d <- function(x, group, weights) {
  group_levels <- unique(group)
  if (length(group_levels) != 2) stop("Nur zwei Gruppen erlaubt.")
  
  # Gruppierte Daten
  x1 <- x[group == group_levels[1]]
  w1 <- weights[group == group_levels[1]]
  x2 <- x[group == group_levels[2]]
  w2 <- weights[group == group_levels[2]]
  
  # Gewichtete Mittelwerte
  m1 <- weighted.mean(x1, w1)
  m2 <- weighted.mean(x2, w2)
  
  # Gewichtete Varianzen
  s1 <- Hmisc::wtd.var(x1, w1)
  s2 <- Hmisc::wtd.var(x2, w2)
  
  # Pooled SD
  pooled_sd <- sqrt((s1 + s2) / 2)
  
  # Cohen's d
  d <- (m1 - m2) / pooled_sd
  return(d)
}

# Cohen's D
weighted_cohen_d(data_filtered$OpennessMittelwert, data_filtered$Wahlwechsel, data_filtered$ipf_gewicht)
weighted_cohen_d(data_filtered$ConscientiousnessMittelwert, data_filtered$Wahlwechsel, data_filtered$ipf_gewicht)
weighted_cohen_d(data_filtered$ExtraversionMittelwert, data_filtered$Wahlwechsel, data_filtered$ipf_gewicht)
weighted_cohen_d(data_filtered$AgreeablenessMittelwert, data_filtered$Wahlwechsel, data_filtered$ipf_gewicht)
weighted_cohen_d(data_filtered$NeuroticismMittelwert, data_filtered$Wahlwechsel, data_filtered$ipf_gewicht)

summary(ttest_openness)
summary(ttest_conscientiousness)
summary(ttest_extraversion)
summary(ttest_agreeableness)
summary(ttest_neuroticism)

print(summary_stats_weighted, n = Inf, width = Inf)
cat("Anzahl der Fälle in der Berechnung:", nrow(data_filtered), "\n")





# 6.3 gewichtete Mediationsanalyse

#Skalenniveau explizit setzen
data$geschlecht <- as.factor(data$geschlecht)
data$region <- as.factor(data$region)
data$bildung <- factor(data$bildung, ordered = FALSE)
data$haushaltseinkommen <- factor(data$haushaltseinkommen, ordered = FALSE)
data$alter <- as.numeric(data$alter)

data_only_wave_1 <-data
data_only_wave_1 <- data_only_wave_1[data$umfragewelle == 7,]

### save dataset for Stata code to build Figure 4
write_dta(data_only_wave_1, "data/data_wahlwechsel.dta") 


# Regressionen

model.m <- lm(LiRe ~ OpennessMittelwert + ConscientiousnessMittelwert + ExtraversionMittelwert + AgreeablenessMittelwert + NeuroticismMittelwert + alter + geschlecht + bildung + haushaltseinkommen + region,
              data = data_only_wave_1,
              weights = ipf_gewicht)

model.y <- glm(Wahlwechsel ~ OpennessMittelwert + ConscientiousnessMittelwert + ExtraversionMittelwert + AgreeablenessMittelwert + NeuroticismMittelwert + LiRe + alter + geschlecht + bildung + haushaltseinkommen + region,
               family = binomial(link = "logit"),
               data = data_only_wave_1,
               weights = ipf_gewicht)

summary(model.m)
summary(model.y)
tab_model(model.y)


# McFadden's Pseudo-R² mit gewichteten Daten
n_effective <- nobs(model.y)
cat("Anzahl einbezogener Subjekte:", n_effective, "\n")
ll_full <- logLik(model.y)
ll_null <- logLik(update(model.y, . ~ 1))
r2_mcfadden <- 1 - (as.numeric(ll_full) / as.numeric(ll_null))

cat("McFadden's Pseudo-R²:", round(r2_mcfadden, 3), "\n")

# Tjur's -R²
r2(model.y)


# Modell tidy machen und exponentiieren (Odds Ratios)
tidy_model_y <- tidy(model.y, conf.int = TRUE) %>%
  mutate(estimate = exp(estimate),      
         conf.low = exp(conf.low),  
         conf.high = exp(conf.high),       
         model = "Wahlwechsel")            

# Ausgabe der Odds Ratios mit Konfidenzintervallen und p-Werten
print(tidy_model_y %>% dplyr::select(term, estimate, conf.low, conf.high, p.value))


# Mediationsanalysen
set.seed(123)
med.out_openness <- mediate(model.m, model.y,
                            treat = "OpennessMittelwert",
                            mediator = "LiRe",
                            boot = TRUE, sims = 1000,
                            cluster = data_only_wave_1$id3)

summary(med.out_openness)


set.seed(123)
med.out_conscientiousness <- mediate(model.m, model.y,
                                     treat = "ConscientiousnessMittelwert",
                                     mediator = "LiRe",
                                     boot = TRUE, sims = 1000,
                                     cluster = data_only_wave_1$id3)

summary(med.out_conscientiousness)


set.seed(123)
med.out_extraversion <- mediate(model.m, model.y,
                                treat = "ExtraversionMittelwert",
                                mediator = "LiRe",
                                boot = TRUE, sims = 1000,
                                cluster = data_only_wave_1$id3)

summary(med.out_extraversion)


set.seed(123)
med.out_agreeableness <- mediate(model.m, model.y,
                                 treat = "AgreeablenessMittelwert",
                                 mediator = "LiRe",
                                 boot = TRUE, sims = 1000,
                                 cluster = data_only_wave_1$id3)

summary(med.out_agreeableness)


set.seed(123)
med.out_neuroticism <- mediate(model.m, model.y,
                               treat = "NeuroticismMittelwert",
                               mediator = "LiRe",
                               boot = TRUE, sims = 1000,
                               cluster = data_only_wave_1$id3)

summary(med.out_neuroticism)



# Funktion zur Extraktion der Mediationseffekte aus einem mediate-Objekt
extract_mediation_results <- function(med.out, variable_label) {
  data.frame(
    variable = variable_label,
    effect = c("ACME", "ADE", "Total Effect"),
    estimate = c(med.out$d0, med.out$z0, med.out$tau.coef),
    ci.low = c(med.out$d0.ci[1], med.out$z0.ci[1], med.out$tau.ci[1]),
    ci.high = c(med.out$d0.ci[2], med.out$z0.ci[2], med.out$tau.ci[2])
  )
}

# Ergebnisse aus allen fünf Mediationen sammeln
results_list <- list(
  extract_mediation_results(med.out_openness, "Openness"),
  extract_mediation_results(med.out_conscientiousness, "Conscientiousness"),
  extract_mediation_results(med.out_extraversion, "Extraversion"),
  extract_mediation_results(med.out_agreeableness, "Agreeableness"),
  extract_mediation_results(med.out_neuroticism, "Neuroticism")
)

# Zusammenführen der Daten
results_df_plot <- bind_rows(results_list)

# In Odds Ratio umwandeln
results_df_table <- results_df_plot %>%
  mutate(
    estimate = exp(estimate),
    ci.low = exp(ci.low),
    ci.high = exp(ci.high)
  )

# Nur Tabelle in OR
print(results_df_table)

# Richtige Reihenfolge
results_df_plot$variable <- factor(results_df_plot$variable, 
                                   levels = c("Neuroticism", "Agreeableness", "Extraversion", "Conscientiousness", "Openness"))

results_df_plot$effect <- factor(results_df_plot$effect, 
                                 levels = c("ACME", "ADE", "Total Effect"))


# PDF-Gerät öffnen
pdf(file = "figures/Figure A1.pdf", width = 10, height = 7)

# Plot
ggplot(results_df_plot, aes(x = estimate, y = variable, color = effect)) +
  geom_point(position = position_dodge(width = 0.5), size = 2.5) +
  geom_errorbar(aes(xmin = ci.low, xmax = ci.high), 
                position = position_dodge(width = 0.5), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  scale_color_manual(
    values = c("ACME" = "#1b9e77", "ADE" = "#d95f02", "Total Effect" = "#7570b3"),
    labels = c("ACME (indirect effect)", "ADE (direct effect)", "Total effect")
  ) +
  labs(
    x = "Effect Estimate (log odds)",
    y = NULL,
    color = "Effect type"
  ) +
  theme_minimal(base_family = "Times") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 17),
    legend.text = element_text(size = 15),
    axis.title.x = element_text(vjust = -1, size = 15),
    axis.title.y = element_text(hjust = 0.5, size = 15),
    axis.text = element_text(size = 15)
  )

# PDF schließen
dev.off()




# 7. Hypthesen H2a-H2d

# 7.1 Variable "Populismusanfällige" bauen
data <- data %>%
  group_by(id3) %>%
  mutate(
    Populismusanfällige = case_when(
      any(partei[1] == 7) ~ NA_character_,  
      any(partei[2:3] == 7) ~ "Ja",        
      TRUE ~ "Nein"                       
    )
  ) %>%
  ungroup()

# Reduzieren auf eindeutige Personen mit gültigem Populismusstatus
data_populismus <- data %>%
  distinct(id3, .keep_all = TRUE) %>%
  filter(!is.na(Populismusanfällige))

# Proentuale Anteile für Populismusanfällige
# Ungewichtet
data_populismus %>%
  count(Populismusanfällige) %>%
  mutate(Prozent = round(n / sum(n) * 100, 2))
# Gewichtet
data_populismus %>%
  group_by(Populismusanfällige) %>%
  summarise(gewicht_sum = sum(ipf_gewicht)) %>%
  mutate(Prozent = round(gewicht_sum / sum(gewicht_sum) * 100, 2))



# 7.2 t-Test zu Populismusanfälligkeit

# Sicherstellen, dass alle nötigen Variablen vorhanden sind
data_filtered <- data %>%
  distinct(id3, .keep_all = TRUE) %>%  
  dplyr::select(Populismusanfällige, OpennessMittelwert, ConscientiousnessMittelwert,
                ExtraversionMittelwert, AgreeablenessMittelwert, NeuroticismMittelwert,
                ipf_gewicht)

# Bereinigen der Daten (nur zwei Gruppen)
data_filtered <- data_filtered %>%
  filter(Populismusanfällige %in% c("Ja", "Nein")) %>%
  droplevels()

# Survey-Design definieren
design <- svydesign(ids = ~1, weights = ~ipf_gewicht, data = data_filtered)

# Gewichteter t-Test (bzw. gewichtete lineare Regression)
ttest_openness <- svyglm(OpennessMittelwert ~ Populismusanfällige, design = design)
ttest_conscientiousness <- svyglm(ConscientiousnessMittelwert ~ Populismusanfällige, design = design)
ttest_extraversion <- svyglm(ExtraversionMittelwert ~ Populismusanfällige, design = design)
ttest_agreeableness <- svyglm(AgreeablenessMittelwert ~ Populismusanfällige, design = design)
ttest_neuroticism <- svyglm(NeuroticismMittelwert ~ Populismusanfällige, design = design)

summary_stats_weighted <- data_filtered %>%
  group_by(Populismusanfällige) %>%
  summarise(
    N = n(),
    M_Openness = weighted.mean(OpennessMittelwert, ipf_gewicht),
    SD_Openness = sqrt(Hmisc::wtd.var(OpennessMittelwert, ipf_gewicht)),
    
    M_Conscientiousness = weighted.mean(ConscientiousnessMittelwert, ipf_gewicht),
    SD_Conscientiousness = sqrt(Hmisc::wtd.var(ConscientiousnessMittelwert, ipf_gewicht)),
    
    M_Extraversion = weighted.mean(ExtraversionMittelwert, ipf_gewicht),
    SD_Extraversion = sqrt(Hmisc::wtd.var(ExtraversionMittelwert, ipf_gewicht)),
    
    M_Agreeableness = weighted.mean(AgreeablenessMittelwert, ipf_gewicht),
    SD_Agreeableness = sqrt(Hmisc::wtd.var(AgreeablenessMittelwert, ipf_gewicht)),
    
    M_Neuroticism = weighted.mean(NeuroticismMittelwert, ipf_gewicht),
    SD_Neuroticism = sqrt(Hmisc::wtd.var(NeuroticismMittelwert, ipf_gewicht))
  )

weighted_cohen_d <- function(x, group, weights) {
  group_levels <- unique(group)
  if (length(group_levels) != 2) stop("Nur zwei Gruppen erlaubt.")
  
  # Gruppierte Daten
  x1 <- x[group == group_levels[1]]
  w1 <- weights[group == group_levels[1]]
  x2 <- x[group == group_levels[2]]
  w2 <- weights[group == group_levels[2]]
  
  # Gewichtete Mittelwerte
  m1 <- weighted.mean(x1, w1)
  m2 <- weighted.mean(x2, w2)
  
  # Gewichtete Varianzen
  s1 <- Hmisc::wtd.var(x1, w1)
  s2 <- Hmisc::wtd.var(x2, w2)
  
  # Pooled SD
  pooled_sd <- sqrt((s1 + s2) / 2)
  
  # Cohen's d
  d <- (m1 - m2) / pooled_sd
  return(d)
}

# Cohen's D
weighted_cohen_d(data_filtered$OpennessMittelwert, data_filtered$Populismusanfällige, data_filtered$ipf_gewicht)
weighted_cohen_d(data_filtered$ConscientiousnessMittelwert, data_filtered$Populismusanfällige, data_filtered$ipf_gewicht)
weighted_cohen_d(data_filtered$ExtraversionMittelwert, data_filtered$Populismusanfällige, data_filtered$ipf_gewicht)
weighted_cohen_d(data_filtered$AgreeablenessMittelwert, data_filtered$Populismusanfällige, data_filtered$ipf_gewicht)
weighted_cohen_d(data_filtered$NeuroticismMittelwert, data_filtered$Populismusanfällige, data_filtered$ipf_gewicht)

summary(ttest_openness)
summary(ttest_conscientiousness)
summary(ttest_extraversion)
summary(ttest_agreeableness)
summary(ttest_neuroticism)

print(summary_stats_weighted, n = Inf, width = Inf)
cat("Anzahl der Fälle in der Berechnung:", nrow(data_filtered), "\n")





# 7.3 gewichtete Mediationsanalyse

#Skalenniveau explizit setzen
data$geschlecht <- as.factor(data$geschlecht)
data$region <- as.factor(data$region)
data$bildung <- factor(data$bildung, ordered = FALSE)
data$haushaltseinkommen <- factor(data$haushaltseinkommen, ordered = FALSE)
data$alter <- as.numeric(data$alter)

data_only_wave_1 <- data
data_only_wave_1 <- data_only_wave_1[data$umfragewelle == 7,]
data_only_wave_1 <- data_only_wave_1[!is.na(data_only_wave_1$Populismusanfällige), ]
data_only_wave_1$Populismusanfällige <- factor(data_only_wave_1$Populismusanfällige, levels = c("Nein", "Ja"))

### save dataset for Stata code to build Figure 4

write_dta(data_only_wave_1, "data/data_populismus.dta") 

# Regressionen

model.n <- lm(LiRe ~ OpennessMittelwert + ConscientiousnessMittelwert + ExtraversionMittelwert + AgreeablenessMittelwert + NeuroticismMittelwert + alter + geschlecht + bildung + haushaltseinkommen + region,
              data = data_only_wave_1,
              weights = ipf_gewicht)

model.x <- glm(Populismusanfällige ~ OpennessMittelwert + ConscientiousnessMittelwert + ExtraversionMittelwert + AgreeablenessMittelwert + NeuroticismMittelwert + LiRe + alter + geschlecht + bildung + haushaltseinkommen + region,
               family = binomial(link = "logit"),
               data = data_only_wave_1,
               weights = ipf_gewicht)

summary(model.n)
summary(model.x)
tab_model(model.x)


# McFadden's Pseudo-R² mit gewichteten Daten
n_effective <- nobs(model.x)
cat("Anzahl einbezogener Subjekte:", n_effective, "\n")
ll_full <- logLik(model.x)
ll_null <- logLik(update(model.x, . ~ 1))
r2_mcfadden <- 1 - (as.numeric(ll_full) / as.numeric(ll_null))

cat("McFadden's Pseudo-R²:", round(r2_mcfadden, 3), "\n")

# Tjur's -R²
r2(model.x)


# Modell tidy machen und exponentiieren (Odds Ratios)
tidy_model_x <- tidy(model.x, conf.int = TRUE) %>%
  mutate(estimate = exp(estimate),      
         conf.low = exp(conf.low),  
         conf.high = exp(conf.high),       
         model = "Populismusanfälligkeit")            

# Ausgabe der Odds Ratios mit Konfidenzintervallen und p-Werten
print(tidy_model_x %>% dplyr::select(term, estimate, conf.low, conf.high, p.value))

# Mediationsanalysen
set.seed(123)
med.out_openness <- mediate(model.n, model.x,
                            treat = "OpennessMittelwert",
                            mediator = "LiRe",
                            boot = TRUE, sims = 1000,
                            cluster = data_only_wave_1$id3)

summary(med.out_openness)


set.seed(123)
med.out_conscientiousness <- mediate(model.n, model.x,
                                     treat = "ConscientiousnessMittelwert",
                                     mediator = "LiRe",
                                     boot = TRUE, sims = 1000,
                                     cluster = data_only_wave_1$id3)

summary(med.out_conscientiousness)


set.seed(123)
med.out_extraversion <- mediate(model.n, model.x,
                                treat = "ExtraversionMittelwert",
                                mediator = "LiRe",
                                boot = TRUE, sims = 1000,
                                cluster = data_only_wave_1$id3)

summary(med.out_extraversion)


set.seed(123)
med.out_agreeableness <- mediate(model.n, model.x,
                                 treat = "AgreeablenessMittelwert",
                                 mediator = "LiRe",
                                 boot = TRUE, sims = 1000,
                                 cluster = data_only_wave_1$id3)

summary(med.out_agreeableness)


set.seed(123)
med.out_neuroticism <- mediate(model.n, model.x,
                               treat = "NeuroticismMittelwert",
                               mediator = "LiRe",
                               boot = TRUE, sims = 1000,
                               cluster = data_only_wave_1$id3)

summary(med.out_neuroticism)


# Funktion zur Extraktion der Mediationseffekte aus einem mediate-Objekt
extract_mediation_results <- function(med.out, variable_label) {
  data.frame(
    variable = variable_label,
    effect = c("ACME", "ADE", "Total Effect"),
    estimate = c(med.out$d0, med.out$z0, med.out$tau.coef),
    ci.low = c(med.out$d0.ci[1], med.out$z0.ci[1], med.out$tau.ci[1]),
    ci.high = c(med.out$d0.ci[2], med.out$z0.ci[2], med.out$tau.ci[2])
  )
}

# Ergebnisse aus allen fünf Mediationen sammeln
results_list <- list(
  extract_mediation_results(med.out_openness, "Openness"),
  extract_mediation_results(med.out_conscientiousness, "Conscientiousness"),
  extract_mediation_results(med.out_extraversion, "Extraversion"),
  extract_mediation_results(med.out_agreeableness, "Agreeableness"),
  extract_mediation_results(med.out_neuroticism, "Neuroticism")
)

# Zusammenführen der Daten
results_df_plot2 <- bind_rows(results_list)

# In Odds Ratio umwandeln
results_df_table2 <- results_df_plot2 %>%
  mutate(
    estimate = exp(estimate),
    ci.low = exp(ci.low),
    ci.high = exp(ci.high)
  )

# Nur Tabelle in OR
print(results_df_table2)

# Richtige Reihenfolge
results_df_plot2$variable <- factor(results_df_plot2$variable, 
                                    levels = c("Neuroticism", "Agreeableness", "Extraversion", "Conscientiousness", "Openness"))

results_df_plot2$effect <- factor(results_df_plot2$effect, 
                                  levels = c("ACME", "ADE", "Total Effect"))


# PDF-Gerät öffnen
pdf(file = "figures/Figure A2.pdf", width = 10, height = 7)

# Plot
ggplot(results_df_plot2, aes(x = estimate, y = variable, color = effect)) +
  geom_point(position = position_dodge(width = 0.5), size = 2.5) +
  geom_errorbar(aes(xmin = ci.low, xmax = ci.high), 
                position = position_dodge(width = 0.5), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  scale_color_manual(
    values = c("ACME" = "#1b9e77", "ADE" = "#d95f02", "Total Effect" = "#7570b3"),
    labels = c("ACME (indirect effect)", "ADE (direct effect)", "Total effect")
  ) +
  labs(
    x = "Effect Estimate (log odds)",
    y = NULL,
    color = "Effect type"
  ) +
  theme_minimal(base_family = "Times") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 17),
    legend.text = element_text(size = 15),
    axis.title.x = element_text(vjust = -1, size = 15),
    axis.title.y = element_text(hjust = 0.5, size = 15),
    axis.text = element_text(size = 15)
  )

# PDF schließen
dev.off()




# ------------------------------------------------------------------------------

## Robustness check 2 for vote switching in general and susceptibility to populism - Control variables from survey wave 3

# 1. Einschluss/Ausschluss von Versuchspersonen

# Ausschluss der Versuchspersonen mit dispcode 22 zu allen Messzeitpunkten
person_ids_dispcode_22 <- unique(dataaaa$id3[dataaaa$dispcode == 22])
dataaa <- dataaaa[!(dataaaa$id3 %in% person_ids_dispcode_22), ]

# Ausschluss der Versuchspersonen mit mindestens einem fehlenden Wert in der Variable partei
person_ids_missing_partei <- unique(dataaa$id3[is.na(dataaa$partei)])
dataa <- dataaa[!(dataaa$id3 %in% person_ids_missing_partei), ]

# Ausschluss der Versuchspersonen mit mindestens einem Wert 16 in der Variable partei
person_ids_with_16 <- unique(dataa$id3[dataa$partei == 16])
data <- dataa[!(dataa$id3 %in% person_ids_with_16), ]

nrow(dataaaa)
nrow(dataaa)
nrow(dataa)
nrow(data)

# Ersetzen von NA in ipf_gewicht durch 1
data$ipf_gewicht[is.na(data$ipf_gewicht)] <- 1

n_distinct(dataaaa$id3)
n_distinct(dataaa$id3)
n_distinct(dataa$id3)
n_distinct(data$id3)



# 2. Vorbereitung der Variablen

# 2.1 Gewicht übertragen

# Extrahiere Gewicht pro id3 aus Umfragewelle 7
gewicht_w7 <- data %>%
  filter(umfragewelle == 7) %>%
  dplyr::select(id3, ipf_gewicht)

# Füge Gewicht über id3 an alle Zeilen im Datensatz an
data <- data %>%
  dplyr::select(-ipf_gewicht) %>% 
  left_join(gewicht_w7, by = "id3")


# 2.2 LiRe Average bilden und transfomireren
data <- data %>%
  filter(umfragewelle %in% c(7, 8, 9)) %>%     
  group_by(id3) %>%
  mutate(LiRe = (mean(linksrechts, na.rm = TRUE) - 1) / 10) %>% 
  ungroup()


# 2.3 Kontrollvariablen

# Region aus Bundesland ableiten
data <- data %>%
  mutate(region = case_when(
    bundesland %in% c(4, 8, 13, 14, 16) ~ "Osten",
    !is.na(bundesland) ~ "Westen",
    TRUE ~ NA_character_
  ))

# Geschlecht als Faktor mit Labels (männlich/weiblich), gleichzeitig numerisch speichern
data$geschlecht[data$geschlecht == 3] <- NA
data$geschlecht <- factor(data$geschlecht, levels = c(1, 2), labels = c("männlich", "weiblich"))
data$geschlecht_num <- as.numeric(data$geschlecht)  # 1 = männlich, 2 = weiblich

# Region: numerisch kodieren für Analysen (Osten = 0, Westen = 1)
data$region <- factor(data$region, levels = c("Osten", "Westen"))
data$region_num <- as.numeric(data$region) - 1  # Osten = 0, Westen = 1

# Haushaltseinkommen: ungültige Werte entfernen, umpolen und als numerisch lassen
data$haushaltseinkommen[data$haushaltseinkommen == 7] <- NA
data$haushaltseinkommen <- 6 - data$haushaltseinkommen
data$haushaltseinkommen <- as.numeric(data$haushaltseinkommen)

# Bildung: ungültige Werte entfernen, als ordinal behandeln (numerisch, aber kein Faktor)
data$bildung[data$bildung %in% c(1, 8, 9, 10)] <- NA
data$bildung <- as.numeric(data$bildung)

# Alter bleibt numerisch
data$alter <- as.numeric(data$alter)


summary(data[, c("geschlecht", "bildung", "haushaltseinkommen", "region", "alter")])



# 2.4 Variable Parteipräferenz bauen

# Umgestaltung der Variable partei und Neubenennung als Parteipräferenz
data <- data %>%
  mutate(Parteipräferenz = case_when(
    partei == 6 ~ 1,
    partei == 4 ~ 2,
    partei == 3 ~ 3,
    partei == 5 ~ 4,
    partei %in% c(2, 8) ~ 5,
    partei == 7 ~ 6,
    partei %in% c(10, 11, 12, 13, 14, 15) ~ 0,
    TRUE ~ partei
  )) %>%
  mutate(Parteipräferenz = factor(Parteipräferenz,
                                  levels = c(0, 1, 2, 3, 4, 5, 6),
                                  labels = c("Andere", "Linke", "Grüne", "SPD", "FDP", "Union", "AfD"),
                                  ordered = TRUE))

# Histogramm zur Häufigkeitsverteilung der neuen Variable Parteipräferenz - ungewichtet
pl1 <- ggplot(data, aes(x = factor(Parteipräferenz))) +
  geom_bar(fill = "purple", color = "black") +
  labs(title = "Häufigkeitsverteilung der Variable Parteipräferenz", 
       x = "Parteipräferenz", 
       y = "Häufigkeit") +
  scale_x_discrete(labels = c("Andere", "Linke", "Grüne", "SPD", "FDP", "Union", "AfD"))

print(pl1)

# Gewichtete Häufigkeiten berechnen
gewichtete_daten <- data %>%
  group_by(Parteipräferenz) %>%
  summarise(Gewicht = sum(ipf_gewicht)) %>%
  mutate(Parteipräferenz = factor(Parteipräferenz,
                                  levels = c("Andere", "Linke", "Grüne", "SPD", "FDP", "Union", "AfD")))

# Histogramm zur Häufigkeitsverteilung der neuen Variable Parteipräferenz - gewichtet
pl2 <- ggplot(gewichtete_daten, aes(x = Parteipräferenz, y = Gewicht)) +
  geom_bar(stat = "identity", fill = "orange", color = "black") +
  labs(title = "Gewichtete Verteilung der Parteipräferenz",
       x = "Parteipräferenz",
       y = "gewichtete Häufigkeit")


# 2.5 Persönlichkeitsvariablen bauen

# Umpolung der angegebenen Variablen
data$v_1280 <- 6 - data$v_1280
data$v_1282 <- 6 - data$v_1282
data$v_1283 <- 6 - data$v_1283
data$v_1284 <- 6 - data$v_1284
data$v_1286 <- 6 - data$v_1286

# Erstellen der neuen Variablen
data$Extraversion <- (data$v_1280 + data$v_1285)/2 
data$Agreeableness <- (data$v_1286 + data$v_1281)/2
data$Conscientiousness <- (data$v_1282 + data$v_1287)/2
data$Neuroticism <- (data$v_1283 + data$v_1288)/2
data$Openness <- (data$v_1284 + data$v_1289)/2

# Sicherstellen, dass die Variablen als intervallskaliert behandelt werden
data <- data %>%
  mutate(Extraversion = as.numeric(Extraversion),
         Agreeableness = as.numeric(Agreeableness),
         Conscientiousness = as.numeric(Conscientiousness),
         Neuroticism = as.numeric(Neuroticism),
         Openness = as.numeric(Openness))

# Berechnung und Skalierung der Big-Five-Mittelwerte pro Person (0 bis 1)
data <- data %>%
  group_by(id3) %>%
  mutate(
    ExtraversionMittelwert = (mean(Extraversion, na.rm = TRUE) - 1) / 4,
    AgreeablenessMittelwert = (mean(Agreeableness, na.rm = TRUE) - 1) / 4,
    ConscientiousnessMittelwert = (mean(Conscientiousness, na.rm = TRUE) - 1) / 4,
    NeuroticismMittelwert = (mean(Neuroticism, na.rm = TRUE) - 1) / 4,
    OpennessMittelwert = (mean(Openness, na.rm = TRUE) - 1) / 4
  ) %>%
  ungroup()



# 6. Hypothese H1a-H1d

# 6.1 Variable Wahlwechsel bauen

# Detaillierte Parteien Codierung
data <- data %>%
  mutate(Parteipräferenz = case_when(
    partei == 6 ~ 1,
    partei == 4 ~ 2,
    partei == 3 ~ 3,
    partei == 5 ~ 4,
    partei %in% c(2, 8) ~ 5,
    partei == 7 ~ 6,
    partei == 10 ~ 7,
    partei == 11 ~ 8,
    partei == 12 ~ 9,
    partei == 13 ~ 10,
    partei == 14 ~ 11,
    partei == 15 ~ 12,
    TRUE ~ partei
  )) %>%
  mutate(Parteipräferenz = factor(Parteipräferenz,
                                  levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                                  labels = c("Linke", "Grüne", "SPD", "FDP", "Union", "AfD", "Freie Wähler", "die PARTEI", "Tierschutzpartei", "dieBasis", "sonstige Partei", "Nichtwähler"),
                                  ordered = TRUE))



# Berechnung der Anzahl der Wechsel in der Variable Parteipräferenz pro Person über die Messzeitpunkte
data <- data %>%
  group_by(id3) %>%
  mutate(Anzahl_Wechsel = n_distinct(Parteipräferenz) - 1) %>%
  ungroup()

# Erstellung der Variable Wahlwechsel
data <- data %>%
  mutate(Wahlwechsel = factor(ifelse(Anzahl_Wechsel >= 1, 1, 0),
                              levels = c(0, 1),
                              labels = c("Nein", "Ja")))


# 6.1a Ungewichtete Prozentanteile
ungw_prozent <- data %>%
  dplyr::select(id3, Wahlwechsel) %>%
  distinct() %>%
  count(Wahlwechsel) %>%
  mutate(Prozent = round(100 * n / sum(n), 1))
ungw_prozent

# 6.1b Gewichtete Prozentanteile
gew_prozent <- data %>%
  dplyr::select(id3, Wahlwechsel, ipf_gewicht) %>%
  distinct() %>%
  group_by(Wahlwechsel) %>%
  summarise(Gewicht = sum(ipf_gewicht)) %>%
  mutate(Prozent = round(100 * Gewicht / sum(Gewicht), 1))
gew_prozent



# 6.2 t-Tests Wahlwechsel Gruppen

# Sicherstellen, dass alle nötigen Variablen vorhanden sind
data_filtered <- data %>%
  distinct(id3, .keep_all = TRUE) %>% 
  dplyr::select(Wahlwechsel, OpennessMittelwert, ConscientiousnessMittelwert,
                ExtraversionMittelwert, AgreeablenessMittelwert, NeuroticismMittelwert,
                ipf_gewicht)

# Survey-Design definieren
design <- svydesign(ids = ~1, weights = ~ipf_gewicht, data = data_filtered)

# Gewichteter t-Test (bzw. gewichtete lineare Regression)
ttest_openness <- svyglm(OpennessMittelwert ~ Wahlwechsel, design = design)
ttest_conscientiousness <- svyglm(ConscientiousnessMittelwert ~ Wahlwechsel, design = design)
ttest_extraversion <- svyglm(ExtraversionMittelwert ~ Wahlwechsel, design = design)
ttest_agreeableness <- svyglm(AgreeablenessMittelwert ~ Wahlwechsel, design = design)
ttest_neuroticism <- svyglm(NeuroticismMittelwert ~ Wahlwechsel, design = design)

summary_stats_weighted <- data_filtered %>%
  group_by(Wahlwechsel) %>%
  summarise(
    N = n(),
    M_Openness = weighted.mean(OpennessMittelwert, ipf_gewicht),
    SD_Openness = sqrt(Hmisc::wtd.var(OpennessMittelwert, ipf_gewicht)),
    
    M_Conscientiousness = weighted.mean(ConscientiousnessMittelwert, ipf_gewicht),
    SD_Conscientiousness = sqrt(Hmisc::wtd.var(ConscientiousnessMittelwert, ipf_gewicht)),
    
    M_Extraversion = weighted.mean(ExtraversionMittelwert, ipf_gewicht),
    SD_Extraversion = sqrt(Hmisc::wtd.var(ExtraversionMittelwert, ipf_gewicht)),
    
    M_Agreeableness = weighted.mean(AgreeablenessMittelwert, ipf_gewicht),
    SD_Agreeableness = sqrt(Hmisc::wtd.var(AgreeablenessMittelwert, ipf_gewicht)),
    
    M_Neuroticism = weighted.mean(NeuroticismMittelwert, ipf_gewicht),
    SD_Neuroticism = sqrt(Hmisc::wtd.var(NeuroticismMittelwert, ipf_gewicht))
  )

weighted_cohen_d <- function(x, group, weights) {
  group_levels <- unique(group)
  if (length(group_levels) != 2) stop("Nur zwei Gruppen erlaubt.")
  
  # Gruppierte Daten
  x1 <- x[group == group_levels[1]]
  w1 <- weights[group == group_levels[1]]
  x2 <- x[group == group_levels[2]]
  w2 <- weights[group == group_levels[2]]
  
  # Gewichtete Mittelwerte
  m1 <- weighted.mean(x1, w1)
  m2 <- weighted.mean(x2, w2)
  
  # Gewichtete Varianzen
  s1 <- Hmisc::wtd.var(x1, w1)
  s2 <- Hmisc::wtd.var(x2, w2)
  
  # Pooled SD
  pooled_sd <- sqrt((s1 + s2) / 2)
  
  # Cohen's d
  d <- (m1 - m2) / pooled_sd
  return(d)
}

# Cohen's D
weighted_cohen_d(data_filtered$OpennessMittelwert, data_filtered$Wahlwechsel, data_filtered$ipf_gewicht)
weighted_cohen_d(data_filtered$ConscientiousnessMittelwert, data_filtered$Wahlwechsel, data_filtered$ipf_gewicht)
weighted_cohen_d(data_filtered$ExtraversionMittelwert, data_filtered$Wahlwechsel, data_filtered$ipf_gewicht)
weighted_cohen_d(data_filtered$AgreeablenessMittelwert, data_filtered$Wahlwechsel, data_filtered$ipf_gewicht)
weighted_cohen_d(data_filtered$NeuroticismMittelwert, data_filtered$Wahlwechsel, data_filtered$ipf_gewicht)

summary(ttest_openness)
summary(ttest_conscientiousness)
summary(ttest_extraversion)
summary(ttest_agreeableness)
summary(ttest_neuroticism)

print(summary_stats_weighted, n = Inf, width = Inf)
cat("Anzahl der Fälle in der Berechnung:", nrow(data_filtered), "\n")



# 6.3 gewichtete Mediationsanalyse

#Skalenniveau explizit setzen
data$geschlecht <- as.factor(data$geschlecht)
data$region <- as.factor(data$region)
data$bildung <- factor(data$bildung, ordered = FALSE)
data$haushaltseinkommen <- factor(data$haushaltseinkommen, ordered = FALSE)
data$alter <- as.numeric(data$alter)

data_only_wave_1 <-data
data_only_wave_1 <- data_only_wave_1[data$umfragewelle == 9,]

### save dataset for Stata code to build Figure 4
write_dta(data_only_wave_1, "data/data_wahlwechsel.dta") 


# Regressionen

model.m <- lm(LiRe ~ OpennessMittelwert + ConscientiousnessMittelwert + ExtraversionMittelwert + AgreeablenessMittelwert + NeuroticismMittelwert + alter + geschlecht + bildung + haushaltseinkommen + region,
              data = data_only_wave_1,
              weights = ipf_gewicht)

model.y <- glm(Wahlwechsel ~ OpennessMittelwert + ConscientiousnessMittelwert + ExtraversionMittelwert + AgreeablenessMittelwert + NeuroticismMittelwert + LiRe + alter + geschlecht + bildung + haushaltseinkommen + region,
               family = binomial(link = "logit"),
               data = data_only_wave_1,
               weights = ipf_gewicht)

summary(model.m)
summary(model.y)
tab_model(model.y)


# McFadden's Pseudo-R² mit gewichteten Daten
n_effective <- nobs(model.y)
cat("Anzahl einbezogener Subjekte:", n_effective, "\n")
ll_full <- logLik(model.y)
ll_null <- logLik(update(model.y, . ~ 1))
r2_mcfadden <- 1 - (as.numeric(ll_full) / as.numeric(ll_null))

cat("McFadden's Pseudo-R²:", round(r2_mcfadden, 3), "\n")

# Tjur's -R²
r2(model.y)


# Modell tidy machen und exponentiieren (Odds Ratios)
tidy_model_y <- tidy(model.y, conf.int = TRUE) %>%
  mutate(estimate = exp(estimate),      
         conf.low = exp(conf.low),  
         conf.high = exp(conf.high),       
         model = "Wahlwechsel")            

# Ausgabe der Odds Ratios mit Konfidenzintervallen und p-Werten
print(tidy_model_y %>% dplyr::select(term, estimate, conf.low, conf.high, p.value))


# Mediationsanalysen
set.seed(123)
med.out_openness <- mediate(model.m, model.y,
                            treat = "OpennessMittelwert",
                            mediator = "LiRe",
                            boot = TRUE, sims = 1000,
                            cluster = data_only_wave_1$id3)

summary(med.out_openness)


set.seed(123)
med.out_conscientiousness <- mediate(model.m, model.y,
                                     treat = "ConscientiousnessMittelwert",
                                     mediator = "LiRe",
                                     boot = TRUE, sims = 1000,
                                     cluster = data_only_wave_1$id3)

summary(med.out_conscientiousness)


set.seed(123)
med.out_extraversion <- mediate(model.m, model.y,
                                treat = "ExtraversionMittelwert",
                                mediator = "LiRe",
                                boot = TRUE, sims = 1000,
                                cluster = data_only_wave_1$id3)

summary(med.out_extraversion)


set.seed(123)
med.out_agreeableness <- mediate(model.m, model.y,
                                 treat = "AgreeablenessMittelwert",
                                 mediator = "LiRe",
                                 boot = TRUE, sims = 1000,
                                 cluster = data_only_wave_1$id3)

summary(med.out_agreeableness)


set.seed(123)
med.out_neuroticism <- mediate(model.m, model.y,
                               treat = "NeuroticismMittelwert",
                               mediator = "LiRe",
                               boot = TRUE, sims = 1000,
                               cluster = data_only_wave_1$id3)

summary(med.out_neuroticism)



# Funktion zur Extraktion der Mediationseffekte aus einem mediate-Objekt
extract_mediation_results <- function(med.out, variable_label) {
  data.frame(
    variable = variable_label,
    effect = c("ACME", "ADE", "Total Effect"),
    estimate = c(med.out$d0, med.out$z0, med.out$tau.coef),
    ci.low = c(med.out$d0.ci[1], med.out$z0.ci[1], med.out$tau.ci[1]),
    ci.high = c(med.out$d0.ci[2], med.out$z0.ci[2], med.out$tau.ci[2])
  )
}

# Ergebnisse aus allen fünf Mediationen sammeln
results_list <- list(
  extract_mediation_results(med.out_openness, "Openness"),
  extract_mediation_results(med.out_conscientiousness, "Conscientiousness"),
  extract_mediation_results(med.out_extraversion, "Extraversion"),
  extract_mediation_results(med.out_agreeableness, "Agreeableness"),
  extract_mediation_results(med.out_neuroticism, "Neuroticism")
)

# Zusammenführen der Daten
results_df_plot <- bind_rows(results_list)

# In Odds Ratio umwandeln
results_df_table <- results_df_plot %>%
  mutate(
    estimate = exp(estimate),
    ci.low = exp(ci.low),
    ci.high = exp(ci.high)
  )

# Nur Tabelle in OR
print(results_df_table)

# Richtige Reihenfolge
results_df_plot$variable <- factor(results_df_plot$variable, 
                                   levels = c("Neuroticism", "Agreeableness", "Extraversion", "Conscientiousness", "Openness"))

results_df_plot$effect <- factor(results_df_plot$effect, 
                                 levels = c("ACME", "ADE", "Total Effect"))


# PDF-Gerät öffnen
pdf(file = "figures/Figure A3.pdf", width = 10, height = 7)

# Plot
ggplot(results_df_plot, aes(x = estimate, y = variable, color = effect)) +
  geom_point(position = position_dodge(width = 0.5), size = 2.5) +
  geom_errorbar(aes(xmin = ci.low, xmax = ci.high), 
                position = position_dodge(width = 0.5), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  scale_color_manual(
    values = c("ACME" = "#1b9e77", "ADE" = "#d95f02", "Total Effect" = "#7570b3"),
    labels = c("ACME (indirect effect)", "ADE (direct effect)", "Total effect")
  ) +
  labs(
    x = "Effect Estimate (log odds)",
    y = NULL,
    color = "Effect type"
  ) +
  theme_minimal(base_family = "Times") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 17),
    legend.text = element_text(size = 15),
    axis.title.x = element_text(vjust = -1, size = 15),
    axis.title.y = element_text(hjust = 0.5, size = 15),
    axis.text = element_text(size = 15)
  )

# PDF schließen
dev.off()



# 7. Hypthesen H2a-H2d

# 7.1 Variable "Populismusanfällige" bauen
data <- data %>%
  group_by(id3) %>%
  mutate(
    Populismusanfällige = case_when(
      any(partei[1] == 7) ~ NA_character_,  
      any(partei[2:3] == 7) ~ "Ja",        
      TRUE ~ "Nein"                       
    )
  ) %>%
  ungroup()

# Reduzieren auf eindeutige Personen mit gültigem Populismusstatus
data_populismus <- data %>%
  distinct(id3, .keep_all = TRUE) %>%
  filter(!is.na(Populismusanfällige))

# Proentuale Anteile für Populismusanfällige
# Ungewichtet
data_populismus %>%
  count(Populismusanfällige) %>%
  mutate(Prozent = round(n / sum(n) * 100, 2))
# Gewichtet
data_populismus %>%
  group_by(Populismusanfällige) %>%
  summarise(gewicht_sum = sum(ipf_gewicht)) %>%
  mutate(Prozent = round(gewicht_sum / sum(gewicht_sum) * 100, 2))



# 7.2 t-Test zu Populismusanfälligkeit

# Sicherstellen, dass alle nötigen Variablen vorhanden sind
data_filtered <- data %>%
  distinct(id3, .keep_all = TRUE) %>%  
  dplyr::select(Populismusanfällige, OpennessMittelwert, ConscientiousnessMittelwert,
                ExtraversionMittelwert, AgreeablenessMittelwert, NeuroticismMittelwert,
                ipf_gewicht)

# Bereinigen der Daten (nur zwei Gruppen)
data_filtered <- data_filtered %>%
  filter(Populismusanfällige %in% c("Ja", "Nein")) %>%
  droplevels()

# Survey-Design definieren
design <- svydesign(ids = ~1, weights = ~ipf_gewicht, data = data_filtered)

# Gewichteter t-Test (bzw. gewichtete lineare Regression)
ttest_openness <- svyglm(OpennessMittelwert ~ Populismusanfällige, design = design)
ttest_conscientiousness <- svyglm(ConscientiousnessMittelwert ~ Populismusanfällige, design = design)
ttest_extraversion <- svyglm(ExtraversionMittelwert ~ Populismusanfällige, design = design)
ttest_agreeableness <- svyglm(AgreeablenessMittelwert ~ Populismusanfällige, design = design)
ttest_neuroticism <- svyglm(NeuroticismMittelwert ~ Populismusanfällige, design = design)

summary_stats_weighted <- data_filtered %>%
  group_by(Populismusanfällige) %>%
  summarise(
    N = n(),
    M_Openness = weighted.mean(OpennessMittelwert, ipf_gewicht),
    SD_Openness = sqrt(Hmisc::wtd.var(OpennessMittelwert, ipf_gewicht)),
    
    M_Conscientiousness = weighted.mean(ConscientiousnessMittelwert, ipf_gewicht),
    SD_Conscientiousness = sqrt(Hmisc::wtd.var(ConscientiousnessMittelwert, ipf_gewicht)),
    
    M_Extraversion = weighted.mean(ExtraversionMittelwert, ipf_gewicht),
    SD_Extraversion = sqrt(Hmisc::wtd.var(ExtraversionMittelwert, ipf_gewicht)),
    
    M_Agreeableness = weighted.mean(AgreeablenessMittelwert, ipf_gewicht),
    SD_Agreeableness = sqrt(Hmisc::wtd.var(AgreeablenessMittelwert, ipf_gewicht)),
    
    M_Neuroticism = weighted.mean(NeuroticismMittelwert, ipf_gewicht),
    SD_Neuroticism = sqrt(Hmisc::wtd.var(NeuroticismMittelwert, ipf_gewicht))
  )

weighted_cohen_d <- function(x, group, weights) {
  group_levels <- unique(group)
  if (length(group_levels) != 2) stop("Nur zwei Gruppen erlaubt.")
  
  # Gruppierte Daten
  x1 <- x[group == group_levels[1]]
  w1 <- weights[group == group_levels[1]]
  x2 <- x[group == group_levels[2]]
  w2 <- weights[group == group_levels[2]]
  
  # Gewichtete Mittelwerte
  m1 <- weighted.mean(x1, w1)
  m2 <- weighted.mean(x2, w2)
  
  # Gewichtete Varianzen
  s1 <- Hmisc::wtd.var(x1, w1)
  s2 <- Hmisc::wtd.var(x2, w2)
  
  # Pooled SD
  pooled_sd <- sqrt((s1 + s2) / 2)
  
  # Cohen's d
  d <- (m1 - m2) / pooled_sd
  return(d)
}

# Cohen's D
weighted_cohen_d(data_filtered$OpennessMittelwert, data_filtered$Populismusanfällige, data_filtered$ipf_gewicht)
weighted_cohen_d(data_filtered$ConscientiousnessMittelwert, data_filtered$Populismusanfällige, data_filtered$ipf_gewicht)
weighted_cohen_d(data_filtered$ExtraversionMittelwert, data_filtered$Populismusanfällige, data_filtered$ipf_gewicht)
weighted_cohen_d(data_filtered$AgreeablenessMittelwert, data_filtered$Populismusanfällige, data_filtered$ipf_gewicht)
weighted_cohen_d(data_filtered$NeuroticismMittelwert, data_filtered$Populismusanfällige, data_filtered$ipf_gewicht)

summary(ttest_openness)
summary(ttest_conscientiousness)
summary(ttest_extraversion)
summary(ttest_agreeableness)
summary(ttest_neuroticism)

print(summary_stats_weighted, n = Inf, width = Inf)
cat("Anzahl der Fälle in der Berechnung:", nrow(data_filtered), "\n")





# 7.3 gewichtete Mediationsanalyse

#Skalenniveau explizit setzen
data$geschlecht <- as.factor(data$geschlecht)
data$region <- as.factor(data$region)
data$bildung <- factor(data$bildung, ordered = FALSE)
data$haushaltseinkommen <- factor(data$haushaltseinkommen, ordered = FALSE)
data$alter <- as.numeric(data$alter)

data_only_wave_1 <- data
data_only_wave_1 <- data_only_wave_1[data$umfragewelle == 9,]
data_only_wave_1 <- data_only_wave_1[!is.na(data_only_wave_1$Populismusanfällige), ]
data_only_wave_1$Populismusanfällige <- factor(data_only_wave_1$Populismusanfällige, levels = c("Nein", "Ja"))

### save dataset for Stata code to build Figure 4

write_dta(data_only_wave_1, "data/data_populismus.dta") 

# Regressionen

model.n <- lm(LiRe ~ OpennessMittelwert + ConscientiousnessMittelwert + ExtraversionMittelwert + AgreeablenessMittelwert + NeuroticismMittelwert + alter + geschlecht + bildung + haushaltseinkommen + region,
              data = data_only_wave_1,
              weights = ipf_gewicht)

model.x <- glm(Populismusanfällige ~ OpennessMittelwert + ConscientiousnessMittelwert + ExtraversionMittelwert + AgreeablenessMittelwert + NeuroticismMittelwert + LiRe + alter + geschlecht + bildung + haushaltseinkommen + region,
               family = binomial(link = "logit"),
               data = data_only_wave_1,
               weights = ipf_gewicht)

summary(model.n)
summary(model.x)
tab_model(model.x)


# McFadden's Pseudo-R² mit gewichteten Daten
n_effective <- nobs(model.x)
cat("Anzahl einbezogener Subjekte:", n_effective, "\n")
ll_full <- logLik(model.x)
ll_null <- logLik(update(model.x, . ~ 1))
r2_mcfadden <- 1 - (as.numeric(ll_full) / as.numeric(ll_null))

cat("McFadden's Pseudo-R²:", round(r2_mcfadden, 3), "\n")

# Tjur's -R²
r2(model.x)


# Modell tidy machen und exponentiieren (Odds Ratios)
tidy_model_x <- tidy(model.x, conf.int = TRUE) %>%
  mutate(estimate = exp(estimate),      
         conf.low = exp(conf.low),  
         conf.high = exp(conf.high),       
         model = "Populismusanfälligkeit")            

# Ausgabe der Odds Ratios mit Konfidenzintervallen und p-Werten
print(tidy_model_x %>% dplyr::select(term, estimate, conf.low, conf.high, p.value))

# Mediationsanalysen
set.seed(123)
med.out_openness <- mediate(model.n, model.x,
                            treat = "OpennessMittelwert",
                            mediator = "LiRe",
                            boot = TRUE, sims = 1000,
                            cluster = data_only_wave_1$id3)

summary(med.out_openness)


set.seed(123)
med.out_conscientiousness <- mediate(model.n, model.x,
                                     treat = "ConscientiousnessMittelwert",
                                     mediator = "LiRe",
                                     boot = TRUE, sims = 1000,
                                     cluster = data_only_wave_1$id3)

summary(med.out_conscientiousness)


set.seed(123)
med.out_extraversion <- mediate(model.n, model.x,
                                treat = "ExtraversionMittelwert",
                                mediator = "LiRe",
                                boot = TRUE, sims = 1000,
                                cluster = data_only_wave_1$id3)

summary(med.out_extraversion)


set.seed(123)
med.out_agreeableness <- mediate(model.n, model.x,
                                 treat = "AgreeablenessMittelwert",
                                 mediator = "LiRe",
                                 boot = TRUE, sims = 1000,
                                 cluster = data_only_wave_1$id3)

summary(med.out_agreeableness)


set.seed(123)
med.out_neuroticism <- mediate(model.n, model.x,
                               treat = "NeuroticismMittelwert",
                               mediator = "LiRe",
                               boot = TRUE, sims = 1000,
                               cluster = data_only_wave_1$id3)

summary(med.out_neuroticism)


# Funktion zur Extraktion der Mediationseffekte aus einem mediate-Objekt
extract_mediation_results <- function(med.out, variable_label) {
  data.frame(
    variable = variable_label,
    effect = c("ACME", "ADE", "Total Effect"),
    estimate = c(med.out$d0, med.out$z0, med.out$tau.coef),
    ci.low = c(med.out$d0.ci[1], med.out$z0.ci[1], med.out$tau.ci[1]),
    ci.high = c(med.out$d0.ci[2], med.out$z0.ci[2], med.out$tau.ci[2])
  )
}

# Ergebnisse aus allen fünf Mediationen sammeln
results_list <- list(
  extract_mediation_results(med.out_openness, "Openness"),
  extract_mediation_results(med.out_conscientiousness, "Conscientiousness"),
  extract_mediation_results(med.out_extraversion, "Extraversion"),
  extract_mediation_results(med.out_agreeableness, "Agreeableness"),
  extract_mediation_results(med.out_neuroticism, "Neuroticism")
)

# Zusammenführen der Daten
results_df_plot2 <- bind_rows(results_list)

# In Odds Ratio umwandeln
results_df_table2 <- results_df_plot2 %>%
  mutate(
    estimate = exp(estimate),
    ci.low = exp(ci.low),
    ci.high = exp(ci.high)
  )

# Nur Tabelle in OR
print(results_df_table2)

# Richtige Reihenfolge
results_df_plot2$variable <- factor(results_df_plot2$variable, 
                                    levels = c("Neuroticism", "Agreeableness", "Extraversion", "Conscientiousness", "Openness"))

results_df_plot2$effect <- factor(results_df_plot2$effect, 
                                  levels = c("ACME", "ADE", "Total Effect"))


# PDF-Gerät öffnen
pdf(file = "figures/Figure A4.pdf", width = 10, height = 7)

# Plot
ggplot(results_df_plot2, aes(x = estimate, y = variable, color = effect)) +
  geom_point(position = position_dodge(width = 0.5), size = 2.5) +
  geom_errorbar(aes(xmin = ci.low, xmax = ci.high), 
                position = position_dodge(width = 0.5), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  scale_color_manual(
    values = c("ACME" = "#1b9e77", "ADE" = "#d95f02", "Total Effect" = "#7570b3"),
    labels = c("ACME (indirect effect)", "ADE (direct effect)", "Total effect")
  ) +
  labs(
    x = "Effect Estimate (log odds)",
    y = NULL,
    color = "Effect type"
  ) +
  theme_minimal(base_family = "Times") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 17),
    legend.text = element_text(size = 15),
    axis.title.x = element_text(vjust = -1, size = 15),
    axis.title.y = element_text(hjust = 0.5, size = 15),
    axis.text = element_text(size = 15)
  )

# PDF schließen
dev.off()




# ------------------------------------------------------------------------------


## Robustness Check 3 for susceptibility to populism - control group: populism resistant switchers


# 1. Einschluss/Ausschluss von Versuchspersonen

# Ausschluss der Versuchspersonen mit dispcode 22 zu allen Messzeitpunkten
person_ids_dispcode_22 <- unique(dataaaa$id3[dataaaa$dispcode == 22])
dataaa <- dataaaa[!(dataaaa$id3 %in% person_ids_dispcode_22), ]

# Ausschluss der Versuchspersonen mit mindestens einem fehlenden Wert in der Variable partei
person_ids_missing_partei <- unique(dataaa$id3[is.na(dataaa$partei)])
dataa <- dataaa[!(dataaa$id3 %in% person_ids_missing_partei), ]

# Ausschluss der Versuchspersonen mit mindestens einem Wert 16 in der Variable partei
person_ids_with_16 <- unique(dataa$id3[dataa$partei == 16])
data <- dataa[!(dataa$id3 %in% person_ids_with_16), ]

nrow(dataaaa)
nrow(dataaa)
nrow(dataa)
nrow(data)

# Ersetzen von NA in ipf_gewicht durch 1
data$ipf_gewicht[is.na(data$ipf_gewicht)] <- 1

n_distinct(dataaaa$id3)
n_distinct(dataaa$id3)
n_distinct(dataa$id3)
n_distinct(data$id3)



# 2. Vorbereitung der Variablen

# 2.1 Gewicht übertragen

# Extrahiere Gewicht pro id3 aus Umfragewelle 7
gewicht_w7 <- data %>%
  filter(umfragewelle == 7) %>%
  dplyr::select(id3, ipf_gewicht)

# Füge Gewicht über id3 an alle Zeilen im Datensatz an
data <- data %>%
  dplyr::select(-ipf_gewicht) %>% 
  left_join(gewicht_w7, by = "id3")


# 2.2 LiRe Average bilden und transfomireren
data <- data %>%
  filter(umfragewelle %in% c(7, 8, 9)) %>%     
  group_by(id3) %>%
  mutate(LiRe = (mean(linksrechts, na.rm = TRUE) - 1) / 10) %>% 
  ungroup()



# 2.3 Kontrollvariablen

# Region aus Bundesland ableiten
data <- data %>%
  mutate(region = case_when(
    bundesland %in% c(4, 8, 13, 14, 16) ~ "Osten",
    !is.na(bundesland) ~ "Westen",
    TRUE ~ NA_character_
  ))

# Geschlecht als Faktor mit Labels (männlich/weiblich), gleichzeitig numerisch speichern
data$geschlecht[data$geschlecht == 3] <- NA
data$geschlecht <- factor(data$geschlecht, levels = c(1, 2), labels = c("männlich", "weiblich"))
data$geschlecht_num <- as.numeric(data$geschlecht)  # 1 = männlich, 2 = weiblich

# Region: numerisch kodieren für Analysen (Osten = 0, Westen = 1)
data$region <- factor(data$region, levels = c("Osten", "Westen"))
data$region_num <- as.numeric(data$region) - 1  # Osten = 0, Westen = 1

# Haushaltseinkommen: ungültige Werte entfernen, umpolen und als numerisch lassen
data$haushaltseinkommen[data$haushaltseinkommen == 7] <- NA
data$haushaltseinkommen <- 6 - data$haushaltseinkommen
data$haushaltseinkommen <- as.numeric(data$haushaltseinkommen)

# Bildung: ungültige Werte entfernen, als ordinal behandeln (numerisch, aber kein Faktor)
data$bildung[data$bildung %in% c(1, 8, 9, 10)] <- NA
data$bildung <- as.numeric(data$bildung)

# Alter bleibt numerisch
data$alter <- as.numeric(data$alter)


summary(data[, c("geschlecht", "bildung", "haushaltseinkommen", "region", "alter")])



# 2.4 Variable Parteipräferenz bauen

# Umgestaltung der Variable partei und Neubenennung als Parteipräferenz
data <- data %>%
  mutate(Parteipräferenz = case_when(
    partei == 6 ~ 1,
    partei == 4 ~ 2,
    partei == 3 ~ 3,
    partei == 5 ~ 4,
    partei %in% c(2, 8) ~ 5,
    partei == 7 ~ 6,
    partei %in% c(10, 11, 12, 13, 14, 15) ~ 0,
    TRUE ~ partei
  )) %>%
  mutate(Parteipräferenz = factor(Parteipräferenz,
                                  levels = c(0, 1, 2, 3, 4, 5, 6),
                                  labels = c("Andere", "Linke", "Grüne", "SPD", "FDP", "Union", "AfD"),
                                  ordered = TRUE))

# Histogramm zur Häufigkeitsverteilung der neuen Variable Parteipräferenz - ungewichtet
pl1 <- ggplot(data, aes(x = factor(Parteipräferenz))) +
  geom_bar(fill = "purple", color = "black") +
  labs(title = "Häufigkeitsverteilung der Variable Parteipräferenz", 
       x = "Parteipräferenz", 
       y = "Häufigkeit") +
  scale_x_discrete(labels = c("Andere", "Linke", "Grüne", "SPD", "FDP", "Union", "AfD"))

print(pl1)

# Gewichtete Häufigkeiten berechnen
gewichtete_daten <- data %>%
  group_by(Parteipräferenz) %>%
  summarise(Gewicht = sum(ipf_gewicht)) %>%
  mutate(Parteipräferenz = factor(Parteipräferenz,
                                  levels = c("Andere", "Linke", "Grüne", "SPD", "FDP", "Union", "AfD")))

# Histogramm zur Häufigkeitsverteilung der neuen Variable Parteipräferenz - gewichtet
pl2 <- ggplot(gewichtete_daten, aes(x = Parteipräferenz, y = Gewicht)) +
  geom_bar(stat = "identity", fill = "orange", color = "black") +
  labs(title = "Gewichtete Verteilung der Parteipräferenz",
       x = "Parteipräferenz",
       y = "gewichtete Häufigkeit")


# 2.5 Persönlichkeitsvariablen bauen

# Umpolung der angegebenen Variablen
data$v_1280 <- 6 - data$v_1280
data$v_1282 <- 6 - data$v_1282
data$v_1283 <- 6 - data$v_1283
data$v_1284 <- 6 - data$v_1284
data$v_1286 <- 6 - data$v_1286

# Erstellen der neuen Variablen
data$Extraversion <- (data$v_1280 + data$v_1285)/2 
data$Agreeableness <- (data$v_1286 + data$v_1281)/2
data$Conscientiousness <- (data$v_1282 + data$v_1287)/2
data$Neuroticism <- (data$v_1283 + data$v_1288)/2
data$Openness <- (data$v_1284 + data$v_1289)/2

# Sicherstellen, dass die Variablen als intervallskaliert behandelt werden
data <- data %>%
  mutate(Extraversion = as.numeric(Extraversion),
         Agreeableness = as.numeric(Agreeableness),
         Conscientiousness = as.numeric(Conscientiousness),
         Neuroticism = as.numeric(Neuroticism),
         Openness = as.numeric(Openness))

# Berechnung und Skalierung der Big-Five-Mittelwerte pro Person (0 bis 1)
data <- data %>%
  group_by(id3) %>%
  mutate(
    ExtraversionMittelwert = (mean(Extraversion, na.rm = TRUE) - 1) / 4,
    AgreeablenessMittelwert = (mean(Agreeableness, na.rm = TRUE) - 1) / 4,
    ConscientiousnessMittelwert = (mean(Conscientiousness, na.rm = TRUE) - 1) / 4,
    NeuroticismMittelwert = (mean(Neuroticism, na.rm = TRUE) - 1) / 4,
    OpennessMittelwert = (mean(Openness, na.rm = TRUE) - 1) / 4
  ) %>%
  ungroup()


# 6. Hypothese H1a-H1d

# 6.1 Variable Wahlwechsel bauen

# Detaillierte Parteien Codierung
data <- data %>%
  mutate(Parteipräferenz = case_when(
    partei == 6 ~ 1,
    partei == 4 ~ 2,
    partei == 3 ~ 3,
    partei == 5 ~ 4,
    partei %in% c(2, 8) ~ 5,
    partei == 7 ~ 6,
    partei == 10 ~ 7,
    partei == 11 ~ 8,
    partei == 12 ~ 9,
    partei == 13 ~ 10,
    partei == 14 ~ 11,
    partei == 15 ~ 12,
    TRUE ~ partei
  )) %>%
  mutate(Parteipräferenz = factor(Parteipräferenz,
                                  levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                                  labels = c("Linke", "Grüne", "SPD", "FDP", "Union", "AfD", "Freie Wähler", "die PARTEI", "Tierschutzpartei", "dieBasis", "sonstige Partei", "Nichtwähler"),
                                  ordered = TRUE))



# Berechnung der Anzahl der Wechsel in der Variable Parteipräferenz pro Person über die Messzeitpunkte
data <- data %>%
  group_by(id3) %>%
  mutate(Anzahl_Wechsel = n_distinct(Parteipräferenz) - 1) %>%
  ungroup()

# Erstellung der Variable Wahlwechsel
data <- data %>%
  mutate(Wahlwechsel = factor(ifelse(Anzahl_Wechsel >= 1, 1, 0),
                              levels = c(0, 1),
                              labels = c("Nein", "Ja")))


# 7. Hypthesen H2a-H2d

# 7.1 Variable "Populismusanfällige" bauen
data <- data %>%
  group_by(id3) %>%
  mutate(
    Populismusanfällige = case_when(
      any(partei[1] == 7) ~ NA_character_,  
      any(partei[2:3] == 7) ~ "Ja",        
      TRUE ~ "Nein"                       
    )
  ) %>%
  ungroup()

# Reduzieren auf eindeutige Personen mit gültigem Populismusstatus
data_populismus <- data %>%
  distinct(id3, .keep_all = TRUE) %>%
  filter(!is.na(Populismusanfällige))

# Proentuale Anteile für Populismusanfällige
# Ungewichtet
data_populismus %>%
  count(Populismusanfällige) %>%
  mutate(Prozent = round(n / sum(n) * 100, 2))
# Gewichtet
data_populismus %>%
  group_by(Populismusanfällige) %>%
  summarise(gewicht_sum = sum(ipf_gewicht)) %>%
  mutate(Prozent = round(gewicht_sum / sum(gewicht_sum) * 100, 2))

# Alternative: 7.1x Variable "Populismuswechsler"
data <- data %>%
  group_by(id3) %>%
  mutate(
    Populismuswechsler = case_when(
      Wahlwechsel == "Nein" ~ NA_character_,
      first(partei[umfragewelle == 7]) == 7 ~ NA_character_,  # Wenn in Welle 7 schon AfD → NA
      any(partei[umfragewelle %in% c(8,9)] == 7, na.rm = TRUE) ~ "Ja",  # Neu AfD ab Welle 8 oder 9
      TRUE ~ "Nein"
    )
  ) %>%
  ungroup()

# Reduzieren auf eindeutige Personen mit gültigem Populismuswechsler-Status
data_populismuswechsler <- data %>%
  distinct(id3, .keep_all = TRUE) %>%
  filter(!is.na(Populismuswechsler))

# Prozentuale Anteile für Populismuswechsler
# Ungewichtet
data_populismuswechsler %>%
  count(Populismuswechsler) %>%
  mutate(Prozent = round(n / sum(n) * 100, 2))
# Gewichtet
data_populismuswechsler %>%
  group_by(Populismuswechsler) %>%
  summarise(gewicht_sum = sum(ipf_gewicht)) %>%
  mutate(Prozent = round(gewicht_sum / sum(gewicht_sum) * 100, 2))


# 7.2x t-Test zu Populismuswechslern

# Sicherstellen, dass alle nötigen Variablen vorhanden sind
data_filtered <- data %>%
  distinct(id3, .keep_all = TRUE) %>%  
  dplyr::select(Populismuswechsler, OpennessMittelwert, ConscientiousnessMittelwert,
                ExtraversionMittelwert, AgreeablenessMittelwert, NeuroticismMittelwert,
                ipf_gewicht)

# Bereinigen der Daten (nur zwei Gruppen)
data_filtered <- data_filtered %>%
  filter(Populismuswechsler %in% c("Ja", "Nein")) %>%
  droplevels()

# Survey-Design definieren
design <- svydesign(ids = ~1, weights = ~ipf_gewicht, data = data_filtered)

# Gewichteter t-Test (bzw. gewichtete lineare Regression)
ttest_openness <- svyglm(OpennessMittelwert ~ Populismuswechsler, design = design)
ttest_conscientiousness <- svyglm(ConscientiousnessMittelwert ~ Populismuswechsler, design = design)
ttest_extraversion <- svyglm(ExtraversionMittelwert ~ Populismuswechsler, design = design)
ttest_agreeableness <- svyglm(AgreeablenessMittelwert ~ Populismuswechsler, design = design)
ttest_neuroticism <- svyglm(NeuroticismMittelwert ~ Populismuswechsler, design = design)

summary_stats_weighted <- data_filtered %>%
  group_by(Populismuswechsler) %>%
  summarise(
    N = n(),
    M_Openness = weighted.mean(OpennessMittelwert, ipf_gewicht),
    SD_Openness = sqrt(Hmisc::wtd.var(OpennessMittelwert, ipf_gewicht)),
    
    M_Conscientiousness = weighted.mean(ConscientiousnessMittelwert, ipf_gewicht),
    SD_Conscientiousness = sqrt(Hmisc::wtd.var(ConscientiousnessMittelwert, ipf_gewicht)),
    
    M_Extraversion = weighted.mean(ExtraversionMittelwert, ipf_gewicht),
    SD_Extraversion = sqrt(Hmisc::wtd.var(ExtraversionMittelwert, ipf_gewicht)),
    
    M_Agreeableness = weighted.mean(AgreeablenessMittelwert, ipf_gewicht),
    SD_Agreeableness = sqrt(Hmisc::wtd.var(AgreeablenessMittelwert, ipf_gewicht)),
    
    M_Neuroticism = weighted.mean(NeuroticismMittelwert, ipf_gewicht),
    SD_Neuroticism = sqrt(Hmisc::wtd.var(NeuroticismMittelwert, ipf_gewicht))
  )

weighted_cohen_d <- function(x, group, weights) {
  group_levels <- unique(group)
  if (length(group_levels) != 2) stop("Nur zwei Gruppen erlaubt.")
  
  # Gruppierte Daten
  x1 <- x[group == group_levels[1]]
  w1 <- weights[group == group_levels[1]]
  x2 <- x[group == group_levels[2]]
  w2 <- weights[group == group_levels[2]]
  
  # Gewichtete Mittelwerte
  m1 <- weighted.mean(x1, w1)
  m2 <- weighted.mean(x2, w2)
  
  # Gewichtete Varianzen
  s1 <- Hmisc::wtd.var(x1, w1)
  s2 <- Hmisc::wtd.var(x2, w2)
  
  # Pooled SD
  pooled_sd <- sqrt((s1 + s2) / 2)
  
  # Cohen's d
  d <- (m1 - m2) / pooled_sd
  return(d)
}

# Cohen's D
weighted_cohen_d(data_filtered$OpennessMittelwert, data_filtered$Populismuswechsler, data_filtered$ipf_gewicht)
weighted_cohen_d(data_filtered$ConscientiousnessMittelwert, data_filtered$Populismuswechsler, data_filtered$ipf_gewicht)
weighted_cohen_d(data_filtered$ExtraversionMittelwert, data_filtered$Populismuswechsler, data_filtered$ipf_gewicht)
weighted_cohen_d(data_filtered$AgreeablenessMittelwert, data_filtered$Populismuswechsler, data_filtered$ipf_gewicht)
weighted_cohen_d(data_filtered$NeuroticismMittelwert, data_filtered$Populismuswechsler, data_filtered$ipf_gewicht)

summary(ttest_openness)
summary(ttest_conscientiousness)
summary(ttest_extraversion)
summary(ttest_agreeableness)
summary(ttest_neuroticism)

print(summary_stats_weighted, n = Inf, width = Inf)
cat("Anzahl der Fälle in der Berechnung:", nrow(data_filtered), "\n")




# 7.3 gewichtete Mediationsanalyse

#Skalenniveau explizit setzen
data$geschlecht <- as.factor(data$geschlecht)
data$region <- as.factor(data$region)
data$bildung <- factor(data$bildung, ordered = FALSE)
data$haushaltseinkommen <- factor(data$haushaltseinkommen, ordered = FALSE)
data$alter <- as.numeric(data$alter)

data_only_wave_1 <- data
data_only_wave_1 <- data_only_wave_1[data$umfragewelle == 7,]
data_only_wave_1 <- data_only_wave_1[!is.na(data_only_wave_1$Populismuswechsler), ]
data_only_wave_1$Populismuswechsler <- factor(data_only_wave_1$Populismuswechsler, levels = c("Nein", "Ja"))

# Regressionen

model.n <- lm(LiRe ~ OpennessMittelwert + ConscientiousnessMittelwert + ExtraversionMittelwert + AgreeablenessMittelwert + NeuroticismMittelwert + alter + geschlecht + bildung + haushaltseinkommen + region,
              data = data_only_wave_1,
              weights = ipf_gewicht)

model.x <- glm(Populismuswechsler ~ OpennessMittelwert + ConscientiousnessMittelwert + ExtraversionMittelwert + AgreeablenessMittelwert + NeuroticismMittelwert + LiRe + alter + geschlecht + bildung + haushaltseinkommen + region,
               family = binomial(link = "logit"),
               data = data_only_wave_1,
               weights = ipf_gewicht)

summary(model.n)
summary(model.x)
tab_model(model.x)


# McFadden's Pseudo-R² mit gewichteten Daten
n_effective <- nobs(model.x)
cat("Anzahl einbezogener Subjekte:", n_effective, "\n")
ll_full <- logLik(model.x)
ll_null <- logLik(update(model.x, . ~ 1))
r2_mcfadden <- 1 - (as.numeric(ll_full) / as.numeric(ll_null))

cat("McFadden's Pseudo-R²:", round(r2_mcfadden, 3), "\n")

# Tjur's -R²
r2(model.x)


# Modell tidy machen und exponentiieren (Odds Ratios)
tidy_model_x <- tidy(model.x, conf.int = TRUE) %>%
  mutate(estimate = exp(estimate),      
         conf.low = exp(conf.low),  
         conf.high = exp(conf.high),       
         model = "Populismusanfälligkeit")            

# Ausgabe der Odds Ratios mit Konfidenzintervallen und p-Werten
print(tidy_model_x %>% dplyr::select(term, estimate, conf.low, conf.high, p.value))

# Mediationsanalysen
set.seed(123)
med.out_openness <- mediate(model.n, model.x,
                            treat = "OpennessMittelwert",
                            mediator = "LiRe",
                            boot = TRUE, sims = 1000,
                            cluster = data_only_wave_1$id3)

summary(med.out_openness)


set.seed(123)
med.out_conscientiousness <- mediate(model.n, model.x,
                                     treat = "ConscientiousnessMittelwert",
                                     mediator = "LiRe",
                                     boot = TRUE, sims = 1000,
                                     cluster = data_only_wave_1$id3)

summary(med.out_conscientiousness)


set.seed(123)
med.out_extraversion <- mediate(model.n, model.x,
                                treat = "ExtraversionMittelwert",
                                mediator = "LiRe",
                                boot = TRUE, sims = 1000,
                                cluster = data_only_wave_1$id3)

summary(med.out_extraversion)


set.seed(123)
med.out_agreeableness <- mediate(model.n, model.x,
                                 treat = "AgreeablenessMittelwert",
                                 mediator = "LiRe",
                                 boot = TRUE, sims = 1000,
                                 cluster = data_only_wave_1$id3)

summary(med.out_agreeableness)


set.seed(123)
med.out_neuroticism <- mediate(model.n, model.x,
                               treat = "NeuroticismMittelwert",
                               mediator = "LiRe",
                               boot = TRUE, sims = 1000,
                               cluster = data_only_wave_1$id3)

summary(med.out_neuroticism)


# Funktion zur Extraktion der Mediationseffekte aus einem mediate-Objekt
extract_mediation_results <- function(med.out, variable_label) {
  data.frame(
    variable = variable_label,
    effect = c("ACME", "ADE", "Total Effect"),
    estimate = c(med.out$d0, med.out$z0, med.out$tau.coef),
    ci.low = c(med.out$d0.ci[1], med.out$z0.ci[1], med.out$tau.ci[1]),
    ci.high = c(med.out$d0.ci[2], med.out$z0.ci[2], med.out$tau.ci[2])
  )
}

# Ergebnisse aus allen fünf Mediationen sammeln
results_list <- list(
  extract_mediation_results(med.out_openness, "Openness"),
  extract_mediation_results(med.out_conscientiousness, "Conscientiousness"),
  extract_mediation_results(med.out_extraversion, "Extraversion"),
  extract_mediation_results(med.out_agreeableness, "Agreeableness"),
  extract_mediation_results(med.out_neuroticism, "Neuroticism")
)

# Zusammenführen der Daten
results_df_plot <- bind_rows(results_list)

# In Odds Ratio umwandeln
results_df_table <- results_df_plot %>%
  mutate(
    estimate = exp(estimate),
    ci.low = exp(ci.low),
    ci.high = exp(ci.high)
  )

# Nur Tabelle in OR
print(results_df_table)

# Richtige Reihenfolge
results_df_plot$variable <- factor(results_df_plot$variable, 
                                   levels = c("Neuroticism", "Agreeableness", "Extraversion", "Conscientiousness", "Openness"))

results_df_plot$effect <- factor(results_df_plot$effect, 
                                 levels = c("ACME", "ADE", "Total Effect"))

# PDF-Gerät öffnen
pdf(file = "figures/Figure A5.pdf", width = 10, height = 7)

# Plot
ggplot(results_df_plot, aes(x = estimate, y = variable, color = effect)) +
  geom_point(position = position_dodge(width = 0.5), size = 2.5) +
  geom_errorbar(aes(xmin = ci.low, xmax = ci.high), 
                position = position_dodge(width = 0.5), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  scale_color_manual(
    values = c("ACME" = "#1b9e77", "ADE" = "#d95f02", "Total Effect" = "#7570b3"),
    labels = c("ACME (indirect effect)", "ADE (direct effect)", "Total effect")
  ) +
  labs(
    x = "Effect Estimate (log odds)",
    y = NULL,
    color = "Effect type"
  ) +
  theme_minimal(base_family = "Times") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 17),
    legend.text = element_text(size = 15),
    axis.title.x = element_text(vjust = -1, size = 15),
    axis.title.y = element_text(hjust = 0.5, size = 15),
    axis.text = element_text(size = 15)
  )

# PDF schließen
dev.off()