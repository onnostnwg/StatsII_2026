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


###########################################################################################################

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
