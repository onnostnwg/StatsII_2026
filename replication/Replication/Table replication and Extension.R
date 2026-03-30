##############################################################################
################ TABLE REPLICATION ###########################################

rm(list = ls())

setwd("C:/Users/onnos/Desktop/TRINITY/PhD/Quantitative Methods II/Replication Paper/Replication")

# packages
library(haven)
library(dplyr)
library(ggplot2)
library(patchwork)

# =========================================================
# PART 1: Vote switching (ww): Stata section 1
# =========================================================

df1 <- read_dta("data/data_wahlwechsel.dta") %>%
  mutate(
    ww = case_when(
      Wahlwechsel == 1 ~ 0,
      Wahlwechsel == 2 ~ 1,
      TRUE ~ NA_real_
    ),
    geschlecht = factor(geschlecht),
    bildung = factor(bildung),
    haushaltseinkommen = factor(haushaltseinkommen),
    region = factor(region)
  ) %>%
  # important: match Stata estimation sample (complete cases)
  filter(complete.cases(
    ww, OpennessMittelwert, ConscientiousnessMittelwert, ExtraversionMittelwert,
    AgreeablenessMittelwert, NeuroticismMittelwert, LiRe, alter,
    geschlecht, bildung, haushaltseinkommen, region, ipf_gewicht
  ))

m1 <- glm(
  ww ~ OpennessMittelwert + ConscientiousnessMittelwert + ExtraversionMittelwert +
    AgreeablenessMittelwert + NeuroticismMittelwert + LiRe + alter +
    geschlecht + bildung + haushaltseinkommen + region,
  data = df1,
  family = binomial(link = "logit"),
  weights = ipf_gewicht
)

# =========================================================
# PART 2: Populism (pop): Stata section 2
# =========================================================

df2 <- read_dta("data/data_populismus.dta") %>%
  mutate(
    pop = case_when(
      Populismusanfällige == 1 ~ 0,
      Populismusanfällige == 2 ~ 1,
      TRUE ~ NA_real_),
    geschlecht = factor(geschlecht),
    bildung = factor(bildung),
    haushaltseinkommen = factor(haushaltseinkommen),
    region = factor(region)
  ) %>%
  filter(complete.cases(
    pop, OpennessMittelwert, ConscientiousnessMittelwert, ExtraversionMittelwert,
    AgreeablenessMittelwert, NeuroticismMittelwert, LiRe, alter,
    geschlecht, bildung, haushaltseinkommen, region, ipf_gewicht))

m2 <- glm(
  pop ~ OpennessMittelwert + ConscientiousnessMittelwert + ExtraversionMittelwert +
    AgreeablenessMittelwert + NeuroticismMittelwert + LiRe + alter +
    geschlecht + bildung + haushaltseinkommen + region,
  data = df2,
  family = binomial(link = "logit"),
  weights = ipf_gewicht)

labels_en <- c("(Intercept)" = "Intercept",
  
  # Big Five
  "OpennessMittelwert" = "Openness",
  "ConscientiousnessMittelwert" = "Conscientiousness",
  "ExtraversionMittelwert" = "Extraversion",
  "AgreeablenessMittelwert" = "Agreeableness",
  "NeuroticismMittelwert" = "Neuroticism",
  
  # Controls
  "LiRe" = "Left-right self-placement (0 = left, 1 = right)",
  "alter" = "Age",
  "geschlecht2" = "Gender (0 = male, 1 = female)",
  "region2" = "Region (0 = East Germany; 1 = West Germany)",
  
  # Education (ref: ISCED 0–2)
  "bildung2" = "Education: ISCED 3",
  "bildung3" = "Education: ISCED 4",
  "bildung4" = "Education: ISCED 5–8",
  
  # Income (ref: I find it very difficult to make ends meet)
  "haushaltseinkommen2" = "Income: I find it difficult to make ends meet",
  "haushaltseinkommen3" = "Income: I find it fairly difficult to make ends meet",
  "haushaltseinkommen4" = "Income: I find it fairly easy to make ends meet",
  "haushaltseinkommen5" = "Income: I find it easy to make ends meet")

sig <- function(p) ifelse(p<.001,"***",
                          ifelse(p<.01,"**",
                                 ifelse(p<.05,"*","")))

make_a_table <- function(m, labels_en, caption){
  
  df <- broom::tidy(m) %>%
    dplyr::mutate(
      Variable = ifelse(term %in% names(labels_en), labels_en[term], term),
      stars = sig(p.value),
      `Coefficient (Standard Error)` = sprintf("%.4f (%.4f) %s", estimate, std.error, stars),
      OR = exp(estimate),
      lo = exp(estimate - 1.96*std.error),
      hi = exp(estimate + 1.96*std.error),
      `Odds Ratio (95\\% CI)` = sprintf("%.2f (%.2f--%.2f) %s", OR, lo, hi, stars)
    ) %>%
    dplyr::select(Variable, `Coefficient (Standard Error)`, `Odds Ratio (95\\% CI)`)
  
  # section headers like in the appendix
  big5 <- c("Openness","Conscientiousness","Extraversion","Agreeableness","Neuroticism")
  
  df2 <- df %>%
    dplyr::mutate(section = dplyr::case_when(
      Variable %in% big5 ~ "Big Five traits",
      Variable == "Intercept" ~ "Intercept",
      TRUE ~ "Controls"
    )) %>%
    dplyr::group_by(section) %>%
    dplyr::group_modify(~{
      if (.y$section %in% c("Big Five traits","Controls")) {
        dplyr::bind_rows(
          dplyr::tibble(
            Variable = .y$section,
            `Coefficient (Standard Error)` = "",
            `Odds Ratio (95\\% CI)` = ""
          ),
          .x
        )
      } else {
        .x
      }
    }) %>%
    dplyr::ungroup() %>%
    dplyr::select(-section)
  
  print(
    xtable(df2, caption = caption, align = c("l","p{7cm}","l","l")),
    include.rownames = FALSE,
    sanitize.text.function = identity,
    comment = FALSE)}

# --- create and print tables ---
make_a_table(m1, labels_en, "Table A4. Logistic regression model M1: Vote switching in general")
make_a_table(m2, labels_en, "Table A6. Logistic regression model M2: Vote switching to AfD")


#####################################################################################################
#################################  EXTENSION  #######################################################


# =========================================================
# Extension 1: Model Assumptions
# =========================================================

vif(m1)
vif(m2)

p1 <- ggplot(df1, aes(OpennessMittelwert, ww)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(x = "Openness", y = "Vote switching (general)")

p2 <- ggplot(df1, aes(ConscientiousnessMittelwert, ww)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(x = "Conscientiousness", y = NULL)

p3 <- ggplot(df1, aes(ExtraversionMittelwert, ww)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(x = "Extraversion", y = NULL)

p4 <- ggplot(df1, aes(AgreeablenessMittelwert, ww)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(x = "Agreeableness", y = NULL)

p5 <- ggplot(df1, aes(NeuroticismMittelwert, ww)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(x = "Neuroticism", y = NULL)

p6 <- ggplot(df2, aes(OpennessMittelwert, pop)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(x = "Openness", y = "Vote switching (AfD)")

p7 <- ggplot(df2, aes(ConscientiousnessMittelwert, pop)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(x = "Conscientiousness", y = NULL)

p8 <- ggplot(df2, aes(ExtraversionMittelwert, pop)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(x = "Extraversion", y = NULL)

p9 <- ggplot(df2, aes(AgreeablenessMittelwert, pop)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(x = "Agreeableness", y = NULL)

p10 <- ggplot(df2, aes(NeuroticismMittelwert, pop)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(x = "Neuroticism", y = NULL)

fig <- (p1 | p2 | p3| p4 | p5) /
  (p6 | p7 | p8| p9 | p10) +
  plot_annotation(
    title = "Linearity of the Logit Check",
    theme = theme(plot.title = element_text(hjust = 0.5))
  )

print(fig)
ggsave("figures/linearity_check.pdf", fig, width = 12, height = 3)


# =========================================================
# Extension 2: Interaction Effects
# =========================================================

m1_int <- glm(
  ww ~ OpennessMittelwert * LiRe +
    ConscientiousnessMittelwert + ExtraversionMittelwert +
    AgreeablenessMittelwert + NeuroticismMittelwert +
    alter + geschlecht + bildung + haushaltseinkommen + region,
  data = df1,
  family = binomial(link = "logit"),
  weights = ipf_gewicht
)
summary(m1_int)
pred <- ggpredict(m1_int,
                  terms = c("OpennessMittelwert",
                            "LiRe [0.0,0.25,0.5,0.75,1]"))

plot(pred) +
  labs(
    title = "Interaction between Openness and Ideological Self-Placement",
    x = "Openness",
    y = "Predicted probability of vote switching",
    color = "Ideological 
self-placement"
  )+ 
  scale_color_discrete(labels = c("Left","Center Left", "Center", "Center Right", "Right")) +
theme(
  plot.title = element_text(hjust = 0.5, size = 16),
  axis.title = element_text(size = 14),
  axis.text = element_text(size = 12),
  legend.title = element_text(size = 14),
  legend.text = element_text(size = 12))


# =========================================================
# Extension 3: Added Variable
# =========================================================

df2 <- read_dta("data/data_populismus.dta") %>%
  mutate(
    pop = case_when(
      Populismusanfällige == 1 ~ 0,
      Populismusanfällige == 2 ~ 1,
      TRUE ~ NA_real_),
    geschlecht = factor(geschlecht),
    bildung = factor(bildung),
    haushaltseinkommen = factor(haushaltseinkommen),
    region = factor(region)
  ) %>%
  filter(complete.cases(
    pop, OpennessMittelwert, ConscientiousnessMittelwert, ExtraversionMittelwert,
    AgreeablenessMittelwert, NeuroticismMittelwert, LiRe, alter,
    geschlecht, bildung, haushaltseinkommen, region, ipf_gewicht))


df2 <- df2 %>%
  mutate(
    GALTAN = (galtan - 1) / 10   # 0 = GAL, 1 = TAN
  )

m2_galtan <- glm(
  pop ~ OpennessMittelwert + ConscientiousnessMittelwert + ExtraversionMittelwert +
    AgreeablenessMittelwert + NeuroticismMittelwert +
    LiRe + GALTAN + alter +
    geschlecht + bildung + haushaltseinkommen + region,
  data = df2,
  family = binomial(link = "logit"),
  weights = ipf_gewicht
)
summary(m2_galtan)

library(stargazer)


anova(m2, m2_galtan, test = "Chisq")

install.packages("MASS")
library(MASS)

# Stepwise AIC model selection starting from the full model
m2_selected <- stepAIC(
  m2_galtan,
  direction = "both",   # allow adding/removing variables
  trace = TRUE
)

summary(m2_selected)

AIC(m2, m2_galtan, m2_selected)

stargazer(
  m2, m2_galtan, m2_selected,
  type = "latex",
  column.labels = c("M2", "M2 + GAL-TAN", "Selected Model"),
  dep.var.labels = "Susceptibility to populism",
  covariate.labels = c(
    "Openness",
    "Conscientiousness",
    "Extraversion",
    "Agreeableness",
    "Neuroticism",
    "Left-right self-placement",
    "GAL–TAN",
    "Age",
    "Gender (female)",
    "Education: ISCED 3",
    "Education: ISCED 4",
    "Education: ISCED 5–8",
    "Income: difficult to make ends meet",
    "Income: fairly difficult to make ends meet",
    "Income: fairly easy to make ends meet",
    "Income: easy to make ends meet",
    "Region (West Germany)"
  )
)





