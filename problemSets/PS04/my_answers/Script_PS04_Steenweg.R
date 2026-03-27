### Question 1
library(eha)
library(survival)
library(stargazer)

# Load dataset
data(child)

# Inspect data
head(child)
str(child)
summary(child)

# Fit Cox proportional hazards model
cox_model <- coxph(Surv(enter, exit, event) ~ m.age + sex, data = child)
summary(cox_model)

stargazer(
  cox_model,
  type = "latex",
  title = "Cox Proportional Hazards Model for Child Mortality",
  dep.var.labels = "Hazard of death before age 15",
  covariate.labels = c("Mother's age", "Female"),
  digits = 3,
  no.space = TRUE,
  star.cutoffs = c(0.05, 0.01, 0.001),
  out = "cox_model_table.tex"
)

### Question 2
setwd("C:/Users/onnos/Documents/GitHub/StatsII_2026/problemSets/PS04/my_answers")

# load dataset
disaster_response <- read.csv("disaster_response.csv")

# Inspect
head(disaster_response)
str(disaster_response)
summary(disaster_response)

library(sampleSelection)

# Heckman selection model
heckman_model <- selection(
  selection = binContribution ~ occurrences + deathsEM + normalizedDamageEMLogged,
  outcome   = originalContributionMillionUSDLogged ~ occurrences + deathsEM + normalizedDamageEMLogged,
  data = disaster_response,
  method = "ml")

# View results
summary(heckman_model)

stargazer(
  heckman_model,
  type = "latex",
  title = "Heckman Selection Model for Disaster Aid",
  out = "heckman_table.tex"
)
