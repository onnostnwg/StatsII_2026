#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_2026/blob/main/datasets/climateSupport.RData?raw=true"))



## Check structure
str(climateSupport) #all good

## Fit model
additive_model <- glm(choice ~ countries + sanctions,
                      data = climateSupport,
                      family = binomial)

summary(additive_model)

## Create table 
library(stargazer)
stargazer(additive_model,
          type = "latex",
          title = "Additive Logistic Regression Model",
          label = "tab:additive_model",
          dep.var.labels = "Support for Policy (1 = Yes)",
          covariate.labels = c("Countries (Linear)",
                               "Countries (Quadratic)",
                               "Sanctions (Linear)",
                               "Sanctions (Quadratic)",
                               "Sanctions (Cubic)"),
          digits = 3,
          out = "additive_model.tex")


## Fit null model for comparison
null_model <- glm(choice ~ 1, data = climateSupport, family = binomial)
anova(null_model, additive_model, test="Chisq")


library(dplyr)

## 1. Odds ratio: 160 countries, sanctions 5% -> 15%

rows_160_5 <- climateSupport %>% 
  filter(countries == "160 of 192", sanctions == "5%")
rows_160_15 <- climateSupport %>% 
  filter(countries == "160 of 192", sanctions == "15%")

logit_160_5 <- predict(additive_model, newdata = rows_160_5, type="link")
logit_160_15 <- predict(additive_model, newdata = rows_160_15, type="link")

# Average logit for each group, then compute odds ratio
odds_ratio_160 <- exp(mean(logit_160_15) - mean(logit_160_5))


## 2. Odds ratio: 20 countries, sanctions 5% -> 15%
rows_20_5 <- climateSupport %>% 
  filter(countries == "20 of 192", sanctions == "5%")
rows_20_15 <- climateSupport %>% 
  filter(countries == "20 of 192", sanctions == "15%")

logit_20_5 <- predict(additive_model, newdata = rows_20_5, type="link")
logit_20_15 <- predict(additive_model, newdata = rows_20_15, type="link")

odds_ratio_20 <- exp(mean(logit_20_15) - mean(logit_20_5))


## 3. Estimated probability: 80 countries, no sanctions
rows_80_none <- climateSupport %>% 
  filter(countries == "80 of 192", sanctions == "None")

prob_80_none <- mean(predict(additive_model, newdata = rows_80_none, type="response"))


## 4. Results
odds_ratio_160
odds_ratio_20
prob_80_none


## Interaction Effect
interaction_model <- glm(choice ~ countries * sanctions, 
                         data = climateSupport, 
                         family = binomial)

anova(additive_model, interaction_model, test = "Chisq")
