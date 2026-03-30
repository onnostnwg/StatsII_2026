# Vector of required packages
packages <- c(
  "haven",
  "ggplot2",
  "dplyr",
  "effsize",
  "broom",
  "pscl",
  "tidyr",
  "car",
  "lme4",
  "performance",
  "reshape2",
  "nnet",
  "effectsize",
  "sjPlot",
  "tidyverse",
  "ggalluvial",
  "mediation",
  "Hmisc",
  "survey"
)

# Install missing packages
installed_packages <- rownames(installed.packages())

for (pkg in packages) {
  if (!pkg %in% installed_packages) {
    install.packages(pkg, dependencies = TRUE)
  }
}

# Load packages
lapply(packages, library, character.only = TRUE)