##################
#### Stats II ####
##################

###############################
#### Tutorial 2: GLMs ####
###############################

# In today's tutorial, we'll begin to explore GLMs
#     1. Import/wrangle data
#     2. Execute lm() and glm() of RQ
#     3. Compare models

#### Case study
# We're interested in central bank governors, specifically their occupational turnover, for almost all countries in the world starting from the year 1970

#### Creat the dataset
# For this task, we first need data.
# 1. Go to https://kof.ethz.ch/en/data/data-on-central-bank-governors.html and download the data on Central Bank Governors
# https://ethz.ch/content/dam/ethz/special-interest/dual/kof-dam/documents/central_bank_governors/cbg_turnover_v23upload.xlsx
# 2. Gather necessary variables
#    codewdi: Country code or name
#    year
#    time to regular turnover	
#    regular turnover dummy	
#    irregular turnover dummy	
#    legal duration

# MAKE SURE THERE AREN'T MISSING VALUES!

# Now, you've got your dataset

#### Import the data
# Your csv file should now be in the desktop folder. Before opening it, we're going to
# load in uour libraries

## loading the data
data <- 

#### Wrangling the data
# We should now have a dataset where our variables are at least of the correct type
# However, we need to do a bit of tidying to get the data into a more user-friendly
# format. 
  
#### Descriptive patterns in turnover
# Compute the average turnover rate (mean of turnover) by country over the full sample period

# (a) Which five countries have the highest average turnover rates?
  
# (b) Which five have the lowest average turnover rates?
  
# (c) Plot the distribution of country‑level average turnover rates (e.g. histogram or density) 
#     Briefly comment on whether high turnover is concentrated in a small set of countries

####  Estimate a linear probability model (LPM) with OLS:
  
# (a) Fit lm() with:
  # Outcome: irregular turnover dummy
  # Covariates: 
  #   time to regular turnover	
  #   legal duration

# (b) For a “typical” observation  (e.g. median time to regular turnover & legal duration), compute the predicted probability
  
# (c) Identify at least one observation for which lm() prediction is below 0 or above 1 and explain why such predictions are problematic for a probability

# Using the full sample, construct a plot of predicted probability of turnover vs time to regular turnover:
  
#### Baseline logistic regression
  
# Estimate a logistic regression with governor turnover as the binary outcome and same covariates using glm(family = "binomial")
  
# (a) Report coefficient estimates and standard errors

# (b) Interpret the sign of each coefficient in terms of how they affect the probability of turnover

# (c) For the same “typical” observation used above, compute the predicted probability of turnover (type = "response"), and compare it to the lm() prediction

#### Compare lm() and glm()  

# (a) Use the lm() to compute fitted values across the observed range of time to regular turnoner, holding legal duration at median value

# (b) Use the logit model to compute fitted probabilities for the same legal duration values

# (c) Plot both curves on the same graph (e.g. blue for lm(), red for glm()) 
  
#### Country heterogeneity and fixed effects

# (a) Introduce country fixed effects into the logit specification using dummy variables 

# (b) Compare the estimated coefficients with and without country fixed effects. How does controlling for unobserved country characteristics affect the relationships w/ turnover?
  
# (c) What kinds of country‑specific factors might be absorbed by these fixed effects in this context
