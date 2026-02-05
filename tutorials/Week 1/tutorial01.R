##################
#### Stats II ####
##################

###############################
#### Tutorial 1: Refresher ####
###############################

# Today's tutorial is a refesher of the R skills we learnt in the first semester.
#     1. Importing data
#     2. Wrangling data
#     3. Analysing data
#     4. Communicating

#### Case study
# A fictional think-tank, the Alliance of Wealthy People who Dislike Tax, has asked
# you to study the relationship between tax, regulation and GDP per capita. They believe
# that countries with low tax and light regulation are wealthier, and they want you to 
# prove it using statistics!

#### Creating the dataset
# For this task, we need data. The World Bank has the information we need.

# 1. Go to https://databank.worldbank.org/source/world-development-indicators#
# 2. In the "add country" screen, select the region "Europe and Central Asia", then 
#    click "select all". Close the pop-up.
# 3. In the "add series" screen, select "Ease of Doing Business Rank", "GDP per capita 
#    (current US$)" and "tax revenue (% of GDP)". Tip: you can use the letters to skip
#    to the right section. Close the pop-up.
# 4. In the "add time" screen, select the year 2019.
# 5. Click "apply changes".
# 6. On the left side of the screen, click the "layout" option.
# 7. Change "country" to "row". Click "apply changes".
# 8. Change "series" to "column". Click "apply changes".
# 9. Change "time" to "page". Click "apply changes".
# 10. In "download options" in the top right, choose "CSV". Save to desktop.
# 11. Open the folder on the desktop, click "extract all". Extract to desktop.

# Well done, you've got your dataset!

#### Importing the data
# Your csv file should now be in the desktop folder. Before opening it, we're going to
# load in our libraries.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

library(tidyverse)
library(stargazer)

## loading the data
data <- read_csv("tutorial01_data.csv")
View(data)

str(data)

data <- read_csv("tutorial01_data.csv",
                 col_types = cols(
                   `Ease of doing business rank (1=most business-friendly regulations)` = col_double(),
                   `Tax revenue (% of GDP)`= col_double(), 
                   `GDP per capita (current US$)`= col_double()))

str(data)

# [IC.BUS.EASE.XQ]
# [GC.TAX.TOTL.GD.ZS]
# [NY.GDP.PCAP.CD]

#### Wrangling the data
# We should now have a dataset where our variables are at least of the correct type.
# However, we need to do a bit of tidying to get the data into a more user-friendly
# format. 
  
# 1. First, let's have a look at our data object. Use the functions we learned from last
#    term. 

# 2. Let's drop the rows and columns we don't need.
# We only have one year, so the two cols related to year can be dropped; also, we only
# really need one col for country name, so let's drop country code too.
  
# 3. Let's also get rid of the variable code in square brackets

names(data) <- #hint: try using the function sub() with the regexp " \\[.*"
  
#### Analysing the data
# Now that we have a dataset in the desired format, we can proceed to the analysis.

# 1. Let's perform some preliminary descriptive analysis using our visualisation skills.
#    Try using ggplot to create a plot of scatter showing GDP p/c vs Tax revenue. Add a
#    simple linear regression line.
  

  
# 2. Now let's try the same using GDP p/c vs Ease of Doing Business.
data <- data %>%
  select(-(starts_with('Time')), -('Country Code'))


ls(data)

str(data)

# 3. And, for the sake of argument, let's see what the relationship is between Tax and
#    Ease of Doing Business.

# 4. Let's think for a minute before we perform the multivariate regression: what kind
#    of interaction are we seeing with these three plots?

# 5. Now let's run a regression!

formula <- `GDP per capita (current US$)` ~ `Tax revenue (% of GDP)` + `Ease of doing business rank (1=most business-friendly regulations)`

# How do we interpret these results?

#### Communicating
# The final task is to communicate our results. We're going to do this in pdf format 
# using latex, and then upload our results to github, just as we would with a problem
# set!

# 1. Visualisation
# We want a good visualisation of our results, including a title. We've seen that Ease 
# of Doing Business doesn't seem to have a very significant effect (statistically or
# substantively), so let's plot GDP vs Tax, and include Ease of Doing Business as
# either a size or alpha variable to our scatter points. Use the "export" option in the
# plots window to create a pdf of the plot below. Save it in the same folder as your 
# latex template.

data %>%
  ggplot(aes(`Tax revenue (% of GDP)`, 
             `GDP per capita (current US$)`, 
             alpha = `Ease of doing business rank (1=most business-friendly regulations)`)) +

# 2. Regression table
# We'll use stargazer to create the latex code for our regression table. Clear your 
# console, then run the code below.

stargazer()

# Now all we need is to update the latex template and upload the pdf to github!