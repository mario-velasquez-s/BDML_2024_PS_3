# 
# Big Data y Machine Learning para Economia Aplicada
# Maria Camila Arias, Martin Velasquez, Mario Velasquez
# Problem Set 3
# 

# Initial Setup -----------------------------------------------------------

rm(list = ls())

if(!require(pacman)) install.packages("pacman") ; require(pacman)
p_load(rio, # import/export data
       tidyverse, # tidy-data
       skimr, # summary data
       gridExtra, ## visualizing missing data
       corrplot, ## Correlation Plots 
       stargazer, ## tables/output to TEX. 
       MASS,
       rvest,
       httr,
       dplyr,
       ggplot2,
       visdat,
       caret,
       sf,
       osmdata)


# 1: Initial Data Manipulation -----------------------------------------------
user <- Sys.getenv("USERNAME") # or you can hardcode the username if needed
if (user == "Maria.Arias") {
  setwd("C:/Users/Maria.Arias/OneDrive - Universidad de los andes/MSc Economics/Big Data & Machine Learning/Problem set 3/carpeta equipo/BDML_2024_PS_3")
} else if (user == "marti") {
  setwd("C:/Users/marti/OneDrive/Documentos/BDML_2024/BDML_2024/BDML_2024_PS_3_definitivo")
}
train <- read.csv("data/train.csv")
test <- read.csv("data/test.csv")
str(test)

train_miss <- skim(train)
test_miss <- skim(test)
print(train_miss)
print(test_miss)
# Surface, rooms and bathrooms have significant missings.

###############################################################################
#           Imputation and creation of variables from description 
###############################################################################
source("scripts/impu_new_variables.R")


###############################################################################
#                           Creation of geographic data                               
###############################################################################


source("scripts/geographic_variables.R")


# Explore data ------------------------------------------------------------

round(prop.table(table(train$property_type)),3) ## In the training set 76% are apartments
round(prop.table(table(test$property_type)),3) ## While in the test set 97% are apartments
## Hence in Chapinero people rarely sell houses.

round(prop.table(table(train$rooms_imp_numerico)), 3)
round(prop.table(table(test$rooms_imp_numerico)),3) ## In Chapinero apartments of 1 rooms seem more popular than in the training set.

round(prop.table(table(train$baños)), 3)
round(prop.table(table(test$baños)),3)

## In Chapinero properties seem smaller than in the train set.
summary(train$area)
summary(test$area)
