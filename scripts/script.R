# 
# Big Data y Machine Learning para Economia Aplicada
# Maria Camila Arias, Martin Velasquez, Mario Velasquez, Daniela Vlasak
# Problem Set 2
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
       caret)


# 1: Initial Data Manipulation -----------------------------------------------
setwd("C:/Users/Maria.Arias/OneDrive - Universidad de los andes/MSc Economics/Big Data & Machine Learning/Problem set 3/data")
train <- read.csv("train.csv")
test <- read.csv("test.csv")
str(test)

train_miss <- skim(train)
test_miss <- skim(test)
print(train_miss)
print(test_miss)
# Surface, rooms and bathrooms have significant missings.

###############################################################################
#                                    Imputation                               
###############################################################################
train %>% count(bathrooms)
ggplot(data=train,aes(x=bedrooms, y=bathrooms)) + geom_point()
ggplot(data=train,aes(x=rooms, y=bathrooms)) + geom_point()
filtered_data <- subset(train, surface_total < 200)
ggplot(data=filtered_data,aes(x=surface_total, y=bathrooms)) + geom_point()

##There is no lineal sense with the bathrooms variable, so, I will recover variables from property description

## Text to lowercase
limpieza_texto <- function(base){
  base <- base %>% mutate(description = str_to_lower(description) )
  base <- base %>% mutate(description = iconv(description, from = "UTF-8", to = "ASCII//TRANSLIT"))
}

limpieza_texto(train)
limpieza_texto(test)

## Creating the number of floors variable:
var_npisos <-function(base){
  base <- base %>% mutate(n_pisos = str_extract(description,"(\\w+|\\d+) pisos")) %>%
    mutate(n_pisos = ifelse(property_type == "Casa", n_pisos,NA))
  
  return(base)
}

train <- var_npisos(train)
test <- var_npisos(test)

#Explore data
prop.table(table(train$property_type)) ## In the training set 76% are apartments
prop.table(table(test$property_type)) ## While in the test set 97% are apartments
## Hence in Chapinero people rarely sell houses.


