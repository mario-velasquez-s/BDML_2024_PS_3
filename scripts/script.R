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
       dplyr,
       ggplot2,
       visdat,
       caret,
       sf,
       osmdata,
       tidymodels,
       parsnip,
       glmnet,
       rattle,
       spatialsample,
       recipes,
       lwgeom,
       class)


# 1: Initial Data Manipulation -----------------------------------------------
user <- Sys.getenv("USERNAME") # or you can hardcode the username if needed
if (user == "Maria.Arias") {
  setwd("C:/Users/Maria.Arias/OneDrive - Universidad de los andes/MSc Economics/Big Data & Machine Learning/Problem set 3/carpeta equipo/BDML_2024_PS_3")
} else if (user == "marti") {
  setwd("C:/Users/marti/OneDrive/Documentos/BDML_2024/BDML_2024/BDML_2024_PS_3_definitivo")
} else if (user == "mario") {
  setwd("C:/Users/mario/Desktop/TODO/UNI ANDES/SEM 8 (2024-1)/Big Data y Machine Learning/Taller 3/BDML_2024_PS_3")
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
#                           Creation of geographic data    
#                   (Takes to long to run. To avoid running, run
#                line 61 and 64 to get the resulting data frames)
###############################################################################

#source("scripts/geographic_variables.R")


###############################################################################
#                         Start here to save time
###############################################################################
# Read the 'test' shapefile
train <- st_read("data/train_json_v3.geojson")

# Read the 'train' shapefile
test <- st_read("data/test_json_v3.geojson")

###############################################################################
#           Imputation and creation of variables from description 
###############################################################################

source("scripts/impu_new_variables.R")

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

## An instrumental function to estimate mean of variables group_by a particular other variable
groupby_mean <- function(base,grupo_por,variable){
  summary <- base %>%
    group_by({{grupo_por}}) %>%
    summarise(count = n(),
              mean = mean({{variable}}),
              sd = sd({{variable}}),
              min= min({{variable}}),
              max = max({{variable}}))
  print(summary)
}


## We notice there are advertisements for different months and years, in both databases.
## There is no significant variation between years and months
ggplot(data=train,aes(x=factor(month), y=price)) + geom_boxplot() + theme_minimal()
groupby_mean(train,month,price)

## Now, we notice that prices in Chapinero are the highest in the train data 
ggplot(data=train,aes(x=factor(cod_loc), y=price)) + geom_boxplot() + theme_minimal()

#We check if is a matter of area. But it isn't. The mean and the minimum are the highest.
train <- train %>% mutate(price_per_area = price / area)
ggplot(data=train,aes(x=factor(cod_loc), y=price_per_area)) + geom_boxplot(outlier.shape = NA) + theme_minimal()
groupby_mean(train,localidad,price_per_area)

# Other variables such as mean number of rooms is different for Chapinero too 
ggplot(data=train,aes(x=factor(cod_loc), y=rooms_imp_numerico)) + geom_boxplot() + theme_minimal() ##Less rooms

train <- train %>% mutate(price_per_rooms = price / rooms_imp_numerico)
groupby_mean(train,localidad,price_per_rooms) ## The highest price per number of rooms

train <- train %>% dplyr::select(-price_per_rooms,-price_per_area)


# Ideally, we will use only Chapinero properties in the train set, but there 
# only 307 observations. So, we will try with different combinations of bases for training.

###############################################################################
#                                 Models 
###############################################################################

#I set my internal test sample
chapitrain <- train %>% subset(localidad== "Chapinero")
traintrain <- train %>% subset(localidad != "Chapinero")
chapicandetrain <- train %>% subset(localidad== "Chapinero" | localidad== "Candelaria")
chapisoletrain <- train %>% subset(localidad== "Chapinero" | barriocomu== "La Soledad")


#nrow(chapitrain)/nrow(train)

#Setting elastic net
elastic_net_spec <- linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet")

grid_values <- grid_regular(penalty(range = c(-2,1)), levels = 10) %>%
  expand_grid(mixture = c(0, 0.25,  0.5, 0.75,  1))

library(xgboost)
#XGBoost
xgboost_spec <- boost_tree(
  trees = 2000,
  tree_depth = tune(),
  learn_rate = 0.00125,
  loss_reduction = tune(),
  sample_size = tune(),
  mtry = tune()
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

#Grid Values XGBoost
grid_values_xgboost <- grid_latin_hypercube(
  tree_depth(),
  #learn_rate(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), train),
  size = 20
)



#-------------------------------------------------------------------------------
#                         VARIABLES' POOL
#-------------------------------------------------------------------------------

#property_type, dist_nearest_restaurant, dist_nearest_parques, dist_nearest_universidades,
#  n_pisos_numerico, rooms_imp_numerico, baños, area, localidad,
# sq_baños, sq_rooms, iluminado, remodelado, terraza, ascensor, estrato

# Set workflows

predecir<- function(base_train){

  rec1 <- recipe(base_train) %>%
    update_role(property_type,  area, dist_nearest_restaurant,
                dist_nearest_parques, baños, n_pisos_numerico,dist_nearest_universidades,
                terraza, ascensor, estrato,
              new_role = "predictor") %>%
  update_role(price, new_role = "outcome") %>%
  step_novel(all_nominal_predictors()) %>%   # para las clases no antes vistas en el train. 
  step_dummy(all_nominal_predictors()) %>%  # crea dummies para las variables categóricas
  step_zv(all_predictors()) %>%   #  elimina predictores con varianza cero (constantes)
  step_normalize(all_predictors())  # normaliza los predictores.

workflow_1 <- workflow() %>% 
  # Agregar la receta de preprocesamiento de datos. En este caso la receta 1
  add_recipe(rec1) %>%
  # Agregar la especificación del modelo de regresión Elastic Net
  #add_model(elastic_net_spec)
  # Agregar la especificación del modelo de regresión XGBoost
  add_model(xgboost_spec)

## Set the validation process
set.seed(15052024)
block_folds <- spatial_block_cv(base_train, v=5)
autoplot(block_folds)

tune_rest1 <- tune_grid(workflow_1,
                        resamples = block_folds,
                        grid = grid_values_xgboost,
                        metrics = metric_set(mae)
)

collect_metrics(tune_rest1)
best_tune_res1 <- select_best(tune_rest1, metric="mae")
print(best_tune_res1)
res1_final <- finalize_workflow(workflow_1,best_tune_res1)
EN_final1_fit <- fit(res1_final, data = base_train)
#EN_final1_fit <- fit(res1_final, data = chapitrain)

print(augment(EN_final1_fit, new_data = chapitrain) %>%
  mae(truth = price, estimate = .pred))

return(EN_final1_fit)

}


pred_final <- predecir(chapitrain)

#Predicciones
predicciones <- predict(pred_final, new_data = test) %>%
  bind_cols(test)

submission <- predicciones %>%
  dplyr::select(property_id, .pred) %>%
  rename(price = .pred)

write_csv(submission, "predicciones/submission_RV2_.csv")
