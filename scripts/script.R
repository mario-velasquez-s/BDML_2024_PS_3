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
       class,
       dials,
       tidymodels)


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
#                   (Takes too long to run. To avoid running, run
#                line 61 and 64 to get the resulting data frames)
###############################################################################

#source("scripts/geographic_variables.R")


###############################################################################
#                         Start here to save time
###############################################################################
# Read the 'test' shapefile
train <- st_read("data/train_json_v5.geojson")

# Read the 'train' shapefile
test <- st_read("data/test_json_v5.geojson")

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
nortetrain <- train %>% subset(localidad== "Chapinero" | localidad== "Barrios Unidos" | localidad== "Suba" | localidad== "Usaquén")

#nrow(chapitrain)/nrow(train)

#Setting elastic net
elastic_net_spec <- linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet")

grid_values <- grid_regular(penalty(range = c(-2,1)), levels = 10) %>%
  expand_grid(mixture = c(0, 0.25,  0.5, 0.75,  1))

library(xgboost)
#XGBoost
xgboost_spec <- boost_tree(
  trees = 5000,
  tree_depth = tune(),
  learn_rate = tune(),
  #learn_rate = 0.00125,
  loss_reduction = tune(),
  sample_size = tune(),
  mtry = tune()
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

#Grid Values XGBoost
grid_values_xgboost <- grid_latin_hypercube(
  tree_depth(),
  learn_rate(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), train),
  size = 20
)

library(keras)
library(tensorflow)
#Neural Network

nn_spec <- function(units, penalty, epochs) {
  model <- keras_model_sequential() %>%
    layer_dense(units = units, activation = "relu") %>%
    layer_dropout(rate = 0.2) %>%
    layer_dense(units = units/2, activation = "relu") %>%
    layer_dense(units = 1)  # Output layer
  
  compile(model,
          optimizer = optimizer_adam(), 
          loss = "mean_squared_error")
}

# Grid Values for Neural Network
grid_values_nn <- expand.grid(
  units = seq(5, 50, by = 5),
  penalty = seq(0, 0.01, length.out = 5),  # Adjust regularization strength as needed
  epochs = seq(50, 200, by = 50)
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
                terraza, ascensor, estrato, dis_centro, dis_andino,
                restaurant_1km, parques_1km, discotecas_1km, colegios_1km,
                iluminado, exterior, remodelado, restaurant_300m, parques_300m,
                discotecas_300m, colegios_300m, restaurant_100m, parques_100m, 
                discotecas_100m, colegios_100m,
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
  # Agregar la especificación del modelo de regresión XGBoost
  #add_model(nn_spec)
  

## Set the validation process
set.seed(15052024)
block_folds <- spatial_block_cv(base_train, v=10)
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



#-------------------------------------------------------------------------------
#                         Sin restringir a Chapinero
#-------------------------------------------------------------------------------


elastic_net_spec <- linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet")

grid_values <- grid_regular(penalty(range = c(-2,1)), levels = 50) %>%
  expand_grid(mixture = c(0, 0.25,  0.5, 0.75,  1))

#-------------------------------------------------------------------------------
#                         Forward best subset selection
#-------------------------------------------------------------------------------
library(car)


model_form<-  price ~ area + dist_nearest_restaurant +
  dist_nearest_parques+ dist_nearest_universidades +
  terraza + ascensor + estrato +
  dist_nearest_bus_station + sq_rooms +
  remodelado + dis_centro + dis_andino + (area + dist_nearest_restaurant +
                                            dist_nearest_parques + dist_nearest_universidades +
                                            terraza + ascensor + estrato + 
                                            dist_nearest_bus_station + sq_rooms +
                                            remodelado)^2

fordward_model <- regsubsets(model_form, ## formula
                             data = chapitrain, ## data frame Note we are using the training sample.
                             nvmax = 2, ## show only the first 3  models models
                             method = "forward" )  ## apply Forward Stepwise Selection

max_nvars= fordward_model[["np"]]-1  ## minus one because it counts the intercept.
max_nvars

predict.regsubsets <- function(object, newdata, id, ...) {
  form <- model_form
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
}

k <- 10
n <- nrow(train)
folds <- sample(rep(1:k, length = n))

calculateMAE <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

cv.mae_back <- matrix(NA, k, 50, dimnames = list(NULL, paste(1:50)))

for (j in 1:k) {
  print(j)
  best_fit <- regsubsets(model_form, data = train[folds != j, ], nvmax = 50, method = "backward")
  
  for (i in 1:50) {
    predicted_values <- predict(best_fit, train[folds == j, ], id = i)
    
    mae <- calculateMAE(train$price[folds == j], predicted_values)
    cv.mae_back[j, i] <- mae
  }
}

mean.mae_back <- apply(cv.mae_back, 2, mean)
minMAEModelIndex_back <- which.min(mean.mae_back)

plot(mean.mae_back, type = "b", xlab = "Number of Variables", ylab = "Mean Absolute Error")
minMAEModelIndex_back

best_fit <- regsubsets(model_form, data = train_pobre_numeric, nvmax = max_nvars, method = "backward")
coef_info <- summary(best_fit)
selected_variables <- names(coef(best_fit, id = minMAEModelIndex_back))
formula_string <- paste("price ~", paste(selected_variables[-1], collapse = " + "))
formula_string


#-------------------------------------------------------------------------------
#                         Random Forest
#-------------------------------------------------------------------------------


predecir_random_forest <- function(base_train) {
  # Define the recipe
  rec1 <- recipe(base_train) %>%
    update_role(property_type, area, dist_nearest_restaurant,
                dist_nearest_parques, baños, n_pisos_numerico, dist_nearest_universidades,
                terraza, ascensor, estrato,
                new_role = "predictor") %>%
    update_role(price, new_role = "outcome") %>%
    step_novel(all_nominal_predictors()) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_zv(all_predictors()) %>%
    step_normalize(all_predictors())
  
  # Define the model specification for Random Forest
  rf_spec <- rand_forest(
    mtry = tune(),
    trees = 128,
    min_n = tune()
  ) %>%
    set_mode("regression") %>%
    set_engine("randomForest")
  
  # Create the workflow
  workflow_1 <- workflow() %>%
    add_recipe(rec1) %>%
    add_model(rf_spec)
  
  # Set the validation process
  set.seed(15052024)
  block_folds <- spatial_block_cv(base_train, v = 5)
  autoplot(block_folds)
  
  # Create a tuning grid, ensuring mtry uses integer values
  rf_grid <- grid_regular(
    mtry(range = c(1, floor(sqrt(ncol(base_train))))),  # Ensure integer values for mtry
    min_n(c(5, 10))
  )
  
  # Perform tuning
  tune_res1 <- tune_grid(
    workflow_1,
    resamples = block_folds,
    grid = rf_grid,
    metrics = metric_set(mae)  # Focus on MAE
  )
  
  # Collect and view metrics
  collect_metrics(tune_res1)
  best_tune_res1 <- select_best(tune_res1, metric = "mae")
  print(best_tune_res1)
  
  # Finalize the workflow
  res1_final <- finalize_workflow(workflow_1, best_tune_res1)
  rf_final_fit <- fit(res1_final, data = base_train)
  
  # Print out the performance on new data
  print(augment(rf_final_fit, new_data = nortetrain) %>%
          mae(truth = price, estimate = .pred))
  
  return(rf_final_fit)
}


resultados_rf <- predecir_random_forest(nortetrain)
predicciones <- predict(resultados_rf, new_data = test) %>%
  bind_cols(test)

submission <- predicciones %>%
  dplyr::select(property_id, .pred) %>%
  rename(price = .pred)

write_csv(submission, "predicciones/submission_random_forest_1.csv")


#-------------------------------------------------------------------------------
#                         Linear regression
#-------------------------------------------------------------------------------

predecir_regresion <- function(base_train) {
  # Define the recipe
  rec1 <- recipe(base_train) %>%
    update_role(area,dist_nearest_restaurant,
                dist_nearest_parques, dist_nearest_universidades,
                terraza, ascensor,
                dist_nearest_bus_station, sq_rooms,
                remodelado, dis_centro, dis_andino,
                new_role = "predictor") %>%
    update_role(price, new_role = "outcome") %>%
    step_novel(all_nominal_predictors()) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_zv(all_predictors()) %>%
    step_normalize(all_predictors()) %>%
    step_interact(terms = ~ area:dist_nearest_restaurant)
  
  # Define the linear regression model specification
  lm_spec <- linear_reg() %>%
    set_engine("lm") %>%
    set_mode("regression")
  
  # Create the workflow
  workflow_1 <- workflow() %>%
    add_recipe(rec1) %>%
    add_model(lm_spec)
  
  # Set the validation process using spatial cross-validation
  set.seed(15052024)
  block_folds <- spatial_block_cv(base_train, v = 10)
  autoplot(block_folds)
  
  # Evaluate the model using cross-validation
  fit_results <- fit_resamples(
    workflow_1,
    resamples = block_folds,
    metrics = metric_set(mae)
  )
  
  # Collect metrics
  metrics <- collect_metrics(fit_results)
  print(metrics)
  
  # Fit the final model on the complete training data
  final_fit <- fit(workflow_1, data = base_train)
  
  # Print out the performance on new data
  print(augment(final_fit, new_data = nortetrain) %>%
          mae(truth = price, estimate = .pred))
  
  return(final_fit)
}



resultados_lm <- predecir_regresion(train) #MAE de 188125112
predicciones <- predict(resultados_lm, new_data = test) %>%
  bind_cols(test)

submission <- predicciones %>%
  dplyr::select(property_id, .pred) %>%
  rename(price = .pred)

write_csv(submission, "predicciones/linear_regression_1.csv")

#-------------------------------------------------------------------------------
#                         Neural network
#-------------------------------------------------------------------------------


base_train <- chapitrain[, c("area", "dist_nearest_restaurant", "dist_nearest_parques", 
                             "dist_nearest_universidades", "terraza", "ascensor", 
                             "dist_nearest_bus_station", "sq_rooms", "remodelado", 
                             "dis_centro", "dis_andino", "dist_nearest_colegios",
                             "restaurant_100m","parques_100m","estrato", "sq_baños","barriocomu",
                             "price"), drop = FALSE]


attr(base_train, "class") <- "data.frame"
attr(base_train, "sf_column") <- NULL
base_train <- separate(base_train, geometry, into = c("latitude", "longitude"), sep = ",")
indices <- which(names(base_train) %in% c("latitude", "longitude"))

# Drop the columns using the column indices
base_train <- base_train[, -indices]


# Create the recipe with non-geometric variables
recipe <- recipe(price ~ ., data = base_train) %>%
  step_dummy(all_nominal(), one_hot = TRUE)

# Prepare and bake the recipe
prepared_recipe <- prep(recipe, training = base_train)
final_data <- bake(prepared_recipe, new_data = NULL)  # NULL implies original data


# Separate features and target
train_data <- final_data[, !names(final_data) %in% "price"]
train_labels <- final_data$price

# Define the neural network model
model <- keras_model_sequential() %>%
  layer_dense(units = 10000, activation = 'relu', input_shape = c(ncol(train_data))) %>%
  layer_dense(units = 5000, activation = 'relu') %>%
  layer_dense(units = 3000, activation = 'relu') %>%
  layer_dense(units = 1000, activation = 'relu') %>%
  layer_dense(units = 1)

# Compile the model
model %>% compile(
  loss = 'mae',
  optimizer = 'adam',
  metrics = c('mean_absolute_error')
)

# Fit the model
history <- model %>% fit(
  x = as.matrix(train_data),
  y = train_labels,
  epochs = 30,
  batch_size = 32,
  validation_split = 0.2
)


# Print model summary
print(model %>% summary())


base_test <- test[, c("area", "dist_nearest_restaurant", "dist_nearest_parques", 
                      "dist_nearest_universidades", "terraza", "ascensor", 
                      "dist_nearest_bus_station", "sq_rooms", "remodelado", 
                      "dis_centro", "dis_andino", "dist_nearest_colegios",
                      "restaurant_100m","parques_100m", "Identificador.unico.de.la.localidad","estrato", "sq_baños",
                      "price"), drop = FALSE]

attr(base_test, "class") <- "data.frame"
attr(base_test, "sf_column") <- NULL
base_test <- separate(base_test, geometry, into = c("latitude", "longitude"), sep = ",")
indices <- which(names(base_test) %in% c("latitude", "longitude"))

# Drop the columns using the column indices
base_test <- base_test[, -indices]

base_test <- bake(prepared_recipe, new_data = base_test)

if ("price" %in% names(base_test)) {
  new_features <- base_test[, !names(base_test) %in% "price"]
} else {
  new_features <- base_test
}

# Since the new data doesn't have a 'price' column, use all columns as features
new_features_matrix <- as.matrix(new_features)

# Convert features to matrix format as expected by the keras model
predicted_prices <- predict(model, new_features_matrix)
predictions_df <- data.frame(Predicted_Prices = predicted_prices)
write.csv(predictions_df, file = "predicciones/predicted_prices_nn_v2.csv", row.names = FALSE)
base_test$price <- as.numeric(predicted_prices)
