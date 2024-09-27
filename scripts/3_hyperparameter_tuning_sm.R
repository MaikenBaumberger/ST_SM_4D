##########################################
#packages

library("CAST")
library("caret")

##########################################
#load training data

directory <- "" #fill in the directory to the folder 

load(file.path(directory, "training_data/train_set_sm.Rdata"))

###########################################
#create space-time-folds

set.seed(5)

cv_spacetimefolds <- CAST::CreateSpacetimeFolds(train_set_sm,spacevar ="plot_id",timevar ="date_hour", k=5)


###########################################
#selected predictors from the forward feature selection

predictors <- c("air_temperature_mountain_72","air_temperature_3_month","precipitation","global_radiation",
                "radolan","radolan_sum_0_6","radolan_sum_0_24",
                "prec_sum_3_month",
                "depths",
                "soil_texture","soil_type","land_use","inclination","topo_wetness",
                "ndvi")

response <- "soil_moisture" 

###########################################
#random forest model with hyperparameter tuning

hyperparameter = expand.grid(mtry = c(2,4,6,8,10,12),
                             min.node.size = c(5,10,15,20),
                             splitrule = "variance")

rfmodel = caret::train(x =  train_set_sm[,predictors],
                       y =   train_set_sm[,response],
                       method = "ranger",
                       tuneGrid = hyperparameter,
                       num.trees = 100,
                       trControl = trainControl(method = "cv", number = 5,
                                                index = cv_spacetimefolds$index, 
                                                indexOut = cv_spacetimefolds$indexOut,
                                                savePredictions = "final"),
                       importance = "permutation")

#save(rfmodel, file = file.path(directory, "rf_model_sm.Rdata"))
