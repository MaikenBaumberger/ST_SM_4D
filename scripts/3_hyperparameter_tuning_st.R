##########################################
#packages

library("CAST")
library("caret")

##########################################
#load training data

directory <- "" #fill in the directory to the folder 

load(file.path(directory, "training_data/train_set_st.Rdata"))

###########################################
#create space-time-folds

set.seed(5)

cv_spacetimefolds <- CAST::CreateSpacetimeFolds(train_set_st,spacevar ="plot_id",timevar ="date_hour", k=5)

###########################################
#selected predictors from the forward feature selection

predictors <- c("air_temperature_mountain","precipitation","global_radiation","relative_humidity",
                "air_temperature_mountain_3","air_temperature_mountain_6","air_temperature_mountain_12",
                "air_temperature_3_month","air_temperature_month",
                "trend_3month","trend_month", "prec_sum_4_month",
                "depths",
                "land_use","soil_type","elevation",
                "air_temperature_day","air_temperature_mountain_72","trend_week")

response <- "soil_temperature" 

###########################################
#random forest model with hyperparameter tuning

hyperparameter = expand.grid(mtry = c(2,4,6,8,10,12),
                             min.node.size = c(5,10,15,20),
                             splitrule = "variance")

rfmodel = caret::train(x =  train_set_st[,predictors],
                       y =   train_set_st[,response],
                       method = "ranger",
                       tuneGrid = hyperparameter,
                       num.trees = 100,
                       trControl = trainControl(method = "cv", number = 5,
                                                index = cv_spacetimefolds$index, 
                                                indexOut = cv_spacetimefolds$indexOut,
                                                savePredictions = "final"),
                       importance = "permutation")

#save(rfmodel, file = file.path(directory, "rf_model_st.Rdata"))