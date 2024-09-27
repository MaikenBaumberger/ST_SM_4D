##########################################
#this script shows one (out of ten) exemplary forward feature selection 

library("CAST")
library("caret")

directory <- "" #fill in the directory to the folder 

load(file.path(directory, "training_data/train_set_sm.Rdata"))

###########################################
#create space-time-folds

set.seed(33)

cv_spacetimefolds <- CAST::CreateSpacetimeFolds(train_set_sm,spacevar = "plot_id",timevar ="date_hour", k=5)

###########################################
#select predictors for the model

predictors <- c("air_temperature_mountain","precipitation","global_radiation","relative_humidity","air_pressure","wind_speed",
                "air_temperature_mountain_3","air_temperature_mountain_6","air_temperature_mountain_12","air_temperature_mountain_24",
                "air_temperature_mountain_48","air_temperature_mountain_72","air_temperature_mountain_96","air_temperature_mountain_120",
                "air_temperature_day","air_temperature_week","air_temperature_month","air_temperature_3_month",
                "trend_week","trend_month", "trend_3month",
                "radolan", "radolan_sum_0_6","radolan_sum_6_12","radolan_sum_12_18","radolan_sum_18_24","radolan_sum_0_24","radolan_sum_24_48",
                "radolan_sum_48_72","radolan_sum_week",
                "prec_sum_1_month","prec_sum_2_month","prec_sum_3_month", "prec_sum_4_month","prec_sum_5_month", "prec_sum_6_month",
                "depths",
                "soil_texture","soil_type","elevation","land_use","inclination","northness","eastness","topo_wetness",
                "ndvi", "ndwi", "radar")

response <- "soil_moisture" 

###########################################
#forward feature selection

hyperparameter = expand.grid(mtry = 2,
                             min.node.size = 5,
                             splitrule = "variance")

ffsmodel = CAST::ffs(predictors =  train_set_sm[,predictors],
                     response =   train_set_sm[,response],
                     method = "ranger",
                     tuneGrid = hyperparameter,
                     num.trees = 100,
                     trControl = trainControl(method = "cv", number = 5,
                                              index = cv_spacetimefolds$index, 
                                              indexOut = cv_spacetimefolds$indexOut,
                                              savePredictions = "final"),
                     importance = "permutation")

#save(ffsmodel, file = file.path(directory, "ffs_model_sm.Rdata"))






