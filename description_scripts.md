**[1_data_preparation](1_data_preparation.R):** 
The script starts at a point, where a data set was created that contains the soil temperature and soil moisture measurement data at all depths as well as the temporally and spatially associated predictor data (meteorological data, soil data, land use, relief and satellite data). The data set which is used is called [data_set_complete.Rdata](training_data/data_set_complete.Rdata) and can be found in the [training_data](training_data) folder. In the script the training and test set are created.

**[2_forward_feature_selection_example_sm](scripts/2_forward_feature_selection_example_sm.R):** 
This script shows an example for one of the ten forward feature selections, to select suitable predictors for the model. In [2_forward_feature_selection_example_sm](scripts/2_forward_feature_selection_example_sm.R) the feature for the soil moisture model are selected and in [2_forward_feature_selection_example_st](scripts/2_forward_feature_selection_example_st.R) the predictors for the soil temperature model are selected.

**[3_hyperparameter_tuning_sm](scripts/3_hyperparameter_tuning_sm.R):**
Here a model for soil moisture is trained based on the predictores selected in the ten forward feature selections. Hyperparameter are also tuned. [3_hyperparameter_tuning_st.](scripts/3_hyperparameter_tuning_st.R) is the same for soil temperature.

**[4_model_performace_evaluation_st_sm](scripts/4_model_performace_evaluation_st_sm.R):**
Here the perfomamnce of the soil temperature model [rfmodel_st](models/rfmodel_st.Rdata) and the soil moisture model [rfmodel_sm](models/rfmodel_sm.Rdata) is evaluated. The model perforamnce evaluation is over space, time and depth.

**[5_calculate_train_di](scripts/5_calculate_train_di.R):** 
To calculate the area of applicability, the dissimalarity index is nessesary. Here the dissimalarity indes is calculated for all depth seperatly.

**[6_spatial_prediction_sm](scripts/6_spatial_prediction_sm.R):** 
As the spatial grids of the predictors are not provided, this script is just an example on how to make a spatial prediction on soil moisture. The same in [6_spatial_prediction_st](scripts/6_spatial_prediction_st.R) for soil temperature.

**[7_partial_dependency_sm](scripts/7_partial_dependency_sm.R):** 
Here the parial dependency for soil moisture is calculated and visualised ([7_partial_dependency_st](scripts/7_partial_dependency_st.R) for soil temperature).

**[8_temporal_prediction_longterm_st_sm](scripts/8_temporal_prediction_longterm_st_sm.R):** 
This scrips shows the temporal prediction of soil temperature and soil moisture for two diffent locations. 
