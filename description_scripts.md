**Description of the scripts**

**[1_data_preparation](scripts/1_data_preparation.R):** 
The script begins at a point where a dataset has been created that contains soil temperature and soil moisture measurement data at all depths, as well as the temporally and spatially associated predictor data (meteorological data, soil data, land use, relief, and satellite data). The dataset used is called [data_set_complete.Rdata](training_data/data_set_complete.Rdata) and can be found in the [training_data](training_data) folder. In the script, the training and test set are created.

**[2_forward_feature_selection_example_sm](scripts/2_forward_feature_selection_example_sm.R):** 
This script demonstrates an example of one of the ten forward feature selection processes to identify suitable predictors for the model. In [2_forward_feature_selection_example_sm](scripts/2_forward_feature_selection_example_sm.R) the features for the soil moisture model are selected, while in [2_forward_feature_selection_example_st](scripts/2_forward_feature_selection_example_st.R) the predictors for the soil temperature model are selected.

**[3_hyperparameter_tuning_sm](scripts/3_hyperparameter_tuning_sm.R):**
In this script, a model for soil moisture is trained based on the predictors selected during the ten forward feature selections. Hyperparameters are also tuned.  [3_hyperparameter_tuning_st.](scripts/3_hyperparameter_tuning_st.R) is the same for soil temperature.

**[4_model_performace_evaluation_st_sm](scripts/4_model_performace_evaluation_st_sm.R):**
This script evaluates the performance of the soil temperature model [rfmodel_st](models/rfmodel_st.Rdata) and the soil moisture model [rfmodel_sm](models/rfmodel_sm.Rdata). The model performance is evaluated over space, time, and depth.

**[5_calculate_train_di](scripts/5_calculate_train_di.R):** 
To calculate the area of applicability, the dissimilarity index is necessary. Here, the dissimilarity index is calculated separately for each depth.

**[6_spatial_prediction_sm](scripts/6_spatial_prediction_sm.R):** 
Since the spatial grids of the predictors are not provided, this script serves as an example of how to make a spatial prediction for soil moisture. The same applies to [6_spatial_prediction_st](scripts/6_spatial_prediction_st.R) for soil temperature.

**[7_partial_dependency_sm](scripts/7_partial_dependency_sm.R):** 
HThis script calculates and visualizes the partial dependency for soil moisture (use [7_partial_dependency_st](scripts/7_partial_dependency_st.R) for soil temperature).

**[8_temporal_prediction_longterm_st_sm](scripts/8_temporal_prediction_longterm_st_sm.R):** 
This script demonstrates the temporal prediction of soil temperature and soil moisture for two different locations.
