**1_data_preparation:** 
The script starts at a point where a data set was created that contains the soil temperature and soil moisture measurement data at all depths as well as the temporally and spatially associated predictor data (meteorological data, soil data, land use, relief and satellite data). The data set which is used is called [data_set_complete.Rdata](training_data/data_set_complete.Rdata) and can be found in the [training_data](training_data) folder. In the script the training and test set are created.

**[2_forward_feature_selection_example_sm](scripts/2_forward_feature_selection_example_sm.R):** 
This script shows an example for one of the ten forward feature selections, to select suitable predictors for the model. In 2_forward_feature_selection_example_sm
