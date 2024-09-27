library(raster)
library(zoo)
library(sf)
library(caret)
library(CAST)

directory <- "" #fill in the directory to the folder 

load(file.path(directory, "training_data/data_set_complete.Rdata"))

############################################
# selection of time period

dt <- subset(predictor_set,
             date_hour >= as.POSIXct('2022-01-01 00:00') &
               date_hour <= as.POSIXct('2022-12-31 23:59'))

############################################
# convert categorical variables to factors

dt$soil_texture = as.factor(dt$soil_texture)
dt$soil_type = as.factor(dt$soil_type)
dt$land_use = as.factor(dt$land_use)

############################################
#split train and test data using space-time-folds

set.seed(33)

spacetimefolds1 <- CAST::CreateSpacetimeFolds(dt,spacevar = "plot_id",timevar ="date_hour", k=4)

train = dt[spacetimefolds1$index[[1]],]
test  = dt[spacetimefolds1$indexOut[[1]],]

##############################################
#remove soil moisture data from the soil temperature train set and vice versa

train_st = train[ , -which(names(train) %in% c("M_05","M_15","M_25","M_35","M_45","M_55","M_65","M_75","M_85","M_95"))]
test_st = test[ , -which(names(test) %in% c("M_05","M_15","M_25","M_35","M_45","M_55","M_65","M_75","M_85","M_95"))]

train_sm = train[ , -which(names(train) %in% c("T_05","T_15","T_25","T_35","T_45","T_55","T_65","T_75","T_85","T_95"))]
test_sm = test[ , -which(names(test) %in% c("T_05","T_15","T_25","T_35","T_45","T_55","T_65","T_75","T_85","T_95"))]

##############################################
#create train set for soil temperature

train_st_melt <- reshape2::melt(train_st, id = c("probe_name","date_hour","id","air_temperature_mountain","precipitation","global_radiation",
                                               "relative_humidity","air_pressure","wind_speed",
                                               "trend_week","trend_month", "trend_3month",
                                               "radolan", "radolan_sum_0_6","radolan_sum_6_12","radolan_sum_12_18","radolan_sum_18_24","radolan_sum_0_24","radolan_sum_24_48",
                                               "radolan_sum_48_72","radolan_sum_week",
                                               "prec_sum_1_month","prec_sum_2_month","prec_sum_3_month", "prec_sum_4_month","prec_sum_5_month", "prec_sum_6_month",
                                               "air_temperature_mountain_3","air_temperature_mountain_6","air_temperature_mountain_12","air_temperature_mountain_24",
                                               "air_temperature_mountain_48","air_temperature_mountain_72","air_temperature_mountain_96","air_temperature_mountain_120",
                                               "air_temperature_day","air_temperature_week","air_temperature_month","air_temperature_3_month",
                                               "soil_texture","soil_type","elevation","land_use","inclination","northness","eastness","topo_wetness","ndvi","ndwi","radar",
                                               "date", "plot_id"
)) 

train_st_melt$depths = substr(train_st_melt$variable, 3,5)

train_st_melt$soil_temperature=train_st_melt$value

train_set_st = train_st_melt[ , -which(names(train_st_melt) %in% c("value","variable"))]

train_set_st=train_set_st[complete.cases(train_set_st), ]

train_set_st$depths = as.numeric(train_set_st$depths)

train_set_st = train_set_st[!duplicated(train_set_st[,c('probe_name', 'date_hour','depths')]),]

train_set_st$soil_temperature <- round(train_set_st$soil_temperature,2)

head(train_set_st)

#################################################
#create test set for soil temperature

test_st_melt <- reshape2::melt(test_st, id = c("probe_name","date_hour","id","air_temperature_mountain","precipitation","global_radiation",
                                         "relative_humidity","air_pressure","wind_speed",
                                         "trend_week","trend_month", "trend_3month",
                                         "radolan", "radolan_sum_0_6","radolan_sum_6_12","radolan_sum_12_18","radolan_sum_18_24","radolan_sum_0_24","radolan_sum_24_48",
                                         "radolan_sum_48_72","radolan_sum_week",
                                         "prec_sum_1_month","prec_sum_2_month","prec_sum_3_month", "prec_sum_4_month","prec_sum_5_month", "prec_sum_6_month",
                                         "air_temperature_mountain_3","air_temperature_mountain_6","air_temperature_mountain_12","air_temperature_mountain_24",
                                         "air_temperature_mountain_48","air_temperature_mountain_72","air_temperature_mountain_96","air_temperature_mountain_120",
                                         "air_temperature_day","air_temperature_week","air_temperature_month","air_temperature_3_month",
                                         "soil_texture","soil_type","elevation","land_use","inclination","northness","eastness","topo_wetness","ndvi","ndwi","radar",
                                         "date", "plot_id"
)) 

test_st_melt$depths = substr(test_st_melt$variable, 3,5)

test_st_melt$soil_temperature=test_st_melt$value

test_set_st = test_st_melt[ , -which(names(test_st_melt) %in% c("value","variable"))]


test_set_st=test_set_st[complete.cases(test_set_st), ]

test_set_st$depths = as.numeric(test_set_st$depths)

test_set_st = test_set_st[!duplicated(test_set_st[,c('probe_name', 'date_hour','depths')]),]


test_set_st$soil_temperature <- round(test_set_st$soil_temperature,2)

head(test_set_st)

#################################################
#create train set for soil moisture

train_sm_melt <- reshape2::melt(train_sm, id = c("probe_name","date_hour","id","air_temperature_mountain","precipitation","global_radiation",
                                                 "relative_humidity","air_pressure","wind_speed",
                                                 "trend_week","trend_month", "trend_3month",
                                                 "radolan", "radolan_sum_0_6","radolan_sum_6_12","radolan_sum_12_18","radolan_sum_18_24","radolan_sum_0_24","radolan_sum_24_48",
                                                 "radolan_sum_48_72","radolan_sum_week",
                                                 "prec_sum_1_month","prec_sum_2_month","prec_sum_3_month", "prec_sum_4_month","prec_sum_5_month", "prec_sum_6_month",
                                                 "air_temperature_mountain_3","air_temperature_mountain_6","air_temperature_mountain_12","air_temperature_mountain_24",
                                                 "air_temperature_mountain_48","air_temperature_mountain_72","air_temperature_mountain_96","air_temperature_mountain_120",
                                                 "air_temperature_day","air_temperature_week","air_temperature_month","air_temperature_3_month",
                                                 "soil_texture","soil_type","elevation","land_use","inclination","northness","eastness","topo_wetness","ndvi","ndwi","radar",
                                                 "date", "plot_id"
)) 

train_sm_melt$depths = substr(train_sm_melt$variable, 3,5)

train_sm_melt$soil_moisture=train_sm_melt$value

train_set_sm = train_sm_melt[ , -which(names(train_sm_melt) %in% c("value","variable"))]

train_set_sm=train_set_sm[complete.cases(train_set_sm), ]

train_set_sm$depths = as.numeric(train_set_sm$depths)

train_set_sm = train_set_sm[!duplicated(train_set_sm[,c('probe_name', 'date_hour','depths')]),]

train_set_sm$soil_moisture <- round(train_set_sm$soil_moisture,2)

head(train_set_sm)

#################################################
#create test set for soil moisture

test_sm_melt <- reshape2::melt(test_sm, id = c("probe_name","date_hour","id","air_temperature_mountain","precipitation","global_radiation",
                                               "relative_humidity","air_pressure","wind_speed",
                                               "trend_week","trend_month", "trend_3month",
                                               "radolan", "radolan_sum_0_6","radolan_sum_6_12","radolan_sum_12_18","radolan_sum_18_24","radolan_sum_0_24","radolan_sum_24_48",
                                               "radolan_sum_48_72","radolan_sum_week",
                                               "prec_sum_1_month","prec_sum_2_month","prec_sum_3_month", "prec_sum_4_month","prec_sum_5_month", "prec_sum_6_month",
                                               "air_temperature_mountain_3","air_temperature_mountain_6","air_temperature_mountain_12","air_temperature_mountain_24",
                                               "air_temperature_mountain_48","air_temperature_mountain_72","air_temperature_mountain_96","air_temperature_mountain_120",
                                               "air_temperature_day","air_temperature_week","air_temperature_month","air_temperature_3_month",
                                               "soil_texture","soil_type","elevation","land_use","inclination","northness","eastness","topo_wetness","ndvi","ndwi","radar",
                                               "date", "plot_id"
)) 

test_sm_melt$depths = substr(test_sm_melt$variable, 3,5)

test_sm_melt$soil_moisture=test_sm_melt$value

test_set_sm = test_sm_melt[ , -which(names(test_sm_melt) %in% c("value","variable"))]


test_set_sm=test_set_sm[complete.cases(test_set_sm), ]

test_set_sm$depths = as.numeric(test_set_sm$depths)

test_set_sm = test_set_sm[!duplicated(test_set_sm[,c('probe_name', 'date_hour','depths')]),]


test_set_sm$soil_moisture <- round(test_set_sm$soil_moisture,2)

head(test_set_sm)

################################################
#save data

# save(test_set_st, file = file.path(directory, "test_set_st.Rdata"))
# save(train_set_st, file = file.path(directory, "train_set_st.Rdata"))
# 
# save(test_set_sm, file = file.path(directory, "test_set_sm.Rdata"))
# save(train_set_sm, file = file.path(directory, "train_set_sm.Rdata"))
# 
#
# write.csv(test_set_st, file = file.path(directory, "test_set_st.csv"), row.names = FALSE)
# write.csv(train_set_st, file = file.path(directory, "train_set_st.csv"), row.names = FALSE)
# 
# write.csv(test_set_sm, file = file.path(directory, "test_set_sm.csv"), row.names = FALSE)
# write.csv(train_set_sm, file = file.path(directory, "train_set_sm.csv"), row.names = FALSE)

#################################################


