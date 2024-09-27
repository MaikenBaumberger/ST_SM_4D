##########################################
#this script is just an example on how to do the spatial prediction, 
#as the spatial grids and time series data of the predictor data can not be provided

##########################################
#packages

library(raster)
library(zoo)
library(mapview)
library(sf)
library(caret)
library(CAST)
library(terra)

##########################################
#load data and models

directory <- "" #fill in the directory to the folder 

load(file.path(directory, "models/rfmodel_st.Rdata"))
rfmodel
ffsmodel <- rfmodel

rm(rfmodel)

##########################################
#load the trainDI calculated in script 5

directory_di <- "" #fill in the directory to the folder of the calculated trainDIs 

DIst05 = readRDS(file.path(directory_di,"DIst05.rds"))
DIst15 = readRDS(file.path(directory_di,"DIst15.rds"))
DIst25 = readRDS(file.path(directory_di,"DIst25.rds"))
DIst35 = readRDS(file.path(directory_di,"DIst35.rds"))
DIst45 = readRDS(file.path(directory_di,"DIst45.rds"))
DIst55 = readRDS(file.path(directory_di,"DIst55.rds"))
DIst65 = readRDS(file.path(directory_di,"DIst65.rds"))
DIst75 = readRDS(file.path(directory_di,"DIst75.rds"))
DIst85 = readRDS(file.path(directory_di,"DIst85.rds"))

####################################
#data preparation

directory_pred <- "" #directory to predictor data sets (the spatial grids and time series data of the predictor data can not be provided)

meteo_data = read.csv(file.path(directory_pred,"meteo_data.csv")) #file with meteorological data (not provided) including 
#datetime, air_temperature_mountain, air_temperature_valley, precipitation, global_radiation, relative_humidity, air_pressure and wind_speed  
meteo_data$datetime <- as.POSIXct(strptime(meteo_data$datetime,"%Y-%m-%d %H:%M:%S"))

load(file.path(directory_pred,"trends.RData")) #file with trends of air temperature (not provided)

static_raster= stack(file.path(directory_pred,"static_raster.tif")) #file with staic raster (not provided)
names(static_raster) = c("soil_texture","soil_type","elevation","land_use","inclination","exposition",
                         "topo_wetness","northness","eastness")

radolan_files <- list.files(file.path(directory_pred,"/radolan"),pattern=".tif$", full.names=TRUE) #file with precipitation radar data (radolan) (not provided)

elevation_mountain = 765 #Waldstein 
elevation_valley = 620  #Voitsumra

elevation = static_raster$elevation
raster_base = elevation*0

#air tempereature means
meteo_data$air_temperature_day = zoo::rollmean(meteo_data$air_temperature_mountain, k = 24, fill = NA)
meteo_data$air_temperature_month = zoo::rollmean(meteo_data$air_temperature_mountain, k = 720, fill = NA)
meteo_data$air_temperature_3_month = zoo::rollmean(meteo_data$air_temperature_mountain, k = 2160, fill = NA)

#precipitaion sums
meteo_data$prec_sum_3_month = zoo::rollsum(meteo_data$precipitation, 168*12, align = "right", fill=NA)
meteo_data$prec_sum_4_month = zoo::rollsum(meteo_data$precipitation, 168*16, align = "right", fill=NA)
meteo_data$prec_sum_5_month = zoo::rollsum(meteo_data$precipitation, 168*20, align = "right", fill=NA)

####################################
# data preparation for prediction

#position = 5485 #"2022-01-15 12:00:00 CET"
#position = 6229 #"2022-02-15 12:00:00 CET"
#position = 6901 #"2022-03-15 12:00:00 CET"
#position = 7645 #"2022-04-15 12:00:00 CEST"
#position = 8365 #"2022-05-15 12:00:00 CEST"
#position = 9109 #"2022-06-15 12:00:00 CEST"
#position = 9829 #"2022-07-15 12:00:00 CEST"
#position = 10573 #"2022-08-15 12:00:00 CEST"
#position = 11317 #"2022-09-15 12:00:00 CEST"
#position = 12037 #"2022-10-15 12:00:00 CEST"
#position = 12781 #"2022-11-15 12:00:00 CET"
#position = 13501 #"2022-12-15 12:00:00 CET"


mid_month = c(5485,6229,6901,7645,8365,9109,9829,10573,11317,12037,12781,13501)
mid_month_2106 <- mid_month+138
mid_month_0800 <- mid_month-180
mid_month_2718 <- mid_month+294

for (i in mid_month){

#date and time
  print(i)
  position = i
  time = strftime(meteo_data$datetime[i], "%Y%m%d%H%M")
  print(time)
  doi = as.numeric(strftime(as.POSIXct(strftime(meteo_data$datetime[i], "%Y-%m-%d %H:%M:%S")), format = "%j"))
  print(doi)
  hour = as.numeric(strftime(as.POSIXct(strftime(meteo_data$datetime[i], "%Y-%m-%d %H:%M:%S")), format = "%H"))
  print(hour)

#create spatially continous data from predictors 
  prec_sum_4_month_spatially = raster_base + meteo_data$prec_sum_4_month[position]
  trend_3_month_spatially = raster_base + trends$trend_3month[position]
  trend_month_spatially = raster_base + trends$trend_month[position]
  trend_week_spatially = raster_base + trends$trend_week[position]
  temp_spatially = round(meteo_data$air_temperature_mountain[position]-((meteo_data$air_temperature_mountain[position]-meteo_data$air_temperature_valley[position])/(elevation_mountain-elevation_valley))*(elevation_mountain-elevation),2)
  radiation_spatially = raster_base+meteo_data$global_radiation[position]
  humidity_spatially = raster_base+meteo_data$relative_humidity[position]
  precipitation_spatially = raster_base+meteo_data$precipitation[position]
  temp_spatially_3 = round(meteo_data$air_temperature_mountain[position-3]-((meteo_data$air_temperature_mountain[position-3]-meteo_data$air_temperature_valley[position-3])/(elevation_mountain-elevation_valley))*(elevation_mountain-elevation),2)
  temp_spatially_6 = round(meteo_data$air_temperature_mountain[position-6]-((meteo_data$air_temperature_mountain[position-6]-meteo_data$air_temperature_valley[position-6])/(elevation_mountain-elevation_valley))*(elevation_mountain-elevation),2)
  temp_spatially_12 = round(meteo_data$air_temperature_mountain[position-12]-((meteo_data$air_temperature_mountain[position-12]-meteo_data$air_temperature_valley[position-12])/(elevation_mountain-elevation_valley))*(elevation_mountain-elevation),2)
  temp_spatially_72 = round(meteo_data$air_temperature_mountain[position-72]-((meteo_data$air_temperature_mountain[position-72]-meteo_data$air_temperature_valley[position-72])/(elevation_mountain-elevation_valley))*(elevation_mountain-elevation),2)
  temp_spatially_day = raster_base+meteo_data$air_temperature_day[position]
  temp_spatially_month = raster_base+meteo_data$air_temperature_month[position]
  temp_spatially_3_month = raster_base+meteo_data$air_temperature_3_month[position]
  type_spatially = static_raster$soil_type
  elevation_spatially = static_raster$elevation
  landuse_spatially = static_raster$land_use

#spatial raster for depths 
  depths_05 = raster_base + 5
  depths_15 = raster_base + 15
  depths_25 = raster_base + 25
  depths_35 = raster_base + 35
  depths_45 = raster_base + 45
  depths_55 = raster_base + 55
  depths_65 = raster_base + 65
  depths_75 = raster_base + 75
  depths_85 = raster_base + 85
  
  ####################################
  #predictions for each depth including the calculation of the area of applicability
  
  predictors_05 = stack(temp_spatially, precipitation_spatially, radiation_spatially, humidity_spatially,
                        temp_spatially_3, temp_spatially_6, temp_spatially_12, 
                        temp_spatially_3_month, temp_spatially_month,
                        trend_3_month_spatially, trend_month_spatially,prec_sum_4_month_spatially,
                        landuse_spatially, type_spatially,elevation_spatially, 
                        depths_05, temp_spatially_day, temp_spatially_72, trend_week_spatially)
  
  plot(predictors_05)
  
  names(predictors_05) <- c("air_temperature_mountain","precipitation","global_radiation","relative_humidity",
                            "air_temperature_mountain_3","air_temperature_mountain_6","air_temperature_mountain_12",
                            "air_temperature_3_month","air_temperature_month",
                            "trend_3month","trend_month", "prec_sum_4_month",
                            "land_use","soil_type","elevation", 
                             "depths", "air_temperature_day", "air_temperature_mountain_72", "trend_week")
  
  predictors_05 <- as(predictors_05,"SpatRaster")
  
  levels(predictors_05$land_use) <- data.frame(ID=c(1000,1500,1600,1700,1800,1900), 
                                               land_use=c("1000","1500","1600","1700","1800","1900"))
  levels(predictors_05$soil_type) <- data.frame(ID=c(1,2,3,4), 
                                                soil_type=c("1","2","3","4"))

  prediction_05 <- predict(predictors_05,ffsmodel,na.rm=TRUE)
  
  AOAst05 <- aoa(predictors_05, ffsmodel, trainDI = DIst05)
  
  AOAst05mask = AOAst05$AOA
  
  DIst05mask = AOAst05$DI
  
  prediction_05_AOA <- prediction_05
  
  prediction_05_AOA[!AOAst05mask] <- NA
  
  
  ################
  
  
  predictors_15 = stack(temp_spatially, precipitation_spatially, radiation_spatially, humidity_spatially,
                        temp_spatially_3, temp_spatially_6, temp_spatially_12, 
                        temp_spatially_3_month, temp_spatially_month,
                        trend_3_month_spatially, trend_month_spatially,prec_sum_4_month_spatially,
                        landuse_spatially, type_spatially,elevation_spatially, 
                        depths_15, temp_spatially_day, temp_spatially_72, trend_week_spatially)
  
  names(predictors_15) <- c("air_temperature_mountain","precipitation","global_radiation","relative_humidity",
                            "air_temperature_mountain_3","air_temperature_mountain_6","air_temperature_mountain_12",
                            "air_temperature_3_month","air_temperature_month",
                            "trend_3month","trend_month", "prec_sum_4_month",
                            "land_use","soil_type","elevation", 
                             "depths", "air_temperature_day", "air_temperature_mountain_72", "trend_week")
  
  predictors_15 <- as(predictors_15,"SpatRaster")
  
  levels(predictors_15$land_use) <- data.frame(ID=c(1000,1500,1600,1700,1800,1900), 
                                               land_use=c("1000","1500","1600","1700","1800","1900"))
  levels(predictors_15$soil_type) <- data.frame(ID=c(1,2,3,4), 
                                                soil_type=c("1","2","3","4"))
  
  prediction_15 <- predict(predictors_15,ffsmodel,na.rm=TRUE)
  
  AOAst15 <- aoa(predictors_15, ffsmodel, trainDI = DIst15)
  
  AOAst15mask = AOAst15$AOA
  
  DIst15mask = AOAst15$DI
  
  prediction_15_AOA <- prediction_15
  
  prediction_15_AOA[!AOAst15mask] <- NA
  

  ################
  
  
  predictors_25 = stack(temp_spatially, precipitation_spatially, radiation_spatially, humidity_spatially,
                        temp_spatially_3, temp_spatially_6, temp_spatially_12, 
                        temp_spatially_3_month, temp_spatially_month,
                        trend_3_month_spatially, trend_month_spatially,prec_sum_4_month_spatially,
                        landuse_spatially, type_spatially,elevation_spatially, 
                        depths_25, temp_spatially_day, temp_spatially_72, trend_week_spatially)
  
  names(predictors_25) <- c("air_temperature_mountain","precipitation","global_radiation","relative_humidity",
                            "air_temperature_mountain_3","air_temperature_mountain_6","air_temperature_mountain_12",
                            "air_temperature_3_month","air_temperature_month",
                            "trend_3month","trend_month", "prec_sum_4_month",
                            "land_use","soil_type","elevation", 
                             "depths", "air_temperature_day", "air_temperature_mountain_72", "trend_week")
  
  predictors_25 <- as(predictors_25,"SpatRaster")
  
  levels(predictors_25$land_use) <- data.frame(ID=c(1000,1500,1600,1700,1800,1900), 
                                               land_use=c("1000","1500","1600","1700","1800","1900"))
  levels(predictors_25$soil_type) <- data.frame(ID=c(1,2,3,4), 
                                                soil_type=c("1","2","3","4"))
  
  prediction_25 <- predict(predictors_25,ffsmodel,na.rm=TRUE)
  
  AOAst25 <- aoa(predictors_25, ffsmodel, trainDI = DIst25)
  
  AOAst25mask = AOAst25$AOA
  
  DIst25mask = AOAst25$DI
  
  prediction_25_AOA <- prediction_25
  
  prediction_25_AOA[!AOAst25mask] <- NA
  
  
  ################
  
  
  predictors_35 = stack(temp_spatially, precipitation_spatially, radiation_spatially, humidity_spatially,
                        temp_spatially_3, temp_spatially_6, temp_spatially_12, 
                        temp_spatially_3_month, temp_spatially_month,
                        trend_3_month_spatially, trend_month_spatially,prec_sum_4_month_spatially,
                        landuse_spatially, type_spatially,elevation_spatially, 
                        depths_35, temp_spatially_day, temp_spatially_72, trend_week_spatially)
  
  names(predictors_35) <- c("air_temperature_mountain","precipitation","global_radiation","relative_humidity",
                            "air_temperature_mountain_3","air_temperature_mountain_6","air_temperature_mountain_12",
                            "air_temperature_3_month","air_temperature_month",
                            "trend_3month","trend_month", "prec_sum_4_month",
                            "land_use","soil_type","elevation", 
                             "depths", "air_temperature_day", "air_temperature_mountain_72", "trend_week")
  
  predictors_35 <- as(predictors_35,"SpatRaster")
  
  levels(predictors_35$land_use) <- data.frame(ID=c(1000,1500,1600,1700,1800,1900), 
                                               land_use=c("1000","1500","1600","1700","1800","1900"))
  levels(predictors_35$soil_type) <- data.frame(ID=c(1,2,3,4), 
                                                soil_type=c("1","2","3","4"))
  
  prediction_35 <- predict(predictors_35,ffsmodel,na.rm=TRUE)
  
  AOAst35 <- aoa(predictors_35, ffsmodel, trainDI = DIst35)
  
  AOAst35mask = AOAst35$AOA
  
  DIst35mask = AOAst35$DI
  
  prediction_35_AOA <- prediction_35
  
  prediction_35_AOA[!AOAst35mask] <- NA
  
  
  ################
  
  
  predictors_45 = stack(temp_spatially, precipitation_spatially, radiation_spatially, humidity_spatially,
                        temp_spatially_3, temp_spatially_6, temp_spatially_12, 
                        temp_spatially_3_month, temp_spatially_month,
                        trend_3_month_spatially, trend_month_spatially,prec_sum_4_month_spatially,
                        landuse_spatially, type_spatially,elevation_spatially, 
                        depths_45, temp_spatially_day, temp_spatially_72, trend_week_spatially)
  
  names(predictors_45) <- c("air_temperature_mountain","precipitation","global_radiation","relative_humidity",
                            "air_temperature_mountain_3","air_temperature_mountain_6","air_temperature_mountain_12",
                            "air_temperature_3_month","air_temperature_month",
                            "trend_3month","trend_month", "prec_sum_4_month",
                            "land_use","soil_type","elevation", 
                             "depths", "air_temperature_day", "air_temperature_mountain_72", "trend_week")
  
  predictors_45 <- as(predictors_45,"SpatRaster")
  
  levels(predictors_45$land_use) <- data.frame(ID=c(1000,1500,1600,1700,1800,1900), 
                                               land_use=c("1000","1500","1600","1700","1800","1900"))
  levels(predictors_45$soil_type) <- data.frame(ID=c(1,2,3,4), 
                                                soil_type=c("1","2","3","4"))
  
  prediction_45 <- predict(predictors_45,ffsmodel,na.rm=TRUE)
  
  AOAst45 <- aoa(predictors_45, ffsmodel, trainDI = DIst45)
  
  AOAst45mask = AOAst45$AOA
  
  DIst45mask = AOAst45$DI
  
  prediction_45_AOA <- prediction_45
  
  prediction_45_AOA[!AOAst45mask] <- NA
  
  
  ################
  
  
  predictors_55 = stack(temp_spatially, precipitation_spatially, radiation_spatially, humidity_spatially,
                        temp_spatially_3, temp_spatially_6, temp_spatially_12, 
                        temp_spatially_3_month, temp_spatially_month,
                        trend_3_month_spatially, trend_month_spatially,prec_sum_4_month_spatially,
                        landuse_spatially, type_spatially,elevation_spatially, 
                        depths_55, temp_spatially_day, temp_spatially_72, trend_week_spatially)
  
  names(predictors_55) <- c("air_temperature_mountain","precipitation","global_radiation","relative_humidity",
                            "air_temperature_mountain_3","air_temperature_mountain_6","air_temperature_mountain_12",
                            "air_temperature_3_month","air_temperature_month",
                            "trend_3month","trend_month", "prec_sum_4_month",
                            "land_use","soil_type","elevation", 
                             "depths", "air_temperature_day", "air_temperature_mountain_72", "trend_week")
  
  predictors_55 <- as(predictors_55,"SpatRaster")
  
  levels(predictors_55$land_use) <- data.frame(ID=c(1000,1500,1600,1700,1800,1900), 
                                               land_use=c("1000","1500","1600","1700","1800","1900"))
  levels(predictors_55$soil_type) <- data.frame(ID=c(1,2,3,4), 
                                                soil_type=c("1","2","3","4"))
  
  prediction_55 <- predict(predictors_55,ffsmodel,na.rm=TRUE)
  
  AOAst55 <- aoa(predictors_55, ffsmodel, trainDI = DIst55)
  
  AOAst55mask = AOAst55$AOA
  
  DIst55mask = AOAst55$DI
  
  prediction_55_AOA <- prediction_55
  
  prediction_55_AOA[!AOAst55mask] <- NA
  
  
  ################
  
  
  predictors_65 = stack(temp_spatially, precipitation_spatially, radiation_spatially, humidity_spatially,
                        temp_spatially_3, temp_spatially_6, temp_spatially_12, 
                        temp_spatially_3_month, temp_spatially_month,
                        trend_3_month_spatially, trend_month_spatially,prec_sum_4_month_spatially,
                        landuse_spatially, type_spatially,elevation_spatially, 
                        depths_65, temp_spatially_day, temp_spatially_72, trend_week_spatially)
  
  names(predictors_65) <- c("air_temperature_mountain","precipitation","global_radiation","relative_humidity",
                            "air_temperature_mountain_3","air_temperature_mountain_6","air_temperature_mountain_12",
                            "air_temperature_3_month","air_temperature_month",
                            "trend_3month","trend_month", "prec_sum_4_month",
                            "land_use","soil_type","elevation", 
                             "depths", "air_temperature_day", "air_temperature_mountain_72", "trend_week")
  
  predictors_65 <- as(predictors_65,"SpatRaster")
  
  levels(predictors_65$land_use) <- data.frame(ID=c(1000,1500,1600,1700,1800,1900), 
                                               land_use=c("1000","1500","1600","1700","1800","1900"))
  levels(predictors_65$soil_type) <- data.frame(ID=c(1,2,3,4), 
                                                soil_type=c("1","2","3","4"))
  
  prediction_65 <- predict(predictors_65,ffsmodel,na.rm=TRUE)
  
  AOAst65 <- aoa(predictors_65, ffsmodel, trainDI = DIst65)
  
  AOAst65mask = AOAst65$AOA
  
  DIst65mask = AOAst65$DI
  
  prediction_65_AOA <- prediction_65
  
  prediction_65_AOA[!AOAst65mask] <- NA
  
  
  ################
  
  
  predictors_75 = stack(temp_spatially, precipitation_spatially, radiation_spatially, humidity_spatially,
                        temp_spatially_3, temp_spatially_6, temp_spatially_12, 
                        temp_spatially_3_month, temp_spatially_month,
                        trend_3_month_spatially, trend_month_spatially,prec_sum_4_month_spatially,
                        landuse_spatially, type_spatially,elevation_spatially, 
                        depths_75, temp_spatially_day, temp_spatially_72, trend_week_spatially)
  
  names(predictors_75) <- c("air_temperature_mountain","precipitation","global_radiation","relative_humidity",
                            "air_temperature_mountain_3","air_temperature_mountain_6","air_temperature_mountain_12",
                            "air_temperature_3_month","air_temperature_month",
                            "trend_3month","trend_month", "prec_sum_4_month",
                            "land_use","soil_type","elevation", 
                             "depths", "air_temperature_day", "air_temperature_mountain_72", "trend_week")
  
  predictors_75 <- as(predictors_75,"SpatRaster")
  
  levels(predictors_75$land_use) <- data.frame(ID=c(1000,1500,1600,1700,1800,1900), 
                                               land_use=c("1000","1500","1600","1700","1800","1900"))
  levels(predictors_75$soil_type) <- data.frame(ID=c(1,2,3,4), 
                                                soil_type=c("1","2","3","4"))
  
  prediction_75 <- predict(predictors_75,ffsmodel,na.rm=TRUE)
  
  AOAst75 <- aoa(predictors_75, ffsmodel, trainDI = DIst75)
  
  AOAst75mask = AOAst75$AOA
  
  DIst75mask = AOAst75$DI
  
  prediction_75_AOA <- prediction_75
  
  prediction_75_AOA[!AOAst75mask] <- NA
  
  
  ################
  
  
  predictors_85 = stack(temp_spatially, precipitation_spatially, radiation_spatially, humidity_spatially,
                        temp_spatially_3, temp_spatially_6, temp_spatially_12, 
                        temp_spatially_3_month, temp_spatially_month,
                        trend_3_month_spatially, trend_month_spatially,prec_sum_4_month_spatially,
                        landuse_spatially, type_spatially,elevation_spatially, 
                        depths_85, temp_spatially_day, temp_spatially_72, trend_week_spatially)
  
  names(predictors_85) <- c("air_temperature_mountain","precipitation","global_radiation","relative_humidity",
                            "air_temperature_mountain_3","air_temperature_mountain_6","air_temperature_mountain_12",
                            "air_temperature_3_month","air_temperature_month",
                            "trend_3month","trend_month", "prec_sum_4_month",
                            "land_use","soil_type","elevation", 
                             "depths", "air_temperature_day", "air_temperature_mountain_72", "trend_week")
  
  predictors_85 <- as(predictors_85,"SpatRaster")
  
  levels(predictors_85$land_use) <- data.frame(ID=c(1000,1500,1600,1700,1800,1900), 
                                               land_use=c("1000","1500","1600","1700","1800","1900"))
  levels(predictors_85$soil_type) <- data.frame(ID=c(1,2,3,4), 
                                                soil_type=c("1","2","3","4"))
  
  prediction_85 <- predict(predictors_85,ffsmodel,na.rm=TRUE)
  
  AOAst85 <- aoa(predictors_85, ffsmodel, trainDI = DIst85)
  
  AOAst85mask = AOAst85$AOA
  
  DIst85mask = AOAst85$DI
  
  prediction_85_AOA <- prediction_85
  
  prediction_85_AOA[!AOAst85mask] <- NA
  
  
  ##############################################
  #stacking predictions of all depths
  
  pred_st = c(prediction_05,prediction_15,prediction_25,prediction_35,prediction_45,prediction_55,
     prediction_65,prediction_75,prediction_85)

  names(pred_st) = c(paste0(time,"_05"),
                     paste0(time,"_15"),
                     paste0(time,"_25"),
                     paste0(time,"_35"),
                     paste0(time,"_45"),
                     paste0(time,"_55"),
                     paste0(time,"_65"),
                     paste0(time,"_75"),
                     paste0(time,"_85"))
  
  
  pred_st_AOA = c(prediction_05_AOA,prediction_15_AOA,prediction_25_AOA,prediction_35_AOA,prediction_45_AOA,prediction_55_AOA,
                  prediction_65_AOA,prediction_75_AOA,prediction_85_AOA)
  
  names(pred_st_AOA) = c(paste0(time,"_05"),
                         paste0(time,"_15"),
                         paste0(time,"_25"),
                         paste0(time,"_35"),
                         paste0(time,"_45"),
                         paste0(time,"_55"),
                         paste0(time,"_65"),
                         paste0(time,"_75"),
                         paste0(time,"_85"))
  
  AOA_mask_st = c(AOAst05mask,AOAst15mask,AOAst25mask,AOAst35mask,AOAst45mask,AOAst55mask,AOAst65mask,AOAst75mask,AOAst85mask)
  
  names(AOA_mask_st) = c(paste0(time,"_05"),
                         paste0(time,"_15"),
                         paste0(time,"_25"),
                         paste0(time,"_35"),
                         paste0(time,"_45"),
                         paste0(time,"_55"),
                         paste0(time,"_65"),
                         paste0(time,"_75"),
                         paste0(time,"_85"))
  
  
  DI_mask_st = c(DIst05mask,DIst15mask,DIst25mask,DIst35mask,DIst45mask,DIst55mask,DIst65mask,DIst75mask,DIst85mask)
  
  names(DI_mask_st) = c(paste0(time,"_05"),
                        paste0(time,"_15"),
                        paste0(time,"_25"),
                        paste0(time,"_35"),
                        paste0(time,"_45"),
                        paste0(time,"_55"),
                        paste0(time,"_65"),
                        paste0(time,"_75"),
                        paste0(time,"_85"))
  
  
  directory_spatial_pred <- "" #path to save spatial predictions
  
  writeRaster(pred_st, file.path(directory_spatial_pred, paste0("pred_st_", time, ".tif")), overwrite=TRUE)
  
  writeRaster(pred_st_AOA, file.path(directory_spatial_pred, paste0("pred_st_AOA_", time, ".tif")), overwrite=TRUE)
  
  writeRaster(AOA_mask_st, file.path(directory_spatial_pred, paste0("AOA_mask_st_", time, ".tif")), overwrite=TRUE)
  
  writeRaster(DI_mask_st, file.path(directory_spatial_pred, paste0("DI_mask_st_", time, ".tif")), overwrite=TRUE)  
  
  
}