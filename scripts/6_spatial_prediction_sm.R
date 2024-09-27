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

load(file.path(directory, "models/rfmodel_sm.Rdata"))
rfmodel
ffsmodel <- rfmodel

rm(rfmodel)

##########################################
#load the trainDI calculated in script 5

directory_di <- "" #fill in the directory to the folder of the calculated trainDIs 

DIsm05 = readRDS(file.path(directory_di,"DIsm05.rds"))
DIsm15 = readRDS(file.path(directory_di,"DIsm15.rds"))
DIsm25 = readRDS(file.path(directory_di,"DIsm25.rds"))
DIsm35 = readRDS(file.path(directory_di,"DIsm35.rds"))
DIsm45 = readRDS(file.path(directory_di,"DIsm45.rds"))
DIsm55 = readRDS(file.path(directory_di,"DIsm55.rds"))
DIsm65 = readRDS(file.path(directory_di,"DIsm65.rds"))
DIsm75 = readRDS(file.path(directory_di,"DIsm75.rds"))
DIsm85 = readRDS(file.path(directory_di,"DIsm85.rds"))

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
meteo_data$air_temperature_month = zoo::rollmean(meteo_data$air_temperature_mountain, k = 720, fill = NA)
meteo_data$air_temperature_3_month = zoo::rollmean(meteo_data$air_temperature_mountain, k = 2160, fill = NA)

#precipitaion sums
meteo_data$prec_sum_3_month = zoo::rollsum(meteo_data$precipitation, 168*12, align = "right", fill=NA)
meteo_data$prec_sum_4_month = zoo::rollsum(meteo_data$precipitation, 168*16, align = "right", fill=NA)
meteo_data$prec_sum_5_month = zoo::rollsum(meteo_data$precipitation, 168*20, align = "right", fill=NA)

#ndvi
ndvi = terra::rast(file.path(directory_pred,"ndvi.tif")) #file with satialy and temporally interpolatetd ndvi (not provided)

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
  
#ndvi preparation
  ndvi_day_befor = ndvi[[doi-1]]
  ndvi_day = ndvi[[doi]]
  ndvi_day_after = ndvi[[doi+1]]
  empty_raster = ndvi[[1]]*NA
  gap <- rast(lapply(1:23, function(i) empty_raster))
  ndvi_gap = c(ndvi_day_befor,gap,ndvi_day,gap,ndvi_day_after)
  ndvi_49 = approximate(ndvi_gap, rule=2)

#create spatially continous data from predictors  
  ndvi_spatially = stack(ndvi_49[[13+hour]])
  ndvi_spatially = resample(ndvi_spatially,elevation)
  
  prec_sum_3_month_spatially = raster_base + meteo_data$prec_sum_3_month[position]
  radiation_spatially = raster_base+meteo_data$global_radiation[position]
  precipitation_spatially = raster_base+meteo_data$precipitation[position]
  temp_spatially_72 = round(meteo_data$air_temperature_mountain[position-72]-((meteo_data$air_temperature_mountain[position-72]-meteo_data$air_temperature_valley[position-72])/(elevation_mountain-elevation_valley))*(elevation_mountain-elevation),2)
  temp_spatially_3_month = raster_base+meteo_data$air_temperature_3_month[position]
  
  texture_spatially = static_raster$soil_texture
  type_spatially = static_raster$soil_type
  landuse_spatially = static_raster$land_use
  inclination_spatially = static_raster$inclination
  wettness_spatially = static_raster$topo_wetness

  date = strftime(meteo_data$datetime[i], "%Y%m%d")
  date_day_before = strftime(meteo_data$datetime[i-24], "%Y%m%d")
  date_day_before
  
#prepare radolan data and calculate multiple hour sums  
  radolan_files_selected =  radolan_files[grepl(date,radolan_files)]
  radolan_raster100 = stack(radolan_files_selected)
  radolan_files_selected_day_before =  radolan_files[grepl(date_day_before,radolan_files)]
  radolan_raster_day_before100 = stack(radolan_files_selected_day_before)
  time = strftime(meteo_data$datetime[i], "%Y%m%d%H%M")
  time_day_before = strftime(meteo_data$datetime[i-24], "%Y%m%d%H%M")
  time_day_before
  hour = as.numeric(strftime(meteo_data$datetime[i], "%H"))
  radolan_raster = radolan_raster100/100
  radolan_raster_day_before = radolan_raster_day_before100/100
  radolan = radolan_raster[[as.numeric(hour)+1]]
  
  if(hour < 23){
  radolan_sum_0_24 = sum(radolan_raster_day_before[[(as.numeric(hour)+2):24]],radolan_raster[[1:(as.numeric(hour)+1)]])
  }
  
  if(hour == 23){
    radolan_sum_0_24 = sum(radolan_raster[[1:24]])
  }
  
  if(hour >= 5){
    radolan_sum_0_6 = sum(radolan_raster[[(as.numeric(hour)-4):(as.numeric(hour)+1)]])
  }
  if(hour < 5){
    radolan_sum_0_6 = sum(radolan_raster_day_before[[(19+as.numeric(hour)):24]],radolan_raster[[1:(as.numeric(hour)+1)]])
  }
  
  
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

  predictors_05 = stack(temp_spatially_72,temp_spatially_3_month,precipitation_spatially,radiation_spatially,
                        radolan, radolan_sum_0_6 ,radolan_sum_0_24, prec_sum_3_month_spatially,
                        texture_spatially, type_spatially, landuse_spatially, inclination_spatially, wettness_spatially, ndvi_spatially,depths_05)
    
  names(predictors_05) <- c("air_temperature_mountain_72","air_temperature_3_month","precipitation","global_radiation",
                           "radolan", "radolan_sum_0_6","radolan_sum_0_24","prec_sum_3_month",
                           "soil_texture","soil_type","land_use","inclination","topo_wetness","ndvi","depths")
  
  predictors_05 <- as(predictors_05,"SpatRaster")
  
  levels(predictors_05$land_use) <- data.frame(ID=c(1000,1500,1600,1700,1800,1900), 
                                               land_use=c("1000","1500","1600","1700","1800","1900"))
  levels(predictors_05$soil_type) <- data.frame(ID=c(1,2,3,4), 
                                                soil_type=c("1","2","3","4"))
  levels(predictors_05$soil_texture) <- data.frame(ID=c(10,20,30,40,50,60), 
                                                   soil_texture=c("10","20","30","40","50","60"))
  
  prediction_05 <- predict(predictors_05,ffsmodel,na.rm=TRUE)
  
  AOAsm05 <- aoa(predictors_05, ffsmodel, trainDI = DIsm05)

  AOAsm05mask = AOAsm05$AOA
  
  DIsm05mask = AOAsm05$DI
  
  prediction_05_AOA <- prediction_05
  
  prediction_05_AOA[!AOAsm05mask] <- NA

  
  ################

  
  predictors_15 =stack(temp_spatially_72,temp_spatially_3_month,precipitation_spatially,radiation_spatially,
                       radolan, radolan_sum_0_6,  radolan_sum_0_24, prec_sum_3_month_spatially,
                       texture_spatially, type_spatially, landuse_spatially, inclination_spatially, wettness_spatially, ndvi_spatially,depths_15)
  
  names(predictors_15) <- c("air_temperature_mountain_72","air_temperature_3_month","precipitation","global_radiation",
                            "radolan", "radolan_sum_0_6",  "radolan_sum_0_24","prec_sum_3_month",
                            "soil_texture","soil_type","land_use","inclination","topo_wetness","ndvi","depths")
  
  predictors_15 <- as(predictors_15,"SpatRaster")
  
  levels(predictors_15$land_use) <- data.frame(ID=c(1000,1500,1600,1700,1800,1900), 
                                               land_use=c("1000","1500","1600","1700","1800","1900"))
  levels(predictors_15$soil_type) <- data.frame(ID=c(1,2,3,4), 
                                                soil_type=c("1","2","3","4"))
  levels(predictors_15$soil_texture) <- data.frame(ID=c(10,20,30,40,50,60), 
                                                   soil_texture=c("10","20","30","40","50","60"))
  
  prediction_15 <- predict(predictors_15,ffsmodel,na.rm=TRUE)
  
  AOAsm15 <- aoa(predictors_15, ffsmodel, trainDI = DIsm15)
  
  AOAsm15mask = AOAsm15$AOA
  
  DIsm15mask = AOAsm15$DI
  
  prediction_15_AOA <- prediction_15
  
  prediction_15_AOA[!AOAsm15mask] <- NA

  
  ################
  
  
  predictors_25 = stack(temp_spatially_72,temp_spatially_3_month,precipitation_spatially,radiation_spatially,
                        radolan, radolan_sum_0_6,  radolan_sum_0_24, prec_sum_3_month_spatially,
                        texture_spatially, type_spatially, landuse_spatially, inclination_spatially, wettness_spatially, ndvi_spatially,depths_25)
  
  names(predictors_25) <- c("air_temperature_mountain_72","air_temperature_3_month","precipitation","global_radiation",
                            "radolan", "radolan_sum_0_6",  "radolan_sum_0_24","prec_sum_3_month",
                            "soil_texture","soil_type","land_use","inclination","topo_wetness","ndvi","depths")
  
  predictors_25 <- as(predictors_25,"SpatRaster")
  
  levels(predictors_25$land_use) <- data.frame(ID=c(1000,1500,1600,1700,1800,1900), 
                                               land_use=c("1000","1500","1600","1700","1800","1900"))
  levels(predictors_25$soil_type) <- data.frame(ID=c(1,2,3,4), 
                                                soil_type=c("1","2","3","4"))
  levels(predictors_25$soil_texture) <- data.frame(ID=c(10,20,30,40,50,60), 
                                                   soil_texture=c("10","20","30","40","50","60"))
  
  prediction_25 <- predict(predictors_25,ffsmodel,na.rm=TRUE)
  
  AOAsm25 <- aoa(predictors_25, ffsmodel, trainDI = DIsm25)
  
  AOAsm25mask = AOAsm25$AOA
  
  DIsm25mask = AOAsm25$DI
  
  prediction_25_AOA <- prediction_25
  
  prediction_25_AOA[!AOAsm25mask] <- NA


  ##################

    
  predictors_35 = stack(temp_spatially_72,temp_spatially_3_month,precipitation_spatially,radiation_spatially,
                        radolan, radolan_sum_0_6,  radolan_sum_0_24, prec_sum_3_month_spatially,
                        texture_spatially, type_spatially, landuse_spatially, inclination_spatially, wettness_spatially, ndvi_spatially,depths_35)
  
  names(predictors_35) <- c("air_temperature_mountain_72","air_temperature_3_month","precipitation","global_radiation",
                            "radolan", "radolan_sum_0_6",  "radolan_sum_0_24","prec_sum_3_month",
                            "soil_texture","soil_type","land_use","inclination","topo_wetness","ndvi","depths")
  
  predictors_35 <- as(predictors_35,"SpatRaster")
  
  levels(predictors_35$land_use) <- data.frame(ID=c(1000,1500,1600,1700,1800,1900), 
                                               land_use=c("1000","1500","1600","1700","1800","1900"))
  levels(predictors_35$soil_type) <- data.frame(ID=c(1,2,3,4), 
                                                soil_type=c("1","2","3","4"))
  levels(predictors_35$soil_texture) <- data.frame(ID=c(10,20,30,40,50,60), 
                                                   soil_texture=c("10","20","30","40","50","60"))
  
  prediction_35 <- predict(predictors_35,ffsmodel,na.rm=TRUE)
  
  AOAsm35 <- aoa(predictors_35, ffsmodel, trainDI = DIsm35)
  
  AOAsm35mask = AOAsm35$AOA
  
  DIsm35mask = AOAsm35$DI
  
  prediction_35_AOA <- prediction_35
  
  prediction_35_AOA[!AOAsm35mask] <- NA
  
  
  ##################
  
  
  predictors_45 = stack(temp_spatially_72,temp_spatially_3_month,precipitation_spatially,radiation_spatially,
                        radolan, radolan_sum_0_6,  radolan_sum_0_24, prec_sum_3_month_spatially,
                        texture_spatially, type_spatially, landuse_spatially, inclination_spatially, wettness_spatially, ndvi_spatially,depths_45)
  
  names(predictors_45) <- c("air_temperature_mountain_72","air_temperature_3_month","precipitation","global_radiation",
                            "radolan", "radolan_sum_0_6",  "radolan_sum_0_24","prec_sum_3_month",
                            "soil_texture","soil_type","land_use","inclination","topo_wetness","ndvi","depths")
  
  predictors_45 <- as(predictors_45,"SpatRaster")
  
  levels(predictors_45$land_use) <- data.frame(ID=c(1000,1500,1600,1700,1800,1900), 
                                               land_use=c("1000","1500","1600","1700","1800","1900"))
  levels(predictors_45$soil_type) <- data.frame(ID=c(1,2,3,4), 
                                                soil_type=c("1","2","3","4"))
  levels(predictors_45$soil_texture) <- data.frame(ID=c(10,20,30,40,50,60), 
                                                   soil_texture=c("10","20","30","40","50","60"))
  
  prediction_45 <- predict(predictors_45,ffsmodel,na.rm=TRUE)
  
  AOAsm45 <- aoa(predictors_45, ffsmodel, trainDI = DIsm45)
  
  AOAsm45mask = AOAsm45$AOA
  
  DIsm45mask = AOAsm45$DI
  
  prediction_45_AOA <- prediction_45
  
  prediction_45_AOA[!AOAsm45mask] <- NA

  
  ##################
  
  
  predictors_55 = stack(temp_spatially_72,temp_spatially_3_month,precipitation_spatially,radiation_spatially,
                        radolan, radolan_sum_0_6,  radolan_sum_0_24, prec_sum_3_month_spatially,
                        texture_spatially, type_spatially, landuse_spatially, inclination_spatially, wettness_spatially, ndvi_spatially,depths_55)
  
  names(predictors_55) <- c("air_temperature_mountain_72","air_temperature_3_month","precipitation","global_radiation",
                            "radolan", "radolan_sum_0_6",  "radolan_sum_0_24","prec_sum_3_month",
                            "soil_texture","soil_type","land_use","inclination","topo_wetness","ndvi","depths")
  
  predictors_55 <- as(predictors_55,"SpatRaster")
  
  levels(predictors_55$land_use) <- data.frame(ID=c(1000,1500,1600,1700,1800,1900), 
                                               land_use=c("1000","1500","1600","1700","1800","1900"))
  levels(predictors_55$soil_type) <- data.frame(ID=c(1,2,3,4), 
                                                soil_type=c("1","2","3","4"))
  levels(predictors_55$soil_texture) <- data.frame(ID=c(10,20,30,40,50,60), 
                                                   soil_texture=c("10","20","30","40","50","60"))
  
  prediction_55 <- predict(predictors_55,ffsmodel,na.rm=TRUE)
  
  AOAsm55 <- aoa(predictors_55, ffsmodel, trainDI = DIsm55)
  
  AOAsm55mask = AOAsm55$AOA
  
  DIsm55mask = AOAsm55$DI
  
  prediction_55_AOA <- prediction_55
  
  prediction_55_AOA[!AOAsm55mask] <- NA
  
  
  ##################
  
  
  predictors_65 = stack(temp_spatially_72,temp_spatially_3_month,precipitation_spatially,radiation_spatially,
                        radolan, radolan_sum_0_6,  radolan_sum_0_24, prec_sum_3_month_spatially,
                        texture_spatially, type_spatially, landuse_spatially, inclination_spatially, wettness_spatially, ndvi_spatially,depths_65)
  
  names(predictors_65) <- c("air_temperature_mountain_72","air_temperature_3_month","precipitation","global_radiation",
                            "radolan", "radolan_sum_0_6",  "radolan_sum_0_24","prec_sum_3_month",
                            "soil_texture","soil_type","land_use","inclination","topo_wetness","ndvi","depths")
  
  predictors_65 <- as(predictors_65,"SpatRaster")
  
  levels(predictors_65$land_use) <- data.frame(ID=c(1000,1500,1600,1700,1800,1900), 
                                               land_use=c("1000","1500","1600","1700","1800","1900"))
  levels(predictors_65$soil_type) <- data.frame(ID=c(1,2,3,4), 
                                                soil_type=c("1","2","3","4"))
  levels(predictors_65$soil_texture) <- data.frame(ID=c(10,20,30,40,50,60), 
                                                   soil_texture=c("10","20","30","40","50","60"))
  
  prediction_65 <- predict(predictors_65,ffsmodel,na.rm=TRUE)
  
  AOAsm65 <- aoa(predictors_65, ffsmodel, trainDI = DIsm65)
  
  AOAsm65mask = AOAsm65$AOA
  
  DIsm65mask = AOAsm65$DI
  
  prediction_65_AOA <- prediction_65
  
  prediction_65_AOA[!AOAsm65mask] <- NA

  
  ##################
  
  
  predictors_75 = stack(temp_spatially_72,temp_spatially_3_month,precipitation_spatially,radiation_spatially,
                        radolan, radolan_sum_0_6,  radolan_sum_0_24, prec_sum_3_month_spatially,
                        texture_spatially, type_spatially, landuse_spatially, inclination_spatially, wettness_spatially, ndvi_spatially,depths_75)
  
  names(predictors_75) <- c("air_temperature_mountain_72","air_temperature_3_month","precipitation","global_radiation",
                            "radolan", "radolan_sum_0_6",  "radolan_sum_0_24","prec_sum_3_month",
                            "soil_texture","soil_type","land_use","inclination","topo_wetness","ndvi","depths")
  
  predictors_75 <- as(predictors_75,"SpatRaster")
  
  levels(predictors_75$land_use) <- data.frame(ID=c(1000,1500,1600,1700,1800,1900), 
                                               land_use=c("1000","1500","1600","1700","1800","1900"))
  levels(predictors_75$soil_type) <- data.frame(ID=c(1,2,3,4), 
                                                soil_type=c("1","2","3","4"))
  levels(predictors_75$soil_texture) <- data.frame(ID=c(10,20,30,40,50,60), 
                                                   soil_texture=c("10","20","30","40","50","60"))
  
  prediction_75 <- predict(predictors_75,ffsmodel,na.rm=TRUE)
  
  AOAsm75 <- aoa(predictors_75, ffsmodel, trainDI = DIsm75)
  
  AOAsm75mask = AOAsm75$AOA
  
  DIsm75mask = AOAsm75$DI
  
  prediction_75_AOA <- prediction_75
  
  prediction_75_AOA[!AOAsm75mask] <- NA
  
  
  ##################
  
  predictors_85 = stack(temp_spatially_72,temp_spatially_3_month,precipitation_spatially,radiation_spatially,
                        radolan, radolan_sum_0_6,  radolan_sum_0_24, prec_sum_3_month_spatially,
                        texture_spatially, type_spatially, landuse_spatially, inclination_spatially, wettness_spatially, ndvi_spatially,depths_85)
  
  names(predictors_85) <- c("air_temperature_mountain_72","air_temperature_3_month","precipitation","global_radiation",
                            "radolan", "radolan_sum_0_6",  "radolan_sum_0_24","prec_sum_3_month",
                            "soil_texture","soil_type","land_use","inclination","topo_wetness","ndvi","depths")
  
  predictors_85 <- as(predictors_85,"SpatRaster")
  
  levels(predictors_85$land_use) <- data.frame(ID=c(1000,1500,1600,1700,1800,1900), 
                                               land_use=c("1000","1500","1600","1700","1800","1900"))
  levels(predictors_85$soil_type) <- data.frame(ID=c(1,2,3,4), 
                                                soil_type=c("1","2","3","4"))
  levels(predictors_85$soil_texture) <- data.frame(ID=c(10,20,30,40,50,60), 
                                                    soil_texture=c("10","20","30","40","50","60"))
  
  prediction_85 <- predict(predictors_85,ffsmodel,na.rm=TRUE)
  
  AOAsm85 <- aoa(predictors_85, ffsmodel, trainDI = DIsm85)
  
  AOAsm85mask = AOAsm85$AOA
  
  DIsm85mask = AOAsm85$DI
  
  prediction_85_AOA <- prediction_85
  
  prediction_85_AOA[!AOAsm85mask] <- NA

  
  ##############################################
  #stacking predictions of all depths
  
  
  pred_sm = c(prediction_05,prediction_15,prediction_25,prediction_35,prediction_45,prediction_55,
     prediction_65,prediction_75,prediction_85)

  names(pred_sm) = c(paste0(time,"_05"),
                     paste0(time,"_15"),
                     paste0(time,"_25"),
                     paste0(time,"_35"),
                     paste0(time,"_45"),
                     paste0(time,"_55"),
                     paste0(time,"_65"),
                     paste0(time,"_75"),
                     paste0(time,"_85"))
  
  pred_sm_AOA = c(prediction_05_AOA,prediction_15_AOA,prediction_25_AOA,prediction_35_AOA,prediction_45_AOA,prediction_55_AOA,
              prediction_65_AOA,prediction_75_AOA,prediction_85_AOA)
  
  names(pred_sm_AOA) = c(paste0(time,"_05"),
                     paste0(time,"_15"),
                     paste0(time,"_25"),
                     paste0(time,"_35"),
                     paste0(time,"_45"),
                     paste0(time,"_55"),
                     paste0(time,"_65"),
                     paste0(time,"_75"),
                     paste0(time,"_85"))
  
  AOA_mask_sm = c(AOAsm05mask,AOAsm15mask,AOAsm25mask,AOAsm35mask,AOAsm45mask,AOAsm55mask,AOAsm65mask,AOAsm75mask,AOAsm85mask)
  
  names(AOA_mask_sm) = c(paste0(time,"_05"),
                         paste0(time,"_15"),
                         paste0(time,"_25"),
                         paste0(time,"_35"),
                         paste0(time,"_45"),
                         paste0(time,"_55"),
                         paste0(time,"_65"),
                         paste0(time,"_75"),
                         paste0(time,"_85"))
  
  
  DI_mask_sm = c(DIsm05mask,DIsm15mask,DIsm25mask,DIsm35mask,DIsm45mask,DIsm55mask,DIsm65mask,DIsm75mask,DIsm85mask)
  
  names(DI_mask_sm) = c(paste0(time,"_05"),
                         paste0(time,"_15"),
                         paste0(time,"_25"),
                         paste0(time,"_35"),
                         paste0(time,"_45"),
                         paste0(time,"_55"),
                         paste0(time,"_65"),
                         paste0(time,"_75"),
                         paste0(time,"_85"))
  
  directory_spatial_pred <- "" #path to save spatial predictions
  
  writeRaster(pred_sm, file.path(directory_spatial_pred, paste0("pred_sm_", time, ".tif")), overwrite=TRUE)
  
  writeRaster(pred_sm_AOA, file.path(directory_spatial_pred, paste0("pred_sm_AOA_", time, ".tif")), overwrite=TRUE)
  
  writeRaster(AOA_mask_sm, file.path(directory_spatial_pred, paste0("AOA_mask_sm_", time, ".tif")), overwrite=TRUE)
  
  writeRaster(DI_mask_sm, file.path(directory_spatial_pred, paste0("DI_mask_sm_", time, ".tif")), overwrite=TRUE)  


}
