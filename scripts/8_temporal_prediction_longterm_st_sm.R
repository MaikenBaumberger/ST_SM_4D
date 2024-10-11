
##########################################
#script for temporal prediction of soil temperature and soil moisture at the permanent measurement locations

##########################################
#packages

library(raster)
library(zoo)
library(mapview)
library(sf)
library(caret)
library(CAST)


##########################################
#load data and models


directory <- "" #fill in the directory to the folder 

load(file.path(directory, "models/rfmodel_st.Rdata"))

rfmodel
rf_model_st <- rfmodel

load(file.path(directory, "models/rfmodel_sm.Rdata"))

rfmodel
rf_model_sm <- rfmodel

rm(rfmodel)

load(file.path(directory, "training_data/data_set_complete.Rdata"))

##########################################
#remove predictors, which are not necessary and reshape the order in the table

predictor_set_temperature = predictor_set_2[ , -which(names(predictor_set_2) %in% c("M_org","M_05","M_15","M_25","M_35","M_45","M_55",
                                                                                    "M_65","M_75","M_85","M_95","M_105","M_115",
                                                                                    "measurement","T_95","T_105","T_115"))]

predictor_set_moisture = predictor_set_2[ , -which(names(predictor_set_2) %in% c("T_org","T_05","T_15","T_25","T_35","T_45","T_55",
                                                                                    "T_65","T_75","T_85","T_95","T_105","T_115",
                                                                                    "measurement","M_95","M_105","M_115"))]

predictor_set_temperature_2 = cbind(predictor_set_temperature[1:2],predictor_set_temperature[61:62],predictor_set_temperature[13:60],predictor_set_temperature[4:12])

predictor_set_moisture_2 = cbind(predictor_set_moisture[1:2],predictor_set_moisture[61:62],predictor_set_moisture[13:60],predictor_set_moisture[4:12])

names(predictor_set_temperature_2)


##########################################
#subset data of 2022

train_st <- subset(predictor_set_temperature_2,
             date_hour >= as.POSIXct('2022-01-01 00:00') &
               date_hour <= as.POSIXct('2022-12-31 23:59'))


train_sm <- subset(predictor_set_moisture_2,
             date_hour >= as.POSIXct('2022-01-01 00:00') &
               date_hour <= as.POSIXct('2022-12-31 23:59'))


##########################################
#convert categorial predictors as factor

train_st$soil_texture = as.factor(train_st$soil_texture)
train_st$soil_type = as.factor(train_st$soil_type)
train_st$land_use = as.factor(train_st$land_use)

train_sm$soil_texture = as.factor(train_sm$soil_texture)
train_sm$soil_type = as.factor(train_sm$soil_type)
train_sm$land_use = as.factor(train_sm$land_use)


##########################################
#reshape soil temperature table by creating a column for depth

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
                                                 "plot_id"
)) 

train_st_melt$depths = substr(train_st_melt$variable, 3,5)

train_st_melt$soil_temperature=train_st_melt$value

train_set_st = train_st_melt[ , -which(names(train_st_melt) %in% c("value","variable"))]

train_set_st$depths = as.numeric(train_set_st$depths)

train_set_st = train_set_st[!duplicated(train_set_st[,c('probe_name', 'date_hour','depths')]),]

train_set_st$soil_temperature <- round(train_set_st$soil_temperature,2)

train_set_st = train_set_st[ , -which(names(train_set_st) %in% c("radolan_sum_week","radolan_sum_24_48"))]

train_set_st=train_set_st[complete.cases(train_set_st), ]

dt_st <- train_set_st

##########################################
#reshape soil moisture table by creating a column for depth

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
                                                 "plot_id"
)) 

train_sm_melt$depths = substr(train_sm_melt$variable, 3,5)

train_sm_melt$soil_moisture=train_sm_melt$value

train_set_sm = train_sm_melt[ , -which(names(train_sm_melt) %in% c("value","variable"))]

train_set_sm$depths = as.numeric(train_set_sm$depths)

train_set_sm = train_set_sm[!duplicated(train_set_sm[,c('probe_name', 'date_hour','depths')]),]

train_set_sm$soil_moisture <- round(train_set_sm$soil_moisture,2)

train_set_sm = train_set_sm[ , -which(names(train_set_sm) %in% c("radolan_sum_week","radolan_sum_24_48"))]

train_set_sm=train_set_sm[complete.cases(train_set_sm), ]

dt_sm <- train_set_sm


#############################################
#predict soil temperautre and soil moisture

dt_st[,"prediction"]=round(predict.train(object=rf_model_st, newdata = dt_st,na.action = na.omit),
                                  digits = 2)

dt_sm[,"prediction"]=round(predict.train(object=rf_model_sm, newdata = dt_sm,na.action = na.omit),
                           digits = 2)


#############################################
#select measurements from waldstein and voitsumra

dt_st_probe_waldstein <- subset(dt_st,probe_name=="Waldstein_2"|probe_name=="Waldstein_3")
dt_sm_probe_waldstein <- subset(dt_sm,probe_name=="Waldstein_2"|probe_name=="Waldstein_3")
dt_st_probe_voitsumra <- subset(dt_st,probe_name=="Voitsumra")
dt_sm_probe_voitsumra <- subset(dt_sm,probe_name=="Voitsumra")

#############################################
#colors for plot

categories <- unique(dt_st_probe_voitsumra$depths)
colors <- viridis::viridis(6) 

colors <- c(colors[1:5],"gold2")

category_colors <- colors[as.numeric(factor(dt_st_probe_voitsumra$depths))]


#############################################
#plot full sequences of 2022 for soil temperature and soil moisture at two different locations


par(mfrow = c(4, 1),mar=c(2.5,4,0.5,0.5))

 
plot(dt_st_probe_waldstein$date_hour,dt_st_probe_waldstein$soil_temperature, col = category_colors, pch = 19,type="n",
     ylab="soil temperature [°C]",xlab="",ylim=c(0,28),xlim=c(dt_st_probe_waldstein$date_hour[22],dt_st_probe_waldstein$date_hour[length(dt_st_probe_waldstein$date_hour)-24]))
for (i in 1:6) {
  cat_data <- dt_st_probe_waldstein[dt_st_probe_waldstein$depths == categories[i], ]
  lines(cat_data$date_hour, cat_data$soil_temperature, col = colors[i], lwd = 2)  
}
 
for (i in 1:6) {
  cat_data <- dt_st_probe_waldstein[dt_st_probe_waldstein$depths == categories[i], ]
  lines(cat_data$date_hour, cat_data$prediction, col = colors[i], lwd = 2,lty = "dashed") 
}

 
plot(dt_sm_probe_waldstein$date_hour,dt_sm_probe_waldstein$soil_moisture, col = category_colors, pch = 19,type="n",
     ylab="soil moisture [%]",xlab="",ylim=c(0,45),xlim=c(dt_sm_probe_waldstein$date_hour[22],dt_sm_probe_waldstein$date_hour[length(dt_sm_probe_waldstein$date_hour)-24]))
for (i in 1:6) {
  cat_data <- dt_sm_probe_waldstein[dt_sm_probe_waldstein$depths == categories[i], ]
  lines(cat_data$date_hour, cat_data$soil_moisture, col = colors[i], lwd = 2)  
}
 
for (i in 1:6) {
  cat_data <- dt_sm_probe_waldstein[dt_sm_probe_waldstein$depths == categories[i], ]
  lines(cat_data$date_hour, cat_data$prediction, col = colors[i], lwd = 2,lty = "dashed")  
}


plot(dt_st_probe_voitsumra$date_hour,dt_st_probe_voitsumra$soil_temperature, col = category_colors, pch = 19,type="n",
     ylab="soil temperature [°C]",xlab="",ylim=c(0,28),xlim=c(dt_st_probe_voitsumra$date_hour[22],dt_st_probe_voitsumra$date_hour[length(dt_st_probe_voitsumra$date_hour)-24]))
for (i in 1:6) {
  cat_data <- dt_st_probe_voitsumra[dt_st_probe_voitsumra$depths == categories[i], ]
  lines(cat_data$date_hour, cat_data$soil_temperature, col = colors[i], lwd = 2) 
}
 
for (i in 1:6) {
  cat_data <- dt_st_probe_voitsumra[dt_st_probe_voitsumra$depths == categories[i], ]
  lines(cat_data$date_hour, cat_data$prediction, col = colors[i], lwd = 2,lty = "dashed") 
}

 
plot(dt_sm_probe_voitsumra$date_hour,dt_sm_probe_voitsumra$soil_moisture, col = category_colors, pch = 19,type="n",
     ylab="soil moisture [%]",xlab="",ylim=c(0,45),xlim=c(dt_sm_probe_voitsumra$date_hour[22],dt_sm_probe_voitsumra$date_hour[length(dt_sm_probe_voitsumra$date_hour)-24]))
for (i in 1:6) {
  cat_data <- dt_sm_probe_voitsumra[dt_sm_probe_voitsumra$depths == categories[i], ]
  lines(cat_data$date_hour, cat_data$soil_moisture, col = colors[i], lwd = 2)  
}
 
for (i in 1:6) {
  cat_data <- dt_sm_probe_voitsumra[dt_sm_probe_voitsumra$depths == categories[i], ]
  lines(cat_data$date_hour, cat_data$prediction, col = colors[i], lwd = 2,lty = "dashed") 
}


#############################################
#plot subsets of sequences of 2022 for soil temperature and soil moisture at two different locations (plot from paper)


par(mfrow = c(4, 4),          
    oma = c(1, 2, 0.5, 0.5),     
    mar = c(2.5, 2.5, 0, 0),       
    mgp = c(3, 1, 0),        
    xpd = NA,
    cex.lab=2,
    cex.axis=2)   

dt_st_probe_waldstein_feb <- subset(dt_st_probe_waldstein,
             date_hour >= as.POSIXct('2022-02-01 00:00') &
               date_hour <= as.POSIXct('2022-02-28 23:59'))

 
plot(dt_st_probe_waldstein_feb$date_hour,dt_st_probe_waldstein_feb$soil_temperature, col = category_colors, pch = 19,type="n",
     xaxt="n",ylab="soil temperature [°C]",xlab="",ylim=c(0,28),xlim=c(dt_st_probe_waldstein_feb$date_hour[22],dt_st_probe_waldstein_feb$date_hour[length(dt_st_probe_waldstein_feb$date_hour)-24]))
axis(1, labels = FALSE)
for (i in 1:6) {
  cat_data <- dt_st_probe_waldstein_feb[dt_st_probe_waldstein_feb$depths == categories[i], ]
  lines(cat_data$date_hour, cat_data$soil_temperature, col = colors[i], lwd =1, lty = "dashed")  
}
 
for (i in 1:6) {
  cat_data <- dt_st_probe_waldstein_feb[dt_st_probe_waldstein_feb$depths == categories[i], ]
  lines(cat_data$date_hour, cat_data$prediction, col = colors[i], lwd =1)  
}
legend(dt_st_probe_waldstein_feb$date_hour[1]-86400*2,30, 
       legend = c("predicted", "measured","5 cm", "15 cm", "25 cm", "35 cm", "45 cm", "55 cm" ),
       col = c("black", "black",colors), 
       lty = c( "solid", "dashed",rep("solid", 6)), 
       lwd = c( 2, 2,rep(2, 6)),
       cex = 2,
       bty = "n",
       seg.len = 1,
       y.intersp=0.35,
       x.intersp=0.4)
text(dt_st_probe_waldstein_feb$date_hour[1]-86400*4.3,28,"A",cex=2.5)



dt_st_probe_waldstein_may <- subset(dt_st_probe_waldstein,
                                    date_hour >= as.POSIXct('2022-05-01 00:00') &
                                      date_hour <= as.POSIXct('2022-05-30 23:59'))
 
plot(dt_st_probe_waldstein_may$date_hour,dt_st_probe_waldstein_may$soil_temperature, col = category_colors, pch = 19,type="n",
     xaxt="n",ylab="",yaxt="n",xlab="",ylim=c(0,28),xlim=c(dt_st_probe_waldstein_may$date_hour[22],dt_st_probe_waldstein_may$date_hour[length(dt_st_probe_waldstein_may$date_hour)-24]))
axis(1, labels = FALSE)
axis(2, labels = FALSE)
for (i in 1:6) {
  cat_data <- dt_st_probe_waldstein_may[dt_st_probe_waldstein_may$depths == categories[i], ]
  lines(cat_data$date_hour, cat_data$soil_temperature, col = colors[i], lwd =1, lty = "dashed")
}
 
for (i in 1:6) {
  cat_data <- dt_st_probe_waldstein_may[dt_st_probe_waldstein_may$depths == categories[i], ]
  lines(cat_data$date_hour, cat_data$prediction, col = colors[i], lwd =1)  
}



dt_st_probe_waldstein_aug <- subset(dt_st_probe_waldstein,
                                    date_hour >= as.POSIXct('2022-08-01 00:00') &
                                      date_hour <= as.POSIXct('2022-08-29 23:59'))

 
plot(dt_st_probe_waldstein_aug$date_hour,dt_st_probe_waldstein_aug$soil_temperature, col = category_colors, pch = 19,type="n",
     xaxt="n",ylab="",yaxt="n",xlab="",ylim=c(0,28),xlim=c(dt_st_probe_waldstein_aug$date_hour[22],dt_st_probe_waldstein_aug$date_hour[length(dt_st_probe_waldstein_aug$date_hour)-24]))
axis(1, labels = FALSE)
axis(2, labels = FALSE)
for (i in 1:6) {
  cat_data <- dt_st_probe_waldstein_aug[dt_st_probe_waldstein_aug$depths == categories[i], ]
  lines(cat_data$date_hour, cat_data$soil_temperature, col = colors[i], lwd =1, lty = "dashed")
}
 
for (i in 1:6) {
  cat_data <- dt_st_probe_waldstein_aug[dt_st_probe_waldstein_aug$depths == categories[i], ]
  lines(cat_data$date_hour, cat_data$prediction, col = colors[i], lwd =1)  
}


dt_st_probe_waldstein_nov <- subset(dt_st_probe_waldstein,
                                    date_hour >= as.POSIXct('2022-11-01 00:00') &
                                      date_hour <= as.POSIXct('2022-11-30 23:59'))


plot(dt_st_probe_waldstein_nov$date_hour,dt_st_probe_waldstein_nov$soil_temperature, col = category_colors, pch = 19,type="n",
     xaxt="n",ylab="",yaxt="n",xlab="",ylim=c(0,28),xlim=c(dt_st_probe_waldstein_nov$date_hour[18],dt_st_probe_waldstein_nov$date_hour[length(dt_st_probe_waldstein_nov$date_hour)-24]))
axis(1, labels = FALSE)
axis(2, labels = FALSE)
for (i in 1:6) {
  cat_data <- dt_st_probe_waldstein_nov[dt_st_probe_waldstein_nov$depths == categories[i], ]
  lines(cat_data$date_hour, cat_data$soil_temperature, col = colors[i], lwd =1, lty = "dashed")
}
 
for (i in 1:6) {
  cat_data <- dt_st_probe_waldstein_nov[dt_st_probe_waldstein_nov$depths == categories[i], ]
  lines(cat_data$date_hour, cat_data$prediction, col = colors[i], lwd =1)  
}
text(x=dt_st_probe_waldstein_nov$date_hour[560],27,"Waldstein",cex=2)


#############################
#voitsumra

dt_st_probe_voitsumra_feb <- subset(dt_st_probe_voitsumra,
                                    date_hour >= as.POSIXct('2022-02-01 00:00') &
                                      date_hour <= as.POSIXct('2022-02-28 23:59'))

 
plot(dt_st_probe_voitsumra_feb$date_hour,dt_st_probe_voitsumra_feb$soil_temperature, col = category_colors, pch = 19,type="n",
     xaxt="n",ylab="soil temperature [°C]",xlab="",ylim=c(0,28),xlim=c(dt_st_probe_voitsumra_feb$date_hour[22],dt_st_probe_voitsumra_feb$date_hour[length(dt_st_probe_voitsumra_feb$date_hour)-24]))
axis(1, labels = FALSE)
for (i in 1:6) {
  cat_data <- dt_st_probe_voitsumra_feb[dt_st_probe_voitsumra_feb$depths == categories[i], ]
  lines(cat_data$date_hour, cat_data$soil_temperature, col = colors[i], lwd =1, lty = "dashed")  
}
 
for (i in 1:6) {
  cat_data <- dt_st_probe_voitsumra_feb[dt_st_probe_voitsumra_feb$depths == categories[i], ]
  lines(cat_data$date_hour, cat_data$prediction, col = colors[i], lwd =1)  
}
text(dt_st_probe_waldstein_feb$date_hour[1]-86400*4.3,28,"B",cex=2.5)

dt_st_probe_voitsumra_may <- subset(dt_st_probe_voitsumra,
                                    date_hour >= as.POSIXct('2022-05-01 00:00') &
                                      date_hour <= as.POSIXct('2022-05-30 23:59'))
 
plot(dt_st_probe_voitsumra_may$date_hour,dt_st_probe_voitsumra_may$soil_temperature, col = category_colors, pch = 19,type="n",
     xaxt="n",ylab="",yaxt="n",xlab="",ylim=c(0,28),xlim=c(dt_st_probe_voitsumra_may$date_hour[22],dt_st_probe_voitsumra_may$date_hour[length(dt_st_probe_voitsumra_may$date_hour)-24]))
axis(1, labels = FALSE)
axis(2, labels = FALSE)
for (i in 1:6) {
  cat_data <- dt_st_probe_voitsumra_may[dt_st_probe_voitsumra_may$depths == categories[i], ]
  lines(cat_data$date_hour, cat_data$soil_temperature, col = colors[i], lwd =1, lty = "dashed")
}
 
for (i in 1:6) {
  cat_data <- dt_st_probe_voitsumra_may[dt_st_probe_voitsumra_may$depths == categories[i], ]
  lines(cat_data$date_hour, cat_data$prediction, col = colors[i], lwd =1)  
}



dt_st_probe_voitsumra_aug <- subset(dt_st_probe_voitsumra,
                                    date_hour >= as.POSIXct('2022-08-01 00:00') &
                                      date_hour <= as.POSIXct('2022-08-29 23:59'))

 
plot(dt_st_probe_voitsumra_aug$date_hour,dt_st_probe_voitsumra_aug$soil_temperature, col = category_colors, pch = 19,type="n",
     xaxt="n",ylab="",yaxt="n",xlab="",ylim=c(0,28),xlim=c(dt_st_probe_voitsumra_aug$date_hour[22],dt_st_probe_voitsumra_aug$date_hour[length(dt_st_probe_voitsumra_aug$date_hour)-24]))
axis(1, labels = FALSE)
axis(2, labels = FALSE)
for (i in 1:6) {
  cat_data <- dt_st_probe_voitsumra_aug[dt_st_probe_voitsumra_aug$depths == categories[i], ]
  lines(cat_data$date_hour, cat_data$soil_temperature, col = colors[i], lwd =1, lty = "dashed")
}
 
for (i in 1:6) {
  cat_data <- dt_st_probe_voitsumra_aug[dt_st_probe_voitsumra_aug$depths == categories[i], ]
  lines(cat_data$date_hour, cat_data$prediction, col = colors[i], lwd =1)  
}


dt_st_probe_voitsumra_nov <- subset(dt_st_probe_voitsumra,
                                    date_hour >= as.POSIXct('2022-11-01 00:00') &
                                      date_hour <= as.POSIXct('2022-11-30 23:59'))

 
plot(dt_st_probe_voitsumra_nov$date_hour,dt_st_probe_voitsumra_nov$soil_temperature, col = category_colors, pch = 19,type="n",
     xaxt="n",ylab="",yaxt="n",xlab="",ylim=c(0,28),xlim=c(dt_st_probe_voitsumra_nov$date_hour[22],dt_st_probe_voitsumra_nov$date_hour[length(dt_st_probe_voitsumra_nov$date_hour)-24]))
axis(1, labels = FALSE)
axis(2, labels = FALSE)
for (i in 1:6) {
  cat_data <- dt_st_probe_voitsumra_nov[dt_st_probe_voitsumra_nov$depths == categories[i], ]
  lines(cat_data$date_hour, cat_data$soil_temperature, col = colors[i], lwd =1, lty = "dashed")
}
 
for (i in 1:6) {
  cat_data <- dt_st_probe_voitsumra_nov[dt_st_probe_voitsumra_nov$depths == categories[i], ]
  lines(cat_data$date_hour, cat_data$prediction, col = colors[i], lwd =1)  
}
text(x=dt_st_probe_waldstein_nov$date_hour[560],27,"Voitsumra",cex=2)

########
########

dt_sm_probe_waldstein_feb <- subset(dt_sm_probe_waldstein,
                                    date_hour >= as.POSIXct('2022-02-01 00:00') &
                                      date_hour <= as.POSIXct('2022-02-28 23:59'))

 
plot(dt_sm_probe_waldstein_feb$date_hour,dt_sm_probe_waldstein_feb$soil_moisture, col = category_colors, pch = 19,type="n",
     xaxt="n",ylab="soil moisture [%]",xlab="",ylim=c(0,45),xlim=c(dt_sm_probe_waldstein_feb$date_hour[22],dt_sm_probe_waldstein_feb$date_hour[length(dt_sm_probe_waldstein_feb$date_hour)-24]))
axis(1, labels = FALSE)
for (i in 1:6) {
  cat_data <- dt_sm_probe_waldstein_feb[dt_sm_probe_waldstein_feb$depths == categories[i], ]
  lines(cat_data$date_hour, cat_data$soil_moisture, col = colors[i], lwd =1, lty = "dashed")  
}
 
for (i in 1:6) {
  cat_data <- dt_sm_probe_waldstein_feb[dt_sm_probe_waldstein_feb$depths == categories[i], ]
  lines(cat_data$date_hour, cat_data$prediction, col = colors[i], lwd =1)  
}
text(dt_st_probe_waldstein_feb$date_hour[1]-86400*4.3,45,"C",cex=2.5)

dt_sm_probe_waldstein_may <- subset(dt_sm_probe_waldstein,
                                    date_hour >= as.POSIXct('2022-05-01 00:00') &
                                      date_hour <= as.POSIXct('2022-05-30 23:59'))
 
plot(dt_sm_probe_waldstein_may$date_hour,dt_sm_probe_waldstein_may$soil_moisture, col = category_colors, pch = 19,type="n",
     xaxt="n",ylab="",yaxt="n",xlab="",ylim=c(0,45),xlim=c(dt_sm_probe_waldstein_may$date_hour[22],dt_sm_probe_waldstein_may$date_hour[length(dt_sm_probe_waldstein_may$date_hour)-24]))
axis(1, labels = FALSE)
axis(2, labels = FALSE)
for (i in 1:6) {
  cat_data <- dt_sm_probe_waldstein_may[dt_sm_probe_waldstein_may$depths == categories[i], ]
  lines(cat_data$date_hour, cat_data$soil_moisture, col = colors[i], lwd =1, lty = "dashed")
}
 
for (i in 1:6) {
  cat_data <- dt_sm_probe_waldstein_may[dt_sm_probe_waldstein_may$depths == categories[i], ]
  lines(cat_data$date_hour, cat_data$prediction, col = colors[i], lwd =1)  
}



dt_sm_probe_waldstein_aug <- subset(dt_sm_probe_waldstein,
                                    date_hour >= as.POSIXct('2022-08-01 00:00') &
                                      date_hour <= as.POSIXct('2022-08-29 23:59'))

 
plot(dt_sm_probe_waldstein_aug$date_hour,dt_sm_probe_waldstein_aug$soil_moisture, col = category_colors, pch = 19,type="n",
     xaxt="n",ylab="",yaxt="n",xlab="",ylim=c(0,45),xlim=c(dt_sm_probe_waldstein_aug$date_hour[22],dt_sm_probe_waldstein_aug$date_hour[length(dt_sm_probe_waldstein_aug$date_hour)-24]))
axis(1, labels = FALSE)
axis(2, labels = FALSE)
for (i in 1:6) {
  cat_data <- dt_sm_probe_waldstein_aug[dt_sm_probe_waldstein_aug$depths == categories[i], ]
  lines(cat_data$date_hour, cat_data$soil_moisture, col = colors[i], lwd =1, lty = "dashed")
}
 
for (i in 1:6) {
  cat_data <- dt_sm_probe_waldstein_aug[dt_sm_probe_waldstein_aug$depths == categories[i], ]
  lines(cat_data$date_hour, cat_data$prediction, col = colors[i], lwd =1)  
}


dt_sm_probe_waldstein_nov <- subset(dt_sm_probe_waldstein,
                                    date_hour >= as.POSIXct('2022-11-01 00:00') &
                                      date_hour <= as.POSIXct('2022-11-30 23:59'))

 
plot(dt_sm_probe_waldstein_nov$date_hour,dt_sm_probe_waldstein_nov$soil_moisture, col = category_colors, pch = 19,type="n",
     xaxt="n",ylab="",yaxt="n",xlab="",ylim=c(0,45),xlim=c(dt_sm_probe_waldstein_nov$date_hour[18],dt_sm_probe_waldstein_nov$date_hour[length(dt_sm_probe_waldstein_nov$date_hour)-24]))
axis(1, labels = FALSE)
axis(2, labels = FALSE)
for (i in 1:6) {
  cat_data <- dt_sm_probe_waldstein_nov[dt_sm_probe_waldstein_nov$depths == categories[i], ]
  lines(cat_data$date_hour, cat_data$soil_moisture, col = colors[i], lwd =1, lty = "dashed")
}
 
for (i in 1:6) {
  cat_data <- dt_sm_probe_waldstein_nov[dt_sm_probe_waldstein_nov$depths == categories[i], ]
  lines(cat_data$date_hour, cat_data$prediction, col = colors[i], lwd =1)  
}
text(x=dt_st_probe_waldstein_nov$date_hour[560],42.5,"Waldstein",cex=2)
##############################


dt_sm_probe_voitsumra_feb <- subset(dt_sm_probe_voitsumra,
                                    date_hour >= as.POSIXct('2022-02-01 00:00') &
                                      date_hour <= as.POSIXct('2022-02-28 23:59'))

 
plot(dt_sm_probe_voitsumra_feb$date_hour,dt_sm_probe_voitsumra_feb$soil_moisture, col = category_colors, pch = 19,type="n",
     ylab="soil moisture [%]",xlab="",ylim=c(0,45),xlim=c(dt_sm_probe_voitsumra_feb$date_hour[22],dt_sm_probe_voitsumra_feb$date_hour[length(dt_sm_probe_voitsumra_feb$date_hour)-24]))
for (i in 1:6) {
  cat_data <- dt_sm_probe_voitsumra_feb[dt_sm_probe_voitsumra_feb$depths == categories[i], ]
  lines(cat_data$date_hour, cat_data$soil_moisture, col = colors[i], lwd =1, lty = "dashed")  
}
 
for (i in 1:6) {
  cat_data <- dt_sm_probe_voitsumra_feb[dt_sm_probe_voitsumra_feb$depths == categories[i], ]
  lines(cat_data$date_hour, cat_data$prediction, col = colors[i], lwd =1)  
}
text(dt_st_probe_waldstein_feb$date_hour[1]-86400*4.3,45,"D",cex=2.5)

dt_sm_probe_voitsumra_may <- subset(dt_sm_probe_voitsumra,
                                    date_hour >= as.POSIXct('2022-05-01 00:00') &
                                      date_hour <= as.POSIXct('2022-05-30 23:59'))
 
plot(dt_sm_probe_voitsumra_may$date_hour,dt_sm_probe_voitsumra_may$soil_moisture, col = category_colors, pch = 19,type="n",
     ylab="",yaxt="n",xlab="",ylim=c(0,45),xlim=c(dt_sm_probe_voitsumra_may$date_hour[22],dt_sm_probe_voitsumra_may$date_hour[length(dt_sm_probe_voitsumra_may$date_hour)-24]))
axis(2, labels = FALSE)
for (i in 1:6) {
  cat_data <- dt_sm_probe_voitsumra_may[dt_sm_probe_voitsumra_may$depths == categories[i], ]
  lines(cat_data$date_hour, cat_data$soil_moisture, col = colors[i], lwd =1, lty = "dashed")
}
 
for (i in 1:6) {
  cat_data <- dt_sm_probe_voitsumra_may[dt_sm_probe_voitsumra_may$depths == categories[i], ]
  lines(cat_data$date_hour, cat_data$prediction, col = colors[i], lwd =1)  
}



dt_sm_probe_voitsumra_aug <- subset(dt_sm_probe_voitsumra,
                                    date_hour >= as.POSIXct('2022-08-01 00:00') &
                                      date_hour <= as.POSIXct('2022-08-29 23:59'))

 
plot(dt_sm_probe_voitsumra_aug$date_hour,dt_sm_probe_voitsumra_aug$soil_moisture, col = category_colors, pch = 19,type="n",
     ylab="",yaxt="n",xlab="",ylim=c(0,45),xlim=c(dt_sm_probe_voitsumra_aug$date_hour[22],dt_sm_probe_voitsumra_aug$date_hour[length(dt_sm_probe_voitsumra_aug$date_hour)-24]))
axis(2, labels = FALSE)
for (i in 1:6) {
  cat_data <- dt_sm_probe_voitsumra_aug[dt_sm_probe_voitsumra_aug$depths == categories[i], ]
  lines(cat_data$date_hour, cat_data$soil_moisture, col = colors[i], lwd =1, lty = "dashed")
}
 
for (i in 1:6) {
  cat_data <- dt_sm_probe_voitsumra_aug[dt_sm_probe_voitsumra_aug$depths == categories[i], ]
  lines(cat_data$date_hour, cat_data$prediction, col = colors[i], lwd =1)  
}


dt_sm_probe_voitsumra_nov <- subset(dt_sm_probe_voitsumra,
                                    date_hour >= as.POSIXct('2022-11-01 00:00') &
                                      date_hour <= as.POSIXct('2022-11-30 23:59'))

 
plot(dt_sm_probe_voitsumra_nov$date_hour,dt_sm_probe_voitsumra_nov$soil_moisture, col = category_colors, pch = 19,type="n",
     ylab="",yaxt="n",xlab="",ylim=c(0,45),xlim=c(dt_sm_probe_voitsumra_nov$date_hour[22],dt_sm_probe_voitsumra_nov$date_hour[length(dt_sm_probe_voitsumra_nov$date_hour)-24]))
axis(2, labels = FALSE)
for (i in 1:6) {
  cat_data <- dt_sm_probe_voitsumra_nov[dt_sm_probe_voitsumra_nov$depths == categories[i], ]
  lines(cat_data$date_hour, cat_data$soil_moisture, col = colors[i], lwd =1, lty = "dashed")
}
 
for (i in 1:6) {
  cat_data <- dt_sm_probe_voitsumra_nov[dt_sm_probe_voitsumra_nov$depths == categories[i], ]
  lines(cat_data$date_hour, cat_data$prediction, col = colors[i], lwd =1)  
}
text(x=dt_sm_probe_voitsumra_nov$date_hour[560],42.5,"Voitsumra",cex=2)



#pdf 14x14

