##########################################
#packages

library(pdp)
library(ggplot2)

##########################################
#load data and models

directory <- "" #fill in the directory to the folder 

load(file.path(directory, "training_data/train_set_st.Rdata"))
load(file.path(directory, "training_data/test_set_st.Rdata"))

load(file.path(directory, "models/rfmodel_st.Rdata"))

##########################################
#create data set

set.seed(12)

test_set_st_sub <- dplyr::sample_n(test_set_st, 1000)

pred <- predict(rfmodel, test_set_st_sub, probability = TRUE)

##########################################
#calculate partial dependency

par_prec4m_depths <- partial(rfmodel, pred.var = c("prec_sum_4_month","depths"), chull = TRUE)

par_at1m_depths <- partial(rfmodel, pred.var = c("air_temperature_month","depths"), chull = TRUE)

par_at3m_depths <- partial(rfmodel, pred.var = c("air_temperature_3_month","depths"), chull = TRUE)

par_landuse_depths <- partial(rfmodel, pred.var = c("land_use","depths"), cats="land_use")

par_trend3m_depths <- partial(rfmodel, pred.var = c("trend_3month","depths"), chull = TRUE)

par_elevation_depths <- partial(rfmodel, pred.var = c("elevation","depths"), chull = TRUE)

par_trend1m_depths <- partial(rfmodel, pred.var = c("trend_month","depths"), chull = TRUE)

par_at3h_depths <- partial(rfmodel, pred.var = c("air_temperature_mountain_3","depths"), chull = TRUE)

par_type_depths <- partial(rfmodel, pred.var = c("soil_type","depths"), cats="soil_type")

par_at_depths <- partial(rfmodel, pred.var = c("air_temperature_mountain","depths"), chull = TRUE)

par_at6h_depths <- partial(rfmodel, pred.var = c("air_temperature_mountain_6","depths"), chull = TRUE)

par_at12h_depths <- partial(rfmodel, pred.var = c("air_temperature_mountain_12","depths"), chull = TRUE)

par_rh_depths <- partial(rfmodel, pred.var = c("relative_humidity","depths"), chull = TRUE)

par_rad_depths <- partial(rfmodel, pred.var = c("global_radiation","depths"), chull = TRUE)

par_prec_depths <- partial(rfmodel, pred.var = c("precipitation","depths"), chull = TRUE)

par_depths_depths <- partial(rfmodel, pred.var = c("depths"), chull = TRUE)

par_atday_depths <- partial(rfmodel, pred.var = c("air_temperature_day","depths"), chull = TRUE)

par_at72h_depths <- partial(rfmodel, pred.var = c("air_temperature_mountain_72","depths"), chull = TRUE)

par_trend1w_depths <- partial(rfmodel, pred.var = c("trend_week","depths"), chull = TRUE)


##########################################
#create partial dependency plots with ggplot

plot_par_prec4m_depths <- autoplot(par_prec4m_depths, contour = FALSE,legend.title = " st [°C]")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90))+
  scale_x_continuous(limits = c(min(test_set_st_sub$prec_sum_4_month),max(test_set_st_sub$prec_sum_4_month)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80,0), expand = c(0, 0))+
  labs(y = "depth [cm]",x="precipitation sum 4 month [mm]")


plot_par_at1m_depths <- autoplot(par_at1m_depths, contour = FALSE,legend.title = " st [°C]")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90))+
  scale_x_continuous(limits = c(min(test_set_st_sub$air_temperature_month),max(test_set_st_sub$air_temperature_month)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80,0), expand = c(0, 0))+
  labs(y = "depth [cm]",x="air temperature mean 1 month")


plot_par_at3m_depths <- autoplot(par_at3m_depths, contour = FALSE,legend.title = " st [°C]")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90))+
  scale_x_continuous(limits = c(min(test_set_st_sub$air_temperature_3_month),max(test_set_st_sub$air_temperature_3_month)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80,0), expand = c(0, 0))+
  labs(y = "depth [cm]",x="air temperature mean 3 month")


par_landuse_depths$land_use <- as.numeric(par_landuse_depths$land_use)
par_landuse_depths$land_use[par_landuse_depths$land_use==1] <- "forest"
par_landuse_depths$land_use[par_landuse_depths$land_use==2] <- "arable"
par_landuse_depths$land_use[par_landuse_depths$land_use==3] <- "grassland"
plot_par_landuse_depths <- autoplot(par_landuse_depths, contour = FALSE,legend.title = " st [°C]")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90))+
  scale_y_reverse(limits = c(80,0), expand = c(0, 0))+
  labs(y = "depth [cm]",x="land use")


plot_par_trend3m_depths <- autoplot(par_trend3m_depths, contour = FALSE,legend.title = " st [°C]")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90))+
  scale_x_continuous(limits = c(min(test_set_st_sub$trend_3month),max(test_set_st_sub$trend_3month)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80,0), expand = c(0, 0))+
  labs(y = "depth [cm]",x = "trend of air temperature mean 3 month")


plot_par_elevation_depths <- autoplot(par_elevation_depths, contour = FALSE,legend.title = " st [°C]")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90))+
  scale_x_continuous(limits = c(min(test_set_st_sub$elevation),max(test_set_st_sub$elevation)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80,0), expand = c(0, 0))+
  labs(y = "depth [cm]",x="elevation [m]")


plot_par_trend1m_depths <- autoplot(par_trend1m_depths, contour = FALSE,legend.title = " st [°C]")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90))+
  scale_x_continuous(limits = c(min(test_set_st_sub$trend_month),max(test_set_st_sub$trend_month)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80,0), expand = c(0, 0))+
  labs(y = "depth [cm]", x = "trend of air temperature mean 1 month")


plot_par_at3h_depths <- autoplot(par_at3h_depths, contour = FALSE,legend.title = " st [°C]")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90))+
  scale_x_continuous(limits = c(min(test_set_st_sub$air_temperature_mountain_3),max(test_set_st_sub$air_temperature_mountain_3)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80,0), expand = c(0, 0))+
  labs(y = "depth [cm]",x="air temperature 3h prior [°C]")


par_type_depths$soil_type <- as.numeric(par_type_depths$soil_type)
par_type_depths$soil_type[par_type_depths$soil_type==1] <- "org"
par_type_depths$soil_type[par_type_depths$soil_type==2] <- "semi"
par_type_depths$soil_type[par_type_depths$soil_type==3] <- "stag"
par_type_depths$soil_type[par_type_depths$soil_type==4] <- "ter"
par_type_depths = par_type_depths[par_type_depths$soil_type!= "org",]
plot_par_type_depths <- autoplot(par_type_depths, contour = FALSE,legend.title = " st [°C]")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90))+
  scale_y_reverse(limits = c(80,0), expand = c(0, 0))+
  labs(y = "depth [cm]", x="aggregated soil group")
plot_par_type_depths


plot_par_at_depths <- autoplot(par_at_depths, contour = FALSE,legend.title = " st [°C]")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90))+
  scale_x_continuous(limits = c(min(test_set_st_sub$air_temperature_mountain),max(test_set_st_sub$air_temperature_mountain)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80,0), expand = c(0, 0))+
  labs(y = "depth [cm]",x="air temperature [°C]")


plot_par_at6h_depths <- autoplot(par_at6h_depths, contour = FALSE,legend.title = " st [°C]")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90))+
  scale_x_continuous(limits = c(min(test_set_st_sub$air_temperature_mountain_6),max(test_set_st_sub$air_temperature_mountain_6)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80,0), expand = c(0, 0))+
  labs(y = "depth [cm]",x="air temperature 6h prior [°C]")


plot_par_at12h_depths <- autoplot(par_at12h_depths, contour = FALSE,legend.title = " st [°C]")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90))+
  scale_x_continuous(limits = c(min(test_set_st_sub$air_temperature_mountain_12),max(test_set_st_sub$air_temperature_mountain_12)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80,0), expand = c(0, 0))+
  labs(y = "depth [cm]",x="air temperature 12h prior [°C]")


plot_par_rh_depths <- autoplot(par_rh_depths, contour = FALSE,legend.title = " st [°C]")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90))+
  scale_x_continuous(limits = c(min(test_set_st_sub$relative_humidity),max(test_set_st_sub$relative_humidity)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80,0), expand = c(0, 0))+
  labs(y = "depth [cm]", x="relative humidity [%]")


plot_par_rad_depths <- autoplot(par_rad_depths, contour = FALSE,legend.title = " st [°C]")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90))+
  scale_x_continuous(limits = c(min(test_set_st_sub$global_radiation),max(test_set_st_sub$global_radiation)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80,0), expand = c(0, 0))+
  labs(y = "depth [cm]", x="global radiation [W/m²]")


plot_par_prec_depths <- autoplot(par_prec_depths, contour = FALSE,legend.title = " st [°C]")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90))+
  scale_x_continuous(limits = c(min(test_set_st_sub$precipitation),max(test_set_st_sub$precipitation)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80,0), expand = c(0, 0))+
  labs(y = "depth [cm]", x="precipitation [mm]")


plot_par_atday_depths <- autoplot(par_atday_depths, contour = FALSE,legend.title = " st [°C]")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90))+
  scale_x_continuous(limits = c(min(test_set_st_sub$air_temperature_day),max(test_set_st_sub$air_temperature_day)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80,0), expand = c(0, 0))+
  labs(y = "depth [cm]", x="air temperature mean 1 day")


plot_par_at72h_depths <- autoplot(par_at72h_depths, contour = FALSE,legend.title = " st [°C]")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90))+
  scale_x_continuous(limits = c(min(test_set_st_sub$air_temperature_mountain_72),max(test_set_st_sub$air_temperature_mountain_72)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80,0), expand = c(0, 0))+
  labs(y = "depth [cm]",x="air temperature 72h prior [°C]")


plot_par_trend1w_depths <- autoplot(par_trend1m_depths, contour = FALSE,legend.title = " st [°C]")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90))+
  scale_x_continuous(limits = c(min(test_set_st_sub$trend_week),max(test_set_st_sub$trend_week)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80,0), expand = c(0, 0))+
  labs(y = "depth [cm]", x = "trend of air temperature mean 1 week")+
  xlim(-0.015,0.015)


##########################################
#arrange partial dependency plots

ggpubr::ggarrange(plot_par_at1m_depths,plot_par_at3m_depths,plot_par_atday_depths,plot_par_at_depths,plot_par_at3h_depths,plot_par_at6h_depths,plot_par_at12h_depths,plot_par_at72h_depths,
                  plot_par_trend3m_depths,plot_par_trend1m_depths,plot_par_trend1w_depths,plot_par_prec4m_depths,plot_par_prec_depths,
                  plot_par_rh_depths,
                  plot_par_rad_depths,
                  plot_par_elevation_depths,NULL,
                  plot_par_landuse_depths,
                  plot_par_type_depths,
                 
                  labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P"," ","Q","R"),
                  font.label = list(size = 20),
                  ncol=4,nrow=5)


