##########################################
#packages

library(pdp)
library(ggplot2)

##########################################
#load data and models

directory <- "" #fill in the directory to the folder 

load(file.path(directory, "training_data/train_set_sm.Rdata"))
load(file.path(directory, "training_data/test_set_sm.Rdata"))

load(file.path(directory, "models/rfmodel_sm.Rdata"))

##########################################
#create data set

set.seed(12)

test_set_sm_sub <- dplyr::sample_n(test_set_sm, 1000)

pred <- predict(rfmodel, test_set_sm_sub, probability = TRUE)

##########################################
#calculate partial dependency

par_depths_depths <- partial(rfmodel, pred.var = c("depths"), chull = TRUE)

par_at3m_depths <- partial(rfmodel, pred.var = c("air_temperature_3_month","depths"), chull = TRUE)

par_landuse_depths <- partial(rfmodel, pred.var = c("land_use","depths"))

par_twi_depths <- partial(rfmodel, pred.var = c("topo_wetness","depths"), chull = TRUE)

par_inclination_depths <- partial(rfmodel, pred.var = c("inclination","depths"), chull = TRUE)

par_prec3m_depths <- partial(rfmodel, pred.var = c("prec_sum_3_month","depths"), chull = TRUE)

par_texture_depths <- partial(rfmodel, pred.var = c("soil_texture","depths"), cats="soil_texture")

par_ndvi_depths <- partial(rfmodel, pred.var = c("ndvi","depths"), chull = TRUE)

par_type_depths <- partial(rfmodel, pred.var = c("soil_type","depths"), cats="soil_type")

par_at72h_depths <- partial(rfmodel, pred.var = c("air_temperature_mountain_72","depths"), chull = TRUE)

par_radolansum0_24_depths <- partial(rfmodel, pred.var = c("radolan_sum_0_24","depths"), chull = TRUE)

par_radolansum0_6_depths <- partial(rfmodel, pred.var = c("radolan_sum_0_6","depths"), chull = TRUE)

par_rad_depths <- partial(rfmodel, pred.var = c("global_radiation","depths"), chull = TRUE)

par_prec_depths <- partial(rfmodel, pred.var = c("precipitation","depths"), chull = TRUE)

par_radolan_depths <- partial(rfmodel, pred.var = c("radolan","depths"), chull = TRUE)


##########################################
#create partial dependency plots with ggplot

plot_par_at3m_depths <- autoplot(par_at3m_depths, contour = FALSE, legend.title = " sm [%]",direction=-1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90)) +
  scale_x_continuous(limits = c(min(test_set_sm_sub$air_temperature_3_month), max(test_set_sm_sub$air_temperature_3_month)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80, 0), expand = c(0, 0))+
  labs(y = "depth [cm]",x="air temperature mean 3 months [°C]")


par_landuse_depths$land_use <- as.numeric(par_landuse_depths$land_use)
par_landuse_depths$land_use[par_landuse_depths$land_use==1] <- "forest"
par_landuse_depths$land_use[par_landuse_depths$land_use==2] <- "arable"
par_landuse_depths$land_use[par_landuse_depths$land_use==3] <- "grassland"
plot_par_landuse_depths <- autoplot(par_landuse_depths, contour = FALSE,legend.title = " sm [%]",direction=-1)+
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90))+
  scale_y_reverse(limits = c(80,0), expand = c(0, 0))+
  labs(y = "depth [cm]",x="land use")


plot_par_twi_depths <- autoplot(par_twi_depths, contour = FALSE, legend.title = " sm [%]",direction=-1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90)) +
  scale_x_continuous(limits = c(min(test_set_sm_sub$topo_wetness), max(test_set_sm_sub$topo_wetness)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80, 0), expand = c(0, 0))+
  labs(y = "depth [cm]",x="TWI")


plot_par_inclination_depths <- autoplot(par_inclination_depths, contour = FALSE, legend.title = " sm [%]",direction=-1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90)) +
  scale_x_continuous(limits = c(min(test_set_sm_sub$inclination), max(test_set_sm_sub$inclination)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80, 0), expand = c(0, 0))+
  labs(y = "depth [cm]",x="inclination")


plot_par_prec3m_depths <- autoplot(par_prec3m_depths, contour = FALSE, legend.title = " sm [%]",direction=-1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90)) +
  scale_x_continuous(limits = c(min(test_set_sm_sub$prec_sum_3_month), max(test_set_sm_sub$prec_sum_3_month)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80, 0), expand = c(0, 0))+
  labs(y = "depth [cm]",x="precipitation sum 3 month [mm]")


par_type_depths$soil_type <- as.numeric(par_type_depths$soil_type)
par_type_depths$soil_type[par_type_depths$soil_type==1] <- "org"
par_type_depths$soil_type[par_type_depths$soil_type==2] <- "semi"
par_type_depths$soil_type[par_type_depths$soil_type==3] <- "stag"
par_type_depths$soil_type[par_type_depths$soil_type==4] <- "ter"
par_type_depths = par_type_depths[par_type_depths$soil_type!= "org",]
plot_par_type_depths <- autoplot(par_type_depths, contour = FALSE,legend.title = " sm [%]",direction=-1)+
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90))+
  scale_y_reverse(limits = c(80,0), expand = c(0, 0))+
  labs(y = "depth [cm]")


par_texture_depths$soil_texture <- as.numeric(par_texture_depths$soil_texture)
par_texture_depths$soil_texture[par_texture_depths$soil_texture==1] <- "clay"
par_texture_depths$soil_texture[par_texture_depths$soil_texture==2] <- "debris"
par_texture_depths$soil_texture[par_texture_depths$soil_texture==3] <- "loam"
par_texture_depths$soil_texture[par_texture_depths$soil_texture==4] <- "org"
par_texture_depths$soil_texture[par_texture_depths$soil_texture==5] <- "sand"
par_texture_depths$soil_texture[par_texture_depths$soil_texture==6] <- "silt"
par_texture_depths$soil_texture[par_texture_depths$soil_texture==7] <- "water"
par_texture_depths = par_texture_depths[par_texture_depths$soil_texture!= "debris",]
par_texture_depths = par_texture_depths[par_texture_depths$soil_texture!= "org",]
par_texture_depths = par_texture_depths[par_texture_depths$soil_texture!= "clay",]
par_texture_depths = par_texture_depths[par_texture_depths$soil_texture!= "silt",]
plot_par_texture_depths <- autoplot(par_texture_depths, contour = FALSE,legend.title = " sm [%]",direction=-1)+
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90))+
  scale_y_reverse(limits = c(80,0), expand = c(0, 0))+
  labs(y = "depth [cm]",x="soil texture")


plot_par_ndvi_depths <- autoplot(par_ndvi_depths, contour = FALSE, legend.title = " sm [%]",direction=-1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90)) +
  scale_x_continuous(limits = c(min(test_set_sm_sub$ndvi), max(test_set_sm_sub$ndvi)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80, 0), expand = c(0, 0))+
  labs(y = "depth [cm]",x="NDVI")


plot_par_type_depths <- autoplot(par_type_depths, contour = FALSE, legend.title = " sm [%]",direction=-1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90)) +
  #scale_x_continuous(limits = c(min(test_set_sm_sub$soil_type), max(test_set_sm_sub$soil_type)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80, 0), expand = c(0, 0))+
  labs(y = "depth [cm]",x="aggregated soil group")


plot_par_at72h_depths <- autoplot(par_at72h_depths, contour = FALSE, legend.title = " sm [%]",direction=-1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90)) +
  scale_x_continuous(limits = c(min(test_set_sm_sub$air_temperature_mountain_72), max(test_set_sm_sub$air_temperature_mountain_72)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80, 0), expand = c(0, 0))+
  labs(y = "depth [cm]",x="air temperature 72h prior [°C]")
 

plot_par_radolansum0_24_depths <- autoplot(par_radolansum0_24_depths, contour = FALSE, legend.title = " sm [%]",direction=-1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90)) +
  scale_x_continuous(limits = c(min(test_set_sm_sub$radolan_sum_0_24), max(test_set_sm_sub$radolan_sum_0_24)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80, 0), expand = c(0, 0))+
  labs(y = "depth [cm]", x="precipitation sum 0-24h prior [mm]")


plot_par_radolansum0_6_depths <- autoplot(par_radolansum0_6_depths, contour = FALSE, legend.title = " sm [%]",direction=-1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90)) +
  scale_x_continuous(limits = c(min(test_set_sm_sub$radolan_sum_0_6), max(test_set_sm_sub$radolan_sum_0_6)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80, 0), expand = c(0, 0))+
  labs(y = "depth [cm]", x="precipitation sum 0-6h prior [mm]")


plot_par_rad_depths <- autoplot(par_rad_depths, contour = FALSE, legend.title = " sm [%]",direction=-1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90)) +
  scale_x_continuous(limits = c(min(test_set_sm_sub$global_radiation), max(test_set_sm_sub$global_radiation)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80, 0), expand = c(0, 0))+
  labs(y = "depth [cm]",x="global radiation [W/m²]")


plot_par_prec_depths <- autoplot(par_prec_depths, contour = FALSE, legend.title = " sm [%]",direction=-1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90)) +
  scale_x_continuous(limits = c(min(test_set_sm_sub$precipitation), max(test_set_sm_sub$precipitation)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80, 0), expand = c(0, 0))+
  labs(y = "depth [cm]",x="precipitation (climate station) [mm]")


plot_par_radolan_depths <- autoplot(par_radolan_depths, contour = FALSE, legend.title = " sm [%]",direction=-1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90)) +
  scale_x_continuous(limits = c(min(test_set_sm_sub$radolan), max(test_set_sm_sub$radolan)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80, 0), expand = c(0, 0))+
  labs(y = "depth [cm]",x="precipitation (RADOLAN) [mm]")


##########################################
#arrange partial dependency plots

ggpubr::ggarrange(plot_par_prec3m_depths, 
                  plot_par_radolansum0_24_depths, 
                  plot_par_radolansum0_6_depths, 
                  plot_par_prec_depths, 
                  plot_par_radolan_depths,
                  plot_par_at3m_depths,
                  plot_par_at72h_depths,
                  plot_par_rad_depths,
                  plot_par_landuse_depths,
                  plot_par_ndvi_depths,
                  plot_par_twi_depths, 
                  plot_par_inclination_depths, NULL,
                  plot_par_texture_depths,
                  plot_par_type_depths,
                  labels=c("A","B","C","D","E","F","G","H","I","J","K","L"," ","M","N"),
                  font.label = list(size = 20))







