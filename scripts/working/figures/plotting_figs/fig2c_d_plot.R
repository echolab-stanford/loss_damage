##############################################################################
# Mustafa Zahid, January 7th, 2023
# This R script prepares the data for plotting figure 3a and 3b 
#############################################################################
remove(list=ls())
gc()
sf::sf_use_s2(FALSE)
setwd("~/GitHub/loss_damage")

run_date <- "20230821"
# read in the needed libraries 
source("scripts/working/analysis/0_read_libs.R")

################################################################################
################################################################################
# read data
world <- read_sf(paste0(fig_prepped_dta, run_date,"/world.shp"))
damages_1990_2020 <- readRDS(paste0(fig_prepped_dta, run_date,"/1gtco2_damages_1990_2020.rds"))
damages_2021_2100 <- readRDS(paste0(fig_prepped_dta, run_date,"/1gtco2_damages_2020_2100.rds"))

world <- sf::st_transform(world,
                          "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
                          
                          
################################################################################
################################################################################
# prep data 
world_1990_2020 <- left_join(world, damages_1990_2020, 
                             by = c("ISO3"))
world_2021_2100 <- left_join(world, damages_2021_2100, 
                             by = c("ISO3"))

world_1990_2020$damages <- world_1990_2020$damages *(-1)
world_2021_2100$damages <- world_2021_2100$damages *(-1)

#world_1990_2020$damages <- world_1990_2020$damages/1000000000 
#world_2021_2100$damages <- world_2021_2100$damages/1000000000 


################################################################################
################################################################################
# plot data 
c<- ggplot(world_1990_2020) + 
  geom_sf(aes(fill = damages)) + 
  scale_fill_gradient2(low="red", mid="white", high="lightblue",
                       trans = modulus_trans(0.5), 
                       limits = c(min(world_2021_2100$damages, na.rm = T),
                                  max(world_2021_2100$damages, na.rm = T)),
                       breaks = c(-9e+10, -6e+10,-3e+10,-1.5e+10,-5e+9,0,5e+9,1.5e+10,3e+10),
                       labels = c("-$90B", "-$60B","-$30B","-$15B","-$5B","0","$5B","$15B","$30B"),
                       name = "Impact of CO2 \nemitted in 1990") + 
  theme_minimal() +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                           panel.background = element_blank(),
                           axis.line = element_blank(), axis.text = element_blank(),
                           legend.key.width = unit(1.75, "cm")) + 
  ggtitle("c) Impacts through 2020")
summary(world_2021_2100$damages)
d <- ggplot(world_2021_2100) + 
  geom_sf(aes(fill = damages)) + 
  scale_fill_gradient2(low="red", mid="white", high="lightblue",
                       trans = modulus_trans(0.5), 
                       limits = c(min(world_2021_2100$damages, na.rm = T),
                                  max(world_2021_2100$damages, na.rm = T)),
                       breaks = c(-9e+10, -6e+10,-3e+10,-1.5e+10,-5e+9,0,5e+9,1.5e+10,3e+10),
                       labels = c("-$90B", "-$60B","-$30B","-$15B","-$5B","0","$5B","$15B","$30B"),
                       name = "Impact of CO2 \nemitted in 1990") +
  theme_minimal() +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                           panel.background = element_blank(), axis.line = element_blank(), axis.text = element_blank(),
                           legend.key.width = unit(1.75, "cm")) + 
  ggtitle("d) Impacts 2021-2100")

# bring plots together in one plot
c_d <- ggpubr::ggarrange(c,d,
                  ncol = 2,nrow = 1,
                  common.legend = TRUE, 
                  legend="bottom")

#save the plot 
ggsave(paste0("figures/", run_date, "/fig2c_d.pdf"), 
       c_d, width = 10, height = 4)

#end of script