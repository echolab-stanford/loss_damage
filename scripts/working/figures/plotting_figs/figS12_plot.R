##############################################################################
# Mustafa Zahid, January 7th, 2023
# This R script prepares the data for plotting figure 3a and 3b 
#############################################################################
remove(list=ls())
gc()
sf::sf_use_s2(FALSE)
setwd("~/GitHub/loss_damage")

# read in the needed libraries 
source("scripts/working/analysis/0_read_libs.R")

################################################################################
################################################################################
# read data
world <- read_sf(paste0(fig_prepped_dta, "20230629/world.shp"))
damages_1990_2020 <- readRDS(paste0(fig_prepped_dta, "20230629/1gtco2_damages_1990_2020.rds"))
damages_2021_2100 <- readRDS(paste0(fig_prepped_dta, "20230629/1gtco2_damages_2021_2100.rds"))


################################################################################
################################################################################
# prep data 
world_1990_2020 <- left_join(world, damages_1990_2020, 
                             by = c("ISO3"))
world_2021_2100 <- left_join(world, damages_2021_2100, 
                             by = c("ISO3"))

world_1990_2020$damages <- world_1990_2020$damages *(-1)
world_2021_2100$damages <- world_2021_2100$damages *(-1)

################################################################################
################################################################################
# plot data 
a <- ggplot(world_1990_2020) + 
  geom_sf(aes(fill = damages)) + 
  scale_fill_gradient2(low="red", mid="white", high="lightblue",
                       trans = modulus_trans(0.5), 
                       limits = c(min(world_2021_2100$damages, na.rm = T),
                                  max(world_2021_2100$damages, na.rm = T)),
                       breaks = c(-6e+10,-4e+10,-2e+10,-5e+9,0,5e+9,1.5e+10,3e+10),
                       labels = c("-$60B","-$40B","-$20B","-$5B","0","$5B","$15B","$30B"),
                       name = "Impact of 1GtCO2 \nemitted in 1990") + 
  theme_minimal() +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                           panel.background = element_blank(),
                           axis.line = element_blank(), axis.text = element_blank()) + 
  ggtitle("a) Impacts through 2020")

b <- ggplot(world_2021_2100) + 
  geom_sf(aes(fill = damages)) + 
  scale_fill_gradient2(low="red", mid="white", high="lightblue",
                       trans = modulus_trans(0.5), 
                       limits = c(min(world_2021_2100$damages, na.rm = T),
                                  max(world_2021_2100$damages, na.rm = T)),
                       breaks = c(-6e+10,-4e+10,-2e+10,-5e+9,0,5e+9,1.5e+10,3e+10),
                       labels = c("-$60B","-$40B","-$20B","-$5B","0","$5B","$15B","$30B"),
                       name = "Impact of 1GtCO2 \nemitted in 1990") +
  theme_minimal() +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                           panel.background = element_blank(), axis.line = element_blank(), axis.text = element_blank()) + 
  ggtitle("b) Impacts 2021-2100")

# bring plots together in one plot
a_b <- ggpubr::ggarrange(a,b,
                  ncol = 1,nrow = 2,
                  common.legend = TRUE, 
                  legend="right")

#save the plot 
ggsave(paste0("figures/", gsub("-", "", Sys.Date()), "/figS12.png"), 
       a_b, width = 8, height = 6)

#end of script