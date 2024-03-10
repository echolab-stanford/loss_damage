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




############### additions following the r&r from nature (01/29/2024)

#### we need to read in the probability dataset and merge the indicator with the 
#### the maps sf objects and try out the highlighting of the borders 
prob_dam <- read_csv("/Users/mustafazahid/Desktop/country_prob_dam_1990.csv")

prob_dam_1990_2020 <- subset(prob_dam, period == "1990-2020")
prob_dam_2021_2100 <- subset(prob_dam, period == "2021-2100")

### ok now we eant to merge in 
world_1990_2020 <- left_join(world_1990_2020, 
                             prob_dam_1990_2020, 
                             by = c("ISO3"))

world_2021_2100 <- left_join(world_2021_2100, 
                             prob_dam_2021_2100, 
                             by = c("ISO3"))

## ok now that we have merged the datasets, we can plot 
################################################################################
################################################################################
# plot data 

world_1990_2020$certainty_of_damages <- 0
world_1990_2020$certainty_of_damages[world_1990_2020$probability >= 0.9] <- 1
world_1990_2020$certainty_of_damages <- as.character(world_1990_2020$certainty_of_damages)

world_2021_2100$certainty_of_damages <- 0
world_2021_2100$certainty_of_damages[world_2021_2100$probability >= 0.9] <- 1
world_2021_2100$certainty_of_damages <- as.character(world_2021_2100$certainty_of_damages)

world_1990_2020$certainty_of_damages <- NA
world_1990_2020$certainty_of_damages[world_1990_2020$probability >= 0.9] <- 1
world_1990_2020$certainty_of_damages[world_1990_2020$probability <= 0.1] <- 0



world_1990_2020$certainty_of_damages[world_1990_2020$ISO3 == "AGO"] <- 1
world_1990_2020$certainty_of_damages <- as.character(world_1990_2020$certainty_of_damages)

world_2021_2100$certainty_of_damages <- NA
world_2021_2100$certainty_of_damages[world_2021_2100$probability >= 0.9] <- 1
world_2021_2100$certainty_of_damages[world_2021_2100$probability <= 0.1] <- 0

world_2021_2100$certainty_of_damages <- as.character(world_2021_2100$certainty_of_damages)



c<- ggplot(world_1990_2020) + 
  geom_sf(aes(fill = damages, col = certainty_of_damages)) + 
  #scale_color_discrete() + 
  scale_colour_discrete(type = c("#16317d","#a40000", ""), 
                        labels = c("benefits", "damages", ""), 
                        "Probability of\ndamages/benefits > 90%",
                        na.value = alpha("lightgrey", 0.55)) + 
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

d <- ggplot(world_2021_2100) + 
  geom_sf(aes(fill = damages, col = certainty_of_damages)) +   
  scale_colour_discrete(type = c("#16317d","#a40000", ""), 
                        labels = c("benefits", "damages", ""), 
                        "Probability of\ndamages/benefits > 90%",
                        na.value = alpha("lightgrey", 0.55)) + 
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
run_date <- "20240129"
ggsave(paste0("figures/", run_date, "/fig2c_d.pdf"), 
       c_d, width = 10, height = 4)

#end of script