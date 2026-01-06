##############################################################################
# Mustafa Zahid, January 7th, 2023
# This R script plots figure 2c, and 2d, as well as supplement figures 
# with per capita damages, as well as damages as % of GDP
# input(s):
# - "~/Github/loss_damage/data/figures/{run_date}/world.shp"
# - "~/Github/loss_damage/data/figures/{run_date}/1gtco2_damages_1990_2020.rds"
# - "~/Github/loss_damage/data/figures/{run_date}/1gtco2_damages_2020_2100.rds"
# - "~/BurkeLab Dropbox/projects/loss_damage/data/processed/world_gdp_pop/temp_gdp_world_panel.rds"
# - "~/Github/loss_damage/data/figures/{run_date}/country_prob_dam_1990_5lag.csv"
# output(s):  
# - "~/Github/loss_damage/figures/{run_date}/fig2c_d.pdf"
# - "~/Github/loss_damage/figures/{run_date}/figED7c_d.pdf"
# - "~/Github/loss_damage/figures/{run_date}/figED7a_b.pdf"

############################################################################# set up env
remove(list=ls())
gc()
sf::sf_use_s2(FALSE)
setwd("~/GitHub/loss_damage")
# specify run_date
run_date <- "loss_damage_r1_mustafa_rep_temp"
# read in the needed libraries 
source("scripts/working/analysis/0_read_libs.R")

################################################################################
################################################################################ read the data
world <- read_sf(paste0(fig_prepped_dta, run_date,"/world.shp"))
world <- sf::st_transform(world,
                          "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

damages_1990_2020 <- readRDS(paste0(fig_prepped_dta, run_date,"/1gtco2_damages_1990_2020.rds"))
damages_2021_2100 <- readRDS(paste0(fig_prepped_dta, run_date,"/1gtco2_damages_2020_2100.rds"))

# we need to read in the population and gdp data we used in the pipeline 
# to calculate per capita dmages, as well as damages as % of 2020 GDP
pop_gdp <- readRDS("~/BurkeLab Dropbox/projects/loss_damage/data/processed/world_gdp_pop/temp_gdp_world_panel.rds")
pop_gdp <- subset(pop_gdp, year == 2020)
colnames(pop_gdp)
pop_gdp <- pop_gdp %>% 
  dplyr::select(c("ISO3", "SP.POP.TOTL", "NY.GDP.PCAP.KD_for_damages"))
pop_gdp$total_gdp_2020 <- pop_gdp$SP.POP.TOTL*pop_gdp$NY.GDP.PCAP.KD_for_damages

############### the probability of damages/benefits is an addition following the
############### r&r from nature (01/29/2024)
#### we need to read in the probability dataset and merge the indicator with the 
#### the maps sf objects and try out the highlighting of the borders 
prob_dam <- read_csv(paste0(fig_prepped_dta,run_date, "/country_prob_dam_1990_5lag.csv"))
prob_dam_1990_2020 <- subset(prob_dam, period == "1990-2020")
prob_dam_2021_2100 <- subset(prob_dam, period == "2021-2100")
                          
                          
################################################################################
################################################################################ prep the data 
# merge shapefile with damages
world_1990_2020 <- left_join(world, damages_1990_2020, 
                             by = c("ISO3"))
world_2021_2100 <- left_join(world, damages_2021_2100, 
                             by = c("ISO3"))
# redirect the damages 
world_1990_2020$damages <- world_1990_2020$damages *(-1)
world_2021_2100$damages <- world_2021_2100$damages *(-1)
# rescale them...
sum(world_1990_2020$damages, na.rm = T)/1000000000
sum(world_2021_2100$damages, na.rm = T)/1000000000
# now bring in the pop data 
world_1990_2020 <- left_join(world_1990_2020, 
                             pop_gdp, 
                             by = c("ISO3"))
world_2021_2100 <- left_join(world_2021_2100, 
                             pop_gdp, 
                             by = c("ISO3"))

# let us calculate damages per capita
world_1990_2020$damages_pcap <- world_1990_2020$damages/world_1990_2020$SP.POP.TOTL
world_2021_2100$damages_pcap <- world_2021_2100$damages/world_2021_2100$SP.POP.TOTL
# now let us calculate damages as % of gdp 
world_1990_2020$damages_pct_2020 <- world_1990_2020$damages/(world_1990_2020$SP.POP.TOTL*world_1990_2020$NY.GDP.PCAP.KD_for_damages)
world_2021_2100$damages_pct_2020 <- world_2021_2100$damages/(world_2021_2100$SP.POP.TOTL*world_1990_2020$NY.GDP.PCAP.KD_for_damages)

#world_1990_2020$damages <- world_1990_2020$damages/1000000000 
#world_2021_2100$damages <- world_2021_2100$damages/1000000000 

### ok now we 1ant to merge in the probability of damages per our data
world_1990_2020 <- left_join(world_1990_2020, 
                             prob_dam_1990_2020, 
                             by = c("ISO3"))

world_2021_2100 <- left_join(world_2021_2100, 
                             prob_dam_2021_2100, 
                             by = c("ISO3"))

# And now let us assign the labels
world_1990_2020$certainty_of_damages <- 0
world_1990_2020$certainty_of_damages[world_1990_2020$probability >= 0.9] <- 1
world_1990_2020$certainty_of_damages[world_1990_2020$probability < 0.9 & world_1990_2020$probability >=0.1] <- NA
world_1990_2020$certainty_of_damages[is.na(world_1990_2020$probability)] <- NA
world_1990_2020$certainty_of_damages[world_1990_2020$ISO3 == "AGO"] <- 1
world_1990_2020$certainty_of_damages <- as.character(world_1990_2020$certainty_of_damages)
# And now let us assign the labels to future map
world_2021_2100$certainty_of_damages <- 0
world_2021_2100$certainty_of_damages[world_2021_2100$probability >= 0.9] <- 1
world_2021_2100$certainty_of_damages[world_2021_2100$probability < 0.9 & world_2021_2100$probability >=0.1] <- NA
world_2021_2100$certainty_of_damages[is.na(world_2021_2100$probability)] <- NA
world_2021_2100$certainty_of_damages[world_2021_2100$ISO3 == "AGO"] <- 1
world_2021_2100$certainty_of_damages <- as.character(world_2021_2100$certainty_of_damages)

## ok now that we have merged the datasets, we can plot 
################################################################################
################################################################################ plot the data 
c <- ggplot(world_1990_2020) + 
  geom_sf(aes(fill = damages, col = certainty_of_damages)) + 
  #scale_color_discrete() + 
  scale_colour_discrete(type = c("#16317d","#a40000", ""), 
                        labels = c("benefits", "damages", ""), 
                        "Probability of\ndamages/benefits > 90%",
                        na.value = alpha("lightgrey", 0.55)) + 
  scale_fill_gradient2(low="red", mid="white", high="lightblue",
                       trans = modulus_trans(0.5), 
                       limits = c(min(world_2021_2100$damages, na.rm = T),
                                  max(world_1990_2020$damages, na.rm = T)),
                       breaks = c(-4e+11, -2e+11, -1e+11, -5e+10,-2.5e+10,-5e+9,0,5e+9,1.5e+10),
                       labels = c("-$400B","-$200B","-$100B","-$50B","-$25B","-$5B","0","$5B","$15B"),
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
                                  max(world_1990_2020$damages, na.rm = T)),
                       breaks = c(-4e+11, -2e+11, -1e+11, -5e+10,-2.5e+10,-5e+9,0,5e+9,1.5e+10),
                       labels = c("-$400B","-$200B","-$100B","-$50B","-$25B","-$5B","0","$5B","$15B"),
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
ggsave(paste0("figures/", run_date, "/figED7a_b.pdf"), 
       c_d, width = 10, height = 4)

################################################################################
# now let us plot the same plot but for damages per capita or damages as % of gdp
# plot data 
c <- ggplot(world_1990_2020) + 
  geom_sf(aes(fill = damages_pcap, col = certainty_of_damages)) + 
  #scale_color_discrete() + 
  scale_colour_discrete(type = c("#16317d","#a40000", ""), 
                        labels = c("benefits", "damages", ""), 
                        "Probability of\ndamages/benefits > 90%",
                        na.value = alpha("lightgrey", 0.55)) + 
  scale_fill_gradient2(low="red", mid="white", high="lightblue",
                       trans = modulus_trans(0.5), 
                       limits = c(min(world_2021_2100$damages_pcap, na.rm = T),
                                  max(world_1990_2020$damages_pcap, na.rm = T)),
                       breaks = c(-3000, -2000, -1000, -500,-250,-50,0,50,150),
                       labels = c("-$3k","-$2k","-$1k","-$500","-$250","-$50","0","$50","$150"),
                       name = "Impact of CO2 \nemitted in 1990") + 
  theme_minimal() +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                           panel.background = element_blank(),
                           axis.line = element_blank(), axis.text = element_blank(),
                           legend.key.width = unit(1.75, "cm")) + 
  ggtitle("c) Impacts through 2020")


d <- ggplot(world_2021_2100) + 
  geom_sf(aes(fill = damages_pcap, col = certainty_of_damages)) +   
  scale_colour_discrete(type = c("#16317d","#a40000", ""), 
                        labels = c("benefits", "damages", ""), 
                        "Probability of\ndamages/benefits > 90%",
                        na.value = alpha("lightgrey", 0.55)) + 
  scale_fill_gradient2(low="red", mid="white", high="lightblue",
                       trans = modulus_trans(0.5), 
                       limits = c(min(world_2021_2100$damages_pcap, na.rm = T),
                                  max(world_1990_2020$damages_pcap, na.rm = T)),
                       breaks = c(-3000, -2000, -1000, -500,-250,-50,0,50,150),
                       labels = c("-$3k","-$2k","-$1k","-$500","-$250","-$50","0","$50","$150"),
                       name = "Impact of CO2 \nemitted in 1990") +
  theme_minimal() +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                           panel.background = element_blank(), axis.line = element_blank(), axis.text = element_blank(),
                           legend.key.width = unit(1.75, "cm")) + 
  ggtitle("d) Impacts 2021-2100")

# bring plots together in one plot
c_d_pcap <- ggpubr::ggarrange(c,d,
                         ncol = 2,nrow = 1,
                         common.legend = TRUE, 
                         legend="bottom")

#save the plot 
ggsave(paste0("figures/", run_date, "/figED7c_d.pdf"), 
       c_d_pcap, width = 10, height = 4)

################################################################################
# now let us plot the same plot but for damages per capita or damages as % of gdp
# plot data 
c <- ggplot(world_1990_2020) + 
  geom_sf(aes(fill = damages_pct_2020, col = certainty_of_damages)) + 
  #scale_color_discrete() + 
  scale_colour_discrete(type = c("#16317d","#a40000", ""), 
                        labels = c("benefits", "damages", ""), 
                        "Probability of\ndamages/benefits > 90%",
                        na.value = alpha("lightgrey", 0.55)) + 
  scale_fill_gradient2(low="red", mid="white", high="lightblue",
                       trans = modulus_trans(0.5), 
                       limits = c(min(world_2021_2100$damages_pct_2020, na.rm = T),
                                  max(world_2021_2100$damages_pct_2020, na.rm = T)),
                       breaks = c(-0.3, -0.2, -0.1, -0.05,0, 0.005, 0.01, 0.015),
                       labels = c("-30%","-20%","-10%","-5%","0","0.5%","1%","1.5%"),
                       name = "Impact of CO2 \nemitted in 1990") + 
  theme_minimal() +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                           panel.background = element_blank(),
                           axis.line = element_blank(), axis.text = element_blank(),
                           legend.key.width = unit(1.75, "cm")) + 
  ggtitle("c) Impacts through 2020")


d <- ggplot(world_2021_2100) + 
  geom_sf(aes(fill = damages_pct_2020, col = certainty_of_damages)) +   
  scale_colour_discrete(type = c("#16317d","#a40000", ""), 
                        labels = c("benefits", "damages", ""), 
                        "Probability of\ndamages/benefits > 90%",
                        na.value = alpha("lightgrey", 0.55)) + 
  scale_fill_gradient2(low="red", mid="white", high="lightblue",
                       trans = modulus_trans(0.5), 
                       limits = c(min(world_2021_2100$damages_pct_2020, na.rm = T),
                                  max(world_2021_2100$damages_pct_2020, na.rm = T)),
                       breaks = c(-0.3, -0.2, -0.1, -0.05,0, 0.005, 0.01, 0.015),
                       labels = c("-30%","-20%","-10%","-5%","0","0.5%","1%","1.5%"),
                       name = "Impact of CO2 \nemitted in 1990") +
  theme_minimal() +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                           panel.background = element_blank(), axis.line = element_blank(), axis.text = element_blank(),
                           legend.key.width = unit(1.75, "cm")) + 
  ggtitle("d) Impacts 2021-2100")

# bring plots together in one plot
c_d_pct_2020 <- ggpubr::ggarrange(c,d,
                              ncol = 2,nrow = 1,
                              common.legend = TRUE, 
                              legend="bottom")

#save the plot 
ggsave(paste0("figures/", run_date, "/fig2c_d.pdf"), 
       c_d_pct_2020, width = 10, height = 4)

#end of script
