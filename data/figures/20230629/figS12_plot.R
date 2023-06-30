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


################################################################################
################################################################################
# plot data 

