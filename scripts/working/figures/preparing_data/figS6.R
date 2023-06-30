##############################################################################
# Mustafa Zahid, June 29th, 2023
# prep the data for the carbon capture figure (figS6). 
##############################################################################
remove(list=ls())
gc()
sf::sf_use_s2(FALSE)
setwd("~/GitHub/loss_damage")

# read in the needed libraries 
source("scripts/working/analysis/0_read_libs.R")

setwd(dropbox_path)
#############################################################################
#############################################################################
# read data 
total_damages_cc <- readRDS(paste0(output_path, "20230410/total_damages_cc.rds"))
fair_exps_cc <- readRDS(paste0(output_path, "20230410/fair_exps_cc.rds"))

#############################################################################
#############################################################################
# prep data 
