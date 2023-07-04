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
total_damages_cc <- readRDS(paste0(output_path, run_date, "/total_damages_cc.rds"))
fair_exps_cc <- readRDS(paste0(output_path, run_date, "/fair_exps_cc.rds"))

#############################################################################
#############################################################################
# prep data 
tots_cc <- total_damages_cc %>% 
  dplyr::group_by(emitter) %>% 
  dplyr::summarise(total_damages = sum(weighted_damages2_scld, na.rm = T))

tots_cc$total_damages <- tots_cc$total_damages*(-1)

fair_exps_cc <- subset(fair_exps_cc, experiment_iso == 2030)

#############################################################################
#############################################################################
# write the data in
write_rds(tots_cc, paste0(fig_prepped_dta, run_date, "/total_cc.rds"))
write_rds(fair_exps_cc, paste0(fig_prepped_dta,  run_date, "/fair_exps_cc_2300.rds"))

# end of script 

