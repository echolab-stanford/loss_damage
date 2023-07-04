##############################################################################
# Mustafa Zahid, January 7th, 2023
# This R script reads the data and prepares the necessary data to plots figure
# 1. Figure 1 demonstrates the different steps taken to calculate teh damages
#############################################################################
remove(list=ls())
gc()
sf::sf_use_s2(FALSE)
setwd("~/GitHub/loss_damage")

# read in the needed libraries 
source("scripts/working/analysis/0_read_libs.R")
# function for calculating warming ratio CGMs
source("scripts/working/analysis/1_r_cgm.R")
# functions for computing deltaT form fair
source("scripts/working/analysis/2a_FaIR_deltaT_hist.R")
source("scripts/working/analysis/2b_FaIR_deltaT_hist_fut.R")
source("scripts/working/analysis/2c_FaIR_deltaT_hist_fut_disagg.R")
# functions for prepping gdp-temp panel and for computing damages
source("scripts/working/analysis/3a0_run_gdptemp_panel.R")
source("scripts/working/analysis/3a1_run_gdptemp_panel_bhmbs.R")
source("scripts/working/analysis/3a2_run_gdptemp_panel_5lags.R")
source("scripts/working/analysis/3b0_run_bhm_model.R")
source("scripts/working/analysis/3c0_calc_total_damages_bilateral.R")
source("scripts/working/analysis/3c1_calc_total_damages.R")
source("scripts/working/analysis/3c2_calc_total_damages_5lags.R")

################################################################################
################################################################################
# read data 
total_damages_uncertainty_cgm <- readRDS(paste0(fig_prepped_dta, run_date, "/total_damages_1gtco2_cgm.rds"))
total_damages_uncertainty_fair <- readRDS(paste0(fig_prepped_dta, run_date, "/total_damages_1gtco2_fair.rds"))
total_damages_uncertainty_bhm <- readRDS(paste0(fig_prepped_dta, run_date, "/total_damages_1gtco2_bhm.rds"))
total_damages_uncertainty_total <- readRDS(paste0(fig_prepped_dta, run_date, "/total_damages_1gtco2_total.rds"))

# prepare data for plotting

# cgm uncertainty
totals_cgm <- total_damages_uncertainty_cgm %>% dplyr::group_by(cgm_id) %>% 
  dplyr::summarise(total_damages = sum(weighted_damages2_scld, na.rm = T))

# bhm uncertainty
totals_bhm <- total_damages_uncertainty_bhm %>% dplyr::group_by(coef_id) %>% 
  dplyr::summarise(total_damages = sum(weighted_damages2_scld, na.rm = T))

# fair uncertainty
totals_fair <- total_damages_uncertainty_fair %>% dplyr::group_by(fair_id) %>% 
  dplyr::summarise(total_damages = sum(weighted_damages2_scld, na.rm = T))

# total uncertainty
totals_all <- total_damages_uncertainty_total %>% dplyr::group_by(emitter, iter_id) %>% 
  dplyr::summarise(total_damages = sum(weighted_damages2_scld, na.rm = T))

totals_all <- ungroup(totals_all)
totals_all <- totals_all %>% dplyr::select(-c("emitter"))

# ok now write the data 
write_rds(totals_all, paste0(fig_prepped_dta, run_date,"totals_all.rds"))
write_rds(totals_bhm, paste0(fig_prepped_dta, run_date,"totals_bhm.rds"))
write_rds(totals_cgm, paste0(fig_prepped_dta, run_date,"totals_cgm.rds"))
write_rds(totals_fair, paste0(fig_prepped_dta,run_date, "totals_fair.rds"))

# end of script

