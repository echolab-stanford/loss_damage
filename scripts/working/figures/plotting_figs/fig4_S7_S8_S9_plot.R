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
test_df_for_table <- readRDS(paste0(fig_prepped_dta, "test_df_for_table.rds"))
total_damages_by_pulse_2020_all <- readRDS(paste0(fig_prepped_dta, "total_damages_by_pulse_2020.rds"))
total_damages_by_pulse_2100_all <- readRDS(paste0(fig_prepped_dta, "total_damages_by_pulse_2100.rds"))

################################################################################
################################################################################
# plot data 