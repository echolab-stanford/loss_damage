##############################################################################
# Mustafa Zahid, June 26th, 2023
# this script to run the estimates under different scenarios. The Scenarios are 
# estimating damages up to 2300, and up to 2100. For up to 2300, we are running
# model where we set post 2100 growth at 2100 levels, the other is to assume 
# 1 and 2 % growth rates moving beyond 2100., and then one where we clamp growth 
# to observed levels pre 2021, and another where we run a 5 lag BHM model. 
# This script prepares data for fig ED7
##############################################################################
remove(list=ls())
gc()
sf::sf_use_s2(FALSE)
setwd("~/GitHub/loss_damage")

replicate <- F# change T to F if you want to create your own data  
if (replicate == T){
  run_date <- "20230523"
}
if (replicate == F){
  run_date <- gsub("-","",Sys.Date())
}

run_date <- "20230821"
# read in the needed libraries 
source("scripts/working/analysis/0_read_libs.R")

setwd(dropbox_path)
#############################################################################
#############################################################################
# read data 
scc_2300_1pct_growth <- readRDS(paste0(output_path,   "/scc_2300_1pct_growth.rds"))
scc_2300_2pct_growth <- readRDS(paste0(output_path,   "/scc_2300_2pct_growth.rds"))
scc_2300_clamped_growth <- readRDS(paste0(output_path,"/scc_2300_clamped_growth.rds"))
scc_2300_2100_5lag <- readRDS(paste0(output_path,      "/scc_2300_2100_5lag.rds"))
scc_2300_2100_5lag_nog <- readRDS(paste0(output_path,      "/scc_2300_2100_5lag_nog.rds"))
scc_2100_2100_5lag <- readRDS(paste0(output_path,      "/scc_2100_2100_5lag.rds"))
scc_2300_nog_post_2100 <- readRDS(paste0(output_path, "/scc_2300_nog_post_2100.rds"))
scc_2300_2100_growth <- readRDS(paste0(output_path,   "/scc_2300_2100_growth.rds"))
scc_2100 <- readRDS(paste0(output_path, "/scc_2100.rds"))
scc_2100_adaptation <- readRDS(paste0(output_path, "/scc_2100_adaptation.rds"))
scc_2300_adaptation <- readRDS(paste0(output_path, "/scc_2300_adaptation.rds"))
scc_2100_2100_5lag_adaptation <- readRDS(paste0(output_path, "/scc_2100_2100_5lag_adaptation.rds"))
scc_2300_2100_5lag_adaptation <- readRDS(paste0(output_path, "/scc_2300_2100_5lag_adaptation.rds"))

#############################################################################
#############################################################################
# prep data 

ex <- data.frame(scenario = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), 
                 dr1 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), 
                 dr2 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), 
                 dr3 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
                 dr_ramsey = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
                 time_horizon = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
                 post_2100_growth = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
                 regression_model = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))

ex$scenario[1] <- "Growth at 2100 + 5lag"
ex$scenario[2] <- "Growth at 2100 + 5lag + No growth effects > 2100"
ex$scenario[3] <- "Growth at 2100 + 5lag + No impacts > 2100"
ex$scenario[4] <- "No impacts > 2100 + 5lag + adaptation"
ex$scenario[5] <-"Growth at 2100 rate + 5lag + adaptation"

ex$scenario[6] <- "Growth at 2100 rate"
ex$scenario[7] <- "Growth at 1%"
ex$scenario[8] <- "Growth at 2%"
#ex$scenario[4] <- "Growth at 2100 + clamping"
ex$scenario[9] <- "No growth effects > 2100"
ex$scenario[10] <- "No impacts > 2100"
ex$scenario[11] <- "No impacts > 2100 + adaptation"
ex$scenario[12] <-"Growth at 2100 rate + adaptation"



ex$dr1[1] <- round(sum(scc_2300_2100_5lag$weighted_damages1_scld, na.rm = T),0)
ex$dr1[2] <- round(sum(scc_2300_2100_5lag_nog$weighted_damages1_scld, na.rm = T),0)
ex$dr1[3] <- round(sum(scc_2100_2100_5lag$weighted_damages1_scld, na.rm = T),0)
ex$dr1[4] <- round(sum(scc_2100_2100_5lag_adaptation$weighted_damages1_scld, na.rm = T),0)
ex$dr1[5] <- round(sum(scc_2300_2100_5lag_adaptation$weighted_damages1_scld, na.rm = T),0)

ex$dr1[6] <- round(sum(scc_2300_2100_growth$weighted_damages1_scld, na.rm = T),0)
ex$dr1[7] <- round(sum(scc_2300_1pct_growth$weighted_damages1_scld, na.rm = T),0)
ex$dr1[8] <- round(sum(scc_2300_2pct_growth$weighted_damages1_scld, na.rm = T),0)
#ex$dr1[4] <- round(sum(scc_2300_clamped_growth$weighted_damages1_scld, na.rm = T),0)
ex$dr1[9] <- round(sum(scc_2300_nog_post_2100$weighted_damages1_scld, na.rm = T),0)
ex$dr1[10] <- round(sum(scc_2100$weighted_damages1_scld, na.rm = T),0)
ex$dr1[11] <- round(sum(scc_2100_adaptation$weighted_damages1_scld, na.rm = T),0)
ex$dr1[12] <- round(sum(scc_2300_adaptation$weighted_damages1_scld, na.rm = T),0)

ex$dr2[1] <- round(sum(scc_2300_2100_5lag$weighted_damages2_scld, na.rm = T),0)
ex$dr2[2] <- round(sum(scc_2300_2100_5lag_nog$weighted_damages2_scld, na.rm = T),0)
ex$dr2[3] <- round(sum(scc_2100_2100_5lag$weighted_damages2_scld, na.rm = T),0)
ex$dr2[4] <- round(sum(scc_2100_2100_5lag_adaptation$weighted_damages2_scld, na.rm = T),0)
ex$dr2[5] <- round(sum(scc_2300_2100_5lag_adaptation$weighted_damages2_scld, na.rm = T),0)
ex$dr2[6] <- round(sum(scc_2300_2100_growth$weighted_damages2_scld, na.rm = T),0)
ex$dr2[7] <- round(sum(scc_2300_1pct_growth$weighted_damages2_scld, na.rm = T),0)
ex$dr2[8] <- round(sum(scc_2300_2pct_growth$weighted_damages2_scld, na.rm = T),0)
#ex$dr2[4] <- round(sum(scc_2300_clamped_growth$weighted_damages2_scld, na.rm = T),0)
ex$dr2[9] <- round(sum(scc_2300_nog_post_2100$weighted_damages2_scld, na.rm = T),0)
ex$dr2[10] <- round(sum(scc_2100$weighted_damages2_scld, na.rm = T),0)
ex$dr2[11] <- round(sum(scc_2100_adaptation$weighted_damages2_scld, na.rm = T),0)
ex$dr2[12] <- round(sum(scc_2300_adaptation$weighted_damages2_scld, na.rm = T),0)

ex$dr3[1] <- round(sum(scc_2300_2100_5lag$weighted_damages3_scld, na.rm = T),0)
ex$dr3[2] <- round(sum(scc_2300_2100_5lag_nog$weighted_damages3_scld, na.rm = T),0)
ex$dr3[3] <- round(sum(scc_2100_2100_5lag$weighted_damages3_scld, na.rm = T),0)
ex$dr3[4] <- round(sum(scc_2100_2100_5lag_adaptation$weighted_damages3_scld, na.rm = T),0)
ex$dr3[5] <- round(sum(scc_2300_2100_5lag_adaptation$weighted_damages3_scld, na.rm = T),0)
ex$dr3[6] <- round(sum(scc_2300_2100_growth$weighted_damages3_scld, na.rm = T),0)
ex$dr3[7] <- round(sum(scc_2300_1pct_growth$weighted_damages3_scld, na.rm = T),0)
ex$dr3[8] <- round(sum(scc_2300_2pct_growth$weighted_damages3_scld, na.rm = T),0)
#ex$dr3[4] <- round(sum(scc_2300_clamped_growth$weighted_damages3_scld, na.rm = T),0)
ex$dr3[9] <- round(sum(scc_2300_nog_post_2100$weighted_damages3_scld, na.rm = T),0)
ex$dr3[10] <- round(sum(scc_2100$weighted_damages3_scld, na.rm = T),0)
ex$dr3[11] <- round(sum(scc_2100_adaptation$weighted_damages3_scld, na.rm = T),0)
ex$dr3[12] <- round(sum(scc_2300_adaptation$weighted_damages3_scld, na.rm = T),0)


ex$dr_ramsey[1] <- round(sum(scc_2300_2100_5lag$weighted_damages_ramsey_scld, na.rm = T),0)
ex$dr_ramsey[2] <- round(sum(scc_2300_2100_5lag_nog$weighted_damages_ramsey_scld, na.rm = T),0)
ex$dr_ramsey[3] <- round(sum(scc_2100_2100_5lag$weighted_damages_ramsey_scld, na.rm = T),0)
ex$dr_ramsey[4] <- round(sum(scc_2100_2100_5lag_adaptation$weighted_damages_ramsey_scld, na.rm = T),0)
ex$dr_ramsey[5] <- round(sum(scc_2300_2100_5lag_adaptation$weighted_damages_ramsey_scld, na.rm = T),0)

ex$dr_ramsey[6] <- round(sum(scc_2300_2100_growth$weighted_damages_ramsey_scld, na.rm = T),0)
ex$dr_ramsey[7] <- round(sum(scc_2300_1pct_growth$weighted_damages_ramsey_scld, na.rm = T),0)
ex$dr_ramsey[8] <- round(sum(scc_2300_2pct_growth$weighted_damages_ramsey_scld, na.rm = T),0)
#ex$dr_ramsey[4] <- round(sum(scc_2300_clamped_growth$weighted_damages_ramsey_scld, na.rm = T),0)

ex$dr_ramsey[9] <- round(sum(scc_2300_nog_post_2100$weighted_damages_ramsey_scld, na.rm = T),0)
ex$dr_ramsey[10] <- round(sum(scc_2100$weighted_damages_ramsey_scld, na.rm = T),0)
ex$dr_ramsey[11] <- round(sum(scc_2100_adaptation$weighted_damages_ramsey_scld, na.rm = T),0)
ex$dr_ramsey[12] <- round(sum(scc_2300_adaptation$weighted_damages_ramsey_scld, na.rm = T),0)


# now let us add the columns 
# first we need to add time horizon 
ex$time_horizon[1] <- "through 2300"
ex$time_horizon[2] <- "through 2300"
ex$time_horizon[3] <- "through 2100"
ex$time_horizon[4] <- "through 2100"
ex$time_horizon[5] <- "through 2300"
ex$time_horizon[6] <- "through 2300"
ex$time_horizon[7] <- "through 2300"
ex$time_horizon[8] <- "through 2300"
#ex$time_horizon[4] <- "through 2300"
ex$time_horizon[9] <- "through 2300"
ex$time_horizon[10] <- "through 2100"
ex$time_horizon[11] <- "through 2100"
ex$time_horizon[12] <- "through 2300"

# now we need to do post-2100 growth column 
ex$post_2100_growth[1] <- "SSP 2100 rates"
ex$post_2100_growth[2] <- "SSP 2100 rates"
ex$post_2100_growth[3] <- "SSP 2100 rates"
ex$post_2100_growth[4] <- ""
ex$post_2100_growth[5] <- "SSP 2100 growth rate"
ex$post_2100_growth[6] <- "SSP 2100 growth rate"
ex$post_2100_growth[7] <- "1% growth rate"
ex$post_2100_growth[8] <- "2% growth rate"
#ex$post_2100_growth[4] <- "SSP 2100 clamped rate"
ex$post_2100_growth[9] <- "SSP 2100 rates"
ex$post_2100_growth[10] <- ""
ex$post_2100_growth[11] <- ""
ex$post_2100_growth[12] <- "SSP 2100 growth rate"

# now let us do regression model 
ex$regression_model[1] <- "5-lag model"
ex$regression_model[2] <- "5-lag model"
ex$regression_model[3] <- "5-lag model"
ex$regression_model[4] <- "5-lag model"
ex$regression_model[5] <- "5-lag model"
ex$regression_model[6] <- "0-lag model"
ex$regression_model[7] <- "0-lag model"
ex$regression_model[8] <- "0-lag model"
#ex$regression_model[4] <- "0-lag  model"
ex$regression_model[9] <- "0-lag model"
ex$regression_model[10] <- "0-lag model"
ex$regression_model[11] <- "0-lag model"
ex$regression_model[12] <- "0-lag model"

write_rds(ex, "~/Desktop/scc_under_diff_scenarios_0228.rds")
# ok ready to plot 
run_date <- "20231208"
write_rds(ex, paste0(fig_prepped_dta, run_date, "/scc_under_diff_scenarios.rds"))

# end of script 