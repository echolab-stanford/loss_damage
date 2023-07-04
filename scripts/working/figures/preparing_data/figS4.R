##############################################################################
# Mustafa Zahid, June 26th, 2023
# this script to run the estimates under different scenarios. The Scenarios are 
# estimating damages up to 2300, and up to 2100. For up to 2300, we are running
# model where we set post 2100 growth at 2100 levels, the other is to assume 
# 1 and 2 % growth rates moving beyond 2100., and then one where we clamp growth 
# to observed levels pre 2021, and another where we run a 5 lag BHM model 
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
scc_2300_1pct_growth <- readRDS(paste0(output_path, run_date,"/scc_2300_1pct_growth.rds"))
scc_2300_2pct_growth <- readRDS(paste0(output_path, run_date,"/scc_2300_2pct_growth.rds"))
scc_2300_clamped_growth <- readRDS(paste0(output_path, run_date,"/scc_2300_clamped_growth.rds"))
scc_2300_2100_5lag <- readRDS(paste0(doutput_path, run_date, "/scc_2300_2100_5lag.rds"))
scc_2300_nog_post_2100 <- readRDS(paste0(output_path, run_date,"/scc_2300_nog_post_2100.rds"))
scc_2300_2100_growth <- readRDS(paste0(output_path, run_date,"/scc_2300_2100_growth.rds"))
scc_2100 <- readRDS(paste0(output_path, run_date, "/scc_2100.rds"))

#############################################################################
#############################################################################
# prep data 

ex <- data.frame(scenario = c(NA, NA, NA, NA, NA, NA, NA), 
                 dr1 = c(NA, NA, NA, NA, NA, NA, NA), 
                 dr2 = c(NA, NA, NA, NA, NA, NA, NA), 
                 dr3 = c(NA, NA, NA, NA, NA, NA, NA),
                 dr_ramsey = c(NA, NA, NA, NA, NA, NA, NA),
                 time_horizon = c(NA, NA, NA, NA, NA, NA, NA),
                 post_2100_growth = c(NA, NA, NA, NA, NA, NA, NA),
                 regression_model = c(NA, NA, NA, NA, NA, NA, NA))

ex$scenario[1] <- "Growth at 2100 rate"
ex$scenario[2] <- "Growth at 1%"
ex$scenario[3] <- "Growth at 2%"
ex$scenario[4] <- "Growth at 2100 + clamping"
ex$scenario[5] <- "Growth at 2100 + 5lag BHM"
ex$scenario[6] <- "No growth effects > 2100"
ex$scenario[7] <- "No impacts > 2100"

ex$dr1[1] <- round(sum(scc_2300_2100_growth$weighted_damages1_scld, na.rm = T),0)
ex$dr1[2] <- round(sum(scc_2300_1pct_growth$weighted_damages1_scld, na.rm = T),0)
ex$dr1[3] <- round(sum(scc_2300_2pct_growth$weighted_damages1_scld, na.rm = T),0)
ex$dr1[4] <- round(sum(scc_2300_clamped_growth$weighted_damages1_scld, na.rm = T),0)
ex$dr1[5] <- round(sum(scc_2300_2100_5lag$weighted_damages1_scld, na.rm = T),0)
ex$dr1[6] <- round(sum(scc_2300_nog_post_2100$weighted_damages1_scld, na.rm = T),0)
ex$dr1[7] <- round(sum(scc_2100$weighted_damages1_scld, na.rm = T),0)

ex$dr2[1] <- round(sum(scc_2300_2100_growth$weighted_damages2_scld, na.rm = T),0)
ex$dr2[2] <- round(sum(scc_2300_1pct_growth$weighted_damages2_scld, na.rm = T),0)
ex$dr2[3] <- round(sum(scc_2300_2pct_growth$weighted_damages2_scld, na.rm = T),0)
ex$dr2[4] <- round(sum(scc_2300_clamped_growth$weighted_damages2_scld, na.rm = T),0)
ex$dr2[5] <- round(sum(scc_2300_2100_5lag$weighted_damages2_scld, na.rm = T),0)
ex$dr2[6] <- round(sum(scc_2300_nog_post_2100$weighted_damages2_scld, na.rm = T),0)
ex$dr2[7] <- round(sum(scc_2100$weighted_damages2_scld, na.rm = T),0)

ex$dr3[1] <- round(sum(scc_2300_2100_growth$weighted_damages3_scld, na.rm = T),0)
ex$dr3[2] <- round(sum(scc_2300_1pct_growth$weighted_damages3_scld, na.rm = T),0)
ex$dr3[3] <- round(sum(scc_2300_2pct_growth$weighted_damages3_scld, na.rm = T),0)
ex$dr3[4] <- round(sum(scc_2300_clamped_growth$weighted_damages3_scld, na.rm = T),0)
ex$dr3[5] <- round(sum(scc_2300_2100_5lag$weighted_damages3_scld, na.rm = T),0)
ex$dr3[6] <- round(sum(scc_2300_nog_post_2100$weighted_damages3_scld, na.rm = T),0)
ex$dr3[7] <- round(sum(scc_2100$weighted_damages3_scld, na.rm = T),0)


ex$dr_ramsey[1] <- round(sum(scc_2300_2100_growth$weighted_damages_ramsey_scld, na.rm = T),0)
ex$dr_ramsey[2] <- round(sum(scc_2300_1pct_growth$weighted_damages_ramsey_scld, na.rm = T),0)
ex$dr_ramsey[3] <- round(sum(scc_2300_2pct_growth$weighted_damages_ramsey_scld, na.rm = T),0)
ex$dr_ramsey[4] <- round(sum(scc_2300_clamped_growth$weighted_damages_ramsey_scld, na.rm = T),0)
ex$dr_ramsey[5] <- round(sum(scc_2300_2100_5lag$weighted_damages_ramsey_scld, na.rm = T),0)
ex$dr_ramsey[6] <- round(sum(scc_2300_nog_post_2100$weighted_damages_ramsey_scld, na.rm = T),0)
ex$dr_ramsey[7] <- round(sum(scc_2100$weighted_damages_ramsey_scld, na.rm = T),0)


# now let us add the columns 
# first we need to add time horizon 
ex$time_horizon[1] <- "through 2300"
ex$time_horizon[2] <- "through 2300"
ex$time_horizon[3] <- "through 2300"
ex$time_horizon[4] <- "through 2300"
ex$time_horizon[5] <- "through 2300"
ex$time_horizon[6] <- "through 2300"
ex$time_horizon[7] <- "through 2100"

# now we need to do post-2100 growth column 
ex$post_2100_growth[1] <- "SSP 2100 growth rate"
ex$post_2100_growth[2] <- "1% growth rate"
ex$post_2100_growth[3] <- "2% growth rate"
ex$post_2100_growth[4] <- "SSP 2100 clamped rate"
ex$post_2100_growth[5] <- "SSP 2100 rates"
ex$post_2100_growth[6] <- "0% growth rate"
ex$post_2100_growth[7] <- ""

# now let us do regression model 
ex$regression_model[1] <- "0-lag BHM model"
ex$regression_model[2] <- "0-lag BHM model"
ex$regression_model[3] <- "0-lag BHM model"
ex$regression_model[4] <- "0-lag BHM model"
ex$regression_model[5] <- "5-lag BHM model"
ex$regression_model[6] <- "0-lag BHM model"
ex$regression_model[7] <- "0-lag BHM model"

# ok ready to plot 
write_rds(ex, paste0(fig_prepped_dta, run_date, "scc_under_diff_scenarios.rds"))

# end of script 