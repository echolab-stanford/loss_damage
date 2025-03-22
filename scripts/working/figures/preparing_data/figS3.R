##############################################################################
# Mustafa Zahid, January 7th, 2023
# This R script reads the data and prepares the necessary data to plots figure
# 2. Figure 2 
#############################################################################
remove(list=ls())
gc()
sf::sf_use_s2(FALSE)
setwd("~/GitHub/loss_damage")

#replicate <- F# change T to F if you want to create your own data  
#if (replicate == T){
#  run_date <- "20230523"
#}
#if (replicate == F){
#  run_date <- gsub("-","",Sys.Date())
#}

run_date <- "loss_damage_r1_replication_v2"

# read in the needed libraries 
source("scripts/working/analysis/0_read_libs.R")

setwd(dropbox_path)
#############################################################################
#############################################################################
# read data 
total_damages_1gtco2_0lag <- readRDS(paste0("~/BurkeLab Dropbox/Projects/loss_damage/data/output/20230821/scc_2100.rds"))
total_damages_1gtco2_5lag <- readRDS(paste0(output_path, "/total_damages_1gtco2_1990_2020.rds"))
total_damages_1gtco2_1lag <- readRDS(paste0(output_path, "/total_damages_1gtco2_1lag_1990_2020.rds"))
total_damages_1gtco2_2lag <- readRDS(paste0(output_path, "/total_damages_1gtco2_2lag_1990_2020.rds"))
total_damages_1gtco2_3lag <- readRDS(paste0(output_path, "/total_damages_1gtco2_3lag_1990_2020.rds"))
total_damages_1gtco2_4lag <- readRDS(paste0(output_path, "/total_damages_1gtco2_4lag_1990_2020.rds"))
total_damages_1gtco2_6lag <- readRDS(paste0(output_path, "/total_damages_1gtco2_6lag_1990_2020.rds"))
total_damages_1gtco2_7lag <- readRDS(paste0(output_path, "/total_damages_1gtco2_7lag_1990_2020.rds"))
total_damages_1gtco2_8lag <- readRDS(paste0(output_path, "/total_damages_1gtco2_8lag_1990_2020.rds"))
total_damages_1gtco2_9lag <- readRDS(paste0(output_path, "/total_damages_1gtco2_9lag_1990_2020.rds"))
total_damages_1gtco2_10lag <- readRDS(paste0(output_path, "/total_damages_1gtco2_10lag_1990_2020.rds"))

#############################################################################
#############################################################################
################################################################################ supplemental figure

# write the function that would summarise the datasets into damages for each 
# discount rate
prep_data <- function(dataset){
  dataset <- subset(dataset, emitter == 2020)
  dataset <- dataset %>% 
    ungroup(.) %>% 
    dplyr::summarise(total_damages_dr1_5 = sum(weighted_damages1_5_scld, na.rm = T),
                     total_damages_dr2 = sum(weighted_damages2_scld, na.rm = T),
                     total_damages_dr_ramsey = sum(weighted_damages_ramsey_scld, na.rm = T),
                     total_damages_dr3 = sum(weighted_damages3_scld, na.rm = T),
                     total_damages_dr5 = sum(weighted_damages5_scld, na.rm = T),
                     total_damages_dr7 = sum(weighted_damages7_scld, na.rm = T),
                     .groups= "keep")
  return(dataset)
}
rescale_data <- function(dataset){
  dataset <- dataset %>% 
    ungroup(.) %>% 
    dplyr::mutate(total_damages_dr1_5 = (total_damages_dr1_5/1000000000),
                  total_damages_dr2 = (total_damages_dr2/1000000000),
                  total_damages_dr_ramsey = sum(total_damages_dr_ramsey/1000000000),
                  total_damages_dr3 = (total_damages_dr3/1000000000),
                  total_damages_dr5 = (total_damages_dr5/1000000000),
                  total_damages_dr7 = (total_damages_dr7/1000000000))
}

# ok now let us summarize all the datasets we have 
total_damages_1gtco2_0lag <- prep_data(total_damages_1gtco2_0lag)
total_damages_1gtco2_0lag$model <- "0-lag"
total_damages_1gtco2_1lag <- rescale_data(prep_data(total_damages_1gtco2_1lag))
total_damages_1gtco2_1lag$model <- "1-lag"
total_damages_1gtco2_2lag <- rescale_data(prep_data(total_damages_1gtco2_2lag))
total_damages_1gtco2_2lag$model <- "2-lag"
total_damages_1gtco2_3lag <- rescale_data(prep_data(total_damages_1gtco2_3lag))
total_damages_1gtco2_3lag$model <- "3-lag"
total_damages_1gtco2_4lag <- rescale_data(prep_data(total_damages_1gtco2_4lag))
total_damages_1gtco2_4lag$model <- "4-lag"
total_damages_1gtco2_5lag <- rescale_data(prep_data(total_damages_1gtco2_5lag))
total_damages_1gtco2_5lag$model <- "5-lag"
total_damages_1gtco2_6lag <- rescale_data(prep_data(total_damages_1gtco2_6lag))
total_damages_1gtco2_6lag$model <- "6-lag"
total_damages_1gtco2_7lag <- rescale_data(prep_data(total_damages_1gtco2_7lag))
total_damages_1gtco2_7lag$model <- "7-lag"
total_damages_1gtco2_8lag <- rescale_data(prep_data(total_damages_1gtco2_8lag))
total_damages_1gtco2_8lag$model <- "8-lag"
total_damages_1gtco2_9lag <- rescale_data(prep_data(total_damages_1gtco2_9lag))
total_damages_1gtco2_9lag$model <- "9-lag"
total_damages_1gtco2_10lag <- rescale_data(prep_data (total_damages_1gtco2_10lag))
total_damages_1gtco2_10lag$model <- "10-lag"

# ok now let us bring it all on one dataset 
total_damages_diff_lags <- rbind(total_damages_1gtco2_0lag, 
                                 total_damages_1gtco2_1lag,
                                 total_damages_1gtco2_2lag,
                                 total_damages_1gtco2_3lag,
                                 total_damages_1gtco2_4lag,
                                 total_damages_1gtco2_5lag,
                                 total_damages_1gtco2_6lag,
                                 total_damages_1gtco2_7lag,
                                 total_damages_1gtco2_8lag,
                                 total_damages_1gtco2_9lag,
                                 total_damages_1gtco2_10lag)

### data is ready to plot. We can write them into directory
setwd("~/GitHub/loss_damage")
write_rds(total_damages_diff_lags, paste0(fig_prepped_dta, run_date, "/total_damages_diff_lags.rds"))


# end of script 