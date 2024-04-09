##############################################################################
# Mustafa Zahid, January 7th, 2023
# This R script reads the data and prepares the necessary data to plots figure
# ED13. Figure ED13 demonstrates the different steps taken to calculate teh damages
#############################################################################
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
total_damages_uncertainty_cgm <- readRDS(paste0(output_path, "/total_damages_1gtco2_cgm.rds"))
total_damages_uncertainty_fair <- readRDS(paste0(output_path, "/total_damages_1gtco2_fair.rds"))
total_damages_uncertainty_bhm <- readRDS(paste0(output_path, "/total_damages_1gtco2_bhm.rds"))


total_damages_uncertainty_cgm <- readRDS("~/Desktop/total_damages_1gtco2_cgm.rds")
total_damages_uncertainty_fair <- readRDS("~/Desktop/total_damages_1gtco2_fair.rds")
#total_damages_uncertainty_bhm <- readRDS(paste0(output_path, "/total_damages_1gtco2_bhm.rds"))


################################################################################ no growth post 2100
# generate list of files 
path <- paste0("~/BurkeLab Dropbox/Projects/loss_damage/data/output/20240311/")

all_data = list.files(path=path,
                      pattern = "scc_" ,
                      full.names = TRUE,
                      recursive = TRUE,
                      include.dirs = FALSE)


# now read files into one list 
#listofdfs_rams <- list()
listofdfs_2dr <- list()
for (i in 1:1000){
  tic()
  data_i <- readRDS(all_data[i])
  #data_i$loop_id <- i
  data_i <- data_i %>% 
    dplyr::select(c("emitter", 
                    "year",
                    "sim_id",
                    "weighted_damages2_scld", 
                    "weighted_damages_ramsey_scld",
                    "coef_id",
                    "temp",
                    "temp2"))
  #  data_ramsey_i <- data_i %>% 
  #    dplyr::group_by(emitter, sim_id) %>% 
  #    dplyr::summarise(total_damages_ramsey = sum(weighted_damages_ramsey_scld, na.rm = T))
  
  data_dr2_i <- data_i %>% 
    dplyr::group_by(emitter, coef_id) %>% 
    dplyr::summarise(total_damages2 = sum(weighted_damages2_scld, na.rm = T))
  
  #  listofdfs_rams[[i]] <- data_ramsey_i
  listofdfs_2dr[[i]] <- data_dr2_i
  
  toc()
}

totals_nog_2dr <- do.call(rbind, listofdfs_2dr)
#totals_nog_ramsey <- do.call(rbind, listofdfs_rams)



################################################################################ no growth post 2100
# generate list of files 
# set path and get list of files from directory

################################################################################ no growth post 2100
# generate list of files 
path <- paste0("~/BurkeLab Dropbox/Projects/loss_damage/data/output/20240311_2")

all_data = list.files(path=path,
                      pattern = "scc_" ,
                      full.names = TRUE,
                      recursive = TRUE,
                      include.dirs = FALSE)


# now read files into one list 
#listofdfs_rams <- list()
listofdfs_2dr <- list()
for (i in 1:1000){
  tic()
  data_i <- readRDS(all_data[i])
  #data_i$loop_id <- i
  data_i <- data_i %>% 
    dplyr::select(c("emitter", 
                    "year",
                    "sim_id",
                    "weighted_damages2_scld", 
                    "weighted_damages_ramsey_scld",
                    "coef_id",
                    "temp",
                    "temp2"))
#  data_ramsey_i <- data_i %>% 
#    dplyr::group_by(emitter, sim_id) %>% 
#    dplyr::summarise(total_damages_ramsey = sum(weighted_damages_ramsey_scld, na.rm = T))
  
  data_dr2_i <- data_i %>% 
    dplyr::group_by(emitter, sim_id) %>% 
    dplyr::summarise(total_damages2 = sum(weighted_damages2_scld, na.rm = T))
  
#  listofdfs_rams[[i]] <- data_ramsey_i
  listofdfs_2dr[[i]] <- data_dr2_i
  
  toc()
}

totals_nog_2dr <- do.call(rbind, listofdfs_2dr)
#totals_nog_ramsey <- do.call(rbind, listofdfs_rams)

totals_nog_2dr <- subset(totals_nog_2dr, sim_id > 1000 & sim_id < 2001)

#total_damages_uncertainty_total <- readRDS(paste0(output_path, "/total_damages_1gtco2_total.rds"))
#total_damages_uncertainty_total2 <- readRDS(paste0(output_path, "/total_damages_1gtco2_total2.rds"))
#total_damages_uncertainty_total3 <- readRDS(paste0(output_path, "/total_damages_1gtco2_total3.rds"))
#total_damages_uncertainty_total <- rbind(total_damages_uncertainty_total,
#                                         total_damages_uncertainty_total2,
#                                         total_damages_uncertainty_total3)
# prepare data for plotting

# cgm uncertainty
totals_cgm <- total_damages_uncertainty_cgm %>% dplyr::group_by(cgm_id) %>% 
  dplyr::summarise(total_damages = sum(weighted_damages2_scld, na.rm = T))

# bhm uncertainty
totals_bhm <- totals_nog_2dr %>% dplyr::group_by(coef_id) %>% 
  dplyr::summarise(total_damages = sum(weighted_damages2_scld, na.rm = T))


# fair uncertainty
totals_fair <- total_damages_uncertainty_fair %>% dplyr::group_by(fair_id) %>% 
  dplyr::summarise(total_damages = sum(weighted_damages2_scld, na.rm = T))
median(totals_fair$total_damages)


# total uncertainty
totals_all <- totals_nog_2dr 
totals_all <- ungroup(totals_all)
totals_all <- totals_all %>% dplyr::select(-c("emitter"))

median(totals_all$total_damages2)


totals_bhm <- ungroup(totals_bhm)
totals_bhm <- totals_bhm %>% dplyr::select(-c("emitter"))

median(totals_all$total_damages2)
median(totals_bhm$total_damages2)
median(totals_cgm$total_damages)
median(totals_fair$total_damages)

#totals_all <- totals_all %>% dplyr::select(-c("emitter"))


# ok now write the data 
write_rds(totals_all, paste0(fig_prepped_dta, run_date,"/totals_all.rds"))
write_rds(totals_bhm, paste0(fig_prepped_dta, run_date,"/totals_bhm.rds"))
write_rds(totals_cgm, paste0(fig_prepped_dta, run_date,"/totals_cgm.rds"))
write_rds(totals_fair, paste0(fig_prepped_dta,run_date, "/totals_fair.rds"))

# end of script

write_rds(totals_all,"~/Desktop/totals_all.rds")
write_rds(totals_bhm, "~/Desktop/totals_bhm.rds")
write_rds(totals_cgm, "~/Desktop/totals_cgm.rds")
write_rds(totals_fair, "~/Desktop/totals_fair.rds")

