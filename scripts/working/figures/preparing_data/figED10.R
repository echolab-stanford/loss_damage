##############################################################################
# Mustafa Zahid, January 7th, 2023
# This R script reads the data and prepares the necessary data to plots figure
# ED13. Figure ED13 demonstrates the different steps taken to calculate teh damages
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
run_date <- "loss_damage_r1"
# read in the needed libraries 
source("scripts/working/analysis/0_read_libs.R")


################################################################################
################################################################################
# read data 
total_damages_uncertainty_cgm <- readRDS(paste0(output_path, "/total_damages_1gtco2_cgm.rds"))
total_damages_uncertainty_fair <- readRDS(paste0(output_path, "/total_damages_1gtco2_fair.rds"))
#total_damages_uncertainty_bhm <- readRDS(paste0(output_path, "/total_damages_1gtco2_bhm.rds"))


#total_damages_uncertainty_cgm <- readRDS("~/Desktop/total_damages_1gtco2_cgm.rds")
#total_damages_uncertainty_cgm <- readRDS("/Users/mustafazahid/BurkeLab Dropbox/Mustafa Zahid/total_damages_1gtco2_cgm.rds")

#total_damages_uncertainty_fair <- readRDS("~/Desktop/total_damages_1gtco2_fair.rds")
#total_damages_uncertainty_bhm <- readRDS(paste0(output_path, "/total_damages_1gtco2_bhm.rds"))


################################################################################ bhm 
# generate list of files 
path <- paste0(output_path, "/20240311/")

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

totals_bhm_nog_2dr <- do.call(rbind, listofdfs_2dr)

################################################################################ all sources
# generate list of files 
# set path and get list of files from directory
path <- paste0(output_path, "/20240311_2/")

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

totals_all_nog_2dr <- do.call(rbind, listofdfs_2dr)

################################################################################
################################################################################
# prepare data for plotting

# cgm uncertainty
totals_cgm <- total_damages_uncertainty_cgm %>% dplyr::group_by(cgm_id) %>% 
  dplyr::summarise(total_damages = sum(weighted_damages2_scld, na.rm = T))

# bhm uncertainty
totals_bhm <- totals_bhm_nog_2dr
totals_bhm <- ungroup(totals_bhm)
totals_bhm <- totals_bhm %>% dplyr::select(-c("emitter"))

# fair uncertainty
totals_fair <- total_damages_uncertainty_fair %>% dplyr::group_by(fair_id) %>% 
  dplyr::summarise(total_damages = sum(weighted_damages2_scld, na.rm = T))
median(totals_fair$total_damages)

# total uncertainty
totals_all <- totals_all_nog_2dr

# check medians
median(totals_all$total_damages2)
median(totals_bhm$total_damages2)
median(totals_cgm$total_damages)
median(totals_fair$total_damages)


# ok now write the data 
write_rds(totals_all, paste0(fig_prepped_dta, run_date,"/totals_all1.rds"))
write_rds(totals_bhm, paste0(fig_prepped_dta, run_date,"/totals_bhm1.rds"))
write_rds(totals_cgm, paste0(fig_prepped_dta, run_date,"/totals_cgm1.rds"))
write_rds(totals_fair, paste0(fig_prepped_dta,run_date, "/totals_fair1.rds"))

# end of script

write_rds(totals_all,"~/Desktop/totals_all.rds")
write_rds(totals_bhm, "~/Desktop/totals_bhm.rds")
write_rds(totals_cgm, "~/Desktop/totals_cgm.rds")
write_rds(totals_fair, "~/Desktop/totals_fair.rds")

