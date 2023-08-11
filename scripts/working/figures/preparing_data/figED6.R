##############################################################################
# Mustafa Zahid, August 7th, 2023
# This R script reads the data and prepares the necessary data to plots figure
# 1. Figure 1 demonstrates the different steps taken to calculate teh damages
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

run_date <- "20230708"

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

setwd(dropbox_path)
#############################################################################
#############################################################################
# read data 
total_damages_1tco2 <- readRDS(paste0(output_path,"/total_damages_1tco2_k90_compare.rds"))
total_damages_10tco2 <- readRDS(paste0(output_path,"/total_damages_10tco2_k90_compare.rds"))
total_damages_1000tco2 <- readRDS(paste0(output_path,"/total_damages_1000tco2_k90_compare.rds"))
total_damages_1mtco2 <- readRDS(paste0(output_path,"/total_damages_1mtco2_k90_compare.rds"))
total_damages_1gtco2 <- readRDS(paste0(output_path,"/total_damages_1gtco2_k90_compare.rds"))
total_damages_10gtco2 <- readRDS(paste0(output_path,"/total_damages_10gtco2_k90_compare.rds"))
total_damages_100gtco2 <- readRDS(paste0(output_path,"/total_damages_100gtco2_k90_compare.rds"))

#############################################################################
#############################################################################

datasets <- list(total_damages_1tco2,
                 total_damages_10tco2,
                 total_damages_1000tco2,
                 total_damages_1mtco2,
                 total_damages_1gtco2,
                 total_damages_10gtco2,
                 total_damages_100gtco2)

processed_datasets <- list()
i <- 1
for (i in 1:length(datasets)){
  tic()
  dataset <- datasets[[i]] 
  dataset <- dataset %>% 
    dplyr::mutate(year_cat = case_when(year < 2020 ~ "1990-2020",
                                       year >2020 ~ "2021-2100"))
  
  dataset <- subset(dataset, !is.na(year_cat))
  
  dataset <- dataset %>% 
    dplyr::group_by(emitter, year_cat) %>% 
    dplyr::summarise(total_damages_dr2 = sum(weighted_damages2_scld, na.rm = T),
                     .groups= "keep")
  dataset <- dataset %>% dplyr::select(c("year_cat", 
                                         "emitter",
                                         "total_damages_dr2"))
  dataset$pulse <- i 

  processed_datasets[[i]] <- dataset
  toc()
}


mother_dataset <- do.call(rbind, processed_datasets)

mother_dataset <- mother_dataset %>% 
  dplyr::mutate(pulse_exp = case_when(pulse == 1 ~ "1tCO2",
                                      pulse == 2 ~ "10tCO2",
                                      pulse == 3 ~ "1000tCO2",
                                      pulse == 4 ~ "1MtCO2",
                                      pulse == 5 ~ "1GtCO2",
                                      pulse == 6 ~ "10GtCO2",
                                      pulse == 7 ~ "100GtCO2"))

# ok let us reshape
mother_dataset <- mother_dataset[,-2]
mother_dataset$id <- paste0(mother_dataset$pulse_exp, "_", mother_dataset$year_cat)
mother_dataset <- mother_dataset[,-1]
mother_dataset <- mother_dataset[,-3]
mother_dataset <- mother_dataset[,-2]

mother_dataset_reshaped <- melt(mother_dataset, id = c("id"))

total_damages_1gtC1 <- subset(total_damages_1gtco2, emitter <=  2020 & emitter >1989)

total_damages_1gtC1 <- total_damages_1gtC1 %>% 
  dplyr::mutate(year_cat = case_when(year < 2020 ~ "1990-2020",
                                     year >2020 ~ "2021-2100"))

total_damages_by_pulse <- total_damages_1gtC1 %>% 
  dplyr::group_by(emitter, year_cat) %>% 
  dplyr::summarise(total_damages_dr2 = sum(weighted_damages2_scld, na.rm = T),
                   total_damages_dr3 = sum(weighted_damages3_scld, na.rm = T),
                   total_damages_dr5 = sum(weighted_damages5_scld, na.rm = T),
                   total_damages_dr7 = sum(weighted_damages7_scld, na.rm = T),
                   .groups= "keep")

total_damages_by_pulse <- total_damages_by_pulse %>% dplyr::select(c("year_cat", 
                                                                     "emitter",
                                                                     "total_damages_dr2",
                                                                     "total_damages_dr3",
                                                                     "total_damages_dr5",
                                                                     "total_damages_dr7"))

ex <- data.frame(hd_actual = c(NA, NA, NA, NA, NA, NA, NA), 
                 hd_comp = c(NA, NA, NA, NA, NA, NA, NA),
                 hd_pct = c(NA, NA, NA, NA, NA, NA, NA),
                 fd_actual = c(NA, NA, NA, NA, NA, NA, NA),
                 fd_comp = c(NA, NA, NA, NA, NA, NA, NA),
                 fd_pct = c(NA, NA, NA, NA, NA, NA, NA))

ex$pulse[1] <- "1tCO2"
ex$pulse[2] <- "10tCO2"
ex$pulse[3] <- "1000tCO2"
ex$pulse[4] <- "1MtCO2"
ex$pulse[5] <- "1GtCO2"
ex$pulse[6] <- "10GtCO2"
ex$pulse[7] <- "100GtCO2"

ex$hd_actual[1] <- round(mother_dataset_reshaped$value[mother_dataset_reshaped$id == "1tCO2_1990-2020"], 2)
ex$hd_actual[2] <- round(mother_dataset_reshaped$value[mother_dataset_reshaped$id == "10tCO2_1990-2020"], 0)
ex$hd_actual[3] <- round(mother_dataset_reshaped$value[mother_dataset_reshaped$id == "1000tCO2_1990-2020"], 0)
ex$hd_actual[4] <- round(mother_dataset_reshaped$value[mother_dataset_reshaped$id == "1MtCO2_1990-2020"], 0)
ex$hd_actual[5] <- round(mother_dataset_reshaped$value[mother_dataset_reshaped$id == "1GtCO2_1990-2020"], 0)
ex$hd_actual[6] <- round(mother_dataset_reshaped$value[mother_dataset_reshaped$id == "10GtCO2_1990-2020"], 0)
ex$hd_actual[7] <- round(mother_dataset_reshaped$value[mother_dataset_reshaped$id == "100GtCO2_1990-2020"], 0)

ex$hd_comp[1] <- round(mother_dataset_reshaped$value[mother_dataset_reshaped$id == "1GtCO2_1990-2020"]*(1/1000000000), 2)
ex$hd_comp[2] <- round(mother_dataset_reshaped$value[mother_dataset_reshaped$id == "1GtCO2_1990-2020"]*(1/100000000), 0)
ex$hd_comp[3] <- round(mother_dataset_reshaped$value[mother_dataset_reshaped$id == "1GtCO2_1990-2020"]*(1/1000000), 0)
ex$hd_comp[4] <- round(mother_dataset_reshaped$value[mother_dataset_reshaped$id == "1GtCO2_1990-2020"]*(1/1000),0)
ex$hd_comp[5] <- round(mother_dataset_reshaped$value[mother_dataset_reshaped$id == "1GtCO2_1990-2020"], 0)
ex$hd_comp[6] <- round(mother_dataset_reshaped$value[mother_dataset_reshaped$id == "1GtCO2_1990-2020"], 0)*10
ex$hd_comp[7] <- round(mother_dataset_reshaped$value[mother_dataset_reshaped$id == "1GtCO2_1990-2020"], 0)*100

ex$hd_actual <- as.numeric(ex$hd_actual)

ex$hd_pct <- round((ex$hd_actual/ ex$hd_comp)*100, 0)


ex$fd_actual[1] <- round(mother_dataset_reshaped$value[mother_dataset_reshaped$id == "1tCO2_2021-2100"], 2)
ex$fd_actual[2] <- round(mother_dataset_reshaped$value[mother_dataset_reshaped$id == "10tCO2_2021-2100"], 0)
ex$fd_actual[3] <- round(mother_dataset_reshaped$value[mother_dataset_reshaped$id == "1000tCO2_2021-2100"], 0)
ex$fd_actual[4] <- round(mother_dataset_reshaped$value[mother_dataset_reshaped$id == "1MtCO2_2021-2100"], 0)
ex$fd_actual[5] <- round(mother_dataset_reshaped$value[mother_dataset_reshaped$id == "1GtCO2_2021-2100"], 0)
ex$fd_actual[6] <- round(mother_dataset_reshaped$value[mother_dataset_reshaped$id == "10GtCO2_2021-2100"], 0)
ex$fd_actual[7] <- round(mother_dataset_reshaped$value[mother_dataset_reshaped$id == "100GtCO2_2021-2100"], 0)

ex$fd_comp[1] <- round(mother_dataset_reshaped$value[mother_dataset_reshaped$id == "1GtCO2_2021-2100"]*(1/1000000000), 2)
ex$fd_comp[2] <- round(mother_dataset_reshaped$value[mother_dataset_reshaped$id == "1GtCO2_2021-2100"]*(1/100000000), 0)
ex$fd_comp[3] <- round(mother_dataset_reshaped$value[mother_dataset_reshaped$id == "1GtCO2_2021-2100"]*(1/1000000), 0)
ex$fd_comp[4] <- round(mother_dataset_reshaped$value[mother_dataset_reshaped$id == "1GtCO2_2021-2100"]*(1/1000),0)
ex$fd_comp[5] <- round(mother_dataset_reshaped$value[mother_dataset_reshaped$id == "1GtCO2_2021-2100"], 0)
ex$fd_comp[6] <- round(mother_dataset_reshaped$value[mother_dataset_reshaped$id == "1GtCO2_2021-2100"], 0)*10
ex$fd_comp[7] <- round(mother_dataset_reshaped$value[mother_dataset_reshaped$id == "1GtCO2_2021-2100"], 0)*100

ex$fd_actual <- as.numeric(ex$fd_actual)

ex$fd_pct <- round((ex$fd_actual/ ex$fd_comp)*100, 0)

ex <- ex %>% 
  dplyr::select(c("pulse", 
                  "hd_actual", 
                  "hd_comp", 
                  "hd_pct", 
                  "fd_actual", 
                  "fd_comp",
                  "fd_pct"))

ex$hd_pct <- paste0(ex$hd_pct, "%")
ex$fd_pct <- paste0(ex$fd_pct, "%")

ex$hd_actual[7] <- paste0("$", as.character(round(ex$hd_actual[7]/(1000000000*100),2)))
ex$hd_comp[7] <- paste0("$", as.character(round(ex$hd_comp[7]/(1000000000*100),2)))
ex$fd_actual[7] <- paste0("$", as.character(round(ex$fd_actual[7]/(1000000000*100),0)))
ex$fd_comp[7] <- paste0("$", as.character(round(ex$fd_comp[7]/(1000000000*100),0)))

ex$hd_actual[6] <- paste0("$", as.character(round(as.numeric(ex$hd_actual[6])/(1000000000*10),2)))
ex$hd_comp[6] <- paste0("$", as.character(round(as.numeric(ex$hd_comp[6])/(1000000000*10),2)))
ex$fd_actual[6] <- paste0("$", as.character(round(as.numeric(ex$fd_actual[6])/(1000000000*10),0)))
ex$fd_comp[6] <- paste0("$", as.character(round(as.numeric(ex$fd_comp[6])/(1000000000*10),0)))

ex$hd_actual[5] <- paste0("$", as.character(round(as.numeric(ex$hd_actual[5])/1000000000,2)))
ex$hd_comp[5] <- paste0("$", as.character(round(as.numeric(ex$hd_comp[5])/1000000000,2)))
ex$fd_actual[5] <- paste0("$", as.character(round(as.numeric(ex$fd_actual[5])/1000000000,0)))
ex$fd_comp[5] <- paste0("$", as.character(round(as.numeric(ex$fd_comp[5])/1000000000,0)))

ex$hd_actual[4] <- paste0("$", as.character(round(as.numeric(ex$hd_actual[4])/1000000,2)))
ex$hd_comp[4] <- paste0("$", as.character(round(as.numeric(ex$hd_comp[4])/1000000,2)))
ex$fd_actual[4] <- paste0("$", as.character(round(as.numeric(ex$fd_actual[4])/1000000,0)))
ex$fd_comp[4] <- paste0("$", as.character(round(as.numeric(ex$fd_comp[4])/1000000,0)))

ex$hd_actual[3] <- paste0("$", as.character(round(as.numeric(ex$hd_actual[3])/1000,2)))
ex$hd_comp[3] <- paste0("$", as.character(round(as.numeric(ex$hd_comp[3])/1000,2)))
ex$fd_actual[3] <- paste0("$", as.character(round(as.numeric(ex$fd_actual[3])/1000,0)))
ex$fd_comp[3] <- paste0("$", as.character(round(as.numeric(ex$fd_comp[3])/1000,0)))

#ex$hd_actual[2] <- paste0("$", as.character(round(as.numeric(ex$hd_actual[2]),0)))
#ex$hd_comp[2] <- paste0("$", as.character(round(as.numeric(ex$hd_comp[2]),0)))
#ex$fd_actual[2] <- paste0("$", as.character(round(as.numeric(ex$fd_actual[2])/1000,2)), "K")
#ex$fd_comp[2] <- paste0("$", as.character(round(as.numeric(ex$fd_comp[2])/1000,2)), "K")
#
#ex$hd_actual[1] <- paste0("$", as.character(round(as.numeric(ex$hd_actual[1]),2)))
#ex$hd_comp[1] <- paste0("$", as.character(round(as.numeric(ex$hd_comp[1]),2)))
#ex$fd_actual[1] <- paste0("$", as.character(round(as.numeric(ex$fd_actual[1]),0)))
#ex$fd_comp[1] <- paste0("$", as.character(round(as.numeric(ex$fd_comp[1]),0)))

ex <- ex %>% dplyr::select(c("pulse", "hd_actual", "hd_pct", "fd_actual", "fd_pct"))
ex <- ex[-1:-2,]

################################################################################
################################################################################
# plot data 
################################################################################ Figure 3a
ex %>%
  tibble%>%
  #group_by(emitter) %>% 
  gt(rowname_col = "pulse") %>% 
  #dplyr::mutate(total_damages_2020_dr2 = paste0("$", total_damages_2020_dr2)) %>% 
  #tab_spanner(label = "HD-GHG",
  #            columns = vars(hd_actual,
  #                           hd_pct)) %>% 
  #tab_spanner(label = "FD-GHG",
  #            columns = vars(fd_actual,
  #                           fd_pct)) %>% 
  cols_label(hd_actual = "Per tonne HD",
             hd_pct = "% Difference relative to 1GtCO2 pulse",
             fd_actual = "Per tonne FD",
             fd_pct = "% Difference relative to 1GtCO2 pulse") %>% 
  cols_align(align = "right",
             columns = c(pulse)) %>% 
  cols_align(align = "center",
             columns = c(hd_actual,
                         fd_actual)) %>% 
  cols_align(align = "center", 
             columns = c(hd_pct,
                         fd_pct)) %>% 
  gt_theme_538_nocaps(table.width = px(700)) %>%
  gtsave(paste0("/Users/mustafazahid/GitHub/loss_damage/figures/", 
                run_date,"/fig_compare_est.png"))

#end of script




# end of script 