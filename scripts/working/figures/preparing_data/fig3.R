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

setwd(dropbox_path)
#############################################################################

total_damages_1gtco2 <- readRDS(paste0(dropbox_path, "data/output/20230628/total_damages_1gtco2_1990_2020.rds"))
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

total_damages_by_pulse$total_damages_dr2 <- total_damages_by_pulse$total_damages_dr2 * (1 /1000000000)
total_damages_by_pulse$total_damages_dr3 <- total_damages_by_pulse$total_damages_dr3 * (1 /1000000000)
total_damages_by_pulse$total_damages_dr5 <- total_damages_by_pulse$total_damages_dr5 * (1 /1000000000)
total_damages_by_pulse$total_damages_dr7 <- total_damages_by_pulse$total_damages_dr7 * (1 /1000000000)

total_damages_by_pulse_2020 <- subset(total_damages_by_pulse, year_cat == "1990-2020")

total_damages_by_pulse_2020_2 <- total_damages_by_pulse_2020 %>% 
  dplyr::select(c("emitter", "total_damages_dr2")) %>% 
  dplyr::mutate(discount_rate = "2%")

colnames(total_damages_by_pulse_2020_2)[3] <- "total_damages"

total_damages_by_pulse_2020_3 <- total_damages_by_pulse_2020 %>% 
  dplyr::select(c("emitter", "total_damages_dr3")) %>% 
  dplyr::mutate(discount_rate = "3%")

colnames(total_damages_by_pulse_2020_3)[3] <- "total_damages"

total_damages_by_pulse_2020_5 <- total_damages_by_pulse_2020 %>% 
  dplyr::select(c("emitter", "total_damages_dr5")) %>% 
  dplyr::mutate(discount_rate = "5%")

colnames(total_damages_by_pulse_2020_5)[3] <- "total_damages"

total_damages_by_pulse_2020_7 <- total_damages_by_pulse_2020 %>% 
  dplyr::select(c("emitter", "total_damages_dr7")) %>% 
  dplyr::mutate(discount_rate = "7%")

colnames(total_damages_by_pulse_2020_7)[3] <- "total_damages"

total_damages_by_pulse_2020_all <- rbind(total_damages_by_pulse_2020_2,
                                         total_damages_by_pulse_2020_3,
                                         total_damages_by_pulse_2020_5,
                                         total_damages_by_pulse_2020_7)

#####


total_damages_by_pulse_2100 <- subset(total_damages_by_pulse, year_cat == "2021-2100")

total_damages_by_pulse_2100_2 <- total_damages_by_pulse_2100 %>% 
  dplyr::select(c("emitter", "total_damages_dr2")) %>% 
  dplyr::mutate(discount_rate = "2%")

colnames(total_damages_by_pulse_2100_2)[3] <- "total_damages"

total_damages_by_pulse_2100_3 <- total_damages_by_pulse_2100 %>% 
  dplyr::select(c("emitter", "total_damages_dr3")) %>% 
  dplyr::mutate(discount_rate = "3%")

colnames(total_damages_by_pulse_2100_3)[3] <- "total_damages"

total_damages_by_pulse_2100_5 <- total_damages_by_pulse_2100 %>% 
  dplyr::select(c("emitter", "total_damages_dr5")) %>% 
  dplyr::mutate(discount_rate = "5%")

colnames(total_damages_by_pulse_2100_5)[3] <- "total_damages"

total_damages_by_pulse_2100_7 <- total_damages_by_pulse_2100 %>% 
  dplyr::select(c("emitter", "total_damages_dr7")) %>% 
  dplyr::mutate(discount_rate = "7%")

colnames(total_damages_by_pulse_2100_7)[3] <- "total_damages"

total_damages_by_pulse_2100_all <- rbind(total_damages_by_pulse_2100_2,
                                         total_damages_by_pulse_2100_3,
                                         total_damages_by_pulse_2100_5,
                                         total_damages_by_pulse_2100_7)


total_damages_by_pulse_2100 <- ungroup(total_damages_by_pulse_2100)

test_df_for_table_2100 <- total_damages_by_pulse_2100 %>% dplyr::select(c("emitter",
                                                                          "total_damages_dr2",
                                                                          "total_damages_dr3",
                                                                          "total_damages_dr5",
                                                                          "total_damages_dr7"))

colnames(test_df_for_table_2100)[2] <- "total_damages_2100_dr2"
colnames(test_df_for_table_2100)[3] <- "total_damages_2100_dr3"
colnames(test_df_for_table_2100)[4] <- "total_damages_2100_dr5"
colnames(test_df_for_table_2100)[5] <- "total_damages_2100_dr7"

test_df_for_table_2100$total_damages_2100_dr2 <- round(test_df_for_table_2100$total_damages_2100_dr2, 0)
test_df_for_table_2100$total_damages_2100_dr3 <- round(test_df_for_table_2100$total_damages_2100_dr3, 0)
test_df_for_table_2100$total_damages_2100_dr5 <- round(test_df_for_table_2100$total_damages_2100_dr5, 0)
test_df_for_table_2100$total_damages_2100_dr7 <- round(test_df_for_table_2100$total_damages_2100_dr7, 0)


total_damages_by_pulse_2020 <- ungroup(total_damages_by_pulse_2020)

test_df_for_table_2020 <- total_damages_by_pulse_2020 %>% dplyr::select(c("emitter",
                                                                          "total_damages_dr2",
                                                                          "total_damages_dr3",
                                                                          "total_damages_dr5",
                                                                          "total_damages_dr7"))

colnames(test_df_for_table_2020)[2] <- "total_damages_2020_dr2"
colnames(test_df_for_table_2020)[3] <- "total_damages_2020_dr3"
colnames(test_df_for_table_2020)[4] <- "total_damages_2020_dr5"
colnames(test_df_for_table_2020)[5] <- "total_damages_2020_dr7"

test_df_for_table_2020$total_damages_2020_dr2[test_df_for_table_2020$emitter <2015] <- round(test_df_for_table_2020$total_damages_2020_dr2[test_df_for_table_2020$emitter <2015], 0)
test_df_for_table_2020$total_damages_2020_dr3[test_df_for_table_2020$emitter <2015] <- round(test_df_for_table_2020$total_damages_2020_dr3[test_df_for_table_2020$emitter <2015], 0)
test_df_for_table_2020$total_damages_2020_dr5[test_df_for_table_2020$emitter <2015] <- round(test_df_for_table_2020$total_damages_2020_dr5[test_df_for_table_2020$emitter <2015], 0)
test_df_for_table_2020$total_damages_2020_dr7[test_df_for_table_2020$emitter <2015] <- round(test_df_for_table_2020$total_damages_2020_dr7[test_df_for_table_2020$emitter <2015], 0)

test_df_for_table_2020$total_damages_2020_dr2[test_df_for_table_2020$emitter >= 2015 & test_df_for_table_2020$emitter < 2018] <- round(test_df_for_table_2020$total_damages_2020_dr2[test_df_for_table_2020$emitter >= 2015 & test_df_for_table_2020$emitter < 2018], 1)
test_df_for_table_2020$total_damages_2020_dr3[test_df_for_table_2020$emitter >= 2015 & test_df_for_table_2020$emitter < 2018] <- round(test_df_for_table_2020$total_damages_2020_dr3[test_df_for_table_2020$emitter >= 2015 & test_df_for_table_2020$emitter < 2018], 1)
test_df_for_table_2020$total_damages_2020_dr5[test_df_for_table_2020$emitter >= 2015 & test_df_for_table_2020$emitter < 2018] <- round(test_df_for_table_2020$total_damages_2020_dr5[test_df_for_table_2020$emitter >= 2015 & test_df_for_table_2020$emitter < 2018], 1)
test_df_for_table_2020$total_damages_2020_dr7[test_df_for_table_2020$emitter >= 2015 & test_df_for_table_2020$emitter < 2018] <- round(test_df_for_table_2020$total_damages_2020_dr7[test_df_for_table_2020$emitter >= 2015 & test_df_for_table_2020$emitter < 2018], 1)

test_df_for_table_2020$total_damages_2020_dr2[test_df_for_table_2020$emitter >= 2018 & test_df_for_table_2020$emitter < 2020] <- round(test_df_for_table_2020$total_damages_2020_dr2[test_df_for_table_2020$emitter >= 2018 & test_df_for_table_2020$emitter < 2020], 2)
test_df_for_table_2020$total_damages_2020_dr3[test_df_for_table_2020$emitter >= 2018 & test_df_for_table_2020$emitter < 2020] <- round(test_df_for_table_2020$total_damages_2020_dr3[test_df_for_table_2020$emitter >= 2018 & test_df_for_table_2020$emitter < 2020], 2)
test_df_for_table_2020$total_damages_2020_dr5[test_df_for_table_2020$emitter >= 2018 & test_df_for_table_2020$emitter < 2020] <- round(test_df_for_table_2020$total_damages_2020_dr5[test_df_for_table_2020$emitter >= 2018 & test_df_for_table_2020$emitter < 2020], 2)
test_df_for_table_2020$total_damages_2020_dr7[test_df_for_table_2020$emitter >= 2018 & test_df_for_table_2020$emitter < 2020] <- round(test_df_for_table_2020$total_damages_2020_dr7[test_df_for_table_2020$emitter >= 2018 & test_df_for_table_2020$emitter < 2020], 2)

test_df_for_table_2020$total_damages_2020_dr2[test_df_for_table_2020$emitter == 2020] <- round(test_df_for_table_2020$total_damages_2020_dr2[test_df_for_table_2020$emitter == 2020], 3)
test_df_for_table_2020$total_damages_2020_dr3[test_df_for_table_2020$emitter == 2020] <- round(test_df_for_table_2020$total_damages_2020_dr3[test_df_for_table_2020$emitter == 2020], 3)
test_df_for_table_2020$total_damages_2020_dr5[test_df_for_table_2020$emitter == 2020] <- round(test_df_for_table_2020$total_damages_2020_dr5[test_df_for_table_2020$emitter == 2020], 3)
test_df_for_table_2020$total_damages_2020_dr7[test_df_for_table_2020$emitter == 2020] <- round(test_df_for_table_2020$total_damages_2020_dr7[test_df_for_table_2020$emitter == 2020], 3)

test_df_for_table_2020$total_damages_2020_dr2 <- as.character(test_df_for_table_2020$total_damages_2020_dr2)
test_df_for_table_2020$total_damages_2020_dr3 <- as.character(test_df_for_table_2020$total_damages_2020_dr3)
test_df_for_table_2020$total_damages_2020_dr5 <- as.character(test_df_for_table_2020$total_damages_2020_dr5)
test_df_for_table_2020$total_damages_2020_dr7 <- as.character(test_df_for_table_2020$total_damages_2020_dr7)

#### now bring them together 
test_df_for_table <- left_join(test_df_for_table_2020,
                               test_df_for_table_2100,
                               by = c("emitter"))

# ok data is ready for plotting 
setwd("~/GitHub/loss_damage")
write_rds(test_df_for_table, paste0(getwd(), "/data/figures/20230629/test_df_for_table.rds"))
write_rds(total_damages_by_pulse_2020_all, paste0(getwd(), "/data/figures/20230629/total_damages_by_pulse_2020.rds"))
write_rds(total_damages_by_pulse_2100_all, paste0(getwd(), "/data/figures/20230629/total_damages_by_pulse_2100.rds"))

# end of script