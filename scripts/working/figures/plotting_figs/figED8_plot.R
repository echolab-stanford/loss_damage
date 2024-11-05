##############################################################################
# Mustafa Zahid, August 7th, 2023
# This R script reads the data and prepares the necessary data to plots figure
# ED6. Figure ED6 demonstrates the different steps taken to calculate teh damages
#############################################################################
remove(list=ls())
gc()
sf::sf_use_s2(FALSE)
setwd("~/GitHub/loss_damage")

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
# plot data 
################################################################################ FigED6
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
  gtsave("~/Desktop/figED8_a.png")

gtsave(paste0("~/GitHub/loss_damage/figures/", run_date, "/figED6.png"))
#  gtsave(paste0("/Users/mustafazahid/GitHub/loss_damage/figures/", 
#               run_date,"/fig_compare_est.png"))

#end of script