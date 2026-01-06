##############################################################################
# Mustafa Zahid, January 7th, 2023
# This R script prepares the data for plotting figure ED8
#############################################################################
remove(list=ls())
gc()
sf::sf_use_s2(FALSE)
setwd("~/GitHub/loss_damage")

run_date <- "loss_damage_r1"
# read in the needed libraries 
source("scripts/working/analysis/0_read_libs.R")

################################################################################
################################################################################
# read data 
scc_est <- readRDS(paste0(fig_prepped_dta, run_date,"/scc_under_diff_scenarios_20250603.rds"))
################################################################################
################################################################################
# plot data 
scc_est %>%
  tibble%>%
  #group_by() %>% 
  gt(rowname_col = "model") %>% 
  #dplyr::mutate(total_damages_2020_dr2 = paste0("$", total_damages_2020_dr2)) %>% 
  tab_spanner(label = "Discount rate",
              columns = vars(dr1,
                             dr2,
                             dr3,
                             dr_ramsey)) %>%  
  cols_label(dr1 = "Discount rate at 1%",
             dr2 = "Discount rate at 2%",
             dr3 = "Discount rate at 3%",
             dr_ramsey = "Ramsey discount (0.2%,1.24)",
             time_horizon = "Time Horizon",
             post_2100_growth = "Post-2100 Growth",
             regression_model = "Regression Model") %>% 
  gt_theme_538(table.width = px(840)) %>% 
  cols_align(align = "center") %>% 
  fmt_currency(
    columns = vars(dr1,
                   dr2,
                   dr3,
                   dr_ramsey),
    currency = "USD",
    decimals = F
  ) %>% gtsave(paste0(getwd(),"/figures/", run_date,"/figED8.png"), expand = 10)

#end of script

