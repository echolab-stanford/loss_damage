##############################################################################
# Mustafa Zahid, January 7th, 2023
# This R script prepares the data for plotting figure 3a and 3b 
#############################################################################
remove(list=ls())
gc()
sf::sf_use_s2(FALSE)
setwd("~/GitHub/loss_damage")

run_date <- "20230821"
# read in the needed libraries 
source("scripts/working/analysis/0_read_libs.R")

################################################################################
################################################################################
# read data 
scc_est <- readRDS(paste0(fig_prepped_dta, run_date,"/scc_under_diff_scenarios.rds"))
scc_est <- readRDS(paste0(fig_prepped_dta, "20231208","/scc_under_diff_scenarios.rds"))
################################################################################
################################################################################
# plot data 
run_date <- "20231212"
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
  gt_theme_538(table.width = px(950)) %>% 
  cols_align(align = "center") %>% 
  fmt_currency(
    columns = vars(dr1,
                   dr2,
                   dr3,
                   dr_ramsey),
    currency = "USD",
    decimals = F
  ) %>% gtsave(paste0("/figures/", run_date,"/figED8_new_rr.pdf"))

#end of script

