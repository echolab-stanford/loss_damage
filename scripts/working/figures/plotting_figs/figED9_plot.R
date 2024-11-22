##############################################################################
# Mustafa Zahid, August 7th, 2023
# This R script reads the data and prepares the necessary data to plots figure
# ED6. Figure ED6 demonstrates the different steps taken to calculate teh damages
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
# read the data 
ex <- readRDS(paste0(getwd(), "/data/figures/", run_date, "/damages_under_diff_marginals.rds"))
figed8b <- readRDS(paste0(getwd(), "/data/figures/", run_date, "/damages_under_diff_baseline_scenarios.rds"))

################################################################################
################################################################################
# plot data 
################################################################################ FigED8a
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
  gtsave(paste0("~/GitHub/loss_damage/figures/", run_date, "/figED8.png"))
#  gtsave(paste0("/Users/mustafazahid/GitHub/loss_damage/figures/", 
#               run_date,"/fig_compare_est.png"))

################################################################################ FigED8b
# ok now let us plot 

egy <- MetBrewer::met.brewer("Egypt")[2]
figed8b$scenario_id <- 1:5
pdf(file = paste0("~/GitHub/loss_damage/figures/",run_date,"/figED8b.pdf"),   # The directory you want to save the file in
    width = 7.85, # The width of the plot in inches
    height = 4.85) # The height of the plot in inches


par(mar= c(6,8,2,2))

plot(figed8b$scenario_id, figed8b$value, type = "l", xaxt = "n", 
     frame.plot = F, las = 1, 
     ylab = "USA damages\nrelative to baseline scenario", 
     xlab = "Baseline emissions for 1990-2020 period", cex.axis = 1.15, cex.lab = 1.25)
points(figed8b$scenario_id, figed8b$value, col = egy, pch = 19, cex = 2.25)
points(figed8b$scenario_id, figed8b$value, col = "black", pch = 21, cex = 2.3, lwd = 1.55)
axis(1, figed8b$scenario, at = figed8b$scenario_id, cex.axis = 1.15)

dev.off()



#end of script