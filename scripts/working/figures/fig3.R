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

#test_df_for_table_2020$total_damages_2020_dr2 <- 

#### now bring them together 
setwd("~/GitHub/loss_damage")


test_df_for_table <- left_join(test_df_for_table_2020,
                               test_df_for_table_2100,
                               by = c("emitter"))

################################################################################ Figure 3a
test_df_for_table %>%
  tibble%>%
  #group_by(emitter) %>% 
  gt(rowname_col = "emitter") %>% 
  #dplyr::mutate(total_damages_2020_dr2 = paste0("$", total_damages_2020_dr2)) %>% 
  tab_spanner(label = "Damages Accumulated Through 2020 (in Billions of $USD)",
              columns = vars(total_damages_2020_dr2,
                             total_damages_2020_dr3,
                             total_damages_2020_dr5,
                             total_damages_2020_dr7)) %>% 
  tab_spanner(label = "Damages Accumulated 2021-2100 (in Billions of $USD)",
              columns = vars(total_damages_2100_dr2,
                             total_damages_2100_dr3,
                             total_damages_2100_dr5,
                             total_damages_2100_dr7)) %>% 
  cols_label(total_damages_2020_dr2 = "2%",
             total_damages_2020_dr3 = "3%",
             total_damages_2020_dr5 = "5%",
             total_damages_2020_dr7 = "7%",
             total_damages_2100_dr2 = "2%",
             total_damages_2100_dr3 = "3%",
             total_damages_2100_dr5 = "5%",
             total_damages_2100_dr7 = "7%") %>% 
  gt_theme_538(table.width = px(550)) %>% 
  #tab_style(style = list(cell_text(font = "Times",
  #                                align = "center",
  #                               weight = "bold")),
  #       cell_borders(sides = "bottom", weight = px(3))),
  #) %>% 
  #  tab_style(
  #   locations = cells_column_labels(columns = everything()),
  #  style     = list(
  #   #Give a thick border below
  #  cell_borders(sides = "bottom", weight = px(3)),
  # #Make text bold
#cell_text(weight = "bold")
#  )) %>% 
tab_header(
  title = md("Total Damages from 1GtCO2 Pulse in a Given Year"),
  subtitle = md("By Year of Pulse (1990-2020) & Discount Rate (2%, 3%, 5%, 7%) ")
) %>%
  gtsave(paste0("figures/", gsub("-", "", Sys.Date()),"/accum_damages_1gtco2_yr_net.png"))


################################################################################ Figure 3b
png(paste0("figures/", gsub("-", "", Sys.Date()) ,"/accum_damages_1gtco2_yr_plt.png"), width=600, height=425)
par(mfrow = (c(1,2)))
par(mar = c(4,8,4,4))
#par(family = "Helvetica")

plot(x = total_damages_by_pulse_2020_all$emitter[total_damages_by_pulse_2020_all$discount_rate == "7%"], 
     y = total_damages_by_pulse_2020_all$total_damages[total_damages_by_pulse_2020_all$discount_rate == "7%"],
     # log = "y",
     #yaxt = "n",
     ylim = range(c(0,10)),
     col = "#023b70",
     pch = 3, xlim = range(c(1990, 2020)),
     xlab = "Year of 1GtCO2 Pulse",  ylab = "Total damages in billions of $USD\n",
     las = 1, lwd = 2, cex.axis = 1.25, cex.lab = 1.5, 
     cex.main = 1.12,
     frame.plot = F,
     main = "a)  Accumulated damages through 2020") 

#axis(side = 2, at = c(0, 100, 200, 300, 400), labels =c("0", "5", '10', '15', '20'))

segments(x0 = 2002, x1 = 2004, y0 = 9.6, y1 = 9.6, col = "#de3623", lwd = 3)
segments(x0 = 2002, x1 = 2004, y0 = 9.2, y1 = 9.2, col = "#f0da32", lwd = 3)
segments(x0 = 2002, x1 = 2004, y0 = 8.8, y1 = 8.8, col = "#2aa83f", lwd = 3)
segments(x0 = 2002, x1 = 2004, y0 = 8.4, y1 = 8.4, col = "#023b70", lwd = 3)

text(2006, 9.6, " 2%")
text(2006, 9.2, " 3%")
text(2006, 8.8, " 5%")
text(2006, 8.4, " 7%")
text(2002, 10, "Discount rates:", col = "black", cex = 1.2, adj = 0)


points(x = total_damages_by_pulse_2020_all$emitter[total_damages_by_pulse_2020_all$discount_rate == "5%"], 
       y = total_damages_by_pulse_2020_all$total_damages[total_damages_by_pulse_2020_all$discount_rate == "5%"],
       pch = 3, col = "#2aa83f",
       lwd = 2)

points(x = total_damages_by_pulse_2020_all$emitter[total_damages_by_pulse_2020_all$discount_rate == "3%"], 
       y = total_damages_by_pulse_2020_all$total_damages[total_damages_by_pulse_2020_all$discount_rate == "3%"],
       pch = 3, col = "#f0da32",
       lwd = 2)

points(x = total_damages_by_pulse_2020_all$emitter[total_damages_by_pulse_2020_all$discount_rate == "2%"], 
       y = total_damages_by_pulse_2020_all$total_damages[total_damages_by_pulse_2020_all$discount_rate == "2%"],
       pch = 3, col = "#de3623",
       lwd = 2)


#total_damages_by_pulse_2100_all$emitter <- 2100 - total_damages_by_pulse_2100_all$emitter


plot(x = total_damages_by_pulse_2100_all$emitter[total_damages_by_pulse_2100_all$discount_rate == "7%"], 
     y = total_damages_by_pulse_2100_all$total_damages[total_damages_by_pulse_2100_all$discount_rate == "7%"],
     # log = "y",
     ylim = range(c(0,450)),
     col = "#023b70",
     pch = 3, xlim = range(c(1990, 2020)),
     xlab = "Year of 1GtCO2 Pulse",  ylab = "Total damages in billions of $USD\n",
     las = 1, lwd = 2, cex.axis = 1.25, cex.lab = 1.5, 
     cex.main = 1.12,
     main = "b)  Accumulated damages 2021-2100",
     frame.plot = F) 

points(x = total_damages_by_pulse_2100_all$emitter[total_damages_by_pulse_2100_all$discount_rate == "5%"], 
       y = total_damages_by_pulse_2100_all$total_damages[total_damages_by_pulse_2100_all$discount_rate == "5%"],
       pch = 3, col = "#2aa83f",
       lwd = 2)


points(x = total_damages_by_pulse_2100_all$emitter[total_damages_by_pulse_2100_all$discount_rate == "3%"], 
       y = total_damages_by_pulse_2100_all$total_damages[total_damages_by_pulse_2100_all$discount_rate == "3%"],
       pch = 3, col = "#f0da32",
       lwd = 2)

points(x = total_damages_by_pulse_2100_all$emitter[total_damages_by_pulse_2100_all$discount_rate == "2%"], 
       y = total_damages_by_pulse_2100_all$total_damages[total_damages_by_pulse_2100_all$discount_rate == "2%"],
       pch = 3, col = "#de3623",
       lwd = 2)
dev.off()



# end of script