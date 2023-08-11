##############################################################################
# Mustafa Zahid, January 7th, 2023
# This R script prepares the data for plotting figure 3a and 3b 
#############################################################################
remove(list=ls())
gc()
sf::sf_use_s2(FALSE)
setwd("~/GitHub/loss_damage")

run_date <- "20230713"
# read in the needed libraries 
source("scripts/working/analysis/0_read_libs.R")

################################################################################
################################################################################
# read data 
test_df_for_table <- readRDS(paste0(fig_prepped_dta, run_date, "/test_df_for_table.rds"))
total_damages_by_pulse_2020_all <- readRDS(paste0(fig_prepped_dta, run_date, "/total_damages_by_pulse_2020.rds"))
total_damages_by_pulse_2100_all <- readRDS(paste0(fig_prepped_dta, run_date, "/total_damages_by_pulse_2100.rds"))

################################################################################
################################################################################
# plot data 
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
#tab_header(
#  title = md("Total Damages from 1GtCO2 Pulse in a Given Year"),
#  subtitle = md("By Year of Pulse (1990-2020) & Discount Rate (2%, 3%, 5%, 7%) ")
#) %>%
  gtsave(paste0("figures/", run_date,"/figS3.png"))


################################################################################ Figure 3b
pdf(paste0("figures/", run_date ,"/fig3a_b.pdf"), width=15, height=6)
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
     xlab = "Year of 1GtCO2 Pulse",  ylab = "Per tonne damages in 2020 $USD\n",
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
     xlab = "Year of 1GtCO2 Pulse",  ylab = "Per tonne damages in 2020 $USD\n",
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
