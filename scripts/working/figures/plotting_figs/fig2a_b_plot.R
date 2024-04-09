##############################################################################
# Mustafa Zahid, January 7th, 2023
# This R script prepares the data for plotting figure 3a and 3b 
#############################################################################
remove(list=ls())
gc()
sf::sf_use_s2(FALSE)
setwd("~/GitHub/loss_damage")

run_date <- "20240314"
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
              columns = vars(total_damages_2020_dr1_5,
                             total_damages_2020_dr2,
                             total_damages_2020_dr3,
                             total_damages_2020_dr5,
                             total_damages_2020_dr7)) %>% 
  tab_spanner(label = "Damages Accumulated 2021-2100 (in Billions of $USD)",
              columns = vars(total_damages_2100_dr1_5,
                             total_damages_2100_dr2,
                             total_damages_2100_dr3,
                             total_damages_2100_dr5,
                             total_damages_2100_dr7)) %>% 
  cols_label(total_damages_2020_dr1_5 = "1.5%",
             total_damages_2020_dr2 = "2%",
             total_damages_2020_dr3 = "3%",
             total_damages_2020_dr5 = "5%",
             total_damages_2020_dr7 = "7%",
             total_damages_2100_dr1_5 = "1.5%",
             total_damages_2100_dr2 = "2%",
             total_damages_2100_dr3 = "3%",
             total_damages_2100_dr5 = "5%",
             total_damages_2100_dr7 = "7%") %>% 
  gt_theme_538(table.width = px(550)) %>% 
  gtsave(paste0("figures/", run_date,"/figED4.png"))


################################################################################ Figure 3b
pdf(paste0("figures/", run_date ,"/fig2a_b.pdf"), width=15, height=6)
par(mfrow = (c(1,2)))
par(mar = c(4,8,4,4))
#par(family = "Helvetica")

plot(x = total_damages_by_pulse_2020_all$emitter[total_damages_by_pulse_2020_all$discount_rate == "7%"], 
     y = total_damages_by_pulse_2020_all$total_damages[total_damages_by_pulse_2020_all$discount_rate == "7%"],
     # log = "y",
     #yaxt = "n",
     ylim = range(c(0,350)),
     col = "#023b70",
     pch = 3, xlim = range(c(1990, 2020)),
     xlab = "Year of CO2 Pulse",  ylab = "Per tonne damages in 2020 $USD\n",
     las = 1, lwd = 2, cex.axis = 1.25, cex.lab = 1.5, 
     cex.main = 1.12,
     frame.plot = F)
     #main = "a)  Accumulated damages through 2020") 

title("a)  Accumulated damages through 2020", adj = 0)

par(f = 2)

#axis(side = 2, at = c(0, 100, 200, 300, 400), labels =c("0", "5", '10', '15', '20'))

segments(x0 = 2010, x1 = 2012, y0 = 285, y1 = 285, col = "#8a5cb4", lwd = 3)
segments(x0 = 2010, x1 = 2012, y0 = 265, y1 = 265, col = "#de3623", lwd = 3)
segments(x0 = 2010, x1 = 2012, y0 = 245, y1 = 245, col = "#f0da32", lwd = 3)
segments(x0 = 2010, x1 = 2012, y0 = 225, y1 = 225, col = "#2aa83f", lwd = 3)
segments(x0 = 2010, x1 = 2012, y0 = 205, y1 = 205, col = "#023b70", lwd = 3)

text(2014, 285, " 1.5%")
text(2014, 265, " 2%")
text(2014, 245, " 3%")
text(2014, 225, " 5%")
text(2014, 205, " 7%")
text(2009, 298, "Discount rates:", col = "black", cex = 1.2, adj = 0)


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

points(x = total_damages_by_pulse_2020_all$emitter[total_damages_by_pulse_2020_all$discount_rate == "1.5%"], 
       y = total_damages_by_pulse_2020_all$total_damages[total_damages_by_pulse_2020_all$discount_rate == "1.5%"],
       pch = 3, col = "#8a5cb4",
       lwd = 2)


#total_damages_by_pulse_2100_all$emitter <- 2100 - total_damages_by_pulse_2100_all$emitter


plot(x = total_damages_by_pulse_2100_all$emitter[total_damages_by_pulse_2100_all$discount_rate == "7%"], 
     y = total_damages_by_pulse_2100_all$total_damages[total_damages_by_pulse_2100_all$discount_rate == "7%"],
     # log = "y",
     ylim = range(c(0,2500)),
     col = "#023b70",
     pch = 3, xlim = range(c(1990, 2021)),
     xlab = "Year of CO2 Pulse",  ylab = "Per tonne damages in 2020 $USD\n",
     las = 1, lwd = 2, cex.axis = 1.25, cex.lab = 1.5, 
     cex.main = 1.12,
     #main = "b)  Accumulated damages 2021-2100",
     frame.plot = F) 

title("b)  Accumulated damages 2021-2100", adj = 0)

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
points(x = total_damages_by_pulse_2100_all$emitter[total_damages_by_pulse_2100_all$discount_rate == "1.5%"], 
       y = total_damages_by_pulse_2100_all$total_damages[total_damages_by_pulse_2100_all$discount_rate == "1.5%"],
       pch = 3, col = "#8a5cb4",
       lwd = 2)


segments(x0 = 2020, 
         x1 = 2020, 
         y0 = 0, 
         y1 = 2000, 
         lty = 2)

text("SC-CO2", x = 2020, y = 2100)

dev.off()

# end of script 
