##############################################################################
# Mustafa Zahid, January 7th, 2023
# This R script prepares the data for plotting figure S6 (carbon capture)
#############################################################################
remove(list=ls())
gc()
sf::sf_use_s2(FALSE)
setwd("~/GitHub/loss_damage")

# read in the needed libraries 
source("scripts/working/analysis/0_read_libs.R")

################################################################################
################################################################################
# read data
fair_exps_cc <- readRDS(paste0(fig_prepped_dta, "20230523/fair_exps_cc_2300.rds"))
total_cc <- readRDS(paste0(fig_prepped_dta, "20230523/total_cc.rds"))

################################################################################
################################################################################
# plot data
# thsi script s to draft the figure that would present the impact of carbon 
# capture on total damages from emissions 

# the idea is to plot a figure where you show the pulse and the capture in one
# figire 
x <- 2019:2100
df <- as.data.frame(x)
df$y <- 0 
df$y[df$x == 2020] <- 1
df$y[df$x == 2030] <- -1
par(mfrow = c(2, 1))

#dev.off()
pdf(paste0("figures/20230629/figS6.pdf"), width=6, height=9)
#gsub("-", "", Sys.Date()) 
# 1 column figure with 3 plots using relative heights
layout(matrix(1:4, ncol=1), heights=c(0.38, 0.38, 0.5, 0.8))
par(oma=c(2, 3, 1, 3), mar=c(4,5,4,5))

################################################################################ panel a
plot(df$x, df$y, type = "l", xlim = range(2019:2100),
     ylab = "",
     xlab = " ",
     frame = F,
     yaxt = "n",
     xaxt = "n",
     cex.lab = 2)
mtext(side=2, text="Net Emissions \n(GtCO2)", line=5, cex = 1)

## Draw the x-axis with no labels.
## Draw the y-axis.
axis(side = 2,
     ## Rotate the labels.
     las = 2,
     labels = c(-1,0,1), at = c("-1", "0", "1"),
     cex.axis = 1.75)

segments(y0 = -2, y1 = 2, 
         x0 = 2030, x1 = 2030, lty = 3, lwd = 1.35)
segments(y0 = 1, y1 = 1, 
         x0 = 2022, x1 = 2036, lty = 1)
text(2037, 0.95, adj = 0, "1 GtCO2 emitted", cex = 1.25)
segments(y0 = -1, y1 = -1, 
         x0 = 2032, x1 = 2046, lty = 1)
text(2047, -0.95, adj = 0, "1 GtCO2 captured", cex = 1.25)

################################################################################ panel b
# now let us bring tempdelta 
plot(fair_exps_cc$year,
     fair_exps_cc$median_deltat*(-1),
     type = "l", ylab = "", xlab = " ",
     xaxt = "n",
     frame = F,
     yaxt = "n",
     xlim = range(2019:2100),
     cex.lab = 2)

mtext(side=2, text=expression("FaIR "*Delta*"T (°C)"), line=6, cex = 1)
segments(x0 = 2019, x1 = 2100, y0 = 0, y1 = 0, lty = 2, lwd = 1.25)

## Draw the y-axis.
axis(side = 2,
     ## Rotate the labels.
     las = 2,
     at = c(0, 0.0004),
     labels = c("0", expression("1x10"^-4)),
     cex.axis = 1.75)

segments(y0 = -1, y1 = 0.005, 
         x0 = 2030, x1 = 2030, lty = 3, lwd = 1.35)

################################################################################ panel c
plot(total_cc$emitter, total_cc$total_damages/1000000000, type = "l",
     frame = F, 
     xlab = "Year",
     ylab = "", xlim = range(2019:2100),
     yaxt = "n",
     cex.axis = 1.75,
     cex.lab = 2)
mtext(side=2, text="Damage/1tCO2 ($)", line=6, cex = 1.35)

## Draw the x-axis with no labels.
axis(side = 2, labels = FALSE)

## Draw the y-axis.
axis(side = 2,
     ## Rotate the labels.
     las = 2,
     cex.axis = 1.75)

segments(y0 = 0, y1 = 10000, 
         x0 = 2030, x1 = 2030, 
         lty = 3, lwd = 1.35)

segments(y0 = total_cc$total_damages[total_cc$emitter == 2030]/1000000000, 
         y1 = total_cc$total_damages[total_cc$emitter == 2030]/1000000000, 
         x0 = 2032, x1 = 2040, lty = 1.45)

text(2041, total_cc$total_damages[total_cc$emitter == 2030]/1000000000, 
     adj = 0, paste0("Total /tC02 damage if capturing \n 2020 emissions in 2030"),
     cex = 1.25)

points(2030, total_cc$total_damages[total_cc$emitter == 2030]/1000000000, pch = 19, 
       bg = "red", col = "black", lwd = 5.75)
points(2030, total_cc$total_damages[total_cc$emitter == 2030]/1000000000, pch = 19, 
       col = "red", lwd = 3.75)

##### let us add another panel (07/13)
total_cc$pct_averted <- (total_cc$total_damages[total_cc$emitter == 2100] - total_cc$total_damages)/total_cc$total_damages[total_cc$emitter == 2100]

plot(total_cc$emitter, total_cc$pct_averted*100, type = "l",
     frame = F, 
     xlab = "Year",
     ylab = "", xlim = range(2019:2100),
     yaxt = "n",
     cex.axis = 1.75,
     cex.lab = 2)


## Draw the x-axis with no labels.
axis(side = 2, labels = FALSE)

## Draw the y-axis.
axis(side = 2,
     ## Rotate the labels.
     las = 2,
     cex.axis = 1.75)

segments(y0 = 0, y1 = 10000, 
         x0 = 2030, x1 = 2030, 
         lty = 3, lwd = 1.35)

mtext(side=2, text="% of Damages Averted", line=6, cex = 1.35)


segments(y0 = total_cc$pct_averted[total_cc$emitter == 2030]*100, 
         y1 = total_cc$pct_averted[total_cc$emitter == 2030]*100, 
         x0 = 2032, x1 = 2040, lty = 1.45)


text(2041, total_cc$pct_averted[total_cc$emitter == 2030]*100, 
     adj = 0, paste0("% of total damages averted"),
     cex = 1.25)

points(2030, total_cc$pct_averted[total_cc$emitter == 2030]*100, pch = 19, 
       bg = "red", col = "black", lwd = 5.75)
points(2030, total_cc$pct_averted[total_cc$emitter == 2030]*100, pch = 19, 
       col = "red", lwd = 3.75)

dev.off()
# end of script 