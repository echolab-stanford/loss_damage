##############################################################################
# Mustafa Zahid, January 7th, 2023
# This R script visualizes figure 2. The figure has 6 panels. 
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
world_emissions <- readRDS(paste0(fig_prepped_dta, run_date, "/world_emissions.rds"))
world_minus_us_emms <- readRDS(paste0(fig_prepped_dta, run_date,"/world_minus_us_emms.rds"))
gtc1_fair_exp <- readRDS(paste0(fig_prepped_dta, run_date,"/gtc1_fair_exp.rds"))
y <- rast(paste0(fig_prepped_dta, run_date,"/y.tiff"))
v <- vect(paste0(fig_prepped_dta, run_date, "/v.shp"))
annual_observed <- readRDS(paste0(fig_prepped_dta, run_date,"/annual_observed.rds"))
usa_bra <- readRDS(paste0(fig_prepped_dta, run_date,"/usa_bra.rds"))
sum_usa_bra <- readRDS(paste0(fig_prepped_dta, run_date,"/sum_usa_bra.rds"))
dev.off()
################################################################################
################################################################################
# visualize data   
pdf(file = paste0(getwd(), "/figures/", run_date, "/fig2_pre_illustrator.pdf"),   # The directory you want to save the file in
    width = 15.85, # The width of the plot in inches
    height = 8.85) # The height of the plot in inches

mat <- matrix(c(2,1,2,1),nrow=2, ncol = 3, byrow=T)
layout(mat)
ax = 1.5  #scaling for axes
par(mar=c(4,4,2,1))
par(mfrow=c(2,3))


#1a
par(mar = c(4,10,4,2))  
plot(x = world_emissions$year, y = world_emissions$total,
     type = "l", lty = 1, xlim = range(c(1900, 2020)),
     xlab = "Year", ylab = "CO2 Emissions (GtCO2)\n",
     las = 1, lwd = 2, cex.axis = 1.85, cex.lab = 2, 
     frame.plot = F) 

title("a", adj = 0, line =1, cex.main = 2.5)
# las = 1 says "turn the y-axis tick labels to be horizontal"
# 3) add info for the other fishery
lines(x = world_minus_us_emms$year, y = world_minus_us_emms$total, 
      type = "l", lty = 2, lwd = 2, pch = 16, col = "red")
# 4) add the legend
segments(1975, 2.2, 1980, 2.2, lwd = 2)
segments(1975, 1, 1980, 1, col = "red", lty = 2, lwd = 2)

text(1981,2.2, " Full Emissions", cex = 1.25, adj = 0)
text(1981,1, " Full Emissions minus \n US Emissions 1990\nOnward", cex = 1.25, 
     adj = 0)

# legend("bottomleft", legend = c("Full Emissions",
#                  "Full Emissions w/out USA \nemissions 1990 onward"), 
#      lty = c(1,2), lwd = c(2,2), bty = "n", 
#     col = c("black", "red"), cex = 0.85)

#write_rds(world_emissions, "~/Desktop/world_emissions.rds")
#world_emissions <- read_rds("~/Desktop/world_emissions.rds")
#write_rds(world_minus_us_emms, "~/Desktop/world_minus_us_emms.rds")
#world_minus_us_emms <- read_rds("~/Desktop/world_minus_us_emms.rds")



################################################################################
################################################################################
# 1b
tail(gtc1_fair_exp)
#gtc1_fair_exp <- subset(gtc1_fair_exp, year <= 2100)
par(mar = c(4,10,4,2))  
plot(x = gtc1_fair_exp[,2], y = gtc1_fair_exp[,4],
     type = "l", lty = 1, xlim = range(c(1990,2100)),
     ylim = range(0, 0.3),
     xlab = "Year", ylab = expression(atop(""*Delta*"GMST (C°)")),
     las = 1, lwd = 2, cex.lab= 2, cex.axis = 1.85, 
     frame.plot = F)  

polygon(c(gtc1_fair_exp[,2],rev(gtc1_fair_exp[,2])),c(gtc1_fair_exp[,3],rev(gtc1_fair_exp[,5])),
        col="grey", border = NA)

lines(x = gtc1_fair_exp[,2], y = gtc1_fair_exp[,4],
      type = "l", lty = 1, xlim = range(c(1990,2300)),
      ylim = range(0, 0.004),
      xlab = "Year", ylab = "Temperature (c°)",
      las = 1, lwd = 2, cex.lab = 2.5, cex.axis = 1.85)  # las = 1 says "turn the y-axis tick labels to be horizontal"

title("b", adj = 0, line =1, cex.main = 2.5)


################################################################################
################################################################################
# 1c

par(mar = c(4,4,10,2))  
#dev.off()
plot((y), col= rev(heat.colors(n = 20)), legend = T, 
     plg=list(shrink=0.85, cex=.8, x = "bottomleft", title = "\nΔT(C°)"), 
     axes = FALSE, boxes = T, mar=c(5, 2, 2, 4))

lines(v, lwd =0.8)

title("\n c", adj = 0, line =1, cex.main = 2.5)




################################################################################
################################################################################
# 1d



par(mar = c(4,10,4,2))  
plot(x = annual_observed$year, y = annual_observed$observed,
     type = "l", lty = 1, xlim = range(c(1990, 2020)),
     ylim = range(c(22,23.5)),
     xlab = "Year", ylab = "Temperature (C°) \n",
     las = 1, lwd = 2, cex.lab = 2, cex.axis = 1.85, 
     frame.plot = F)  
# las = 1 says "turn the y-axis tick labels to be horizontal"
# 3) add info for the other fishery
lines(x = annual_observed$year, y = annual_observed$corrected, 
      type = "l", lty = 1, lwd = 2, cex.axis = 1.85, cex.lab = 2, 
      pch = 16, col = "red")

segments(2000, 22.15, 2002, 22.15, lwd = 2)
segments(2000, 22.05, 2002, 22.05, col = "red", lty = 1, lwd = 2)

text(2003,22.15, "Observed temperature (C°)", cex = 1.25, adj = 0)
text(2003,22.05, "Observed temperature \nminus ΔT", cex = 1.25, adj = 0)

title("d", adj = 0, line =1, cex.main = 2.5)

################################################################################
################################################################################
# 1e

par(mar = c(4,10,4,2))  
plot(x = usa_bra$year, y = usa_bra$adjusted_gdp1,
     type = "l", lty = 1, xlim = range(c(1990, 2020)),
     xlab = "Year", ylab = "Real GDP (in Bil of $USD) \n",
     las = 1, lwd = 2, #family = "Arial", 
     cex.lab = 1.85, cex.axis = 1.85, 
     frame.plot = F, col = "red") 

# 3) add info for the other fishery
lines(x = usa_bra$year, y = usa_bra$gdp1, 
      type = "l", lty = 1, lwd = 2, pch = 16, col = "black")


segments(2000, 900, 2002, 900, lwd = 2, col = "black")
segments(2000, 800, 2002, 800, col = "red", lty = 1, lwd = 2)

text(2003,900, " Observed GDP", cex = 1.25, adj = 0)
text(2003,800, " GDP absent US Emissions \n1990 onward", cex = 1.25, adj = 0)

title("e", adj = 0, line =1, cex.main = 2.5)

################################################################################
################################################################################
#1f

par(mar = c(4,10,4,2))  
plot(x = sum_usa_bra$year, y = sum_usa_bra$cumsum2,
     type = "l", lty = 1, xlim = range(c(1990, 2020)),
     ylim = range(0,250),
     xlab = "Year", ylab =  "Cumulative Damages \n(in Billions of $USD)\n",
     las = 1, lwd = 2, cex.lab = 2, cex.axis = 1.85, 
     frame.plot = F) 
title("f", adj = 0, line = 1, cex.main = 2.5)


# end of script 

dev.off()