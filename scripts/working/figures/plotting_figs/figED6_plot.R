##############################################################################
# Mustafa Zahid, March 13th, 2024
# This R script reads the data and plot the figure ED6. 
#############################################################################
remove(list=ls())
gc()
sf::sf_use_s2(FALSE)
setwd("~/GitHub/loss_damage")

run_date <- "20241104"

#############################################################################
#############################################################################
# read the data first
data_1990_2020_moments <- readRDS(paste0(getwd(),"/data/figures/", run_date, "/data_1990_2020_moments.rds"))
data_2021_2100_moments <- readRDS(paste0(getwd(),"/data/figures/", run_date, "/data_2021_2100_moments.rds"))

#############################################################################
#############################################################################
# now plot the data 

# ok now let us plot 
pdf(file = paste0(getwd(), "/figures/", run_date, "/figED6.pdf"),   # The directory you want to save the file in
    width = 12.85, # The width of the plot in inches
    height = 5.85) # The height of the plot in inches
par(mfrow = c(1,2))
par(mar = c(5, 5, 3, 3))

##################
plot(-1,-1,pch = 15, #xlim = range(0,4), 
     xlim = range(1990:2020),
     ylim = range(0,650),
     cex= 0,
     frame.plot = F, col = "#d1def0", 
     xlab = "Year of CO2 Pulse", 
     family = "Helvetica",
     cex.lab = 1.25, cex.axis = 1.25, 
     ylab = "Per tonne damages in 2020 $USD")
mtext("a) Cumulative damages through 2020 (HD-CO2)", cex = 1.25)

# Add a polygon between p_25 and p_75
polygon(x = c(data_1990_2020_moments$emitter, rev(data_1990_2020_moments$emitter)), 
        y = c(data_1990_2020_moments$p_95, rev(data_1990_2020_moments$p_05)), col = "lightgrey", border = NA)

# Add a polygon between p_25 and p_75
polygon(x = c(data_1990_2020_moments$emitter, rev(data_1990_2020_moments$emitter)), 
        y = c(data_1990_2020_moments$p_90, rev(data_1990_2020_moments$p_10)), col = "darkgrey", border = NA)

# Add a polygon between p_25 and p_75
polygon(x = c(data_1990_2020_moments$emitter, rev(data_1990_2020_moments$emitter)), 
        y = c(data_1990_2020_moments$p_75, rev(data_1990_2020_moments$p_25)), col = "black", border = NA)

lines(data_1990_2020_moments$emitter,
      data_1990_2020_moments$median, 
      col = "gold", lwd = 1.35)


segments(x0 = 2006,
         x1 = 2006,
         y0 = 580, 
         y1 = 340)
segments(x0 = 2018,
         x1 = 2018,
         y0 = 580, 
         y1 = 340)
segments(x0 = 2006,
         x1 = 2018,
         y0 = 340, 
         y1 = 340)
segments(x0 = 2012,
         x1 = 2012,
         y0 = 340, 
         y1 = 300)
text(2012, 280, "95% range")

segments(x0 = 2009,
         x1 = 2009,
         y0 = 580, 
         y1 = 430)
segments(x0 = 2015,
         x1 = 2015,
         y0 = 580, 
         y1 = 430)
segments(x0 = 2009,
         x1 = 2015,
         y0 = 430, 
         y1 = 430)
segments(x0 = 2012,
         x1 = 2012,
         y0 = 430, 
         y1 = 390)
text(2012, 370, "90% range")

segments(x0 = 2011,
         x1 = 2011,
         y0 = 580, 
         y1 = 500)
segments(x0 = 2013,
         x1 = 2013,
         y0 = 580, 
         y1 = 500)
segments(x0 = 2011,
         x1 = 2013,
         y0 = 500, 
         y1 = 500)
segments(x0 = 2012,
         x1 = 2012,
         y0 = 500,
         y1 = 470)
text(2012, 450, "IQR")

segments(x0 = 2012,
         x1 = 2012,
         y0 = 580,
         y1 = 610)
text(2012, 630, "Median")

polygon(x = c(2006, 2018,
              2018, 2006),
        border = "lightgrey",      # X-Coordinates of polygon
        y = c(580, 580,530, 530),                             # Y-Coordinates of polygon
        col = "lightgrey")  
polygon(x = c(2009, 2015,
              2015, 2009),
        border = "darkgrey",      # X-Coordinates of polygon
        y = c(580, 580,530, 530),                             # Y-Coordinates of polygon                         # Y-Coordinates of polygon
        col = "darkgrey")  
polygon(x = c(2011, 2013,
              2013, 2011),
        border = "black",      # X-Coordinates of polygon
        y = c(580, 580,530, 530),                             # Y-Coordinates of polygon
        col = "black")  

polygon(x = c(2011.965, 2012.035,
              2012.035, 2011.965),
        border = "gold",      # X-Coordinates of polygon
        y = c(580, 580,530, 530),                             # Y-Coordinates of polygon
        col = "gold")  





####################

plot(-1,-1,pch = 15, #xlim = range(0,4), 
     xlim = range(1990:2020),
     ylim = range(0,6000),
     cex= 0,
     frame.plot = F, col = "#d1def0", 
     xlab = "Year of CO2 Pulse", 
     family = "Helvetica",
     cex.lab = 1.25, cex.axis = 1.25, 
     ylab = "Per tonne damages in 2020 $USD")

mtext("b Cumulative damages 2021-2100 (FD-CO2)", cex = 1.25)


# Add a polygon between p_25 and p_75
polygon(x = c(data_2021_2100_moments$emitter, rev(data_2021_2100_moments$emitter)), 
        y = c(data_2021_2100_moments$p_95, rev(data_2021_2100_moments$p_05)), col = "lightgrey", border = NA)

# Add a polygon between p_25 and p_75
polygon(x = c(data_2021_2100_moments$emitter, rev(data_2021_2100_moments$emitter)), 
        y = c(data_2021_2100_moments$p_90, rev(data_2021_2100_moments$p_10)), col = "darkgrey", border = NA)

# Add a polygon between p_25 and p_75
polygon(x = c(data_2021_2100_moments$emitter, rev(data_2021_2100_moments$emitter)), 
        y = c(data_2021_2100_moments$p_75, rev(data_2021_2100_moments$p_25)), col = "black", border = NA)

lines(data_2021_2100_moments$emitter,
      data_2021_2100_moments$median, 
      col = "gold", lwd = 1.35)

segments(x0 = 2020, 
         x1 = 2020,
         y0 = 0,
         y1 = 5000, 
         lty = "dashed")
text(2020, 5200, 
     "SC-CO2", adj = 0.75)

dev.off()
