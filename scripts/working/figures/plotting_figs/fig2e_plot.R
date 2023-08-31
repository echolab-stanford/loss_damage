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
load(paste0(fig_prepped_dta, run_date,"/listof_scc_est_dfs_figs13.RData"))

################################################################################
################################################################################
# prep data

for (i in 1:length(listofdfs)) {
  tic()
  df <- listofdfs[[i]]
  colnames(df)[3] <- "total_damages"
  totals_iqr <- subset(df, total_damages >= (quantile(df$total_damages, c(.25, .75))[1]) &
                         total_damages <= (quantile(df$total_damages, c(.25, .75))[2]))
  
  assign(paste0("totals_iqr_", unique(df$scenario)), totals_iqr)
  
  totals_10_90 <- subset(df, total_damages >= (quantile(df$total_damages, c(.10, .90))[1])  & 
                           total_damages <= (quantile(df$total_damages, c(.10, .90))[2]))
  assign(paste0("totals_10_90_", unique(df$scenario)), totals_10_90)
  
  
  totals_5_95 <- subset(df, total_damages >= (quantile(df$total_damages, c(.05, .95))[1]) & 
                          total_damages <= (quantile(df$total_damages, c(.05, .95))[2]))
  assign(paste0("totals_5_95_", unique(df$scenario)), totals_5_95)
  
  totals_median <- median(df$total_damages)
  assign(paste0("totals_median_", unique(df$scenario)), totals_median)
  
  toc()
}

################################################################################
################################################################################
# plot data
dev.off()

pdf(paste0("figures/", run_date ,"/fig2e.pdf"), width=14, height=5.75)

par(mfrow = c(1,1))
par(mar= c(8,6,2,2))

plot(-1,-1,pch = 15, #xlim = range(0,4), 
     ylim = range(0,20), 
     xlim = range(0,8500),
     cex= 0, yaxt = "n",
     frame.plot = F, col = "#d1def0", ylab = "", 
     xlab = "\n \n SC-CO2 (per tonne damages in $USD)", 
     family = "Helvetica",
     cex.lab = 2, cex.axis = 1.25)

title("e SC-CO2 estimates with total uncertainty under different scenarios", adj = 0)


polygon(x = c(min(totals_5_95_ssp_2100_2dr$total_damages), max(totals_5_95_ssp_2100_2dr$total_damages),
              max(totals_5_95_ssp_2100_2dr$total_damages), min(totals_5_95_ssp_2100_2dr$total_damages)),
        border = "#ffe6e6",      # X-Coordinates of polygon
        y = c(1, 1,2, 2),                             # Y-Coordinates of polygon
        col = "#ffe6e6")  

polygon(x = c(min(totals_10_90_ssp_2100_2dr$total_damages), max(totals_10_90_ssp_2100_2dr$total_damages),
              max(totals_10_90_ssp_2100_2dr$total_damages), min(totals_10_90_ssp_2100_2dr$total_damages)),
        border = "#ffb3b3",      # X-Coordinates of polygon
        y = c(1, 1,2, 2),                             # Y-Coordinates of polygon
        col = "#ffb3b3")  

polygon(x = c(min(totals_iqr_ssp_2100_2dr$total_damages), max(totals_iqr_ssp_2100_2dr$total_damages),
              max(totals_iqr_ssp_2100_2dr$total_damages), min(totals_iqr_ssp_2100_2dr$total_damages)),
        border = "#990000",      # X-Coordinates of polygon
        y = c(1, 1,2, 2),                             # Y-Coordinates of polygon
        col = "#990000")  

polygon(x = c(totals_median_ssp_2100_2dr - 25, totals_median_ssp_2100_2dr + 25,
              totals_median_ssp_2100_2dr + 25, totals_median_ssp_2100_2dr - 25),
        border = "gold1",      # X-Coordinates of polygon
        y = c(1, 1,2, 2),                             # Y-Coordinates of polygon
        col = "gold1")  



polygon(x = c(min(totals_5_95_ssp_2100_ramsey$total_damages), max(totals_5_95_ssp_2100_ramsey$total_damages),
              max(totals_5_95_ssp_2100_ramsey$total_damages), min(totals_5_95_ssp_2100_ramsey$total_damages)),
        border = "#efe8f6",      # X-Coordinates of polygon
        y = c(3.25, 3.25,4.25, 4.25),                             # Y-Coordinates of polygon
        col = "#efe8f6")  

polygon(x = c(min(totals_10_90_ssp_2100_ramsey$total_damages), max(totals_10_90_ssp_2100_ramsey$total_damages),
              max(totals_10_90_ssp_2100_ramsey$total_damages), min(totals_10_90_ssp_2100_ramsey$total_damages)),
        border = "#c0a3db",      # X-Coordinates of polygon
        y = c(3.25, 3.25,4.25, 4.25),                             # Y-Coordinates of polygon
        col = "#c0a3db")  

polygon(x = c(min(totals_iqr_ssp_2100_ramsey$total_damages), max(totals_iqr_ssp_2100_ramsey$total_damages),
              max(totals_iqr_ssp_2100_ramsey$total_damages), min(totals_iqr_ssp_2100_ramsey$total_damages)),
        border = "#581795",      # X-Coordinates of polygon
        y = c(3.25, 3.25,4.25, 4.25),                             # Y-Coordinates of polygon
        col = "#581795")  

polygon(x = c(totals_median_ssp_2100_ramsey - 25, totals_median_ssp_2100_ramsey + 25,
              totals_median_ssp_2100_ramsey + 25, totals_median_ssp_2100_ramsey - 25),
        border = "gold1",      # X-Coordinates of polygon
        y = c(3.25, 3.25,4.25, 4.25),                             # Y-Coordinates of polygon
        col = "gold1")  



polygon(x = c(min(totals_5_95_nogrowth_2dr$total_damages), max(totals_5_95_nogrowth_2dr$total_damages),
              max(totals_5_95_nogrowth_2dr$total_damages), min(totals_5_95_nogrowth_2dr$total_damages)),
        border = "#ffe6e6",      # X-Coordinates of polygon
        y = c(7, 7,8, 8),                             # Y-Coordinates of polygon
        col = "#ffe6e6")  

polygon(x = c(min(totals_10_90_nogrowth_2dr$total_damages), max(totals_10_90_nogrowth_2dr$total_damages),
              max(totals_10_90_nogrowth_2dr$total_damages), min(totals_10_90_nogrowth_2dr$total_damages)),
        border = "#ffb3b3",      # X-Coordinates of polygon
        y = c(7, 7,8, 8),                             # Y-Coordinates of polygon        
        col = "#ffb3b3")  

polygon(x = c(min(totals_iqr_nogrowth_2dr$total_damages), max(totals_iqr_nogrowth_2dr$total_damages),
              max(totals_iqr_nogrowth_2dr$total_damages), min(totals_iqr_nogrowth_2dr$total_damages)),
        border = "#990000",      # X-Coordinates of polygon
        y = c(7, 7,8, 8),                             # Y-Coordinates of polygon        
        col = "#990000")  

polygon(x = c(totals_median_nogrowth_2dr - 25, totals_median_nogrowth_2dr + 25,
              totals_median_nogrowth_2dr + 25, totals_median_nogrowth_2dr - 25),
        border = "gold1",      # X-Coordinates of polygon
        y = c(7, 7,8, 8),                             # Y-Coordinates of polygon        
        col = "gold1")  



polygon(x = c(min(totals_5_95_nogrowth_ramsey$total_damages), max(totals_5_95_nogrowth_ramsey$total_damages),
              max(totals_5_95_nogrowth_ramsey$total_damages), min(totals_5_95_nogrowth_ramsey$total_damages)),
        border = "#efe8f6",      # X-Coordinates of polygon
        y = c(9.25, 9.25,10.25, 10.25),                             # Y-Coordinates of polygon
        col = "#efe8f6")  

polygon(x = c(min(totals_10_90_nogrowth_ramsey$total_damages), max(totals_10_90_nogrowth_ramsey$total_damages),
              max(totals_10_90_nogrowth_ramsey$total_damages), min(totals_10_90_nogrowth_ramsey$total_damages)),
        border = "#c0a3db",      # X-Coordinates of polygon
        y = c(9.25, 9.25,10.25, 10.25),                             # Y-Coordinates of polygon
        col = "#c0a3db")  

polygon(x = c(min(totals_iqr_nogrowth_ramsey$total_damages), max(totals_iqr_nogrowth_ramsey$total_damages),
              max(totals_iqr_nogrowth_ramsey$total_damages), min(totals_iqr_nogrowth_ramsey$total_damages)),
        border = "#581795",      # X-Coordinates of polygon
        y = c(9.25, 9.25,10.25, 10.25),                             # Y-Coordinates of polygon
        col = "#581795")  

polygon(x = c(totals_median_nogrowth_ramsey - 25, totals_median_nogrowth_ramsey + 25,
              totals_median_nogrowth_ramsey + 25, totals_median_nogrowth_ramsey - 25),
        border = "gold1",      # X-Coordinates of polygon
        y = c(9.25, 9.25,10.25, 10.25),                             # Y-Coordinates of polygon      
        col = "gold1")  


polygon(x = c(min(totals_5_95_noimpacts_2dr$total_damages), max(totals_5_95_noimpacts_2dr$total_damages),
              max(totals_5_95_noimpacts_2dr$total_damages), min(totals_5_95_noimpacts_2dr$total_damages)),
        border = "#ffe6e6",      # X-Coordinates of polygon
        y = c(13, 13,14, 14),                             # Y-Coordinates of polygon
        col = "#ffe6e6")  

polygon(x = c(min(totals_10_90_noimpacts_2dr$total_damages), max(totals_10_90_noimpacts_2dr$total_damages),
              max(totals_10_90_noimpacts_2dr$total_damages), min(totals_10_90_noimpacts_2dr$total_damages)),
        border = "#ffb3b3",      # X-Coordinates of polygon
        y = c(13, 13,14, 14),                             # Y-Coordinates of polygon        
        col = "#ffb3b3")  

polygon(x = c(min(totals_iqr_noimpacts_2dr$total_damages), max(totals_iqr_noimpacts_2dr$total_damages),
              max(totals_iqr_noimpacts_2dr$total_damages), min(totals_iqr_noimpacts_2dr$total_damages)),
        border = "#990000",      # X-Coordinates of polygon
        y = c(13, 13,14, 14),                             # Y-Coordinates of polygon        
        col = "#990000")  

polygon(x = c(totals_median_noimpacts_2dr - 25, totals_median_noimpacts_2dr + 25,
              totals_median_noimpacts_2dr + 25, totals_median_noimpacts_2dr - 25),
        border = "gold1",      # X-Coordinates of polygon
        y = c(13, 13,14, 14),                             # Y-Coordinates of polygon        
        col = "gold1")  



polygon(x = c(min(totals_5_95_noimpacts_ramsey$total_damages), max(totals_5_95_noimpacts_ramsey$total_damages),
              max(totals_5_95_noimpacts_ramsey$total_damages), min(totals_5_95_noimpacts_ramsey$total_damages)),
        border = "#efe8f6",      # X-Coordinates of polygon
        y = c(15.25, 15.25,16.25, 16.25),                             # Y-Coordinates of polygon
        col = "#efe8f6")  

polygon(x = c(min(totals_10_90_noimpacts_ramsey$total_damages), max(totals_10_90_noimpacts_ramsey$total_damages),
              max(totals_10_90_noimpacts_ramsey$total_damages), min(totals_10_90_noimpacts_ramsey$total_damages)),
        border = "#c0a3db",      # X-Coordinates of polygon
        y = c(15.25, 15.25,16.25, 16.25),                             # Y-Coordinates of polygon
        col = "#c0a3db")  

polygon(x = c(min(totals_iqr_noimpacts_ramsey$total_damages), max(totals_iqr_noimpacts_ramsey$total_damages),
              max(totals_iqr_noimpacts_ramsey$total_damages), min(totals_iqr_noimpacts_ramsey$total_damages)),
        border = "#581795",      # X-Coordinates of polygon
        y = c(15.25, 15.25,16.25, 16.25),                             # Y-Coordinates of polygon
        col = "#581795")  

polygon(x = c(totals_median_noimpacts_ramsey - 25, totals_median_noimpacts_ramsey + 25,
              totals_median_noimpacts_ramsey + 25, totals_median_noimpacts_ramsey - 25),
        border = "gold1",      # X-Coordinates of polygon
        y = c(15.25, 15.25,16.25, 16.25),                             # Y-Coordinates of polygon
        col = "gold1")  

text(2000, 6, "Impacts through 2300", adj = 0, cex = 1.45, family = "Helvetica")
text(2000, 12, "No growth impacts after 2100", adj = 0, cex = 1.45, family = "Helvetica")
text(2000, 18, "Impacts through 2100", adj = 0, cex = 1.45, family = "Helvetica")



#legend 
polygon(x = c(6600, 7100,
              7100, 6600),
        border = "#581795",      # X-Coordinates of polygon
        y = c(9.5, 9.5,9, 9),                             # Y-Coordinates of polygon
        col = "#581795")  

polygon(x = c(6600, 7100,
              7100, 6600),
        border = "#990000",      # X-Coordinates of polygon
        y = c(8.5, 8.5,8, 8),                             # Y-Coordinates of polygon
        col = "#990000")  

#polygon(x = c(6600, 7100,
#              7100, 6600),
#        border = "gold1",      # X-Coordinates of polygon
#        y = c(7.5, 7.5,7.25, 7.25),                             # Y-Coordinates of polygon
#        col = "gold1")  

polygon(x = c(6400, 7300,
              7300, 6400),
        border = "grey95",      # X-Coordinates of polygon
        y = c(5.75, 5.75,6.25, 6.25),                             # Y-Coordinates of polygon
        col = "grey95")  
polygon(x = c(6600, 7100,
              7100, 6600),
        border = "grey",      # X-Coordinates of polygon
        y = c(5.75, 5.75,6.25, 6.25),                             # Y-Coordinates of polygon
        col = "grey")  

polygon(x = c(6800, 6900,
              6900, 6800),
        border = "black",      # X-Coordinates of polygon
        y = c(5.75, 5.75,6.25, 6.25),                             # Y-Coordinates of polygon
        col = "black")  

polygon(x = c(6840, 6860,
              6860, 6840),
        border = "gold1",      # X-Coordinates of polygon
        y = c(5.75, 5.75,6.25, 6.25),                             # Y-Coordinates of polygon
        col = "gold1")  

segments(x0 = 7300, x1 = 7300, y0 = 5.60, y1 = 1.6)
segments(x0 = 6400, x1 = 7300, y0 = 1.6, y1 = 1.6)
segments(x0 = 6400, x1 = 6400, y0 = 1.6, y1 = 5.6)
segments(x0 = 6850, x1 = 6850, y0 = 1.6, y1 = 1.2)
text(6850, 1, "95% range", cex = 0.95, family = "Helvetica", face = "bold")

segments(x0 = 7100, x1 = 6600, y0 = 2.60, y1 = 2.6)
segments(x0 = 6600, x1 = 6600, y0 = 5.6, y1 = 2.6)
segments(x0 = 7100, x1 = 7100, y0 = 2.6, y1 = 5.6)
segments(x0 = 6850, x1 = 6850, y0 = 2.6, y1 = 2.4)
text(6850, 2.2, "90% range", cex = 0.95, family = "Helvetica", face = "bold")


segments(x0 = 6900, x1 = 6900, y0 = 5.60, y1 = 3.6)
segments(x0 = 6800, x1 = 6900, y0 = 3.6, y1 = 3.6)
segments(x0 = 6800, x1 = 6800, y0 = 3.6, y1 = 5.6)
segments(x0 = 6850, x1 = 6850, y0 = 3.6, y1 = 3.4)
text(6850, 3.2, "IQR", cex = 0.95, family = "Helvetica", face = "bold")

segments(x0 = 6850, x1 = 6850, y0 = 6, y1 = 6.5)
text(6850, 6.9, "median", cex = 0.95, family = "Helvetica", face = "bold")


text(6475,8.5, "Discount rate:", adj = c(1,0), family = "Helvetica", face = "bold", cex = 1.25)

text(7150,9.15, "Ramsey", adj = 0, family = "Helvetica")
text(7150,8.15, "2% fixed", adj = 0, family = "Helvetica")
#text(6475,7.15, "median estimate", adj = c(1,0), family = "Helvetica")

segments(x0 = totals_median_nogrowth_ramsey, 
         x1 = totals_median_nogrowth_ramsey,
         y0 = 10.25, y1 = 10.50, lwd = 1.75)
text(totals_median_nogrowth_ramsey,
     10.85, 
     paste0("$", round(totals_median_nogrowth_ramsey, 0)),
     cex = 1.45,
     lwd = 2.25)

segments(x0 = totals_median_nogrowth_2dr, 
         x1 = totals_median_nogrowth_2dr,
         y0 = 8, y1 = 8.25, lwd = 1.75)
text(totals_median_nogrowth_2dr,
     8.6, 
     paste0("$", round(totals_median_nogrowth_2dr, 0)),
     cex = 1.45,
     lwd = 2.25)

segments(x0 = totals_median_ssp_2100_ramsey, 
         x1 = totals_median_ssp_2100_ramsey,
         y0 = 4.25, y1 = 4.50, lwd = 1.75)
text(totals_median_ssp_2100_ramsey,
     4.85, 
     paste0("$", round(totals_median_ssp_2100_ramsey, 0)),
     cex = 1.45,
     lwd = 2.25)

segments(x0 = totals_median_ssp_2100_2dr, 
         x1 = totals_median_ssp_2100_2dr,
         y0 = 2, y1 = 2.25, lwd = 1.75)
text(totals_median_ssp_2100_2dr,
     2.6, 
     paste0("$", round(totals_median_ssp_2100_2dr, 0)),
     cex = 1.45,
     lwd = 2.25)

segments(x0 = totals_median_noimpacts_ramsey, 
         x1 = totals_median_noimpacts_ramsey,
         y0 = 16.25, y1 = 16.50, lwd = 1.75)
text(totals_median_noimpacts_ramsey,
     16.85, 
     paste0("$", round(totals_median_noimpacts_ramsey, 0)),
     cex = 1.45,
     lwd = 2.25)

segments(x0 = totals_median_noimpacts_2dr, 
         x1 = totals_median_noimpacts_2dr,
         y0 = 14, y1 = 14.25, lwd = 1.75)
text(totals_median_noimpacts_2dr,
     14.6, 
     paste0("$", round(totals_median_noimpacts_2dr, 0)),
     cex = 1.45,
     lwd = 2.25)


#segments(x0 = 6850, x1 = 6850, y0 = 3.6, y1 = 3.4)

dev.off()

# end of script 

