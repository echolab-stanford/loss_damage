##############################################################################
# Mustafa Zahid, January 7th, 2023
# This R script prepares the data for plotting figure 3a and 3b 
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
load(paste0(fig_prepped_dta, "20230629/listof_scc_est_dfs_figs13.RData"))

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
  
  totals_median <-median(df$total_damages)
  assign(paste0("totals_median_", unique(df$scenario)), totals_median)
  
  toc()
}

################################################################################
################################################################################
# plot data
dev.off()

pdf(paste0("figures/", gsub("-", "", Sys.Date()) ,"/figS13.pdf"), width=12, height=6)

par(mfrow = c(1,1))
par(mar= c(8,6,2,2))

plot(-1,-1,pch = 15, #xlim = range(0,4), 
     ylim = range(0,15), 
     xlim = range(0,7500),
     cex= 0, yaxt = "n",
     frame.plot = F, col = "#d1def0", ylab = "", 
     xlab = "\n \n SCC (per tonne damages in $USD) Test", 
     family = "Helvetica",
     cex.lab = 2, cex.axis = 1.25)


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
        y = c(3, 3,4, 4),                             # Y-Coordinates of polygon
        col = "#efe8f6")  

polygon(x = c(min(totals_10_90_ssp_2100_ramsey$total_damages), max(totals_10_90_ssp_2100_ramsey$total_damages),
              max(totals_10_90_ssp_2100_ramsey$total_damages), min(totals_10_90_ssp_2100_ramsey$total_damages)),
        border = "#c0a3db",      # X-Coordinates of polygon
        y = c(3, 3,4, 4),                             # Y-Coordinates of polygon
        col = "#c0a3db")  

polygon(x = c(min(totals_iqr_ssp_2100_ramsey$total_damages), max(totals_iqr_ssp_2100_ramsey$total_damages),
              max(totals_iqr_ssp_2100_ramsey$total_damages), min(totals_iqr_ssp_2100_ramsey$total_damages)),
        border = "#581795",      # X-Coordinates of polygon
        y = c(3, 3,4, 4),                             # Y-Coordinates of polygon
        col = "#581795")  

polygon(x = c(totals_median_ssp_2100_ramsey - 25, totals_median_ssp_2100_ramsey + 25,
              totals_median_ssp_2100_ramsey + 25, totals_median_ssp_2100_ramsey - 25),
        border = "gold1",      # X-Coordinates of polygon
        y = c(3, 3,4, 4),                             # Y-Coordinates of polygon
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
        y = c(9, 9,10, 10),                             # Y-Coordinates of polygon
        col = "#efe8f6")  

polygon(x = c(min(totals_10_90_nogrowth_ramsey$total_damages), max(totals_10_90_nogrowth_ramsey$total_damages),
              max(totals_10_90_nogrowth_ramsey$total_damages), min(totals_10_90_nogrowth_ramsey$total_damages)),
        border = "#c0a3db",      # X-Coordinates of polygon
        y = c(9, 9,10, 10),                             # Y-Coordinates of polygon
        col = "#c0a3db")  

polygon(x = c(min(totals_iqr_nogrowth_ramsey$total_damages), max(totals_iqr_nogrowth_ramsey$total_damages),
              max(totals_iqr_nogrowth_ramsey$total_damages), min(totals_iqr_nogrowth_ramsey$total_damages)),
        border = "#581795",      # X-Coordinates of polygon
        y = c(9, 9,10, 10),                             # Y-Coordinates of polygon
        col = "#581795")  

polygon(x = c(totals_median_nogrowth_ramsey - 25, totals_median_nogrowth_ramsey + 25,
              totals_median_nogrowth_ramsey + 25, totals_median_nogrowth_ramsey - 25),
        border = "gold1",      # X-Coordinates of polygon
        y = c(9, 9,10, 10),                             # Y-Coordinates of polygon      
        col = "gold1")  

text(2000, 5, "SSP 2100 growth rates", adj = 0, cex = 1.45, family = "Helvetica")
text(2000, 11, "No growth post 2100", adj = 0, cex = 1.45, family = "Helvetica")



#legend 
polygon(x = c(5600, 6100,
              6100, 5600),
        border = "#581795",      # X-Coordinates of polygon
        y = c(9.5, 9.5,9, 9),                             # Y-Coordinates of polygon
        col = "#581795")  

polygon(x = c(5600, 6100,
              6100, 5600),
        border = "#990000",      # X-Coordinates of polygon
        y = c(8.5, 8.5,8, 8),                             # Y-Coordinates of polygon
        col = "#990000")  

polygon(x = c(5600, 6100,
              6100, 5600),
        border = "gold1",      # X-Coordinates of polygon
        y = c(7.5, 7.5,7.25, 7.25),                             # Y-Coordinates of polygon
        col = "gold1")  


polygon(x = c(5400, 6300,
              6300, 5400),
        border = "grey95",      # X-Coordinates of polygon
        y = c(5.75, 5.75,6.25, 6.25),                             # Y-Coordinates of polygon
        col = "grey95")  
polygon(x = c(5600, 6100,
              6100, 5600),
        border = "grey",      # X-Coordinates of polygon
        y = c(5.75, 5.75,6.25, 6.25),                             # Y-Coordinates of polygon
        col = "grey")  

polygon(x = c(5800, 5900,
              5900, 5800),
        border = "black",      # X-Coordinates of polygon
        y = c(5.75, 5.75,6.25, 6.25),                             # Y-Coordinates of polygon
        col = "black")  

segments(x0 = 6300, x1 = 6300, y0 = 5.60, y1 = 1.6)
segments(x0 = 5400, x1 = 6300, y0 = 1.6, y1 = 1.6)
segments(x0 = 5400, x1 = 5400, y0 = 1.6, y1 = 5.6)
segments(x0 = 5850, x1 = 5850, y0 = 1.6, y1 = 1.2)
text(5850, 1, "95% range", cex = 0.95, family = "Helvetica", face = "bold")

segments(x0 = 6100, x1 = 5600, y0 = 2.60, y1 = 2.6)
segments(x0 = 5600, x1 = 5600, y0 = 5.6, y1 = 2.6)
segments(x0 = 6100, x1 = 6100, y0 = 2.6, y1 = 5.6)
segments(x0 = 5850, x1 = 5850, y0 = 2.6, y1 = 2.4)
text(5850, 2.2, "90% range", cex = 0.95, family = "Helvetica", face = "bold")


segments(x0 = 5900, x1 = 5900, y0 = 5.60, y1 = 3.6)
segments(x0 = 5800, x1 = 5900, y0 = 3.6, y1 = 3.6)
segments(x0 = 5800, x1 = 5800, y0 = 3.6, y1 = 5.6)
segments(x0 = 5850, x1 = 5850, y0 = 3.6, y1 = 3.4)
text(5850, 3.2, "IQR", cex = 0.95, family = "Helvetica", face = "bold")

text(5475,10.15, "Discount rate:", adj = c(1,0), family = "Helvetica", face = "bold", cex = 1.25)

text(5475,9.15, "ramsey(0.2%,1.24)", adj = c(1,0), family = "Helvetica")
text(5475,8.15, "2%", adj = c(1,0), family = "Helvetica")
text(5475,7.15, "median estimate", adj = c(1,0), family = "Helvetica")

dev.off()

# end of script 