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
totals_all   <- readRDS(paste0(fig_prepped_dta,run_date, "/totals_all.rds"))
totals_bhm   <- readRDS(paste0(fig_prepped_dta,run_date, "/totals_bhm.rds"))
totals_cgm   <- readRDS(paste0(fig_prepped_dta,run_date, "/totals_cgm.rds"))
totals_fair  <- readRDS(paste0(fig_prepped_dta,run_date, "/totals_fair.rds"))

################################################################################
################################################################################
# plot data 

# we will need to further ready the data for plotting here
totals_all  <- ungroup(totals_all)
totals_bhm <- ungroup(totals_bhm)
totals_cgm <- ungroup(totals_cgm)
totals_fair <- ungroup(totals_fair)

totals_all$uncertainty <- "all"
totals_bhm$uncertainty <- "bhm"
totals_fair$uncertainty <- "fair"
totals_cgm$uncertainty <- "cgm"


listofdfs <- list(totals_all,
                  totals_bhm,
                  totals_cgm,
                  totals_fair)

for (i in 1:length(listofdfs)) {
  tic()
  df <- listofdfs[[i]]
  colnames(df)[2] <- "total_damages"
  #if (i !=3){
  #  df$total_damages <- df$total_damages/1000000000
  #}
  
  totals_iqr <- subset(df, total_damages >= (quantile(df$total_damages, c(.25, .75))[1]) &
                         total_damages <= (quantile(df$total_damages, c(.25, .75))[2]))
  assign(paste0("totals_iqr_", unique(df$uncertainty)), totals_iqr)
  
  totals_10_90 <- subset(df, total_damages >= (quantile(df$total_damages, c(.10, .90))[1])  & 
                           total_damages <= (quantile(df$total_damages, c(.10, .90))[2]))
  assign(paste0("totals_10_90_", unique(df$uncertainty)), totals_10_90)
  
  
  totals_5_95 <- subset(df, total_damages >= (quantile(df$total_damages, c(.05, .95))[1]) & 
                          total_damages <= (quantile(df$total_damages, c(.05, .95))[2]))
  assign(paste0("totals_5_95_", unique(df$uncertainty)), totals_5_95)
  toc()
}

all_median <- median(totals_all$total_damages)
fair_median <- median(totals_fair$total_damages)
cgm_median <- median(totals_cgm$total_damages)
bhm_median <- median(totals_bhm$total_damages)

pdf(paste0("figures/", run_date,"/fig3c.pdf"), width=12, height=6)

par(mfrow = c(1,1))
par(mar= c(8,6,2,2))
plot(-1,-1,pch = 15, #xlim = range(0,4), 
     ylim = range(0,15), 
     xlim = range(0,2800),
     cex= 0, yaxt = "n",
     frame.plot = F, col = "#d1def0", ylab = "", 
     xlab = "\n \n SCC (per tonne damages in $USD)", cex.lab = 2, cex.axis = 1.25)


polygon(x = c(min(totals_5_95_all$total_damages), max(totals_5_95_all$total_damages),
              max(totals_5_95_all$total_damages), min(totals_5_95_all$total_damages)),
        border = "#ffe6e6",      # X-Coordinates of polygon
        y = c(1, 1,2, 2),                             # Y-Coordinates of polygon
        col = "#ffe6e6")  

polygon(x = c(min(totals_10_90_all$total_damages), max(totals_10_90_all$total_damages),
              max(totals_10_90_all$total_damages), min(totals_10_90_all$total_damages)),
        border = "#ffb3b3",      # X-Coordinates of polygon
        y = c(1, 1,2, 2),                             # Y-Coordinates of polygon
        col = "#ffb3b3")  

polygon(x = c(min(totals_iqr_all$total_damages), max(totals_iqr_all$total_damages),
              max(totals_iqr_all$total_damages), min(totals_iqr_all$total_damages)),
        border = "#990000",      # X-Coordinates of polygon
        y = c(1, 1,2, 2),                             # Y-Coordinates of polygon
        col = "#990000")  

polygon(x = c(all_median - 5, all_median + 5,
              all_median + 5, all_median - 5),
        border = "gold1",      # X-Coordinates of polygon
        y = c(1, 1,2, 2),                             # Y-Coordinates of polygon
        col = "gold1")  

# bhm
polygon(x = c(min(totals_5_95_bhm$total_damages), max(totals_5_95_bhm$total_damages),
              max(totals_5_95_bhm$total_damages), min(totals_5_95_bhm$total_damages)),
        border = "#e6f2ff",      # X-Coordinates of polygon
        y = c(3, 3,4, 4),                             # Y-Coordinates of polygon
        col = "#e6f2ff")  

polygon(x = c(min(totals_10_90_bhm$total_damages), max(totals_10_90_bhm$total_damages),
              max(totals_10_90_bhm$total_damages), min(totals_10_90_bhm$total_damages)),
        border = "#80bdff",      # X-Coordinates of polygon
        y = c(3, 3,4, 4),                             # Y-Coordinates of polygon
        col = 	"#80bdff")  

polygon(x = c(min(totals_iqr_bhm$total_damages), max(totals_iqr_bhm$total_damages),
              max(totals_iqr_bhm$total_damages), min(totals_iqr_bhm$total_damages)),
        border = "#0056b3",      # X-Coordinates of polygon
        y = c(3, 3,4, 4),                             # Y-Coordinates of polygon
        col = "#0056b3")  

polygon(x = c(bhm_median - 5, bhm_median + 5,
              bhm_median + 5, bhm_median - 5),
        border = "gold1",      # X-Coordinates of polygon
        y = c(3, 3,4, 4),                             # Y-Coordinates of polygon
        col = "gold1")  


# fair
polygon(x = c(min(totals_5_95_fair$total_damages), max(totals_5_95_fair$total_damages),
              max(totals_5_95_fair$total_damages), min(totals_5_95_fair$total_damages)),
        border = "#e8f6f1",      # X-Coordinates of polygon
        y = c(5, 5,6, 6),                             # Y-Coordinates of polygon
        col = "#e8f6f1")  

polygon(x = c(min(totals_10_90_fair$total_damages), max(totals_10_90_fair$total_damages),
              max(totals_10_90_fair$total_damages), min(totals_10_90_fair$total_damages)),
        border = "#8cd3b9",      # X-Coordinates of polygon
        y = c(5, 5,6, 6),                             # Y-Coordinates of polygon
        col = 	"#8cd3b9")  

polygon(x = c(min(totals_iqr_fair$total_damages), max(totals_iqr_fair$total_damages),
              max(totals_iqr_fair$total_damages), min(totals_iqr_fair$total_damages)),
        border = "#127450",      # X-Coordinates of polygon
        y = c(5, 5,6, 6),                             # Y-Coordinates of polygon
        col = "#127450")  

polygon(x = c(fair_median - 5, fair_median + 5,
              fair_median + 5, fair_median - 5),
        border = "gold1",      # X-Coordinates of polygon
        y = c(5, 5,6, 6),                             # Y-Coordinates of polygon
        col = "gold1")  


# cgm
polygon(x = c(min(totals_5_95_cgm$total_damages), max(totals_5_95_cgm$total_damages),
              max(totals_5_95_cgm$total_damages), min(totals_5_95_cgm$total_damages)),
        border = "#efe8f6",      # X-Coordinates of polygon
        y = c(7, 7,8, 8),                             # Y-Coordinates of polygon
        col = "#efe8f6")  

polygon(x = c(min(totals_10_90_cgm$total_damages), max(totals_10_90_cgm$total_damages),
              max(totals_10_90_cgm$total_damages), min(totals_10_90_cgm$total_damages)),
        border = "#c0a3db",      # X-Coordinates of polygon
        y = c(7, 7,8, 8),                             # Y-Coordinates of polygon
        col = 	"#c0a3db")  

polygon(x = c(min(totals_iqr_cgm$total_damages), max(totals_iqr_cgm$total_damages),
              max(totals_iqr_cgm$total_damages), min(totals_iqr_cgm$total_damages)),
        border = "#581795",      # X-Coordinates of polygon
        y = c(7, 7,8, 8),                             # Y-Coordinates of polygon
        col = "#581795")  

polygon(x = c(cgm_median - 5, cgm_median + 5,
              cgm_median + 5, cgm_median - 5),
        border = "gold1",      # X-Coordinates of polygon
        y = c(7, 7,8, 8),                             # Y-Coordinates of polygon
        col = "gold1")  

#legend 
polygon(x = c(2100, 2300,
              2300, 2100),
        border = "#581795",      # X-Coordinates of polygon
        y = c(10.5, 10.5,10, 10),                             # Y-Coordinates of polygon
        col = "#581795")  
polygon(x = c(2100, 2300,
              2300, 2100),
        border = "#127450",      # X-Coordinates of polygon
        y = c(9.5, 9.5,9, 9),                             # Y-Coordinates of polygon
        col = "#127450")  

polygon(x = c(2100, 2300,
              2300, 2100),
        border = "#0056b3",      # X-Coordinates of polygon
        y = c(8.5, 8.5,8, 8),                             # Y-Coordinates of polygon
        col = "#0056b3")  
polygon(x = c(2100, 2300,
              2300, 2100),
        border = "#990000",      # X-Coordinates of polygon
        y = c(7.5, 7.5,7, 7),                             # Y-Coordinates of polygon
        col = "#990000")  

polygon(x = c(2100, 2300,
              2300, 2100),
        border = "gold1",      # X-Coordinates of polygon
        y = c(6.5, 6.5,6.25, 6.25),                             # Y-Coordinates of polygon
        col = "gold1")  


polygon(x = c(2000, 2400,
              2400, 2000),
        border = "grey95",      # X-Coordinates of polygon
        y = c(4.75, 4.75,5.25, 5.25),                             # Y-Coordinates of polygon
        col = "grey95")  
polygon(x = c(2100, 2300,
              2300, 2100),
        border = "grey",      # X-Coordinates of polygon
        y = c(4.75, 4.75,5.25, 5.25),                             # Y-Coordinates of polygon
        col = "grey")  

polygon(x = c(2150, 2250,
              2250, 2150),
        border = "black",      # X-Coordinates of polygon
        y = c(4.75, 4.75,5.25, 5.25),                             # Y-Coordinates of polygon
        col = "black")  

segments(x0 = 2400, x1 = 2400, y0 = 4.60, y1 = 1.6)
segments(x0 = 2000, x1 = 2400, y0 = 1.6, y1 = 1.6)
segments(x0 = 2000, x1 = 2000, y0 = 1.6, y1 = 4.6)
segments(x0 = 2200, x1 = 2200, y0 = 1.6, y1 = 1.2)
text(2200, 1, "95% range", cex = 0.75)

segments(x0 = 2100, x1 = 2300, y0 = 2.60, y1 = 2.6)
segments(x0 = 2300, x1 = 2300, y0 = 4.6, y1 = 2.6)
segments(x0 = 2100, x1 = 2100, y0 = 2.6, y1 = 4.6)
segments(x0 = 2200, x1 = 2200, y0 = 2.6, y1 = 2.4)
text(2200, 2.2, "90% range", cex = 0.75)


segments(x0 = 2150, x1 = 2150, y0 = 4.60, y1 = 3.6)
segments(x0 = 2250, x1 = 2150, y0 = 3.6, y1 = 3.6)
segments(x0 = 2250, x1 = 2250, y0 = 3.6, y1 = 4.6)
segments(x0 = 2200, x1 = 2200, y0 = 3.6, y1 = 3.4)
text(2200, 3.2, "IQR", cex = 0.75)

text(2075,10.15, "climate pattern uncertainty", adj = c(1,0))
text(2075,9.15, "climate sensitivity uncertainty", adj = c(1,0))
text(2075,8.15, "regression uncertainty", adj = c(1,0))
text(2075,7.15, "total uncertainty", adj = c(1,0))
text(2075,6.35, "median estimate", adj = c(1,0))

dev.off()
# end of script
