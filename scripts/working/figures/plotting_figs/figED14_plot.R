##############################################################################
# Mustafa Zahid, March 13th, 2024
# This R script reads the data and plot the figure ED6. 
#############################################################################
remove(list=ls())
gc()
sf::sf_use_s2(FALSE)
setwd("~/GitHub/loss_damage")

run_date <- "loss_damage_r1_replication_v2"
#############################################################################
#############################################################################
# read the data first
aggregated_data_neg <- readRDS(paste0(getwd(),"/data/figures/", run_date, "/aggregated_transfers_neg.rds"))
#aggregated_data_pos <- readRDS(paste0(getwd(),"/data/figures/", run_date, "/aggregated_transfers_pos.rds"))
#aggregated_data_net <- readRDS(paste0(getwd(),"/data/figures/", run_date, "/aggregated_transfers_net.rds"))
us_top_transfers <- readRDS(paste0(getwd(),"/data/figures/", run_date, "/us_top_transfers.rds"))

#############################################################################
#############################################################################
# now plot the data 
pdf(file = paste0(getwd(), "/figures/", run_date, "/figED14.pdf"),   # The directory you want to save the file in
    width = 8.85, # The width of the plot in inches
    height = 6.85) # The height of the plot in inches
    
### let us start with a) 
par(mfrow = c(2, 1), mar = c(2, 6, 4, 2))
plot(aggregated_data_neg$id, 
     aggregated_data_neg$mean, 
     xaxt = "n", pch = 16, cex = 1.25, 
     frame.plot = F, ylim = range(-1,30), 
     ylab = "Impact (in $trillions)", 
     las = 1, cex.lab = 1.25, col = "red", cex.axis = 1.15)

abline(h = 0)
for (i in 1:10){
  segments(x0 = i, x1 = i, 
           y0 = aggregated_data_neg$p_05[i], 
           y1 = aggregated_data_neg$p_95[i], col = "pink", lwd = 1.35)
  
}
points(aggregated_data_neg$id, 
       aggregated_data_neg$mean, 
       xaxt = "n", pch = 16, cex = 1.25, 
        ylim = range(-5,7), col = "red")
axis(1, at = c(1:10), label = (unique(aggregated_data_neg$emitter)), cex = 1.15)
mtext("a Damages from selected top emitters\n", cex = 1.25, adj = 0)

######### now let us do b 
#par(mar = c(2, 6, 4, 2))
plot(us_top_transfers$id, 
     us_top_transfers$median, 
     xaxt = "n", pch = 16, cex = 1.25, xlab= "", 
     frame.plot = F, ylim = range(0,10), 
     ylab = "net impact (in $trillions)", 
     las = 1, col = "red", cex.lab = 1.25, cex.axis = 1.15)

abline(h = 0)

for (i in 1:10){
  segments(x0 = i, x1 = i, 
           y0 = us_top_transfers$p_05[i], 
           y1 = us_top_transfers$p_95[i], 
           col = "pink", lwd = 1.35)
}


points(us_top_transfers$id, #[us_top_transfers$id < 6], 
       us_top_transfers$median, #[us_top_transfers$id < 6], 
       xaxt = "n", pch = 16, cex = 1.25, xlab= "", 
       frame.plot = F, ylim = range(-1.5,1.5), 
       ylab = "net impact (in $trillions)", 
       las = 1, col = "red", cex.lab = 1.15)


mtext("b Impact of USA emissions (top damaged)" ,adj = 0, cex = 1.25)

us_top_transfers$id[us_top_transfers$id > 5] <- us_top_transfers$id[us_top_transfers$id > 5] + 1
us_top_transfers[nrow(us_top_transfers) + 1,] <- list("USA", '', NA, NA, NA, 6)

text(1, us_top_transfers$p_05[1] + 0.5, "USA")
text(2, us_top_transfers$p_05[2] + 0.75, "CHN")
text(3, us_top_transfers$p_05[3] + 0.75, "JPN")
text(4, us_top_transfers$p_05[4] + 0.75, "IND")
text(5, us_top_transfers$p_05[5] + 0.75, "BRA")
text(6, us_top_transfers$p_95[6] + 0.95, "SAU")
text(7, us_top_transfers$p_95[7] + 0.95, "ITA")
text(8, us_top_transfers$p_95[8] + 0.95, "MEX")
text(9, us_top_transfers$p_95[9] + 0.95, "IDN")
text(10, us_top_transfers$p_95[10] + 0.95, "FRA")

dev.off()

# end of script

