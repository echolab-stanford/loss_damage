##############################################################################
# Mustafa Zahid, March 14, 2024
# This R script reads the data and prepares the necessary data to plots figure
# ED14. 
#############################################################################
remove(list=ls())
gc()
sf::sf_use_s2(FALSE)
setwd("~/GitHub/loss_damage")
run_date <- "loss_damage_r1_replication_v2"
#replicate <- F# change T to F if you want to create your own data  
#if (replicate == T){
#  run_date <- "20230523"
#}
#if (replicate == F){
#  run_date <- gsub("-","",Sys.Date())
#}
# read in the needed libraries 
source("scripts/working/analysis/0_read_libs.R")

run_date <- "loss_damage_r1_replication_v2"
setwd("~/BurkeLab Dropbox/projects/loss_damage")

#############################################################################
#############################################################################
########## read the data 
# ok get the path to the files
files <- list.files(paste0(getwd(), "/data/output/", run_date, "/20240228/"),
                    pattern = "bi",
                    full.names = T)

########## we are gonna read the data in 2 chuncks
list_files <- mclapply(files[1:500], readRDS)
list_files2 <- mclapply(files[501:1000], readRDS)

# now let us rbind all of them to calculate total damages for each of the emitter - reciever pair
master_df <- do.call(rbind, list_files)
master_df2 <- do.call(rbind, list_files2)

# ok now let us collect them into one dataframe
gran_master_df <- rbind(master_df,
                        master_df2)

#############################################################################
#############################################################################
########## prep the data 
# ok now we can aggregate the dataset
#aggregated_data_pos <- gran_master_df %>% 
#  dplyr::group_by(sim_id, emitter, ISO3) %>% 
#  subset(.,weighted_damages2 > 0) %>% 
#  dplyr::summarise(total_transfer_pos_damages = sum(weighted_damages2, na.rm = T))
#summary(aggregated_data_pos$total_transfer_pos_damages/1000000000000)
#aggregated_data_pos <- aggregated_data_pos %>% 
#  dplyr::group_by(sim_id, emitter) %>% 
#  dplyr::summarise(total_transfer_pos_damages = sum(total_transfer_pos_damages, na.rm = T))
## now let us save the 95th percentile and the median 
#aggregated_data_pos <- aggregated_data_pos %>% 
#  dplyr::group_by(emitter) %>% 
#  dplyr::summarise(p_05 = quantile(total_transfer_pos_damages, 0.05), 
#                   median = median(total_transfer_pos_damages),
#                   p_95 = quantile(total_transfer_pos_damages, 0.95))
#
# ok now we can aggregate the dataset
aggregated_data_neg <- gran_master_df %>% 
  subset(.,weighted_damages2 <0) %>% 
  dplyr::group_by(sim_id, emitter, ISO3) %>% 
  dplyr::summarise(total_transfer_neg_damages = sum(weighted_damages2, na.rm = T))
aggregated_data_neg <- aggregated_data_neg %>% 
  dplyr::group_by(sim_id, emitter) %>% 
  dplyr::summarise(total_transfer_neg_damages = sum(total_transfer_neg_damages, na.rm = T))

summary(aggregated_data_neg$total_transfer_neg_damages[aggregated_data_neg$emitter == "USA"])

# now let us save the 95th percentile and the median 
aggregated_data_neg <- aggregated_data_neg %>% 
  dplyr::group_by(emitter) %>% 
  dplyr::summarise(p_05 = quantile(total_transfer_neg_damages, 0.05), 
                   mean = mean(total_transfer_neg_damages),
                   p_95 = quantile(total_transfer_neg_damages, 0.95))

# ok now we can aggregate the dataset
#aggregated_data_net <- gran_master_df %>% 
#  dplyr::group_by(sim_id, emitter, ISO3) %>% 
#  dplyr::summarise(total_transfer_net_damages = sum(weighted_damages2, na.rm = T))
#aggregated_data_net <- aggregated_data_net %>% 
#  dplyr::group_by(sim_id, emitter) %>% 
#  dplyr::summarise(total_transfer_net_damages = sum(total_transfer_net_damages, na.rm = T))
## now let us save the 95th percentile and the median 
#aggregated_data_net <- aggregated_data_net %>% 
#  dplyr::group_by(emitter) %>% 
#  dplyr::summarise(p_05 = quantile(total_transfer_net_damages, 0.05), 
#                   mean = mean(total_transfer_net_damages),
#                   p_95 = quantile(total_transfer_net_damages, 0.95))
#
#
################# prep the data 
# pos
#aggregated_data_pos$median <- aggregated_data_pos$median/-1000000000000
#aggregated_data_pos$p_05 <- aggregated_data_pos$p_05/-1000000000000
#aggregated_data_pos$p_95 <- aggregated_data_pos$p_95/-1000000000000
#aggregated_data_pos <- aggregated_data_pos[order(aggregated_data_pos$median),]
#aggregated_data_pos$id <- 1:nrow(aggregated_data_pos)

# neg
aggregated_data_neg$mean <- aggregated_data_neg$mean/-1000000000000
aggregated_data_neg$p_05 <- aggregated_data_neg$p_05/-1000000000000
aggregated_data_neg$p_95 <- aggregated_data_neg$p_95/-1000000000000
aggregated_data_neg <- aggregated_data_neg[order(-aggregated_data_neg$mean),]
aggregated_data_neg$id <- 1:nrow(aggregated_data_neg)

# net
#aggregated_data_net$median <- aggregated_data_net$median/-1000000000000
#aggregated_data_net$p_05 <- aggregated_data_net$p_05/-1000000000000
#aggregated_data_net$p_95 <- aggregated_data_net$p_95/-1000000000000
#aggregated_data_net <- aggregated_data_net[order(-aggregated_data_net$median),]
#aggregated_data_net$id <- 1:nrow(aggregated_data_net)
#
#aggregated_data_pos <- subset(aggregated_data_pos, id <11)
aggregated_data_neg <- subset(aggregated_data_neg, id <11)
#aggregated_data_net <- subset(aggregated_data_net, id <11)

##### now let us focus on the US 
us_transfers <- subset(gran_master_df, emitter == "USA")

#### ok now let us aggregate
us_transfers <- us_transfers %>% 
  dplyr::group_by(sim_id, emitter, ISO3) %>% 
  dplyr::summarise(total_damages = sum(weighted_damages2, na.rm = T))

us_transfers_median <- us_transfers %>% 
  dplyr::group_by(emitter, ISO3) %>% 
  dplyr::summarise(median(total_damages))

us_top_transfers <- subset(us_transfers, ISO3 %in% c("USA", "CHN", "JPN", "IND", "BRA", 
                                                     "ITA", "SAU", "IDN", "FRA", "MEX"))


# now let us save the 95th percentile and the median 
us_top_transfers <- us_top_transfers %>% 
  dplyr::group_by(emitter, ISO3) %>% 
  dplyr::summarise(p_05 = quantile(total_damages, 0.05), 
                   median = median(total_damages),
                   p_95 = quantile(total_damages, 0.95))

us_top_transfers$median <- us_top_transfers$median/-1000000000000
us_top_transfers$p_05 <- us_top_transfers$p_05/-1000000000000
us_top_transfers$p_95 <- us_top_transfers$p_95/-1000000000000
us_top_transfers <- us_top_transfers[order(-us_top_transfers$median),]
us_top_transfers$id <- 1:nrow(us_top_transfers)

#############################################################################
#############################################################################
########## save the data 
run_date <- "loss_damage_r1_replication_v2"
setwd("~/GitHub/loss_damage")
write_rds(aggregated_data_neg, paste0(getwd(), "/data/figures/", run_date, "/aggregated_transfers_neg.rds"))
#write_rds(aggregated_data_pos, paste0(getwd(), "/data/figures/", run_date, "/aggregated_transfers_pos.rds"))
#write_rds(aggregated_data_net, paste0(getwd(), "/data/figures/", run_date, "/aggregated_transfers_net.rds"))
write_rds(us_top_transfers, paste0(getwd(), "/data/figures/", run_date, "/us_top_transfers.rds"))

# end of script