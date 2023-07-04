##############################################################################
# Mustafa Zahid, January 26th, 2023
# This R script is to produce the best sankey diagram to every grace the 
# universie. This is only possible due to Jeff's help. 
# Input(s): bilateral damages dataset, 
# Output(s): a sankey diagram
##############################################################################
# Sankey k90 
# Sankey k80
# Sankey k90-prod 
# Sankey k90-consump
remove(list=ls())
gc()
sf::sf_use_s2(FALSE)
setwd("~/GitHub/loss_damage")

# read in the needed libraries 
source("scripts/working/analysis/0_read_libs.R")

setwd(dropbox_path)
#############################################################################
#############################################################################
# read data 
total_damages_k90 <- readRDS(paste0(output_path, run_date, "/total_damages_k90_v2022.rds"))
total_damages_k80 <- readRDS(paste0(output_path, run_date, "/total_damages_k80_v2022.rds"))
total_damages_k90_prod <- readRDS(paste0(output_path, run_date, "/total_damages_k90_prod_v2022.rds"))
total_damages_k90_consump <- readRDS(paste0(output_path, run_date, "/total_damages_k90_consump_v2022.rds"))

#############################################################################
#############################################################################
# prepa data  

# we will build a funciton that takes in the dataset and prepares it and then 
# just loop over the 4 datasets 
dataset <- total_damages_k80
prep_data_for_sankey <- function(dataset){
  colnames(dataset)[1] <- "reciever"
  # set up EU countries
  dataset$reciever[dataset$reciever%in%c("AUT", "BEL", "BGR", "HRV", 'CYP',
                                                                             "CZE", "DNK", "EST", "FIN","FRA", "DEU", 
                                                                             "GRC", "HUN", "IRL", 'ITA', "LVA", "LTU", 
                                                                             "LUX", "MLT", "NLD", "POL","PRT",
                                                                             "ROU","SVK", "SVN", "ESP", "SWE")] <- "EU"
  dataset$emitter[dataset$emitter%in%c("AUT", "BEL", "BGR", "HRV", 'CYP',
                                                                           "CZE", "DNK", "EST", "FIN","FRA", "DEU", 
                                                                           "GRC", "HUN", "IRL", 'ITA', "LVA", "LTU", 
                                                                           "LUX", "MLT", "NLD", "POL","PRT",
                                                                           "ROU","SVK", "SVN", "ESP", "SWE")] <- "EU"
  
  
  
  # let us create a bitransfer dataset where we can differntiae between negatives and 
  # positive (i.e., damages benefits)
  dataset <- dataset %>% 
    dplyr::mutate(status_transfer = case_when(weighted_damages2 <0 ~ "damages",
                                              weighted_damages2 >0 ~ "benefits",
                                              TRUE ~ "NONE")) 
  
  # now let us sum up the transfers by category (damages and benefits)
  #colnames(dataset)[1] <- "reciever"
  bitransfers <- dataset %>% dplyr::group_by(emitter, reciever) %>% 
    dplyr::summarise(damages = sum(weighted_damages2[status_transfer == 'damages'], na.rm = T),
                     benefits = sum(weighted_damages2[status_transfer == 'benefits'], na.rm = T))
  
  
  # now let us consuder both separatley so we can rank them
  damages_strictly <- dataset %>% dplyr::group_by(emitter, reciever) %>% 
    dplyr::summarise(transfer = sum(weighted_damages2[status_transfer == 'damages'], na.rm = T))
  
  # now rename the columns to make more sense
  colnames(damages_strictly)[1] <- "owing"
  colnames(damages_strictly)[2] <- "owed_to"
  
  # now let us do teh same but with benefits
  benefits_strictly <- dataset %>% dplyr::group_by(emitter, reciever) %>% 
    dplyr::summarise(transfer = sum(weighted_damages2[status_transfer == 'benefits'], na.rm = T))
  
  colnames(benefits_strictly)[1] <- "owed_to"
  colnames(benefits_strictly)[2] <- "owing"
  
  # create a column that records whether the transfer is damage or benefit
  damages_strictly$transfer_status <- "damages"
  benefits_strictly$transfer_status <- "benefits"
  
  
  # Now let us comput ethe total transfer for both sides indpenedently 
  damages_strictly <- damages_strictly %>% dplyr::group_by(owing) %>% 
    dplyr::mutate(total_transfer_owing = sum(transfer, na.rm = T))
  damages_strictly <- damages_strictly %>% dplyr::group_by(owed_to) %>% 
    dplyr::mutate(total_transfer_owed_to = sum(transfer, na.rm = T))
  
  # now let us rename the columns 
  damages_strictly$owing_real <- paste0(damages_strictly$owing, " ($", round(damages_strictly$total_transfer_owing/-1000000000000, 2), 
                                        "T)")
  damages_strictly$owed_to_real <- paste0(damages_strictly$owed_to, " ($", round(damages_strictly$total_transfer_owed_to/-1000000000000, 2), 
                                          "T)")
  
  # now we're donw with damages side
  # remember that we need to do others, we can still do others though i think 
  # Now let us comput ethe total transfer for both sides indpenedently 
  benefits_strictly <- benefits_strictly %>% dplyr::group_by(owing) %>% 
    dplyr::mutate(total_transfer_owing = sum(transfer, na.rm = T))
  benefits_strictly <- benefits_strictly %>% dplyr::group_by(owed_to) %>% 
    dplyr::mutate(total_transfer_owed_to = sum(transfer, na.rm = T))
  
  # now let us rename the columns 
  benefits_strictly$owing_real <- paste0(benefits_strictly$owing, " ($", round(benefits_strictly$total_transfer_owing/1000000000000, 2), 
                                         "T)")
  benefits_strictly$owed_to_real <- paste0(benefits_strictly$owed_to, " ($", round(benefits_strictly$total_transfer_owed_to/1000000000000, 2), 
                                           "T)")
  
  # now we are done woth the naming. What we want to do is to rank, as such we take 
  # the sums 
  # now let us sum up by direction of transfer to rnak
  damages_strictly_sums <- damages_strictly %>% dplyr::group_by(owed_to) %>% 
    dplyr::summarise(transfer = sum(transfer, na.rm = T))
  damages_strictly_sums <- damages_strictly_sums[order(damages_strictly_sums$transfer),] 
  damages_strictly_sums$rank <- 1:nrow(damages_strictly_sums)
  
  benefits_strictly_sums <- benefits_strictly %>% dplyr::group_by(owed_to) %>% 
    dplyr::summarise(transfer = sum(transfer, na.rm = T))
  benefits_strictly_sums <- benefits_strictly_sums[order( - benefits_strictly_sums$transfer),] 
  benefits_strictly_sums$rank <- 1:nrow(benefits_strictly_sums)
  
  # now let us do the same but for the other side
  damages_strictly_sums_owe <- damages_strictly %>% dplyr::group_by(owing) %>% 
    dplyr::summarise(transfer = sum(transfer, na.rm = T))
  damages_strictly_sums_owe <- damages_strictly_sums_owe[order(damages_strictly_sums_owe$transfer),] 
  damages_strictly_sums_owe$rank <- 1:nrow(damages_strictly_sums_owe)
  
  # now with benefit
  benefits_strictly_sums_owe <- benefits_strictly %>% dplyr::group_by(owing) %>% 
    dplyr::summarise(transfer = sum(transfer, na.rm = T))
  benefits_strictly_sums_owe <- benefits_strictly_sums_owe[order(-benefits_strictly_sums_owe$transfer),] 
  benefits_strictly_sums_owe$rank <- 1:nrow(benefits_strictly_sums_owe)
  
  # now let us bring the damages and benefits together
  damages_and_benefits_transfers <- rbind(damages_strictly,
                                          benefits_strictly)
  
  
  # NOW RANK BOTH by total owed to and total owing 
  # what we want to do is to rank the countries on each side of the sankey 
  # we want the countries with the highest amount owed to to be at the top of the 
  # positive side of the trasnfer axis. We are going to limit to top 20 on each side 
  damages_strictly_sums <- damages_strictly_sums %>% dplyr::select(c("owed_to", "rank"))
  damages_and_benefits_transfers <- left_join(damages_and_benefits_transfers,
                                              damages_strictly_sums,
                                              by = c("owed_to"))
  unique(damages_and_benefits_transfers$transfer_status)
  colnames(damages_and_benefits_transfers)[9] <- "rank_damages_owed_to"
  
  damages_strictly_sums_owe <- damages_strictly_sums_owe %>% dplyr::select(c("owing", "rank"))
  damages_and_benefits_transfers <- left_join(damages_and_benefits_transfers,
                                              damages_strictly_sums_owe,
                                              by = c("owing"))
  colnames(damages_and_benefits_transfers)[10] <- "rank_damages_owing"
  
  benefits_strictly_sums_owe <- benefits_strictly_sums_owe %>% dplyr::select(c("owing", "rank"))
  damages_and_benefits_transfers <- left_join(damages_and_benefits_transfers,
                                              benefits_strictly_sums_owe,
                                              by = c("owing"))
  colnames(damages_and_benefits_transfers)[11] <- "rank_benefits_owing"
  
  
  benefits_strictly_sums <- benefits_strictly_sums %>% dplyr::select(c("owed_to", "rank"))
  damages_and_benefits_transfers <- left_join(damages_and_benefits_transfers,
                                              benefits_strictly_sums,
                                              by = c("owed_to"))
  colnames(damages_and_benefits_transfers)[12] <- "rank_benefits_owed_to"
  
  
  # now we need to do a couple of more things. We want to group EU vcountirees 
  # toigether, we want to assign other and we want to keepo top 15 only by name 
  # multiply sum by (-1) to generate total damage in positive sign
  damages_and_benefits_transfers$transfer <- damages_and_benefits_transfers$transfer * (-1) 
  
  damages_back <- subset(damages_and_benefits_transfers, transfer_status == "damages") 
  damages_back$owed_to[damages_back$rank_damages_owed_to > 15] <- "Others"
  damages_back$owing[damages_back$rank_damages_owing > 15] <- "Other emitters"
  
  #damages_back <- subset(damages_and_benefits_transfers, transfer_status == "damages") 
  damages_back$owed_to_real[damages_back$rank_damages_owed_to > 15] <- "Others"
  damages_back$owing_real[damages_back$rank_damages_owing > 15] <- "Other emitters"
  
  
  benefits_back <- subset(damages_and_benefits_transfers, transfer_status == "benefits") 
  benefits_back$owed_to[benefits_back$rank_benefits_owed_to > 6] <- "Others emitters."
  benefits_back$owing[benefits_back$rank_benefits_owing > 6] <- "Others."
  
  benefits_back$owed_to_real[benefits_back$rank_benefits_owed_to > 6] <- "Other emitters."
  benefits_back$owing_real[benefits_back$rank_benefits_owing > 6] <- "Others."
  
  damages_and_benefits_transfers2 <- rbind(damages_back,
                                           benefits_back)
  
  damages_transfers2_owing <- damages_and_benefits_transfers2 %>% 
    dplyr::group_by(owing_real) %>% 
    dplyr::summarise(transfer = sum(transfer[transfer > 0], na.rm = T))
  damages_transfers2_owing <- damages_transfers2_owing[order(-damages_transfers2_owing$transfer),] 
  damages_transfers2_owing$rank <- 1:nrow(damages_transfers2_owing)
  
  benefits_transfers2_owing <- damages_and_benefits_transfers2 %>% 
    dplyr::group_by(owing_real) %>% 
    dplyr::summarise(transfer = sum(transfer[transfer < 0], na.rm = T))
  benefits_transfers2_owing <- benefits_transfers2_owing[order(-benefits_transfers2_owing$transfer),] 
  benefits_transfers2_owing$rank <- 1:nrow(benefits_transfers2_owing)
  
  damages_transfers2_owed_to <- damages_and_benefits_transfers2 %>% 
    dplyr::group_by(owed_to_real) %>% 
    dplyr::summarise(transfer = sum(transfer[transfer > 0], na.rm = T))
  damages_transfers2_owed_to <- damages_transfers2_owed_to[order(-damages_transfers2_owed_to$transfer),] 
  damages_transfers2_owed_to$rank <- 1:nrow(damages_transfers2_owed_to)
  
  benefits_transfers2_owed_to <- damages_and_benefits_transfers2 %>% 
    dplyr::group_by(owed_to_real) %>% 
    dplyr::summarise(transfer = sum(transfer[transfer < 0], na.rm = T))
  benefits_transfers2_owed_to <- benefits_transfers2_owed_to[order(-benefits_transfers2_owed_to$transfer),] 
  benefits_transfers2_owed_to$rank <- 1:nrow(benefits_transfers2_owed_to)
  
  damages_and_benefits_transfers2 <- damages_and_benefits_transfers2 %>% 
    dplyr::mutate(owing_real = case_when(owing_real == "Other emitters" ~ paste0("Other emitters ($", 
                                                                                 round(damages_transfers2_owing$transfer[damages_transfers2_owing$owing_real == "Other emitters"]/1000000000000,2), "T)"),
                                         TRUE ~ owing_real))
  
  
  damages_and_benefits_transfers2 <- damages_and_benefits_transfers2 %>% 
    dplyr::mutate(owing_real = case_when(owing_real == "Others." ~ paste0("Others ($", 
                                                                          round(benefits_transfers2_owing$transfer[benefits_transfers2_owing$owing_real == "Others."]/-1000000000000,2), "T)"),
                                         TRUE ~ owing_real))
  
  damages_and_benefits_transfers2 <- damages_and_benefits_transfers2 %>% 
    dplyr::mutate(owed_to_real = case_when(owed_to_real == "Others" ~ paste0("Others ($", 
                                                                             round(damages_transfers2_owed_to$transfer[damages_transfers2_owed_to$owed_to_real == "Others"]/1000000000000,2), "T)"),
                                           TRUE ~ owed_to_real))
  
  damages_and_benefits_transfers2 <- damages_and_benefits_transfers2 %>% 
    dplyr::mutate(owed_to_real = case_when(owed_to_real == "Other emitters." ~ paste0("Other emitters ($", 
                                                                                      round(benefits_transfers2_owed_to$transfer[benefits_transfers2_owed_to$owed_to_real == "Other emitters."]/-1000000000000,2), "T)"),
                                           TRUE ~ owed_to_real))
  
  
  damages_and_benefits_transfers2 <- ungroup(damages_and_benefits_transfers2)
  damages_and_benefits_transfers2 <- damages_and_benefits_transfers2 %>% dplyr::select(c("owing_real", 
                                                                                         "owed_to_real",
                                                                                         "transfer"))
  damages_and_benefits_transfers2 <- subset(damages_and_benefits_transfers2, transfer != 0)
  
  
  damages_and_benefits_transfers2a <- to_lodes_form(damages_and_benefits_transfers2,axes = 1:2 , id = "flow")

  damages_transfers2_owed_to <- subset(damages_transfers2_owed_to, transfer != 0)
  damages_transfers2_owing <- subset(damages_transfers2_owing, transfer != 0)
  benefits_transfers2_owed_to <- subset(benefits_transfers2_owed_to, transfer != 0)
  benefits_transfers2_owing <- subset(benefits_transfers2_owing, transfer != 0)
  unique(damages_and_benefits_transfers2a$stratum)
  
  damages_and_benefits_transfers2a <- damages_and_benefits_transfers2a %>% 
    dplyr::mutate(stratum = ordered(stratum, levels=c(damages_transfers2_owing$owing_real,
                                                      as.character(unique(damages_and_benefits_transfers2a$stratum)[40]),
                                                      as.character(unique(damages_and_benefits_transfers2a$stratum)[1]),
                                                      as.character(unique(damages_and_benefits_transfers2a$stratum)[17]),
                                                      benefits_transfers2_owing$owing_real,
                                                      damages_transfers2_owed_to$owed_to_real,
                                                      as.character(unique(damages_and_benefits_transfers2a$stratum)[24]),
                                                      benefits_transfers2_owed_to$owed_to_real)))
  
  return(damages_and_benefits_transfers2a)
}

# ok now apply the function on the damages dataset 
damages_and_benefits_k90 <- prep_data_for_sankey(total_damages_k90)
damages_and_benefits_k80 <- prep_data_for_sankey(total_damages_k80)
damages_and_benefits_k90_prod <- prep_data_for_sankey(total_damages_k90_prod)
damages_and_benefits_k90_consump <- prep_data_for_sankey(total_damages_k90_consump)

# data is ready 
write_rds(damages_and_benefits_k90, paste0(fig_prepped_dta,        run_date, "damages_and_benefits_k90.rds"))
write_rds(damages_and_benefits_k80, paste0(fig_prepped_dta,        run_date, "damages_and_benefits_k80.rds"))
write_rds(damages_and_benefits_k90_prod, paste0(fig_prepped_dta,   run_date, "damages_and_benefits_k90_prod.rds"))
write_rds(damages_and_benefits_k90_consump, paste0(fig_prepped_dta,run_date,  "damages_and_benefits_k90_consump.rds"))

# end of script

