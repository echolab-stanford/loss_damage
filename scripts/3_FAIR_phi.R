##############################################################################
# Mustafa Zahid, April 5th, 2022
# This R script reads in the FAIR climate model temperature response at the 
# country-year level.
##############################################################################

read_FAIR_median_phi <- function(year) {
  # Let us read in the FAIR estimates 
  fair <- readRDS(paste0("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/fair_temp_resp_k_", year, ".rds"))
  # select the needed variables only
  fair_temp_response <- fair %>% 
    dplyr::select(c("iso", "year", "loop" , "temp_response"))
  # calculate the proportional contribution to warming at the country-year level 
  fair_temp_response <- fair_temp_response %>% dplyr::group_by(year, loop) %>% 
    dplyr::mutate(total_temp_d = sum(temp_response, na.rm = T)) %>% 
    dplyr::mutate(phi = temp_response / total_temp_d)
  # keep only years that are within the sample 
  fair_temp_response <- subset(fair_temp_response, year <2021)
  median_phi <- fair_temp_response %>% dplyr::group_by(iso, year) %>% 
    dplyr::summarise(phi = median(phi, na.rm = T),
                     .groups = "keep")
}


read_FAIR_phi <- function(year) {
  # Let us read in the FAIR estimates 
  fair <-readRDS(paste0("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/fair_temp_resp_k_", year, ".rds"))
  # select the needed variables only
  fair_temp_response <- fair %>% 
    dplyr::select(c("iso", "year", "loop" , "temp_response"))
  # calculate the proportional contribution to warming at the country-year level 
  fair_temp_response <- fair_temp_response %>% dplyr::group_by(year, loop) %>% 
    dplyr::mutate(total_temp_d = sum(temp_response, na.rm = T)) %>% 
    dplyr::mutate(phi = temp_response / total_temp_d)
  # keep only years that are within the sample 
  fair_temp_response <- subset(fair_temp_response, year <2021)
  #median_phi <- fair_temp_response %>% dplyr::group_by(iso, year, loop) %>% 
    #dplyr::summarise(phi = median(phi, na.rm = T),
     #                .groups = "keep")
}




# end of script

