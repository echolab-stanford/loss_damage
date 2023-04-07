##############################################################################
# Mustafa Zahid, April 1st, 2022
# This script calculates the total damages owed by emitter country j to receiver 
# country i 
#############################################################################


run_bhm_model_reg_lag5 <- function(bhm_mode){
  
  bhm_mode_option <- bhm_mode
  
  gdp_temp_data <- readRDS("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/temp_gdp_world_panel.rds")
  # create the growth variable and set the dataset as a pdataframe
  gdp_temp_data <- subset(gdp_temp_data, year < 2021)
  # create the growth variable and set the dataset as a pdataframe
  gdp_temp_data$gdp_pc <- log(gdp_temp_data$NY.GDP.PCAP.KD)
  gdp_temp_data <- plm::pdata.frame(gdp_temp_data, index = c("ISO3","year"))
  gdp_temp_data$lgdp_pc <- plm::lag(gdp_temp_data$gdp_pc)
  gdp_temp_data$diff_lgdp <- gdp_temp_data$gdp_pc - gdp_temp_data$lgdp_pc
  lagged_temp <- plm::lag(gdp_temp_data$era_mwtemp, 1:5)
  lagged_precip <- plm::lag(gdp_temp_data$era_mwprecip, 1:5)
  gdp_temp_data[c("era_mwtemp_l1",
                  "era_mwtemp_l2",
                  "era_mwtemp_l3",
                  "era_mwtemp_l4",
                  "era_mwtemp_l5")] <- lagged_temp
  gdp_temp_data[c("era_mwprecip_l1",
                  "era_mwprecip_l2",
                  "era_mwprecip_l3",
                  "era_mwprecip_l4",
                  "era_mwprecip_l5")] <- lagged_precip
  #generating second order of year for quadratic country-year trends 
  gdp_temp_data$year2 <- as.numeric(gdp_temp_data$year)^2
  # generate poor identifiers 
  gdp_temp_data <- gdp_temp_data %>% dplyr::mutate(median_gdp = median(NY.GDP.PCAP.KD, na.rm = T)) %>% 
    dplyr::group_by(ISO3) %>% dplyr::mutate(avg_gdp = mean(NY.GDP.PCAP.KD, na.rm = T))
  gdp_temp_data <- gdp_temp_data %>% 
    dplyr::mutate(poor = case_when(avg_gdp < median_gdp ~ 1,
                                   avg_gdp >= median_gdp ~ 0))
  # Now let us subset teh dataset to teh data that we will need
  
  if (bhm_mode_option == "pooled") {
    # Now let us subset teh dataset to teh data that we will need
    gdp_temp_data <- gdp_temp_data %>% dplyr::select(c("ISO3",
                                                       "year",
                                                       "era_mwtemp",
                                                       "era_mwprecip",
                                                       "era_mwtemp",
                                                       "era_mwtemp_l1",
                                                       "era_mwtemp_l2",
                                                       "era_mwtemp_l3",
                                                       "era_mwtemp_l4",
                                                       "era_mwtemp_l5",
                                                       "era_mwprecip_l1",
                                                       "era_mwprecip_l2",
                                                       "era_mwprecip_l3",
                                                       "era_mwprecip_l4",
                                                       "era_mwprecip_l5",
                                                       "era_mwprecip",
                                                       "diff_lgdp",
                                                       "year2",
                                                       "NY.GDP.PCAP.KD"))
    
    #era
    bhm_era_reg <-  fixest::feols(diff_lgdp ~ era_mwtemp + era_mwtemp^2 + 
                                    era_mwtemp_l1 + era_mwtemp_l1^2 + 
                                    era_mwtemp_l2 + era_mwtemp_l2^2 + 
                                    era_mwtemp_l3 + era_mwtemp_l3^2 +
                                    era_mwtemp_l4 + era_mwtemp_l4^2 +
                                    era_mwtemp_l5 + era_mwtemp_l5^2 +
                                    era_mwprecip + era_mwprecip^2 + 
                                    era_mwprecip_l1 + era_mwprecip_l1^2 + 
                                    era_mwprecip_l2 + era_mwprecip_l2^2 + 
                                    era_mwprecip_l3 + era_mwprecip_l3^2 + 
                                    era_mwprecip_l4 + era_mwprecip_l4^2 + 
                                    era_mwprecip_l5 + era_mwprecip_l5^2 + 
                                    ISO3*as.numeric(year) + ISO3*year2 | 
                                    ISO3 + year, gdp_temp_data)
    
    #estimate the response for every country-year
    gdp_temp_data$response_tempactual_era <- ((gdp_temp_data$era_mwtemp)*(coef(bhm_era_reg)[1])) +
      ((gdp_temp_data$era_mwtemp^2)*(coef(bhm_era_reg)[2]))
  }
  else if (bhm_mode_option == "richpoor") {
    gdp_temp_data <- gdp_temp_data %>% dplyr::select(c("ISO3",
                                                       "year",
                                                       "era_mwtemp",
                                                       "era_mwprecip",
                                                       "era_mwtemp",
                                                       "era_mwtemp_l1",
                                                       "era_mwtemp_l2",
                                                       "era_mwtemp_l3",
                                                       "era_mwtemp_l4",
                                                       "era_mwtemp_l5",
                                                       "era_mwprecip_l1",
                                                       "era_mwprecip_l2",
                                                       "era_mwprecip_l3",
                                                       "era_mwprecip_l4",
                                                       "era_mwprecip_l5",
                                                       "era_mwprecip",
                                                       "diff_lgdp",
                                                       "year2",
                                                       "NY.GDP.PCAP.KD"))
    
    # Merge both datasets
    #  gdp_temp_data$year <- as.numeric(as.character(gdp_temp_data$year))
    # gdp_temp_data <- left_join(joined_final_df,
    #                           gdp_temp_data,
    #                          by = c("ISO3", "year"))
    
    ##############################################################################
    ##################### PART II: Save estimates of f()  ########################
    ##############################################################################
    # Run the models to estimate the response function f() using both era and ERA 
    
    #era
    bhm_era_reg <-  fixest::feols(diff_lgdp ~ era_mwtemp + era_mwtemp^2 + 
                                    era_mwtemp_l1 + era_mwtemp_l1^2 + 
                                    era_mwtemp_l2 + era_mwtemp_l2^2 + 
                                    era_mwtemp_l3 + era_mwtemp_l3^2 +
                                    era_mwtemp_l4 + era_mwtemp_l4^2 +
                                    era_mwtemp_l5 + era_mwtemp_l5^2 +
                                    era_mwprecip + era_mwprecip^2 + 
                                    era_mwprecip_l1 + era_mwprecip_l1^2 + 
                                    era_mwprecip_l2 + era_mwprecip_l2^2 + 
                                    era_mwprecip_l3 + era_mwprecip_l3^2 + 
                                    era_mwprecip_l4 + era_mwprecip_l4^2 + 
                                    era_mwprecip_l5 + era_mwprecip_l5^2 + 
                                    ISO3*as.numeric(year) + ISO3*year2 | 
                                    ISO3 + year, gdp_temp_data)
    
    
    #estimate the response for every country-year
    class(gdp_temp_data)
    
    gdp_temp_data <- ungroup(gdp_temp_data)
    
    gdp_temp_data <- gdp_temp_data %>% dplyr::mutate(response_tempactual_era = case_when(
      poor == 1 ~ ((gdp_temp_data$era_mwtemp)*(coef(bhm_era_reg)[1]) + (gdp_temp_data$era_mwtemp^2)*(coef(bhm_era_reg)[2]))+              
        (((gdp_temp_data$era_mwtemp)*(coef(bhm_era_reg)[5])) + (gdp_temp_data$era_mwtemp^2)*(coef(bhm_era_reg)[6])),
      poor == 0 ~ ((gdp_temp_data$era_mwtemp)*(coef(bhm_era_reg)[1])) +
        ((gdp_temp_data$era_mwtemp^2)*(coef(bhm_era_reg)[2]))))
  }
  return(bhm_era_reg)
}

