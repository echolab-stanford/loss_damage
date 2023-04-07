##############################################################################
# Mustafa Zahid, April 1st, 2022
# This script calculates the total damages owed by emitter country j to receiver 
# country i 
#############################################################################


generate_gdptemp_panel_bhmbs <- function(bhm_mode, 
                                   future_forecast, 
                                   year_k, 
                                   temp_dataset){
  
  bhm_mode_option <- bhm_mode
  
  if (temp_dataset == "ERA"){
    gdp_temp_data <- readRDS("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/world_gdp_pop/temp_gdp_world_panel_102822.rds")
  }
  if (temp_dataset == "CRU"){
    gdp_temp_data <- readRDS("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/world_gdp_pop/temp_gdp_world_panel_102822_cru.rds")
  }
  
  # create the growth variable and set the dataset as a pdataframe
  
  gdp_temp_data <- subset(gdp_temp_data, year < 2021)
  # create the growth variable and set the dataset as a pdataframe
  gdp_temp_data$gdp_pc <- log(gdp_temp_data$NY.GDP.PCAP.KD)
  gdp_temp_data <- plm::pdata.frame(gdp_temp_data, index = c("ISO3","year"))
  gdp_temp_data$lgdp_pc <- plm::lag(gdp_temp_data$gdp_pc)
  gdp_temp_data$diff_lgdp <- gdp_temp_data$gdp_pc - gdp_temp_data$lgdp_pc
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
    if (temp_dataset == "ERA"){
      # Now let us subset teh dataset to teh data that we will need
      gdp_temp_data <- gdp_temp_data %>% dplyr::select(c("ISO3",
                                                         "year",
                                                         "era_mwtemp",
                                                         "era_mwprecip",
                                                         "era_mwtemp",
                                                         "era_mwprecip",
                                                         "diff_lgdp",
                                                         "year2",
                                                         "NY.GDP.PCAP.KD"))
      
    }
    if (temp_dataset == "CRU"){
      # Now let us subset teh dataset to teh data that we will need
      gdp_temp_data <- gdp_temp_data %>% dplyr::select(c("ISO3",
                                                         "year",
                                                         "cru_mwtemp",
                                                         "cru_mwprecip",
                                                         "cru_mwtemp",
                                                         "cru_mwprecip",
                                                         "diff_lgdp",
                                                         "year2",
                                                         "NY.GDP.PCAP.KD"))
      
    }
    
    #cru
    #    bhm_cru_reg <-  fixest::feols(diff_lgdp ~ cru_mwtemp + cru_mwtemp^2 + cru_mwprecip + 
    #                                   cru_mwprecip^2 + ISO3*as.numeric(year) + ISO3*year2 | 
    #                                  ISO3 + year, gdp_temp_data)
    #estimate the response for every country-year
    gdp_temp_data <- rbind.fill(gdp_temp_data,
                                future_forecast)
    
    #gdp_temp_data <- left_join(gdp_temp_data,
    #                          ssp_gdp_pop,
    #                         by = c("ISO3", "year"))
    
    # gdp_temp_data <- gdp_temp_data %>% 
    
    #  dplyr::mutate(cru_mwtemp = case_when(!is.na(cru_mwtemp.x) ~ cru_mwtemp.x,
    #                                      !is.na(cru_mwtemp.y) ~ cru_mwtemp.y))
    
    if (temp_dataset == "ERA"){
      pooledbs <- as.data.frame(readRDS("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/bhm/pooledregression_boostraps_era.rds"))
    }
    if (temp_dataset == "CRU"){
      pooledbs <- readRDS("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/bhm/pooledregression_bootstraps_cru.rds")
    }
    
    gdp_temp_data$merge_id <- 1
    pooledbs$merge_id <- 1
    pooledbs$coef_id <- 1:nrow(pooledbs)
    
    gdp_temp_data <- left_join(gdp_temp_data,
                               pooledbs)
    
    
    gdp_temp_data <- subset(gdp_temp_data, year >= year_k)
    
    if (temp_dataset == "ERA"){
      gdp_temp_data$response_tempactual_era <- ((gdp_temp_data$era_mwtemp)*(gdp_temp_data$temp)) +
        ((gdp_temp_data$era_mwtemp^2)*(gdp_temp_data$temp2))
    }
    if (temp_dataset == "CRU"){
      gdp_temp_data$response_tempactual_cru <- ((gdp_temp_data$cru_mwtemp)*(gdp_temp_data$temp)) +
        ((gdp_temp_data$cru_mwtemp^2)*(gdp_temp_data$temp2))
    }
    
    # we need to create country level annual average for last 5 years of observed 
    # data 
    if (temp_dataset == "ERA"){
      gdp_temp_data <- gdp_temp_data %>% dplyr::group_by(ISO3) %>% 
        dplyr::mutate(avg_temp_2016_2020 = mean(era_mwtemp[year > 2015 & year <= 2020], na.rm = T))
    }
    if (temp_dataset == "CRU"){
      gdp_temp_data <- gdp_temp_data %>% dplyr::group_by(ISO3) %>% 
        dplyr::mutate(avg_temp_2016_2020 = mean(cru_mwtemp[year > 2015 & year <= 2020], na.rm = T))
    }
    
    gdp_temp_data <- subset(gdp_temp_data, !is.na(coef_id))
    
    gdp_temp_data$year <- as.numeric(as.character(gdp_temp_data$year))
  }
  else if (bhm_mode_option == "richpoor") {
    gdp_temp_data <- gdp_temp_data %>% dplyr::select(c("ISO3",
                                                       "year",
                                                       "cru_mwtemp",
                                                       "cru_mwprecip",
                                                       "cru_mwtemp",
                                                       "cru_mwprecip",
                                                       "diff_lgdp",
                                                       "year2",
                                                       "NY.GDP.PCAP.PP.KD",
                                                       "NY.GDP.PCAP.KD",
                                                       "poor"))
    
    # Merge both datasets
    #  gdp_temp_data$year <- as.numeric(as.character(gdp_temp_data$year))
    # gdp_temp_data <- left_join(joined_final_df,
    #                           gdp_temp_data,
    #                          by = c("ISO3", "year"))
    
    ##############################################################################
    ##################### PART II: Save estimates of f()  ########################
    ##############################################################################
    # Run the models to estimate the response function f() using both cru and cru 
    
    #cru
    bhm_cru_reg <-  fixest::feols(diff_lgdp ~ cru_mwtemp + cru_mwtemp^2 + (poor*cru_mwtemp) + (poor*cru_mwtemp^2) + 
                                    cru_mwprecip + cru_mwprecip^2 + (poor*cru_mwprecip) + (poor*cru_mwprecip^2) + 
                                    + poor + ISO3*as.numeric(year) + ISO3*year2 | 
                                    ISO3 + year, gdp_temp_data)
    
    
    #estimate the response for every country-year
    class(gdp_temp_data)
    
    gdp_temp_data <- ungroup(gdp_temp_data)
    
    #estimate the response for every country-year
    gdp_temp_data <- left_join(gdp_temp_data,
                               future_forecast,
                               by = c("ISO3",
                                      "year"))
    
    #  gdp_temp_data <- gdp_temp_data %>% 
    #   dplyr::mutate(cru_mwtemp = case_when(!is.na(cru_mwtemp.x) ~ cru_mwtemp.x,
    #                                       !is.na(cru_mwtemp.y) ~ cru_mwtemp.y))
    
    
    
    gdp_temp_data <- gdp_temp_data %>% dplyr::mutate(response_tempactual_cru = case_when(
      poor == 1 ~ ((gdp_temp_data$cru_mwtemp)*(coef(bhm_cru_reg)[1]) + (gdp_temp_data$cru_mwtemp^2)*(coef(bhm_cru_reg)[2]))+              
        (((gdp_temp_data$cru_mwtemp)*(coef(bhm_cru_reg)[5])) + (gdp_temp_data$cru_mwtemp^2)*(coef(bhm_cru_reg)[6])),
      poor == 0 ~ ((gdp_temp_data$cru_mwtemp)*(coef(bhm_cru_reg)[1])) +
        ((gdp_temp_data$cru_mwtemp^2)*(coef(bhm_cru_reg)[2]))))
  }
  return(gdp_temp_data)
}

