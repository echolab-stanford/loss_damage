##############################################################################
# Mustafa Zahid, April 1st, 2022
# This script calculates the total damages owed by emitter country j to receiver 
# country i 
# This script is associated with any damages calculation. It prepares the 
# country-year temperature and income observations. 
# Last edited: June 2023
#############################################################################


generate_gdptemp_panel <- function(bhm_mode, 
                          future_forecast, 
                          year_k, 
                          temp_dataset){
  
  bhm_mode_option <- bhm_mode
  
  if (temp_dataset == "ERA"){
    #gdp_temp_data1 <- readRDS("data/processed/world_gdp_pop/temp_gdp_world_panel_052223.rds")
    gdp_temp_data <- readRDS("data/processed/temp_gdp_world_panel_052223.rds")
                              
    
  }
  if (temp_dataset == "CRU"){
    gdp_temp_data <- readRDS("data/processed/world_gdp_pop/temp_gdp_world_panel_102822_cru.rds")
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
  
  
  
  # Now let us apply the same transofrmations for the GDP variable we want to 
  # use for the calculation of damages and not for the response function
  gdp_temp_data$gdp_pc_for_damages <- log(gdp_temp_data$NY.GDP.PCAP.KD_for_damages)
  gdp_temp_data <- plm::pdata.frame(gdp_temp_data, index = c("ISO3","year"))
  gdp_temp_data$lgdp_pc_for_damages <- plm::lag(gdp_temp_data$gdp_pc_for_damages)
  gdp_temp_data$diff_lgdp_for_damages <- gdp_temp_data$gdp_pc_for_damages - gdp_temp_data$lgdp_pc_for_damages
  
 # gdp_temp_data <- as.data.frame(gdp_temp_data)
  
  # Now let us subset teh dataset to teh data that we will need
  
  if (bhm_mode_option == "pooled") {
    if (temp_dataset == "ERA"){
      # Now let us subset teh dataset to teh data that we will need
      gdp_temp_data <- gdp_temp_data %>% dplyr::select(c("ISO3",
                                                         "year",
                                                         "era_mwtemp",
                                                         "era_mwprecip",
                                                         "diff_lgdp",
                                                         "year2",
                                                         "NY.GDP.PCAP.KD",
                                                         "gdp_pc_for_damages",
                                                         "NY.GDP.PCAP.KD_for_damages",
                                                         "diff_lgdp_for_damages"))
                                                         #"Income.group"))
      
      
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
                                                         "NY.GDP.PCAP.KD",
                                                         "gdp_pc_for_damages",
                                                         "gdp_for_damages",
                                                         "diff_lgdp_for_damages"))
      
    }
    
    if (temp_dataset == "ERA"){
      
    #era
    bhm_model <-  fixest::feols(diff_lgdp ~ era_mwtemp + era_mwtemp^2 + era_mwprecip + 
                                  era_mwprecip^2 + ISO3*as.numeric(year) + ISO3*year2 | 
                                  ISO3 + year, gdp_temp_data)
  }
  
  if (temp_dataset == "CRU"){
    
    #cru
    bhm_model <-  fixest::feols(diff_lgdp ~ era_mwtemp + era_mwtemp^2 + era_mwprecip + 
                                  era_mwprecip^2 + ISO3*as.numeric(year) + ISO3*year2 | 
                                  ISO3 + year, gdp_temp_data)
  }
  
   
    colnames(future_forecast)[4] <- "diff_lgdp_for_damages"
    gdp_temp_data <- rbind.fill(gdp_temp_data,
                                future_forecast)
    gdp_temp_data <- subset(gdp_temp_data, year >= year_k)
    
    if (temp_dataset == "ERA"){
      gdp_temp_data$response_tempactual_era <- ((gdp_temp_data$era_mwtemp)*(coef(bhm_model)[1])) +
        ((gdp_temp_data$era_mwtemp^2)*(coef(bhm_model)[2]))
    }
    if (temp_dataset == "CRU"){
      gdp_temp_data$response_tempactual_cru <- ((gdp_temp_data$cru_mwtemp)*(coef(bhm_model)[1])) +
        ((gdp_temp_data$cru_mwtemp^2)*(coef(bhm_model)[2]))
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
    
    gdp_temp_data$year <- as.numeric(as.character(gdp_temp_data$year))
    
    if (year_k == 1990){
      for(i in unique(gdp_temp_data$ISO3)){
        gdp_temp_data$diff_lgdp_for_damages[gdp_temp_data$ISO3 == i & is.na(gdp_temp_data$diff_lgdp_for_damages) & gdp_temp_data$year == 1990] <- 0
      }
      
      for(i in unique(gdp_temp_data$ISO3)){
        if (i == "TKM" | i == "VEN" | i == "ERI" | i == "YEM"){
          gdp_temp_data$diff_lgdp_for_damages[gdp_temp_data$ISO3 == i & is.na(gdp_temp_data$diff_lgdp_for_damages) & (gdp_temp_data$year > 2014 & gdp_temp_data$year <2021) ] <- 0
        }
        
      }
      
      for(i in unique(gdp_temp_data$ISO3)){
        if (i == "LBY"){
          gdp_temp_data$diff_lgdp_for_damages[gdp_temp_data$ISO3 == i & is.na(gdp_temp_data$diff_lgdp_for_damages) & gdp_temp_data$year < 2000] <- 0
        }
      }
      
    }
    if (year_k == 1980){
      for(i in unique(gdp_temp_data$ISO3)){
        gdp_temp_data$diff_lgdp_for_damages[gdp_temp_data$ISO3 == i & is.na(gdp_temp_data$diff_lgdp_for_damages) & (gdp_temp_data$year >= 1980 & gdp_temp_data$year <= 1990)] <- 0
      }
      
      for(i in unique(gdp_temp_data$ISO3)){
        if (i == "TKM" | i == "VEN" | i == "ERI" | i == "YEM"){
          gdp_temp_data$diff_lgdp_for_damages[gdp_temp_data$ISO3 == i & is.na(gdp_temp_data$diff_lgdp_for_damages) & (gdp_temp_data$year > 2014 & gdp_temp_data$year <2021) ] <- 0
        }
        
      }
      
      for(i in unique(gdp_temp_data$ISO3)){
        if (i == "LBY"){
          gdp_temp_data$diff_lgdp_for_damages[gdp_temp_data$ISO3 == i & is.na(gdp_temp_data$diff_lgdp_for_damages) & gdp_temp_data$year < 2000] <- 0
        }
      }
      
    }
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
    
    
    gdp_temp_data <- gdp_temp_data %>% dplyr::mutate(response_tempactual_cru = case_when(
      poor == 1 ~ ((gdp_temp_data$cru_mwtemp)*(coef(bhm_cru_reg)[1]) + (gdp_temp_data$cru_mwtemp^2)*(coef(bhm_cru_reg)[2]))+              
        (((gdp_temp_data$cru_mwtemp)*(coef(bhm_cru_reg)[5])) + (gdp_temp_data$cru_mwtemp^2)*(coef(bhm_cru_reg)[6])),
      poor == 0 ~ ((gdp_temp_data$cru_mwtemp)*(coef(bhm_cru_reg)[1])) +
        ((gdp_temp_data$cru_mwtemp^2)*(coef(bhm_cru_reg)[2]))))
  }
  return(gdp_temp_data)
}

