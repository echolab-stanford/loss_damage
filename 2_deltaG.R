##############################################################################
# Mustafa Zahid, March 24th, 2022
# This R script calculates the growth decrement as a result of anthropogenic 
# forcing-driven warming. It takes in the deltaT's and the BHM model and 
# produces teh growth decrement as a result from anthropogenic forcing
# Input(s): deltaT at the country-year level, and the BHM model (including
# observed growth and temperature data)
# Output(s): A country-year level estimates of the growth decrement as a 
# result of the anthropogenic driven warming
##############################################################################

# calculating model growth response after adjusting for the delta T by creating 
# a function that takes in the temperature variable, as well as the model used
calc_delta_g <- function(dataset, temp_var, model, deltaT) {
  response_tempnew <- ((temp_var - deltaT)*(coef(model)[1])) +
    (((temp_var - deltaT)^2)*(coef(model)[2])) 
  response_tempnew
}

calculate_degrowth <- function(delta_dataset,
                               status) {
  # create the growth variable and set the dataset as a pdataframe
  gdp_temp_data$gdp_pc <- log(gdp_temp_data$NY.GDP.PCAP.KD)
  gdp_temp_data <- plm::pdata.frame(gdp_temp_data, index = c("ISO3","year"))
  gdp_temp_data$lgdp_pc <- plm::lag(gdp_temp_data$gdp_pc)
  gdp_temp_data$diff_lgdp <- gdp_temp_data$gdp_pc - gdp_temp_data$lgdp_pc
  
  #generating second order of year for quadratic country-year trends 
  gdp_temp_data$year2 <- as.numeric(gdp_temp_data$year)^2
  
  # Now let us subset teh dataset to teh data that we will need
  gdp_temp_data <- gdp_temp_data %>% dplyr::select(c("ISO3",
                                                     "year",
                                                     "cru_mwtemp",
                                                     "cru_mwprecip",
                                                     "era_mwtemp",
                                                     "era_mwprecip",
                                                     "diff_lgdp",
                                                     "year2",
                                                     "NY.GDP.PCAP.PP.KD",
                                                     "NY.GDP.PCAP.KD"))
  

  # Merge both datasets
  gdp_temp_data$year <- as.numeric(as.character(gdp_temp_data$year))
  gdp_temp_data <- left_join(delta_dataset,
                             gdp_temp_data,
                             by = c("ISO3", "year"))
  
  
  ##############################################################################
  ##################### PART II: Save estimates of f()  ########################
  ##############################################################################
  # Run the models to estimate the response function f() using both CRU and ERA 
  
  #CRU
  bhm_cru_reg <-  fixest::feols(diff_lgdp ~ cru_mwtemp + cru_mwtemp^2 + cru_mwprecip + 
                                  cru_mwprecip^2 + ISO3*as.numeric(year) + ISO3*year2 | 
                                  ISO3 + year, gdp_temp_data)
  #estimate the response for every country-year
  gdp_temp_data$response_tempactual_cru <- ((gdp_temp_data$cru_mwtemp)*(coef(bhm_cru_reg)[1])) +
    ((gdp_temp_data$cru_mwtemp^2)*(coef(bhm_cru_reg)[2]))
  
  #ERA
  bhm_era_reg <- fixest::feols(diff_lgdp ~ era_mwtemp + era_mwtemp^2 + era_mwprecip + 
                                 era_mwprecip^2 + ISO3*as.numeric(year) + ISO3*year2 | 
                                 ISO3 + year, gdp_temp_data)
  #estimate the response for every country-year
  gdp_temp_data$response_tempactual_era <- ((gdp_temp_data$era_mwtemp)*(coef(bhm_era_reg)[1])) +
    ((gdp_temp_data$era_mwtemp^2)*(coef(bhm_era_reg)[2]))
  
  

  for (i in list_of_deltaTs) {
    gdp_temp_data$response_tempnew <- calc_delta_g(gdp_temp_data,
                                                   gdp_temp_data$cru_mwtemp,
                                                   bhm_cru_reg,
                                                   gdp_temp_data[i])
    
    gdp_temp_data$delta_g_cru <- gdp_temp_data$response_tempactual_cru - 
      gdp_temp_data$response_tempnew
    
    gdp_temp_data$delta_g_cru <- unlist(gdp_temp_data$delta_g_cru)
    
    new_name <- paste0("tempresp_cru_", i)
    new_name1 <- paste0("dg_cru_", i)
    
    gdp_temp_data$delta_g_cru <- unlist(gdp_temp_data$delta_g_cru)
    
    gdp_temp_data <- gdp_temp_data %>% 
      dplyr::rename(!!new_name := response_tempnew,
                    !!new_name1 := delta_g_cru)
  }
  
  #Now with ERA
  for (i in list_of_deltaTs) {
    gdp_temp_data$response_tempnew <- calc_delta_g(gdp_temp_data,
                                                   gdp_temp_data$era_mwtemp,
                                                   bhm_era_reg,
                                                   gdp_temp_data[i])
    
    gdp_temp_data$delta_g_era <- gdp_temp_data$response_tempactual_era - 
      gdp_temp_data$response_tempnew
    
    gdp_temp_data$delta_g_era <- unlist(gdp_temp_data$delta_g_era)
    
    new_name <- paste0("tempresp_era_", i)
    new_name1 <- paste0("dg_era_", i)
    
    gdp_temp_data$delta_g_era <- unlist(gdp_temp_data$delta_g_era)
    
    
    gdp_temp_data <- gdp_temp_data %>% 
      dplyr::rename(!!new_name := response_tempnew,
                    !!new_name1 := delta_g_era)
  }
  
  #list teh deltaG variables from the processed dataset
  list_of_deltaGs <- list("dg_cru_deltaT_1960",
                          "dg_cru_deltaT_1980",
                          "dg_cru_deltaT_1990")
  
  # Now calculating the damages
  for ( i in list_of_deltaGs) {
    year_i <- substr(i, 15, 18)
    gdp_temp_data1 <- subset(gdp_temp_data, year >= year_i)
    
    gdp_temp_data1 <- gdp_temp_data1 %>% 
      dplyr::group_by(ISO3) %>% 
      dplyr::mutate(gdp_year1 = NY.GDP.PCAP.KD[year == year_i])
    
    gdp_temp_data1$adj_growth <- (gdp_temp_data1[i] + gdp_temp_data1$diff_lgdp)
    
    gdp_temp_data1$diff_lgdp <- gdp_temp_data1$diff_lgdp + 1
    
    gdp_temp_data1$adj_growth <- gdp_temp_data1$adj_growth + 1
    
    gdp_temp_data1$diff_lgdp[is.na(gdp_temp_data1$diff_lgdp)] <- 1
    gdp_temp_data1$adj_growth[is.na(gdp_temp_data1$adj_growth)] <- 1
    
    gdp_temp_data1 <- gdp_temp_data1 %>% dplyr::group_by(ISO3) %>% 
      dplyr::mutate(cum_growth = cumprod(diff_lgdp),
                    cum_adj_growth = cumprod(adj_growth))
    
    
    new_name <- paste0("gdp_cru_", year_i)
    new_name1 <- paste0("damages_cru_", year_i)
    
    gdp_temp_data1 <- gdp_temp_data1 %>% dplyr::rename(!!new_name := gdp_year1)
    #gdp_temp_data1 <- gdp_temp_data1 %>% dplyr::rename(!!new_name1 := damages)
    
    write_rds(gdp_temp_data1, paste0("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/damages_", 
                                     status, "_cru_", year_i, ".rds"))
  }
  
  #list teh deltaG variables from the processed dataset
  list_of_deltaGs <- list("dg_era_deltaT_1960",
                          "dg_era_deltaT_1980",
                          "dg_era_deltaT_1990")
  # Now calculating the damages
  for ( i in list_of_deltaGs) {
    year_i <- substr(i, 15, 18)
    gdp_temp_data1 <- subset(gdp_temp_data, year >= year_i)
    
    gdp_temp_data1 <- gdp_temp_data1 %>% 
      dplyr::group_by(ISO3) %>% 
      dplyr::mutate(gdp_year1 = NY.GDP.PCAP.KD[year == year_i])
    
    gdp_temp_data1$adj_growth <- (gdp_temp_data1[i] + gdp_temp_data1$diff_lgdp)
    
    gdp_temp_data1$diff_lgdp <- gdp_temp_data1$diff_lgdp + 1
    gdp_temp_data1$adj_growth <- gdp_temp_data1$adj_growth + 1
    
    gdp_temp_data1$diff_lgdp[is.na(gdp_temp_data1$diff_lgdp)] <- 1
    gdp_temp_data1$adj_growth[is.na(gdp_temp_data1$adj_growth)] <- 1
    
    gdp_temp_data1 <- gdp_temp_data1 %>% dplyr::group_by(ISO3) %>% 
      dplyr::mutate(cum_growth = cumprod(diff_lgdp),
                    cum_adj_growth = cumprod(adj_growth))
    
    new_name <- paste0("gdp_era_", year_i)
    #new_name1 <- paste0("damages_era_", year_i)
    
    gdp_temp_data1 <- gdp_temp_data1 %>% dplyr::rename(!!new_name := gdp_year1)
    #gdp_temp_data1 <- gdp_temp_data1 %>% dplyr::rename(!!new_name1 := damages)
    
    write_rds(gdp_temp_data1, paste0("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/damages_", 
                                     status, "_era_", year_i, ".rds"))
  }
  
  return(gdp_temp_data)
  
}


#calculate growth decrement by e ensemble/realization 
calculate_degrowth_byvar <- function(delta_dataset,
                                     status,
                                     byvar) {
  # create the growth variable and set the dataset as a pdataframe
  gdp_temp_data <- readRDS("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/temp_gdp_world_panel.rds")
  
  gdp_temp_data$gdp_pc <- log(gdp_temp_data$NY.GDP.PCAP.KD)
  gdp_temp_data <- plm::pdata.frame(gdp_temp_data, index = c("ISO3","year"))
  gdp_temp_data$lgdp_pc <- plm::lag(gdp_temp_data$gdp_pc)
  gdp_temp_data$diff_lgdp <- gdp_temp_data$gdp_pc - gdp_temp_data$lgdp_pc
  
  #generating second order of year for quadratic country-year trends 
  gdp_temp_data$year2 <- as.numeric(gdp_temp_data$year)^2
  
  # Now let us subset teh dataset to teh data that we will need
  gdp_temp_data <- gdp_temp_data %>% dplyr::select(c("ISO3",
                                                     "year",
                                                     "cru_mwtemp",
                                                     "cru_mwprecip",
                                                     "era_mwtemp",
                                                     "era_mwprecip",
                                                     "diff_lgdp",
                                                     "year2",
                                                     "NY.GDP.PCAP.PP.KD",
                                                     "NY.GDP.PCAP.KD"))
  

  # Merge both datasets
  gdp_temp_data$year <- as.numeric(as.character(gdp_temp_data$year))
  gdp_temp_data <- left_join(delta_dataset,
                             gdp_temp_data,
                             by = c("ISO3", "year"))
  
  
  ##############################################################################
  ##################### PART II: Save estimates of f()  ########################
  ##############################################################################
  # Run the models to estimate the response function f() using both CRU and ERA 
  
  #CRU
  bhm_cru_reg <-  fixest::feols(diff_lgdp ~ cru_mwtemp + cru_mwtemp^2 + cru_mwprecip + 
                                  cru_mwprecip^2 + ISO3*as.numeric(year) + ISO3*year2 | 
                                  ISO3 + year, gdp_temp_data)
  #estimate the response for every country-year
  gdp_temp_data$response_tempactual_cru <- ((gdp_temp_data$cru_mwtemp)*(coef(bhm_cru_reg)[1])) +
    ((gdp_temp_data$cru_mwtemp^2)*(coef(bhm_cru_reg)[2]))
  
  #ERA
  bhm_era_reg <- fixest::feols(diff_lgdp ~ era_mwtemp + era_mwtemp^2 + era_mwprecip + 
                                 era_mwprecip^2 + ISO3*as.numeric(year) + ISO3*year2 | 
                                 ISO3 + year, gdp_temp_data)
  #estimate the response for every country-year
  gdp_temp_data$response_tempactual_era <- ((gdp_temp_data$era_mwtemp)*(coef(bhm_era_reg)[1])) +
    ((gdp_temp_data$era_mwtemp^2)*(coef(bhm_era_reg)[2]))
  
  
  
  for (i in list_of_deltaTs) {
    gdp_temp_data$response_tempnew <- calc_delta_g(gdp_temp_data,
                                                   gdp_temp_data$cru_mwtemp,
                                                   bhm_cru_reg,
                                                   gdp_temp_data[i])
    
    gdp_temp_data$delta_g_cru <- gdp_temp_data$response_tempactual_cru - 
      gdp_temp_data$response_tempnew
    
    gdp_temp_data$delta_g_cru <- unlist(gdp_temp_data$delta_g_cru)
    
    new_name <- paste0("tempresp_cru_", i)
    new_name1 <- paste0("dg_cru_", i)
    
    gdp_temp_data$delta_g_cru <- unlist(gdp_temp_data$delta_g_cru)
    
    gdp_temp_data <- gdp_temp_data %>% 
      dplyr::rename(!!new_name := response_tempnew,
                    !!new_name1 := delta_g_cru)
  }
  
  #Now with ERA
  for (i in list_of_deltaTs) {
    gdp_temp_data$response_tempnew <- calc_delta_g(gdp_temp_data,
                                                   gdp_temp_data$era_mwtemp,
                                                   bhm_era_reg,
                                                   gdp_temp_data[i])
    
    gdp_temp_data$delta_g_era <- gdp_temp_data$response_tempactual_era - 
      gdp_temp_data$response_tempnew
    
    gdp_temp_data$delta_g_era <- unlist(gdp_temp_data$delta_g_era)
    
    new_name <- paste0("tempresp_era_", i)
    new_name1 <- paste0("dg_era_", i)
    
    gdp_temp_data$delta_g_era <- unlist(gdp_temp_data$delta_g_era)
    
    
    gdp_temp_data <- gdp_temp_data %>% 
      dplyr::rename(!!new_name := response_tempnew,
                    !!new_name1 := delta_g_era)
  }
  
  #list teh deltaG variables from the processed dataset
  list_of_deltaGs <- list("dg_cru_deltaT_1960",
                          "dg_cru_deltaT_1980",
                          "dg_cru_deltaT_1990")
  
  # Now calculating the damages
  for ( i in list_of_deltaGs) {
    year_i <- substr(i, 15, 18)
    gdp_temp_data1 <- subset(gdp_temp_data, year >= year_i)
    
    gdp_temp_data1 <- gdp_temp_data1 %>% 
      dplyr::group_by(ISO3, {{byvar}}) %>% 
      dplyr::mutate(gdp_year1 = NY.GDP.PCAP.KD[year == year_i])
    
    gdp_temp_data1$adj_growth <- (gdp_temp_data1[i] + gdp_temp_data1$diff_lgdp)
    
    gdp_temp_data1$diff_lgdp <- gdp_temp_data1$diff_lgdp + 1
    
    gdp_temp_data1$adj_growth <- gdp_temp_data1$adj_growth + 1
    
    gdp_temp_data1$diff_lgdp[is.na(gdp_temp_data1$diff_lgdp)] <- 1
    gdp_temp_data1$adj_growth[is.na(gdp_temp_data1$adj_growth)] <- 1
    
    gdp_temp_data1 <- gdp_temp_data1 %>% dplyr::group_by(ISO3, {{byvar}}) %>% 
      dplyr::mutate(cum_growth = cumprod(diff_lgdp),
                    cum_adj_growth = cumprod(adj_growth))
    
    
    new_name <- paste0("gdp_cru_", year_i)
    new_name1 <- paste0("damages_cru_", year_i)
    
    gdp_temp_data1 <- gdp_temp_data1 %>% dplyr::rename(!!new_name := gdp_year1)
    #gdp_temp_data1 <- gdp_temp_data1 %>% dplyr::rename(!!new_name1 := damages)
    
    write_rds(gdp_temp_data1, paste0("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/damages_", status, "_cru_", year_i, ".rds"))
  }
  
  #list teh deltaG variables from the processed dataset
  list_of_deltaGs <- list("dg_era_deltaT_1960",
                          "dg_era_deltaT_1980",
                          "dg_era_deltaT_1990")
  # Now calculating the damages
  for ( i in list_of_deltaGs) {
    year_i <- substr(i, 15, 18)
    gdp_temp_data1 <- subset(gdp_temp_data, year >= year_i)
    
    gdp_temp_data1 <- gdp_temp_data1 %>% 
      dplyr::group_by(ISO3, {{byvar}}) %>% 
      dplyr::mutate(gdp_year1 = NY.GDP.PCAP.KD[year == year_i])
    
    gdp_temp_data1$adj_growth <- (gdp_temp_data1[i] + gdp_temp_data1$diff_lgdp)
    
    gdp_temp_data1$diff_lgdp <- gdp_temp_data1$diff_lgdp + 1
    gdp_temp_data1$adj_growth <- gdp_temp_data1$adj_growth + 1
    
    gdp_temp_data1$diff_lgdp[is.na(gdp_temp_data1$diff_lgdp)] <- 1
    gdp_temp_data1$adj_growth[is.na(gdp_temp_data1$adj_growth)] <- 1
    
    gdp_temp_data1 <- gdp_temp_data1 %>% dplyr::group_by(ISO3, {{byvar}}) %>% 
      dplyr::mutate(cum_growth = cumprod(diff_lgdp),
                    cum_adj_growth = cumprod(adj_growth))
    
    new_name <- paste0("gdp_era_", year_i)
    #new_name1 <- paste0("damages_era_", year_i)
    
    gdp_temp_data1 <- gdp_temp_data1 %>% dplyr::rename(!!new_name := gdp_year1)
    #gdp_temp_data1 <- gdp_temp_data1 %>% dplyr::rename(!!new_name1 := damages)
    
    write_rds(gdp_temp_data1, paste0("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/damages_", status, "_era_", year_i, ".rds"))
  }
  
  return(gdp_temp_data)
  
}


# end of script 