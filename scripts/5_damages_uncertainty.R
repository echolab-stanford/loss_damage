##############################################################################
# Mustafa Zahid, March 24th, 2022
# This script plots the distribution of damages owed by emitter country to 
# receiver country. by sources of uncertainty 
##############################################################################

# create a function to calculate total damages by FAIR model inputs
calc_damages_all_fair_loops <- function(emitter, 
                                        reciever, 
                                        dataset_dg, 
                                        dataset_name,
                                        dataset_fair,
                                        dg_variable,
                                        gdp_year, 
                                        gdp_var_year, 
                                        variable,
                                        year_i,
                                        i) {
  dataset_dga <- readRDS(paste0("~/Desktop/loss_damages/data/", dataset_name,
                                ".rds"))
  yeari <- as.numeric(substr(gdp_year, 9, 12))
  dataset_dg <- dataset_dga %>% dplyr::mutate(test_id = 1,
                                             year = as.numeric(year))  %>% 
    subset(ISO3 == emitter | ISO3 == reciever)
  dataset_fair <- dataset_fair %>% dplyr::mutate(test_id = 1)
  dataset_fair_i <- subset(dataset_fair, loop == i) %>% 
    subset(iso == emitter | iso == reciever)
  damages_i_t <- left_join(dataset_fair_i,
                           dataset_dg,
                           by = c("year", 
                                  "test_id"))
  damages_i_t$diff_lgdp <- damages_i_t$diff_lgdp - 1
  damages_i_t$diff_lgdp[damages_i_t$diff_lgdp == 0.000] <- NA
  damages_i_t1 <- damages_i_t %>% 
    dplyr::mutate(dg_cru_deltaT = {{dg_variable}} * phi) 
  damages_i_t2 <- damages_i_t1 %>% 
    dplyr::mutate(adj_growthx = dg_cru_deltaT + diff_lgdp)
  damages_i_t3 <- damages_i_t2 %>% 
    dplyr::mutate(adj_growthxz = adj_growthx + 1)
  damages_i_t3$diff_lgdp1 <- damages_i_t3$diff_lgdp +1
  damages_i_t4 <- damages_i_t3 %>% dplyr::group_by(ISO3, iso, loop) %>% 
    dplyr::mutate(cum_adj_growthz = cumprod(adj_growthxz),
                  cum_growth_real = cumprod(diff_lgdp1))
  damages_i_t4 <- damages_i_t4 %>% dplyr::select(c("ISO3", 
                                                   "year",
                                                   "NY.GDP.PCAP.KD",
                                                   gdp_year,
                                                   variable,
                                                   "diff_lgdp",
                                                   "cum_growth", 
                                                   "cum_adj_growth",
                                                   "iso",
                                                   "phi",
                                                   "cum_adj_growthz",
                                                   "cum_growth_real",
                                                   "loop"))
  damages_i_t4 <- damages_i_t4 %>% dplyr::mutate(damages = ({{gdp_var_year}} * cum_adj_growthz) - 
                                                   ({{gdp_var_year}} * cum_growth_real))
  damages_i_t4 <- damages_i_t4 %>% dplyr::mutate(t_since_k = year - yeari,
                                                 weighted_damages2 = damages*((1+(0.02))^t_since_k),
                                                 weighted_damages3 = damages*((1+(0.03))^t_since_k),
                                                 weighted_damages5 = damages*((1+(0.05))^t_since_k),
                                                 weighted_damages7 = damages*((1+(0.06))^t_since_k))
}

# write code for calculating the by loops total damages 
calc_byloops <- function(emitter, 
                         reciever,
                         num_loops,
                         list_dfs){
  damages_loop <- do.call(rbind, list_dfs[1:num_loops])
  damages_loop <- damages_loop %>% dplyr::group_by(ISO3, iso, loop) %>% 
    dplyr::summarise(total_damages = sum(weighted_damages2, na.rm = T),
                     .groups = "keep")
  damages_1990_loop <- damages_loop %>% 
    subset(iso == emitter & ISO3 == reciever)
}


#function for calcing damages 
calc_damages_by_ensembles <- function(emitter,
                                       reciever,
                                       dataset_dg, 
                                       dataset_name,
                                       dataset_fair,
                                       dg_variable,
                                       gdp_year, 
                                       gdp_var_year, 
                                       variable) {
  dataset_dga <- readRDS(paste0("~/Desktop/loss_damages/data/", dataset_name, 
                                ".rds"))
  yeari <- as.numeric(substr(gdp_year, 9, 12))
  dataset_dg <- dataset_dga %>% dplyr::mutate(test_id = 1,
                                              year = as.numeric(year)) %>% 
    subset(ISO3 == emitter | ISO3 == reciever)
  yeari <- as.numeric(substr(gdp_year, 9, 12))
  dataset_fair <- dataset_fair %>% dplyr::mutate(test_id = 1) %>% 
    subset(iso == emitter | iso == reciever)
  damages_i_t <- left_join(dataset_fair,
                           dataset_dg,
                           by = c("year", 
                                  "test_id"))
  damages_i_t$diff_lgdp <- damages_i_t$diff_lgdp - 1
  damages_i_t$diff_lgdp[damages_i_t$diff_lgdp == 0.000] <- NA
  damages_i_t1 <- damages_i_t %>% 
    dplyr::mutate(dg_cru_deltaT = {{dg_variable}} * phi) 
  damages_i_t2 <- damages_i_t1 %>% 
    dplyr::mutate(adj_growthx = dg_cru_deltaT + diff_lgdp)
  damages_i_t3 <- damages_i_t2 %>% 
    dplyr::mutate(adj_growthxz = adj_growthx + 1)
  damages_i_t3$diff_lgdp1 <- damages_i_t3$diff_lgdp +1
  damages_i_t4 <- damages_i_t3 %>% dplyr::group_by(ISO3, iso, realization) %>% 
    dplyr::mutate(cum_adj_growthz = cumprod(adj_growthxz),
                  cum_growth_real = cumprod(diff_lgdp1))
  damages_i_t4 <- damages_i_t4 %>% dplyr::select(c("ISO3", 
                                                   "year",
                                                   "NY.GDP.PCAP.KD",
                                                   gdp_year,
                                                   variable,
                                                   "diff_lgdp",
                                                   "cum_growth", 
                                                   "cum_adj_growth",
                                                   "iso",
                                                   "phi",
                                                   "cum_adj_growthz",
                                                   "cum_growth_real",
                                                   "realization"))
  damages_i_t4 <- damages_i_t4 %>% dplyr::mutate(damages = ({{gdp_var_year}} * cum_adj_growthz) - 
                                                   ({{gdp_var_year}} * cum_growth_real))
  damages_i_t4 <- damages_i_t4 %>% dplyr::mutate(t_since_k = year - yeari,
                                                 weighted_damages2 = damages*((1+(0.02))^t_since_k),
                                                 weighted_damages3 = damages*((1+(0.03))^t_since_k),
                                                 weighted_damages5 = damages*((1+(0.05))^t_since_k),
                                                 weighted_damages7 = damages*((1+(0.06))^t_since_k))
  
}


# calculating model growth response after adjusting for the delta T by creating 
# a function that takes in the temperature variable, as well as the model used
calc_response_tempnew <- function(temp_var, deltaT, coef1, coef2) {
  response_tempnew <- ((temp_var - deltaT)*(coef1)) +
    (((temp_var - deltaT)^2)*(coef2))
  response_tempnew
}


calc_response_actual <- function(temp_var, coef1, coef2) {
  response_tempactual <- ((temp_var)*(coef1)) +
    (((temp_var)^2)*(coef2)) 
  response_tempactual
}


#read in the gdp/temp dataset to calculate the degrouwth at the country-year 
# level, and recover dG. 
calculate_degrowth_w_uncertainty <- function(delta_dataset, num_boots, status) {
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
  gdp_temp_data1 <- subset(gdp_temp_data, !is.na(diff_lgdp))
  

  ##############################################################################
  ##################### PART II: Save estimates of f()  ########################
  ##############################################################################
  # Run the models to estimate the response function f() using both CRU and ERA 
  #Bootstrap the BHM model for both CRU and ERA
  #CRU
  list_of_cru_bhm <- list()
  for (i in 1:num_boots){
    tic()
    country <- sample(unique(as.character(gdp_temp_data$ISO3)), size = 1, 
                      replace = TRUE, prob = NULL)
    year <- sample(unique(as.character(gdp_temp_data$year)), size = 1, 
                   replace = TRUE, prob = NULL)
    sample <- subset(gdp_temp_data, ISO3 != country & year != as.numeric(year))
    model <- fixest::feols(diff_lgdp ~ cru_mwtemp + cru_mwtemp^2 + cru_mwprecip + 
                            cru_mwprecip^2 + ISO3*as.numeric(year) + ISO3*year2 | 
                            ISO3 + year, sample)
    c <- c(coef(model)[1], coef(model)[2]) 
    list_of_cru_bhm[[i]] <- c
    toc()
  }
  #bring together in a dataframe 
  df_booted_cru <- as.data.frame(do.call(rbind, list_of_cru_bhm))
  #column names 
  colnames(df_booted_cru)[1] <- "coef1"
  colnames(df_booted_cru)[2] <- "coef2"
 
  #create id column 
  df_booted_cru$iteration <- seq.int(nrow(df_booted_cru))
  
  # now join the datasets to calculate the degrowth 
  gdp_temp_data_a <- left_join(df_booted_cru, 
                             gdp_temp_data,
                             by = character())
  
  #ERA
  list_of_era_bhm <- list()
  for (i in 1:num_boots){
    tic()
    country <- sample(unique(as.character(gdp_temp_data$ISO3)), size = 1, 
                      replace = TRUE, prob = NULL)
    year <- sample(unique(as.character(gdp_temp_data$year)), size = 1, 
                   replace = TRUE, prob = NULL)
    sample <- subset(gdp_temp_data, ISO3 != country & year != as.numeric(year))
    model <- fixest::feols(diff_lgdp ~ era_mwtemp + era_mwtemp^2 + era_mwprecip + 
                             era_mwprecip^2 + ISO3*as.numeric(year) + ISO3*year2 | 
                             ISO3 + year, sample)
    c <- c(coef(model)[1], coef(model)[2]) 
    list_of_era_bhm[[i]] <- c
    toc()
  }
  #bring together in a dataframe 
  df_booted_era <- as.data.frame(do.call(rbind, list_of_era_bhm))

  #rename columns
  colnames(df_booted_era)[1] <- "era_coef1"
  colnames(df_booted_era)[2] <- "era_coef2"
  
  #create id column 
  df_booted_era$iteration <- seq.int(nrow(df_booted_era))
  
  # now join the datasets to calculate the degrowth 
  gdp_temp_data <- left_join(gdp_temp_data_a, 
                             df_booted_era,
                             by = c("iteration"))
  
  
  gdp_temp_data$year <- as.numeric(as.character(gdp_temp_data$year))
  
  #join in delaT datasets before calculating dG
  gdp_temp_data <- left_join(gdp_temp_data,
                             delta_dataset,
                             by = c("ISO3", 
                                    "year"))
  
  for (i in list_of_deltaTs) {
    gdp_temp_data$response_tempnew_cru <- calc_response_tempnew(gdp_temp_data$cru_mwtemp,
                                                                gdp_temp_data[i],
                                                                gdp_temp_data$coef1,
                                                                gdp_temp_data$coef2)
    
    gdp_temp_data$response_tempactual_cru <- calc_response_actual(gdp_temp_data$cru_mwtemp,
                                                                  gdp_temp_data$coef1,
                                                                  gdp_temp_data$coef2)
    
    
    gdp_temp_data$delta_g_cru <- gdp_temp_data$response_tempactual_cru - gdp_temp_data$response_tempnew_cru
    
    gdp_temp_data$delta_g_cru <- unlist(gdp_temp_data$delta_g_cru)
    
    new_name <- paste0("tempresp_cru_", i)
    new_name1 <- paste0("dg_cru_", i)
    
    gdp_temp_data$delta_g_cru <- unlist(gdp_temp_data$delta_g_cru)
    
    gdp_temp_data <- gdp_temp_data %>% 
      dplyr::rename(!!new_name := response_tempnew_cru,
                    !!new_name1 := delta_g_cru)
  }
  
  #Now with ERA
  for (i in list_of_deltaTs) {
    gdp_temp_data$response_tempnew_era <- calc_response_tempnew(gdp_temp_data$era_mwtemp,
                                                                gdp_temp_data[i],
                                                                gdp_temp_data$coef1,
                                                                gdp_temp_data$coef2)
    
    gdp_temp_data$response_tempactual_era <- calc_response_actual(gdp_temp_data$era_mwtemp,
                                                                  gdp_temp_data$coef1,
                                                                  gdp_temp_data$coef2)
    
    
    gdp_temp_data$delta_g_era <- gdp_temp_data$response_tempactual_era - gdp_temp_data$response_tempnew_era
    
    
    gdp_temp_data$delta_g_era <- unlist(gdp_temp_data$delta_g_era)
    
    new_name <- paste0("tempresp_era_", i)
    new_name1 <- paste0("dg_era_", i)
    
    gdp_temp_data$delta_g_era <- unlist(gdp_temp_data$delta_g_era)
    
    
    gdp_temp_data <- gdp_temp_data %>% 
      dplyr::rename(!!new_name := response_tempnew_era,
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
      ungroup() %>% 
      dplyr::group_by(ISO3, iteration) %>% 
      dplyr::mutate(gdp_year1 = NY.GDP.PCAP.KD[year == year_i]) 
    
    print("worked")
    
    gdp_temp_data1$adj_growth <- (gdp_temp_data1[i] + gdp_temp_data1$diff_lgdp)
    
    gdp_temp_data1$diff_lgdp <- gdp_temp_data1$diff_lgdp + 1
    
    gdp_temp_data1$adj_growth <- gdp_temp_data1$adj_growth + 1
    
    gdp_temp_data1$diff_lgdp[is.na(gdp_temp_data1$diff_lgdp)] <- 1
    gdp_temp_data1$adj_growth[is.na(gdp_temp_data1$adj_growth)] <- 1
    
    gdp_temp_data1 <- gdp_temp_data1 %>% dplyr::group_by(ISO3, iteration) %>% 
      dplyr::mutate(cum_growth = cumprod(diff_lgdp),
                    cum_adj_growth = cumprod(adj_growth))
    
    
    new_name <- paste0("gdp_cru_", year_i)
    new_name1 <- paste0("damages_cru_", year_i)
    
    gdp_temp_data1 <- gdp_temp_data1 %>% dplyr::rename(!!new_name := gdp_year1)
    #gdp_temp_data1 <- gdp_temp_data1 %>% dplyr::rename(!!new_name1 := damages)
    
    write_rds(gdp_temp_data1, paste0("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/damages_", 
                                     status,"_cru_", year_i, ".rds"))
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
      dplyr::group_by(ISO3, iteration) %>% 
      dplyr::mutate(gdp_year1 = NY.GDP.PCAP.KD[year == year_i])
    
    print("worked")
    
    gdp_temp_data1$adj_growth <- (gdp_temp_data1[i] + gdp_temp_data1$diff_lgdp)
    
    gdp_temp_data1$diff_lgdp <- gdp_temp_data1$diff_lgdp + 1
    gdp_temp_data1$adj_growth <- gdp_temp_data1$adj_growth + 1
    
    gdp_temp_data1$diff_lgdp[is.na(gdp_temp_data1$diff_lgdp)] <- 1
    gdp_temp_data1$adj_growth[is.na(gdp_temp_data1$adj_growth)] <- 1
    
    gdp_temp_data1 <- gdp_temp_data1 %>% dplyr::group_by(ISO3, iteration) %>% 
      dplyr::mutate(cum_growth = cumprod(diff_lgdp),
                    cum_adj_growth = cumprod(adj_growth))
    
    new_name <- paste0("gdp_era_", year_i)
    #new_name1 <- paste0("damages_era_", year_i)
    
    gdp_temp_data1 <- gdp_temp_data1 %>% dplyr::rename(!!new_name := gdp_year1)
    #gdp_temp_data1 <- gdp_temp_data1 %>% dplyr::rename(!!new_name1 := damages)
    
    write_rds(gdp_temp_data1,paste0("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/damages_", 
                                    status,"_era_", year_i, ".rds"))
  }
  
  return(gdp_temp_data)
}


#function for calcing damages 
calc_damages_all_bhm_iterations <- function(
                                       emitter,
                                       reciever,
                                       dataset_name,
                                       dataset_fair,
                                       dg_variable,
                                       gdp_year, 
                                       gdp_var_year, 
                                       variable) {
  dataset_dga <- readRDS(paste0("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/", dataset_name, ".rds"))
  yeari <- as.numeric(substr(gdp_year, 9, 12))
  dataset_dg <- dataset_dga %>% dplyr::mutate(test_id = 1,
                                              year = as.numeric(year)) %>% 
    subset(ISO3 == emitter | ISO3 == reciever)
  dataset_fair <- dataset_fair %>% dplyr::mutate(test_id = 1) %>% 
    subset(iso == emitter | iso == reciever)
  damages_i_t <- left_join(dataset_dg,
                           dataset_fair,
                           by = c("year", 
                                  "test_id"))
  damages_i_t$diff_lgdp <- damages_i_t$diff_lgdp - 1
  damages_i_t$diff_lgdp[damages_i_t$diff_lgdp == 0.000] <- NA
  damages_i_t1 <- damages_i_t %>% 
    dplyr::mutate(dg_cru_deltaT = dg_cru_deltaT_1980 * phi) 
  damages_i_t2 <- damages_i_t1 %>% 
    dplyr::mutate(adj_growthx = dg_cru_deltaT + diff_lgdp)
  damages_i_t3 <- damages_i_t2 %>% 
    dplyr::mutate(adj_growthxz = adj_growthx + 1)
  damages_i_t3$diff_lgdp1 <- damages_i_t3$diff_lgdp +1
  damages_i_t4 <- damages_i_t3 %>% dplyr::group_by(ISO3, iso, iteration) %>% 
    dplyr::mutate(cum_adj_growthz = cumprod(adj_growthxz),
                  cum_growth_real = cumprod(diff_lgdp1))
  damages_i_t4 <- damages_i_t4 %>% dplyr::select(c("ISO3", 
                                                   "year",
                                                   "NY.GDP.PCAP.KD",
                                                   gdp_year,
                                                   variable,
                                                   "diff_lgdp",
                                                   "cum_growth", 
                                                   "cum_adj_growth",
                                                   "iso",
                                                   "phi",
                                                   "cum_adj_growthz",
                                                   "cum_growth_real",
                                                   "iteration"))
  damages_i_t4 <- damages_i_t4 %>% dplyr::mutate(damages = ({{gdp_var_year}} * cum_adj_growthz) - 
                                                   ({{gdp_var_year}} * cum_growth_real))
  damages_i_t4 <- damages_i_t4 %>% dplyr::mutate(t_since_k = year - yeari,
                                                 weighted_damages2 = damages*((1+(0.02))^t_since_k),
                                                 weighted_damages3 = damages*((1+(0.03))^t_since_k),
                                                 weighted_damages5 = damages*((1+(0.05))^t_since_k),
                                                 weighted_damages7 = damages*((1+(0.06))^t_since_k))
}





visualize_uncertainty <- function(dataset1,
                                  dataset2,
                                  dataset3,
                                  dataset4,
                                  isoi) {
  one <- dataset1 %>%  dplyr::select(c("ISO3",
                                       "iso",
                                       "year",
                                       "realization",
                                       "weighted_damages2")) %>% 
    subset(ISO3 == isoi & iso == "USA")
  one <- one %>% dplyr::group_by(ISO3, iso, realization) %>% 
    dplyr::summarise(total_damages = sum(weighted_damages2),
                     .groups = "keep")
  
  two <- dataset2 %>%  dplyr::select(c("ISO3",
                                       "iso",
                                       "year",
                                       "realization",
                                       "weighted_damages2")) %>% 
    subset(ISO3 == isoi & iso == "USA")
  two <- two %>% dplyr::group_by(ISO3, iso, realization) %>% 
    dplyr::summarise(total_damages = sum(weighted_damages2),
                     .groups = "keep")
  
  
  
  three <- dataset3 %>%  dplyr::select(c("ISO3", 
                                         "iso",
                                         "year",
                                         "iteration",
                                         "weighted_damages2")) %>% 
    subset(ISO3 == isoi & iso == "USA")
  three <- three %>% dplyr::group_by(ISO3, iso, iteration) %>% 
    dplyr::summarise(total_damages = sum(weighted_damages2),
                     .groups = "keep")
  
  
  
  four <- dataset4 %>%  dplyr::select(c("ISO3", 
                                        "iso",
                                        "loop",
                                        "total_damages")) %>% 
    subset(ISO3 == isoi & iso == "USA")
  
  
  one$uncertainty <- "a) Damages Across CMIP6 Ensembles"   
  four$uncertainty <- "b) Damages Under Varying FAIR Model Inputs"
  two$uncertainty <- "c) Damages Across MIROC6 Runs"
  three$uncertainty <- "d) Damages Under Bootstrapped BHM Model"
  
  
  all <- rbind.fill(one, 
                    two,
                    three,
                    four)
  
  
  all <- left_join(all, 
                   pop,
                   by = c("ISO3" = "iso3c"))
  
  all$total_damages <- all$total_damages * (-1)
  all$total_damages <- all$total_damages * all$SP.POP.TOTL
  
  return(all)
  
}

# end of script
