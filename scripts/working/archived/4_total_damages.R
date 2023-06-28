##############################################################################
# Mustafa Zahid, April 1st, 2022
# This script calculates the total damages owed by emitter country j to receiver 
# country i 
#############################################################################

# calculating damages owed by each country to another. It takes in arguments 
# for the warming ratio used, as well as the dataframe from FaIR temperature 
# response and the list of emitters (list_of_exps)
calculate_bidamages <- function(ratio_raster, experiment_df, list_of_exps, 
                                year_k, future_forecast){
  #read raster data for warming ratio 
  deltat_df <- as.data.frame(as.matrix(rasterToPoints(ratio_raster)))
  colnames(deltat_df)[3] <- "layer"
  
  # assign merge if to merge with fair data
  deltat_df$merge_id <- "1"
  
  # now assign the experiment data frame FROM FAIR
  fair_exps <- experiment_df 
  #fair_exps <- subset(fair_exps, num_loop == num_loop)
  #assign merge id 
  fair_exps$merge_id <- "1"

  fair_exps <- subset(fair_exps, !is.na(experiment_iso))
  # now bring them together
  deltat_calced_df <- left_join(deltat_df,
                                fair_exps,
                                by = c("merge_id"))

  
  # now multuply the grid level warming ratio by median temp response from FaIR
  deltat_calced_df$deltat <- deltat_calced_df$layer * deltat_calced_df$median_deltat
  
  # now let us take the columns we need from the fair experiment dataframe 
  deltaT_df <- deltat_calced_df %>% dplyr::select(c("x",
                                                    "y",
                                                    "deltat",
                                                    "year",
                                                    "experiment_iso"))
  
  # we need to aggregate the deltat to the country level by weighting by pop
  # so let us read the pop raster and resample it to match coordinates and 
  # convert to a dataframe and then join
  pop <- raster("~/BurkeLab Dropbox/Projects/loss_damage/data/raw/population/gpw_v4_population_count_rev11_2010_1_deg.tif")
  pop <- readAll(pop)
  pop <- resample(pop, ratio_raster)
  popdf <- as.data.frame(as.matrix(rasterToPoints(pop)))
  
  # the other thing we need is to get a country id for each lon-lat combo
  data("wrld_simpl")
  world <- wrld_simpl[,3]
  world <- st_as_sf(world)
  
  # now join base coordaintes with country ids and remove NAs
  base_coords <- deltat_df %>% dplyr::select(c("x", "y"))
  base_coords <- st_as_sf(base_coords, coords = c("x", "y"), 
                          crs = crs(ratio_raster))
  base_coords <- st_join(base_coords,world)
  base_coords <- base_coords %>%
    mutate(x = unlist(map(base_coords$geometry,1)),
           y = unlist(map(base_coords$geometry,2)))
  st_geometry(base_coords) <- NULL 
  base_coords <- subset(base_coords, !is.na(ISO3))
  
  # now let us get a unique dataframe of all experiemtns (i.e. countries
  # - shutting off emissions)
  #deltaT_df <- subset(deltaT_df, !is.na(experiment_iso))
  #fair_experiment_iso <- unique(deltaT_df$experiment_iso)
  pop <- wdi_dat %>% dplyr::select(c("iso3c", "SP.POP.TOTL", "year")) 
  
  # calculating model growth response after adjusting for the delta T by creating 
  # a function that takes in the temperature variable, as well as the model used
  calc_delta_g <- function(dataset, temp_var, model, deltaT, coef1, coef2) {
    response_tempnew <- ((temp_var - deltaT)*(coef1)) +
      (((temp_var - deltaT)^2)*(coef2)) 
    response_tempnew
  }
  i <- "all"
  # start an empty dataframe 
  mother_df <- data.frame()
  # now loop over the experiments and calculate total damages owed by each of 
  # the countries
  for (i in list_of_exps){
    tic()
    # subset by keeping one experiment for each loop
    deltaT_df1 <- subset(deltaT_df, experiment_iso == i)
    # merge with coordinates and country ids
    joined_final_df <- left_join(deltaT_df1,
                                 base_coords,
                                 by = c("x", "y"))
    
    # keep non missing ISOs exoeriments
    joined_final_df <- subset(joined_final_df, !is.na(ISO3))
    # keep to a certain year
    #joined_final_df <- subset(joined_final_df, year < 2021)
    
    # now let us calculate the average deltaT by country-year
    joined_final_df <- joined_final_df %>% dplyr::group_by(ISO3, year) %>% 
      dplyr::summarise(deltat = mean(deltat, na.rm = T),
                       .groups = "keep")
    
    # now let us get the country-year panel
    gdp_temp_data1 <- gdp_temp_data
    gdp_temp_data1$year <- as.numeric(as.character(gdp_temp_data1$year))
    
    #gdp_temp_data1 <- subset(gdp_temp_data1, year >= 1980)
    # join country-year temp change with the country-year panel
    gdp_temp_data1 <- left_join(joined_final_df,
                                gdp_temp_data1,
                                by = c("ISO3", "year"))
    
  #  gdp_temp_data1$adjusted_temp <- gdp_temp_data1$era_mwtemp - gdp_temp_data1$deltat
    
   # dev.off()
  #  par(mfrow = c(1,1))
    
  #  hist(gdp_temp_data1$era_mwtemp, add =T)
  #  hist(gdp_temp_data1$adjusted_temp, col = "red", add = T)
    # calculate reposnse from added temperature
    gdp_temp_data1$response_tempnew <- calc_delta_g(gdp_temp_data1,
                                                    gdp_temp_data1$era_mwtemp,
                                                   bhm_era_reg,
                                                   gdp_temp_data1$deltat,
                                                   coef(bhm_era_reg)[1],
                                                   coef(bhm_era_reg)[2])
    
    # now let us calculate deltaG
    gdp_temp_data1$delta_g_era <- gdp_temp_data1$response_tempactual_era - gdp_temp_data1$response_tempnew
    
   
  # test1 <- gdp_temp_data1 %>% dplyr::select(c("response_tempactual_era", "response_tempnew","delta_g_era", "ISO3", "year"))
    # let us bring in the population data at the country-year level
    gdp_temp_data1 <- left_join(gdp_temp_data1,
                               pop_wdi,
                               by = c("ISO3" = "iso3c",
                                      "year" = "year"))
    
    gdp_temp_data1 <- gdp_temp_data1 %>% 
      dplyr::mutate(SP.POP.TOTL = case_when(is.na(SP.POP.TOTL) ~ pop,
                                            TRUE ~ SP.POP.TOTL))

    # unlist...
    gdp_temp_data1$delta_g_era <- unlist(gdp_temp_data1$delta_g_era)
    
    # now let us calculate adjusted growht rate by adding deltaG to observed growth
    gdp_temp_data1$adj_growth <- (gdp_temp_data1$delta_g_era + gdp_temp_data1$diff_lgdp)
    
    # let us add 1 to growth variables so we can calculate cumulative growth
    gdp_temp_data1$diff_lgdp <- gdp_temp_data1$diff_lgdp + 1
    gdp_temp_data1$adj_growth <- gdp_temp_data1$adj_growth + 1
    
    # let us set missing NAs to 1 so we can run the script without breaking 
    # becasue of NAs
    gdp_temp_data1$diff_lgdp[is.na(gdp_temp_data1$diff_lgdp)] <- 1
    gdp_temp_data1$adj_growth[is.na(gdp_temp_data1$adj_growth)] <- 1
    
    # we now want to set 0 growth to NA
    gdp_temp_data1$diff_lgdp <- gdp_temp_data1$diff_lgdp - 1
    gdp_temp_data1$diff_lgdp[gdp_temp_data1$diff_lgdp == 0.000] <- NA
    # then let us calculate adjusted growth rate
    gdp_temp_data1 <- gdp_temp_data1 %>% 
      dplyr::mutate(adj_growthx = delta_g_era + diff_lgdp)
    # now let us add 1 so that we can calculate cumulative growth rate 
    damages_i_t3 <- gdp_temp_data1 %>% 
      dplyr::mutate(adj_growthxz = adj_growthx + 1)
    # now let us set GDP at year k as initial gdp number to calculate impact of 
    # added temoerature
    damages_i_t3 <- damages_i_t3 %>% dplyr::group_by(ISO3) %>% 
      dplyr::mutate(gdp_year = NY.GDP.PCAP.KD[year == year_k])
    
    # now let us add 1 to observed growth so we can calculate cumulative
    # gdp growth
    damages_i_t3$diff_lgdp1 <- damages_i_t3$diff_lgdp +1
    # now let us calculate cumulative GDP growth
    damages_i_t4 <- damages_i_t3 %>% dplyr::group_by(ISO3) %>% 
      dplyr::mutate(cum_adj_growthz = cumprod(adj_growthxz),
                    cum_growth_real = cumprod(diff_lgdp1))
    # finally comput edamages...
    damages_i_t4 <- damages_i_t4 %>% 
      dplyr::mutate(damages = (gdp_year * cum_adj_growthz) - (gdp_year * cum_growth_real))
    
    #summary(damages_i_t4$damages)
   
    # scale by population
    damages_i_t4$damages_pop <- damages_i_t4$damages * damages_i_t4$SP.POP.TOTL
    
    # calculate discounted damages for past and for future (note different 
    #- processes for discounting past damages and future damages)
    damages_i_t4 <- damages_i_t4 %>% 
      dplyr::mutate(t_since_k = year - year_k,
                    t_since_today = year - 2020,
                    weighted_damages2 = case_when(year <= 2020 ~ (damages_pop*((1+(0.02))^t_since_k)),
                                                  year > 2020 ~ (damages_pop*(1/(1+(0.02))^t_since_today))),
                    weighted_damages3 = case_when(year <= 2020 ~ (damages_pop*((1+(0.03))^t_since_k)),
                                                  year > 2020 ~ (damages_pop*(1/(1+(0.03))^t_since_today))),
                    weighted_damages5 = case_when(year <= 2020 ~ (damages_pop*((1+(0.05))^t_since_k)),
                                                  year > 2020 ~ (damages_pop*(1/(1+(0.05))^t_since_today))),
                    weighted_damages7 = case_when(year <= 2020 ~ (damages_pop*((1+(0.07))^t_since_k)),
                                                  year > 2020 ~ (damages_pop*(1/(1+(0.07))^t_since_today))))
    
    # keep pnly negative damages
    damages_neg <- subset(damages_i_t4, damages_pop < 0)
    #sum(damages_neg$weighted_damages2, na.rm = T)
    #sum(damages_i_t4$weighted_damages2, na.rm = T)
    #damages_neg$status <- paste0("data_", i)
    #damages_i_t4$status <- paste0("data_", i)
    # name th emitter
    damages_i_t4$emitter <- i
   
    #damages_i_t4$emitter <- i
    #test_dfs[[i]] <- damages_neg
    # add to data frame
    mother_df <- rbind(mother_df, damages_i_t4)
    toc()
  }
  return(mother_df)

}



run_bhm_model <- function(bhm_mode, future_forecast){
  
  bhm_mode_option <- bhm_mode

  gdp_temp_data <- readRDS("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/temp_gdp_world_panel_102822.rds")
  
  #sum(gdp_temp_data$NY.GDP.PCAP.KD[gdp_temp_data$year == 2020]*gdp_temp_data$SP.POP.TOTL[gdp_temp_data$year == 2020], na.rm = T)
  
  #gdp_temp_data1 <- left_join(identifier, gdp_temp_data)
  
  #sum(gdp_temp_data1$NY.GDP.PCAP.KD[gdp_temp_data1$year == 2020]*gdp_temp_data1$SP.POP.TOTL[gdp_temp_data1$year == 2020], na.rm = T) / sum(gdp_temp_data$NY.GDP.PCAP.KD[gdp_temp_data$year == 2020]*gdp_temp_data$SP.POP.TOTL[gdp_temp_data$year == 2020], na.rm = T)
  
  
  gdp_temp_data <- subset(gdp_temp_data, year < 2020)
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
    # Now let us subset teh dataset to teh data that we will need
    gdp_temp_data <- gdp_temp_data %>% dplyr::select(c("ISO3",
                                                       "year",
                                                       "era_mwtemp",
                                                       "era_mwprecip",
                                                       "era_mwtemp",
                                                       "era_mwprecip",
                                                       "diff_lgdp",
                                                       "year2",
#                                                       "NY.GDP.PCAP.PP.KD",
                                                       "NY.GDP.PCAP.KD"))
    
    #era
    bhm_era_reg <-  fixest::feols(diff_lgdp ~ era_mwtemp + era_mwtemp^2 + era_mwprecip + 
                                    era_mwprecip^2 + ISO3*as.numeric(year) + ISO3*year2 | 
                                    ISO3 + year, gdp_temp_data)
    #estimate the response for every country-year
    gdp_temp_data <- rbind.fill(gdp_temp_data,
                               future_forecast)
    
    #gdp_temp_data <- left_join(gdp_temp_data,
     #                          ssp_gdp_pop,
      #                         by = c("ISO3", "year"))
    
   # gdp_temp_data <- gdp_temp_data %>% 
    
    #  dplyr::mutate(era_mwtemp = case_when(!is.na(era_mwtemp.x) ~ era_mwtemp.x,
     #                                      !is.na(era_mwtemp.y) ~ era_mwtemp.y))
    
    
    gdp_temp_data$response_tempactual_era <- ((gdp_temp_data$era_mwtemp)*(coef(bhm_era_reg)[1])) +
      ((gdp_temp_data$era_mwtemp^2)*(coef(bhm_era_reg)[2]))
  }
    else if (bhm_mode_option == "richpoor") {
      gdp_temp_data <- gdp_temp_data %>% dplyr::select(c("ISO3",
                                                         "year",
                                                         "era_mwtemp",
                                                         "era_mwprecip",
                                                         "era_mwtemp",
                                                         "era_mwprecip",
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
      # Run the models to estimate the response function f() using both era and ERA 
      
      #era
      bhm_era_reg <-  fixest::feols(diff_lgdp ~ era_mwtemp + era_mwtemp^2 + (poor*era_mwtemp) + (poor*era_mwtemp^2) + 
                                      era_mwprecip + era_mwprecip^2 + (poor*era_mwprecip) + (poor*era_mwprecip^2) + 
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
      
      gdp_temp_data$year <- as.numeric(as.character(gdp_temp_data$year))
      
    #  gdp_temp_data <- gdp_temp_data %>% 
     #   dplyr::mutate(era_mwtemp = case_when(!is.na(era_mwtemp.x) ~ era_mwtemp.x,
      #                                       !is.na(era_mwtemp.y) ~ era_mwtemp.y))
      
      
      
      gdp_temp_data <- gdp_temp_data %>% dplyr::mutate(response_tempactual_era = case_when(
        poor == 1 ~ ((gdp_temp_data$era_mwtemp)*(coef(bhm_era_reg)[1]) + (gdp_temp_data$era_mwtemp^2)*(coef(bhm_era_reg)[2]))+              
          (((gdp_temp_data$era_mwtemp)*(coef(bhm_era_reg)[5])) + (gdp_temp_data$era_mwtemp^2)*(coef(bhm_era_reg)[6])),
        poor == 0 ~ ((gdp_temp_data$era_mwtemp)*(coef(bhm_era_reg)[1])) +
          ((gdp_temp_data$era_mwtemp^2)*(coef(bhm_era_reg)[2]))))
    }
  return(gdp_temp_data)
}

run_bhm_model_reg <- function(bhm_mode){
  
  bhm_mode_option <- bhm_mode
  
  gdp_temp_data <- readRDS("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/temp_gdp_world_panel_102822.rds")
  gdp_temp_data <- subset(gdp_temp_data, year < 2020)
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
    # Now let us subset teh dataset to teh data that we will need
    gdp_temp_data <- gdp_temp_data %>% dplyr::select(c("ISO3",
                                                       "year",
                                                       "era_mwtemp",
                                                       "era_mwprecip",
                                                       "era_mwtemp",
                                                       "era_mwprecip",
                                                       "diff_lgdp",
                                                       "year2",
                                                #       "NY.GDP.PCAP.PP.KD",
                                                       "NY.GDP.PCAP.KD"))
    
    #era
    bhm_era_reg <-  fixest::feols(diff_lgdp ~ era_mwtemp + era_mwtemp^2 + era_mwprecip + 
                                    era_mwprecip^2 + ISO3*as.numeric(year) + ISO3*year2 | 
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
                                                       "era_mwprecip",
                                                       "diff_lgdp",
                                                       "year2",
                                                 #      "NY.GDP.PCAP.PP.KD",
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
    # Run the models to estimate the response function f() using both era and ERA 
    
    #era
    bhm_era_reg <-  fixest::feols(diff_lgdp ~ era_mwtemp + era_mwtemp^2 + (poor*era_mwtemp) + (poor*era_mwtemp^2) + 
                                    era_mwprecip + era_mwprecip^2 + (poor*era_mwprecip) + (poor*era_mwprecip^2) + 
                                    + poor + ISO3*as.numeric(year) + ISO3*year2 | 
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



#now let's write the rich/poor separate response functions 
calc_delta_g_richpoor <- function(dataset, temp_var, model, deltaT, interact) {
  response_tempnew <- ((temp_var - deltaT)*(coef(model)[1])) +
    (((temp_var - deltaT)^2)*(coef(model)[2])) + (interact*(temp_var-deltaT)*(coef(model)[5])) +
    (interact*((temp_var - deltaT)^2)*(coef(model)[6]))
  response_tempnew
}

calculate_bidamages_richpoor <- function(ratio_raster, experiment_df, list_of_exps, year_k){
  deltat_df <- as.data.frame(as.matrix(rasterToPoints(ratio_raster)))
  # assign merge if to merge with fair data
  deltat_df$merge_id <- "1"
  
  # now assign the experiment data frame FROM FAIR
  fair_exps <- experiment_df 
  #assign merge id 
  fair_exps$merge_id <- "1"
  
  fair_exps <- subset(fair_exps, !is.na(experiment_iso))
  
  # now bring them together
  deltat_calced_df <- left_join(deltat_df,
                                fair_exps,
                                by = c("merge_id"))
  
  # now multuply the grid level warming ratio 
  deltat_calced_df$deltat <- deltat_calced_df$raster_deltat_ratio_cmip6 * deltat_calced_df$median_deltat
  
  # now let us take the columns we need from the fair experiment dataframe 
  deltaT_df <- deltat_calced_df %>% dplyr::select(c("x",
                                                    "y",
                                                    "deltat",
                                                    "year",
                                                    "experiment_iso"))
  
  # we need to aggregate the deltat to the country level by weighting by pop
  # so let us read the pop raster and resample it to match coordinates and 
  # convert to a dataframe and then join
  pop <- raster("~/BurkeLab Dropbox/Projects/loss_damage/data/raw/population/gpw_v4_population_count_rev11_2010_1_deg.tif")
  pop <- readAll(pop)
  pop <- resample(pop, raster_cgm_ratio)
  popdf <- as.data.frame(as.matrix(rasterToPoints(pop)))
  
  # the other thing we need is to get a country id for each lon-lat combo
  data("wrld_simpl")
  world <- wrld_simpl[,3]
  world <- st_as_sf(world)
  
  # now join base coordaintes with country ids and remove NAs
  base_coords <- deltat_df %>% dplyr::select(c("x", "y"))
  base_coords <- st_as_sf(base_coords, coords = c("x", "y"), 
                          crs = crs(raster_cgm_ratio))
  base_coords <- st_join(base_coords,world)
  base_coords <- base_coords %>%
    mutate(x = unlist(map(base_coords$geometry,1)),
           y = unlist(map(base_coords$geometry,2)))
  st_geometry(base_coords) <- NULL 
  base_coords <- subset(base_coords, !is.na(ISO3))
  
  # now let us get a unique dataframe of all experiemtns (i.e. countries
  # - shutting off emissions)
  #fair_experiment_iso <- unique(deltaT_df$experiment_iso)
  pop <- wdi_dat %>% dplyr::select(c("iso3c", "SP.POP.TOTL", "year")) 
  
  # start an empty dataframe 
  mother_df <- data.frame()
  # now loop over the experiments and calculate total damages owed by each of 
  # the countries
  for (i in list_of_exps){
    tic()
    # example 
    deltaT_df1 <- subset(deltaT_df, experiment_iso == i)
    joined_final_df <- left_join(deltaT_df1,
                                 base_coords,
                                 by = c("x", "y"))
    
    joined_final_df <- subset(joined_final_df, !is.na(ISO3))
    #joined_final_df <- subset(joined_final_df, year < 2021)
    
    joined_final_df <- joined_final_df %>% dplyr::group_by(ISO3, year) %>% 
      dplyr::summarise(deltat = mean(deltat, na.rm = T),
                       .groups = "keep")
    
    # Merge both datasets
    gdp_temp_data1 <- gdp_temp_data
    gdp_temp_data1$year <- as.numeric(as.character(gdp_temp_data1$year))
    gdp_temp_data1 <- left_join(joined_final_df,
                               gdp_temp_data1,
                               by = c("ISO3", "year"))
    
 
    gdp_temp_data1$response_tempnew <- calc_delta_g_richpoor(gdp_temp_data1,
                                                             gdp_temp_data1$era_mwtemp,
                                                            bhm_era_reg,
                                                            gdp_temp_data1$deltat,
                                                            gdp_temp_data1$poor)
    
    gdp_temp_data1$delta_g_era <- gdp_temp_data1$response_tempactual_era - 
      gdp_temp_data1$response_tempnew
    
    gdp_temp_data1$delta_g_era <- unlist(gdp_temp_data1$delta_g_era)
   
    gdp_temp_data1 <- left_join(gdp_temp_data1,
                               pop_wdi,
                             by = c("ISO3" = "iso3c",
                                      "year" = "year"))
    

    gdp_temp_data1$delta_g_era <- unlist(gdp_temp_data1$delta_g_era)
    
    gdp_temp_data1$adj_growth <- (gdp_temp_data1$delta_g_era + gdp_temp_data1$diff_lgdp)
    
    gdp_temp_data1$diff_lgdp <- gdp_temp_data1$diff_lgdp + 1
    
    gdp_temp_data1$adj_growth <- gdp_temp_data1$adj_growth + 1
    
    gdp_temp_data1$diff_lgdp[is.na(gdp_temp_data1$diff_lgdp)] <- 1
    gdp_temp_data1$adj_growth[is.na(gdp_temp_data1$adj_growth)] <- 1
    
    
    gdp_temp_data1$diff_lgdp <- gdp_temp_data1$diff_lgdp - 1
    gdp_temp_data1$diff_lgdp[gdp_temp_data1$diff_lgdp == 0.000] <- NA
    gdp_temp_data1 <- gdp_temp_data1 %>% 
      dplyr::mutate(adj_growthx = delta_g_era + diff_lgdp)
    damages_i_t3 <- gdp_temp_data1 %>% 
      dplyr::mutate(adj_growthxz = adj_growthx + 1)
    damages_i_t3 <- damages_i_t3 %>% dplyr::group_by(ISO3) %>% 
      dplyr::mutate(gdp_year = NY.GDP.PCAP.KD[year == year_k])
    damages_i_t3$diff_lgdp1 <- damages_i_t3$diff_lgdp +1
    damages_i_t4 <- damages_i_t3 %>% dplyr::group_by(ISO3) %>% 
      dplyr::mutate(cum_adj_growthz = cumprod(adj_growthxz),
                    cum_growth_real = cumprod(diff_lgdp1))
    damages_i_t4 <- damages_i_t4 %>% dplyr::mutate(damages = (gdp_year * cum_adj_growthz) - 
                                                     (gdp_year * cum_growth_real))
    damages_i_t4$damages_pop <- damages_i_t4$damages * damages_i_t4$SP.POP.TOTL
    damages_i_t4 <- damages_i_t4 %>% 
      dplyr::mutate(t_since_k = year - year_k,
                    t_since_today = 2020 - year,
                    weighted_damages2 = case_when(year <= 2020 ~ (damages_pop*((1+(0.02))^t_since_k)),
                                                year > 2020 ~ (damages_pop*(1/(1+(0.02))^t_since_today))),
                    weighted_damages3 = case_when(year <= 2020 ~ (damages_pop*((1+(0.03))^t_since_k)),
                                                  year > 2020 ~ (damages_pop*(1/(1+(0.03))^t_since_today))),
                    weighted_damages5 = case_when(year <= 2020 ~ (damages_pop*((1+(0.05))^t_since_k)),
                                                  year > 2020 ~ (damages_pop*(1/(1+(0.05))^t_since_today))),
                    weighted_damages7 = case_when(year <= 2020 ~ (damages_pop*((1+(0.07))^t_since_k)),
                                                  year > 2020 ~ (damages_pop*(1/(1+(0.07))^t_since_today))))
    damages_neg <- subset(damages_i_t4, damages_pop < 0)
    #sum(damages_neg$weighted_damages2, na.rm = T)
    damages_i_t4$status <- paste0("data_", i)
    damages_i_t4$emitter <- i
    mother_df <- rbind(mother_df, damages_i_t4)
    toc()
  }
  return(mother_df)
}


#end of script






run_bhm_model <- function(bhm_mode, future_forecast, year_k){
  
  bhm_mode_option <- bhm_mode
  
  gdp_temp_data <- readRDS("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/temp_gdp_world_panel.rds")
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
    # Now let us subset teh dataset to teh data that we will need
    gdp_temp_data <- gdp_temp_data %>% dplyr::select(c("ISO3",
                                                       "year",
                                                       "era_mwtemp",
                                                       "era_mwprecip",
                                                       "era_mwtemp",
                                                       "era_mwprecip",
                                                       "diff_lgdp",
                                                       "year2",
                                                       "NY.GDP.PCAP.PP.KD",
                                                       "NY.GDP.PCAP.KD"))
    
    #era
#    bhm_era_reg <-  fixest::feols(diff_lgdp ~ era_mwtemp + era_mwtemp^2 + era_mwprecip + 
 #                                   era_mwprecip^2 + ISO3*as.numeric(year) + ISO3*year2 | 
  #                                  ISO3 + year, gdp_temp_data)
    #estimate the response for every country-year
    gdp_temp_data <- rbind.fill(gdp_temp_data,
                                future_forecast)
    
    #gdp_temp_data <- left_join(gdp_temp_data,
    #                          ssp_gdp_pop,
    #                         by = c("ISO3", "year"))
    
    # gdp_temp_data <- gdp_temp_data %>% 
    
    #  dplyr::mutate(era_mwtemp = case_when(!is.na(era_mwtemp.x) ~ era_mwtemp.x,
    #                                      !is.na(era_mwtemp.y) ~ era_mwtemp.y))
    
    gdp_temp_data$merge_id <- 1
    pooledbs$merge_id <- 1
    pooledbs$coef_id <- 1:nrow(pooledbs)
    
    gdp_temp_data <- left_join(gdp_temp_data,
                               pooledbs)
    
    gdp_temp_data <- subset(gdp_temp_data, year >= year_k)
    
    gdp_temp_data$response_tempactual_era <- ((gdp_temp_data$era_mwtemp)*(gdp_temp_data$temp)) +
      ((gdp_temp_data$era_mwtemp^2)*(gdp_temp_data$temp2))
    
    gdp_temp_data <- subset(gdp_temp_data, !is.na(coef_id))
    
    gdp_temp_data$year <- as.numeric(as.character(gdp_temp_data$year))
  }
  else if (bhm_mode_option == "richpoor") {
    gdp_temp_data <- gdp_temp_data %>% dplyr::select(c("ISO3",
                                                       "year",
                                                       "era_mwtemp",
                                                       "era_mwprecip",
                                                       "era_mwtemp",
                                                       "era_mwprecip",
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
    # Run the models to estimate the response function f() using both era and ERA 
    
    #era
    bhm_era_reg <-  fixest::feols(diff_lgdp ~ era_mwtemp + era_mwtemp^2 + (poor*era_mwtemp) + (poor*era_mwtemp^2) + 
                                    era_mwprecip + era_mwprecip^2 + (poor*era_mwprecip) + (poor*era_mwprecip^2) + 
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
    #   dplyr::mutate(era_mwtemp = case_when(!is.na(era_mwtemp.x) ~ era_mwtemp.x,
    #                                       !is.na(era_mwtemp.y) ~ era_mwtemp.y))
    
    
    
    gdp_temp_data <- gdp_temp_data %>% dplyr::mutate(response_tempactual_era = case_when(
      poor == 1 ~ ((gdp_temp_data$era_mwtemp)*(coef(bhm_era_reg)[1]) + (gdp_temp_data$era_mwtemp^2)*(coef(bhm_era_reg)[2]))+              
        (((gdp_temp_data$era_mwtemp)*(coef(bhm_era_reg)[5])) + (gdp_temp_data$era_mwtemp^2)*(coef(bhm_era_reg)[6])),
      poor == 0 ~ ((gdp_temp_data$era_mwtemp)*(coef(bhm_era_reg)[1])) +
        ((gdp_temp_data$era_mwtemp^2)*(coef(bhm_era_reg)[2]))))
  }
  return(gdp_temp_data)
}

# calculating damages owed by each country to another. It takes in arguments 
# for the warming ratio used, as well as the dataframe from FaIR temperature 
# response and the list of emitters (list_of_exps)
calculate_bidamages <- function(ratio_raster, experiment_df, list_of_exps, 
                                year_k, future_forecast){
  tic()
  #read raster data for warming ratio 
  deltat_df <- as.data.frame(as.matrix(rasterToPoints(ratio_raster)))
  colnames(deltat_df)[3] <- "layer"
  
  # assign merge if to merge with fair data
  deltat_df$merge_id <- "1"
  
  # now assign the experiment data frame FROM FAIR
  fair_exps <- experiment_df 
  #fair_exps <- subset(fair_exps, num_loop == num_loop)
  #assign merge id 
  fair_exps$merge_id <- "1"
  
  fair_exps <- subset(fair_exps, !is.na(experiment_iso))
  # now bring them together
  deltat_calced_df <- left_join(deltat_df,
                                fair_exps,
                                by = c("merge_id"))
  
  # now multuply the grid level warming ratio by median temp response from FaIR
  deltat_calced_df$deltat <- deltat_calced_df$layer * deltat_calced_df$median_deltat
  
  # now let us take the columns we need from the fair experiment dataframe 
  deltaT_df <- deltat_calced_df %>% dplyr::select(c("x",
                                                    "y",
                                                    "deltat",
                                                    "year",
                                                    "experiment_iso"))
  
  # we need to aggregate the deltat to the country level by weighting by pop
  # so let us read the pop raster and resample it to match coordinates and 
  # convert to a dataframe and then join
  pop <- raster("~/BurkeLab Dropbox/Projects/loss_damage/data/raw/population/gpw_v4_population_count_rev11_2010_1_deg.tif")
  pop <- readAll(pop)
  pop <- resample(pop, ratio_raster)
  popdf <- as.data.frame(as.matrix(rasterToPoints(pop)))
  
  # the other thing we need is to get a country id for each lon-lat combo
  data("wrld_simpl")
  world <- wrld_simpl[,3]
  world <- st_as_sf(world)
  
  # now join base coordaintes with country ids and remove NAs
  base_coords <- deltat_df %>% dplyr::select(c("x", "y"))
  base_coords <- st_as_sf(base_coords, coords = c("x", "y"), 
                          crs = crs(ratio_raster))
  base_coords <- st_join(base_coords,world)
  base_coords <- base_coords %>%
    mutate(x = unlist(map(base_coords$geometry,1)),
           y = unlist(map(base_coords$geometry,2)))
  st_geometry(base_coords) <- NULL 
  base_coords <- subset(base_coords, !is.na(ISO3))
  
  # now let us get a unique dataframe of all experiemtns (i.e. countries
  # - shutting off emissions)
  #deltaT_df <- subset(deltaT_df, !is.na(experiment_iso))
  #fair_experiment_iso <- unique(deltaT_df$experiment_iso)
  pop <- wdi_dat %>% dplyr::select(c("iso3c", "SP.POP.TOTL", "year")) 
  
  # calculating model growth response after adjusting for the delta T by creating 
  # a function that takes in the temperature variable, as well as the model used
  calc_delta_g <- function(dataset, temp_var, model, deltaT, coef1, coef2) {
    response_tempnew <- ((temp_var - deltaT)*(coef1)) +
      (((temp_var - deltaT)^2)*(coef2)) 
    response_tempnew
  }

    i <- 2020
  # start an empty dataframe 
  mother_df <- data.frame()
  # now loop over the experiments and calculate total damages owed by each of 
  # the countries
  for (i in list_of_exps){
    
    # subset by keeping one experiment for each loop
    deltaT_df1 <- subset(deltaT_df, experiment_iso == i)
    # merge with coordinates and country ids
    joined_final_df <- left_join(deltaT_df1,
                                 base_coords,
                                 by = c("x", "y"))
    
    # keep non missing ISOs exoeriments
    joined_final_df <- subset(joined_final_df, !is.na(ISO3))
    # keep to a certain year
    #joined_final_df <- subset(joined_final_df, year < 2021)
    
    # now let us calculate the average deltaT by country-year
    joined_final_df <- joined_final_df %>% dplyr::group_by(ISO3, year) %>% 
      dplyr::summarise(deltat = mean(deltat, na.rm = T),
                       .groups = "keep")
    
    # now let us get the country-year panel
    gdp_temp_data1 <- gdp_temp_data
    #gdp_temp_data1$year <- as.numeric(as.character(gdp_temp_data1$year))
    
    #gdp_temp_data1 <- subset(gdp_temp_data1, year >= 1980)
    # join country-year temp change with the country-year panel
    gdp_temp_data1 <- left_join(joined_final_df,
                                gdp_temp_data1,
                                by = c("ISO3", "year"))
    
    # calculate reposnse from added temperature
    gdp_temp_data1$response_tempnew <- calc_delta_g(gdp_temp_data1,
                                                    gdp_temp_data1$era_mwtemp,
                                                    bhm_era_reg,
                                                    gdp_temp_data1$deltat,
                                                    gdp_temp_data1$temp,
                                                    gdp_temp_data1$temp2)
    
    # now let us calculate deltaG
    gdp_temp_data1$delta_g_era <- gdp_temp_data1$response_tempactual_era - 
      gdp_temp_data1$response_tempnew
    
    # let us bring in the population data at the country-year level
    gdp_temp_data1 <- left_join(gdp_temp_data1,
                                pop_wdi,
                                by = c("ISO3" = "iso3c",
                                       "year" = "year"))
    
    gdp_temp_data1 <- gdp_temp_data1 %>% 
      dplyr::mutate(SP.POP.TOTL = case_when(is.na(SP.POP.TOTL) ~ pop,
                                            TRUE ~ SP.POP.TOTL))
    
    # unlist...
    gdp_temp_data1$delta_g_era <- unlist(gdp_temp_data1$delta_g_era)
    
    # now let us calculate adjusted growht rate by adding deltaG to observed growth
    gdp_temp_data1$adj_growth <- (gdp_temp_data1$delta_g_era + gdp_temp_data1$diff_lgdp)
    
    # let us add 1 to growth variables so we can calculate cumulative growth
    gdp_temp_data1$diff_lgdp <- gdp_temp_data1$diff_lgdp + 1
    gdp_temp_data1$adj_growth <- gdp_temp_data1$adj_growth + 1
    
    # let us set missing NAs to 1 so we can run the script without breaking 
    # becasue of NAs
    gdp_temp_data1$diff_lgdp[is.na(gdp_temp_data1$diff_lgdp)] <- 1
    gdp_temp_data1$adj_growth[is.na(gdp_temp_data1$adj_growth)] <- 1
    
    # we now want to set 0 growth to NA
    gdp_temp_data1$diff_lgdp <- gdp_temp_data1$diff_lgdp - 1
    gdp_temp_data1$diff_lgdp[gdp_temp_data1$diff_lgdp == 0.000] <- NA
    # then let us calculate adjusted growth rate
    gdp_temp_data1 <- gdp_temp_data1 %>% 
      dplyr::mutate(adj_growthx = delta_g_era + diff_lgdp)
    # now let us add 1 so that we can calculate cumulative growth rate 
    damages_i_t3 <- gdp_temp_data1 %>% 
      dplyr::mutate(adj_growthxz = adj_growthx + 1)
    # now let us set GDP at year k as initial gdp number to calculate impact of 
    # added temoerature
    
    damages_i_t3 <- subset(damages_i_t3, !is.na(coef_id))
    
    damages_i_t3 <- damages_i_t3 %>% dplyr::group_by(ISO3, coef_id) %>% 
      dplyr::mutate(gdp_year = NY.GDP.PCAP.KD[year == year_k])
    
    # now let us add 1 to observed growth so we can calculate cumulative
    # gdp growth
    damages_i_t3$diff_lgdp1 <- damages_i_t3$diff_lgdp +1
    # now let us calculate cumulative GDP growth
    damages_i_t4 <- damages_i_t3 %>% dplyr::group_by(ISO3, coef_id) %>% 
      dplyr::mutate(cum_adj_growthz = cumprod(adj_growthxz),
                    cum_growth_real = cumprod(diff_lgdp1))
    # finally comput edamages...
    damages_i_t4 <- damages_i_t4 %>% dplyr::mutate(damages = (gdp_year * cum_adj_growthz) - 
                                                     (gdp_year * cum_growth_real))
    # scale by population
    damages_i_t4$damages_pop <- damages_i_t4$damages * damages_i_t4$SP.POP.TOTL
    
    # calculate discounted damages for past and for future (note different 
    #- processes for discounting past damages and future damages)
    damages_i_t4 <- damages_i_t4 %>% 
      dplyr::mutate(t_since_k = year - year_k,
                    t_since_today = year - 2020)
  

    damages_i_t4 <- damages_i_t4 %>% 
      dplyr::mutate(weighted_damages2 = case_when(year <= 2020 ~ (damages_pop*((1.02)^t_since_k)),
                                                  year > 2020 ~ (damages_pop*((0.98)^t_since_today))),
                    weighted_damages3 = case_when(year <= 2020 ~ (damages_pop*((1.03)^t_since_k)),
                                                  year > 2020 ~ (damages_pop*((0.97)^t_since_today))))
                    #weighted_damages5 = case_when(year <= 2020 ~ (damages_pop*((1+(0.05))^t_since_k)),
                     #                             year > 2020 ~ (damages_pop*(1/(1+(0.05))^t_since_today))),
                    #weighted_damages7 = case_when(year <= 2020 ~ (damages_pop*((1+(0.07))^t_since_k)),
                      #                            year > 2020 ~ (damages_pop*(1/(1+(0.07))^t_since_today))))
    # keep pnly negative damages
    damages_neg <- subset(damages_i_t4, damages_pop < 0)
    #sum(damages_neg$weighted_damages2, na.rm = T)
    #damages_neg$status <- paste0("data_", i)
    #damages_i_t4$status <- paste0("data_", i)
    # name th emitter
    damages_neg$emitter <- i
    
    damages_summ <- damages_neg %>% dplyr::group_by(emitter, coef_id) %>% 
      dplyr::summarise(total_damages = sum(damages_pop, na.rm = T),
                       total_damages2 = sum(weighted_damages2, na.rm = T),
                       total_damages3 = sum(weighted_damages3, na.rm = T),
                       .groups = "keep")
    #damages_i_t4$emitter <- i
    #test_dfs[[i]] <- damages_neg
    # add to data frame
    mother_df <- rbind(mother_df, damages_summ)
    toc()
  }
  return(mother_df)
}





run_bhm_model <- function(bhm_mode, future_forecast, year_k){
  
  bhm_mode_option <- bhm_mode
  
  gdp_temp_data <- readRDS("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/temp_gdp_world_panel.rds")
  # create the growth variable and set the dataset as a pdataframe
  gdp_temp_data$gdp_pc <- log(gdp_temp_data$NY.GDP.PCAP.KD)
  gdp_temp_data <- plm::pdata.frame(gdp_temp_data, index = c("ISO3","year"))
  gdp_temp_data$lgdp_pc <- plm::lag(gdp_temp_data$gdp_pc)
  gdp_temp_data$diff_lgdp <- gdp_temp_data$gdp_pc - gdp_temp_data$lgdp_pc
  #gencruting second order of year for quadratic country-year trends 
  gdp_temp_data$year2 <- as.numeric(gdp_temp_data$year)^2
  # gencrute poor identifiers 
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
                                                       "cru_mwtemp",
                                                       "cru_mwprecip",
                                                       "cru_mwtemp",
                                                       "cru_mwprecip",
                                                       "diff_lgdp",
                                                       "year2",
                                                       "NY.GDP.PCAP.PP.KD",
                                                       "NY.GDP.PCAP.KD"))
    
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
    
    gdp_temp_data$merge_id <- 1
    pooledbs$merge_id <- 1
    pooledbs$coef_id <- 1:nrow(pooledbs)
    
    gdp_temp_data <- left_join(gdp_temp_data,
                               pooledbs)
    
    gdp_temp_data <- subset(gdp_temp_data, year >= year_k)
    
    gdp_temp_data$response_tempactual_cru <- ((gdp_temp_data$cru_mwtemp)*(gdp_temp_data$temp)) +
      ((gdp_temp_data$cru_mwtemp^2)*(gdp_temp_data$temp2))
    
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



# calculating damages owed by each country to another. It takes in arguments 
# for the warming ratio used, as well as the dataframe from FaIR temperature 
# response and the list of emitters (list_of_exps)
calculate_bidamages <- function(ratio_raster, experiment_df, list_of_exps, 
                                year_k, future_forecast){
  tic()
  #read raster data for warming ratio 
  deltat_df <- as.data.frame(as.matrix(rasterToPoints(ratio_raster)))
  colnames(deltat_df)[3] <- "layer"
  
  # assign merge if to merge with fair data
  deltat_df$merge_id <- "1"
  
  # now assign the experiment data frame FROM FAIR
  fair_exps <- experiment_df 
  #fair_exps <- subset(fair_exps, num_loop == num_loop)
  #assign merge id 
  fair_exps$merge_id <- "1"
  
  fair_exps <- subset(fair_exps, !is.na(experiment_iso))
  # now bring them together
  deltat_calced_df <- left_join(deltat_df,
                                fair_exps,
                                by = c("merge_id"))
  
  # now multuply the grid level warming ratio by median temp response from FaIR
  deltat_calced_df$deltat <- deltat_calced_df$layer * deltat_calced_df$median_deltat
  
  # now let us take the columns we need from the fair experiment dataframe 
  deltaT_df <- deltat_calced_df %>% dplyr::select(c("x",
                                                    "y",
                                                    "deltat",
                                                    "year",
                                                    "experiment_iso"))
  
  # we need to aggregate the deltat to the country level by weighting by pop
  # so let us read the pop raster and resample it to match coordinates and 
  # convert to a dataframe and then join
  pop <- raster("~/BurkeLab Dropbox/Projects/loss_damage/data/raw/population/gpw_v4_population_count_rev11_2010_1_deg.tif")
  pop <- readAll(pop)
  pop <- resample(pop, ratio_raster)
  popdf <- as.data.frame(as.matrix(rasterToPoints(pop)))
  
  # the other thing we need is to get a country id for each lon-lat combo
  data("wrld_simpl")
  world <- wrld_simpl[,3]
  world <- st_as_sf(world)
  
  # now join base coordaintes with country ids and remove NAs
  base_coords <- deltat_df %>% dplyr::select(c("x", "y"))
  base_coords <- st_as_sf(base_coords, coords = c("x", "y"), 
                          crs = crs(ratio_raster))
  base_coords <- st_join(base_coords,world)
  base_coords <- base_coords %>%
    mutate(x = unlist(map(base_coords$geometry,1)),
           y = unlist(map(base_coords$geometry,2)))
  st_geometry(base_coords) <- NULL 
  base_coords <- subset(base_coords, !is.na(ISO3))
  
  # now let us get a unique dataframe of all experiemtns (i.e. countries
  # - shutting off emissions)
  #deltaT_df <- subset(deltaT_df, !is.na(experiment_iso))
  #fair_experiment_iso <- unique(deltaT_df$experiment_iso)
  pop <- wdi_dat %>% dplyr::select(c("iso3c", "SP.POP.TOTL", "year")) 
  
  # calculating model growth response after adjusting for the delta T by creating 
  # a function that takes in the temperature variable, as well as the model used
  calc_delta_g <- function(dataset, temp_var, model, deltaT, coef1, coef2) {
    response_tempnew <- ((temp_var - deltaT)*(coef1)) +
      (((temp_var - deltaT)^2)*(coef2)) 
    response_tempnew
  }
i <- 2020  
  # start an empty dataframe 
  mother_df <- data.frame()
  # now loop over the experiments and calculate total damages owed by each of 
  # the countries
  for (i in list_of_exps){
    
    # subset by keeping one experiment for each loop
    deltaT_df1 <- subset(deltaT_df, experiment_iso == i)
    # merge with coordinates and country ids
    joined_final_df <- left_join(deltaT_df1,
                                 base_coords,
                                 by = c("x", "y"))
    
    # keep non missing ISOs exoeriments
    joined_final_df <- subset(joined_final_df, !is.na(ISO3))
    # keep to a certain year
    #joined_final_df <- subset(joined_final_df, year < 2021)
    
    # now let us calculate the average deltaT by country-year
    joined_final_df <- joined_final_df %>% dplyr::group_by(ISO3, year) %>% 
      dplyr::summarise(deltat = mean(deltat, na.rm = T),
                       .groups = "keep")
    
    # now let us get the country-year panel
    gdp_temp_data1 <- gdp_temp_data
    #gdp_temp_data1$year <- as.numeric(as.character(gdp_temp_data1$year))
    
    #gdp_temp_data1 <- subset(gdp_temp_data1, year >= 1980)
    # join country-year temp change with the country-year panel
    gdp_temp_data1 <- left_join(joined_final_df,
                                gdp_temp_data1,
                                by = c("ISO3", "year"))
    
    # calculate reposnse from added temperature
    gdp_temp_data1$response_tempnew <- calc_delta_g(gdp_temp_data1,
                                                    gdp_temp_data1$cru_mwtemp,
                                                    bhm_era_reg,
                                                    gdp_temp_data1$deltat,
                                                    gdp_temp_data1$temp,
                                                    gdp_temp_data1$temp2)
    
    # now let us calculate deltaG
    gdp_temp_data1$delta_g_cru<- gdp_temp_data1$response_tempactual_cru- 
      gdp_temp_data1$response_tempnew
    
    # let us bring in the population data at the country-year level
    gdp_temp_data1 <- left_join(gdp_temp_data1,
                                pop_wdi,
                                by = c("ISO3" = "iso3c",
                                       "year" = "year"))
    
    gdp_temp_data1 <- gdp_temp_data1 %>% 
      dplyr::mutate(SP.POP.TOTL = case_when(is.na(SP.POP.TOTL) ~ pop,
                                            TRUE ~ SP.POP.TOTL))
    
    # unlist...
    gdp_temp_data1$delta_g_cru<- unlist(gdp_temp_data1$delta_g_cru)
    
    # now let us calculate adjusted growht rate by adding deltaG to observed growth
    gdp_temp_data1$adj_growth <- (gdp_temp_data1$delta_g_cru+ gdp_temp_data1$diff_lgdp)
    
    # let us add 1 to growth variables so we can calculate cumulative growth
    gdp_temp_data1$diff_lgdp <- gdp_temp_data1$diff_lgdp + 1
    gdp_temp_data1$adj_growth <- gdp_temp_data1$adj_growth + 1
    
    # let us set missing NAs to 1 so we can run the script without breaking 
    # becasue of NAs
    gdp_temp_data1$diff_lgdp[is.na(gdp_temp_data1$diff_lgdp)] <- 1
    gdp_temp_data1$adj_growth[is.na(gdp_temp_data1$adj_growth)] <- 1
    
    # we now want to set 0 growth to NA
    gdp_temp_data1$diff_lgdp <- gdp_temp_data1$diff_lgdp - 1
    gdp_temp_data1$diff_lgdp[gdp_temp_data1$diff_lgdp == 0.000] <- NA
    # then let us calculate adjusted growth rate
    gdp_temp_data1 <- gdp_temp_data1 %>% 
      dplyr::mutate(adj_growthx = delta_g_cru + diff_lgdp)
    # now let us add 1 so that we can calculate cumulative growth rate 
    damages_i_t3 <- gdp_temp_data1 %>% 
      dplyr::mutate(adj_growthxz = adj_growthx + 1)
    # now let us set GDP at year k as initial gdp number to calculate impact of 
    # added temoerature
    
    damages_i_t3 <- subset(damages_i_t3, !is.na(coef_id))
    
    damages_i_t3 <- damages_i_t3 %>% dplyr::group_by(ISO3, coef_id) %>% 
      dplyr::mutate(gdp_year = NY.GDP.PCAP.KD[year == year_k])
    
    # now let us add 1 to observed growth so we can calculate cumulative
    # gdp growth
    damages_i_t3$diff_lgdp1 <- damages_i_t3$diff_lgdp +1
    # now let us calculate cumulative GDP growth
    damages_i_t4 <- damages_i_t3 %>% dplyr::group_by(ISO3, coef_id) %>% 
      dplyr::mutate(cum_adj_growthz = cumprod(adj_growthxz),
                    cum_growth_real = cumprod(diff_lgdp1))
    # finally comput edamages...
    damages_i_t4 <- damages_i_t4 %>% dplyr::mutate(damages = (gdp_year * cum_adj_growthz) - 
                                                     (gdp_year * cum_growth_real))
    # scale by population
    damages_i_t4$damages_pop <- damages_i_t4$damages * damages_i_t4$SP.POP.TOTL
    
    # calculate discounted damages for past and for future (note different 
    #- processes for discounting past damages and future damages)
    damages_i_t4 <- damages_i_t4 %>% 
      dplyr::mutate(t_since_k = year - year_k,
                    t_since_today = year - 2020)
    
    
    damages_i_t4 <- damages_i_t4 %>% 
      dplyr::mutate(weighted_damages2 = case_when(year <= 2020 ~ (damages_pop*((1.02)^t_since_k)),
                                                  year > 2020 ~ (damages_pop*((0.98)^t_since_today))),
                    weighted_damages3 = case_when(year <= 2020 ~ (damages_pop*((1.03)^t_since_k)),
                                                  year > 2020 ~ (damages_pop*((0.97)^t_since_today))))
    #weighted_damages5 = case_when(year <= 2020 ~ (damages_pop*((1+(0.05))^t_since_k)),
    #                             year > 2020 ~ (damages_pop*(1/(1+(0.05))^t_since_today))),
    #weighted_damages7 = case_when(year <= 2020 ~ (damages_pop*((1+(0.07))^t_since_k)),
    #                            year > 2020 ~ (damages_pop*(1/(1+(0.07))^t_since_today))))
    # keep pnly negative damages
    damages_neg <- subset(damages_i_t4, damages_pop < 0)
    #sum(damages_neg$weighted_damages2, na.rm = T)
    #damages_neg$status <- paste0("data_", i)
    #damages_i_t4$status <- paste0("data_", i)
    # name th emitter
    damages_neg$emitter <- i
    
    damages_summ <- damages_neg %>% dplyr::group_by(emitter, coef_id) %>% 
      dplyr::summarise(total_damages = sum(damages_pop, na.rm = T),
                       total_damages2 = sum(weighted_damages2, na.rm = T),
                       total_damages3 = sum(weighted_damages3, na.rm = T),
                       .groups = "keep")
    
    summary(damages_summ$total_damages2 / 1000000000)
    #damages_i_t4$emitter <- i
    #test_dfs[[i]] <- damages_neg
    # add to data frame
    mother_df <- rbind(mother_df, damages_summ)
    toc()
  }
  return(mother_df)
}



run_bhm_model_reg <- function(bhm_mode){
  
  bhm_mode_option <- bhm_mode
  
  gdp_temp_data <- readRDS("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/temp_gdp_world_panel.rds")
  # create the growth variable and set the dataset as a pdataframe
  gdp_temp_data$gdp_pc <- log(gdp_temp_data$NY.GDP.PCAP.KD)
  gdp_temp_data <- plm::pdata.frame(gdp_temp_data, index = c("ISO3","year"))
  gdp_temp_data$lgdp_pc <- plm::lag(gdp_temp_data$gdp_pc)
  gdp_temp_data$diff_lgdp <- gdp_temp_data$gdp_pc - gdp_temp_data$lgdp_pc
  #gencruting second order of year for quadratic country-year trends 
  gdp_temp_data$year2 <- as.numeric(gdp_temp_data$year)^2
  # gencrute poor identifiers 
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
                                                       "cru_mwtemp",
                                                       "cru_mwprecip",
                                                       "cru_mwtemp",
                                                       "cru_mwprecip",
                                                       "diff_lgdp",
                                                       "year2",
                                                       "NY.GDP.PCAP.PP.KD",
                                                       "NY.GDP.PCAP.KD"))
    
    #cru
    bhm_cru_reg <-  fixest::feols(diff_lgdp ~ cru_mwtemp + cru_mwtemp^2 + cru_mwprecip + 
                                    cru_mwprecip^2 + ISO3*as.numeric(year) + ISO3*year2 | 
                                    ISO3 + year, gdp_temp_data)
    #estimate the response for every country-year
    gdp_temp_data$response_tempactual_cru <- ((gdp_temp_data$cru_mwtemp)*(coef(bhm_cru_reg)[1])) +
      ((gdp_temp_data$cru_mwtemp^2)*(coef(bhm_cru_reg)[2]))
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
    unique(damages_i_t4$year)
    
    damages_i_t3 <- subset(damages_i_t3, year >2020)
    
    gdp_temp_data1$response_tempnew <- calc_delta_g(gdp_temp_data1,
                                                    gdp_temp_data1$cru_mwtemp,
                                                    bhm_era_reg,
                                                    gdp_temp_data1$deltat,
                                                    gdp_temp_data1$temp,
                                                    gdp_temp_data1$temp2)
    
    # now let us calculate deltaG
    gdp_temp_data1$delta_g_cru<- gdp_temp_data1$response_tempactual_cru- 
      gdp_temp_data1$response_tempnew
    
    
    #estimate the response for every country-year
    class(gdp_temp_data)
    
    gdp_temp_data <- ungroup(gdp_temp_data)
    
    gdp_temp_data <- gdp_temp_data %>% dplyr::mutate(response_tempactual_cru = case_when(
      poor == 1 ~ ((gdp_temp_data$cru_mwtemp)*(coef(bhm_cru_reg)[1]) + (gdp_temp_data$cru_mwtemp^2)*(coef(bhm_cru_reg)[2]))+              
        (((gdp_temp_data$cru_mwtemp)*(coef(bhm_cru_reg)[5])) + (gdp_temp_data$cru_mwtemp^2)*(coef(bhm_cru_reg)[6])),
      poor == 0 ~ ((gdp_temp_data$cru_mwtemp)*(coef(bhm_cru_reg)[1])) +
        ((gdp_temp_data$cru_mwtemp^2)*(coef(bhm_cru_reg)[2]))))
  }
  return(bhm_cru_reg)
}




















# calculating damages owed by each country to another. It takes in arguments 
# for the warming ratio used, as well as the dataframe from FaIR temperature 
# response and the list of emitters (list_of_exps)
calculate_bidamages <- function(ratio_raster, experiment_df, list_of_exps, 
                                year_k, future_forecast){
  tic()
  #read raster data for warming ratio 
  deltat_df <- as.data.frame(as.matrix(rasterToPoints(ratio_raster)))
  colnames(deltat_df)[3] <- "layer"
  
  # assign merge if to merge with fair data
  deltat_df$merge_id <- "1"
  
  # now assign the experiment data frame FROM FAIR
  fair_exps <- experiment_df 
  #fair_exps <- subset(fair_exps, num_loop == num_loop)
  #assign merge id 
  fair_exps$merge_id <- "1"
  
  fair_exps <- subset(fair_exps, !is.na(experiment_iso))
  # now bring them together
  deltat_calced_df <- left_join(deltat_df,
                                fair_exps,
                                by = c("merge_id"))
  
  # now multuply the grid level warming ratio by median temp response from FaIR
  deltat_calced_df$deltat <- deltat_calced_df$layer * deltat_calced_df$median_deltat
  
  # now let us take the columns we need from the fair experiment dataframe 
  deltaT_df <- deltat_calced_df %>% dplyr::select(c("x",
                                                    "y",
                                                    "deltat",
                                                    "year",
                                                    "experiment_iso"))
  
  # we need to aggregate the deltat to the country level by weighting by pop
  # so let us read the pop raster and resample it to match coordinates and 
  # convert to a dataframe and then join
  pop <- raster("~/BurkeLab Dropbox/Projects/loss_damage/data/raw/population/gpw_v4_population_count_rev11_2010_1_deg.tif")
  pop <- readAll(pop)
  pop <- resample(pop, ratio_raster)
  popdf <- as.data.frame(as.matrix(rasterToPoints(pop)))
  
  # the other thing we need is to get a country id for each lon-lat combo
  data("wrld_simpl")
  world <- wrld_simpl[,3]
  world <- st_as_sf(world)
  
  # now join base coordaintes with country ids and remove NAs
  base_coords <- deltat_df %>% dplyr::select(c("x", "y"))
  base_coords <- st_as_sf(base_coords, coords = c("x", "y"), 
                          crs = crs(ratio_raster))
  base_coords <- st_join(base_coords,world)
  base_coords <- base_coords %>%
    mutate(x = unlist(map(base_coords$geometry,1)),
           y = unlist(map(base_coords$geometry,2)))
  st_geometry(base_coords) <- NULL 
  base_coords <- subset(base_coords, !is.na(ISO3))
  
  # now let us get a unique dataframe of all experiemtns (i.e. countries
  # - shutting off emissions)
  #deltaT_df <- subset(deltaT_df, !is.na(experiment_iso))
  #fair_experiment_iso <- unique(deltaT_df$experiment_iso)
  pop <- wdi_dat %>% dplyr::select(c("iso3c", "SP.POP.TOTL", "year")) 
  
  # calculating model growth response after adjusting for the delta T by creating 
  # a function that takes in the temperature variable, as well as the model used
  calc_delta_g <- function(dataset, temp_var, temp_var1, temp_var2, temp_var3,
                           model, deltaT, coef1, coef2,
                           coef1a, coef2a, coef1b, coef2b, coef1c, coef2c) {
    response_tempnew <- ((temp_var - deltaT)*(coef1)) +
      (((temp_var - deltaT)^2)*(coef2)) +
      ((temp_var1 - deltaT)*(coef1a)) +
      (((temp_var1 - deltaT)^2)*(coef2a))
    ((temp_var2 - deltaT)*(coef1b)) +
      (((temp_var2 - deltaT)^2)*(coef2b))
    ((temp_var3 - deltaT)*(coef1c)) +
      (((temp_var3 - deltaT)^2)*(coef2c))
    response_tempnew
  }
  
  i <- 2020
  # start an empty dataframe 
  mother_df <- data.frame()
  # now loop over the experiments and calculate total damages owed by each of 
  # the countries
  for (i in list_of_exps){
    
    # subset by keeping one experiment for each loop
    deltaT_df1 <- subset(deltaT_df, experiment_iso == i)
    # merge with coordinates and country ids
    joined_final_df <- left_join(deltaT_df1,
                                 base_coords,
                                 by = c("x", "y"))
    
    # keep non missing ISOs exoeriments
    joined_final_df <- subset(joined_final_df, !is.na(ISO3))
    # keep to a certain year
    #joined_final_df <- subset(joined_final_df, year < 2021)
    
    # now let us calculate the average deltaT by country-year
    joined_final_df <- joined_final_df %>% dplyr::group_by(ISO3, year) %>% 
      dplyr::summarise(deltat = mean(deltat, na.rm = T),
                       .groups = "keep")
    
    # now let us get the country-year panel
    gdp_temp_data1 <- gdp_temp_data
    #gdp_temp_data1$year <- as.numeric(as.character(gdp_temp_data1$year))
    
    #gdp_temp_data1 <- subset(gdp_temp_data1, year >= 1980)
    # join country-year temp change with the country-year panel
    gdp_temp_data1 <- left_join(joined_final_df,
                                gdp_temp_data1,
                                by = c("ISO3", "year"))
    
    # calculate reposnse from added temperature
    gdp_temp_data1$response_tempnew <- calc_delta_g(gdp_temp_data1,
                                                    gdp_temp_data1$era_mwtemp,
                                                    gdp_temp_data1$era_mwtemp_l1,
                                                    gdp_temp_data1$era_mwtemp_l2,
                                                    gdp_temp_data1$era_mwtemp_l3,
                                                    bhm_era_reg,
                                                    gdp_temp_data1$deltat,
                                                    gdp_temp_data1$temp1,
                                                    gdp_temp_data1$temp1_sq,
                                                    gdp_temp_data1$temp2,
                                                    gdp_temp_data1$temp2_sq,
                                                    gdp_temp_data1$temp3,
                                                    gdp_temp_data1$temp3_sq,
                                                    gdp_temp_data1$temp4,
                                                    gdp_temp_data1$temp4_sq)
    
    # now let us calculate deltaG
    gdp_temp_data1$delta_g_era <- gdp_temp_data1$response_tempactual_era - 
      gdp_temp_data1$response_tempnew
    
    # let us bring in the population data at the country-year level
    gdp_temp_data1 <- left_join(gdp_temp_data1,
                                pop_wdi,
                                by = c("ISO3" = "iso3c",
                                       "year" = "year"))
    
    gdp_temp_data1 <- gdp_temp_data1 %>%
      dplyr::mutate(SP.POP.TOTL = case_when(is.na(SP.POP.TOTL) ~ pop,
                                            TRUE ~ SP.POP.TOTL))
    
    # unlist...
    gdp_temp_data1$delta_g_era <- unlist(gdp_temp_data1$delta_g_era)
    
    # now let us calculate adjusted growht rate by adding deltaG to observed growth
    gdp_temp_data1$adj_growth <- (gdp_temp_data1$delta_g_era + gdp_temp_data1$diff_lgdp)
    
    # let us add 1 to growth variables so we can calculate cumulative growth
    gdp_temp_data1$diff_lgdp <- gdp_temp_data1$diff_lgdp + 1
    gdp_temp_data1$adj_growth <- gdp_temp_data1$adj_growth + 1
    
    # let us set missing NAs to 1 so we can run the script without breaking 
    # becasue of NAs
    gdp_temp_data1$diff_lgdp[is.na(gdp_temp_data1$diff_lgdp)] <- 1
    gdp_temp_data1$adj_growth[is.na(gdp_temp_data1$adj_growth)] <- 1
    
    # we now want to set 0 growth to NA
    gdp_temp_data1$diff_lgdp <- gdp_temp_data1$diff_lgdp - 1
    gdp_temp_data1$diff_lgdp[gdp_temp_data1$diff_lgdp == 0.000] <- NA
    # then let us calculate adjusted growth rate
    gdp_temp_data1 <- gdp_temp_data1 %>% 
      dplyr::mutate(adj_growthx = delta_g_era + diff_lgdp)
    # now let us add 1 so that we can calculate cumulative growth rate 
    damages_i_t3 <- gdp_temp_data1 %>% 
      dplyr::mutate(adj_growthxz = adj_growthx + 1)
    # now let us set GDP at year k as initial gdp number to calculate impact of 
    # added temoerature
    
    damages_i_t3 <- subset(damages_i_t3, !is.na(coef_id))
    
    damages_i_t3 <- damages_i_t3 %>% dplyr::group_by(ISO3, coef_id) %>% 
      dplyr::mutate(gdp_year = NY.GDP.PCAP.KD[year == year_k])
    
    # now let us add 1 to observed growth so we can calculate cumulative
    # gdp growth
    damages_i_t3$diff_lgdp1 <- damages_i_t3$diff_lgdp +1
    # now let us calculate cumulative GDP growth
    damages_i_t4 <- damages_i_t3 %>% dplyr::group_by(ISO3, coef_id) %>% 
      dplyr::mutate(cum_adj_growthz = cumprod(adj_growthxz),
                    cum_growth_real = cumprod(diff_lgdp1))
    # finally comput edamages...
    damages_i_t4 <- damages_i_t4 %>% dplyr::mutate(damages = (gdp_year * cum_adj_growthz) - 
                                                     (gdp_year * cum_growth_real))
    # scale by population
    damages_i_t4$damages_pop <- damages_i_t4$damages * damages_i_t4$SP.POP.TOTL
    
    # calculate discounted damages for past and for future (note different 
    #- processes for discounting past damages and future damages)
    damages_i_t4 <- damages_i_t4 %>% 
      dplyr::mutate(t_since_k = year - year_k,
                    t_since_today = year - 2020)
    
    
    damages_i_t4 <- damages_i_t4 %>% 
      dplyr::mutate(weighted_damages2 = case_when(year <= 2020 ~ (damages_pop*((1.02)^t_since_k)),
                                                  year > 2020 ~ (damages_pop*((0.98)^t_since_today))),
                    weighted_damages3 = case_when(year <= 2020 ~ (damages_pop*((1.03)^t_since_k)),
                                                  year > 2020 ~ (damages_pop*((0.97)^t_since_today))))
    #weighted_damages5 = case_when(year <= 2020 ~ (damages_pop*((1+(0.05))^t_since_k)),
    #                             year > 2020 ~ (damages_pop*(1/(1+(0.05))^t_since_today))),
    #weighted_damages7 = case_when(year <= 2020 ~ (damages_pop*((1+(0.07))^t_since_k)),
    #                            year > 2020 ~ (damages_pop*(1/(1+(0.07))^t_since_today))))
    # keep pnly negative damages
    damages_neg <- subset(damages_i_t4, damages_pop < 0)
    #sum(damages_neg$weighted_damages2, na.rm = T)
    #damages_neg$status <- paste0("data_", i)
    #damages_i_t4$status <- paste0("data_", i)
    # name th emitter
    damages_neg$emitter <- i
    
    damages_summ <- damages_neg %>% dplyr::group_by(emitter, coef_id) %>% 
      dplyr::summarise(total_damages = sum(damages_pop, na.rm = T),
                       total_damages2 = sum(weighted_damages2, na.rm = T),
                       total_damages3 = sum(weighted_damages3, na.rm = T),
                       .groups = "keep")
    #damages_i_t4$emitter <- i
    #test_dfs[[i]] <- damages_neg
    # add to data frame
    mother_df <- rbind(mother_df, damages_summ)
    toc()
  }
  return(mother_df)
}


run_bhm_model <- function(bhm_mode, future_forecast, year_k){
  
  bhm_mode_option <- bhm_mode
  
  gdp_temp_data <- readRDS("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/temp_gdp_world_panel.rds")
  # create the growth variable and set the dataset as a pdataframe
  gdp_temp_data$gdp_pc <- log(gdp_temp_data$NY.GDP.PCAP.KD)
  gdp_temp_data <- plm::pdata.frame(gdp_temp_data, index = c("ISO3","year"))
  gdp_temp_data$lgdp_pc <- plm::lag(gdp_temp_data$gdp_pc)
  gdp_temp_data$diff_lgdp <- gdp_temp_data$gdp_pc - gdp_temp_data$lgdp_pc
  #generating second order of year for quadratic country-year trends 
  gdp_temp_data$year2 <- as.numeric(gdp_temp_data$year)^2
  # generate poor identifiers 
  gdp_temp_data <- gdp_temp_data %>% 
    dplyr::mutate(median_gdp = median(NY.GDP.PCAP.KD, na.rm = T)) %>% 
    dplyr::group_by(ISO3) %>% 
    dplyr::mutate(avg_gdp = mean(NY.GDP.PCAP.KD, na.rm = T))
  
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
                                                       "era_mwprecip",
                                                       "diff_lgdp",
                                                       "year2",
                                                       "NY.GDP.PCAP.PP.KD",
                                                       "NY.GDP.PCAP.KD"))
    
    #era
    #    bhm_era_reg <-  fixest::feols(diff_lgdp ~ era_mwtemp + era_mwtemp^2 + era_mwprecip + 
    #                                   era_mwprecip^2 + ISO3*as.numeric(year) + ISO3*year2 | 
    #                                  ISO3 + year, gdp_temp_data)
    #estimate the response for every country-year
    gdp_temp_data <- rbind.fill(gdp_temp_data,
                                future_forecast)
    
    # create 5 lags of temperature 
    gdp_temp_data <- gdp_temp_data %>%  dplyr::group_by(ISO3) %>% 
      dplyr::mutate(era_mwtemp_l1 = lag(era_mwtemp, 1),
                    era_mwtemp_l2 = lag(era_mwtemp, 2),
                    era_mwtemp_l3 = lag(era_mwtemp, 3),
                    era_mwtemp_l4 = lag(era_mwtemp, 4),
                    era_mwtemp_l5 = lag(era_mwtemp, 5))
    
    gdp_temp_data <- subset(gdp_temp_data, !is.na(era_mwtemp))
                    

    #gdp_temp_data <- left_join(gdp_temp_data,
    #                          ssp_gdp_pop,
    #                         by = c("ISO3", "year"))
    
    # gdp_temp_data <- gdp_temp_data %>% 
    
    #  dplyr::mutate(era_mwtemp = case_when(!is.na(era_mwtemp.x) ~ era_mwtemp.x,
    #                                      !is.na(era_mwtemp.y) ~ era_mwtemp.y))
    
    gdp_temp_data$merge_id <- 1
    pooledbs$merge_id <- 1
    pooledbs$coef_id <- 1:nrow(pooledbs)
  
    colnames(pooledbs)[1] <- "temp1"
    colnames(pooledbs)[2] <- "temp1_sq"
    colnames(pooledbs)[3] <- "temp2"
    colnames(pooledbs)[4] <- "temp2_sq"
    colnames(pooledbs)[5] <- "temp3"
    colnames(pooledbs)[6] <- "temp3_sq"
    colnames(pooledbs)[7] <- "temp4"
    colnames(pooledbs)[8] <- "temp4_sq"
    
    
    gdp_temp_data <- left_join(gdp_temp_data,
                               pooledbs) 
    
    gdp_temp_data$year <- as.numeric(as.character(gdp_temp_data$year))
    gdp_temp_data <- subset(gdp_temp_data, year >= year_k)
    
    gdp_temp_data$response_tempactual_era <- ((gdp_temp_data$era_mwtemp)*(gdp_temp_data$temp1)) +
      ((gdp_temp_data$era_mwtemp^2)*(gdp_temp_data$temp1_sq)) + 
      ((gdp_temp_data$era_mwtemp_l1)*(gdp_temp_data$temp2)) +
      ((gdp_temp_data$era_mwtemp_l1^2)*(gdp_temp_data$temp2_sq)) + 
      ((gdp_temp_data$era_mwtemp_l2)*(gdp_temp_data$temp3)) +
      ((gdp_temp_data$era_mwtemp_l2^2)*(gdp_temp_data$temp3_sq)) + 
      ((gdp_temp_data$era_mwtemp_l3)*(gdp_temp_data$temp4)) +
      ((gdp_temp_data$era_mwtemp_l3^2)*(gdp_temp_data$temp4_sq)) 
    
    gdp_temp_data <- subset(gdp_temp_data, !is.na(coef_id))
    
    #gdp_temp_data$year <- as.numeric(as.character(gdp_temp_data$year))
  }
  else if (bhm_mode_option == "richpoor") {
    gdp_temp_data <- gdp_temp_data %>% dplyr::select(c("ISO3",
                                                       "year",
                                                       "era_mwtemp",
                                                       "era_mwprecip",
                                                       "era_mwtemp",
                                                       "era_mwprecip",
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
    # Run the models to estimate the response function f() using both era and ERA 
    
    #era
    bhm_era_reg <-  fixest::feols(diff_lgdp ~ era_mwtemp + era_mwtemp^2 + (poor*era_mwtemp) + (poor*era_mwtemp^2) + 
                                    era_mwprecip + era_mwprecip^2 + (poor*era_mwprecip) + (poor*era_mwprecip^2) + 
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
    #   dplyr::mutate(era_mwtemp = case_when(!is.na(era_mwtemp.x) ~ era_mwtemp.x,
    #                                       !is.na(era_mwtemp.y) ~ era_mwtemp.y))
    
    
    
    gdp_temp_data <- gdp_temp_data %>% dplyr::mutate(response_tempactual_era = case_when(
      poor == 1 ~ ((gdp_temp_data$era_mwtemp)*(coef(bhm_era_reg)[1]) + (gdp_temp_data$era_mwtemp^2)*(coef(bhm_era_reg)[2]))+              
        (((gdp_temp_data$era_mwtemp)*(coef(bhm_era_reg)[5])) + (gdp_temp_data$era_mwtemp^2)*(coef(bhm_era_reg)[6])),
      poor == 0 ~ ((gdp_temp_data$era_mwtemp)*(coef(bhm_era_reg)[1])) +
        ((gdp_temp_data$era_mwtemp^2)*(coef(bhm_era_reg)[2]))))
  }
  return(gdp_temp_data)
}
