##############################################################################
# Mustafa Zahid, April 1st, 2022
# This script calculates the total damages owed by emitter country j to receiver 
# country i 
#############################################################################

#ratio_raster <- master_raster
#experiment_df <- fair_exps_cc
#year_k <- 1980
#future_forecast <- future_forecast_ssp370_2300
#gdp_temp_dataset <- gdp_temp_data_k80_2300
#temp_dataset <- "ERA"
#bhm_model <- bhm_era_reg


#ratio_raster <- master_raster
#experiment_df <- fair_exps_1tco2_2300

#list_of_exps <- unique(fair_exps_1tco2_2300$experiment_iso)
#year_k <- 1990
#future_forecast <- future_forecast_ssp370_2300
#

calculate_bidamages <- function(ratio_raster, experiment_df, list_of_exps, 
                                year_k, future_forecast, gdp_temp_dataset, 
                                temp_dataset, bhm_model,
                                clamping, growth_past_2100){
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
  
  #  fair_exps <- subset(fair_exps, !is.na(experiment_iso))
  # now bring them together
  deltat_calced_df <- left_join(deltat_df,
                                fair_exps,
                                by = c("merge_id"), multiple = "all")
  # now multuply the grid level warming ratio by median temp response from FaIR
  #deltat_calced_df$deltat <- deltat_calced_df$layer * deltat_calced_df$median_deltat
  deltat_calced_df$deltat_scld <- deltat_calced_df$layer * deltat_calced_df$median_deltat   
  deltat_calced_df$deltat_fullemms_scld <- deltat_calced_df$layer * deltat_calced_df$deltat_fullemms
  deltat_calced_df$deltat_preturb_scld <- deltat_calced_df$layer * deltat_calced_df$deltaT_preturb
  # now let us take the columns we need from the fair experiment dataframe 
  deltaT_df <- deltat_calced_df %>% dplyr::select(c("x",
                                                    "y",
                                                    "deltat_fullemms_scld",
                                                    "deltat_preturb_scld",
                                                    "deltat_scld",
                                                    "year",
                                                    "experiment_iso"))
  #  "exp_yr"))
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

  #base_coords
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
  toc()
  # start an empty dataframe 
  mother_df <- data.frame()
  #unique(fair_exps_1tco2_2300$experiment_iso)
  i <- 2029
  #i <- "USA"
  #i <- "all0"
  # now loop over the experiments and calculate total damages owed by each of 
  # the countries
  for (i in list_of_exps){
    tic()
    
    # subset by keeping one experiment for each loop
  #  deltaT_df1 <- subset(deltaT_df, experiment_iso == i)
  #  tic()
  #  deltaT_df1 <- deltaT_df %>% dplyr::filter(experiment_iso == i)
    
  #  tic()
    deltaT_df1 <- deltaT_df[ which(deltaT_df$experiment_iso==i), ]
    
    toc()
    
    tic()
    #  deltaT_df1 <- deltaT_df
    # merge with coordinates and country ids
    joined_final_df <- left_join(deltaT_df1,
                                 base_coords,
                                 by = c("x", "y"))
    toc()
    tic()

    # keep non missing ISOs exoeriments
    joined_final_df <- subset(joined_final_df, !is.na(ISO3))
    toc()
    # keep to a certain year
    #joined_final_df <- subset(joined_final_df, year < 2021)
    
    # now let us calculate the average deltaT by country-year
    #    joined_final_df <- joined_final_df %>% dplyr::group_by(ISO3, year) %>% 
    #     dplyr::summarise(deltat = mean(deltat, na.rm = T),
    #                     .groups = "keep")
    tic()
    joined_final_df <- joined_final_df %>% dplyr::group_by(ISO3, year) %>% 
      dplyr::summarise(deltat_fullemms = mean(deltat_fullemms_scld, na.rm = T),
                       deltat_preturb = mean(deltat_preturb_scld, na.rm = T),
                       deltat = mean(deltat_scld, na.rm = T),
                       .groups = "keep")
    toc()
    
    tic()

    joined_final_df$deltat_preturb[joined_final_df$year < 2021] <- 0
    joined_final_df$deltat_fullemms[joined_final_df$year < 2021] <- 0
    
    # now let us get the country-year panel
    #plot(gdp_temp_data_k90_2300$year, gdp_temp_data_k90_2300$diff_lgdp)
    gdp_temp_data1 <- gdp_temp_dataset
    
    gdp_temp_data1$year <- as.numeric(as.character(gdp_temp_data1$year))
    toc()
    #gdp_temp_data1 <- subset(gdp_temp_data1, year >= 1980)
    # join country-year temp change with the country-year panel
    tic()
    gdp_temp_data1 <- left_join(joined_final_df,
                                gdp_temp_data1,
                                by = c("ISO3", "year"))
    toc()
    tic()
    # we need to create country level annual average for last 5 years of observed 
    # data 
    if (temp_dataset == "ERA"){
      gdp_temp_data1$era_mwtemp[gdp_temp_data1$year >2020] <- NA 
      #  if (i < 2020){
      gdp_temp_data1$era_mwtemp_preturb_pre2020 <- gdp_temp_data1$era_mwtemp - gdp_temp_data1$deltat
      gdp_temp_data1 <- gdp_temp_data1 %>% dplyr::group_by(ISO3) %>% 
        dplyr::mutate(avg_temp_2016_2020_preturb = mean(era_mwtemp_preturb_pre2020[year > 2015 & year <= 2020], na.rm = T))
      
    }
    if (temp_dataset == "CRU"){
      gdp_temp_data1$cru_mwtemp[gdp_temp_data1$year >2020] <- NA 
      #  if (i < 2020){
      gdp_temp_data1$cru_mwtemp_preturb_pre2020 <- gdp_temp_data1$cru_mwtemp - gdp_temp_data1$deltat
      gdp_temp_data1 <- gdp_temp_data1 %>% dplyr::group_by(ISO3) %>% 
        dplyr::mutate(avg_temp_2016_2020_preturb = mean(cru_mwtemp_preturb_pre2020[year > 2015 & year <= 2020], na.rm = T))
    }
    toc()
    
    # plot(gdp_temp_data1$year[gdp_temp_data1$ISO3 == "USA"], gdp_temp_data1$deltat[gdp_temp_data1$ISO3== "SAU"], type = "l")
    #  plot(fair_exps_1GtCO2_updated$year[fair_exps_1GtCO2_updated$experiment_iso == 2020], fair_exps_1GtCO2_updated$median_deltat[fair_exps_1GtCO2_updated$experiment_iso == 2020])
    
    
    #  }
    
    #  gdp_temp_data1 <- gdp_temp_data1 %>% dplyr::group_by(ISO3) %>% 
    #   dplyr::mutate(avg_temp_2016_2020 = mean(era_mwtemp[year > 2015 & year <= 2020], na.rm = T))
    
    #  if (i >= 2020){
    #  gdp_temp_data1 <- gdp_temp_data1 %>%  dplyr::group_by(ISO3, year) %>% 
    #  dplyr::mutate(era_mwtemp_fullemms = case_when(!is.na(era_mwtemp) ~ era_mwtemp, 
    #                                            is.na(era_mwtemp) & year >2020 ~ avg_temp_2016_2020 + deltat_fullemms),
    #               era_mwtemp_preturb= case_when(!is.na(era_mwtemp) ~ era_mwtemp, 
    #                                             is.na(era_mwtemp) & year >2020 ~ avg_temp_2016_2020 + deltat_preturb))
    # }
    
    #  if (i >= 2020){
    #    gdp_temp_data1 <- gdp_temp_data1 %>%  dplyr::group_by(ISO3, year) %>% 
    #      dplyr::mutate(era_mwtemp_preturb= case_when(!is.na(era_mwtemp) ~ era_mwtemp, 
    #                                                  is.na(era_mwtemp) & year >2020 ~ avg_temp_2016_2020_preturb + deltat_preturb))
    #  }
    
    #  gdp_temp_data1$era_mwtemp_fullemms <- gdp_temp_data1$era_mwtemp_preturb + gdp_temp_data1$deltat 
    
    #  if (i < 2020) {
    #  gdp_temp_data1 <- gdp_temp_data1 %>%  dplyr::group_by(ISO3, year) %>% 
    #  dplyr::mutate(era_mwtemp_fullemms = case_when(!is.na(era_mwtemp) ~ era_mwtemp, 
    #                                                 is.na(era_mwtemp) & year >2020 ~ avg_temp_2016_2020 + deltat_fullemms),
    #                  era_mwtemp_preturb= case_when(!is.na(era_mwtemp) ~ era_mwtemp_preturb_pre2020, 
    #                                                 is.na(era_mwtemp) & year >2020 ~ avg_temp_2016_2020_preturb + deltat_preturb))
    #  }
    #  gdp_temp_data1$deltat <- gdp_temp_data1$era_mwtemp_ssp370 - gdp_temp_data1$era_mwtemp_noemms
    #  if (i < 2020) {
    #remotes::install_version("dplyr", "1.0.10")
    toc()
    tic()
    if (temp_dataset == "ERA") {
      
      gdp_temp_data1 <- gdp_temp_data1 %>%  dplyr::group_by(ISO3, year) %>% 
        dplyr::mutate(
          era_mwtemp_preturb= case_when(!is.na(era_mwtemp) ~ era_mwtemp_preturb_pre2020, 
                                        is.na(era_mwtemp) & year >2020 ~ avg_temp_2016_2020_preturb + deltat_preturb))
    }
    toc()
    if (temp_dataset == "CRU"){
      gdp_temp_data1 <- gdp_temp_data1 %>%  dplyr::group_by(ISO3, year) %>% 
        dplyr::mutate(
          cru_mwtemp_preturb= case_when(!is.na(cru_mwtemp) ~ cru_mwtemp_preturb_pre2020, 
                                        is.na(cru_mwtemp) & year >2020 ~ avg_temp_2016_2020_preturb + deltat_preturb))
    }
    #  }
  #  toc()
    
    #growth_past_2100 <- "no"
    #clamping <- "no"
    tic()
    
    if (temp_dataset == "ERA"){
      gdp_temp_data1$era_mwtemp_fullemms <- gdp_temp_data1$era_mwtemp_preturb + gdp_temp_data1$deltat  
    }
    if (temp_dataset == "CRU"){
      gdp_temp_data1$cru_mwtemp_fullemms <- gdp_temp_data1$cru_mwtemp_preturb + gdp_temp_data1$deltat  
    }
    
    #  gdp_temp_data1$deltat <- gdp_temp_data1$era_mwtemp_ssp370 - gdp_temp_data1$era_mwtemp_noemms
    #  gdp_temp_data1$adjusted_temp <- gdp_temp_data1$era_mwtemp - gdp_temp_data1$deltat
    
    # dev.off()
    #  par(mfrow = c(1,1))
    
    #  hist(gdp_temp_data1$era_mwtemp, add =T)
    #  hist(gdp_temp_data1$adjusted_temp, col = "red", add = T)
    # calculate reposnse from added temperature
    # gdp_temp_data1$response_tempnew <- calc_delta_g(gdp_temp_data1,
    #                                                gdp_temp_data1$era_mwtemp,
    #                                               bhm_era_reg,
    #                                              gdp_temp_data1$deltat,
    #                                             coef(bhm_era_reg)[1],
    #                                            coef(bhm_era_reg)[2])
    

    #   bhm_model <- run_bhm_model_reg("pooled")
    
    gdp_temp_data1$temp <- coef(bhm_model)[1]
    gdp_temp_data1$temp2 <- coef(bhm_model)[2]
    
    gdp_temp_data1$era_mwtemp_fullemms[gdp_temp_data1$era_mwtemp_fullemms > 30 & !is.na(gdp_temp_data1$era_mwtemp_fullemms)] <- 30
    gdp_temp_data1$era_mwtemp_preturb[gdp_temp_data1$era_mwtemp_preturb > 30 & !is.na(gdp_temp_data1$era_mwtemp_preturb)] <- 30
    
    #plot(gdp_temp_data1$year, gdp_temp_data1$era_mwtemp_fullemms)
    #if (experiment == "ssp370" | experiment == "ssp370" | experiment == "ssp370" | experiment == "ssp370"){
  #    gdp_temp_data1$era_mwtemp_fullemms[gdp_temp_data1$era_mwtemp_fullemms > 30] <- 30
  #  }
    
    if (temp_dataset == "ERA"){
      gdp_temp_data1$resp_temp_fullemms <- (gdp_temp_data1$era_mwtemp_fullemms * gdp_temp_data1$temp) + 
        ((gdp_temp_data1$era_mwtemp_fullemms^2) * gdp_temp_data1$temp2)
      
      gdp_temp_data1$resp_temp_preturb <- (gdp_temp_data1$era_mwtemp_preturb * gdp_temp_data1$temp) + 
        ((gdp_temp_data1$era_mwtemp_preturb^2) * gdp_temp_data1$temp2)
      
      gdp_temp_data1$delta_g_era <- gdp_temp_data1$resp_temp_preturb - gdp_temp_data1$resp_temp_fullemms
      
    #  maxtemp_2020 <- max(gdp_temp_data1$era_mwtemp[gdp_temp_data1$year < 2021], na.rm = T)
     # gdp_temp_data1$max_delta_g_era <- gdp_temp_data1$delta_g_era[gdp_temp_data1$year < 2021 & gdp_temp_data1$era_mwtemp == maxtemp_2020 & !is.na(gdp_temp_data1$delta_g_era)]
      #max_g <- gdp_temp_data1$delta_g_era[gdp_temp_data1$year < 2021 & gdp_temp_data1$era_mwtemp == maxtemp_2020 & !is.na(gdp_temp_data1$delta_g_era)]
      #gdp_temp_data1$delta_g_era[gdp_temp_data1$year > 2020 & gdp_temp_data1$delta_g_era < max_g] <- max_g 
      
    }
    
    if (growth_past_2100 == 0){
      gdp_temp_data1$delta_g_era[gdp_temp_data1$year > 2100] <- 0
    }
  
    if (temp_dataset == "CRU"){
      gdp_temp_data1$resp_temp_fullemms <- (gdp_temp_data1$cru_mwtemp_fullemms * gdp_temp_data1$temp) + 
        ((gdp_temp_data1$cru_mwtemp_fullemms^2) * gdp_temp_data1$temp2)
      
      gdp_temp_data1$resp_temp_preturb <- (gdp_temp_data1$cru_mwtemp_preturb * gdp_temp_data1$temp) + 
        ((gdp_temp_data1$cru_mwtemp_preturb^2) * gdp_temp_data1$temp2)
      
      gdp_temp_data1$delta_g_cru <- gdp_temp_data1$resp_temp_preturb - gdp_temp_data1$resp_temp_fullemms
      
    }
    
        #summary(gdp_temp_data1$resp_temp_preturb)
    #summary(gdp_temp_data1$resp_temp_fullemms)
    #hist(gdp_temp_data1$delta_g_era)
    
    #gdp_temp_data1_vra <- subset(gdp_temp_data1, ISO3 == "BRA")
    #plot(gdp_temp_data1_vra$year, gdp_temp_data1_vra$era_mwtemp_fullemms - gdp_temp_data1_vra$era_mwtemp_preturb, type = "l")
    #lines(gdp_temp_data1_vra$year, gdp_temp_data1_vra$era_mwtemp_preturb, type = "l", col = "red")
    
    #gdp_temp_data1$temp,
    #gdp_temp_data1$temp2)
    
    # now let us calculate deltaG
    #gdp_temp_data1$delta_g_era <- gdp_temp_data1$response_tempactual_era - gdp_temp_data1$response_tempnew
    
    # test1 <- gdp_temp_data1 %>% dplyr::select(c("response_tempactual_era", "response_tempnew","delta_g_era", "ISO3", "year"))
    # let us bring in the population data at the country-year level
    gdp_temp_data1 <- left_join(gdp_temp_data1,
                                pop_wdi,
                                by = c("ISO3" = "iso3c",
                                       "year" = "year"))
    
    gdp_temp_data1 <- gdp_temp_data1 %>% 
      dplyr::mutate(SP.POP.TOTL = case_when(is.na(SP.POP.TOTL) ~ pop,
                                            TRUE ~ SP.POP.TOTL))
    
    identifier <- gdp_temp_data1 %>% 
      #  dplyr::group_by(ISO3, year) %>% 
      # dplyr::summarise(diff_lgdp = mean(diff_lgdp, na.rm = T),
      #                 .group = "keep") %>% 
      ungroup(.) %>%  
      dplyr::select(c("ISO3", "diff_lgdp", "year", "NY.GDP.PCAP.KD")) %>% 
      subset(.,year == year_k) %>% 
      dplyr::mutate(ssp_data = case_when(is.na(diff_lgdp) ~ "no",
                                         TRUE ~ "yes")) %>% 
      subset(., ssp_data == "yes") %>% 
      dplyr::select(c("ISO3"))
    
    
    gdp_temp_data1 <- left_join(identifier, gdp_temp_data1)
    
    # Now let us clamp teh growth rates on both sides 
    # turn this off if you do not want clamping
    if (clamping == "clamp_growth"){
      gdp_temp_data1 <- left_join(gdp_temp_data1,
                                  minmax_data,
                                  by = c("Income.group" = "Income.group"))
      
    gdp_temp_data1 <- gdp_temp_data1 %>% 
      dplyr::mutate(diff_lgdp = case_when(diff_lgdp > max_dg ~ max_dg,
                                          diff_lgdp < min_dg ~ min_dg,
                                          TRUE ~ diff_lgdp))
  }
    
    # unlist...
    if (temp_dataset == "ERA"){
      gdp_temp_data1$delta_g_era <- unlist(gdp_temp_data1$delta_g_era)
      # now let us calculate adjusted growht rate by adding deltaG to observed growth
      gdp_temp_data1$adj_growth <- (gdp_temp_data1$delta_g_era + gdp_temp_data1$diff_lgdp)
      
    }
    if (temp_dataset == "CRU"){
      gdp_temp_data1$delta_g_cru <- unlist(gdp_temp_data1$delta_g_cru)
      # now let us calculate adjusted growht rate by adding deltaG to observed growth
      gdp_temp_data1$adj_growth <- (gdp_temp_data1$delta_g_cru + gdp_temp_data1$diff_lgdp)
      
    }
    
    
    # let us add 1 to growth variables so we can calculate cumulative growth
    gdp_temp_data1$diff_lgdp <- gdp_temp_data1$diff_lgdp + 1
    gdp_temp_data1$adj_growth <- gdp_temp_data1$adj_growth + 1
    #year_k <- 1990  
    # now let us set GDP at year k as initial gdp number to calculate impact of 
    # added temoerature
    # gdp_temp_data1 <- subset(gdp_temp_data1, !is.na(coef_id))
    #year_k <- 1990
    
    gdp_temp_data1 <- gdp_temp_data1 %>% dplyr::group_by(ISO3) %>% 
      dplyr::mutate(gdp_year = NY.GDP.PCAP.KD[year == year_k])
    
    gdp_temp_data1 <- subset(gdp_temp_data1, year >= year_k)
    
    damages_i_t4 <- gdp_temp_data1 %>% dplyr::group_by(ISO3) %>% 
      dplyr::mutate(cum_adj_growthz = cumprod(adj_growth),
                    cum_growth_real = cumprod(diff_lgdp))
    
    # finally comput edamages...
    #  damages_i_t4 <- damages_i_t4 %>% 
    #    dplyr::mutate(damages = (gdp_year * cum_adj_growthz) - (gdp_year * cum_growth_real))
    
    damages_i_t4 <- damages_i_t4 %>% 
      dplyr::mutate(gdp_noemms = (gdp_year * cum_adj_growthz),
                    gdp_ssp370 = (gdp_year * cum_growth_real),
                    damages = gdp_noemms - gdp_ssp370)
    
    #  bra <- subset(damages_i_t4, ISO3 == "BRA")
    
    #  plot(bra$year, bra$diff_lgdp_for_damages)
  #  lines()
    
    #damages_i_t4$gdp <- (damages_i_t4$gdp_year * damages_i_t4$cum_growth_real)
    #experiment_year <- unique(damages_i_t4$exp_yr)
    
    damages_i_t4 <- ungroup(damages_i_t4)
    
    damages_i_t4 <- damages_i_t4 %>% 
      dplyr::mutate(average_gdp_2020 = mean(gdp_ssp370[year == 2020], na.rm = T))
    
    damages_i_t4 <- damages_i_t4 %>% 
      dplyr::group_by(year) %>% 
      dplyr::mutate(average_gdp = mean(gdp_ssp370, na.rm = T))
                    
    # calculate discounted damages for past and for future (note different 
    #- processes for discounting past damages and future damages)
    damages_i_t4 <- damages_i_t4 %>% 
      dplyr::mutate(t_since_k = year - year_k,
                    t_since_today = year - 2020,
                    weighted_damages1 = case_when(year <= 2020 ~ (damages*((1+(0.01))^t_since_k)),
                                                  year > 2020 ~ (damages*(1/(1+(0.01))^t_since_today))),
                    weighted_damages2 = case_when(year <= 2020 ~ (damages*((1+(0.02))^t_since_k)),
                                                  year > 2020 ~ (damages*(1/(1+(0.02))^t_since_today))),
                    weighted_damages3 = case_when(year <= 2020 ~ (damages*((1+(0.03))^t_since_k)),
                                                  year > 2020 ~ (damages*(1/(1+(0.03))^t_since_today))),
                    weighted_damages5 = case_when(year <= 2020 ~ (damages*((1+(0.05))^t_since_k)),
                                                  year > 2020 ~ (damages*(1/(1+(0.05))^t_since_today))),
                    weighted_damages7 = case_when(year <= 2020 ~ (damages*((1+(0.07))^t_since_k)),
                                                  year > 2020 ~ (damages*(1/(1+(0.07))^t_since_today))),      
                    weighted_damages_ramsey = case_when(year <= 2020 ~ (damages*((1+(0.02))^t_since_k)),
                                                        year > 2020 ~ damages*((1/(1.002^(t_since_today)))*((average_gdp/average_gdp_2020)^(-1.24)))),
                    ramsey_discount = ((1/(1.002^(t_since_today)))*((average_gdp/average_gdp_2020)^(-1.24))))
    
    #  damages_i_t4 <- damages_i_t4 %>% 
    #  dplyr::mutate(t_since_k = year - year_k,
    #        t_since_today = year - experiment_year,
    #         weighted_damages2 = case_when(year <= 2020 ~ (damages*((1+(0.02))^t_since_k)),
    #                                        year > 2020 ~ (damages*(1/(1+(0.02))^t_since_today))),
    #           weighted_damages3 = case_when(year <= 2020 ~ (damages*((1+(0.03))^t_since_k)),
    #                                          year > 2020 ~ (damages*(1/(1+(0.03))^t_since_today))),
    #             weighted_damages5 = case_when(year <= 2020 ~ (damages*((1+(0.05))^t_since_k)),
    #                                            year > 2020 ~ (damages*(1/(1+(0.05))^t_since_today))),
    #               weighted_damages7 = case_when(year <= 2020 ~ (damages*((1+(0.07))^t_since_k)),
    #                                              year > 2020 ~ (damages*(1/(1+(0.07))^t_since_today))))
    
    #  damages_i_t4 <- subset(damages_i_t4, !is.na(coef_id))
    
    # damages_i_t4 <- subset(damages_i_t4, ISO3 != i)
    
    # scale by population
    damages_i_t4$damages_pop <- damages_i_t4$damages * damages_i_t4$SP.POP.TOTL
    damages_i_t4 <- damages_i_t4 %>% 
      dplyr::mutate(weighted_damages1_scld = weighted_damages1 *SP.POP.TOTL,
                    weighted_damages2_scld = weighted_damages2 *SP.POP.TOTL,
                    weighted_damages3_scld = weighted_damages3 *SP.POP.TOTL,
                    weighted_damages5_scld = weighted_damages5 *SP.POP.TOTL,
                    weighted_damages7_scld = weighted_damages7 *SP.POP.TOTL,
                    weighted_damages_ramsey_scld = weighted_damages_ramsey *SP.POP.TOTL,
                    gdp_noemms_scld = gdp_noemms *SP.POP.TOTL,
                    gdp_ssp370_scld = gdp_ssp370 *SP.POP.TOTL)
    
    damages_i_t4$emitter <- i
    
    #sum(damages_i_t4$weighted_damages_ramsey_scld, na.rm = T)
    
  #  sum(damages_i_t4$weighted_damages2_scld[damages_i_t4$year <= 2100 & damages_i_t4$weighted_damages2_scld > 0 ], na.rm = T)
    
#    damages_summ <- damages_i_t4 %>% dplyr::group_by(emitter, year) %>% 
 #     dplyr::summarise(total_damages = sum(damages_pop, na.rm = T),
  #                     total_damages2 = sum(weighted_damages2_scld, na.rm = T),
   #                    total_damages5 = sum(weighted_damages5_scld, na.rm = T),
    #                   total_damages7 = sum(weighted_damages7_scld, na.rm = T),
     #                  .groups = "keep")
    
    toc()
    
    # add to data frame
    mother_df <- rbind(mother_df, damages_i_t4)
    
  }
  return(mother_df)
}

# end of script


#write_rds(total_damages_cc, "~/Desktop/total_damages_cc.rds")
