##############################################################################
# Mustafa Zahid, April 1st, 2022
# This script calculates the total damages bore by a specified pulse of emissions 
# for example 1gtco2 pulse with BHM model under 5 lags specification. 
# Last edited: November 2024 to handle multiple lagd
#############################################################################

run_bhm_model_reg_lags <- function(bhm_mode, max_lag = 10) {
  bhm_mode_option <- bhm_mode
  
  # Load and prepare data
  gdp_temp_data <- readRDS("data/processed/world_gdp_pop/temp_gdp_world_panel.rds")
  gdp_temp_data <- subset(gdp_temp_data, year < 2021)
  gdp_temp_data$gdp_pc <- log(gdp_temp_data$NY.GDP.PCAP.KD)
  gdp_temp_data <- plm::pdata.frame(gdp_temp_data, index = c("ISO3", "year"))
  gdp_temp_data$lgdp_pc <- plm::lag(gdp_temp_data$gdp_pc)
  gdp_temp_data$diff_lgdp <- gdp_temp_data$gdp_pc - gdp_temp_data$lgdp_pc
  gdp_temp_data$year2 <- as.numeric(gdp_temp_data$year)^2
  
  # Create lagged variables up to `max_lag`
  for (lag in 1:max_lag) {
    gdp_temp_data[[paste0("era_mwtemp_l", lag)]] <- plm::lag(gdp_temp_data$era_mwtemp, lag)
    gdp_temp_data[[paste0("era_mwprecip_l", lag)]] <- plm::lag(gdp_temp_data$era_mwprecip, lag)
  }
  
  # Calculate poor identifiers
  gdp_temp_data <- gdp_temp_data %>%
    dplyr::mutate(median_gdp = median(NY.GDP.PCAP.KD, na.rm = TRUE)) %>%
    dplyr::group_by(ISO3) %>%
    dplyr::mutate(avg_gdp = mean(NY.GDP.PCAP.KD, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(poor = ifelse(avg_gdp < median_gdp, 1, 0))
  
  # Subset data for specific model
  selected_vars <- c("ISO3", "year", "era_mwtemp", "era_mwprecip", "diff_lgdp", "year2")
  for (lag in 1:max_lag) {
    selected_vars <- c(selected_vars, paste0("era_mwtemp_l", lag), paste0("era_mwprecip_l", lag))
  }
  gdp_temp_data <- gdp_temp_data %>% dplyr::select(all_of(selected_vars))
  
  # Construct the formula with lag terms
  lagged_terms <- paste(
    sapply(1:max_lag, function(lag) {
      paste0("era_mwtemp_l", lag, " + era_mwtemp_l", lag, "^2 + ",
             "era_mwprecip_l", lag, " + era_mwprecip_l", lag, "^2")
    }), collapse = " + "
  )
  
  formula <- as.formula(
    paste("diff_lgdp ~ era_mwtemp + era_mwtemp^2 + era_mwprecip + era_mwprecip^2 +",
          lagged_terms, "+ ISO3*as.numeric(year) + ISO3*year2 | ISO3 + year")
  )
  
  # Run the model based on mode
  if (bhm_mode_option == "pooled") {
    bhm_era_reg <- fixest::feols(formula, gdp_temp_data)
  } else if (bhm_mode_option == "richpoor") {
    bhm_era_reg <- fixest::feols(formula, gdp_temp_data)
    
    # Estimate the response for every country-year based on richpoor categorization
    gdp_temp_data <- gdp_temp_data %>%
      dplyr::mutate(response_tempactual_era = ifelse(
        poor == 1,
        (era_mwtemp * coef(bhm_era_reg)[1] + era_mwtemp^2 * coef(bhm_era_reg)[2]) +
          (era_mwtemp * coef(bhm_era_reg)[5] + era_mwtemp^2 * coef(bhm_era_reg)[6]),
        (era_mwtemp * coef(bhm_era_reg)[1] + era_mwtemp^2 * coef(bhm_era_reg)[2])
      ))
  }
  
  return(bhm_era_reg)
}

# end of script