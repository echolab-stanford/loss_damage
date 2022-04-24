##############################################################################
# Mustafa Zahid, April 1st, 2022
# This script calculates the total damages owed by emitter country j to receiver 
# country i 
#############################################################################

#function for calculating damages 
calc_damages_all_ensembles <- function(dataset_dg, 
                                       dataset_fair,
                                       dg_variable,
                                       gdp_year, 
                                       gdp_var_year, 
                                       variable) {
  dataset_dg <- dataset_dg %>% dplyr::mutate(test_id = 1,
                                              year = as.numeric(year))
  yeari <- as.numeric(substr(gdp_year, 9, 12))
  dataset_fair <- dataset_fair %>% dplyr::group_by(iso, year) %>% 
    dplyr::summarise(phi = mean(phi, na.rm = T),
                     .groups = "keep")
  dataset_fair <- dataset_fair %>% dplyr::mutate(test_id = 1) 
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
  damages_i_t4 <- damages_i_t3 %>% dplyr::group_by(ISO3, iso) %>% 
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
                                                   "cum_growth_real"))
  damages_i_t4 <- damages_i_t4 %>% dplyr::mutate(damages = ({{gdp_var_year}} * cum_adj_growthz) - 
                                                   ({{gdp_var_year}} * cum_growth_real))
  damages_i_t4 <- damages_i_t4 %>% dplyr::mutate(t_since_k = year - yeari,
                             weighted_damages2 = damages*((1+(0.02))^t_since_k),
                             weighted_damages3 = damages*((1+(0.03))^t_since_k),
                             weighted_damages5 = damages*((1+(0.05))^t_since_k),
                             weighted_damages7 = damages*((1+(0.06))^t_since_k))
}

#end of script

