##############################################################################
# Mustafa Zahid, Jan 23, 2023
# This r script will produce the SCC estimates for various models under 
# different assumptions 
##############################################################################
# read world bank data to be used later on in calculation for GDP and POP
wdi_dat <- wb_data(indicator = c("NY.GDP.PCAP.KD", "SP.POP.TOTL", "NY.GDP.PCAP.CD", "NY.GDP.PCAP.PP.KD"),
                   country = "countries_only",
                   start_date = 1960, end_date = 2020,
                   return_wide = T)


# first let us readin the world bank gdp and classification data and 
# categorize the countries, take the averages, and finally record the 
# minimum and maximum for each of the income groups. Then we can 
# combine with the SSP growth data and clamp the growth on both ends 
# by income group. This will then go through the calculation script. There 
# we need to make sure we are also constraining the counterfactual grwoth 
# rate from giong beyond the set bounds 
income_class <- read_csv("~/Downloads/data-XHzgJ.csv")
income_class <- income_class %>% dplyr::select(c("Country", "Income group"))
income_class$ISO3 <- countrycode::countrycode(income_class$Country, 
                                              origin = "country.name",
                                              destination = "iso3c")
#income_class$Country[income_class$Country == "Turkey"] <- "Turkiye"

wdi_dat <- left_join(wdi_dat,
                     income_class,
                     by = c("iso3c" = "ISO3"))

colnames(wdi_dat)[4] <- "year"


wdi_dat <- subset(wdi_dat, year < 2021)
# create the growth variable and set the dataset as a pdataframe
wdi_dat$gdp_pc <- log(wdi_dat$NY.GDP.PCAP.KD)
wdi_dat <- plm::pdata.frame(wdi_dat, index = c("iso3c","year"))
wdi_dat$lgdp_pc <- plm::lag(wdi_dat$gdp_pc)
wdi_dat$diff_lgdp <- wdi_dat$gdp_pc - wdi_dat$lgdp_pc

wdi_dat <- wdi_dat %>% dplyr::group_by(iso3c) %>% 
  dplyr::mutate(missing = case_when(is.na(diff_lgdp) ~ 0,
                                    TRUE ~ 1))
wdi_dat <- wdi_dat %>% dplyr::group_by(iso3c) %>% 
  dplyr::mutate(missing_tot = sum(missing ,na.rm = T))
wdi_dat <- subset(wdi_dat, missing_tot >= 10)
average_data <- wdi_dat %>% 
  dplyr::group_by(iso3c, Income.group) %>% 
  dplyr::summarise(diff_lgdp = mean(diff_lgdp, na.rm = T))
average_data <- ungroup(average_data)
minmax_data <- average_data %>% dplyr::group_by(Income.group) %>% 
  dplyr::summarize(min_dg = min(diff_lgdp, na.rm = T),
                max_dg = max(diff_lgdp, na.rm = T),
                p_90 = quantile(diff_lgdp, 0.90),
                p_10 = quantile(diff_lgdp, 0.10))

minmax_data <- minmax_data %>% dplyr::group_by(Income.group) %>% 
  dplyr::summarize(min_dg = mean(min_dg, na.rm = T),
                max_dg = mean(max_dg, na.rm = T))


minmax_data <- subset(minmax_data, !is.na(Income.group))


minmax_data <- minmax_data %>% dplyr::select(c("iso3c", "Income.group", 
                                               "min_dg", "max_dg", "p_90"))

# now let us merge in with the forecast data 
future_forecast_ssp370_2300_adj <- left_join(future_forecast_ssp370_2300,
                                             minmax_data,
                                             by = c("ISO3" = "iso3c"))

# now let us establish the clamping on observed growth rate. Basically 
# we are gonna say in our future projection sample we are not going to allow 
# growth to go out of bounds. 
future_forecast_ssp370_2300_adj <- future_forecast_ssp370_2300_adj %>% 
  dplyr::mutate(diff_lgdp = case_when(diff_lgdp > p_90 ~ p_90,
                                      diff_lgdp < min_dg ~ min_dg,
                                      TRUE ~ diff_lgdp))

hist(future_forecast_ssp370_2300_adj$diff_lgdp)
summary(future_forecast_ssp370_2300_adj$diff_lgdp)