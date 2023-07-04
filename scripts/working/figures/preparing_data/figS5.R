##############################################################################
# Mustafa Zahid, November 15th, 2022
# This R script brings in the SCC numbers and calculate the carbon debt for 
# specific emitters. the groups of emissions data include carbon majors and 
# celebreities private jet usage
# Input(s): processed SCC data per year of emmission
# Output(s): cumulative carbon debt to the end of century
##############################################################################
remove(list=ls())
gc()
sf::sf_use_s2(FALSE)
setwd("~/GitHub/loss_damage")

# read in the needed libraries 
source("scripts/working/analysis/0_read_libs.R")
options(scipen = 999)

setwd(dropbox_path)
#############################################################################
#############################################################################
# read data 
# now let us read in the emissions data
carb_majors_jet <- read_csv(paste0(raw_path, "emissions/carbon_majors_emms.csv"))
celebs_networth <- read_csv(paste0(raw_path, "emissions/celeb_networth.csv"))
# now let us read in the emissions data
sportsjet <- readRDS(paste0(processed_path, "ind_ent_emms/sportsjets.rds"))
celebsjet <- readRDS(paste0(processed_path, "ind_ent_emms/celebjets.rds"))
elonjet <-   readRDS(paste0(processed_path, "ind_ent_emms/elonmuskjet.rds"))
gatesjet <-  readRDS(paste0(processed_path, "ind_ent_emms/billgatesjet.rds"))
bezosjet <-  readRDS(paste0(processed_path, "ind_ent_emms/jeffbezosjet.rds"))
corpsjet <-  readRDS(paste0(processed_path, "ind_ent_emms/corpjets.rds"))

# bring the celeb emissions data set
celebs_jet_emissions <- rbind(celebsjet,
                              elonjet,
                              gatesjet,
                              bezosjet)
# now let us read in the 1gtc/yr exp data 
damages_per_1tco2yr <- readRDS(paste0(output_path, "20230628/total_damages_1tco2_1980_2022.rds"))

#sum(damages_per_1tco2yr$weighted_damages2_scld[damages_per_1tco2yr$emitter == 2020], na.rm = T)

#############################################################################
#############################################################################
# prep data 

################################################################################ majors emissions
#take out non-entity parties 
carb_majors_jet <- carb_majors_jet %>% 
  subset(., emitter != "China (Coal)" & emitter != "Russia (Coal)" & emitter != "Poland Coal")


#damages_per_1tco2yr <- total_damages_1tCO2
# now let calculate total carbon deby by year and emitter
damages_per_1tco2yr_sum <- damages_per_1tco2yr %>% 
  dplyr::group_by(emitter, year) %>% 
  dplyr::summarise(total_damages = sum(weighted_damages2_scld, na.rm = T))
colnames(damages_per_1tco2yr_sum)[1] <- "emission_year"

# now we have the estimates for each 1tco2 / yr, let us bring in the 
carb_majors_jet <- subset(carb_majors_jet, years >= 1988 & years <= 2015)
carb_majors_jet <- left_join(carb_majors_jet, damages_per_1tco2yr_sum,
                             by = c("years" = "emission_year"),
                             relationship = "many-to-many")
carb_majors_jet1 <- carb_majors_jet %>% 
  dplyr::mutate(debt = (emissions*1000000) * total_damages) %>% 
  dplyr::group_by(years, year, emitter) %>% 
  dplyr::summarise(total_debt = sum(debt, na.rm = T))

# now let us calculate total debt by year and emitter
carb_majors_jet2 <- carb_majors_jet1 %>% 
  dplyr::group_by(year, emitter) %>% 
  dplyr::summarise(total_debt = sum(total_debt, na.rm = T)) 

# noe let us calculate cumulative debt 
carb_majors_jet2 <- carb_majors_jet2 %>% 
  dplyr::group_by(emitter) %>% 
  dplyr::mutate(total_debt_cum = cumsum(total_debt))

# we want to ficus on the top 15, since there are 100 of them
total_by_emitter <- carb_majors_jet2 %>% dplyr::group_by(emitter) %>% 
  dplyr::summarise(total_debt = sum(total_debt, na.rm = T))
total_by_emitter <- total_by_emitter[order(-total_by_emitter$total_debt),] 
total_by_emitter$rank <- 1:nrow(total_by_emitter)
total_by_emitter <- total_by_emitter %>% dplyr::select(c("emitter", "rank"))
carb_majors_jet2 <- left_join(total_by_emitter, carb_majors_jet2, by = c("emitter"))
carb_majors_jet2 <- subset(carb_majors_jet2, rank <= 17)
carb_majors_jet2 <- subset(carb_majors_jet2)

# let us try tp plot a bar plot 
total_carb_majors_jet <- subset(carb_majors_jet2, year == 2100)
carb_majors_jet2_2020 <- subset(carb_majors_jet2, year == 2020)
carb_majors_jet2_2020 <- carb_majors_jet2_2020 %>% dplyr::select(c("emitter", 
                                                                   "total_debt_cum"))
colnames(carb_majors_jet2_2020) <- c("emitter", "total_debt_cum_2020")
total_carb_majors_jet <- left_join(total_carb_majors_jet,
                                   carb_majors_jet2_2020,
                                   by = c("emitter"))

total_carb_majors_jet <- total_carb_majors_jet[order(total_carb_majors_jet$total_debt_cum),] 
total_carb_majors_jet$emitter <- factor(total_carb_majors_jet$emitter, levels = total_carb_majors_jet$emitter)

total_carb_majors_jet$total_debt_cum <- total_carb_majors_jet$total_debt_cum / 1000000000000
total_carb_majors_jet$total_debt_cum_2020 <- total_carb_majors_jet$total_debt_cum_2020 / 1000000000000 

# first we are going to read in all the revenus
oil_majors_rev <- read_csv(paste0(raw_path, "emissions/NRGI-NOCdatabase-ExploreByIndicator-2.csv"))

oil_majors_rev <- oil_majors_rev %>% dplyr::select(c("company",
                                                     "2011",
                                                     "2012",
                                                     "2013",
                                                     "2014",
                                                     "2015",
                                                     "2016",
                                                     "2017",
                                                     "2018",
                                                     "2019",
                                                     "2020",
                                                     "2021"))

oil_majors_rev_long <- reshape2::melt(oil_majors_rev, id = c("company"))
oil_majors_rev_long <- subset(oil_majors_rev_long, !is.na(oil_majors_rev_long$value))
oil_majors_rev_long$value <- oil_majors_rev_long$value * 1000000

# now let us read the american companies data 
exxon_oil_rev <- read_excel("~/Downloads/statistic_id264119_exxonmobils-operating-revenue-2001-2021.xlsx", 
                            sheet = 2)
exxon_oil_rev <- exxon_oil_rev[-1:-2,]
colnames(exxon_oil_rev) <- c("year", "rev")
exxon_oil_rev$rev <- exxon_oil_rev$rev * 1000000
exxon_oil_rev$company <- "ExxonMobil"

chevron_oil_rev <- read_excel("~/Downloads/statistic_id269079_chevrons-operating-revenue-2008-2021.xlsx",
                              sheet = 2)
chevron_oil_rev <- chevron_oil_rev[-1:-2,]
colnames(chevron_oil_rev) <- c("year", "rev")
chevron_oil_rev$rev <- chevron_oil_rev$rev * 1000000000
chevron_oil_rev$company <- "Chevron"

shell_oil_rev <- read_excel("~/Downloads/statistic_id268734_shells-revenue-2005-2021.xlsx",
                            sheet = 2)
shell_oil_rev <- shell_oil_rev[-1:-2,]
colnames(shell_oil_rev) <- c("year", "rev")
shell_oil_rev$rev <- shell_oil_rev$rev * 1000000000
shell_oil_rev$company <- "Shell Oil"

# now let us bring all american companies data together in one dataframe before 
# bringing in them with the rest of the companies
american_oil_revs <- rbind(exxon_oil_rev,
                           shell_oil_rev,
                           chevron_oil_rev)

# now let us see
american_oil_revs_2021 <- subset(american_oil_revs, year == 2021) 
colnames(oil_majors_rev_long) <- c("company", "year", "rev")

# now bring all together 
oil_majors_rev_long <- rbind(oil_majors_rev_long,
                             american_oil_revs_2021)

# select year
oil_majors_rev_2021 <- subset(oil_majors_rev_long, year == 2021)

# now in order to bring both datasets together we need to make syre that the names 
# corespond, and as such, we will rename the columns in the revenue dataset 
# to the damages dataset 
oil_majors_rev_2021 <- oil_majors_rev_2021 %>% 
  dplyr::mutate(company = case_when(company == "Saudi Aramco" ~ "Saudi Arabian Oil Company (Aramco)",
                                    company == "Gazprom" ~ "Gazprom OAO",
                                    company == "ExxonMobil"~ "ExxonMobil Corp",
                                    company == "Pemex" ~ "Petroleos Mexicanos (Pemex)",
                                    company == "Shell Oil" ~ "Royal Dutch Shell PLC",
                                    company == "CNPC" ~ "China National Petroleum Corp (CNPC)",
                                    company == "Chevron" ~ "Chevron Corp",
                                    TRUE ~ company)) 

oil_majors_rev_2021 <- oil_majors_rev_2021 %>% dplyr::select(-c("year"))
total_carb_majors_jet <- left_join(total_carb_majors_jet,
                                   oil_majors_rev_2021,
                                   by = c("emitter"= "company"))

total_carb_majors_jet$rev[total_carb_majors_jet$emitter == "BP PLC"] <- 164200000000
total_carb_majors_jet$rev[total_carb_majors_jet$emitter == "Royal Dutch Shell PLC"] <- 261500000000
total_carb_majors_jet$rev[total_carb_majors_jet$emitter == "Poland Coal"] <- 15000000000
total_carb_majors_jet$rev[total_carb_majors_jet$emitter == "Coal India"] <- 75000000000
total_carb_majors_jet$rev[total_carb_majors_jet$emitter == "China (Coal)"] <- 600000000000
total_carb_majors_jet$rev[total_carb_majors_jet$emitter == "Russia (Coal)"] <- 46800000000
total_carb_majors_jet$rev[total_carb_majors_jet$emitter == "National Iranian Oil Co"] <- 76450000000
total_carb_majors_jet$rev[total_carb_majors_jet$emitter == "Petroleos de Venezuela SA (PDVSA)"] <- 48000000000
total_carb_majors_jet$rev[total_carb_majors_jet$emitter == "Abu Dhabi National Oil Co"] <- 103600000000
#https://www.macrotrends.net/stocks/charts/BHP/bhp-group/revenue#:~:text=BHP%20Group%20revenue%20for%20the%20twelve%20months%20ending%20December%2031,a%2032.59%25%20increase%20from%202020.
total_carb_majors_jet$rev[total_carb_majors_jet$emitter == "BHP Billiton Ltd"] <- 56921000000
#https://www.upstreamonline.com/finance/kuwait-poised-for-decade-high-oil-revenues-amid-political-turmoil/2-1-1394384
total_carb_majors_jet$rev[total_carb_majors_jet$emitter == "Kuwait Petroleum Corp"] <- 52720000000
#https://sonatrach.com/wp-content/uploads/2022/11/RAPPORT-ANNUEL-2021-An.pdf
total_carb_majors_jet$rev[total_carb_majors_jet$emitter == "Sonatrach SPA"] <- 35400000000
#https://www.macrotrends.net/stocks/charts/TTE/totalenergies-se/revenue#:~:text=TotalEnergies%20SE%20revenue%20for%20the%20twelve%20months%20ending%20March%2031,a%2054.24%25%20increase%20from%202020.
total_carb_majors_jet$rev[total_carb_majors_jet$emitter == "Total SA"] <- 184634000000
#https://www.macrotrends.net/stocks/charts/BTU/peabody-energy/revenue
total_carb_majors_jet$rev[total_carb_majors_jet$emitter == "Peabody Energy Corp"] <- 3310000000

# Ok now that we have all the estoimated revenue for the top 15 companies, let us
#rename the emitter column to include the % of damages (revenue/damages)
total_carb_majors_jet$rev <- total_carb_majors_jet$rev / 1000000000000

# ok now let us do the math 
total_carb_majors_jet$pct <- total_carb_majors_jet$total_debt_cum / total_carb_majors_jet$rev
total_carb_majors_jet$pct_2020 <- total_carb_majors_jet$total_debt_cum_2020 / total_carb_majors_jet$rev

total_carb_majors_jet_ex3 <- total_carb_majors_jet

total_carb_majors_jet_ex3 <- total_carb_majors_jet_ex3 %>% 
  dplyr::mutate(emitter = case_when(emitter == "Saudi Arabian Oil Company (Aramco)" ~ paste0(emitter, 
                                                                                             " (Total 2021 Revenue = $", 
                                                                                             round(rev, 2), "T, ",
                                                                                             "% of 2021 Revenue = %", round(pct_2020*100, 0), ")"),
                                    is.na(rev) ~ paste0(emitter, " "),
                                    TRUE ~ paste0(emitter, " ($", round(rev, 2), "T, %",
                                                  round(pct_2020*100, 0), ")")))

################################################################################ celebs emissions 

celebs_jet_emissions <- as.data.frame(unique(celebs_jet_emissions$emitter))
celebs_jet_emissions$`unique(celebs_jet_emissions$emitter)` <- gsub("'s",'',celebs_jet_emissions$`unique(celebs_jet_emissions$emitter)`)
celebs_jet_emissions$`unique(celebs_jet_emissions$emitter)` <- gsub("jet",'',celebs_jet_emissions$`unique(celebs_jet_emissions$emitter)`)
colnames(celebs_jet_emissions)[1] <- "emitter_name"

# let us do private jets 
#damages_per_1tco2yr_sum$total_damages <- damages_per_1tco2yr_sum$total_damages / 1000000000

celebsjet <- subset(celebsjet, years == 2022)
celebsjet <- left_join(celebsjet, damages_per_1tco2yr_sum,
                       by = c("years" = "emission_year"))

celebsjet1 <- celebsjet %>% 
  dplyr::mutate(debt = emissions * total_damages) %>% 
  dplyr::group_by(years, year, emitter) %>% 
  dplyr::summarise(total_debt = sum(debt, na.rm = T))

celebsjet2 <- celebsjet1 %>% 
  dplyr::group_by(year, emitter) %>% 
  dplyr::summarise(total_debt = sum(total_debt, na.rm = T))

# let us do private jets 
elonjet <- subset(elonjet, years == 2022)
elonjet <- left_join(elonjet, damages_per_1tco2yr_sum,
                     by = c("years" = "emission_year"))

elonjet1 <- elonjet %>% 
  dplyr::mutate(debt = emissions * total_damages) %>% 
  dplyr::group_by(years, year, emitter) %>% 
  dplyr::summarise(total_debt = sum(debt, na.rm = T))

elonjet2 <- elonjet1 %>% 
  dplyr::group_by(year, emitter) %>% 
  dplyr::summarise(total_debt = sum(total_debt, na.rm = T))


# let us do private jets 
bezosjet <- subset(bezosjet, years== 2022)
bezosjet <- left_join(bezosjet, damages_per_1tco2yr_sum,
                      by = c("years" = "emission_year"))

bezosjet1 <- bezosjet %>% 
  dplyr::mutate(debt = emissions * total_damages) %>% 
  dplyr::group_by(years, year, emitter) %>% 
  dplyr::summarise(total_debt = sum(debt, na.rm = T))

bezosjet2 <- bezosjet1 %>% 
  dplyr::group_by(year, emitter) %>% 
  dplyr::summarise(total_debt = sum(total_debt, na.rm = T))

# let us do private jets 
corpsjet <- subset(corpsjet, years == 2022)
corpsjet <- left_join(corpsjet, damages_per_1tco2yr_sum,
                      by = c("years" = "emission_year"))

corpsjet1 <- corpsjet %>% 
  dplyr::mutate(debt = emissions * total_damages) %>% 
  dplyr::group_by(years, year, emitter) %>% 
  dplyr::summarise(total_debt = sum(debt, na.rm = T))

corpsjet2 <- corpsjet1 %>% 
  dplyr::group_by(year, emitter) %>% 
  dplyr::summarise(total_debt = sum(total_debt, na.rm = T))

corpsjet2 <- corpsjet2 %>% 
  dplyr::group_by(emitter) %>% 
  dplyr::mutate(total_debt_cum = cumsum(total_debt))


# let us do private jets 
sportsjet <- subset(sportsjet, years == 2022)
sportsjet <- left_join(sportsjet, damages_per_1tco2yr_sum,
                       by = c("years" = "emission_year"))

sportsjet1 <- sportsjet %>% 
  dplyr::mutate(debt = emissions * total_damages) %>% 
  dplyr::group_by(years, year, emitter) %>% 
  dplyr::summarise(total_debt = sum(debt, na.rm = T))

sportsjet2 <- sportsjet1 %>% 
  dplyr::group_by(year, emitter) %>% 
  dplyr::summarise(total_debt = sum(total_debt, na.rm = T))

sportsjet2 <- sportsjet2 %>% 
  dplyr::group_by(emitter) %>% 
  dplyr::mutate(total_debt_cum = cumsum(total_debt))


# ok now we have the numbers for every year and every emitter, we can 
# construct the figures 

# let us do private jets 
gatesjet <- subset(gatesjet, years == 2022)
gatesjet <- left_join(gatesjet, damages_per_1tco2yr_sum,
                      by = c("years" = "emission_year"))

gatesjet1 <- gatesjet %>% 
  dplyr::mutate(debt = emissions * total_damages) %>% 
  dplyr::group_by(years, year, emitter) %>% 
  dplyr::summarise(total_debt = sum(debt, na.rm = T))

gatesjet2 <- gatesjet1 %>% 
  dplyr::group_by(year, emitter) %>% 
  dplyr::summarise(total_debt = sum(total_debt, na.rm = T))

# let us do co author marshall burke flights
emitter <- c("Marshall Burke's Flights")
years <- c(1990:2100)
total_emms <- c(0) 
mburkedf <- data.frame(emitter, years, total_emms)

################################################################################
# let us do private jets 
celebsjet <- subset(celebsjet, years == 2022) 
mburkedf <- left_join(mburkedf, damages_per_1tco2yr_sum,
                      by = c("years" = "emission_year"))

mburkedf$total_emms[mburkedf$years == 2022] <- 4.5
mburkedf1 <- mburkedf %>% 
  dplyr::mutate(debt = total_emms * total_damages) %>% 
  dplyr::group_by(years, year, emitter) %>% 
  dplyr::summarise(total_debt = sum(debt, na.rm = T))

mburkedf2 <- mburkedf1 %>% 
  dplyr::group_by(year, emitter) %>% 
  dplyr::summarise(total_debt = sum(total_debt, na.rm = T))

allcelebs <- rbind(celebsjet2,
                   gatesjet2,
                   bezosjet2,
                   elonjet2,
                   mburkedf2)

# we want to ficus on the top 15, since there are 100 of them
total_by_emitter <- allcelebs %>% dplyr::group_by(emitter) %>% 
  dplyr::summarise(total_debt = sum(total_debt, na.rm = T))
total_by_emitter <- total_by_emitter[order(-total_by_emitter$total_debt),] 
total_by_emitter$rank <- 1:nrow(total_by_emitter)
total_by_emitter <- total_by_emitter %>% dplyr::select(c("emitter", "rank"))
allcelebs <- left_join(total_by_emitter, allcelebs, by = c("emitter"))

mburke22 <- subset(allcelebs, emitter == "Marshall Burke's Flights")
allcelebs <- subset(allcelebs, rank <= 15)
allcelebs <- rbind(allcelebs, mburke22)

# noe let us calculate cumulative debt 
allcelebs <- allcelebs %>% 
  dplyr::group_by(emitter) %>% 
  dplyr::mutate(total_debt_cum = cumsum(total_debt))

all_celebs_tot <- subset(allcelebs, year == 2100)



colnames(celebs_networth)[2] <- "emitter"
celebs_networth <- celebs_networth %>% dplyr::select(c("emitter", 
                                                       "net_worth"))
celebs_networth$emitter <- str_to_title(celebs_networth$emitter)
celebs_networth$emitter[celebs_networth$emitter == "Jay-Z"] <- "Puma/Jay-Z"
celebs_networth$emitter[celebs_networth$emitter == "Alex Rodriguez"] <- "A-Rod"
all_celebs_tot$emitter <- trimws(all_celebs_tot$emitter, which = c("right"))
all_celebs_tot$emitter <- gsub("'s",'',all_celebs_tot$emitter)
all_celebs_tot$emitter <- gsub("jet",'',all_celebs_tot$emitter)

all_celebs_tot <- left_join(all_celebs_tot,
                            celebs_networth,
                            by = c("emitter"))
all_celebs_tot$emitter <- trimws(all_celebs_tot$emitter, which = c("right"))

all_celebs_tot$net_worth[all_celebs_tot$emitter == "Jeff Bezos"] <- 121400000000
all_celebs_tot$net_worth[all_celebs_tot$emitter == "Elon Musk"] <- 1.60e+11
all_celebs_tot$net_worth[all_celebs_tot$emitter == "Bill Gates"] <- 1.31e+11

all_celebs_tot <- all_celebs_tot %>% 
  dplyr::mutate(emitter = case_when(emitter == "Marshall Burke Flights" ~ paste0(emitter, " ($1.1)"),
                                    TRUE ~ emitter))


all_celebs_tot <- all_celebs_tot[order(all_celebs_tot$total_debt_cum),] 
all_celebs_tot$emitter <- factor(all_celebs_tot$emitter, levels = all_celebs_tot$emitter)
all_celebs_tot$total_debt_cum <- all_celebs_tot$total_debt_cum / 1000
all_celebs_tot$net_worth <- all_celebs_tot$net_worth / 1000
all_celebs_tot$pct <- all_celebs_tot$total_debt_cum / all_celebs_tot$net_worth
all_celebs_tot$pct <- all_celebs_tot$pct * 100
all_celebs_tot$emitter <- paste0(all_celebs_tot$emitter, " (%", round(all_celebs_tot$pct, 4), " of net worth)")
all_celebs_tot <- all_celebs_tot[-7,]
all_celebs_tot <- all_celebs_tot[order(all_celebs_tot$total_debt_cum),] 
all_celebs_tot$emitter <- factor(all_celebs_tot$emitter, levels = all_celebs_tot$emitter)

# alright data is ready for plotting 
write_rds(total_carb_majors_jet_ex3, paste0(fig_prepped_dta, "20230629/carbon_debt_majors_hist.rds"))
write_rds(all_celebs_tot, paste0(fig_prepped_dta, "20230629/carbon_debt_celebs_fut.rds"))

# end of script 