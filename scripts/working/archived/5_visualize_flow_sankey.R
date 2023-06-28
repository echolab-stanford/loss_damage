##############################################################################
# Mustafa Zahid, April 4th, 2022
# This R script is to visualize the flow of damages owed by each country to 
# to another following a snakey diagram
# NOTE : This script might require manual modification of countries' ranks, 
# which dictate thier position in the sankey. The out out of this script will 
# need further modifications through illustrator.
# Input(s): bilateral damages dataset, year, temperature variable, damages 
# variable, the discount rate.
# Output(s): a sankey diagram
##############################################################################
year <- 1980
dataset <- total_damages_k80 

total_damages_k80 <- readRDS("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/total_bidamages_k80.rds")

head(total_damages_k80)

colnames(total_damages_k80)[1] <- "reciever"

head(total_damages_k80)



dataset <- total_damages_k80

visualize_bidamages <- function(year, dataset){
  
  dataset <- subset(dataset, year <= 2020)
  sum_all_emmiter <- dataset %>% dplyr::group_by(emitter) %>% 
    dplyr::summarise(sum2 = sum(weighted_damages2 ,na.rm = T),
                     .groups = "keep")
  
  
  sum_all_harmed <- dataset %>% dplyr::group_by(reciever) %>% 
    dplyr::summarise(sum2 = sum(weighted_damages2 ,na.rm = T),
                     .groups = "keep")
  #aggregate total damages by emitter-harmed pair 
  # ISO3 here refers to the harmed 
  sum_all <- dataset %>% dplyr::group_by(emitter, reciever) %>% 
    dplyr::summarise(sum2 = sum(weighted_damages2 ,na.rm = T),
                     .groups = "keep")
  
  damages <- sum_all 
  
  # now generate region codes for each of the countries for both emitter and 
  # reciever
  damages$region_reciever <- countrycode(damages$reciever, 
                                     origin = "iso3c",
                                     destination = "region")
  damages$region_emitter <- countrycode(damages$emitter, 
                                    origin = "iso3c",
                                    destination = "region")
  
  # now rereciever_cntry by adding ranks so that they can be ordered by order of total 
  # damages
  damages <- damages %>% 
    dplyr::mutate(region_emitter = case_when(emitter == "USA" ~ "01) USA",
                                         emitter == "CHN" ~ "03) China",
                                         emitter == "RUS" ~ "04) Russia",
                                         emitter == "FRA" ~ "02) EU",
                                         emitter == "GER" ~ "02) EU",
                                         emitter == "GBR" ~ "02) EU",
                                         emitter == "UKR" ~ "02) EU",
                                         emitter == "AUT" ~ "02) EU",
                                         emitter == "MLT" ~ "02) EU",
                                         emitter == "POL" ~ "02) EU",
                                         emitter == "PRT" ~ "02) EU",
                                         emitter == "ROU" ~ "02) EU",
                                         emitter == "NLD" ~ "02) EU",
                                         emitter == "SWE" ~ "02) EU",
                                         emitter == "SVK" ~ "02) EU",
                                         emitter == "SVN" ~ "02) EU",
                                         emitter == "ESP" ~ "02) EU",
                                         emitter == "LUX" ~ "02) EU",
                                         emitter == "LVA" ~ "02) EU",
                                         emitter == "ITA" ~ "02) EU",
                                         emitter == "IRL" ~ "02) EU",
                                         emitter == "HUN" ~ "02) EU",
                                         emitter == "GRC" ~ "02) EU",
                                         emitter == "FIN" ~ "02) EU",
                                         emitter == "DNK" ~ "02) EU",
                                         emitter == "EST" ~ "02) EU",
                                         emitter == "CZE" ~ "02) EU",
                                         emitter == "CYP" ~ "02) EU",
                                         emitter == "HRV" ~ "02) EU",
                                         emitter == "BGR" ~ "02) EU",
                                         emitter == "BEL" ~ "02) EU",
                                         emitter == "JPN" ~ "05) Japan",
                                         emitter == "IND" ~ "06) India",
                                         emitter == "CAN" ~ "07) Canada",
                                         emitter == "KOR" ~ "11) South Korea",
                                         emitter == "MEX" ~ "08) Mexico",
                                         emitter == "ZAF" ~ '09) South Africa',
                                         emitter == "AUS" ~ "10) Australia",
                                        # emitter == "IRN" ~ "11) Iran",
                                         emitter == "SAU" ~ "12) Saudi Arabia",
                                         #iso == "IND" ~ "India",
                                         #iso == "BRA" ~ "South America",
                                         #iso == "PER" ~ "South America",
                                         #iso == "ARG" ~ "South America",
                                         #iso == "CHL" ~ "South America",
                                         #iso == "VEN" ~ "South America",
                                         #iso == "COL" ~ "South America",
                                         #iso == "ECU" ~ "South America",
                                         #iso == "BOL" ~ "South America",
                                         #iso == "URU" ~ "South America",
                                         #iso == "PAR" ~ "South America",
                                         #iso == "AUS" ~ "Australia & NZ",
                                         #iso == "NZA" ~ "Australia & NZ",
                                        # region_iso == "Middle East & North Africa" ~ "05) MENA",
                                         #iso == "CAN" ~ "Canada & Mexico",
                                         #iso == "MEX" ~ "Canada & Mexico",
                                         TRUE ~ "13) Others"))
  
  #damages <- damages %>% subset(iso == "USA")
  
  
  #population1 <- wdi_dat %>% dplyr::select(c("year", "iso3c", "SP.POP.TOTL"))
  
  #damages <- left_join(damages,
  #                    pop,
  #                   by = c("ISO3" = "iso3c",
  #                         "year" = "year"))
  
  #  damages <- damages %>% dplyr::mutate(total_damages1 = weighted_damages2 * SP.POP.TOTL)
  
  #damages <- subset(damages, damages <= 0)
  
  # generate country reciever_cntrys to go in the diagram
  damages$reciever_cntry <- countrycode(damages$reciever,
                              origin = "iso3c",
                              destination = "country.name") 
  
  # now rereciever_cntry by adding the ranks to the country reciever_cntry so that they can be ordered
  # in the diagram 
  damages <- damages %>% 
    dplyr::mutate(reciever_cntry = case_when(reciever_cntry == "India" ~ "001) India",
                                   reciever_cntry == "Brazil" ~ "00003) Brazil",
                                   reciever == "CHN" ~ "002) China",
                                   reciever_cntry == "Saudi Arabia" ~ "04) Saudi Arabia",
                                   reciever_cntry == "Indonesia" ~ "05) Indonesia",
                                   reciever_cntry == "Mexico" ~ "06) Mexico",
                                   reciever == "USA" ~ "08) USA",
                                   reciever_cntry == "Nigeria" ~ "07) Nigeria",
                                   reciever_cntry == "Australia" ~ "11) Australia",
                                   reciever_cntry == "United Arab Emirates" ~ "09) UAE",
                                   reciever_cntry == "Egypt" ~ "14) Egypt",
                                   reciever_cntry == "Thailand" ~ "11) Thailand",
                                   reciever_cntry == "Malaysia" ~ "13) Malaysia",
                                   reciever_cntry == "Philippines" ~ "15) Philippines",
                                   reciever_cntry == "Argentina" ~ "16) Argentina",
                                   
                                  # reciever_cntry == "Singapore" ~ "13) Singapore",
                                   reciever_cntry == "Pakistan" ~ "12) Pakistan",
                                  # reciever_cntry == "Hong Kong SAR China" ~ "15) Hong Kong",
                                   reciever_cntry == "Colombia" ~ "17) Colombia",
                                   #reciever_cntry == "Vietnam" ~ "17) Vietnam",
                                  # reciever_cntry == "Sri Lanka" ~ "18) Sri Lanka",
                                  # reciever_cntry == "Bangladesh" ~ "17) Bangladesh",
                                  # reciever_cntry == "Sudan" ~ "16) Sudan",
                                   reciever_cntry == "South Africa" ~ "18) South Africa",
                                   reciever == "ESP" ~ "19) Spain",
                                   reciever == "SDN" ~ "20) Sudan",
                                   #reciever_cntry == "Puerto Rico" ~ "20) Puerto Rico",
                                   TRUE ~ "21) Other"))
  
  # now aggregate by the created identifiers
  # here "reciever_cntry" is the harmed country and "region_iso" is the emitter
  damages <- damages %>% dplyr::group_by(reciever_cntry,
                                         emitter) %>% 
    dplyr::summarise(total_damages1 = sum(sum2, na.rm = T), 
                     .groups = "keep")
  
  # look at sum
  sum(damages$total_damages1, na.rm = T)
  
  # multiply sum by (-1) to generate total damage in positive sign
  damages$total_damages1 <- damages$total_damages1 * (-1) 
  
  # finally plot the sankey
  damages_sankey_k80 <- ggplot(data = damages,
                           aes(axis1 = emitter, 
                               axis2 = reciever_cntry, 
                               y = total_damages1)) +
    geom_alluvium(aes(fill = reciever_cntry)) +
    geom_stratum() +

    geom_text(stat = "stratum",
              aes(label = after_stat(stratum))) +
    theme_minimal() +
    
    ggrepel::geom_text_repel(
      stat="stratum", 
      aes(label = after_stat(stratum)),
      nudge_x = 1.5,
      direction="y",
      hjust=0.5,
      segment.curvature = 1e-20,
      segment.color = "gray80",
      size=3.5
    ) +
    scale_x_discrete(limits = c("Emitter", "Reciever")) +
    ylab("Total Damages (US$)") +
    ggtitle(paste0("Start Year = ", year)) +
    scale_y_continuous(labels = addUnits ,limits = c(sum(damages$total_damages1),max(damages$total_damages1))) +
    scale_fill_manual(values = colorRampPalette(brewer.pal(9, "Set1"))(22), 
                      breaks = unique(damages$reciever_cntry)) +
    ggtitle(paste0(title = "Total Damages Owed by Top Emitters to Top Harmed",
                   subtitle = paste0("Damages Starting ", year, " Discounted at 2%"))) +
    theme_minimal() +
    theme(plot.title = element_text(size = 40, face = "bold"), #family = "Times"),
          axis.title.y = element_text(size = 35, face = "bold"), #family = "Times"),
          legend.text = element_text(size = 25, face = "plain"), #family = "Times"),
          legend.title = element_text(size = 10, face = "plain"),#family = "Times"),
          legend.position = "none",
          axis.text.x = element_text(size = 35, face = "plain", #family = "Times", 
                                     color = "black"),
          axis.text.y = element_text(size = 35, face = "plain", #family = "Times",
                                     color = "black")) 
return(damages_sankey)
}


# create a function to add a unit letter next to the total amount displayed 
addUnits <- function(n) {
  labels <- ifelse(n < -1e12, paste0(round(n/-1e12), 'T'),
                   ifelse(n < 1000, n,  # less than thousands
                      ifelse(n < 1e6, paste0(round(n/1e3), 'k'),  # in thousands
                          ifelse(n < 1e9, paste0(round(n/1e6), 'M'),  # in millions
                                 ifelse(n < 1e12, paste0(round(n/1e9), 'B'), # in billions
                                        ifelse(n < 1e15, paste0(round(n/1e12), 'T'),# in trillions
                                               'too big!'
                                        ))))))
  return(labels)
}

# end of script



StatStratum$compute_panel()


ex <- ggalluvial::to_alluvia_form(damages)



damages


example_alluvial <- ggalluvial::to_alluvia_form(damages)

rlang::last_error()
rlang::last_trace()



damages$group_strata <- paste0(damages$reciever_cntry, "-", damages$region_emitter)
damages$x <- 1
damages_lodes <- is_lodes_form(damages, key = x,
                               value = total_damages1, id = group_strata) # TRUE


damages_lodes <- ggalluvial::to_lodes_form(damages)


ggplot(damages_lodes, 
       aes(y = freq, x = x, alluvium = alluvium, stratum = stratum)) + 
  geom_alluvium(aes(fill = group)) + 
  geom_stratum() +
  geom_label(stat = "stratum", aes(label = strata)) +
  scale_fill_viridis()

