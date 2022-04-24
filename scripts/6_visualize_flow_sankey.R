##############################################################################
# Mustafa Zahid, April 4th, 2022
# This R script is to visualize the flow of damages owed by each country to 
# to another following a snakey diagram
# Input(s): bilateral damages dataset, year, temperature variable, damages 
# variable, the discount rate.
# Output(s): a sankey diagram
##############################################################################

visualize_bidamages <- function(year, temp_var, dataset, damages_var, disc_rate){
 
  damages <- dataset 
 
  damages$region_ISO3 <- countrycode(damages$ISO3, 
                                     origin = "iso3c",
                                     destination = "region")
  damages$region_iso <- countrycode(damages$iso, 
                                    origin = "iso3c",
                                    destination = "region")
  
  damages <- damages %>% 
    dplyr::mutate(region_iso = case_when(iso == "USA" ~ "1) USA",
                                         iso == "CHN" ~ "2) China",
                                         iso == "RUS" ~ "4) Russia",
                                         iso == "FRA" ~ "3) EU",
                                         iso == "GER" ~ "3) EU",
                                         iso == "GBR" ~ "3) EU",
                                         iso == "UKR" ~ "3) EU",
                                         iso == "AUT" ~ "3) EU",
                                         iso == "MLT" ~ "3) EU",
                                         iso == "POL" ~ "3) EU",
                                         iso == "PRT" ~ "3) EU",
                                         iso == "ROU" ~ "3) EU",
                                         iso == "NLD" ~ "3) EU",
                                         iso == "SWE" ~ "3) EU",
                                         iso == "SVK" ~ "3) EU",
                                         iso == "SVN" ~ "3) EU",
                                         iso == "ESP" ~ "3) EU",
                                         iso == "LUX" ~ "3) EU",
                                         iso == "LVA" ~ "3) EU",
                                         iso == "ITA" ~ "3) EU",
                                         iso == "IRL" ~ "3) EU",
                                         iso == "HUN" ~ "3) EU",
                                         iso == "GRC" ~ "3) EU",
                                         iso == "FIN" ~ "3) EU",
                                         iso == "DNK" ~ "3) EU",
                                         iso == "EST" ~ "3) EU",
                                         iso == "CZE" ~ "3) EU",
                                         iso == "CYP" ~ "3) EU",
                                         iso == "HRV" ~ "3) EU",
                                         iso == "BGR" ~ "3) EU",
                                         iso == "BEL" ~ "3) EU",
                                         iso == "JPN" ~ "6) Japan",
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
                                         region_iso == "Middle East & North Africa" ~ "5) MENA",
                                         #iso == "CAN" ~ "Canada & Mexico",
                                         #iso == "MEX" ~ "Canada & Mexico",
                                         TRUE ~ "7) Others"))
  
  damages <- damages %>% subset(iso == "USA")


  population1 <- wdi_dat %>% dplyr::select(c("year", "iso3c", "SP.POP.TOTL"))
  
  damages <- left_join(damages,
                       population1,
                       by = c("ISO3" = "iso3c",
                              "year" = "year"))
  
  damages <- damages %>% dplyr::mutate(total_damages1 = weighted_damages2 * SP.POP.TOTL)
  
  #damages <- subset(damages, damages <= 0)
  damages$name <- countrycode(damages$ISO3,
                              origin = "iso3c",
                              destination = "country.name") 
  damages <- damages %>% subset(ISO3 != "USA" &
                                ISO3 != "CHN" &
                                ISO3 != "RUS" & 
                                ISO3 != "FRA" &
                                ISO3 != "GER" &
                                ISO3 != "GBR" &
                                ISO3 != "UKR" &
                                ISO3 != "AUT" &
                                ISO3 != "MLT" &
                                ISO3 != "POL" &
                                ISO3 != "PRT" &
                                ISO3 != "ROU" &
                                ISO3 != "NLD" &
                                ISO3 != "SWE" &
                                ISO3 != "SVK" &
                                ISO3 != "SVN" &
                                ISO3 != "ESP" &
                                ISO3 != "LUX" &
                                ISO3 != "LVA" &
                                ISO3 != "ITA" &
                                ISO3 != "IRL" &
                                ISO3 != "HUN" &
                                ISO3 != "GRC" &
                                ISO3 != "FIN" &
                                ISO3 != "DNK" &
                                ISO3 != "EST" &
                                ISO3 != "CZE" &
                                ISO3 != "CYP" &
                                ISO3 != "HRV" &
                                ISO3 != "BGR" &
                                ISO3 != "BEL" &
                                ISO3 != "JPN" &
                                ISO3 != "DEU" &
                                ISO3 != "NOR" &
                                ISO3 != "KOR")

  damages <- damages %>% 
    dplyr::mutate(name = case_when(name == "India" ~ "01) India",
                                    name == "Brazil" ~ "02) Brazil",
                                    name == "Saudi Arabia" ~ "03) Saudi Arabia",
                                    name == "Indonesia" ~ "04) Indonesia",
                                    name == "Mexico" ~ "05) Mexico",
                                    name == "Nigeria" ~ "06) Nigeria",
                                    name == "Australia" ~ "07) Australia",
                                    name == "United Arab Emirates" ~ "08) UAE",
                                    name == "Egypt" ~ "09) Egypt",
                                    name == "Thailand" ~ "10) Thailand",
                                    name == "Malaysia" ~ "11) Malaysia",
                                    name == "Philippines" ~ "12) Philippines",
                                    name == "Singapore" ~ "13) Singapore",
                                    name == "Pakistan" ~ "14) Pakistan",
                                    name == "Hong Kong SAR China" ~ "15) Hong Kong",
                                    name == "Colombia" ~ "16) Colombia",
                                    name == "Vietnam" ~ "17) Vietnam",
                                    name == "Bangladesh" ~ "18) Bangladesh",
                                    name == "South Africa" ~ "19) South Africa",
                                    name == "Puerto Rico" ~ "20) Puerto Rico",
                                    TRUE ~ "21) Other"))
  
  
  damages <- damages %>% dplyr::group_by(name,
                                         region_iso) %>% 
    dplyr::summarise(total_damages1 = sum(total_damages1, na.rm = T), 
                     .groups = "keep")
  
  sum(damages$total_damages1, na.rm = T)

  damages$total_damages1 <- damages$total_damages1 * (-1) 
  
  damages_sankey <- ggplot(data = damages,
                      aes(axis1 = region_iso, axis2 = name, y = total_damages1)) +
    geom_alluvium(aes(fill = name)) +
    geom_stratum() +
    geom_text(stat = "stratum",
              aes(label = after_stat(stratum))) +
    scale_x_discrete(limits = c("Emitter", "Reciever")) +
    ylab("Total Damages (US$)") +
    ggtitle(paste0("Start Year = ", year)) +
    scale_y_continuous(labels = addUnits,limits = c(0,12000000000000)) +
    scale_fill_manual(values = colorRampPalette(brewer.pal(9, "Set1"))(22), breaks = unique(damages$name)) +
    #ggtitle(paste0("b")) +
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
  
  assign(paste0("damages_sankey_", temp_var, "_", disc_rate, "_", year), damages_sankey) 
}


addUnits <- function(n) {
  labels <- ifelse(n < 1000, n,  # less than thousands
                   ifelse(n < 1e6, paste0(round(n/1e3), 'k'),  # in thousands
                          ifelse(n < 1e9, paste0(round(n/1e6), 'M'),  # in millions
                                 ifelse(n < 1e12, paste0(round(n/1e9), 'B'), # in billions
                                        ifelse(n < 1e15, paste0(round(n/1e12), 'T'), # in trillions
                                               'too big!'
                                        )))))
  return(labels)
}
