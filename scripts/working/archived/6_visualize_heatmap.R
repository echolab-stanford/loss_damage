##############################################################################
# Mustafa Zahid, April 4th, 2022
# This R script is to visualize the bilateral transfers of damages in a heatmap 
# format. NOTE: this script requires some manual alteration to dictate the 
# breaks for the color gradient of teh heatmap. This script is set for 2% discount,
# but can be adjusted
# Input(s): bilateral damages dataset, 
# Output(s): a sankey diagram
##############################################################################

visualize_heatmaps <- function(dataset, year_k){
  sum_all <- dataset %>% dplyr::group_by(emitter, ISO3) %>% 
    dplyr::summarise(sum2 = sum(weighted_damages2, na.rm = T),
                     .groups = "keep")
  
  
  #bi_damages <- damages_across_all_cru_k_1990 %>% dplyr::group_by(iso, ISO3) %>% 
   # subset(damages <0) %>% 
    #dplyr::summarise(weighted_damages = sum(weighted_damages2, na.rm = T),
     #                .groups = "keep")
  
  
  #bi_damages <- left_join(bi_damages,
   #                       pop,
    #                      by = c("ISO3" = "iso3c"))
  
  #bi_damages$toal_damages <- bi_damages$weighted_damages * bi_damages$SP.POP.TOTL
  
  
  
  sum_all$region_ISO3 <- countrycode(sum_all$ISO3, 
                                        origin = "iso3c",
                                        destination = "region")
  sum_all$region_iso <- countrycode(sum_all$emitter, 
                                       origin = "iso3c",
                                       destination = "region")
  
  sum_all <- sum_all %>% 
    dplyr::mutate(region_iso = case_when(emitter == "USA" ~ "1) USA",
                                         emitter == "CHN" ~ "3) China",
                                         emitter == "RUS" ~ "4) Russia",
                                         emitter == "FRA" ~ "2) EU",
                                         emitter == "DEU" ~ "2) EU",
                                         emitter == "GBR" ~ "2) EU",
                                         emitter == "UKR" ~ "2) EU",
                                         emitter == "AUT" ~ "2) EU",
                                         emitter == "MLT" ~ "2) EU",
                                         emitter == "POL" ~ "2) EU",
                                         emitter == "PRT" ~ "2) EU",
                                         emitter == "ROU" ~ "2) EU",
                                         emitter == "NLD" ~ "2) EU",
                                         emitter == "SWE" ~ "2) EU",
                                         emitter == "SVK" ~ "2) EU",
                                         emitter == "SVN" ~ "2) EU",
                                         emitter == "ESP" ~ "2) EU",
                                         emitter == "LUX" ~ "2) EU",
                                         emitter == "LVA" ~ "2) EU",
                                         emitter == "ITA" ~ "2) EU",
                                         emitter == "IRL" ~ "2) EU",
                                         emitter == "HUN" ~ "2) EU",
                                         emitter == "GRC" ~ "2) EU",
                                         emitter == "FIN" ~ "2) EU",
                                         emitter == "DNK" ~ "2) EU",
                                         emitter == "EST" ~ "2) EU",
                                         emitter == "CZE" ~ "2) EU",
                                         emitter == "CYP" ~ "2) EU",
                                         emitter == "HRV" ~ "2) EU",
                                         emitter == "BGR" ~ "2) EU",
                                         emitter == "BEL" ~ "2) EU",
                                         emitter == "JPN" ~ "5) Japan",
                                         #iso == "IND" ~ dia",
                                         #iso == "BRA" ~ uth America",
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
                                         region_iso == "Middle East & North Africa" ~ "6) MENA",
                                         #iso == "CAN" ~ "Canada & Mexico",
                                         #iso == "MEX" ~ "Canada & Mexico",
                                         TRUE ~ "7) Others"))
  
  
  #damages <- subset(damages, damages <= 0)
  sum_all$name <- countrycode(sum_all$ISO3,
                                 origin = "iso3c",
                                 destination = "country.name") 
  sum_all <- sum_all %>% subset(ISO3 != "USA" &
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
  
  sum_all <- sum_all %>% 
    dplyr::mutate(name = case_when(name == "India" ~ "02) India",
                                   name == "Brazil" ~ "01) Brazil",
                                   name == "Saudi Arabia" ~ "04) Saudi Arabia",
                                   name == "Indonesia" ~ "03) Indonesia",
                                   name == "Mexico" ~ "06) Mexico",
                                   name == "Nigeria" ~ "07) Nigeria",
                                   name == "Australia" ~ "05) Australia",
                                   name == "United Arab Emirates" ~ "08) UAE",
                                   name == "Egypt" ~ "11) Egypt",
                                   name == "Thailand" ~ "09) Thailand",
                                   name == "Malaysia" ~ "10) Malaysia",
                                   name == "Philippines" ~ "12) Philippines",
                                   #name == "Singapore" ~ "13) Singapore",
                                   name == "Pakistan" ~ "13) Pakistan",
                                   #name == "Hong Kong SAR China" ~ "15) Hong Kong",
                                   name == "Colombia" ~ "14) Colombia",
                                   #name == "Vietnam" ~ "17) Vietnam",
                                   name == "Bangladesh" ~ "16) Bangladesh",
                                   name == "South Africa" ~ "15) South Africa",
                                   #name == "Puerto Rico" ~ "20) Puerto Rico",
                                   TRUE ~ "17) Other"))
  
  
  sum_all1 <- sum_all %>% dplyr::group_by(name,
                                                region_iso) %>% 
    dplyr::summarise(total_damages1 = sum(sum2, na.rm = T), 
                     .groups = "keep")
  
  sum(sum_all1$total_damages1, na.rm = T)
  
  sum_all1$total_damages1 <- sum_all1$total_damages1 * (-1) 
  
  #bi_damages1$faactor_ex <- factor(bi_damages1$region_iso, 
   #                                levels=(bi_damages1$region_iso)[order(bi_damages1$total_damages1)])
  
  emitter_tot <- sum_all1 %>% dplyr::group_by(region_iso) %>% 
    dplyr::summarise(totaldamages = sum(total_damages1))
    
  reciever_tot <- sum_all1 %>% dplyr::group_by(name) %>% 
    dplyr::summarise(totaldamages = sum(total_damages1))
  
  sum_all1$total_damages1 <- sum_all1$total_damages1 * (-1)
  
  # manually adjust breaks to be reflected in the color gradient
  breaks1 <- c(0, 5000000000, 6000000000, 7000000000, 9000000000, 
               10000000000, 15000000000,30000000000, 50000000000, 
               60000000000, 70000000000,100000000000, 120000000000, 
               130000000000, 140000000000 ,150000000000, 160000000000, 
               170000000000,  200000000000,300000000000, 500000000000, 
               600000000000, 800000000000, 1000000000000,1100000000000)
  
  # before plotting create function that would add a unit letter next to the total 
  # in the figure 
  addUnits <- function(n) {
    labels <- ifelse(n < 1000, n,  # less than thousands
                     ifelse(n < 1e6, paste0(round(n/1e3), 'k'),  # in thousands
                            ifelse(n < 1e9, paste0(round(n/1e6), 'M'),  # in millions
                                   ifelse(n < 1e12, paste0(round(n/1e9), 'B'), # in billions
                                          ifelse(n < 1e15, paste0(round(n/1e12, 2), 'T'), # in trillions
                                                 'too big!'
                                          )))))
    return(labels)
  }  
  
  # before constructing the figure, we need to calculate column and row-wide totals 
  # to be displayed in the margins of the heatmap
  h_total <- sum_all1 %>%  dplyr::group_by(name) %>% dplyr::summarise(total_damages1 = sum(total_damages1)) %>% 
    dplyr::mutate(region_iso = "Total")
  v_total <- sum_all1 %>%  dplyr::group_by(region_iso) %>% dplyr::summarise(total_damages1 = sum(total_damages1)) %>% 
    dplyr::mutate(name = "Total")
  
  # now finally plot the heatmap  
  heatmap <- ggplot(sum_all1, aes(name, region_iso, fill= total_damages1)) + 
    geom_tile() + scale_y_discrete(limits = rev) + 
    scale_fill_steps(low = "#f0cee1",
                     high = "#0b2a61",
                     trans = "log",
                     breaks = breaks1,
                     labels = addUnits, 
                     name = "Total Amount ($)") +
    scale_x_discrete(position = "top") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 20, hjust=0),
          axis.text.y = element_text(angle = 45, vjust = 1, hjust=1),
          axis.text = element_text(size = 14),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          title = element_text(size = 18, face = "bold"),
          legend.text = element_text(size = 12),
          legend.key.size = unit(1.5, 'cm')) + 
    ylab("Emitter (Owed From)") +
    xlab("Harmed (Owed To)") +
    ggtitle(label = "Amount Owed due to Damages from Emissions-induced Warming",
            subtitle = paste0("Start year = ", year_k, " & Discount Rate = 2%")) +
    geom_point(data = v_total, 
               aes(color = total_damages1), 
               size = 10, 
               shape = 19) + 
    geom_point(data = h_total, 
               aes(color = total_damages1), 
               size = 10, 
               shape = 19) + 
    scale_color_gradient2(low = "grey", #colors
                          mid = "white",
                          high = "gold",
                          midpoint = 2000000000,
                          label = addUnits,
                          name = "Total Amount ($)") +
    geom_text(data = v_total, size = 3, aes(label = paste0(round((total_damages1/1000000000000), 1), "T"))) +
    geom_text(data = h_total, size = 3, aes(label = paste0(round((total_damages1/1000000000000), 1), "T")))
  
    return(heatmap)
}

# end of script
