##############################################################################
# Mustafa Zahid, January 7th, 2023
# This R script plots figURES 3, ED12, ED13
#############################################################################
remove(list=ls())
gc()
sf::sf_use_s2(FALSE)
setwd("~/GitHub/loss_damage")

run_date <- "loss_damage_r1_replication_v2"

# read in the needed libraries 
source("scripts/working/analysis/0_read_libs.R")

################################################################################
################################################################################
# read data 
total_carb_majors_jet <- readRDS(paste0(fig_prepped_dta, run_date, "/carbon_debt_majors_hist.rds"))
total_carb_majors_jet_scp1 <- readRDS(paste0(fig_prepped_dta, run_date, "/carbon_debt_majors_hist_scp1.rds"))
all_celebs_tot <- readRDS(paste0(fig_prepped_dta, run_date, "/carbon_debt_celebs_fut.rds"))
all_celebs_tot <- subset(all_celebs_tot, rank <= 15)
total_carb_majors_jet <- subset(total_carb_majors_jet, rank <= 15)

ind_beh_emms <- readRDS(paste0(fig_prepped_dta, run_date, "/carbon_debt_ind_beh.rds"))

# read the spread data 
celebsjet_spread <- readRDS(paste0(fig_prepped_dta, run_date, "/carbon_debt_celebs_spread.rds"))
carb_majors_spread <- readRDS(paste0(fig_prepped_dta, run_date, "/carbon_debt_majors_spread.rds"))
individual_beh_emms_spread <- readRDS(paste0(fig_prepped_dta, run_date, "/carbon_debt_ind_beh_spread.rds"))

################################################################################
################################################################################
# plot data 
################################################################################ panel a
ind_beh_emms <- rbind(ind_beh_emms, 
                      ind_beh_emms[rep(1, 9), ])
ind_beh_emms <- ind_beh_emms[order(-ind_beh_emms$total_debt_cum),] 
ind_beh_emms$rank <- 1:15
ind_beh_emms$behavior <- as.character(ind_beh_emms$behavior)
ind_beh_emms$behavior[as.numeric(ind_beh_emms$rank) > 6] <- paste0("recyclingz9",ind_beh_emms$rank[as.numeric(ind_beh_emms$rank) > 6])
ind_beh_emms <- ind_beh_emms[order(ind_beh_emms$total_debt_cum),] 
ind_beh_emms$emitter <- factor(ind_beh_emms$behavior, levels = ind_beh_emms$behavior)
#ind_beh_emms$total_debt_cum_2021_2100 <- log(ind_beh_emms$total_debt_cum_2021_2100)
#ind_beh_emms$total_debt_cum_2020 <- log(ind_beh_emms$total_debt_cum_2020+1)
#industry level

ind_beh_emms <- ind_beh_emms[-(2:10),]
ind_beh_emms$total_debt_cum_2020 <- ind_beh_emms$total_debt_cum_2020 + 1
figS5a <- ggplot(ind_beh_emms) +
  #geom_col(aes(total_debt_cum, emitter), fill = "#365191", width = 0.6) +
  geom_col(aes(total_debt_cum_2021_2100, emitter), fill = "#365191", width = 0.6) +
  geom_col(aes(total_debt_cum_2020, emitter), fill = "#aabae0", width = 0.6) +
  xlim(0,25000) + 
  scale_x_continuous(trans = log_trans(), breaks = c(1, 10, 50, 150, 500, 5000, 25000),
                     position = "top", labels = scales::dollar_format())
#scale_x_continuous(trans = "log10")
#scale_x_continuous(breaks = log10(c(0, 250, 1000, 3000, 5000),
#                                  labels = c(0, 3.2, 10, 32, 100)))

#scale_x_continuous(trans='log2') + 
#coord_trans(x="log2")+
#scale_x_log10()


figS5a <- figS5a + 
  #scale_x_continuous(
  #limits = c(0, 4500),
  #breaks = seq(0, 4500, by = 500), 
  # expand = c(0,0.0005), # The horizontal axis does not extend to either side
  #position = "top",  # Labels are located on the top,
  
  #labels = scales::dollar_format()
  #unit_format(unit = "T", scale = 1e-12),
  scale_y_discrete(expand = expansion(add = c(0, 0.6))) +
  theme(
    # Set background color to white
    panel.background = element_rect(fill = "white"),
    # Set the color and the width of the grid lines for the horizontal axis
    panel.grid.major.x = element_line(color = "#A8BAC4", size = 0.7),
    # Remove the title for both axes
    axis.ticks.length = unit(0, "mm"),
    axis.title = element_blank(),
    # Only left line of the vertical axis is painted in black
    axis.line.y.left = element_line(color = "black", ),
    # Remove labels from the vertical axis
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 12, face = "bold"),
    plot.margin = margin(t = 1,  # Top margin
                         r = 2,  # Right margin
                         b = 3,  # Bottom margin
                         l = 2,  # Left margin
                         unit = "cm")
    #plot.margin =(margin(t = 2, r = 5, b = 2, l = 5, unit = "pt"))
    # Remove tick marks by setting their length to 
    # But customize labels for the horizontal axis
  )


figS5a <- figS5a + 
  #  geom_shadowtext(
  #    data = subset(ind_beh_emms, total_debt_cum < 500),
  #    aes(total_debt_cum_2020, y = emitter, label = emitter),
  #    hjust = -0.45,
  #    nudge_x = 0.02,
  #    colour = "#365191",
  #    bg.colour = "white",
  #    bg.r = 0.2,
  #    size = 6,
  #    fontface = "bold"
  #  ) + 
geom_text(
  data = subset(ind_beh_emms,  total_debt_cum > 0),
  aes(0, y = emitter, label = emitter),
  hjust = -0.1,
  nudge_x = 0.025,
  colour = "white",
  size = 6,
  fontface = "bold"
)


figS5a <- figS5a +
  labs(
    title = "",
    subtitle = "a) Reduction in damages through 2100 of a decade (2010-2020) of individual behaviors",
    color = "Legend"
  ) + 
  theme(
    plot.title = element_text(
      face = "bold",
      size = 20
    ),
    plot.subtitle = element_text(
      size = 16
    )
  )
figS5a


################################################################################ panel a
total_carb_majors_jet <- total_carb_majors_jet[order(total_carb_majors_jet$total_debt_cum),] 
total_carb_majors_jet$emitter <- factor(total_carb_majors_jet$emitter, levels = total_carb_majors_jet$emitter)
#total_carb_majors_jet$total_debt_cum_2020 <- total_carb_majors_jet$total_debt_cum_2020*1000000000
#total_carb_majors_jet$total_debt_cum <- total_carb_majors_jet$total_debt_cum*1000000000

#industry level

total_carb_majors_jet$total_debt_cum_2020 <- 1+ total_carb_majors_jet$total_debt_cum_2020

figS5c <- ggplot(total_carb_majors_jet) +
  #geom_col(aes(total_debt_cum, emitter), fill = "#365191", width = 0.6) +
  geom_col(aes(total_debt_cum, emitter), fill = "#004a3d", width = 0.6) +
  geom_col(aes(total_debt_cum_2020, emitter), fill = "#00967d", width = 0.6) +
  scale_x_continuous(trans = log_trans(), breaks = c(1,2, 5, 10, 15, 25,40,65),
                     position = "top", labels = scales::dollar_format())
#scale_x_continuous(trans = "log") 



figS5c <- figS5c + 
  #  scale_x_continuous(
  #    limits = c(0, 0.25),
  #    breaks = seq(0, 0.3, by = 0.05), 
  #    expand = c(0,0.0005), # The horizontal axis does not extend to either side
  #    position = "top",  # Labels are located on the top,
  #    labels = scales::dollar_format()
  #    #unit_format(unit = "T", scale = 1e-12),
  #    
  #)  + 
  scale_y_discrete(expand = expansion(add = c(0, 0.6))) +
  theme(
    # Set background color to white
    panel.background = element_rect(fill = "white"),
    # Set the color and the width of the grid lines for the horizontal axis
    panel.grid.major.x = element_line(color = "#A8BAC4", size = 0.7),
    # Remove the title for both axes
    axis.ticks.length = unit(0, "mm"),
    axis.title = element_blank(),
    # Only left line of the vertical axis is painted in black
    axis.line.y.left = element_line(color = "black", ),
    # Remove labels from the vertical axis
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 12, face = "bold"),
    plot.margin = margin(t = 1,  # Top margin
                         r = 2,  # Right margin
                         b = 3,  # Bottom margin
                         l = 2,  # Left margin
                         unit = "cm")
    #plot.margin =(margin(t = 2, r = 5, b = 2, l = 5, unit = "pt"))
    # Remove tick marks by setting their length to 
    # But customize labels for the horizontal axis
  )


figS5c <- figS5c + 
  geom_shadowtext(
    data = subset(total_carb_majors_jet, total_debt_cum_2020 < 1),
    aes(total_debt_cum_2020, y = emitter, label = emitter),
    hjust = 0,
    nudge_x = 0.002,
    colour = "#00967d",
    bg.colour = "white",
    bg.r = 0.2,
    size = 6,
    fontface = "bold"
  ) + 
  geom_text(
    data = subset(total_carb_majors_jet,  total_debt_cum_2020 > 1),
    aes(0, y = emitter, label = emitter),
    hjust = 0.0005,
    nudge_x = 0.002,
    colour = "white",
    size = 6,
    fontface = "bold"
  )


figS5c <- figS5c +
  labs(
    title = "",
    subtitle = "c) Accumulated damages by 2020 of emissions of carbon majors 1988-2015 (Scope 1 and 3, $T)",
    color = "Legend"
  ) + 
  theme(
    plot.title = element_text(
      face = "bold",
      size = 20
    ),
    plot.subtitle = element_text(
      size = 16
    )
  )
figS5c

################################################################################ panel b
total_carb_majors_jet_scp1 <- total_carb_majors_jet_scp1[order(total_carb_majors_jet_scp1$total_debt_cum),] 
total_carb_majors_jet_scp1$emitter <- factor(total_carb_majors_jet_scp1$emitter, levels = total_carb_majors_jet_scp1$emitter)

total_carb_majors_jet_scp1$total_debt_cum_2020 <- total_carb_majors_jet_scp1$total_debt_cum_2020 +1 
total_carb_majors_jet_scp1$total_debt_cum <- total_carb_majors_jet_scp1$total_debt_cum +1 

#industry level
figS5b <- ggplot(total_carb_majors_jet_scp1) +
  #geom_col(aes(total_debt_cum, emitter), fill = "#365191", width = 0.6) +
  geom_col(aes(total_debt_cum, emitter), fill = "#004a3d", width = 0.6) +
  geom_col(aes(total_debt_cum_2020, emitter), fill = "#00967d", width = 0.6) +
  scale_x_continuous(trans = log_trans(), breaks = c(0, 1, 2, 4, 6, 8),
                     position = "top", labels = scales::dollar_format())
#scale_x_continuous(trans = "log") 


figS5b <- figS5b + 
  #  scale_x_continuous(
  #    limits = c(0, 0.03),
  #    breaks = seq(0, 0.03, by = 0.01), 
  #    expand = c(0,0.00005), # The horizontal axis does not extend to either side
  #    position = "top",  # Labels are located on the top,
  #    labels = scales::dollar_format()
  #    #unit_format(unit = "T", scale = 1e-12),
  #    
  #  )  + 
  scale_y_discrete(expand = expansion(add = c(0, 0.6))) +
  theme(
    # Set background color to white
    panel.background = element_rect(fill = "white"),
    # Set the color and the width of the grid lines for the horizontal axis
    panel.grid.major.x = element_line(color = "#A8BAC4", size = 0.7),
    # Remove the title for both axes
    axis.ticks.length = unit(0, "mm"),
    axis.title = element_blank(),
    # Only left line of the vertical axis is painted in black
    axis.line.y.left = element_line(color = "black", ),
    # Remove labels from the vertical axis
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 12, face = "bold"),
    plot.margin = margin(t = 1,  # Top margin
                         r = 2,  # Right margin
                         b = 3,  # Bottom margin
                         l = 2,  # Left margin
                         unit = "cm")
    #plot.margin =(margin(t = 2, r = 5, b = 2, l = 5, unit = "pt"))
    # Remove tick marks by setting their length to 
    # But customize labels for the horizontal axis
  )


figS5b <- figS5b + 
  geom_shadowtext(
    data = subset(total_carb_majors_jet_scp1, total_debt_cum_2020 < 0.01),
    aes(total_debt_cum_2020, y = emitter, label = emitter),
    hjust = 0,
    nudge_x = 0.00012,
    colour = "#365191",
    bg.colour = "white",
    bg.r = 0.2,
    size = 6,
    fontface = "bold"
  ) + 
  geom_text(
    data = subset(total_carb_majors_jet_scp1,  total_debt_cum_2020 > 0.010),
    aes(0, y = emitter, label = emitter),
    hjust = 0.0000005,
    nudge_x = 0.0002,
    colour = "white",
    size = 6,
    fontface = "bold"
  )


figS5b <- figS5b +
  labs(
    title = "",
    subtitle = "a) Accumulated damages by 2020 of emissions of carbon majors 1988-2015 (Scope 1, $T)",
    color = "Legend"
  ) + 
  theme(
    plot.title = element_text(
      face = "bold",
      size = 20
    ),
    plot.subtitle = element_text(
      size = 16
    )
  )
figS5b

################################################################################ panel c
figs5b1 <- ggplot(all_celebs_tot) +
  geom_col(aes(total_debt_cum/1000, emitter), fill = "#2d7a93", width = 0.6)


figs5b1 <- figs5b1 + 
  scale_x_continuous(
    limits = c(0, 2000),
    breaks = seq(0, 1750, by = 250), 
    expand = c(0,0.05), # The horizontal axis does not extend to either side
    position = "top",  # Labels are located on the top,
    labels = scales::dollar_format()
    #unit_format(unit = "T", scale = 1e-12),
    
  )  + scale_y_discrete(expand = expansion(add = c(0, 0.6))) +
  theme(
    # Set background color to white
    panel.background = element_rect(fill = "white"),
    # Set the color and the width of the grid lines for the horizontal axis
    panel.grid.major.x = element_line(color = "#A8BAC4", size = 0.7),
    # Remove the title for both axes
    axis.ticks.length = unit(0, "mm"),
    axis.title = element_blank(),
    # Only left line of the vertical axis is painted in black
    axis.line.y.left = element_line(color = "black", ),
    # Remove labels from the vertical axis
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 12, face = "bold"),
    plot.margin = margin(t = 1,  # Top margin
                         r = 2,  # Right margin
                         b = 3,  # Bottom margin
                         l = 2,  # Left margin
                         unit = "cm")
    #plot.margin =(margin(t = 2, r = 5, b = 2, l = 5, unit = "pt"))
    # Remove tick marks by setting their length to 
    # But customize labels for the horizontal axis
  )


figs5b1 <- figs5b1 + 
  geom_shadowtext(
    data = subset(all_celebs_tot, total_debt_cum < 100),
    aes(total_debt_cum, y = emitter, label = emitter),
    hjust = -0.03,
    nudge_x = 0.05,
    colour = "#2d7a93",
    bg.colour = "white",
    bg.r = 0.2,
    size = 6,
    fontface  = "bold"
  ) + 
  geom_text(
    data = subset(all_celebs_tot, total_debt_cum >= 100),
    aes(0, y = emitter, label = emitter),
    hjust = -0.03,
    nudge_x = 0.05,
    colour = "white",
    size = 6,
    fontface  = "bold"
  )


figs5b1 <- figs5b1 +
  labs(
    title = "",
    subtitle = "b. Present value of future cumulative damages (through 2100) of celebrities private jet emissions in 2022 (thousands of $)"
  ) + 
  theme(
    plot.title = element_text(
      face = "bold",
      size = 20
    ),
    plot.subtitle = element_text(
      size = 16
    )
  )
figs5b1

unique(total_carb_majors_jet$emitter)
################################################################################
################################################################################
# plot data FIGED11
carb_majors_spread$emitter[carb_majors_spread$emitter == "Kuwait Petroleum Corp"] <- "Kuwait Petroleum Corp ($0.05T, %1220)"
carb_majors_spread$emitter[carb_majors_spread$emitter == "Sonatrach SPA"] <- "Sonatrach SPA ($0.04T, %1860)"
carb_majors_spread$emitter[carb_majors_spread$emitter == "Peabody Energy Corp"] <- "Peabody Energy Corp ($0T, %21007)"
carb_majors_spread$emitter[carb_majors_spread$emitter == "Petroleos de Venezuela SA (PDVSA)"] <- "Petroleos de Venezuela SA (PDVSA) ($0.05T, %1952)"
carb_majors_spread$emitter[carb_majors_spread$emitter == "Chevron Corp"] <- "Chevron Corp ($0.16T, %692)"
carb_majors_spread$emitter[carb_majors_spread$emitter == "China National Petroleum Corp (CNPC)"] <- "China National Petroleum Corp (CNPC) ($0.44T, %253)"
carb_majors_spread$emitter[carb_majors_spread$emitter == "BP PLC"] <- "BP PLC ($0.16T, %737)"
carb_majors_spread$emitter[carb_majors_spread$emitter == "Coal India"] <- "Coal India ($0.07T, %1507)"
carb_majors_spread$emitter[carb_majors_spread$emitter == "Petroleos Mexicanos (Pemex)"] <- "Petroleos Mexicanos (Pemex) ($0.07T, %1884)"
carb_majors_spread$emitter[carb_majors_spread$emitter == "ExxonMobil Corp"] <- "ExxonMobil Corp ($0.29T, %561)"
carb_majors_spread$emitter[carb_majors_spread$emitter == "National Iranian Oil Co"] <- "National Iranian Oil Co ($0.08T, %2027)"
carb_majors_spread$emitter[carb_majors_spread$emitter == "Gazprom OAO"] <- "Gazprom OAO ($0.14T, %2166)"
carb_majors_spread$emitter[carb_majors_spread$emitter == "Royal Dutch Shell PLC"] <- "Royal Dutch Shell PLC ($0.26T, %488)"
carb_majors_spread$emitter[carb_majors_spread$emitter == "Abu Dhabi National Oil Co"] <- "Abu Dhabi National Oil Co ($0.1T, %771)"
carb_majors_spread$emitter[carb_majors_spread$emitter == "Saudi Arabian Oil Company (Aramco)"] <- "Saudi Arabian Oil Company (Aramco) (Total 2021 Revenue = $0.36T, % of 2021 Revenue = %861)"

total_carb_majors_jet <- left_join(total_carb_majors_jet, 
                                   carb_majors_spread, 
                                   by = c("emitter"))

total_carb_majors_jet$emitter <- gsub(r"{\s*\([^\)]+\)}","",as.character(total_carb_majors_jet$emitter))
total_carb_majors_jet <- total_carb_majors_jet %>%  
  dplyr::select(c("emitter", 
                  "total_debt_cum_2020",
                  "total_debt_cum_2021_2100", 
                  "debt_05_2020", 
                  "debt_10_2020", 
                  "debt_25_2020", 
                  "debt_75_2020", 
                  "debt_90_2020", 
                  "debt_95_2020",
                  "debt_05_2100", 
                  "debt_10_2100", 
                  "debt_25_2100", 
                  "debt_75_2100", 
                  "debt_90_2100", 
                  "debt_95_2100"))
total_carb_majors_jet$id <- 1:nrow(total_carb_majors_jet)
total_carb_majors_jet <- total_carb_majors_jet[order(-total_carb_majors_jet$id),]
all_celebs_tot$emitter <- gsub(r"{\s*\([^\)]+\)}","",as.character(all_celebs_tot$emitter))
all_celebs_tot <- all_celebs_tot %>%  
  dplyr::select(c("emitter", 
                  "total_debt_cum"))
all_celebs_tot <- all_celebs_tot[order(-all_celebs_tot$total_debt_cum),]

#celebsjet_spread$emitter <- trimws(celebsjet_spread$emitter)
all_celebs_tot <- left_join(all_celebs_tot, 
                            celebsjet_spread, 
                            by = c("emitter"))
ind_beh_emms$behavior <- as.character(ind_beh_emms$behavior)
ind_beh_emms <- left_join(ind_beh_emms, 
                          individual_beh_emms_spread, 
                          by = c("behavior"))
ind_beh_emms$behavior[ind_beh_emms$behavior == "A long-haul flight (8000km)"] <- "Additional long-haul (8000km) flight per year"
ind_beh_emms$behavior[ind_beh_emms$behavior == "Driving 10% more"] <- "Driving 10% more than average American"
ind_beh_emms$behavior[ind_beh_emms$behavior == "A non-vegetarian diet"] <- "Eating and average American diet instead of vegetarian diet"
ind_beh_emms$behavior[ind_beh_emms$behavior == "Installing heat pump"] <- "Using a gas furnace instead of a heat pump"
ind_beh_emms <- ind_beh_emms %>%  
  dplyr::select(c("behavior", 
                  "total_debt_cum_2020",
                  "total_debt_cum_2021_2100", 
                  "debt_05_2020",
                  "debt_10_2020",
                  "debt_25_2020",
                  "debt_75_2020",
                  "debt_90_2020",
                  "debt_95_2020",
                  "debt_05_2100",
                  "debt_10_2100",
                  "debt_25_2100",
                  "debt_75_2100",
                  "debt_90_2100",
                  "debt_95_2100"))

ind_beh_emms <- ind_beh_emms[order(-ind_beh_emms$total_debt_cum_2021_2100),]
ind_beh_emms$total_debt_cum_2020[ind_beh_emms$total_debt_cum_2020 < 1] <- round(ind_beh_emms$total_debt_cum_2020[ind_beh_emms$total_debt_cum_2020 < 1], 1)
ind_beh_emms$total_debt_cum_2020[ind_beh_emms$total_debt_cum_2020 > 1] <- round(ind_beh_emms$total_debt_cum_2020[ind_beh_emms$total_debt_cum_2020 > 1], 0)
ind_beh_emms$total_debt_cum_2021_2100 <- round(ind_beh_emms$total_debt_cum_2021_2100, 0)
ind_beh_emms$total_debt_cum_2020 <- paste0("$", as.character(ind_beh_emms$total_debt_cum_2020), " ($", round(ind_beh_emms$debt_05_2020, 0), ",$" ,round(ind_beh_emms$debt_95_2020, 0), ")")
ind_beh_emms$total_debt_cum_2021_2100 <- paste0("$",ind_beh_emms$total_debt_cum_2021_2100, 
                                                " ($",round(ind_beh_emms$debt_05_2100, 0), ",$",
                                                round(ind_beh_emms$debt_95_2100, 0), ")")
                                                

total_carb_majors_jet$total_debt_cum_2020[total_carb_majors_jet$total_debt_cum_2020 < 1] <- round(total_carb_majors_jet$total_debt_cum_2020[total_carb_majors_jet$total_debt_cum_2020 < 1], 2)
total_carb_majors_jet$total_debt_cum_2020[total_carb_majors_jet$total_debt_cum_2020 > 1] <- round(total_carb_majors_jet$total_debt_cum_2020[total_carb_majors_jet$total_debt_cum_2020 > 1], 2)
total_carb_majors_jet$total_debt_cum_2021_2100 <- round(total_carb_majors_jet$total_debt_cum_2021_2100, 2)
total_carb_majors_jet$total_debt_cum_2020 <- paste0("$", as.character(total_carb_majors_jet$total_debt_cum_2020), "T", " ($", round(total_carb_majors_jet$debt_05_2020/1000000000000, 2), "T,$", round(total_carb_majors_jet$debt_95_2020/1000000000000, 2), "T)")
total_carb_majors_jet$total_debt_cum_2021_2100 <- paste0("$", as.character(total_carb_majors_jet$total_debt_cum_2021_2100), "T", " ($", round(total_carb_majors_jet$debt_05_2100/1000000000000, 2), "T,$", round(total_carb_majors_jet$debt_95_2100/1000000000000, 2), "T)")
all_celebs_tot$total_debt_cum <- all_celebs_tot$total_debt_cum/1000
all_celebs_tot$total_debt_cum <- round(all_celebs_tot$total_debt_cum, 0)
all_celebs_tot$total_debt_cum <- paste0("$", as.character(all_celebs_tot$total_debt_cum), "K ($", round(all_celebs_tot$debt_05/1000, 0), "K,$", round(all_celebs_tot$debt_95/1000,0), "K)")
ind_beh_emms <- ind_beh_emms[1:6,]


ind_beh_emms <- ind_beh_emms %>% 
  dplyr::select(c("behavior", 
                  "total_debt_cum_2020", 
                  "total_debt_cum_2021_2100"))
total_carb_majors_jet <- total_carb_majors_jet %>% 
  dplyr::select(c("emitter", 
                  "total_debt_cum_2020", 
                  "total_debt_cum_2021_2100"))
all_celebs_tot <- all_celebs_tot %>% 
  dplyr::select(c("emitter", 
                  "total_debt_cum"))

# plot data 
################################################################################ Figure 3a
ind_beh <- ind_beh_emms %>%
  tibble%>%
  #group_by(emitter) %>% 
  gt(rowname_col = "behavior") %>% 
  #dplyr::mutate(total_damages_2020_dr2 = paste0("$", total_damages_2020_dr2)) %>% 
  tab_spanner(label = "a Cumulative damages (through 2100) of a decade (2010-2020) of individual behaviors",
              columns = vars(total_debt_cum_2020,
                             total_debt_cum_2021_2100)) %>% 
  cols_label(total_debt_cum_2020 = "Damages through 2020",
             total_debt_cum_2021_2100 = "Damages 2021-2100") %>% 
  cols_align(align = "center") %>%
  gt_theme_538(table.width = px(650)) %>% 
  gtsave(paste0(getwd(), "/figures/", run_date, "/figED11_a_w_spread.pdf"))


carb_majors <-  total_carb_majors_jet %>%
  tibble%>%
  #group_by(emitter) %>% 
  gt(rowname_col = "emitter") %>% 
  #dplyr::mutate(total_damages_2020_dr2 = paste0("$", total_damages_2020_dr2)) %>% 
  tab_spanner(label = "c Accumulated damages by 2020 of emissions of carbon majors 1988-2015 (Scope 1 and 3, $T)",
              columns = vars(total_debt_cum_2020,
                             total_debt_cum_2021_2100)) %>% 
  cols_label(total_debt_cum_2020 = "Damages through 2020",
             total_debt_cum_2021_2100 = "Damages 2021-2100") %>% 
  cols_align(align = "center") %>% 
  gt_theme_538(table.width = px(650)) %>% 
  gtsave(paste0(getwd(), "/figures/", run_date, "/figED11_c_W_spread.pdf"))

all_celebs_tot <- all_celebs_tot[1:14,]
celeb_jets <- all_celebs_tot %>%
  tibble%>%
  #group_by(emitter) %>% 
  gt(rowname_col = "emitter") %>% 
  #dplyr::mutate(total_damages_2020_dr2 = paste0("$", total_damages_2020_dr2)) %>% 
  tab_spanner(label = "b Present value of future cumulative damages (through 2100) of celebrities private jet emissions in 2022 (thousands of $)",
              columns = vars(total_debt_cum)) %>% 
  cols_label(total_debt_cum = "Damages through 2100") %>% 
  cols_align(align = "center") %>% 
  gt_theme_538(table.width = px(650)) %>% 
  gtsave(paste0(getwd(), "/figures/", run_date, "/figED11_b_w_spread.pdf"))



# bring the plots together in one plot 
#figS5 <- ggpubr::ggarrange(figS5a, 
#                           figS5b,
#                          figS5c,
#                          ncol = 1, 
#                          nrow = 3)

# save the figure 
ggsave(paste0(getwd(), "/figures/", run_date, "/fig3a_new.pdf"), figS5a, width = 16, height = 8)
ggsave(paste0(getwd(), "/figures/", run_date, "/fig3b_new.pdf"), figs5b1, width = 16, height = 8)
ggsave(paste0(getwd(), "/figures/", run_date, "/fig3c_new.pdf"), figS5c, width = 16, height = 8)
ggsave(paste0(getwd(), "/figures/", run_date, "/figED13.pdf"), figS5b, width = 16, height = 8)

# end of script 