##############################################################################
# Mustafa Zahid, January 7th, 2023
# This R script plots fig S5 from the paper
#############################################################################
remove(list=ls())
gc()
sf::sf_use_s2(FALSE)
setwd("~/GitHub/loss_damage")

run_date <- "20230821"

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

ind_beh_emms 
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

ind_beh_emms$total_debt_cum_2020 <- ind_beh_emms$total_debt_cum_2020 + 1
figS5a <- ggplot(ind_beh_emms) +
  #geom_col(aes(total_debt_cum, emitter), fill = "#365191", width = 0.6) +
  geom_col(aes(total_debt_cum_2021_2100, emitter), fill = "#365191", width = 0.6) +
  geom_col(aes(total_debt_cum_2020, emitter), fill = "#aabae0", width = 0.6) +
  xlim(0,5500) + 
  scale_x_continuous(trans = log_trans(), breaks = c(1, 10, 50, 100, 500, 1000, 5000),
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
  scale_x_continuous(trans = log_trans(), breaks = c(1, 1.5, 2, 5, 10, 15),
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
    data = subset(total_carb_majors_jet, total_debt_cum_2020 < 0.10),
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
    data = subset(total_carb_majors_jet,  total_debt_cum_2020 > 0.10),
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
  scale_x_continuous(trans = log_trans(), breaks = c(0, 0.5, 1, 1.25, 1.5, 2, 2.5, 3),
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
    limits = c(0, 425),
    breaks = seq(0, 425, by = 50), 
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
ggsave(paste0(getwd(), "/figures/", run_date, "/figED8.pdf"), figS5b, width = 16, height = 8)

# end of script 