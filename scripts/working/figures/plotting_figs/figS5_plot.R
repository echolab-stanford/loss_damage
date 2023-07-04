##############################################################################
# Mustafa Zahid, January 7th, 2023
# This R script plots fig S5 from the paper
#############################################################################
remove(list=ls())
gc()
sf::sf_use_s2(FALSE)
setwd("~/GitHub/loss_damage")

# read in the needed libraries 
source("scripts/working/analysis/0_read_libs.R")

################################################################################
################################################################################
# read data 
total_carb_majors_jet <- readRDS(paste0(fig_prepped_dta, run_date, "/carbon_debt_majors_hist.rds"))
all_celebs_tot <- readRDS(paste0(fig_prepped_dta, run_date, "/carbon_debt_celebs_fut.rds"))


################################################################################
################################################################################
# plot data 

################################################################################ panel a
total_carb_majors_jet <- total_carb_majors_jet[order(total_carb_majors_jet$total_debt_cum_2020),] 
total_carb_majors_jet$emitter <- factor(total_carb_majors_jet$emitter, levels = total_carb_majors_jet$emitter)


#industry level
figS5a <- ggplot(total_carb_majors_jet) +
  #geom_col(aes(total_debt_cum, emitter), fill = "#365191", width = 0.6) +
  geom_col(aes(total_debt_cum_2020, emitter), fill = "#365191", width = 0.6) 

figS5a <- figS5a + 
  scale_x_continuous(
    limits = c(0, 0.25),
    breaks = seq(0, 0.3, by = 0.05), 
    expand = c(0,0.0005), # The horizontal axis does not extend to either side
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


figS5a <- figS5a + 
  geom_shadowtext(
    data = subset(total_carb_majors_jet, total_debt_cum_2020 < 0.10),
    aes(total_debt_cum_2020, y = emitter, label = emitter),
    hjust = 0,
    nudge_x = 0.002,
    colour = "#365191",
    bg.colour = "white",
    bg.r = 0.2,
    size = 6
  ) + 
  geom_text(
    data = subset(total_carb_majors_jet,  total_debt_cum_2020 > 0.10),
    aes(0, y = emitter, label = emitter),
    hjust = 0.0005,
    nudge_x = 0.002,
    colour = "white",
    size = 6
  )


figS5a <- figS5a +
  labs(
    title = "",
    subtitle = "a) Accumulated damages by 2020 of emissions of carbon majors 1988-2015 (Scope 1 and 3, $T)",
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

################################################################################ panel b

figS5b <- ggplot(all_celebs_tot) +
  geom_col(aes(total_debt_cum, emitter), fill = "#365191", width = 0.6) 

figS5b <- figS5b + 
  scale_x_continuous(
    limits = c(0, 500),
    breaks = seq(0, 500, by = 50), 
    expand = c(0,0.05), # The horizontal axis does not extend to either side
    position = "top",  # Labels are located on the top,
    labels = scales::dollar_format()
    #unit_format(unit = "T", scale = 1e-12),
    
  )  + scale_y_discrete(expand = expansion(add = c(0, 0.5))) +
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
    data = subset(all_celebs_tot, total_debt_cum < 180),
    aes(total_debt_cum, y = emitter, label = emitter),
    hjust = -0.03,
    nudge_x = 0.5,
    colour = "#365191",
    bg.colour = "white",
    bg.r = 0.2,
    size = 6
  ) + 
  geom_text(
    data = subset(all_celebs_tot, total_debt_cum >= 180),
    aes(0, y = emitter, label = emitter),
    hjust = -0.03,
    nudge_x = 0.5,
    colour = "white",
    size = 6
  )


figS5b <- figS5b +
  labs(
    title = "",
    subtitle = "b) Future damages from celebrities' private jet emissions in 2022 (thousands of $)"
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


# bring the plots together in one plot 
figS5 <- ggpubr::ggarrange(figS5a, 
                           figS5b, 
                           ncol = 1, 
                           nrow = 2)

# save the figure 
ggsave(paste0(getwd(), "/figures/", run_date, "/figS5a.png"), figS5a, width = 14, height = 8)
ggsave(paste0(getwd(), "/figures/", run_date, "/figS5b.png"), figS5b, width = 14, height = 8)

# end of script 