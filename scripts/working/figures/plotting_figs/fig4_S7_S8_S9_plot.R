##############################################################################
# Mustafa Zahid, January 7th, 2023
# This R script prepares the data for plotting figure 3a and 3b 
#############################################################################
remove(list=ls())
gc()
sf::sf_use_s2(FALSE)
setwd("~/GitHub/loss_damage")

run_date <- "20230713"
# read in the needed libraries 
source("scripts/working/analysis/0_read_libs.R")

# establish a function
`%not_in%` <- purrr::negate(`%in%`)

run_date <- "20230602"
################################################################################
################################################################################
# read data 
damages_and_benefits_k90 <- readRDS(paste0(fig_prepped_dta,run_date, "/damages_and_benefits_k90.rds"))
damages_and_benefits_k80 <- readRDS(paste0(fig_prepped_dta,run_date, "/damages_and_benefits_k80.rds"))
damages_and_benefits_k90_prod <- readRDS(paste0(fig_prepped_dta, run_date,"/damages_and_benefits_k90_prod.rds"))
damages_and_benefits_k90_consump <- readRDS(paste0(fig_prepped_dta,run_date, "/damages_and_benefits_k90_consump.rds"))

################################################################################
################################################################################
# prepare data before plotting it

################################################################################ Sankey-> k = 1990
# we need to use "owed to" as a category for determining the different colors
owed_to_colors <- subset(damages_and_benefits_k90, x == "owed_to_real")
owed_to_colors <- owed_to_colors %>% dplyr::select(c("flow", "stratum"))
# rename before joining
colnames(owed_to_colors)[2] <- "owed_to_real"
# join data 
damages_and_benefits_k90 <- left_join(damages_and_benefits_k90,
                                              owed_to_colors,
                                              by = c("flow"))
# determine the year of k 
damages_and_benefits_k90$year_k <- 1990

# now this is a manual step. But we need to determine which labels we need to 
# take outside the boxes and send a line to 
label_out_rect_k90 <- c(9:18)
label_out_rect_right_k90 <- c(1:31, 37:46)

# now let us plot 
sankey_k90 <- ggplot(damages_and_benefits_k90,
                     aes(y = transfer, 
                         stratum = stratum, 
                         alluvium = flow, 
                         x=x, 
                         label=stratum)) +
  theme_minimal() + 
  theme(panel.grid.major.y = element_line(color = "gray80",
                                          size = 0.75,
                                          linetype = 2),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none",
        plot.margin = margin(0,1.7,0,1.9, "cm"),
        axis.text.x = element_text(size = 35, face = "plain", #family = "Times", 
                                   color = "black"),
        axis.text.y = element_text(size = 35, face = "plain", #family = "Times",
                                   color = "black"),
        axis.title.y = element_text(size = 39),
        strip.text = element_text(size = 43)
  ) +     geom_stratum() + geom_hline(yintercept = 0)+
  geom_alluvium(aes(fill = owed_to_real)) +
  #geom_text(stat="stratum", aes(label=after_stat(deposit)))
  #geom_flow(aes(fill=stratum)) +      + 
  geom_stratum() + 
  labs(y="Benefits from warming ($USD)                                  Damages from warming ($USD)", x="") +
  scale_x_discrete(limits = c("owing_real", "owed_to_real"),
                   labels=c("owing_real"="Emitter", "owed_to_real"="Recipient")) + 
  geom_text(stat="stratum", aes(label=ifelse(after_stat(deposit)%not_in%label_out_rect_k90 &after_stat(deposit)%in%label_out_rect_right_k90,
                                             as.character(after_stat(stratum)),"")),
            size=9) +  ggrepel::geom_text_repel(
              stat="stratum", 
              aes(label=ifelse(after_stat(deposit)%in%label_out_rect_k90,
                               as.character(after_stat(stratum)),"")),
              nudge_x = -0.5,
              direction="y",
              hjust=-0.06,
              segment.curvature = 1e-20,
              segment.color = "gray80",
              size=9
            ) +
  ggrepel::geom_text_repel(
    stat="stratum", 
    aes(label=ifelse(after_stat(deposit)%not_in%label_out_rect_right_k90,
                     as.character(after_stat(stratum)),"")),
    nudge_x = 0.5,
    direction="y",
    hjust=-0.5,
    segment.curvature = 1e-20,
    segment.color = "gray80",
    size=9
  ) + scale_fill_manual(values = colorRampPalette(brewer.pal(9, "Set1"))(23), 
                        breaks = unique(damages_and_benefits_k90$owed_to_real)) +
  scale_y_continuous(labels = addUnits, limits = range(-8000000000000,12000000000000)) 
################################################################################ Sankey-> k = 1980
# we need to use "owed to" as a category for determining the different colors
owed_to_colors <- subset(damages_and_benefits_k80, x == "owed_to_real")
owed_to_colors <- owed_to_colors %>% dplyr::select(c("flow", "stratum"))
# rename before joining
colnames(owed_to_colors)[2] <- "owed_to_real"
# join data 
damages_and_benefits_k80 <- left_join(damages_and_benefits_k80,
                                      owed_to_colors,
                                      by = c("flow"))
# determine the year of k 
damages_and_benefits_k80$year_k <- 1980

# now this is a manual step. But we need to determine which labels we need to 
# take outside the boxes and send a line to 
label_out_rect_k80 <- c(1:4, 9:18)
label_out_rect_right_k80 <- c(1:24, 28:31, 40:46)

# now let us plot 
sankey_k80 <- ggplot(damages_and_benefits_k80,
                     aes(y = transfer, 
                         stratum = stratum, 
                         alluvium = flow, 
                         x=x, 
                         label=stratum)) +
  theme_minimal() + 
  theme(panel.grid.major.y = element_line(color = "gray80",
                                          size = 0.75,
                                          linetype = 2),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none",
        plot.margin = margin(0,1.7,0,1.9, "cm"),
        axis.text.x = element_text(size = 35, face = "plain", #family = "Times", 
                                   color = "black"),
        axis.text.y = element_text(size = 35, face = "plain", #family = "Times",
                                   color = "black"),
        axis.title.y = element_text(size = 39),
        strip.text = element_text(size = 43)
  ) +     geom_stratum() + geom_hline(yintercept = 0)+
  geom_alluvium(aes(fill = owed_to_real)) +
  #geom_text(stat="stratum", aes(label=after_stat(deposit)))
  #geom_flow(aes(fill=stratum)) +      + 
  geom_stratum() + 
  labs(y="Benefits from warming ($USD)                         Damages from warming ($USD)", x="") +
  scale_x_discrete(limits = c("owing_real", "owed_to_real"),
                   labels=c("owing_real"="Emitter", "owed_to_real"="Recipient")) + 
  geom_text(stat="stratum", 
            aes(label=ifelse(after_stat(deposit)%not_in%label_out_rect_k80 &after_stat(deposit)%in%label_out_rect_right_k80,
                                             as.character(after_stat(stratum)),"")),
            size=9) +  ggrepel::geom_text_repel(
              stat="stratum", 
              aes(label=ifelse(after_stat(deposit)%in%label_out_rect_k80,
                               as.character(after_stat(stratum)),"")),
              nudge_x = -0.5,
              direction="y",
              hjust=-0.06,
              segment.curvature = 1e-20,
              segment.color = "gray80",
              size=9
            ) +
  ggrepel::geom_text_repel(
    stat="stratum", 
    aes(label=ifelse(after_stat(deposit)%not_in%label_out_rect_right_k80,
                     as.character(after_stat(stratum)),"")),
    nudge_x = 0.5,
    direction="y",
    hjust=-0.5,
    segment.curvature = 1e-20,
    segment.color = "gray80",
    size=9
  ) + scale_fill_manual(values = colorRampPalette(brewer.pal(9, "Set1"))(23), 
                        breaks = unique(damages_and_benefits_k80$owed_to_real)) +
  scale_y_continuous(labels = addUnits, limits = range(-15000000000000,20000000000000)) 

################################################################################ Sankey-> k = 1990 - production
# we need to use "owed to" as a category for determining the different colors
owed_to_colors <- subset(damages_and_benefits_k90_prod, x == "owed_to_real")
owed_to_colors <- owed_to_colors %>% dplyr::select(c("flow", "stratum"))
# rename before joining
colnames(owed_to_colors)[2] <- "owed_to_real"
# join data 
damages_and_benefits_k90_prod <- left_join(damages_and_benefits_k90_prod,
                                      owed_to_colors,
                                      by = c("flow"))
# determine the year of k 
damages_and_benefits_k90_prod$year_k <- 1990

# now this is a manual step. But we need to determine which labels we need to 
# take outside the boxes and send a line to 
label_out_rect_k90_prod <- c(1:4, 9:18)
label_out_rect_right_k90_prod <- c(1:24, 28:31, 40:46)

# now let us plot 
sankey_k90_prod <- ggplot(damages_and_benefits_k90_prod,
                     aes(y = transfer, 
                         stratum = stratum, 
                         alluvium = flow, 
                         x=x, 
                         label=stratum)) +
  theme_minimal() + 
  theme(panel.grid.major.y = element_line(color = "gray80",
                                          size = 0.75,
                                          linetype = 2),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none",
        plot.margin = margin(0,1.7,0,1.9, "cm"),
        axis.text.x = element_text(size = 35, face = "plain", #family = "Times", 
                                   color = "black"),
        axis.text.y = element_text(size = 35, face = "plain", #family = "Times",
                                   color = "black"),
        axis.title.y = element_text(size = 39),
        strip.text = element_text(size = 43)
  ) +     geom_stratum() + geom_hline(yintercept = 0)+
  geom_alluvium(aes(fill = owed_to_real)) +
  #geom_text(stat="stratum", aes(label=after_stat(deposit)))
  #geom_flow(aes(fill=stratum)) +      + 
  geom_stratum() + 
  labs(y="Benefits from warming ($USD)                       Damages from warming ($USD)", x="") +
  scale_x_discrete(limits = c("owing_real", "owed_to_real"),
                   labels=c("owing_real"="Emitter", "owed_to_real"="Recipient")) + 
  geom_text(stat="stratum", aes(label=ifelse(after_stat(deposit)%not_in%label_out_rect_k90 &after_stat(deposit)%in%label_out_rect_right_k90,
                                             as.character(after_stat(stratum)),"")),
            size=9) +  ggrepel::geom_text_repel(
              stat="stratum", 
              aes(label=ifelse(after_stat(deposit)%in%label_out_rect_k90,
                               as.character(after_stat(stratum)),"")),
              nudge_x = -0.5,
              direction="y",
              hjust=-0.06,
              segment.curvature = 1e-20,
              segment.color = "gray80",
              size=9
            ) +
  ggrepel::geom_text_repel(
    stat="stratum", 
    aes(label=ifelse(after_stat(deposit)%not_in%label_out_rect_right_k90,
                     as.character(after_stat(stratum)),"")),
    nudge_x = 0.5,
    direction="y",
    hjust=-0.5,
    segment.curvature = 1e-20,
    segment.color = "gray80",
    size=9
  ) + scale_fill_manual(values = colorRampPalette(brewer.pal(9, "Set1"))(23), 
                        breaks = unique(damages_and_benefits_k90_prod$owed_to_real)) +
  scale_y_continuous(labels = addUnits, limits = range(-6000000000000,10000000000000)) 

################################################################################ Sankey-> k = 1990 - consumption 
# we need to use "owed to" as a category for determining the different colors
owed_to_colors <- subset(damages_and_benefits_k90_consump, x == "owed_to_real")
owed_to_colors <- owed_to_colors %>% dplyr::select(c("flow", "stratum"))
# rename before joining
colnames(owed_to_colors)[2] <- "owed_to_real"
# join data 
damages_and_benefits_k90_consump <- left_join(damages_and_benefits_k90_consump,
                                           owed_to_colors,
                                           by = c("flow"))
# determine the year of k 
damages_and_benefits_k90_consump$year_k <- 1990

# now this is a manual step. But we need to determine which labels we need to 
# take outside the boxes and send a line to 
label_out_rect_k90_prod <- c(1:4, 9:18)
label_out_rect_right_k90_prod <- c(1:24, 28:31, 40:46)

# now let us plot 
sankey_k90_consump <- ggplot(damages_and_benefits_k90_consump,
                          aes(y = transfer, 
                              stratum = stratum, 
                              alluvium = flow, 
                              x=x, 
                              label=stratum)) +
  theme_minimal() + 
  theme(panel.grid.major.y = element_line(color = "gray80",
                                          size = 0.75,
                                          linetype = 2),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none",
        plot.margin = margin(0,1.7,0,1.9, "cm"),
        axis.text.x = element_text(size = 35, face = "plain", #family = "Times", 
                                   color = "black"),
        axis.text.y = element_text(size = 35, face = "plain", #family = "Times",
                                   color = "black"),
        axis.title.y = element_text(size = 39),
        strip.text = element_text(size = 43)
  ) +     geom_stratum() + geom_hline(yintercept = 0)+
  geom_alluvium(aes(fill = owed_to_real)) +
  #geom_text(stat="stratum", aes(label=after_stat(deposit)))
  #geom_flow(aes(fill=stratum)) +      + 
  geom_stratum() + 
  labs(y="Benefits from warming ($USD)                       Damages from warming ($USD)", x="") +
  scale_x_discrete(limits = c("owing_real", "owed_to_real"),
                   labels=c("owing_real"="Emitter", "owed_to_real"="Recipient")) + 
  geom_text(stat="stratum", aes(label=ifelse(after_stat(deposit)%not_in%label_out_rect_k90 &after_stat(deposit)%in%label_out_rect_right_k90,
                                             as.character(after_stat(stratum)),"")),
            size=9) +  ggrepel::geom_text_repel(
              stat="stratum", 
              aes(label=ifelse(after_stat(deposit)%in%label_out_rect_k90,
                               as.character(after_stat(stratum)),"")),
              nudge_x = -0.5,
              direction="y",
              hjust=-0.06,
              segment.curvature = 1e-20,
              segment.color = "gray80",
              size=9
            ) +
  ggrepel::geom_text_repel(
    stat="stratum", 
    aes(label=ifelse(after_stat(deposit)%not_in%label_out_rect_right_k90,
                     as.character(after_stat(stratum)),"")),
    nudge_x = 0.5,
    direction="y",
    hjust=-0.5,
    segment.curvature = 1e-20,
    segment.color = "gray80",
    size=9
  ) + scale_fill_manual(values = colorRampPalette(brewer.pal(9, "Set1"))(23), 
                        breaks = unique(damages_and_benefits_k90_consump$owed_to_real)) +
  scale_y_continuous(labels = addUnits, limits = range(-6000000000000,10000000000000)) 



################################################################################
################################################################################
# write out the figure 

save_plot(paste0('figures/',run_date, '/fig4.pdf'),plot=sankey_k90, 
          base_width=26.5, base_height=28, scale=1.1)
save_plot(paste0('figures/', run_date, '/figED10.pdf'),plot=sankey_k80, 
          base_width=26.5, base_height=28, scale=1.1)
save_plot(paste0('figures/', run_date, '/figED12.pdf'),plot=sankey_k90_prod, 
          base_width=26.5, base_height=28, scale=1.1)
save_plot(paste0('figures/', run_date, '/figED11.pdf'),plot=sankey_k90_consump, 
          base_width=26.5, base_height=28, scale=1.1)

# end of script
