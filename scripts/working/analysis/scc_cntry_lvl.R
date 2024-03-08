# read data 
scc_2300_1pct_growth <- readRDS(paste0("~/Downloads/20230821/scc_2300_1pct_growth.rds"))
scc_2300_2pct_growth <- readRDS(paste0("~/Downloads/20230821/scc_2300_2pct_growth.rds"))
scc_2300_clamped_growth <- readRDS(paste0("~/Downloads/20230821/scc_2300_clamped_growth.rds"))
scc_2300_2100_5lag <- readRDS(paste0("~/Downloads/20230821/scc_2300_2100_5lag.rds"))
scc_2100_2100_5lag <- readRDS(paste0("~/Downloads/20230821/scc_2100_2100_5lag.rds"))
scc_2300_nog_post_2100 <- readRDS(paste0("~/Downloads/20230821/scc_2300_nog_post_2100.rds"))
scc_2300_2100_growth <- readRDS(paste0("~/Downloads/20230821/scc_2300_2100_growth.rds"))
scc_2100 <- readRDS(paste0("~/Downloads/20230821/scc_2100.rds"))

scc_2100 <- scc_2100 %>% 
  dplyr::group_by(ISO3) %>% 
  dplyr::summarise(scc_2100_dr2 = sum(weighted_damages2_scld, na.rm = T),
                   scc_2100_drramsey = sum(weighted_damages_ramsey_scld, na.rm = T))

scc_2100 <- subset(scc_2100, scc_2100$scc_2100_dr2 !=0)

scc_2300_nog_post_2100 <- scc_2300_nog_post_2100 %>% 
  dplyr::group_by(ISO3) %>% 
  dplyr::summarise(scc_2300_nog_post_2100_dr2 = sum(weighted_damages2_scld, na.rm = T),
                   scc_2300_nog_post_2100_drramsey = sum(weighted_damages_ramsey_scld, na.rm = T))

scc_2300_nog_post_2100 <- subset(scc_2300_nog_post_2100, scc_2300_nog_post_2100$scc_2300_nog_post_2100_dr2 !=0)

scc_2300_2100_growth <- scc_2300_2100_growth %>% 
  dplyr::group_by(ISO3) %>% 
  dplyr::summarise(scc_2300_2100_growth_dr2 = sum(weighted_damages2_scld, na.rm = T),
                   scc_2300_2100_growth_drramsey = sum(weighted_damages_ramsey_scld, na.rm = T))

scc_2300_2100_growth <- subset(scc_2300_2100_growth, scc_2300_2100_growth$scc_2300_2100_growth_dr2 !=0)

scc_country_lvl <- left_join(scc_2100, scc_2300_nog_post_2100, 
                             by = c("ISO3"))
scc_country_lvl <- left_join(scc_country_lvl, scc_2300_2100_growth, 
                             by = c("ISO3"))

scc_country_lvl$country_name <- countrycode::countrycode(scc_country_lvl$ISO3,
                                                         origin = "iso3c",
                                                         destination = "country.name")

scc_country_lvl <- scc_country_lvl %>% 
  dplyr::select(c("ISO3", "country_name",
                  "scc_2100_dr2","scc_2100_drramsey",
                  "scc_2300_nog_post_2100_dr2","scc_2300_nog_post_2100_drramsey",
                  "scc_2300_2100_growth_dr2","scc_2300_2100_growth_drramsey"))

colnames(scc_country_lvl)

write_csv(scc_country_lvl, "~/Desktop/scc_countries.csv")

