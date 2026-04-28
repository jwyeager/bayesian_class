###################################################
#                                                 #
#                 Project Script                  #
#                                                 #
###################################################
library(tidyverse)
library(magrittr)
library(R2OpenBUGS)


det <- read_csv('SI_Detections_StationName_24-25.csv')

det.summary <- det %>% group_by(Station) %>% 
  summarise(n.dets = n(), n.ind = length(unique(Transmitter)))

bio <- read_csv('MSCIP_2024_biomass.csv')
bio$sample.id <- gsub("-", "_", bio$sample.id)
bio <- bio %>% mutate(sta = str_sub(sample.id, 1, -3)) %>% 
  mutate(rep = rep(c("A", "B", "C"), length.out = n()))
bio.summary <- bio %>% group_by(sta) %>% 
  summarise(mean.ann = mean(ann), mean.mol = mean(mol), mean.art = mean(art),
            mean.ech = mean(ech), mean.misc = mean(misc), mean.tot = mean(tot.sample.biomass))

d <- inner_join(det.summary, bio.summary, by = c('Station' = 'sta'))

sed <- read_csv('MSCIP_2024_sediment.csv')
sed$sta <- gsub("-","_", sed$sta)

d <- inner_join(d, sed, by = c('Station' = 'sta'))

wq <- read_csv('MSCIP_2024_WQ.csv')
wq$sta.scratch <- gsub("-","_", wq$sta.scratch)
wq <- wq %>% mutate(sta.left = str_extract(sta.scratch, "^[^ ]+"))
sta.left.list <- wq %>% filter(!sta.left %in% c('M','S'))
sta.left.list <- sta.left.list$sta.left
wq <- wq %>% mutate(strata = rep(c("B", "M", "S"), length.out = n())) %>% 
  mutate(sta = rep(sta.left.list, each = 3))
wq.bot <- wq %>% select(sta, strata, 3:9) %>% filter(strata == 'B') %>% 
  rename_with(~ paste0(.x, ".bot"), 3:9) %>% select(!strata)
wq.mid <- wq %>% select(sta, strata, 3:9) %>% filter(strata == 'M') %>% 
  rename_with(~ paste0(.x, ".mid"), 3:9) %>% select(!strata)
wq.sur <- wq %>% select(sta, strata, 3:9) %>% filter(strata == 'S') %>% 
  rename_with(~ paste0(.x, ".sur"), 3:9) %>% select(!strata)
wq.pivot <- full_join(wq.bot, wq.mid, by = 'sta')
wq.pivot <- full_join(wq.pivot, wq.sur, by = 'sta')

d <- inner_join(d, wq.pivot, by = c('Station' = 'sta'))

station.coords <- read_csv('SI_Station_Coords_Match.csv')

d <- inner_join(d, station.coords, by = c('Station' = 'sta.name'))
write_csv(d, file = 'SI_BI_GS_AllCovariates.csv')
