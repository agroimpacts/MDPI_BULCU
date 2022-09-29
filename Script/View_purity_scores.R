library(dplyr)
library(sf)


scores <- list.files('/media/sitian/TB/BULCU_project/Purity_score/scores',full.names = T,
           pattern = 'original') %>% 
  lapply(read.csv) %>% 
  do.call(rbind,.) %>% 
  mutate(score = score %>% as.character() %>% 
           gsub("\\[", '', .) %>% 
           gsub("\\]", '', .) %>% 
           as.numeric())

grids <- read_sf('/media/sitian/HDD1/BULCU_TIFFS/grid_50x50/grid_50x50.shp') %>% 
  inner_join(scores,by=c("id"="cluster_id"))

write_sf(grids,'/media/sitian/TB/BULCU_project/Purity_score/spatial/original_2015.geojson')
