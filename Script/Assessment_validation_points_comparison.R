library(sf)
library(dplyr)

procrop <- read_sf('/media/sitian/HDD1/BULCU_TIFFS/Validation_Points/shp/favor-crop_validation.shp')
vote <- read_sf('/media/sitian/HDD1/BULCU_TIFFS/Validation_Points/shp/vote_validation.shp')

# Let 
# procp2015==0 & vote2015==0 --> 0
# procp2015==1 & vote2015==1 --> 1
# procp2015==0 & vote2015==1 --> 2
# procp2015==1 & vote2015==0 --> 3
comp <- procrop %>% 
  inner_join(vote %>% st_drop_geometry()) %>% 
  mutate(comb2015 = ifelse(procp2015==0 & vote2015==0,0,
           ifelse(procp2015==1 & vote2015==1,1,
           ifelse(procp2015==0 & vote2015==1,2,
           ifelse(procp2015==1 & vote2015==0,3,NA)))),
         comb2010 = ifelse(procp2010==0 & vote2010==0,0,
           ifelse(procp2010==1 & vote2010==1,1,
           ifelse(procp2010==0 & vote2010==1,2,
           ifelse(procp2010==1 & vote2010==0,3,NA)))),
         comb2000 = ifelse(procp2000==0 & vote2000==0,0,
           ifelse(procp2000==1 & vote2000==1,1,
           ifelse(procp2000==0 & vote2000==1,2,
           ifelse(procp2000==1 & vote2000==0,3,NA)))))
         
write_sf(comp,'/media/sitian/HDD1/BULCU_TIFFS/Validation_Points/shp/compare.shp')

