library(dplyr)
library(raster)
library(sf)


wd <- '/media/sitian/HDD1/BULCU_TIFFS/'

# load bulcu2000
bulcu2000 <- raster(paste0(wd,'BULCU_grid2500_2016backward/shapelet_analysis/BULCU_updated_2000.tif'))

# load validation points
vote_dat <- read_sf(paste0(wd, 'Validation_Points/shp/vote_validation.shp')) %>% 
  dplyr::select(PL_PLOTID,vote2000)
procp_dat <- read_sf(paste0(wd, 'Validation_Points/shp/favor-crop_validation.shp')) %>% 
  dplyr::select(PL_PLOTID,procp2000)
pronon_dat <- read_sf(paste0(wd, 'Validation_Points/shp/favor-noncrop_validation.shp')) %>% 
  dplyr::select(PL_PLOTID,pronon2000)


# extract GFSAD using validation data (vote_dat is just geom template)
bulcu2000_vec <- bulcu2000 %>% 
  extract(.,as_Spatial(vote_dat$geometry)) 

bulcu2000_vali <- cbind(bulcu2000_vec, vote_dat, procp_dat$procp2000,pronon_dat$pronon2000) %>%
  dplyr::rename(BULCU2000 = bulcu2000_vec,
                procp2000 = procp_dat.procp2000,
                pronon2000 = pronon_dat.pronon2000) 

rm(vote_dat,procp_dat,pronon_dat)


# error mat
table(bulcu2000_vali$BULCU2000, bulcu2000_vali$vote2000)
table(bulcu2000_vali$BULCU2000, bulcu2000_vali$procp2000)
table(bulcu2000_vali$BULCU2000, bulcu2000_vali$pronon2000)

# calculate some score
b2000_vote_mat <- table(bulcu2000_vali$BULCU2000, bulcu2000_vali$vote2000)
b2000_procp_mat <- table(bulcu2000_vali$BULCU2000, bulcu2000_vali$procp2000)
b2000_pronon_mat <- table(bulcu2000_vali$BULCU2000, bulcu2000_vali$pronon2000)


BULCU2000_cp <- b2000_vote_mat[2,] %>% sum() 
b2000_vote_cp <- b2000_vote_mat[,2] %>% sum()
BULCU2000_hit_vote <- b2000_vote_mat[2,2] # gfsad and vote agree for crop
b2000_vote_cp_usracc <- BULCU2000_hit_vote/BULCU2000_cp
b2000_vote_cp_prdacc <- BULCU2000_hit_vote/b2000_vote_cp
cat('BULCU 2000 vote strategy','\n',
    paste0('user\'s acc.: ', round(b2000_vote_cp_usracc,3)),'\n',
    paste0('producer\'s acc.: ', round(b2000_vote_cp_prdacc,3))) 

b2000_procp_cp <- b2000_procp_mat[,2] %>% sum()
BULCU2000_hit_procp <- b2000_procp_mat[2,2] # gfsad and vote agree for crop
b2000_procp_cp_usracc <- BULCU2000_hit_procp/BULCU2000_cp
b2000_procp_cp_prdacc <- BULCU2000_hit_procp/b2000_procp_cp
cat('BULCU 2000 favor crop strategy','\n',
    paste0('user\'s acc.: ', round(b2000_procp_cp_usracc,3)),'\n',
    paste0('producer\'s acc.: ', round(b2000_procp_cp_prdacc,3))) 

b2000_pronon_cp <- b2000_pronon_mat[,2] %>% sum()
BULCU2000_hit_pronon <- b2000_pronon_mat[2,2] # gfsad and vote agree for crop
b2000_pronon_cp_usracc <- BULCU2000_hit_pronon/BULCU2000_cp
b2000_pronon_cp_prdacc <- BULCU2000_hit_pronon/b2000_pronon_cp 
cat('BULCU 2000 favor noncrop strategy','\n',
    paste0('user\'s acc.: ', round(b2000_pronon_cp_usracc,3)),'\n',
    paste0('producer\'s acc.: ', round(b2000_pronon_cp_prdacc,3))) 


b2000_vali_rslt <- data.frame(
  map_eval = "BULCU 2000",
  strategy = c('vote', 'favor_crop', 'favor_noncrop'),
  user_acc = c(round(b2000_vote_cp_usracc,3), 
               round(b2000_procp_cp_usracc,3),
               round(b2000_pronon_cp_usracc,3)),
  producer_acc = c(round(b2000_vote_cp_prdacc,3),
                   round(b2000_procp_cp_prdacc,3),
                   round(b2000_pronon_cp_prdacc,3))
)
write.csv(b2000_vali_rslt,paste0(wd,'Error_assess/bulcu2000_eval.csv'))
# becareful, some variables use same name as in gfsad evaluation!
# better remove all variables and have refesh run on gfsad eval!
rm(list=ls())
