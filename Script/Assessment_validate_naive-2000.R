# naive 2000 means using GFSAD 2015 as if it is for the year 2000.
# This serves as a baseline for our BULC-U method,
# meaning without effort, how wrong would be in 2000.

library(dplyr)
library(raster)
library(sf)


wd <- '/media/sitian/HDD1/BULCU_TIFFS/'

# load GFSAD
gfsad <- raster(paste0(wd,'ZAM_GFSAD_2015/Zam_GFSAD_2015.tif'))

# load validation points
vote_dat <- read_sf(paste0(wd, 'Validation_Points/shp/vote_validation.shp')) %>% 
  dplyr::select(PL_PLOTID,vote2000) # this is NOT an error, we intentionally use 2000
procp_dat <- read_sf(paste0(wd, 'Validation_Points/shp/favor-crop_validation.shp')) %>% 
  dplyr::select(PL_PLOTID,procp2000)
pronon_dat <- read_sf(paste0(wd, 'Validation_Points/shp/favor-noncrop_validation.shp')) %>% 
  dplyr::select(PL_PLOTID,pronon2000)


# extract GFSAD using validation data
gfsad_vec <- gfsad %>% 
  extract(.,as_Spatial(vote_dat$geometry)) 

gfsad_vali <- cbind(gfsad_vec, vote_dat, procp_dat$procp2000,pronon_dat$pronon2000) %>%
  dplyr::rename(GFSAD = gfsad_vec,
                procp2000 = procp_dat.procp2000,
                pronon2000 = pronon_dat.pronon2000) 

rm(vote_dat,procp_dat,pronon_dat)

# error mat (horizontal index is ref, which is our sample)
table(gfsad_vali$GFSAD, gfsad_vali$vote2000)
table(gfsad_vali$GFSAD, gfsad_vali$procp2000)
table(gfsad_vali$GFSAD, gfsad_vali$pronon2000)

# calculate some score
vote_mat <- table(gfsad_vali$GFSAD, gfsad_vali$vote2000)
procp_mat <- table(gfsad_vali$GFSAD, gfsad_vali$procp2000)
pronon_mat <- table(gfsad_vali$GFSAD, gfsad_vali$pronon2000)


gfsad_cp <- vote_mat[2,] %>% sum() # =450

# calculate scores for vote
vote_cp <- vote_mat[,2] %>% sum()
gfsad_hit_vote <- vote_mat[2,2] # gfsad and vote agree for crop
vote_cp_usracc <- gfsad_hit_vote/gfsad_cp
vote_cp_prdacc <- gfsad_hit_vote/vote_cp
cat('naive 2000 + vote strategy','\n',
    paste0('user\'s acc.: ', round(vote_cp_usracc,3)),'\n',
    paste0('producer\'s acc.: ', round(vote_cp_prdacc,3))) 

# calculate scores for favor crop 
procp_cp <- procp_mat[,2] %>% sum()
gfsad_hit_procp <- procp_mat[2,2] # gfsad and vote agree for crop
procp_cp_usracc <- gfsad_hit_procp/gfsad_cp
procp_cp_prdacc <- gfsad_hit_procp/procp_cp 
cat('naive 2000 + favor crop strategy','\n',
    paste0('user\'s acc.: ', round(procp_cp_usracc,3)),'\n',
    paste0('producer\'s acc.: ', round(procp_cp_prdacc,3))) 


pronon_cp <- pronon_mat[,2] %>% sum()
gfsad_hit_pronon <- pronon_mat[2,2] # gfsad and vote agree for crop
pronon_cp_usracc <- gfsad_hit_pronon/gfsad_cp
pronon_cp_prdacc <- gfsad_hit_pronon/pronon_cp
cat('naive 2000 + favor non-crop strategy','\n',
    paste0('user\'s acc.: ', round(pronon_cp_usracc,3)),'\n',
    paste0('producer\'s acc.: ', round(pronon_cp_prdacc,3))) 

gfsad_vali_rslt <- data.frame(
  map_eval = "naive 2000 ",
  strategy = c('vote', 'favor_crop', 'favor_noncrop'),
  user_acc = c(round(vote_cp_usracc,3), 
               round(procp_cp_usracc,3),
               round(pronon_cp_usracc,3)),
  producer_acc = c(round(vote_cp_prdacc,3),
                   round(procp_cp_prdacc,3),
                   round(pronon_cp_prdacc,3))
)
write.csv(gfsad_vali_rslt,paste0(wd,'Error_assess/naive2000_eval.csv'))
# becareful, some variables use same name as in bulcu2000 evaluation!
# better remove all variables and have refesh run on bulcu2000 eval!
rm(list=ls())
