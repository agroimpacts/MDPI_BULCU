library(dplyr)
library(raster)
library(sf)


wd <- '/media/sitian/HDD1/BULCU_TIFFS/'

# load GFSAD
gfsad <- raster(paste0(wd,'ZAM_GFSAD_2015/Zam_GFSAD_2015.tif'))

# load validation points
vote_dat <- read_sf(paste0(wd, 'Validation_Points/shp/vote_validation.shp')) %>% 
  dplyr::select(PL_PLOTID,vote2015)
procp_dat <- read_sf(paste0(wd, 'Validation_Points/shp/favor-crop_validation.shp')) %>% 
  dplyr::select(PL_PLOTID,procp2015)
pronon_dat <- read_sf(paste0(wd, 'Validation_Points/shp/favor-noncrop_validation.shp')) %>% 
  dplyr::select(PL_PLOTID,pronon2015)


# extract GFSAD using validation data
gfsad_vec <- gfsad %>% 
  extract(.,as_Spatial(vote_dat$geometry)) 

gfsad_vali <- cbind(gfsad_vec, vote_dat, procp_dat$procp2015,pronon_dat$pronon2015) %>%
  dplyr::rename(GFSAD = gfsad_vec,
                procp2015 = procp_dat.procp2015,
                pronon2015 = pronon_dat.pronon2015) 

rm(vote_dat,procp_dat,pronon_dat)

# error mat (horizontal index is ref, which is our sample)
table(gfsad_vali$GFSAD, gfsad_vali$vote2015)
table(gfsad_vali$GFSAD, gfsad_vali$procp2015)
table(gfsad_vali$GFSAD, gfsad_vali$pronon2015)

# calculate some score
vote_mat <- table(gfsad_vali$GFSAD, gfsad_vali$vote2015)
procp_mat <- table(gfsad_vali$GFSAD, gfsad_vali$procp2015)
pronon_mat <- table(gfsad_vali$GFSAD, gfsad_vali$pronon2015)


gfsad_cp <- vote_mat[2,] %>% sum() # =450

# calculate scores for vote
vote_cp <- vote_mat[,2] %>% sum()
gfsad_hit_vote <- vote_mat[2,2] # gfsad and vote agree for crop
vote_cp_usracc <- gfsad_hit_vote/gfsad_cp
vote_cp_prdacc <- gfsad_hit_vote/vote_cp
cat('GFSAD 2015 vote strategy','\n',
    paste0('user\'s acc.: ', round(vote_cp_usracc,3)),'\n',
    paste0('producer\'s acc.: ', round(vote_cp_prdacc,3))) 

# calculate scores for favor crop 
procp_cp <- procp_mat[,2] %>% sum()
gfsad_hit_procp <- procp_mat[2,2] # gfsad and vote agree for crop
procp_cp_usracc <- gfsad_hit_procp/gfsad_cp
procp_cp_prdacc <- gfsad_hit_procp/procp_cp 
cat('GFSAD 2015 favor crop strategy','\n',
    paste0('user\'s acc.: ', round(procp_cp_usracc,3)),'\n',
    paste0('producer\'s acc.: ', round(procp_cp_prdacc,3))) 


pronon_cp <- pronon_mat[,2] %>% sum()
gfsad_hit_pronon <- pronon_mat[2,2] # gfsad and vote agree for crop
pronon_cp_usracc <- gfsad_hit_pronon/gfsad_cp
pronon_cp_prdacc <- gfsad_hit_pronon/pronon_cp
cat('GFSAD 2015 favor non-crop strategy','\n',
    paste0('user\'s acc.: ', round(pronon_cp_usracc,3)),'\n',
    paste0('producer\'s acc.: ', round(pronon_cp_prdacc,3))) 

gfsad_vali_rslt <- data.frame(
  map_eval = "GFSAD_2015",
  strategy = c('vote', 'favor_crop', 'favor_noncrop'),
  user_acc = c(round(vote_cp_usracc,3), 
               round(procp_cp_usracc,3),
               round(pronon_cp_usracc,3)),
  producer_acc = c(round(vote_cp_prdacc,3),
                   round(procp_cp_prdacc,3),
                   round(pronon_cp_prdacc,3))
)
write.csv(gfsad_vali_rslt,paste0(wd,'Error_assess/gfsad2015_eval.csv'))
# becareful, some variables use same name as in bulcu2000 evaluation!
# better remove all variables and have refesh run on bulcu2000 eval!
rm(list=ls())
