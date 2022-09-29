library(dplyr)
library(raster)
library(sf)

wd <- '/media/sitian/HDD1/BULCU_TIFFS/'

# load bulcu detected crop gain
# gain_map <- 'masked_shapelet_Mosaic_binary.tif'
# gain_map <- 'crop_gain_n0005_Mosaic.tif'
            #'crop_gain_n001_Mosaic.tif'
            #'crop_gain_n0015_Mosaic.tif'
            #'crop_gain_n002_Mosaic.tif'
            'masked_shapelet_Mosaic_binary.tif'# this is n003
gain_map_list <- c('crop_gain_n0005_Mosaic.tif',
                   'crop_gain_n001_Mosaic.tif',
                   'crop_gain_n0015_Mosaic.tif',
                   'crop_gain_n002_Mosaic.tif',
                   'crop_gain_n0025_Mosaic.tif',
                   'masked_shapelet_Mosaic_binary.tif')

# We do two type of assessment:
# 1. crop change captured by BULC-U
# 2. crop captured by GFSAD, and change captured BULC-U

# val_type <- "favor-crop"
# val_nick <- "procp"

val_type <- "vote"
val_nick <- "vote"

# load validation points
val_dat <- read_sf(paste0(wd, paste0('Validation_Points/shp/',val_type,'_validation.shp'))) %>%
  dplyr::select(PL_PLOTID,paste0(val_nick,'2000'),paste0(val_nick,'2010'),paste0(val_nick,'2015'))

# Type 1 assessment (not consider GFSAD)
type1 <- sapply(gain_map_list, function(gain_map){
  print(gain_map)
  bulcu_gain <- raster(paste0(wd,'BULCU_grid2500_2016backward/shapelet_analysis/',gain_map))

  # find out crop changed samples
  
  # real crop gain
  val_dlt_p <- val_dat %>% filter(!!as.name(paste0(val_nick,'2000')) == 0 & !!as.name(paste0(val_nick,'2015'))==1)
  # real persistent (persist crop & persist non-crop)
  val_dlt_static <- val_dat %>% filter(!!as.name(paste0(val_nick,'2000')) == 0 & 
                                       !!as.name(paste0(val_nick,'2010'))==0 & 
                                       !!as.name(paste0(val_nick,'2015'))==0 |
                                       !!as.name(paste0(val_nick,'2000')) == 1 & 
                                       !!as.name(paste0(val_nick,'2010'))==1 & 
                                       !!as.name(paste0(val_nick,'2015'))==1 )
  # real crop lost (this include both 1,0,0 and 1,1,0 in 2000, 2010, and 2015)
  val_dlt_n <- val_dat %>% filter(!!as.name(paste0(val_nick,'2000')) == 1 & 
                                  !!as.name(paste0(val_nick,'2015'))==0)


  # extract

  gain_vec_val <- bulcu_gain %>%
    extract(.,as_Spatial(val_dlt_p$geometry))
  falsegain_vec_val <- bulcu_gain %>%
    extract(.,as_Spatial(val_dlt_static$geometry))


  table(gain_vec_val)
  table(falsegain_vec_val)

  val_score <- (table(gain_vec_val)[2]/sum(table(gain_vec_val))) %>% round(3)



  false_gain_rate <- (table(falsegain_vec_val)[2]/sum(table(falsegain_vec_val))) %>%
    round(3)
  # cat('Crop change caught by BULCU','\n',
  #     'hit rate: ', procp_score, '\n',
  #     'false positive rate: ',false_gain_rate)
  return(c(val_score,false_gain_rate))

})



# Type 2 assessment, considering the GFSAD 2015 accuracy,
# how much crop (captured by GFSAD) was detected by BULC-U
# vote_dlt_p_gfsad <- cbind(vote_dat,gfsad_vec) %>%
#   dplyr::rename(GFSAD = 'gfsad_vec') %>%
#   filter(vote2000 == 0 & vote2015==1 & GFSAD ==1)

# load GFSAD
gfsad <- raster(paste0(wd,'ZAM_GFSAD_2015/Zam_GFSAD_2015.tif'))
# extract GFSAD
gfsad_vec <- gfsad %>%
  extract(.,as_Spatial(val_dat$geometry))

type2 <- sapply(gain_map_list, function(gain_map){
  print(gain_map)
  bulcu_gain <- raster(paste0(wd,'BULCU_grid2500_2016backward/shapelet_analysis/',gain_map))
  val_dlt_p_gfsad <- cbind(val_dat,gfsad_vec) %>%
    dplyr::rename(GFSAD = 'gfsad_vec') %>%
    filter(!!as.name(paste0(val_nick,'2000')) == 0 &
           !!as.name(paste0(val_nick,'2015'))==1 & GFSAD ==1)

  val_static_gfsad <- cbind(val_dat,gfsad_vec) %>%
    dplyr::rename(GFSAD = 'gfsad_vec') %>%
    filter(!!as.name(paste0(val_nick,'2000')) == 0 & 
             !!as.name(paste0(val_nick,'2010'))==0 &
             !!as.name(paste0(val_nick,'2015'))==0 & GFSAD ==1|
             !!as.name(paste0(val_nick,'2000')) == 1 & 
             !!as.name(paste0(val_nick,'2010'))==1 &
             !!as.name(paste0(val_nick,'2015'))==1 & GFSAD ==1)

  # extract


  gain_vec_val_withG <- bulcu_gain %>%
    extract(.,as_Spatial(val_dlt_p_gfsad$geometry))
  falsegain_vec_val_withG <- bulcu_gain %>%
    extract(.,as_Spatial(val_static_gfsad$geometry))

  table(gain_vec_val_withG)
  table(falsegain_vec_val_withG)


  val_withG_score <- (table(gain_vec_val_withG)[2]/sum(table(gain_vec_val_withG))) %>% round(3)
  false_gain_rate <- (table(falsegain_vec_val_withG)[2]/sum(table(falsegain_vec_val_withG))) %>%
    round(3)
  return(c(val_withG_score,false_gain_rate))
})


type1_tidy <- type1 %>% 
  t %>% 
  as.data.frame() %>% 
  cbind(seq(0.005,0.03,0.005)) %>% 
  cbind(val_type) %>% 
  cbind("In Zambia")
names(type1_tidy) <- c('hit','false_alarm','thresh','val_type','where')

type2_tidy <- type2 %>% 
  t %>% 
  as.data.frame() %>% 
  cbind(seq(0.005,0.03,0.005)) %>% 
  cbind(val_type) %>% 
  cbind("In GFSAD")
names(type2_tidy) <- c('hit','false_alarm','thresh','val_type','where')

types_eval <- type1_tidy %>% rbind(type2_tidy)
write.csv(types_eval, paste0(wd,paste0('Error_assess/cropgain_evals_',
                                       val_type,
                                       '.csv')))


### Scratch################################
cat('Crop change in GFSAD caught by BULCU','\n',
  #  'vote: ', vote_withG_score, '\n',
    'favor crop: ', val_withG_score, '\n')
   # 'favor non-crop: ', pronon_withG_score)

#write.csv(crop_gain_vali_rslt,paste0(wd,'Error_assess/cropgain_n0005_eval.csv'))
#rm(list=ls())



# export points to see which crop gain not captured by BULC-U

procp_dlt_p_gfsad$BULCU_gain <- gain_vec_procp_withG
procp_dlt_p_gfsad_miss <- procp_dlt_p_gfsad %>% filter(BULCU_gain==0)

#write_sf(vote_dlt_p_gfsad_miss, paste0(wd,'Error_assess/spatial/cropgain_n0005_vote_miss.geojson'))
#write_sf(procp_dlt_p_gfsad_miss, paste0(wd,'Error_assess/spatial/cropgain_n0005_procp_miss.geojson'))
#write_sf(pronon_dlt_p_gfsad_miss, paste0(wd,'Error_assess/spatial/cropgain_n0005_pronon_miss.geojson'))




# evaluate false positive in GFSAD
all_vec_procp <- bulcu_gain %>%
  extract(., procp_dat)

procp_dat$bulcu_gain <- all_vec_procp
false_gain <- procp_dat %>% filter(procp2000 ==1 & bulcu_gain ==1) %>% nrow()
procp_cp_2000 <- procp_dat %>% filter(procp2000 ==1 ) %>% nrow()
false_gain_rate <- (false_gain/procp_cp_2000) %>% round(3)
