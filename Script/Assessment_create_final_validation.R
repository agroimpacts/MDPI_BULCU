library(dplyr)
library(raster)
library(sf)


wd <- '/media/sitian/HDD1/BULCU_TIFFS/'

# load validation points
a_dat <- read.csv(paste0(wd,
                         '/Validation_Points/ceo-zmb-classified-crop-validation-a-plot-data-2020-09-14.csv'))
b_dat <- read.csv(paste0(wd,
                         '/Validation_Points/ceo-zmb-classified-crop-validation-b-plot-data-2020-09-14.csv'))
c_dat <- read.csv(paste0(wd,
                         '/Validation_Points/ceo-zmb-classified-crop-validation-c-plot-data-2020-09-14.csv'))

# clean the data
a_dat <- a_dat %>% dplyr::select(PL_PLOTID,USER_ID,"CENTER_LON","CENTER_LAT",
                                 'IS.THIS.MORE.THAN.50..CROP.FOR.2000..CROP',
                                 'IS.THIS.MORE.THAN.50..CROP.FOR.2010.CROP',
                                 'IS.THIS.MORE.THAN.50..CROP.2015.CROP')
names(a_dat) <- c('PL_PLOTID','USER_ID','LON','LAT','2000','2010','2015')

b_dat <- b_dat %>% dplyr::select(PL_PLOTID,USER_ID,"CENTER_LON","CENTER_LAT",
                                 'IS.THIS.MORE.THAN.50..CROP.FOR.2000..CROP',
                                 'IS.THIS.MORE.THAN.50..CROP.FOR.2010.CROP',
                                 'IS.THIS.MORE.THAN.50..CROP.2015.CROP')
names(b_dat) <- c('PL_PLOTID','USER_ID','LON','LAT','2000','2010','2015')

c_dat <- c_dat %>% dplyr::select(PL_PLOTID,USER_ID,"CENTER_LON","CENTER_LAT",
                                 'IS.THIS.MORE.THAN.50..CROP.FOR.2000..CROP',
                                 'IS.THIS.MORE.THAN.50..CROP.FOR.2010.CROP',
                                 'IS.THIS.MORE.THAN.50..CROP.2015.CROP')
names(c_dat) <- c('PL_PLOTID','USER_ID','LON','LAT','2000','2010','2015')

# hide the worker's email
v_dat <- rbind(a_dat,b_dat,c_dat) %>% 
  mutate(USER_ID = ifelse(USER_ID == 'priscilla.baltezar@gmail.com','P',
                          ifelse(USER_ID == 'EBaldwin@Clarku.edu','E',
                                 ifelse(USER_ID == 'screma@clarku.edu','S','other'))))

# get the coordinates of plots for later use
coords_dat <- v_dat %>% 
  dplyr::select(PL_PLOTID, LON, LAT) %>% 
  distinct()

v_dat <- v_dat %>% 
  group_by(PL_PLOTID) %>% 
  summarise_at(., vars(c('2000','2010','2015')), tibble::lst(sum)) %>% 
  ungroup() %>% 
  dplyr::rename('2000'='2000_sum','2010'='2010_sum','2015'='2015_sum')

rm(a_dat,b_dat,c_dat)

# strategy 1: vote.
vote_dat <- v_dat %>% 
  mutate(vote2000 = ifelse(`2000` >=200, 1,0), # 200 means two works think it is crop (100+100+0)
         vote2010 = ifelse(`2010` >=200, 1,0),
         vote2015 = ifelse(`2015` >=200, 1,0)) %>% 
  dplyr::select(-c(`2000`,`2010`,`2015`))

procrop_dat <- v_dat %>% 
  mutate(procp2000 = ifelse(`2000` >=100, 1,0),
         procp2010 = ifelse(`2010` >=100, 1,0),
         procp2015 = ifelse(`2015` >=100, 1,0)) %>% 
  dplyr::select(-c(`2000`,`2010`,`2015`))

pronon_dat <- v_dat %>% 
  mutate(pronon2000 = ifelse(`2000` ==300, 1,0),
         pronon2010 = ifelse(`2010` ==300, 1,0),
         pronon2015 = ifelse(`2015` ==300, 1,0)) %>% 
  dplyr::select(-c(`2000`,`2010`,`2015`))

# add coords
vote_dat <- vote_dat %>% 
  inner_join(., coords_dat) %>% 
  st_as_sf(., coords = c("LON", "LAT"), crs = 4326)

procrop_dat <- procrop_dat %>% 
  inner_join(., coords_dat) %>% 
  st_as_sf(., coords = c("LON", "LAT"), crs = 4326)

pronon_dat <- pronon_dat %>% 
  inner_join(., coords_dat) %>% 
  st_as_sf(., coords = c("LON", "LAT"), crs = 4326)

# export
write_sf(vote_dat,paste0(wd,'Validation_Points/shp/vote_validation.shp'))
write_sf(procrop_dat,paste0(wd,'Validation_Points/shp/favor-crop_validation.shp'))
write_sf(pronon_dat,paste0(wd,'Validation_Points/shp/favor-noncrop_validation.shp'))

