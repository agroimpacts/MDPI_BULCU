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
names(a_dat) <- c('PL_PLOTID','USER_ID','LON','LAT','2000a','2010a','2015a')

b_dat <- b_dat %>% dplyr::select(PL_PLOTID,USER_ID,"CENTER_LON","CENTER_LAT",
                                 'IS.THIS.MORE.THAN.50..CROP.FOR.2000..CROP',
                                 'IS.THIS.MORE.THAN.50..CROP.FOR.2010.CROP',
                                 'IS.THIS.MORE.THAN.50..CROP.2015.CROP')
names(b_dat) <- c('PL_PLOTID','USER_ID','LON','LAT','2000b','2010b','2015b')

c_dat <- c_dat %>% dplyr::select(PL_PLOTID,USER_ID,"CENTER_LON","CENTER_LAT",
                                 'IS.THIS.MORE.THAN.50..CROP.FOR.2000..CROP',
                                 'IS.THIS.MORE.THAN.50..CROP.FOR.2010.CROP',
                                 'IS.THIS.MORE.THAN.50..CROP.2015.CROP')
names(c_dat) <- c('PL_PLOTID','USER_ID','LON','LAT','2000c','2010c','2015c')

# conpair three works' validation results.
v_dat <- cbind(a_dat,b_dat,c_dat)
table(v_dat$`2000a`,v_dat$`2000b`)
table(v_dat$`2000a`,v_dat$`2000c`)
table(v_dat$`2000b`,v_dat$`2000c`)

table(v_dat$`2010a`,v_dat$`2010b`)
table(v_dat$`2010a`,v_dat$`2010c`)
table(v_dat$`2010b`,v_dat$`2010c`)

table(v_dat$`2015a`,v_dat$`2015b`)
table(v_dat$`2015a`,v_dat$`2015c`)
table(v_dat$`2015b`,v_dat$`2015c`)

