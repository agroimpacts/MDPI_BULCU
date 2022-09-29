# We have decided to use favoring crop strategy to combine the 3 workers' results
# in Assessment_create_final_validation.R

# Here we exam some statistics of crop and non-crop in 2000, 2010, and 2015

library(dplyr)
library(raster)
library(sf)


wd <- '/media/sitian/HDD1/BULCU_TIFFS/'

# val_type <- "favor-crop"
# val_nick <- "procp"

val_type <- "vote"
val_nick <- "vote"


dat <- read_sf(paste0(wd,paste0('Validation_Points/shp/',val_type,'_validation.shp')))

# the number of crop and non crop in 2000
table(dat[[paste0(val_nick,'2000')]])
# the number of crop and non crop in 2010
table(dat[[paste0(val_nick,'2010')]])
# the number of crop and non crop in 2015
table(dat[[paste0(val_nick,'2015')]])


# 2000 to 2010
# Check the persistent crop 
dat %>% filter(!!as.name(paste0(val_nick,'2000'))==1&!!as.name(paste0(val_nick,'2010'))==1) %>% nrow()
# Check the crop gain 
dat %>% filter(!!as.name(paste0(val_nick,'2000'))==0&!!as.name(paste0(val_nick,'2010'))==1) %>% nrow()
# Check the crop lost 
dat %>% filter(!!as.name(paste0(val_nick,'2000'))==1&!!as.name(paste0(val_nick,'2010'))==0) %>% nrow()
# Check the persistent non-crop
dat %>% filter(!!as.name(paste0(val_nick,'2000'))==0&!!as.name(paste0(val_nick,'2010'))==0) %>% nrow()

# 2010 to 2015
# Check the persistent crop 
dat %>% filter(!!as.name(paste0(val_nick,'2010'))==1&!!as.name(paste0(val_nick,'2015'))==1) %>% nrow()
# Check the crop gain 
dat %>% filter(!!as.name(paste0(val_nick,'2010'))==0&!!as.name(paste0(val_nick,'2015'))==1) %>% nrow()
# Check the crop lost 
dat %>% filter(!!as.name(paste0(val_nick,'2010'))==1&!!as.name(paste0(val_nick,'2015'))==0) %>% nrow()
# Check the persistent non-crop 
dat %>% filter(!!as.name(paste0(val_nick,'2010'))==0&!!as.name(paste0(val_nick,'2015'))==0) %>% nrow()





# 2000 to 2015 
dat %>% filter(!!as.name(paste0(val_nick,'2000'))==1&!!as.name(paste0(val_nick,'2015'))==1) %>% nrow()
# Check the expected crop gain (non crop in 2000 then crop in 2015)
dat %>% filter(!!as.name(paste0(val_nick,'2000'))==0&!!as.name(paste0(val_nick,'2015'))==1) %>% nrow()
# Check the expected crop lost (crop in 2000 then noncrop in 2015)
dat %>% filter(!!as.name(paste0(val_nick,'2000'))==1&!!as.name(paste0(val_nick,'2015'))==0) %>% nrow()
# Check the persistent non-crop
dat %>% filter(!!as.name(paste0(val_nick,'2000'))==0&!!as.name(paste0(val_nick,'2015'))==0) %>% nrow()


# 2000, 2010, and 2015
dat %>% filter(!!as.name(paste0(val_nick,'2000'))==1&!!as.name(paste0(val_nick,'2010'))==1&!!as.name(paste0(val_nick,'2015'))==1) %>% nrow()
# Check the expected crop gain (non crop in 2000 then crop in 2015)
dat %>% filter(!!as.name(paste0(val_nick,'2000'))==0&!!as.name(paste0(val_nick,'2015'))==1) %>% nrow()
# Check the expected crop lost (crop in 2000 then noncrop in 2015)
dat %>% filter(!!as.name(paste0(val_nick,'2000'))==1&!!as.name(paste0(val_nick,'2015'))==0) %>% nrow()
# Check the persistent non-crop in 2000,2010,2015
dat %>% filter(!!as.name(paste0(val_nick,'2000'))==0&!!as.name(paste0(val_nick,'2010'))==0&!!as.name(paste0(val_nick,'2015'))==0) %>% nrow()
