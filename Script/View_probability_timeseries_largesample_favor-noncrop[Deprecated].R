library(sf)
library(raster)
library(dplyr)
library(ggplot2)

wd <- '/media/sitian/HDD1/BULCU_TIFFS/'

# randomly sample crop and non-crop from 812 crops.
# use favoring crop strategy
pts <- read_sf('/media/sitian/HDD1/BULCU_TIFFS/Validation_Points/shp/favor-noncrop_validation.shp')

#pts <- read_sf('/media/sitian/HDD1/BULCU_TIFFS/Validation_Points/shp/vote_validation.shp')

# get points for calibration and visualization purpose
#set.seed(2)
#cpts <- pts %>% sample_n(., size=160) 



cpts1 <- pts %>% filter(pronon2000==0&pronon2010==0&pronon2015==0) #%>% sample_n(., size=30)
cpts2 <- pts %>% filter(pronon2000==0&pronon2010==0&pronon2015==1) #%>% sample_n(., size=30,replace = T)
cpts3 <- pts %>% filter(pronon2000==0&pronon2010==1&pronon2015==1) #%>% sample_n(., size=30,replace = T)
cpts4 <- pts %>% filter(pronon2000==1&pronon2010==1&pronon2015==1) #%>% sample_n(., size=30)
cpts <- rbind(cpts1, cpts2, cpts3, cpts4)
#
grids <- read_sf(paste0(wd,'grid_50x50/grid_50x50.shp'))

cpts <- cpts %>% st_intersection(grids) %>%
  mutate(grid_id = id) %>% 
  dplyr::select(-id)


probs <- sapply(1:nrow(cpts), function(x){
  print(x)
  grid_id <- cpts[[x,'grid_id']]
  r <- raster::brick(paste0(wd,
                            'BULCU_grid2500_2016backward/BULCU-all_2016_backward/',
                            'grid_',
                            grid_id,
                            '_BULCU.tif'))
  extract(r, cpts[x,])/65532
}) 

crop_probs <- probs %>% 
  as.data.frame() %>% 
  filter(row_number() %% 2 == 0) #get every other val (ignor non-crop, get crop)

names(crop_probs) <-cpts$grid_id
crop_probs$year <- seq(2015,2000)
crop_probs <- reshape2::melt(crop_probs, id='year')
crop_probs <- crop_probs %>% 
  mutate(id = as.character(variable)) %>% 
  dplyr::select(-variable)

typevec <- cpts %>% 
  mutate(change_type = as.character(group_indices_(., .dots=c("pronon2000",
                                                              "pronon2010",
                                                              "pronon2015"))),
         grid_id = as.character(grid_id)) %>% 
  dplyr::select(grid_id,change_type)

check_type <- cpts %>% 
  mutate(change_type = as.character(group_indices_(., .dots=c("pronon2000",
                                                              "pronon2010",
                                                              "pronon2015"))),
         grid_id = as.character(grid_id)) %>% 
  dplyr::select("pronon2000","pronon2010","pronon2015",change_type)

crop_probs <- crop_probs %>% inner_join(., typevec,
                                        by = c('id'='grid_id'))


save(crop_probs,file="/media/sitian/TB/BULCU_project/Rdata_figrues/prob_pronon_812spl.rda")
load("/media/sitian/TB/BULCU_project/Rdata_figrues/prob_pronon_812spl.rda")


# plot them
p1 <- crop_probs %>% filter(change_type==1) %>% 
  # filter(!id %in% c(crop_probs %>% 
  #                     filter(change_type==1) %>% 
  #                     filter(year==2015&value>0.5) %>% 
  #                     pull(id) %>% unique())) %>% 
  ggplot(.,aes(year,value))+
  #geom_point()+
  geom_boxplot(aes(y=value, group=year),
               outlier.shape = NA)+
  geom_jitter(width = 0.2,size=0.1)+
  scale_y_continuous(breaks = seq(0,1,0.2), limits = c(0,1))+
  geom_line(aes(y=value, x=year,group=id),alpha=0.2)+
  ggthemes::theme_few()+
  labs(title="persistent non-crop probabilities",
       x ="Year", y = "Crop Probability")


p2 <- crop_probs %>% filter(change_type==2) %>% 
  # filter(!id %in% c(crop_probs %>% 
  #                     filter(change_type==2) %>% 
  #                     filter(year==2015&value<0.5) %>% 
  #                     pull(id) %>% unique())) %>% 
  ggplot(.,aes(year,value))+
  #  geom_point()+
  geom_boxplot(aes(y=value, group=year),
               outlier.shape = NA)+
  geom_jitter(width = 0.2,size=0.1)+
  scale_y_continuous(breaks = seq(0,1,0.2), limits = c(0,1))+
  geom_line(aes(y=value, x=year,group=id),alpha=0.2)+
  ggthemes::theme_few()+
  labs(title="late crop gain",
       x ="Year", y = "Crop Probability")

p3 <- crop_probs %>% filter(change_type==3) %>% 
  # filter(!id %in% c(crop_probs %>% 
  #                     filter(change_type==3) %>% 
  #                     filter(year==2015&value<0.5) %>% 
  #                     pull(id) %>% unique())) %>% 
  ggplot(.,aes(year,value))+
  #  geom_point()+
  geom_boxplot(aes(y=value, group=year),
               outlier.shape = NA)+
  geom_jitter(width = 0.2,size=0.1)+
  scale_y_continuous(breaks = seq(0,1,0.2), limits = c(0,1))+
  ggthemes::theme_few()+
  geom_line(aes(y=value, x=year,group=id),alpha=0.2)+
  labs(title="early crop gain",
       x ="Year", y = "Crop Probability")

p4 <- crop_probs %>% filter(change_type==4) %>% 
  # filter(!id %in% c(crop_probs %>% 
  #                     filter(change_type==4) %>% 
  #                     filter(year==2015&value<0.5) %>% 
  #                     pull(id) %>% unique())) %>% 
  ggplot(.,aes(year,value))+
  #  geom_point()+
  geom_boxplot(aes(y=value, group=year),
               outlier.shape = NA)+
  geom_jitter(width = 0.2,size=0.1)+
  scale_y_continuous(breaks = seq(0,1,0.2), limits = c(0,1))+
  ggthemes::theme_few()+
  labs(title="persistent crop",
       x ="Year", y = "Crop Probability")


gridExtra::grid.arrange(grobs=list(p1,p2,p3, p4), 
                        #  top="Main Title",
                        ncol = 2, nrow = 2)
