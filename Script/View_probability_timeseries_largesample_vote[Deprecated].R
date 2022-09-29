library(sf)
library(raster)
library(dplyr)
library(ggplot2)

wd <- '/media/sitian/HDD1/BULCU_TIFFS/'

# randomly sample crop and non-crop from 812 crops.
# use vote strategy
pts <- read_sf('/media/sitian/HDD1/BULCU_TIFFS/Validation_Points/shp/vote_validation.shp')

# load GFSAD
gfsad <- raster(paste0(wd,'ZAM_GFSAD_2015/Zam_GFSAD_2015.tif'))

# get points for calibration and visualization purpose
#set.seed(2)
#cpts <- pts %>% sample_n(., size=160) 

cpts0 <- pts %>% filter(vote2000==0&vote2015==1)

cpts1 <- pts %>% filter(vote2000==0&vote2010==0&vote2015==0) #%>% sample_n(., size=30)
cpts2 <- pts %>% filter(vote2000==0&vote2010==0&vote2015==1) #%>% sample_n(., size=30,replace = T)
cpts3 <- pts %>% filter(vote2000==0&vote2010==1&vote2015==1) #%>% sample_n(., size=30,replace = T)
cpts4 <- pts %>% filter(vote2000==1&vote2010==1&vote2015==1) #%>% sample_n(., size=30)
cpts <- rbind(cpts1, cpts2, cpts3, cpts4)
rm(cpts0,cpts1,cpts2,cpts3,cpts4)
#

grids <- read_sf(paste0(wd,'grid_50x50/grid_50x50.shp'))
# cpts <- cpts0
cpts <- cpts %>% st_intersection(grids) %>%
  mutate(grid_id = id) %>% 
  dplyr::select(-id)


init_prob <- cbind(init_prob=extract(gfsad,cpts),cpts) %>% 
  dplyr::select(init_prob) %>% 
  mutate(init_prob=ifelse(init_prob==1,0.8,0.2)) 


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

names(crop_probs) <-cpts$PL_PLOTID
crop_probs <- rbind(init_prob$init_prob,crop_probs) # add gfsad init prob to be 2016
crop_probs$year <- seq(2016,2000)
crop_probs <- reshape2::melt(crop_probs, id='year')
crop_probs <- crop_probs %>% 
  mutate(PL_PLOTID = as.character(variable)) %>% 
  dplyr::select(-variable)

typevec <- cpts %>% 
  mutate(change_type = as.character(group_indices_(., .dots=c("vote2000",
                                                 "vote2010",
                                                 "vote2015"))),
         PL_PLOTID=as.character(PL_PLOTID)) %>% 
  dplyr::select(PL_PLOTID,change_type)

check_type <- cpts %>% 
  mutate(change_type = as.character(group_indices_(., .dots=c("vote2000",
                                                              "vote2010",
                                                              "vote2015"))),
         PL_PLOTID=as.character(PL_PLOTID)) %>% 
  dplyr::select("vote2000","vote2010","vote2015",change_type)

crop_probs <- crop_probs %>% inner_join(., typevec,
                          by = c('PL_PLOTID'='PL_PLOTID'))

save(crop_probs,file="/media/sitian/TB/BULCU_project/Rdata_figrues/prob_vote_812spl_withGFSAD.rda")
rm(cpts,gfsad,pts,grids)
load("/media/sitian/TB/BULCU_project/Rdata_figrues/prob_vote_812spl_withGFSAD.rda")

crop_probs <- crop_probs %>% 
  dplyr::group_by(PL_PLOTID) %>%
  mutate(initcon =case_when(any(year == 2016 & value >0.5) ~ '1', TRUE ~ '0'))


# plot them
p5 <- crop_probs %>% filter(change_type==1) %>% 
  # filter(!id %in% c(crop_probs %>% 
  #                     filter(change_type==1) %>% 
  #                     filter(year==2015&value>0.5) %>% 
  #                     pull(id) %>% unique())) %>% 
ggplot(.,aes(year,value))+
  #geom_point()+
  # geom_line(aes(y=value, x=year,group=PL_PLOTID,
  #               col = initcon),
  #           alpha=0.5)+
  geom_boxplot(aes(y=value, group=year),
               fatten = 3,
               outlier.shape = NA,
               #outlier.shape = 1,
               #outlier.size = 0.5,
               alpha=0.6)+
  #geom_jitter(width = 0.2,size=0.1)+
  scale_y_continuous(breaks = seq(0,1,0.2), limits = c(0,1))+
  scale_x_continuous(breaks = seq(2000,2015,1), limits = c(1999,2017))+
#  geom_line(aes(group=id))+
  ggthemes::theme_few()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=12),
        legend.position="none")+
  labs(title="Persistent non-crop",
       x ="Year", y = "Crop Probability")


p6 <- crop_probs %>% filter(change_type==2) %>% 
  # filter(!id %in% c(crop_probs %>% 
  #                     filter(change_type==2) %>% 
  #                     filter(year==2015&value<0.5) %>% 
  #                     pull(id) %>% unique())) %>% 
  ggplot(.,aes(year,value))+
  #geom_point()+
  # geom_line(aes(y=value, x=year,group=PL_PLOTID,
  #               col = initcon),alpha=0.5)+
  geom_boxplot(aes(y=value, group=year),
               outlier.shape = NA,
               #outlier.shape = 1,
               #outlier.size = 0.5,
               alpha=0.6,
               fatten = 3)+
  #geom_jitter(width = 0.2,size=0.1)+

  scale_y_continuous(breaks = seq(0,1,0.2), limits = c(0,1))+
  scale_x_continuous(breaks = seq(2000,2015,1), limits = c(1999,2017))+
  #  geom_line(aes(group=id))+
  ggthemes::theme_few()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=12),
        legend.position="none")+
  labs(title="Late cropland gain",
       x ="Year", y = "Crop Probability")

p7 <- crop_probs %>% filter(change_type==3) %>% 
  # filter(!id %in% c(crop_probs %>% 
  #                     filter(change_type==3) %>% 
  #                     filter(year==2015&value<0.5) %>% 
  #                     pull(id) %>% unique())) %>% 
  ggplot(.,aes(year,value))+
  #geom_point()+

  #geom_jitter(width = 0.2,size=0.1)+
  # geom_line(aes(y=value, x=year,group=PL_PLOTID,
  #               col = initcon),alpha=0.5)+
  geom_boxplot(aes(y=value, group=year),
               outlier.shape = NA,
               #outlier.shape = 1,
               #outlier.size = 0.5,
               alpha=0.6,
               fatten = 3)+
  scale_y_continuous(breaks = seq(0,1,0.2), limits = c(0,1))+
  scale_x_continuous(breaks = seq(2000,2015,1), limits = c(1999,2017))+
  #  geom_line(aes(group=id))+
  ggthemes::theme_few()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=12),
        legend.position="none")+
  labs(title="Early cropland gain",
       x ="Year", y = "Crop Probability")

p8 <- crop_probs %>% filter(change_type==4) %>% 
  # filter(!id %in% c(crop_probs %>% 
  #                    filter(change_type==4) %>% 
  #                    filter(year==2015&value<0.5) %>% 
  #                    pull(id) %>% unique())) %>% 
  ggplot(.,aes(year,value))+
  #geom_point()+
  # geom_line(aes(y=value, x=year,group=PL_PLOTID,col=initcon),alpha=0.5)+
  geom_boxplot(aes(y=value, group=year),
               outlier.shape = NA,
               #outlier.shape = 1,
               #outlier.size = 0.5,
               alpha=0.6,
               fatten = 3)+
  #geom_jitter(width = 0.2,size=0.1)+
  
  scale_x_continuous(breaks = seq(2000,2015,1), limits = c(1999,2017))+
  scale_y_continuous(breaks = seq(0,1,0.2), limits = c(0,1))+
  #  geom_line(aes(group=id))+
  ggthemes::theme_few()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=12),
        legend.position="none")+
  labs(title="Persistent crop",
       x ="Year", y = "Crop Probability")

png("/media/sitian/TB/BULCU_project/Figures/boxplot_probts_vote_812sample.png",
    width = 960, height = 480)
gridExtra::grid.arrange(grobs=list(p5,p6,p7,p8), 
                        #  top="Main Title",
                        ncol = 2, nrow = 2)

dev.off()
ggsave("/media/sitian/TB/BULCU_project/Figures/boxplot_probts_vote_812sample.png")


save(p5,p6,p7,p8,file="/media/sitian/TB/BULCU_project/Rdata_figrues/figure_prob_vote_812spl_withGFSAD.rda")
load("/media/sitian/TB/BULCU_project/Rdata_figrues/figure_prob_vote_812spl_withGFSAD.rda")


