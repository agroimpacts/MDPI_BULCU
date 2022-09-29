library(sf)
library(raster)
library(dplyr)
library(ggplot2)

wd <- '/media/sitian/HDD1/BULCU_TIFFS/'

# randomly sample crop and non-crop from 812 crops (Done, just import them here).
val_type <- "favor-crop"
val_nick <- "procp"

strokcol <- 'purple'
filcol <- "pink"

tags <- c("a)","b)","c)","d)")

val_type <- "vote" #"favor-crop"
val_nick <- "vote" #"procp"

strokcol <- 'blue'
filcol <- "cyan"

tags <- c("e)","f)","g)","h)")
# use favoring crop strategy
pts <- read_sf(paste0(wd,'/Validation_Points/shp/',val_type,'_validation.shp'))

# load GFSAD
gfsad <- raster(paste0(wd,'ZAM_GFSAD_2015/Zam_GFSAD_2015.tif'))
# get points for calibration and visualization purpose
#set.seed(2)
#cpts <- pts %>% sample_n(., size=160)

# persistent noncrop
cpts1 <- pts %>% filter(!!as.name(paste0(val_nick,'2000'))==0&
                        !!as.name(paste0(val_nick,'2010'))==0&
                        !!as.name(paste0(val_nick,'2015'))==0) #%>% sample_n(., size=30)
# late gain
cpts2 <- pts %>% filter(!!as.name(paste0(val_nick,'2000'))==0&
                        !!as.name(paste0(val_nick,'2010'))==0&
                        !!as.name(paste0(val_nick,'2015'))==1) #%>% sample_n(., size=30,replace = T)
# early gain
cpts3 <- pts %>% filter(!!as.name(paste0(val_nick,'2000'))==0&
                        !!as.name(paste0(val_nick,'2010'))==1&
                        !!as.name(paste0(val_nick,'2015'))==1) #%>% sample_n(., size=30,replace = T)
# persistent crop
cpts4 <- pts %>% filter(!!as.name(paste0(val_nick,'2000'))==1&
                        !!as.name(paste0(val_nick,'2010'))==1&
                        !!as.name(paste0(val_nick,'2015'))==1) #%>% sample_n(., size=30)
cpts <- rbind(cpts1, cpts2, cpts3, cpts4)
#
grids <- read_sf(paste0(wd,'grid_50x50/grid_50x50.shp'))

cpts <- cpts %>% st_intersection(grids) %>%
  mutate(grid_id = id) %>%
  dplyr::select(-id)

# we set init crop prob as 0.8 for BULC-U, so an 1 (crop) in
# GFSAD will be reclassed as 0.8
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


#############
typevec <- cpts %>%
  mutate(change_type = as.character(group_indices_(., .dots=c(paste0(val_nick,"2000"),
                                                 paste0(val_nick,"2010"),
                                                 paste0(val_nick,"2015")))),
         PL_PLOTID=as.character(PL_PLOTID)) %>%
  dplyr::select(PL_PLOTID,change_type)

check_type <- cpts %>%
  mutate(change_type = as.character(group_indices_(., .dots=c(paste0(val_nick,"2000"),
                                                              paste0(val_nick,"2010"),
                                                              paste0(val_nick,"2015")))),
         PL_PLOTID=as.character(PL_PLOTID)) %>%
  dplyr::select(paste0(val_nick,"2000"),
                paste0(val_nick,"2010"),
                paste0(val_nick,"2015"),change_type)

crop_probs <- crop_probs %>% inner_join(., typevec,
                          by = c('PL_PLOTID'='PL_PLOTID'))



save(crop_probs,file=paste0("/media/sitian/TB/BULCU_project/Rdata_figrues/prob_"
                            ,val_nick,"_812spl.rda"))
load(paste0("/media/sitian/TB/BULCU_project/Rdata_figrues/prob_"
            ,val_nick,"_812spl.rda"))


# plot.  Let p1 to be persistent crop
p1 <- crop_probs %>% filter(change_type==4) %>%
  # filter(!id %in% c(crop_probs %>%
  #                     filter(change_type==4) %>%
  #                     filter(year==2015&value<0.5) %>%
  #                     pull(id) %>% unique())) %>%
  ggplot(.,aes(year,value))+
  #  geom_point()+
  geom_boxplot(aes(y=value, group=year),
               outlier.shape = NA,
               color=strokcol, fill=filcol)+
  #geom_jitter(width = 0.2,size=0.1)+
  scale_y_continuous(breaks = seq(0,1,0.2), limits = c(0,1))+
  scale_x_continuous(breaks = seq(2000,2015,1), limits = c(1999,2017))+
  #  geom_line(aes(group=id))+
  ggthemes::theme_few()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=12),
        legend.position="none")+
  labs(title="Persistent cropland",
       x ="Year", y = "Crop Probability",tag=tags[1])


# test <- crop_probs %>% filter(change_type==1) %>% filter(year==2016)
# ggplot(test,aes(year,value))+  
#   geom_boxplot(aes(y=value, group=year))

p2 <- crop_probs %>% filter(change_type==1) %>%
  # filter(!id %in% c(crop_probs %>%
  #                     filter(change_type==1) %>%
  #                     filter(year==2015&value>0.5) %>%
  #                     pull(id) %>% unique())) %>%
ggplot(.,aes(year,value))+
  #geom_point()+
  geom_boxplot(aes(y=value, group=year),
               outlier.shape = NA,
               color=strokcol, fill=filcol)+
  #geom_jitter(width = 0.2,size=0.1)+
  scale_y_continuous(breaks = seq(0,1,0.2), limits = c(0,1))+
  scale_x_continuous(breaks = seq(2000,2015,1), limits = c(1999,2017))+
  #  geom_line(aes(group=id))+
  ggthemes::theme_few()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=12),
        legend.position="none")+
  labs(title="Persistent non-cropland",
       x ="Year", y = "Crop Probability",tag=tags[2])


p3 <- crop_probs %>% filter(change_type==3) %>%
  # filter(!id %in% c(crop_probs %>%
  #                     filter(change_type==3) %>%
  #                     filter(year==2015&value<0.5) %>%
  #                     pull(id) %>% unique())) %>%
  ggplot(.,aes(year,value))+
#  geom_point()+
  geom_boxplot(aes(y=value, group=year),
               outlier.shape = NA,
               color=strokcol, fill=filcol)+
  #geom_jitter(width = 0.2,size=0.1)+
  scale_y_continuous(breaks = seq(0,1,0.2), limits = c(0,1))+
  scale_x_continuous(breaks = seq(2000,2015,1), limits = c(1999,2017))+
  ggthemes::theme_few()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=12),
        legend.position="none")+
  #  geom_line(aes(group=id))+
  labs(title="Early cropland gain",
       x ="Year", y = "Crop Probability",tag=tags[3])

p4 <- crop_probs %>% filter(change_type==2) %>%
  # filter(!id %in% c(crop_probs %>%
  #                     filter(change_type==2) %>%
  #                     filter(year==2015&value<0.5) %>%
  #                     pull(id) %>% unique())) %>%
  ggplot(.,aes(year,value))+
  #  geom_point()+
  geom_boxplot(aes(y=value, group=year),
               outlier.shape = NA,
               color=strokcol, fill=filcol)+
  #geom_jitter(width = 0.2,size=0.1)+
  scale_y_continuous(breaks = seq(0,1,0.2), limits = c(0,1))+
  scale_x_continuous(breaks = seq(2000,2015,1), limits = c(1999,2017))+
  #  geom_line(aes(group=id))+
  ggthemes::theme_few()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=12),
        legend.position="none")+
  labs(title="Late cropland gain",
       x ="Year", y = "Crop Probability",tag=tags[4])

png(paste0("/media/sitian/TB/BULCU_project/Figures/boxplot_probts_",
        val_nick,"_812sample.png"),
        width = 1800, height = 900)
gridExtra::grid.arrange(grobs=list(p1,p2,p3, p4),
                        #  top="Main Title",
                        ncol = 2, nrow = 2)
dev.off()
ggsave(paste0("/media/sitian/TB/BULCU_project/Figures/boxplot_probts_",
              val_nick,"_812sample.jpg"),
              width=10,height=5,units=c("in"))
#save(p1,p2,p3,p4,file="/media/sitian/TB/BULCU_project/Rdata_figrues/figure_prob_procp_812spl_withGFSAD.rda")
#load("/media/sitian/TB/BULCU_project/Rdata_figrues/figure_prob_vote_812spl_withGFSAD.rda")

# Alternatively, we can use cowplot to export the 4-grid figrue.
# It seems has better control of the resolution of the figure
cowplot::ggdraw()+
  cowplot::draw_plot(p1,x=0,y=0.5,width=0.5,height=0.5)+
  cowplot::draw_plot(p2,x=0.5,y=0.5,width=0.5,height=0.5)+
  cowplot::draw_plot(p3,x=0,y=0,width=0.5,height=0.5)+
  cowplot::draw_plot(p4,x=0.5,y=0,width=0.5,height=0.5)
ggsave(paste0("/media/sitian/TB/BULCU_project/Figures/boxplot_probts_",
              val_nick,"_812sample-ver4.jpg"),
       width=10,height=5,units=c("in"))
