library(raster)
library(sf)
library(dplyr)
library(ggplot2)
library(gridExtra)

wd <- '/media/sitian/HDD1/BULCU_TIFFS/'
# load sample points
pts <- read_sf(paste0(wd,'example_probability/example_points.shp'))

# intersect points with grid to find out
#   what grids contain the example (otherwise has to mosiac BULCU-images)
grids <- read_sf(paste0(wd,'grid_50x50/grid_50x50.shp'))

pts <- pts %>% st_intersection(grids) %>% 
  mutate(grid_id = id.1,
         id = as.character(id)) %>% 
  dplyr::select(-id.1)

# extract probability timeseries
probs <- sapply(1:nrow(pts), function(x){
  print(x)
  grid_id <- pts[[x,'grid_id']]
  r <- raster::brick(paste0(wd,
                       'BULCU_grid2500_2016backward/BULCU-all_2016_backward/',
                       'grid_',
                       grid_id,
                       '_BULCU.tif'))
  extract(r, pts[x,])/65532
}) 

crop_probs <- probs %>% 
  as.data.frame() %>% 
  filter(row_number() %% 2 == 0) #get every other val (ignor non-crop, get crop)

names(crop_probs) <-pts$id
crop_probs$year <- seq(2015,2000)
crop_probs <- reshape2::melt(crop_probs, id='year')
crop_probs <- crop_probs %>% 
  mutate(id = as.character(variable)) %>% 
  dplyr::select(-variable)
  

# add some lc type info
crop_probs <- crop_probs %>% inner_join(., pts,by=c('id'='id'))

# plot some probability timeseries
cp_gains_erl_ids <- crop_probs %>% 
  filter(lc_type=='cpgain_erl') %>% 
  dplyr::pull(id) %>% unique() 
cpgain_erl_plts <- lapply(cp_gains_erl_ids, function(site){
  cp_gains <- crop_probs %>% 
    filter(id == site)
  
  p <- ggplot(cp_gains,aes(year,value))+
    geom_point()+
    geom_line()+
    labs(title=site,
         x ="Year", y = "Crop Probability")+
    scale_x_reverse(breaks=seq(2015,2000,-1))+
    scale_y_continuous(breaks = seq(1,0,-0.2),limits = c(0,1))+
    ggthemes::theme_clean()+
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
})
# plot early crop gain
gridExtra::grid.arrange(grobs=cpgain_erl_plts, 
          #labels = c("2", "1", "10","7","3"),
        #  top="Main Title",
          ncol = 2, nrow = 3)

# late crop gain---------------------------
cp_gains_lat_ids <- crop_probs %>% 
  filter(lc_type=='cpgain_lat') %>% 
  dplyr::pull(id) %>% unique()
cpgain_lat_plts <- lapply(cp_gains_lat_ids, function(site){
  cp_gains <- crop_probs %>% 
    filter(id == site)
  
  p <- ggplot(cp_gains,aes(year,value))+
    geom_point()+
    geom_line()+
    labs(title="Plot of crop probabilities",
         x ="Year", y = "Crop Probability")+
    scale_x_reverse(breaks=seq(2015,2000,-1))+
    scale_y_continuous(breaks = seq(1,0,-0.2),limits = c(0,1))+
    ggthemes::theme_clean()+
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
})
# plot late crop gain
gridExtra::grid.arrange(grobs=cpgain_lat_plts, 
                        #  top="Main Title",
                        ncol = 2, nrow = 3)



# persistent crop
cp_alltime_ids <- crop_probs %>% 
  filter(lc_type=='cp_alltime') %>% 
  dplyr::pull(id) %>% unique()
cp_alltime_plts <- lapply(cp_alltime_ids, function(site){
  cp_persist <- crop_probs %>% 
    filter(id == site)
  
  p <- ggplot(cp_persist,aes(year,value))+
    geom_point()+
    geom_line()+
    labs(title="Plot of crop probabilities",
         x ="Year", y = "Crop Probability")+
    scale_x_reverse(breaks=seq(2015,2000,-1))+
    scale_y_continuous(breaks = seq(1,0,-0.2), limits = c(0,1))+
    ggthemes::theme_clean()+
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
})
# plot persistent crop 
gridExtra::grid.arrange(grobs=cp_alltime_plts, 
                        #  top="Main Title",
                        ncol = 2, nrow = 3)

# persistent non-crop
# persistent crop
noncp_ids <- crop_probs %>% 
  filter(lc_type=='noncp') %>% 
  dplyr::pull(id) %>% unique()
noncp_plts <- lapply(noncp_ids, function(site){
  noncp_persist <- crop_probs %>% 
    filter(id == site)
  
  p <- ggplot(noncp_persist,aes(year,value))+
    geom_point()+
    geom_line()+
    labs(title="Plot of crop probabilities",
         x ="Year", y = "Crop Probability")+
    scale_x_reverse(breaks=seq(2015,2000,-1))+
    scale_y_continuous(breaks = seq(1,0,-0.2), limits = c(0,1))+
    ggthemes::theme_clean()+
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
})
# plot persistent crop 
gridExtra::grid.arrange(grobs=noncp_plts, 
                        #  top="Main Title",
                        ncol = 2, nrow = 3)


test <- crop_probs %>% 
  mutate(slope = round(lm(value ~ year)$coefficients[2], 2),
         xlabel = ifelse(lc_type == "cpgain_erl"|lc_type == "cpgain_med"|lc_type == "cpgain_lat", "Cropland gain",
                         ifelse(lc_type == "cplost","Cropland lost",
                                ifelse(lc_type == "cp_alltime","Persistent cropland",
                                       ifelse(lc_type == "noncp","Persistent non-cropland",
                                              "Toggling cropland")))))


# plot a designated point-----------------------------
desig_ids <- c(7, 3 ,17,6, 18,19)
desig_plts <- lapply(desig_ids, function(site){
  desig_pts <- crop_probs %>% 
    filter(id == site) %>% 
    mutate(slope = round(lm(value ~ year)$coefficients[2], 2),
           xlabel = ifelse(lc_type == "cpgain_erl"|lc_type == "cpgain_med"|lc_type == "cpgain_lat", "Cropland gain",
                           ifelse(lc_type == "cplost","Cropland lost",
                                  ifelse(lc_type == "cp_alltime","Persistent cropland",
                                         ifelse(lc_type == "noncp","Persistent non-cropland",
                                                "unknown"))))) %>% 
    mutate(xlabel = ifelse(id == 19, "Shifting cropland", xlabel))
  xlabel = desig_pts$xlabel
  slope <- desig_pts$slope[[1]]
  p <- ggplot(desig_pts,aes(year,value))+
    geom_point(size=2)+
    geom_line(size=1)+
    labs(title="",
         x =xlabel, y = "Crop Probability")+
   # scale_x_reverse(breaks=seq(2015,2000,-1))+
    scale_x_continuous(breaks = seq(2000,2015,1), limits = c(2000,2015))+
    scale_y_continuous(breaks = seq(1,0,-0.2), limits = c(0,1))+
    geom_smooth(method = "lm", se = FALSE,col='red',size=0.4)+
    # geom_text(
    #   data = desig_pts, aes(x = year, y = value, label = slope))+
    annotate("text", x = 2012, y = 0.5, label = paste("slope=",slope))+
    theme_classic()+
    #ggthemes::theme_clean()+
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.x=element_text(size=15,face="bold"),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
})




# png("/media/sitian/TB/BULCU_project/Figures/probts_examples.png",
#     width = 900, height = 400
#     )

pexp <- ggpubr::ggarrange(plotlist=desig_plts, 
                  labels = c("a","b","c","d","e","f"),
                  ncol = 3, nrow = 2)
ggsave("/media/sitian/TB/BULCU_project/Figures/probts_examples.png",pexp)
  

# gridExtra::grid.arrange(grobs=desig_plts, 
#                         #  top="Main Title",
#                         ncol = 2, nrow = 3)
dev.off()
#ggsave("/media/sitian/TB/BULCU_project/Figures/boxplot_probts_812sample.png")






desig_pts <- crop_probs %>% 
  filter(id == 7)

p <- ggplot(desig_pts,aes(year,value))+
  geom_point()+
  geom_line()+
  labs(title=7,
       x ="Year", y = "Crop Probability")+
  # scale_x_reverse(breaks=seq(2015,2000,-1))+
  scale_x_continuous(breaks = seq(2000,2015,1), limits = c(2000,2015))+
  scale_y_continuous(breaks = seq(1,0,-0.2), limits = c(0,1))+
  geom_smooth(method = "lm", se = FALSE,col='red',size=0.4)+
  theme_classic()+
  #ggthemes::theme_base()
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.background = element_rect(color = NA))+
  geom_rect(aes(xmin = 2007,
                xmax = 2009,
                ymin = 0,
                ymax = 1),
            fill = "transparent", color = "blue", size = 0.2)

ggpubr::ggarrange(plotlist=plot_list, 
                  labels = c("A", "B", "C"),
                  ncol = 2, nrow = 2)

# png("/media/sitian/TB/BULCU_project/Figures/remvoe_boarder_test.png",
#     width = 300, height = 200)
# 
# plot(p,frame.plot=0, box = FALSE)
# dev.off()
