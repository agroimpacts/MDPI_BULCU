library(raster)
library(sf)
library(dplyr)
library(ggplot2)
library(gridExtra)

wd <- '/media/sitian/HDD1/BULCU_TIFFS/'

# for crop lost, we use validation points, otherwise it may hard to find
# examples.
# load sample points
pts <- read_sf(paste0(wd,'Validation_Points/shp/favor-crop_validation.shp')) %>% 
  subset(procp2000==1&procp2015==0)


grids <- read_sf(paste0(wd,'grid_50x50/grid_50x50.shp'))

pts <- pts %>% st_intersection(grids)

probs <- sapply(1:nrow(pts), function(x){
  print(x)
  grid_id <- pts[[x,'id']]
  r <- raster::brick(paste0(wd,
                            'BULCU_grid2500_2016backward/BULCU-all_2016_backward/',
                            'grid_',
                            grid_id,
                            '_BULCU.tif'))
  extract(r, pts[x,])/65532
}) 

lost_probs <- probs %>% 
  as.data.frame() %>% 
  filter(row_number() %% 2 == 0)

names(lost_probs) <-pts$id
lost_probs$year <- seq(2015,2000)
lost_probs <- reshape2::melt(lost_probs, id='year')
lost_probs <- lost_probs %>% 
  mutate(id = as.character(variable)) %>% 
  dplyr::select(-variable)




plts <- lapply(unique(lost_probs$id), function(x){
  dat <- lost_probs %>% filter(id==x)
  p <- ggplot(dat,aes(year,value))+
    geom_point()+
    geom_line()+
    labs(title=x,
         x ="Year", y = "Crop Probability")+
    scale_x_reverse(breaks=seq(2015,2000,-1))+
    scale_y_continuous(breaks = seq(1,0,-0.2),limits = c(0,1))+
    ggthemes::theme_clean()+
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
})
  
  
gridExtra::grid.arrange(grobs=plts[1:6], 
                        #  top="Main Title",
                        ncol = 2, nrow = 3)

gridExtra::grid.arrange(grobs=plts[7:12], 
                        #  top="Main Title",
                        ncol = 2, nrow = 3)
gridExtra::grid.arrange(grobs=plts[13:18], 
                        #  top="Main Title",
                        ncol = 2, nrow = 3)
gridExtra::grid.arrange(grobs=plts[19:24], 
                        #  top="Main Title",
                        ncol = 2, nrow = 3)
gridExtra::grid.arrange(grobs=plts[24:25], 
                        #  top="Main Title",
                        ncol = 2, nrow = 3)
