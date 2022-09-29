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


plts <- lapply(c(7,3), function(i){
  desig_pts <- crop_probs %>% 
    filter(id == i)
  
  shapelet_result <- shapelet(desig_pts$value,3)
  win_s <- 2015+1 -shapelet_result[[1]]
  win_e <- 2015+1 -shapelet_result[[1]]-2
  GAP_max = max(shapelet_result[[2]]) %>% round(3)
  
  win_pnt <- desig_pts %>% filter(year==win_s)
  
  box_up <- desig_pts$value[desig_pts$year==win_s]*1.03
  box_lo <- desig_pts$value[desig_pts$year==win_e]*0.97
  desig_pts$GAP <- c(shapelet_result[[2]],NA,NA) %>% round(3)
  p <- ggplot(desig_pts,aes(year,value))+
    geom_line(size=0.8)+
    geom_point(size=2)+
    geom_rect(aes(xmin = win_e-0.15,
                  xmax = win_s+0.15,
                  ymin = box_lo,
                  ymax = box_up),
              fill = "transparent", color = "blue",
              linetype=2, size = 0.5)+
    labs(title="",
         x ="Year", y = "Crop Probability")+
    # scale_x_reverse(breaks=seq(2015,2000,-1))+
    scale_x_continuous(breaks = seq(2000,2015,1), limits = c(2000,2015))+
    scale_y_continuous(breaks = seq(1,0,-0.2), limits = c(0,1))+
    #geom_smooth(method = "lm", se = FALSE,col='red',size=0.4)+
    ggrepel::geom_label_repel(aes(label = GAP),
                              box.padding   = 0.35, 
                              point.padding = 0.5,
                              alpha = 0.8, 
                              position = position_dodge(3),
                              # hjust=0.5,
                              #vjust=0.1,
                              direction="y",
                              segment.color = 'grey50')+
    annotate("text", x = win_s+3, y = (box_up+box_lo)/2, label = paste("GAP max =",GAP_max))+
    theme_classic()+
    #ggthemes::theme_base()
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=12),
          axis.line = element_line(colour = 'black', size = 0.6),
          axis.ticks = element_line(colour = "black", size = 1),
          panel.background = element_rect(color = NA))+
    geom_point(aes(x=win_pnt$year, y=win_pnt$value), 
               size=3, colour="red",shape=17)
  return(p)
})

ggpubr::ggarrange(plotlist=plts, 
                  labels = c("A", "B"),
                  ncol = 2, nrow = 1)


exp <- ggpubr::ggarrange(plotlist=plts, 
                  labels = c("A", "B"),
                  ncol = 2, nrow = 1)

ggsave("/media/sitian/TB/BULCU_project/Figures/shapelet_comparison.png",
       width = 12, height = 4.7,
       exp)




# shapelet function.
shapelet = function(ts, w){
  l <- length(ts)
  # construct candidate_shapelet list and candidate_nonshapelet list
  candidate_S_list = list()
  candidate_N_list = list()
  i <- 1
  while_counter <- 1
  while (i+(w-1) < (l+1)){
    S_ind = c(i, i+w-1)  # a moving window size =3
    # left non-shapelet segment
    
    if (i > 1) {
      N1_ind = c(1, i-1)
    } else {
      N1_ind = c()
    }
    
    if (i+w < (l+1)) {
      N2_ind = c(i+w, l)
    } else{
      N2_ind = c()}
    
    candidate_S_list[[while_counter]] <- S_ind # like [[s1_start, s1_end], [s2_start, s2_end] ...]
    candidate_N_list[[while_counter]] <- list(N1_ind, N2_ind)  # like [[[N_left1_start, N_left1_end], [N_right1_start, N_right1_end]], [[N_left2_start, N_left2_end], [N_right2_start, N_right2_end]]]
    i <- i+1
    while_counter <- while_counter+1
  }
  
  
  
  
  # calculate GAP
  GAP_list = c()
  
  for (i in 1:length(candidate_S_list)){
    S = ts[candidate_S_list[[i]][[1]]:candidate_S_list[[i]][[2]]] 
    S_sd = sd(S)
    #S_mean = mean(S)
    
    N_sd_list = c()
    # check N1 (N_left) and N2 (N_rigth)
    N_ind <- candidate_N_list[[i]]  
    
    if (!length(N_ind[[1]])==0){
      N_left = ts[N_ind[[1]][[1]]:N_ind[[1]][[2]]]
      if(length(N_left==1)){
        N_sd_list = append(N_sd_list,0) # let one number's sd to be 0
      }else{
        N_left_sd = sd(N_left)
        N_sd_list = append(N_sd_list,N_left_sd)}
    }
    
    if (!length(N_ind[[2]])==0){
      N_right = ts[N_ind[[2]][[1]]:N_ind[[2]][[2]]]
      if(length(N_right)==1){
        N_sd_list = append(N_sd_list,0)
      } else{
        N_right_sd = sd(N_right)
        N_sd_list = append(N_sd_list,N_right_sd)
      }
      
    }
    
    # calculate GAP
    N_mean_sd = mean(N_sd_list) # the mean of two sd
    GAP = S_sd - N_mean_sd
    GAP_list = append(GAP_list, GAP)
  }
 GAP_max_ind = which(GAP_list==max(GAP_list)) # return the time of max GAP
 return(list(GAP_max_ind, GAP_list))
}



