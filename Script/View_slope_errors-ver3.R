library(dplyr)
library(ggplot2)

wd <- '/media/sitian/HDD1/BULCU_TIFFS/Error_assess/'

val_type_list <- c('favor-crop','vote')
val_nick_list <- c('Cropland wins','Majority agreement')
plst <- lapply(1:2,function(x){
  val_type <- val_type_list[x]
  val_nick <- val_nick_list[x]
  assess_hit <- read.csv(paste0(wd,
                                paste0("cropgain_evals_",
                                       val_type,".csv"))) %>% 
    dplyr::select(hit,thresh,where)
  
  assess_false <- read.csv(paste0(wd,
                                  paste0("cropgain_evals_",
                                         val_type,".csv"))) %>% 
    dplyr::select(false_alarm,thresh,where)
  
  p <- ggplot() +
    geom_line(data = assess_hit, 
              linetype='solid',
              aes(thresh,
                  hit, 
                  color=as.factor(where)))+
    geom_line(data = assess_false, 
              linetype='longdash',
              aes(thresh,
                  false_alarm, 
                  color=as.factor(where)))+
    scale_x_continuous(breaks = seq(0.005,0.03,0.005), limits = c(0.005,0.03),
                       name="Slope threshold")+
    scale_y_continuous(breaks = seq(0,1,0.2), limits = c(0,1),
                       name="Hit",
                       sec.axis = sec_axis(~., name="False Positive Rate",
                                           breaks = seq(0,1,0.2)))+
    ggthemes::theme_few()+
    ggtitle(val_nick)+
    theme(axis.title = element_text(face="bold"),
          legend.position="none")
  
  return(p)
  })

assess_hit <- read.csv(paste0(wd,
                       paste0("cropgain_evals_",
                              val_type,".csv"))) %>% 
  dplyr::select(hit,thresh,where)

assess_false <- read.csv(paste0(wd,
                              paste0("cropgain_evals_",
                                     val_type,".csv"))) %>% 
  dplyr::select(false_alarm,thresh,where)


# This is just for its label
assess_hit <- read.csv(paste0(wd,
                              paste0("cropgain_evals_vote.csv"))) %>% 
  dplyr::select(hit,thresh,where)

assess_false <- read.csv(paste0(wd,
                                paste0("cropgain_evals_vote.csv"))) %>% 
  dplyr::select(false_alarm,thresh,where)
p_legend <- ggplot() +
  geom_line(data = assess_hit, 
            aes(thresh,
                hit, 
                linetype='Proportion crop gain captured',
                color=as.factor(where)))+
  geom_line(data = assess_false, 
            aes(thresh,
                false_alarm, 
                linetype='False positive rate',
                color=as.factor(where)))+
  scale_x_continuous(breaks = seq(0.005,0.03,0.005), limits = c(0.005,0.03),
                     name="slope threshold")+
  scale_y_continuous(breaks = seq(0,1,0.2), limits = c(0,1),
                     sec.axis = sec_axis(~., name="False Positive Rate",
                                         breaks = seq(0,1,0.2)))+
  ggthemes::theme_few()+
  ggtitle('abc')+
  theme(axis.title = element_text(face="bold"))+
  labs(title="",
       color = 'Validation extent',
       x ="Slope Threshold",
       y = "Proportion Crop Gain Captured")+
  labs(linetype="line type")
ggsave(paste0("/media/sitian/TB/BULCU_project/Figures/slope_errors-ver2-legend.jpg"),
       width=12,height=6,units=c("in")) 

  
cowplot::ggdraw()+
  theme(plot.background = element_rect(fill="white", color = NA))+
  cowplot::draw_plot(plst[[1]],x=0,y=0.3,width=0.4,height=0.7)+
  cowplot::draw_plot(plst[[2]],x=0.45,y=0.3,width=0.4,height=0.7)

ggsave(paste0("/media/sitian/TB/BULCU_project/Figures/slope_errors-ver3.jpg"),
       width=12,height=6,units=c("in"))
