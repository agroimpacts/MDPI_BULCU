library(dplyr)
library(ggplot2)


slope_thresh_lst <- c(0.005,0.01,0.015,0.02,0.03)

# hit dat
dat <- data.frame(
  slope_thresh = slope_thresh_lst,
  in_Zambia = c(0.275,0.406,0.435,0.507,0.696),
  within_GFSAD = c(0.358,0.509,0.547,0.623,0.736)
) %>% reshape2::melt(., id='slope_thresh') %>% 
  mutate(typeid = ifelse(variable=="in_Zambia","in Zambia","in GFSAD"))

# false alarm dat
dat2 <- data.frame(slope_thresh = slope_thresh_lst,
                   false_positive_Zambia = c(0.069, 0.108,0.144,0.178,0.376),
                   false_positive_GFSAD = c(0.13, 0.198,0.269,0.329,0.598)) %>% 
  reshape2::melt(., id='slope_thresh') %>% 
  mutate(typeid = ifelse(variable=="false_positive_Zambia","in Zambia","in GFSAD"))

ggplot() +
  geom_line(data = dat, aes(slope_thresh,
                value, 
                color=as.factor(typeid),
                linetype='Proportion crop gain captured'))+ 
  geom_line(data = dat2, aes(slope_thresh,
                             value,
                             color = as.factor(typeid),
                             linetype='False positive rate'))+
  scale_y_continuous(breaks = seq(0,1,0.2), limits = c(0,1),
                     sec.axis = sec_axis(~., name="False Positive Rate",
                                         breaks = seq(0,1,0.2)))+
  ggthemes::theme_few()+
  theme(axis.title = element_text(face="bold"))+
  labs(title="",
       color = 'Validation extent',
       x ="Slope Threshold",
       y = "Proportion Crop Gain Captured")

ggsave("/media/sitian/TB/BULCU_project/Figures/slope_errors.png",dpi=600)
