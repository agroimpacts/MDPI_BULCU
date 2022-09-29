library(dplyr)
library(ggplot2)

wd <- '/media/sitian/HDD1/BULCU_TIFFS/Error_assess/'

assess_vote <- read.csv(paste0(wd,"cropgain_evals_vote.csv"))
assess_procp <- read.csv(paste0(wd,"cropgain_evals_favor-crop.csv"))

slope_thresh_lst <- c(0.005,0.01,0.015,0.02,0.03)


# data.zso <- crop.area %>%  
#   dplyr::select(ADM1_EN,zso_area,zso_maize_area) %>%  
#   #arrange(desc(zso_area)) %>%  
#   tidyr::gather(variable,value,(zso_area:zso_maize_area)) 

test <- assess_procp %>% rbind(assess_vote) %>% 
  mutate(group_id = group_indices_(., .dots=c("val_type",
                                              "where"))) %>% 
  mutate(grp = ifelse(where=="In Zambia"&val_type=="vote", "In Zambia, majority",
               ifelse(where=="In GFSAD"&val_type=="vote", "In GFSAD, majority",
               ifelse(where=="In Zambia"&val_type=="favor-crop", "In Zambia, at least 1",
               ifelse(where=="In GFSAD"&val_type=="favor-crop", "In GFSAD, at least 1",
                       NA)))))

# # hit dat
dat <- data.frame(
  slope_thresh = slope_thresh_lst,
  in_Zambia = c(0.275,0.406,0.435,0.507,0.696),
  within_GFSAD = c(0.358,0.509,0.547,0.623,0.736)
) %>% reshape2::melt(., id='slope_thresh') %>%
  mutate(typeid = ifelse(variable=="in_Zambia","in Zambia","in GFSAD"))
# 
# # false alarm dat
# dat2 <- data.frame(slope_thresh = slope_thresh_lst,
#                    false_positive_Zambia = c(0.069, 0.108,0.144,0.178,0.376),
#                    false_positive_GFSAD = c(0.13, 0.198,0.269,0.329,0.598)) %>% 
#   reshape2::melt(., id='slope_thresh') %>% 
#   mutate(typeid = ifelse(variable=="false_positive_Zambia","in Zambia","in GFSAD"))

# ggplot() +
#   geom_col(data = assess_vote, 
#            aes(thresh,
#                 hit, 
#                 color=as.factor(where)),
#                 alpha=0.5,
#                 position='dodge',
#                 stat='identity')+
#   geom_col(data = assess_vote, aes(thresh,
#                            false_alarm, 
#                            color=as.factor(where)),
#            position='dodge',
#            stat='identity') +
#   geom_col(data = assess_procp, 
#            aes(thresh,
#                hit, 
#                color=as.factor(where)),
#            alpha=0.5,
#            position='dodge',
#            stat='identity')

ggplot() +
  geom_col(data = test, 
           aes(thresh,
               hit,
               fill=as.factor(grp)),
           alpha=0.5,
           position='dodge',
           stat='identity')+
  geom_col(data = test, 
           aes(thresh,
               false_alarm,
               #color="black",
               fill=as.factor(grp)),
           alpha=0.5,
           position='dodge',
           stat='identity')
  

# ggplot() +
#   geom_col(data = testdat1, aes(slope_thresh,
#                                 in_Zambia),
#            alpha=0.5,
#            position='stack',
#            stat='identity')+
#   geom_col(data = testdat1, aes(slope_thresh,
#                                 false_positive_Zambia),
#            position='stack',
#            stat='identity')


ggsave("/media/sitian/TB/BULCU_project/Figures/slope_errors_ver2.png",dpi=600)
