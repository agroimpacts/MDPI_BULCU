#library("ggVennDiagram")
library(eulerr)
library(dplyr)

# set color pallete
color_pal <- c("cornflowerblue", 
  "darkolivegreen2", 
  "firebrick3")


# run Assessment validation consistency to load a_dat, b_dat, c_dat
a_crop2000 <- a_dat %>% filter(`2000a`==100) %>% pull(PL_PLOTID)
b_crop2000 <- b_dat %>% filter(`2000b`==100) %>% pull(PL_PLOTID)
c_crop2000 <- c_dat %>% filter(`2000c`==100) %>% pull(PL_PLOTID)


x2000 <- list(
  a = a_crop2000,
  b = b_crop2000,
  c = c_crop2000
)
#ggVennDiagram(x, label_alpha = 0)

# This has proportion
p2000 <- euler(x2000) %>% plot(., quantities = TRUE, 
                               main='2000', fills = color_pal)



# 2010
a_crop2010 <- a_dat %>% filter(`2010a`==100) %>% pull(PL_PLOTID)
b_crop2010 <- b_dat %>% filter(`2010b`==100) %>% pull(PL_PLOTID)
c_crop2010 <- c_dat %>% filter(`2010c`==100) %>% pull(PL_PLOTID)


x2010 <- list(
  a = a_crop2010,
  b = b_crop2010,
  c = c_crop2010
)
#ggVennDiagram(x, label_alpha = 0)

# This has proportion
p2010 <- euler(x2010) %>% plot(., quantities = TRUE, 
                               main='2010', fills =color_pal)


# 2015
a_crop2015 <- a_dat %>% filter(`2015a`==100) %>% pull(PL_PLOTID)
b_crop2015 <- b_dat %>% filter(`2015b`==100) %>% pull(PL_PLOTID)
c_crop2015 <- c_dat %>% filter(`2015c`==100) %>% pull(PL_PLOTID)


x2015 <- list(
  a = a_crop2015,
  b = b_crop2015,
  c = c_crop2015
)
#ggVennDiagram(x, label_alpha = 0)

# This has proportion
p2015 <- euler(x2015) %>% plot(., quantities = T, 
                               main='2015', fills = color_pal)



gridExtra::grid.arrange(grobs=list(p2000,p2010,p2015), 
                        #  top="Main Title",
                        common.legend = TRUE,
                        ncol = 3, nrow = 1)


##############################################################
# for non crop consistency------------------------------------
#-------------------------------------------------------------
#-------------------------------------------------------------
##############################################################
a_ncp2000 <- a_dat %>% filter(`2000a`==0) %>% pull(PL_PLOTID)
b_ncp2000 <- b_dat %>% filter(`2000b`==0) %>% pull(PL_PLOTID)
c_ncp2000 <- c_dat %>% filter(`2000c`==0) %>% pull(PL_PLOTID)


y2000 <- list(
  a_ncp = a_ncp2000,
  b_ncp = b_ncp2000,
  c_ncp = c_ncp2000
)
#ggVennDiagram(x, label_alpha = 0)
np2000 <- euler(y2000) %>% plot(., quantities = TRUE, 
                                main='2000',
                                fills = color_pal)
np2000

# 2010
a_ncp2010 <- a_dat %>% filter(`2010a`==0) %>% pull(PL_PLOTID)
b_ncp2010 <- b_dat %>% filter(`2010b`==0) %>% pull(PL_PLOTID)
c_ncp2010 <- c_dat %>% filter(`2010c`==0) %>% pull(PL_PLOTID)


y2010 <- list(
  a_ncp = a_ncp2010,
  b_ncp = b_ncp2010,
  c_ncp = c_ncp2010
)
#ggVennDiagram(x, label_alpha = 0)

# This has proportion
np2010 <- euler(y2010) %>% plot(., quantities = TRUE, 
                               main='2010', fills=color_pal)


# 2015
a_ncp2015 <- a_dat %>% filter(`2015a`==0) %>% pull(PL_PLOTID)
b_ncp2015 <- b_dat %>% filter(`2015b`==0) %>% pull(PL_PLOTID)
c_ncp2015 <- c_dat %>% filter(`2015c`==0) %>% pull(PL_PLOTID)


y2015 <- list(
  a_ncp = a_ncp2015,
  b_ncp = b_ncp2015,
  c_ncp = c_ncp2015
)
#ggVennDiagram(x, label_alpha = 0)

# This has proportion
np2015 <- euler(y2015) %>% plot(., quantities = T, 
                               main='2015',fills=color_pal)



gridExtra::grid.arrange(grobs=list(np2000,np2010,np2015), 
                        #  top="Main Title",
                        common.legend = TRUE,
                        ncol = 3, nrow = 1)




#################################################
# combining crop and non-crop consistency figures
gridExtra::grid.arrange(grobs=list(p2000,p2010,p2015,
                                   np2000,np2010,np2015), 
                        #  top="Main Title",
                        common.legend = TRUE,
                        ncol = 3, nrow = 2)
