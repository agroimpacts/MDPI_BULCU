library(sf)
library(raster)
library(dplyr)
library(ggplot2)


# load validation points and divide into early and late crop gain.


wd <- '/media/sitian/HDD1/BULCU_TIFFS/'

# randomly sample crop and non-crop from 812 crops.
# choose validation points set

val_type <- "favor-crop"
val_nick <- "procp"

val_type <- "vote" #"favor-crop"
val_nick <- "vote" #"procp"

pts <- read_sf(paste0(wd,paste0('/Validation_Points/shp/',val_type,'_validation.shp')))


pts_lat <- pts %>% filter(!!as.name(paste0(val_nick,"2000"))==0&
                          !!as.name(paste0(val_nick,"2010"))==0&
                          !!as.name(paste0(val_nick,"2015"))==1)
pts_erl <- pts %>% filter(!!as.name(paste0(val_nick,"2000"))==0&
                            !!as.name(paste0(val_nick,"2010"))==1&
                            !!as.name(paste0(val_nick,"2015"))==1)


# use validation points to extract shapelet estimated cropgain year.
gain_yr <- raster(paste0(wd,'BULCU_grid2500_2016backward/shapelet_analysis/shapelet_Mosaic0005.tif'))
#gain_yr <- raster(paste0(wd,'BULCU_grid2500_2016backward/shapelet_analysis/shapelet_Mosaic001.tif'))
#gain_yr <- raster(paste0(wd,'BULCU_grid2500_2016backward/shapelet_analysis/shapelet_Mosaic002.tif'))
#gain_yr <- raster(paste0(wd,'BULCU_grid2500_2016backward/shapelet_analysis/shapelet_Mosaic003.tif'))


lat_yrs <- extract(gain_yr, pts_lat)
lat_yrs <- lat_yrs[lat_yrs<20&lat_yrs>-1]
lat_yrs <- 2015- lat_yrs


erl_yrs <- extract(gain_yr, pts_erl)
erl_yrs <- erl_yrs[erl_yrs<20&erl_yrs>-1]
erl_yrs <- 2015 - erl_yrs


# If overlay erl and late hist
# par(mfrow=c(1,2))
# hist(lat_yrs,xlim=c(2000,2015),ylim=c(0, 10),col=rgb(0,0,1,1),
#      xlab = 'Estimated Crop expansion year',
#      main=paste0('(',val_type,')', 'Histogram of estimated crop expansion year'))
# hist(erl_yrs,col=rgb(0.8,0,0,0.4),add=T)
# legend("topright",c('labeled late crop expansion','labeled early expansion'),
#        fill=c("blue", "red"))
# 
# png('/media/sitian/TB/BULCU_project/Figures/Assess_early_late_cropgain')
# png('/media/sitian/TB/BULCU_project/Figures/TEST--Assess_early_late_cropgain_ver2',
#     width     = 14,
#     height    = 5.25,
#     units     = "in",
#     res       = 1200)
# dev.off()


# If keep erl and late hist in two rows
dev.off()
png(paste0("/media/sitian/TB/BULCU_project/Figures/",
        "early_late_estimation-legend-ver2.png"),
    width = 9, height = 6,
    units="in",
    res=300)

par(mfrow=c(2,2),mar=c(5,5,3,0))

# early
hist(erl_yrs,col=rgb(0.8,0,0,1),xlim=c(2000,2015),ylim=c(0, 30),
     axes=F,
     breaks = seq(2000,2015,1),
     xlab = 'Estimated Crop expansion year',
#     main="Cropland wins"
     main="Majority agreement"
     )
legend("topright",c('labeled early expansion','labeled late crop expansion'),
       fill=c("blue", "red"))

legend("topright",c('labeled early expansion (yearly)',
                    'labeled late expansion (yearly)',
                    'labeled early expansion (5-year bin)',
                    'labeled late expansion (5-year bin)',
                    'cropland'),
       fill=c("blue", "red","slateblue2","plum1"))


axis(1,at=seq(2000,2015,1), las=2)
axis(2,at=seq(0,30,5), las=2)
hist(erl_yrs[erl_yrs<=2010],col=rgb(0.8,0,0,0.2),xlim=c(2000,2015),ylim=c(0, 500),
     axes=F,
     breaks = c(2000,2005,2010),
     xlab = 'Estimated Crop expansion year',
     main=NA,add=T)
hist(erl_yrs[erl_yrs>2010],col=rgb(0.8,0,0,0.2),xlim=c(2000,2015),ylim=c(0, 500),
     axes=F,
     breaks = c(2010,2015),
     xlab = 'Estimated Crop expansion year',
     main=NA,add=T)

#late
hist(lat_yrs,xlim=c(2000,2015),ylim=c(0, 10),col=rgb(0,0,1,1),
     axes=F,
     breaks = seq(2000,2015,1),
     main=NA,
     #xlab = 'Estimated Crop expansion year',
     )
axis(1,at=seq(2000,2015,1), las=2)
axis(2,at=seq(0,10,2), las=2)
hist(lat_yrs[lat_yrs<=2010],xlim=c(2000,2015),ylim=c(0, 10),col=rgb(0,0,1,0.2),
     axes=F,
     breaks = c(2000,2005,2010),add=T,
     xlab = 'Estimated Crop expansion year'
)
hist(lat_yrs[lat_yrs>2010],xlim=c(2000,2015),ylim=c(0, 10),col=rgb(0,0,1,0.2),
     axes=F,
     breaks = c(2010,2015),add=T,
     xlab = 'Estimated Crop expansion year'
)


