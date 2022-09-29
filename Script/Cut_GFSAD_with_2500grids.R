library(raster)
library(sf)

# load GFSAD 2015
gfsad <- raster("/media/sitian/HDD1/GFSAD/Zam_GFSAD_2015.tif")

# load mass bound
grids <- read_sf('/media/sitian/HDD1/BULCU_TIFFS/grid_50x50/grid_50x50.shp')

# project mass's crs to lc's crs (Might not needed because they seems are in the same crs)
grids_prj <- grids %>% st_transform(crs(gfsad))

# plot see if bound and lc overlapped
plot(gfsad)
plot(grids, add=T)
plot(grids_prj, add=T)

# loop; for each mass county, use this county to cut lc and write the cutted 
# raster to disk, using the corresponding town's name
lapply(1:nrow(grids_prj), function(x){
  r_name <- paste0('grid',grids_prj[[x,'id']])
  r <- raster::crop(gfsad,grids_prj[x,])
  
  # if you want to only keep the pixels within the bound, then add this:
  r <- r %>% mask(., grids_prj[x,]) # you can specify a new value for NA pixels in mask()
  
  writeRaster(r, paste0("/media/sitian/HDD1/GFSAD/Grid_GFSAD2015/",r_name,".tif"))
  
})


# version 2: reproject/resample GFSAD to the corresponding BULCU tif
# so that GFSAD and BULC-U grid tif will have same dimension 

# get ids in bulcu (fewer than grid id)
bulcu_list = list(list.files("/media/sitian/HDD1/BULCU_TIFFS/BULCU_grid2500_2016backward/BULCU-all_2016_backward/",
                             pattern = '.tif'))
bulcu_list = sapply(bulcu_list, function(x){
  stringr::str_match(x,"grid_(\\w+?)_BULCU")[,2]
})

lapply(bulcu_list,function(x){
  cat(x,'\n')
  gfsad_r <- raster(paste0("/media/sitian/HDD1/GFSAD/Grid_GFSAD2015/grid",x,".tif"))
  bulcu <- raster(paste0("/media/sitian/HDD1/BULCU_TIFFS/BULCU_grid2500_2016backward/BULCU-all_2016_backward/grid_",
                         x,"_BULCU.tif"))
  # creat an empty raster with col and rows equal to bulcu
  r <- raster(nrow=nrow(bulcu), ncol=ncol(bulcu), 
              xmn = extent(bulcu)[1],
              xmx = extent(bulcu)[2], 
              ymn = extent(bulcu)[3], 
              ymx = extent(bulcu)[4])
  # file in some values for this raster.
  r[] <- 1:ncell(r)
  
  # use the raster r as template, to resample the modis_lc
  gfsad_resampled <- resample(gfsad_r, r, method='ngb')
  writeRaster(gfsad_resampled, 
              filename=paste0('/media/sitian/HDD1/GFSAD/Grid_GFSAD2015_resampled/grid', 
                                 x,".tif"), 
              format="GTiff", overwrite=TRUE)
})
