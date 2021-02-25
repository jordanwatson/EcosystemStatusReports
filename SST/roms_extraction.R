#-----------------------------------------------------------------------------------------
# This script will extract bottom temperature data from the GOA ROMS netcdf file
# It will extract daily data for the annual temporal periods (seasons) that are critical for each species and life stage.
# For those date periods, a point-in-polygon operation will extract the temperatures for only those gridded locations
# that fall within the designated EFH polygons. 
# Finally, for each year, the annual bottom temperatures within the EFH are averaged to yield a single temperature value
# for each year, species, life stage combination.
# Author: Jordan Watson jordan.watson@noaa.gov
# Date created: 02/18/2021
#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
# Chunk 1 - Load packages and function dependencies
#-----------------------------------------------------------------------------------------
library(tidyverse)
library(sf)
library(tidync)
library(lubridate)
library(parallel)
#library(devtools)
#install_github('nathanvan/parallelsugar')
library(parallelsugar)

#  Parallel version of the point in polygon operation.
#  For use on Windows, this modified version of MCApply is from the parallelsugar package
source("parallel_st_join_function.R")
#-----------------------------------------------------------------------------------------
# End Chunk 1
#-----------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------
# Chunk 2 - Match the lat-lon grid (points) from the ROMS data with the species-stage-EFH polygons.
#-----------------------------------------------------------------------------------------

#  Read in but do not unzip the ROMS data. This is the value of tidync - we can look at the massive file 
#  without unpackaging it (i.e., very fast)
data <- tidync("Data/btemp_2000-2018.nc")

#  Examine the structure of the data - by default, the active layer is the bottom temp
data

#  Extract the lat-long grid for one day, which we'll use as a lookup table for point-in-polygon operations.
#  Format longitude to match the shapefile.
sstpt <- data %>% 
  hyper_filter(tda=tda==tda[1]) %>% # Filter only the first date (tda) of the .nc file.
  hyper_tibble() %>% # Convert to a tibble (i.e., unpackage the beast)
  dplyr::select(LONGITUDE,LATITUDE) %>% # Select only the lat-lon
  rename_all(tolower) %>% 
  mutate(longitude=longitude-360, # ROMS data has positive longitudes but the shapefile has negative. Match to the shapefile.
         longitude2=longitude, # This looks weird but we are going to end up modifying the latitude and longitude fields so I'm copying them for later.
         latitude2=latitude)

#  Load the parallel processing point-in-polygon function. The ROMS data have a coarser grid than the SST data so the parallelization isn't really necessary.
#  Nonetheless I use the same process as I did for the SST data. I did this on my laptop with only three cores.
mymatchfun <- function(myshape,cores){
  #  Set a crs of WGS84 - the point data must be projected.
  points_sf = st_as_sf(sstpt %>% dplyr::select(longitude,latitude,longitude2,latitude2), coords = c("longitude", "latitude"), crs = 4326, agr = "constant") 
  
  #  load the polygon layer
  efh_shp <- st_read(myshape)
  
  #  Let's see what its projection is.
  print(st_crs(efh_shp)$proj4string)
  
  #  Transform the points projection to match the polygon projection
  efh_pts <- points_sf %>% st_transform(st_crs(efh_shp)$proj4string)
  
  #  Match the points to the EFH polygon. This might take a while.
  #  Remove those points that did not fall within a polygon (i.e., OBJECTID == NA)
  return(st_parallel(efh_pts, st_join, cores, y = efh_shp) %>% 
           filter(!is.na(OBJECTID)))
}

#  For bottom temperatures, we are interested only in the subadult and adult stages.
#  Note that I am running this on 3 cores (yawn).
#  I run each of the below separately to allow for troubleshooting (but yeah, kinda ghetto).
stages <- c("earlyjuv","subadult","adult")
lapply(stages,function(x) mymatchfun(paste0("Data/ATF_Shapes/atf_",x,"_efhcore.shp"),3) %>% 
         mutate(stage=x)) %>% 
  bind_rows() %>% 
  saveRDS("Data/ATF_EFH_coord_BottomTemp.RDS")

lapply(stages,function(x) mymatchfun(paste0("Data/Pcod_Shapes/pcod_",x,"_efhcore.shp"),3) %>% 
         mutate(stage=x)) %>% 
  bind_rows() %>% 
  saveRDS("Data/PCOD_EFH_coord_BottomTemp.RDS")

lapply(stages,function(x) mymatchfun(paste0("Data/Pollock_Shapes/poll_",x,"_efhcore.shp"),3) %>% 
         mutate(stage=x)) %>% 
  bind_rows() %>% 
  saveRDS("Data/POLLOCK_EFH_coord_BottomTemp.RDS")

lapply(stages,function(x) mymatchfun(paste0("Data/POP_Shapes/pop_",x,"_efhcore.shp"),3) %>% 
         mutate(stage=x)) %>% 
  bind_rows() %>% 
  saveRDS("Data/POP_EFH_coord_BottomTemp.RDS")

lapply(stages,function(x) mymatchfun(paste0("Data/Sablefish_Shapes/sab_",x,"_efhcore.shp"),3) %>% 
         mutate(stage=x)) %>% 
  bind_rows() %>% 
  saveRDS("Data/SABLEFISH_EFH_coord_BottomTemp.RDS")
#-----------------------------------------------------------------------------------------
# End Chunk 2
#-----------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------
# Chunk 3 - For each set of EFH lat-lon locations, extract the daily bottom temperature values and 
# average those annually for the critical temporal periods (seasons).
#-----------------------------------------------------------------------------------------

#  Determine what the netcdf origin date is for our bottom temperature data.
#  I stole this clever bit from a tidync vignette. Note that "tda" is the name of our date-time field from the .nc file.
tunit <- ncmeta::nc_atts("Data/btemp_2000-2018.nc", "tda") %>% tidyr::unnest(cols = c(value)) %>% dplyr::filter(name == "units")
print(tunit) # 1901-01-15 (this is the origin)
time_ex <- tidync("Data/btemp_2000-2018.nc") %>% activate("D3") %>% hyper_array()
range(time_ex$tda) # What is the date range of our dataset (these are in number days since our origin date)
mintime <- min(time_ex$tda) # For use later we store the first and last date
maxtime <- max(time_ex$tda)

#  Create a data.frame that we'll use to loop through each species and life stage and extract just the dates of interest.
#  This data frame includes dates formatted for the .nc and dates formatted for our filtering.
mydates <- data.frame(date=as_date(mintime:maxtime,origin="1901-01-15")) %>% 
  mutate(month=month(date),
         tda=mintime:maxtime)

#  Create a data.frame of the months of interest for each species and life history.
#  If reviewers want different critical periods for a species / stage, the minmo and maxmo are what would need to change here.
datefilter <- data.frame(spp=c(rep("SABLEFISH",3),rep("POLLOCK",3),rep("PCOD",3),rep("POP",3),rep("ATF",3)),
           lifestage=rep(c("earlyjuv","subadult","adult"),5),
           minmo=c(7,4,1,6,5,2,7,5,2,9,1,10,5,4,1),
           maxmo=c(10,11,3,10,12,4,10,12,4,12,9,12,10,12,3))

#  Create empty data frame for storing output.
tempdat <- data.frame()

#  Determine the set of species-stage-dates for each year to determine which data to extract from the ROMS .nc
#  Clearly there is a tidy way to do this but I just couldn't quite figure it out. I'm disappointed in myself.
for(i in 1:nrow(datefilter)){
spp_stage <- datefilter[i,] # Iterate through species stage and seasons

#  Subset the dates based on the months of interest for that species and stage
tempdat <- mydates %>% 
    filter(month>=spp_stage$minmo & month<=spp_stage$maxmo) %>% 
    bind_cols(spp_stage[,1:2]) %>% 
  bind_rows(tempdat) %>% 
  mutate(year=year(date))
}

#  Identify the full set of species and life stages.
species <- c("ATF","PCOD","POLLOCK","POP","SABLEFISH")
stage <- c("earlyjuv","subadult","adult")

#  Finally, extract the annual ROMS data for each set of days critical to each species-stage, retain only those points that fall within the EFH
#  polygons, and average each year's data.
#  I did this as a loop in case certain time periods yielded too much data and crashed things. The loop provided more flexibility.
#  On my janky laptop it takes about five minutes to run 4 species, two life stages, and all temporal periods.
myout <- data.frame()
for(myspp in 1:length(species)){
  for(mystage in 1:length(stage)){
    
    ncdat <- readRDS(paste0("Data/",species[myspp],"_EFH_coord_BottomTemp.RDS")) %>% # Load the set of EFH lat-lon points for each species-stage
      data.frame %>% 
      dplyr::select(LONGITUDE=longitude2,LATITUDE=latitude2,lifestage=stage) %>% 
      filter(lifestage==stage[mystage]) %>% 
      mutate(LONGITUDE=LONGITUDE+360)
    
    for(myyear in unique(tempdat$year)){
      print(myyear) # Print progress for troubleshooting.
      print(myspp)
      print(mystage)
      yeardat <- (tempdat %>% filter(year==myyear & spp==species[myspp] & lifestage==stage[mystage]))$tda #Extract the integer of relevant dates each year.
      
      myout <- data %>% # data is the netcdf file we defined previously.
        hyper_filter(tda=tda%in%yeardat) %>% # this filters only the relevant dates
        hyper_tibble() %>% # this is quite a large object as it is the full lat-lon grid from the ROMS data for each date selected.
        inner_join(ncdat) %>% # inner join with the EFH lat-lon locations.
        summarise(btemp=mean(btemp_latlon)) %>% # average the bottom temperatures for the full set of dates within each year.
        mutate(year=myyear, #  append the species-stage-year details.
               spp=species[myspp],
               lifestage=stage[mystage]) %>% 
        bind_rows(myout) # Combine with previous years
    }
  }
}

#  Save the output. 
saveRDS(myout,file="ROMS_BT.RDS")

#  Plot the data to make sure things look okay (i.e., is each species-stage plot different?)
readRDS("ROMS_BT.RDS") %>% 
  ggplot(aes(year,btemp,linetype=lifestage)) + 
  geom_line() + 
  facet_wrap(~spp,scales="free_y")

#-----------------------------------------------------------------------------------------
# End Chunk 3
#-----------------------------------------------------------------------------------------



data <- tidync("Data/btemp_2000-2018.nc")
bathy <- tidync("Data/cgoa_bathymetry.nc") %>% 
  hyper_tibble() %>% 
  filter(altitude>(-1000)) %>% 
  mutate(LONGITUDE=LONGITUDE-360)

bathy %>% 
  filter(altitude==0) %>% 
  ggplot(aes(LONGITUDE,LATITUDE,color=altitude)) + 
  geom_point()


lapply(mintime:maxtime,function(x) data %>% 
  hyper_filter(tda=tda==x) %>% # Filter only the first date (tda) of the .nc file.
    hyper_tibble() %>% # Convert to a tibble (i.e., unpackage the beast)
  inner_join(bathy) %>% 
    group_by(tda) %>% 
    summarise(btemp=mean(btemp_latlon))) %>% 
  bind_rows() %>% 
  inner_join(mydates %>% dplyr::select(-month)) %>% 
  saveRDS("ROMS_BT_GOA_1000m_TOTAL_DAILY.RDS")

readRDS("ROMS_BT_GOA_1000m_TOTAL_DAILY.RDS") %>% 
  mutate(year=year(date)) %>% 
  group_by(year) %>% 
  summarise(meanbtemp=mean(btemp)) %>% 
  saveRDS("ROMS_BT_GOA_1000m_TOTAL_ANNUAL.RDS") 
  
  ggplot(aes(year,meanbtemp)) + 
  geom_line()

  dplyr::select(LONGITUDE,LATITUDE) %>% # Select only the lat-lon
  rename_all(tolower) %>% 
  mutate(longitude=longitude-360, # ROMS data has positive longitudes but the shapefile has negative. Match to the shapefile.
         longitude2=longitude, # This looks weird but we are going to end up modifying the latitude and longitude fields so I'm copying them for later.
         latitude2=latitude) %>% 
  ggplot(aes(longitude,latitude)) + 
  geom_point()
