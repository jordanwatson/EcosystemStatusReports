#-----------------------------------------------------------------------------------------
# This script will extract sea surface temperature data from the Coral Reef Watch data stored within AKFIN.
# These data were previously downloaded from https://coastwatch.pfeg.noaa.gov/erddap/griddap/NOAA_DHW.html
# It will extract daily data for the annual periods (seasons) that are critical for each species and life stage.
# For those date periods, a point-in-polygon operation will extract the temperatures for only those gridded locations
# that fall within the designated EFH polygons. 
# Finally, for each year, the annual surface temperatures within the EFH are averaged to yield a single temperature value
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
#  For use on Windows, we use a modified version of MCApply from the parallelsugar package
source("parallel_st_join_function.R")
#-----------------------------------------------------------------------------------------
# End Chunk 1
#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
# Chunk 2 - Match the lat-lon grid (points) from the SST data with the species-stage-EFH polygons.
#-----------------------------------------------------------------------------------------

#  Read in our spatial lookup table. This is all of the sst points in the lat / long grid that we download each day for the SST data.
#  The file is available at https://github.com/jordanwatson/AKFIN_WebService/blob/main/Data/crwsst_spatial_lookup_table.RDS
#  To optimize the process, I filtered out those records that occurred in the GOA and were shallower than 1000m, based on knowledge of the EFH areas.
sstpt <- readRDS("Data/crwsst_spatial_lookup_table.RDS") %>% 
  filter(depth>(-1000) & Ecosystem=="Gulf of Alaska" & !is.na(longitude) & longitude<0) %>% 
  mutate(longitude2=longitude, # This looks weird but we are going to end up modifying the latitude and longitude fields so I'm copying them for later.
         latitude2=latitude)

#  Load the parallel processing point-in-polygon function. 
#  Note that this is setup to run on Windows, which handles parallelization differently than Linux or Mac.
mymatchfun <- function(myshape,cores){
  #  Set a crs of WGS84 - the point data must be projected.
  points_sf = st_as_sf(sstpt %>% dplyr::select(id,longitude,latitude,longitude2,latitude2), coords = c("longitude", "latitude"), crs = 4326, agr = "constant") 
  
  #  Load the polygon layer
  efh_shp <- st_read(myshape)
  #  Let's see waht its projection is.
  print(st_crs(efh_shp)$proj4string)
  
  #  Transform the points to match the bsierp regions projection
  efh_pts <- points_sf %>% st_transform(st_crs(efh_shp)$proj4string)
  
  print("!!***************************************!! Matching points to polygons: This may take a minute.")
  
  #  Match the points to the GOA EFH polygon. This might take a while.
  return(st_parallel(efh_pts, st_join, cores, y = efh_shp) %>% 
           filter(!is.na(OBJECTID)))
}

#  Identify the relevant stages for each species.
stages <- c("adult","earlyjuv","larval","subadult")

#  Run the point-in-polygon function for each of the EFH shapefiles for each species. 
#  Save the outputs so we don't have to rerun this.
#  I did these separately for each species as I wasn't sure how glitchy they would be. 
#  Each shapefile took about 30 seconds to process with 15 cores running on my virtual machine. 
lapply(stages,function(x) mymatchfun(paste0("Data/ATF_Shapes/atf_",x,"_efhcore.shp"),15) %>% 
  dplyr::select(id) %>% 
  mutate(stage=x)) %>% 
  bind_rows() %>% 
  saveRDS("Data/ATF_EFH_coord_ID.RDS")

lapply(stages,function(x) mymatchfun(paste0("Data/Pcod_Shapes/pcod_",x,"_efhcore.shp"),15) %>% 
         dplyr::select(id) %>% 
         mutate(stage=x)) %>% 
  bind_rows() %>% 
  saveRDS("Data/PCOD_EFH_coord_ID.RDS")

lapply(stages,function(x) mymatchfun(paste0("Data/Pollock_Shapes/poll_",x,"_efhcore.shp"),15) %>% 
         dplyr::select(id) %>% 
         mutate(stage=x)) %>% 
  bind_rows() %>% 
  saveRDS("Data/POLLOCK_EFH_coord_ID.RDS")

lapply(stages,function(x) mymatchfun(paste0("Data/POP_Shapes/pop_",x,"_efhcore.shp"),15) %>% 
         dplyr::select(id) %>% 
         mutate(stage=x)) %>% 
  bind_rows() %>% 
  saveRDS("Data/POP_EFH_coord_ID.RDS")

lapply(stages,function(x) mymatchfun(paste0("Data/Sablefish_Shapes/sab_",x,"_efhcore.shp"),15) %>% 
         dplyr::select(id) %>% 
         mutate(stage=x)) %>% 
  bind_rows() %>% 
  saveRDS("Data/SABLEFISH_EFH_coord_ID.RDS")


#  Plot to make sure it looks ok.
# readRDS("Data/SABLEFISH_EFH_coord_ID.RDS") %>%
#   ggplot() + 
#   geom_sf(size=0.25) + 
# facet_wrap(~stage)

#-----------------------------------------------------------------------------------------
# End Chunk 2
#-----------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------
# Chunk 3 - Extract and summarizes the SST data for each year-season-species-stage.
#-----------------------------------------------------------------------------------------

#  Create a data.frame of the months of interest for each species and life history.
#  If reviewers want different critical periods for a species / stage, the minmo and maxmo are what would need to change here.
# datefilter <- data.frame(spp=c(rep("SABLEFISH",2),rep("POLLOCK",2),rep("PCOD",2),rep("POP",2),rep("ATF",2)),
#                          lifestage=rep(c("larval","earlyjuv"),5),
#                          minmo=c(5,7,4,6,4,6,5,9,1,5),
#                          maxmo=c(6,10,5,10,5,10,8,10,4,9))

datefilter <- data.frame(spp=c(rep("SABLEFISH",1),rep("POLLOCK",1),rep("PCOD",1),rep("POP",1),rep("ATF",1)),
                         lifestage=rep(c("larval"),5),
                         minmo=c(5,4,4,5,2),
                         maxmo=c(6,5,6,8,4))

#  Load the EFH lookup tables. Because our internal spatial lookup table has a unique ID for each lat-lon combo,
#  these files do not contain lat-lons but instead an id field that we'll match with the daily SST data.
data <- readRDS("Data/SABLEFISH_EFH_coord_ID.RDS") %>% data.frame %>% mutate(species="SABLEFISH") %>% dplyr::select(-geometry) %>% 
  bind_rows(readRDS("Data/POLLOCK_EFH_coord_ID.RDS") %>% data.frame %>% mutate(species="POLLOCK") %>% dplyr::select(-geometry)) %>% 
  bind_rows(readRDS("Data/PCOD_EFH_coord_ID.RDS") %>% data.frame %>% mutate(species="PCOD") %>% dplyr::select(-geometry)) %>% 
  bind_rows(readRDS("Data/POP_EFH_coord_ID.RDS") %>% data.frame %>% mutate(species="POP") %>% dplyr::select(-geometry)) %>% 
  bind_rows(readRDS("Data/ATF_EFH_coord_ID.RDS") %>% data.frame %>% mutate(species="ATF") %>% dplyr::select(-geometry)) %>% 
  rename(lifestage=stage,spp=species) %>% 
  inner_join(datefilter)


#  Load this lookup table for use in the loop - it will drastically reduce the size of the sst file to speed operations
sstpt <- readRDS("Data/crwsst_spatial_lookup_table.RDS") %>% 
  filter(depth>(-1000) & Ecosystem=="Gulf of Alaska" & !is.na(longitude) & longitude<0)

#  These files are stored in a different Rproject.
#  This operation is pretty huge and takes about 30 seconds per year. 
tempdat <- data.frame()
for(myyear in 1986:2020){
  
  #  Load the annual sst data file.
  yeardat <- readRDS(paste0("../../ESR/Data/crwsst/crw_sst_matched/crw_sst_matched_",myyear,".RDS")) %>% 
    inner_join(sstpt) # This file is huge so clip it with a set of coordinates for the GOA that are shallower than 1000m to reduce size.
  
  for(myspp in unique(as.character(datefilter$spp))){
    for(mystage in unique(as.character(datefilter$lifestage))){
      
      newdat <- yeardat %>% # Load daily annual sst file for Alaska
        inner_join(data %>% filter(spp==myspp & lifestage==mystage)) %>% #  merge with the EFH spatial coordinates
        mutate(date=as_date(date), #  reformat the date, extract month
               month=month(date),
               year=year(date),
               flag=ifelse(between(month,minmo,maxmo),1,0)) %>% # flag those dates that fall within our critical periods for each species-stage
        filter(flag==1) %>% # remove those dates outside our critical periods.
        group_by(year,spp,lifestage) %>% # for each species-stage combo, average the SST data across the season of interest
        summarise(sst=mean(CRW_SST)) %>% # retain the year to merge across years.
        bind_rows() #  collapse the data from list to data.frame
      
      tempdat <- newdat %>% bind_rows(tempdat)
    }
  }
}
  
tempdat %>% 
  saveRDS("EFH_SST_new.RDS")

# Take a look!
readRDS("EFH_SST_new.RDS") %>% 
  ggplot(aes(year,sst,linetype=lifestage)) + 
  geom_line() + 
  facet_wrap(~spp)



lapply(1986:2020, function(x) readRDS(paste0("../../ESR/Data/crwsst/crw_sst_matched/crw_sst_matched_",x,".RDS")) %>% 
    inner_join(sstpt %>% dplyr::select(id)) %>% 
      bind_rows() %>% 
      mutate(year=year(date)) %>% 
      group_by(year) %>% 
      summarise(meansst=mean(CRW_SST))) %>% 
      bind_rows() %>% 
  saveRDS("SST_GOA_1000m_ANNUAL.RDS")

readRDS("SST_GOA_1000m_ANNUAL.RDS") %>% 
  ggplot(aes(year,meansst)) + 
  geom_line()

