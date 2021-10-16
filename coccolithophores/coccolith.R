#  Coccolithophore ESR contribution

library(tidyverse)
library(lubridate)
library(tidync)
library(sf)
library(marmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(raster)
library(scales)

#  Download data here. I used the boundaries 54-60, -179.972- to -156
#https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdVH2018picmday.html


#-------------------------------------------------------------------------
#  First we have to match the spatial data to the ESR regions. 
#  Jens, you can skip this first stuff and start below where you ready in the RDS file.
#  But here are the steps to match the spatial data from VIIRS with our ESR Bering Sea shapefile
#-------------------------------------------------------------------------

#  Extract the longitudes and latitudes from the netCDF file
lkp <- tidync("coccolithophores/erdVH2018picmday_through_Aug2021.nc") %>% 
  hyper_transforms()


#  define as shapefile and set a crs of WGS84
mygrid <- expand.grid(longitude=lkp$longitude$longitude,latitude=lkp$latitude$latitude) %>% 
  mutate(lon=longitude,lat=latitude) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326, agr = "constant")


#read in Bering Sea ESR shapefile, automatically reads in the first layer
esr <- st_read("Data/Shapefiles/Alaska_Marine_Management_Areas.gdb") %>% 
  filter(!is.na(BSIERP_ID))

#  Transform the points to match the adfg projection and perform point-in-polygon operation
beringgrid <- mygrid %>% 
  st_transform(st_crs(esr)$proj4string) %>% 
  st_join(esr,join=st_within)


# #  Download a bathymetry grid using marmap (note that this method yields too high of resolution bathymetry)
# r.ak10 <- getNOAA.bathy(lon1=min(lkp$longitude$longitude),
#                                         lon2=max(lkp$longitude$longitude),
#                                         lat1=min(lkp$latitude$latitude),
#                                         lat2=max(lkp$latitude$latitude), 
#                                         resolution=25)
# 
# #convert the match polygon points to data frame, add depth, classify regions
# newdat <-beringgrid %>% 
#   data.frame %>%
#   filter(!is.na(BSIERP_ID)) %>% 
#   dplyr::select(longitude,latitude,BSIERP_ID,BSIERP_Region_Name) %>% 
#   mutate(depth=round(raster::extract(marmap::as.raster(r.ak10),cbind(longitude,latitude),method="bilinear"),0), # Extract depth from bathy data
#          location=case_when((between(depth,-50,-30)) & (grepl("inner",BSIERP_Region_Name) | BSIERP_Region_Name=="AK peninsula")~"Inner", #Classify the inner waters
#                             ((between(depth,-100,-50)) & (grepl("middle",BSIERP_Region_Name) | BSIERP_Region_Name=="AK peninsula")~"Middle"))) %>% # Classify middle waters
#   filter(!is.na(depth) & !is.na(location)) #Drop points outside the inner and middle domain.

#  With the higher res bathy data, I had to make sure that the depth ranges were restricted to the inner and middle domain but with the new 
#  bathy data that was no longer necessary
# newdat <-beringgrid %>% 
#   data.frame %>%
#   filter(!is.na(BSIERP_ID)) %>% 
#   dplyr::select(longitude,latitude,BSIERP_ID,BSIERP_Region_Name) %>% 
#   mutate(depth=round(raster::extract(marmap::as.raster(t.ak),cbind(longitude,latitude),method="bilinear"),0), # Extract depth from bathy data
#          location=case_when((depth>-50 & depth<(-30)) & (grepl("inner",BSIERP_Region_Name) | BSIERP_Region_Name=="AK peninsula")~"Inner", #Classify the inner waters
#                             ((depth>(-100) & depth<(-50)) & (grepl("middle",BSIERP_Region_Name) | BSIERP_Region_Name=="AK peninsula")~"Middle"))) %>% # Classify middle waters
#   filter(!is.na(depth) & !is.na(location)) #Drop points outside the inner and middle domain.

#  Instead, we can download a coarser resolution bathymetry product
#  https://pae-paha.pacioos.hawaii.edu/erddap/griddap/etopo5.html
#  Here is the link to the actual netcdf parameters I used (54:60, 180 - 210)
#https://pae-paha.pacioos.hawaii.edu/erddap/griddap/etopo5.nc?ROSE%5B(54):1:(60)%5D%5B(180):1:(210)%5D
t.ak <- as.bathy(tidync("coccolithophores/etopo5_3896_f0f5_d49b.nc") %>% # Since I already had the raster code written with the marmap pkg, I convert to marmap file with as.bathy()
                   hyper_tibble() %>% 
                   data.frame %>% 
                   dplyr::select(longitude,latitude,ROSE) %>% 
                   mutate(longitude=longitude-360)) #The ETOPO bathy data were in 0-360 so convert.

#  Create a version of the spatial grid that includes bathymetry and assign the shallow data to inner domain and deeper to middle
newdat <-beringgrid %>% 
  data.frame %>%
  filter(!is.na(BSIERP_ID)) %>% 
  dplyr::select(longitude,latitude,BSIERP_ID,BSIERP_Region_Name) %>% 
  mutate(depth=round(raster::extract(marmap::as.raster(t.ak),cbind(longitude,latitude),method="bilinear"),0), # Extract depth from bathy data
         location=case_when((depth>-50 & depth<(-30))~"Inner", #Classify the inner waters
                            ((depth>(-100) & depth<(-50))~"Middle"))) %>% # Classify middle waters
  filter(!is.na(depth) & !is.na(location)) #Drop points outside the inner and middle domain.

#  Check it, yo!
# newdat %>%
#   ggplot(aes(longitude,latitude,color=depth)) +
#   geom_point()
# 
# newdat %>%
#   ggplot(aes(longitude,latitude,color=location)) +
#   geom_point()

#  Match the spatial lookup grid (above) with the full pic dataset from VIIRS
picdat <- tidync("coccolithophores/erdVH2018picmday_through_Aug2021.nc") %>% 
  hyper_filter(longitude=longitude%in%c(unique(newdat$longitude)), # Filter unused coordinates to speed the extraction
               latitude=latitude%in%c(unique(newdat$latitude))) %>% 
  hyper_tibble() %>% 
  inner_join(newdat) %>% # Join the spatial data with the netcdf
  mutate(date=as_date(as_datetime(time)),
         year=year(date),
         month=month(date))

picdat %>% 
  saveRDS("coccolithophores/picdata.RDS")
#--------------------------------------------------------------------------------------------------
#  End Data Prep
#--------------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------------
#  Jens - start here
#--------------------------------------------------------------------------------------------------
picdat <- readRDS("coccolithophores/picdata.RDS")

#  Plot the data

#  Load a basemap
world <- ne_countries(scale = "medium", returnclass = "sf")

#  Load bathymetry contour data. Use coarse resolution for less messiness
#  No longer using this. Just using the ETOPO5 download
# b.ak <- getNOAA.bathy(lon1=min(picdat$longitude),
#                       lon2=max(picdat$longitude),
#                       lat1=min(picdat$latitude),
#                       lat2=max(picdat$latitude), 
#                       resolution=25)

# Map it, yo! Looks really similar to Carol's.
ggplot()+
  geom_sf(data=world, fill="tan")+
  xlim(c(-177, -160))+
  ylim(c(53,63)) +
  geom_point(data=picdat %>% filter(pic>0.0011 & month==9),
             aes(longitude,latitude,color=location),size=0.1) +
  geom_contour(data=fortify(t.ak),aes(x=x,y=y,z=z),breaks=c(-30,-50,-100),col="black",alpha=0.5,size=0.1) +
  facet_wrap(~year) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text=element_blank(),
        axis.title=element_blank())

#  Carol filters based on areas with more than 0.0011 pic, so I did that. 
# picdat %>% 
#   filter(pic>0.0011 & month==9) %>% 
#   group_by(year,location) %>% 
#   summarise(area=(4.64^2)*sum(!is.na(pic))) %>% # The 4km grid is technically 4.64km so I squared that as a multiplier for each value that is not NA.
#   ggplot(aes(year,area,color=location)) + 
#   geom_point() +
#   geom_line() +
#   xlim(2002,2020) +
#   ylim(0,140000) # I just tried to scale similar to Carol

sumdat <- picdat %>% 
  filter(pic>0.0011 & month==9) %>% 
  group_by(year,location) %>% 
  summarise(area=(4^2)*sum(!is.na(pic))) %>% # The 4km grid is technically 4.64km but I don't know if Carol use 4 or 4.64.
  right_join(expand.grid(year=2012:2021,location=c("Inner","Middle"))) %>% 
  replace_na(list(area=0))

total <- sumdat %>% 
  group_by(year) %>% 
  summarise(location="Total",
            area=sum(area))

sumdat %>% 
  bind_rows(total) %>% 
  bind_rows(
    expand.grid(year=2012:2021,
                location="Mean") %>% 
      mutate(area=mean(total$area))
    ) %>% 
  mutate(location=fct_relevel(location,"Inner","Middle","Total","Mean")) %>% 
  ggplot(aes(year,area,color=location,linetype=location)) + 
  geom_line() +
  theme_bw() +
  scale_linetype_manual(values=c(1,1,1,2),guides(title="")) +
  scale_color_manual(values=c("red","blue","black","black"),guides(title="")) +
  scale_y_continuous(label=comma,limits = c(0,140000),breaks=c(0,20000,40000,60000,80000,100000,120000,140000)) +
  ylab("Area (km^2)") + 
  theme(legend.position = "none")


#  The following just compared the new PIC downloaded data with Carol's .nc from last year.

#  To make sure we're working with similar data as Carol, we can compare her file with ours. 
#  From the name of her file, V20202452020274.L3m_MO_SNPP_PIC_pic_4km.nc, I discerned this to be the single global image file for Sept 2020.
#  If you break down 20202452020274, you see 2020 245 and 2020 279. These correspond to the Julian days for September.
#  So I extracted the September 2020 data from our new netcdf (erdVH2018picmday_through_Aug2021.nc) which corresponds to the date index of 105.
#  I then picked a random grid coordinate and compared the pic values from the two files and they match. So it looks like our data are the same.
new <- tidync("coccolithophores/erdVH2018picmday_through_Aug2021.nc") %>% 
  hyper_filter(latitude=index<10,
               longitude=index <10,
               time=index==105) %>% 
  hyper_tibble() %>% 
  mutate(date=as_date(as_datetime(time)),
         year=year(date),
         month=month(date))

old <- tidync("coccolithophores/V20202452020274.L3m_MO_SNPP_PIC_pic_4km.nc") %>% 
  hyper_filter(lon=lon==new$longitude[1],
               lat=lat==new$latitude[1]) %>% 
  hyper_tibble()