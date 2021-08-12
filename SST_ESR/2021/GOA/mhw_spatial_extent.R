library(sf)

data <- tidync("SST_ESR/2021/GOA/noaa-crw_mhw_v1.0_category_20210425.nc") %>% 
  hyper_filter(lat=(lat>790 & lat<3290)) %>% # Tidync won't allow a compound condition so we just do one. Will do the others below.
  hyper_tibble() %>% 
  filter(mask==0) %>% 
  mutate(lon2=180+(lon/20)-360, # The map is centered on the dateline - recenter around the prime meridian
         lon2=ifelse(lon2<0,lon2+360,lon2), # Transform the recentered data as 0-360
         lat2=(lat/20)-90, #  Convert lats to +/- 90
         rlat=round(lat2),
         rlon=round(lon2)) %>% 
  filter(between(rlon,100,300)) %>%  #  Keep just the Pacific
  rename(mhw=heatwave_category)

#  Let's make sure all those transformations worked. 
data %>% 
  filter(lat2>=53.5 & lat2<=60.5 & lon2>213 & lon2<228) %>% 
  ggplot(aes(lon2,lat2,color=factor(mhw))) +
  geom_point()


lkp.grid <- data %>% 
  filter(mask==0 & lat2>=53.5 & lat2<=60.5 & lon2>213 & lon2<228) %>% 
  mutate(lat=lat2,lon=lon2-360)

lkp.grid %>% 
ggplot(aes(lon2,lat2,color=factor(mhw))) +
  geom_point()

#  Read in the shapefile with all of our polygons
esr_shp <- st_read('Data/Shapefiles/Alaska_Marine_Management_Areas.gdb',layer="Alaska_Marine_Areas_dd")

esr_pts = st_join(
  st_as_sf(lkp.grid, coords = c("lon", "lat"), crs = 4326, agr = "constant")  %>% # Use the duplicated lat/lon columns for matching to avoid rounding issues.
    st_transform(st_crs(esr_shp)$proj4string),
  esr_shp) %>% 
  data.frame %>% 
  filter(Ecosystem_Subarea=="Eastern Gulf of Alaska")

esr_pts %>% 
  ggplot(aes(lon2,lat2,color=factor(mhw))) +
  geom_point()

esr_pts %>% 
  group_by(mhw) %>% 
  tally() %>% 
  mutate(prop=n/sum(n),
         prop2=1-prop)
