library(tidync)

t.ak <- as.bathy(tidync("Data/Bering_Sea_Bathy.nc") %>% # Since I already had the raster code written with the marmap pkg, I convert to marmap file with as.bathy()
                   hyper_tibble() %>% 
                   data.frame %>% 
                   dplyr::select(longitude,latitude,ROSE) %>% 
                   mutate(longitude=longitude-360)) #The ETOPO bathy data were in 0-360 so convert.

GDB <- st_read("Data/Shapefiles/Alaska_Marine_Management_Areas.gdb")
#filter to regions, if you don't do this df3 will end up with >1000 points
ESR <- GDB %>% filter(Ecosystem_Area=="Eastern Bering Sea")

#  Load a basemap
world <- ne_countries(scale = "medium", returnclass = "sf")

#  Load bathymetry contour data. Use coarse resolution for less messiness
#  No longer using this. Just using the ETOPO5 download
# b.ak <- getNOAA.bathy(lon1=min(picdat$longitude),
#                       lon2=max(picdat$longitude),
#                       lat1=min(picdat$latitude),
#                       lat2=max(picdat$latitude), 
#                       resolution=25)

pdf("Map_with_bathy.pdf")
ggplot()+
  geom_sf(data=ESR) +
  geom_contour(data=fortify(t.ak),aes(x=x,y=y,z=z),breaks=c(-50,-200),col="black",alpha=0.5,size=0.1) +
  geom_hline(yintercept=60,linetype=2) +
  geom_sf(data=world, fill="tan")+
  xlim(c(-180, -157))+
  ylim(c(54,67)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text=element_blank(),
        axis.title=element_blank()) +
  geom_label(label="Northern Bering Sea",x=--170,y=61)
dev.off()