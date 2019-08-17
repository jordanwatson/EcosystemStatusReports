library(tidyverse)
library(broom)
library(dplyr)
library(tibble)
library(ncdf4)
library(rgdal)
library(RCurl)
library(forcats)


mydate <- data.frame(mydate=seq(as.Date("2006-03-01"), as.Date("2019-07-31"), "days")) %>% 
  mutate(year=format(mydate,"%Y"),
         month=format(mydate,"%m")) %>% 
  filter(month%in%c("03","04","05","06","07"))

statlkp <- readRDS("ERDDAP_Chlorophyll/Data/Stat_Lookup.RDS") %>% 
  mutate(STAT_AREA=as.character(STAT_AREA),
         lon=round(lon,2),
         lat=round(lat,2)) %>% 
  inner_join(readRDS("ERDDAP_Chlorophyll/Data/spatial_data.RDS")) %>% 
  mutate(region=ifelse(NMFSAREA=="610","wgoa",
                       ifelse(NMFSAREA%in%c("640","650"),"egoa",
                              ifelse(NMFSAREA%in%c("620","630"),"cgoa","other")))) %>% 
  filter(region!="other")

wgoa <- statlkp %>% filter(region=="wgoa")
egoa <- statlkp %>% filter(region=="egoa")
cgoa <- statlkp %>% filter(region=="cgoa")

mystat <- statlkp %>% 
  group_by(region) %>% 
  summarise(minlon=min(minlon),
            maxlon=max(maxlon),
            minlat=min(minlat),
            maxlat=max(maxlat))

#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------



mygoafun <- function(myregion){
  
  newdat <- data.frame()
  
  #  Create regional subset of spatial coordinates
  statlkp_region <- statlkp %>% filter(region==myregion) %>% mutate(lon=ifelse(lon<0,lon+360,lon))
  
  mybound <- mystat %>% 
    filter(region==myregion) %>% 
    mutate(minlon=ifelse(minlon<0,minlon+360,minlon),
           maxlon=ifelse(maxlon<0,maxlon+360,maxlon))
  
for(i in 1:nrow(mydate)){
    print(mydate$mydate[i])
    #URL <- "https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.nc?analysed_sst[(2010-01-04T09:00:00Z):1:(2010-01-04T09:00:00Z)][(47):(68)][(-179.99):(-130.01)]"
    x <- getBinaryURL(paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdMBchla14day.nc?chlorophyll[(",mydate$mydate[i],"T09:00:00Z):1:(",mydate$mydate[i],"T09:00:00Z)][(0.0):1:(0.0)][(",mybound$minlat,"):1:(",mybound$maxlat,")][(",mybound$minlon,"):1:(",mybound$maxlon,")]"))
    #x <- getBinaryURL(paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdMBchla14day.nc?chlorophyll[(",mydate$mydate[i],"T09:00:00Z):1:(",mydate$mydate[i],"T09:00:00Z)][(0.0):1:(0.0)][(56):1:(60)][(190):1:(200)]"))
    #x <- getBinaryURL(paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.nc?analysed_sst[(",mydate$mydate[i],"T09:00:00Z):1:(",mydate$mydate[i],"T09:00:00Z)][(47):(68.5)][(-179.99):(-130.01)]"))
    
    tmpSST <- tempfile(pattern="xwB", fileext=".nc")
    writeBin(object=x, con=tmpSST)
    nc <- nc_open(tmpSST)
    
    tempneg <- expand.grid(lon=ncvar_get(nc, varid = "longitude"),
                           lat=ncvar_get(nc, varid = "latitude")) %>% 
                           {bind_cols(.,data.frame(chl=as.vector(ncvar_get(nc,"chlorophyll"))))} #%>% 
    #mutate(sst=ifelse(sst<(-2),-2,sst)) %>% 
    #filter(!is.na(sst))
    nc_close(nc)
    
    tempneg <- tempneg %>% 
      mutate(lon=round(lon,2),
             lat=round(lat,2)) %>% 
      inner_join(statlkp_region) %>% 
      mutate(date=mydate$mydate[i]) %>% 
      filter(!is.na(chl))
    
    newdat <- newdat %>% 
      bind_rows(tempneg)
    
    rm(tempneg)
  }
  return(newdat)
}

temp <- mygoafun("wgoa")
saveRDS(temp,file="ERDDAP_Chlorophyll/Data/wGOA_Chlorophyll_Spring.RDS")

temp <- mygoafun("egoa")
saveRDS(temp,file="ERDDAP_Chlorophyll/Data/eGOA_Chlorophyll_Spring.RDS")

temp <- mygoafun("cgoa")
saveRDS(temp,file="ERDDAP_Chlorophyll/Data/cGOA_Chlorophyll_Spring.RDS")


nbsdat <- data.frame()

for(i in 1:nrow(mydate)){
  print(mydate$mydate[i])
  #URL <- "https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.nc?analysed_sst[(2010-01-04T09:00:00Z):1:(2010-01-04T09:00:00Z)][(47):(68)][(-179.99):(-130.01)]"
  x <- getBinaryURL(paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdMBchla14day.nc?chlorophyll[(",mydate$mydate[i],"T09:00:00Z):1:(",mydate$mydate[i],"T09:00:00Z)][(0.0):1:(0.0)][(60):1:(64)][(180):1:(195)]"))
  #x <- getBinaryURL(paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.nc?analysed_sst[(",mydate$mydate[i],"T09:00:00Z):1:(",mydate$mydate[i],"T09:00:00Z)][(47):(68.5)][(-179.99):(-130.01)]"))
  
  tmpSST <- tempfile(pattern="xwB", fileext=".nc")
  writeBin(object=x, con=tmpSST)
  nc <- nc_open(tmpSST)
  
  tempneg <- expand.grid(lon=ncvar_get(nc, varid = "longitude"),
                         lat=ncvar_get(nc, varid = "latitude")) %>% 
                         {bind_cols(.,data.frame(chl=as.vector(ncvar_get(nc,"chlorophyll"))))} #%>% 
  #mutate(sst=ifelse(sst<(-2),-2,sst)) %>% 
  #filter(!is.na(sst))
  nc_close(nc)
  
  tempneg <- tempneg %>% 
    mutate(lon=round(lon,2),
           lat=round(lat,2)) %>% 
    inner_join(statlkp) %>% 
    mutate(date=mydate$mydate[i])
  
  nbsdat <- nbsdat %>% 
    bind_rows(tempneg)
  
  rm(tempneg)
}

saveRDS(nbsdat,file="ERDDAP_Chlorophyll/Data/NBS_Chlorophyll_Spring.RDS")


#----------------------------------------------------------------------
#----------------------------------------------------------------------
#  Now summarize the data
#----------------------------------------------------------------------

newdat <- readRDS("ERDDAP_Chlorophyll/Data/EBS_Chlorophyll_Spring.RDS")
nbsdat <- readRDS("ERDDAP_Chlorophyll/Data/NBS_Chlorophyll_Spring.RDS")

#Eastern Bering Sea
readRDS("ERDDAP_Chlorophyll/Data/EBS_Chlorophyll_Spring.RDS") %>% 
  filter(lon<(197)) %>% 
  mutate(year=format(date,"%Y"),
         month=format(date,"%m")) %>% 
  group_by(month,year) %>% 
  summarise(chlvals=sum(!is.na(chl)),
            chl=mean(chl,na.rm=TRUE)) %>% 
  ungroup %>% 
  mutate(Year=as.numeric(year),
         Month=month.name[as.numeric(month)]) %>% 
  write_csv(.,"EBS_Summary_Chlorophyll.csv")


#Northern Bering Sea
#  Join month and year. Faster to merge with the date object than to extract from
#  the dates themselves
readRDS("ERDDAP_Chlorophyll/Data/NBS_Chlorophyll_Spring.RDS") %>% 
  mutate(year=format(date,"%Y"),
         month=format(date,"%m")) %>% 
  group_by(month,year) %>% 
  summarise(chlvals=sum(!is.na(chl)),
            chl=mean(chl,na.rm=TRUE)) %>% 
  ungroup %>% 
  mutate(Year=as.numeric(year),
         Month=month.name[as.numeric(month)]) %>% 
  write_csv(.,"NBS_Summary_Chlorophyll.csv")



#----------------------------------------------------------------------
#  Plot the data
#----------------------------------------------------------------------

# Blues (the fourth one is just white)
OceansBlue1='#0093D0'
WavesTeal3='#007078'
SeagrassGreen1='#93D500'

png("Spring_Chlorophyll_EBS.png",width=10,height=7.5,units="in",res=300)
testsum %>%
  mutate(Month=fct_relevel(Month,"March","April","May")) %>% 
  ggplot(aes(as.numeric(year),chl,color=Month)) + 
  geom_line(size=1) + 
  geom_point(size=2) + 
  theme_bw() + 
  xlab("Year") + 
  ylab("Chlorophyll") + 
  theme(legend.position=c(0.9,0.75),
        axis.text=element_text(size=11),
        legend.text = element_text(size=11),
        legend.title = element_text(size=11)) + 
  scale_x_continuous(breaks=c(1996:2019)) + 
  scale_color_manual(values=c(OceansBlue1,SeagrassGreen1,WavesTeal3))
dev.off()


png("Spring_Chlorophyll_NBS.png",width=10,height=7.5,units="in",res=300)
testsumnbs %>%
  mutate(Month=fct_relevel(Month,"March","April","May")) %>% 
  ggplot(aes(as.numeric(year),chl,color=Month)) + 
  geom_line(size=1) + 
  geom_point(size=2) + 
  theme_bw() + 
  xlab("Year") + 
  ylab("Chlorophyll") + 
  theme(legend.position=c(0.9,0.75),
        axis.text=element_text(size=11),
        legend.text = element_text(size=11),
        legend.title = element_text(size=11)) + 
  scale_x_continuous(breaks=c(1996:2019)) + 
  scale_color_manual(values=c(OceansBlue1,SeagrassGreen1,WavesTeal3))
dev.off()


#--------------------------------------------------------------------------------
# Now try with VIIRS data
#--------------------------------------------------------------------------------

mydate <- data.frame(mydate=seq(as.Date("2015-03-01"), as.Date("2019-07-31"), "days")) %>% 
  mutate(year=format(mydate,"%Y"),
         month=format(mydate,"%m")) %>% 
  filter(month%in%c("03","04","05","06","07"))

statlkpVIIRS <- readRDS("ERDDAP_Chlorophyll/Data/Stat_Lookup.RDS") %>% 
  mutate(STAT_AREA=as.character(STAT_AREA),
         lon=round(lon,2),
         lat=round(lat,2)) %>% 
  #inner_join(readRDS("../../PSESV_Manuscript/psesv_resubmission/Data/spatial_data.RDS") %>% 
  inner_join(readRDS("ERDDAP_Chlorophyll/Data/spatial_data.RDS") %>% 
               filter(m.depth<=-50 & m.depth>=-100) %>% 
               dplyr::select(STAT_AREA,m.depth))


#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------

newdat <- data.frame()

for(i in 1:nrow(mydate)){
  print(mydate$mydate[i])
  #URL <- "https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.nc?analysed_sst[(2010-01-04T09:00:00Z):1:(2010-01-04T09:00:00Z)][(47):(68)][(-179.99):(-130.01)]"
  x <- getBinaryURL(paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdVHNchla8day.nc?chla[(",mydate$mydate[i],"T09:00:00Z):1:(",mydate$mydate[i],"T09:00:00Z)][(0.0):1:(0.0)][(60):1:(56)][(-170):1:(-160)]"))
  #https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdVHNchla8day.nc?chla[(2015-03-01T00:00:00Z):1:(2015-03-01T00:00:00Z)][(0.0):1:(0.0)][(60):1:(56)][(-170):1:(-160)]
  
  tmpSST <- tempfile(pattern="xwB", fileext=".nc")
  writeBin(object=x, con=tmpSST)
  nc <- nc_open(tmpSST)
  
  tempneg <- expand.grid(lon=ncvar_get(nc, varid = "longitude"),
                         lat=ncvar_get(nc, varid = "latitude")) %>% 
                         {bind_cols(.,data.frame(chl=as.vector(ncvar_get(nc,"chla"))))} #%>% 
  #mutate(sst=ifelse(sst<(-2),-2,sst)) %>% 
  #filter(!is.na(sst))
  nc_close(nc)
  
  tempneg <- tempneg %>% 
    mutate(lon=round(lon,2),
           lat=round(lat,2)) %>% 
    inner_join(statlkpVIIRS) %>% 
    mutate(date=mydate$mydate[i])
  
  newdat <- newdat %>% 
    bind_rows(tempneg)
  
  rm(tempneg)
}

saveRDS(newdat,file="ERDDAP_Chlorophyll/Data/EBS_Chlorophyll_Spring_VIIRS.RDS")


#---------------------------------------------------------------------------------------------------
#  Now do the same for the NBS with VIIRS data
#---------------------------------------------------------------------------------------------------

nbsdat <- data.frame()

for(i in 1:nrow(mydate)){
  print(mydate$mydate[i])
  #URL <- "https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.nc?analysed_sst[(2010-01-04T09:00:00Z):1:(2010-01-04T09:00:00Z)][(47):(68)][(-179.99):(-130.01)]"
  x <- getBinaryURL(paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdVHNchla8day.nc?chla[(",mydate$mydate[i],"T09:00:00Z):1:(",mydate$mydate[i],"T09:00:00Z)][(0.0):1:(0.0)][(64):1:(60)][(-180):1:(-165)]"))
  
  #https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdVHNchla8day.nc?chla[(2015-03-01T00:00:00Z):1:(2015-03-01T00:00:00Z)][(0.0):1:(0.0)][(60):1:(56)][(-170):1:(-160)]
  
  tmpSST <- tempfile(pattern="xwB", fileext=".nc")
  writeBin(object=x, con=tmpSST)
  nc <- nc_open(tmpSST)
  
  tempneg <- expand.grid(lon=ncvar_get(nc, varid = "longitude"),
                         lat=ncvar_get(nc, varid = "latitude")) %>% 
                         {bind_cols(.,data.frame(chl=as.vector(ncvar_get(nc,"chla"))))} #%>% 
  #mutate(sst=ifelse(sst<(-2),-2,sst)) %>% 
  #filter(!is.na(sst))
  nc_close(nc)
  
  tempneg <- tempneg %>% 
    mutate(lon=round(lon,2),
           lat=round(lat,2)) %>% 
    inner_join(statlkpVIIRS) %>% 
    mutate(date=mydate$mydate[i]) %>% 
    filter(!is.na(chl))
  
  nbsdat <- nbsdat %>% 
    bind_rows(tempneg)
  
  rm(tempneg)
}


saveRDS(nbsdat,file="ERDDAP_Chlorophyll/Data/NBS_Chlorophyll_Spring_VIIRS.RDS")














data <- readRDS("ERDDAP_Chlorophyll/Data/EBS_Chlorophyll_Spring_VIIRS.RDS")

data <- data %>% 
  filter(!is.na(chl)) %>% 
  inner_join(statlkpVIIRS)


testebs <- nbsdat %>% 
  left_join(mydate %>% rename(date=mydate))

#  Summarize the chlorophyll data by month and year.
testsumnbs <- data %>% 
  inner_join(mydate %>% 
               rename(date=mydate) %>% 
               mutate(Year=as.numeric(year),
                      Month=month.name[as.numeric(month)])) %>% 
  group_by(Month,year) %>% 
  summarise(chlvals=sum(!is.na(chl)),
            chl=mean(chl,na.rm=TRUE)) %>% 
  ungroup

data <- data %>% filter(!is.na(chl))

ebs <- readRDS("ERDDAP_Chlorophyll/Data/EBS_Chlorophyll_Spring.RDS")
readRDS("ERDDAP_Chlorophyll/Data/NBS_Chlorophyll_Spring.RDS") %>% filter(!is.na(chl)) %>% saveRDS(.,file="ERDDAP_Chlorophyll/Data/NBS_Chlorophyll_Spring.RDS")

rm(nbsdat);gc()
ebsviirs <- readRDS("ERDDAP_Chlorophyll/Data/EBS_Chlorophyll_Spring_VIIRS.RDS")
nbsviirs <- readRDS("ERDDAP_Chlorophyll/Data/NBS_Chlorophyll_Spring_VIIRS.RDS")

data %>% 
  inner_join(mydate %>% 
               rename(date=mydate) %>% 
               mutate(Year=as.numeric(year),
                      Month=month.name[as.numeric(month)])) %>% 
  group_by(Month,year) %>% 
  summarise(chlvals=sum(!is.na(chl)),
            chl=mean(chl,na.rm=TRUE)) %>% 
  ungroup

testsumnbs %>%
  mutate(Month=fct_relevel(Month,"March","April","May","June","July")) %>% 
  ggplot(aes(as.numeric(year),chl,color=Month)) + 
  geom_line(size=1) + 
  geom_point(size=2) + 
  theme_bw() + 
  xlab("Year") + 
  ylab("Chlorophyll") + 
  theme(legend.position=c(0.9,0.75),
        axis.text=element_text(size=11),
        legend.text = element_text(size=11),
        legend.title = element_text(size=11)) + 
  scale_x_continuous(breaks=c(2006:2019)) + 
  scale_color_manual(values=c(OceansBlue1,SeagrassGreen1,WavesTeal3,UrchinPurple1,CrustaceanOrange1))


#----------------------------------------------------------------------
#  Plot the data
#----------------------------------------------------------------------

# Blues (the fourth one is just white)
OceansBlue1='#0093D0'
WavesTeal3='#007078'
SeagrassGreen1='#93D500'
UrchinPurple1='#7F7FFF'
CrustaceanOrange1='#FF8300'

#png("Spring_Chlorophyll_EBS.png",width=10,height=7.5,units="in",res=300)
testsum %>%
  mutate(Month=fct_relevel(Month,"March","April","May")) %>% 
  ggplot(aes(as.numeric(year),chl,color=Month)) + 
  geom_line(size=1) + 
  geom_point(size=2) + 
  theme_bw() + 
  xlab("Year") + 
  ylab("Chlorophyll") + 
  theme(legend.position=c(0.9,0.75),
        axis.text=element_text(size=11),
        legend.text = element_text(size=11),
        legend.title = element_text(size=11)) + 
  scale_x_continuous(breaks=c(1996:2019)) + 
  scale_color_manual(values=c(OceansBlue1,SeagrassGreen1,WavesTeal3))
dev.off()


#png("Spring_Chlorophyll_NBS.png",width=10,height=7.5,units="in",res=300)
testsumnbs %>%
  mutate(Month=fct_relevel(Month,"March","April","May","June","July")) %>% 
  ggplot(aes(as.numeric(year),chl,color=Month)) + 
  geom_line(size=1) + 
  geom_point(size=2) + 
  theme_bw() + 
  xlab("Year") + 
  ylab("Chlorophyll") + 
  theme(legend.position=c(0.9,0.75),
        axis.text=element_text(size=11),
        legend.text = element_text(size=11),
        legend.title = element_text(size=11)) + 
  scale_x_continuous(breaks=c(2006:2019)) + 
  scale_color_manual(values=c(OceansBlue1,SeagrassGreen1,WavesTeal3,UrchinPurple1,CrustaceanOrange1))
dev.off()


