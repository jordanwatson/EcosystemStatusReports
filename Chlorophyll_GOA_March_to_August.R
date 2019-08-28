library(tidyverse)
library(ncdf4)
library(RCurl)
library(forcats)


mydate <- data.frame(mydate=seq(as.Date("2006-03-01"), as.Date("2019-07-31"), "days")) %>% 
  mutate(year=format(mydate,"%Y"),
         month=format(mydate,"%m")) %>% 
  filter(month%in%c("03","04","05","06","07"))

statlkp <- readRDS("Stat_Lookup.RDS") %>% 
  mutate(STAT_AREA=as.character(STAT_AREA),
         lon=round(lon,2),
         lat=round(lat,2)) %>% 
  inner_join(readRDS("spatial_data.RDS")) %>% 
  mutate(region=ifelse(NMFSAREA=="610","wgoa",
                       ifelse(NMFSAREA%in%c("640","650"),"egoa",
                              ifelse(NMFSAREA%in%c("620","630"),"cgoa","other")))) %>% 
  filter(region!="other")


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
  
for(i in 1:nrow(mydate[1:500,])){
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


# Blues (the fourth one is just white)
OceansBlue1='#0093D0'
WavesTeal3='#007078'
SeagrassGreen1='#93D500'
UrchinPurple1='#7F7FFF'
CrustaceanOrange1='#FF8300'

myplotfun <- function(myregion){
  data <- read_csv(paste0("ERDDAP_Chlorophyll/Data/",myregion,"_Summary_Chlorophyll.csv")) %>%
    mutate(Month=fct_relevel(Month,"March","April","May","June","July"),
           Season=ifelse(Month%in%c("March","April","May"),"Mar-May","Jun-Aug"),
           Season=fct_relevel(Season,"Mar-May","Jun-Aug"))
  
  png(paste0("ERDDAP_Chlorophyll/Spring_Chlorophyll_",myregion,"2.png"),width=10,height=7.5,units="in",res=300)
  print(data %>%
          ggplot(aes(as.numeric(year),chl,color=Month)) + 
          geom_line(size=1) + 
          geom_point(size=2) + 
          theme_bw() + 
          xlab("Year") + 
          ylab("Chlorophyll") + 
          theme(legend.position=c(0.1,0.75),
                axis.text=element_text(size=11),
                legend.text = element_text(size=11),
                legend.title = element_text(size=11)) + 
          scale_color_manual(values=c(OceansBlue1,SeagrassGreen1,WavesTeal3,UrchinPurple1,CrustaceanOrange1)) +
        scale_x_continuous(breaks=2006:2019,
                           labels=c("2006","","2008","","2010","","2012","","2014","","2016","","2018","")))
  dev.off()
  
  
  png(paste0("ERDDAP_Chlorophyll/Spring_Chlorophyll_",myregion,"2_Seasons.png"),width=10,height=7.5,units="in",res=300)
  print(data %>%
          group_by(Season,year) %>% 
          summarise(chl=mean(chl,na.rm=TRUE)) %>% 
          ungroup %>%
          ggplot(aes(as.numeric(year),chl,color=Season)) + 
          geom_line(size=1) + 
          geom_point(size=2) + 
          theme_bw() + 
          xlab("Year") + 
          ylab("Chlorophyll") + 
          theme(legend.position=c(0.1,0.75),
                axis.text=element_text(size=11),
                legend.text = element_text(size=11),
                legend.title = element_text(size=11)) + 
          scale_color_manual(values=c(OceansBlue1,SeagrassGreen1)) + 
          scale_x_continuous(breaks=2006:2019,
                             labels=c("2006","","2008","","2010","","2012","","2014","","2016","","2018","")))

  dev.off()
  
  
  png(paste0("ERDDAP_Chlorophyll/Seasonal_Chlorophyll_",myregion,"_Standardized_Anomaly.png"),width=10,height=7.5,units="in",res=300)
  print(data %>%
          group_by(Season,Year) %>% 
          summarise(meanchl=mean(chl,na.rm=TRUE)) %>% 
          ungroup %>% 
          group_by(Season) %>% 
          mutate(chlanom=(meanchl-mean(meanchl))/sd(meanchl)) %>% 
          ggplot(aes(factor(Year),chlanom)) + 
          geom_bar(stat="identity") + 
          theme_bw() + 
          xlab("Year") + 
          ylab("Chlorophyll") + 
          facet_wrap(~Season))
  dev.off()
  
  
  png(paste0("ERDDAP_Chlorophyll/Seasonal_Chlorophyll_",myregion,"_Centered_Anomaly.png"),width=10,height=7.5,units="in",res=300)
  print(data %>%
          group_by(Season,Year) %>% 
          summarise(meanchl=mean(chl,na.rm=TRUE)) %>% 
          ungroup %>% 
          group_by(Season) %>% 
          mutate(chlanom=(meanchl-mean(meanchl))) %>% 
          ggplot(aes(factor(Year),chlanom)) + 
          geom_bar(stat="identity") + 
          theme_bw() + 
          xlab("Year") + 
          ylab("Chlorophyll") + 
          facet_wrap(~Season))
  dev.off()
  
  png("ERDDAP_Chlorophyll/Seasonal_Chlorophyll_Centered_Anomaly_Combined.png",width=10,height=7.5,units="in",res=300)
  print(bind_rows(read_csv("ERDDAP_Chlorophyll/Data/eGOA_Summary_Chlorophyll.csv") %>% mutate(Region="eGOA"),
                  read_csv("ERDDAP_Chlorophyll/Data/cGOA_Summary_Chlorophyll.csv") %>% mutate(Region="cGOA")) %>%
          mutate(Month=fct_relevel(Month,"March","April","May","June","July"),
                 Season=ifelse(Month%in%c("March","April","May"),"Mar - May","June - August"),
                 Season=fct_relevel(Season,"Mar - May","June - August")) %>% 
          group_by(Season,Year,Region) %>% 
          summarise(meanchl=mean(chl,na.rm=TRUE)) %>% 
          ungroup %>% 
          group_by(Season,Region) %>% 
          mutate(chlanom=(meanchl-mean(meanchl))) %>% 
          ggplot(aes(factor(Year),chlanom,fill=Region)) + 
          geom_bar(stat="identity",position="dodge") + 
          theme_bw() + 
          xlab("Year") + 
          ylab("Chlorophyll") + 
          facet_wrap(~Season) + 
          theme(legend.position=c(0.1,0.75),
                axis.text=element_text(size=11),
                legend.text = element_text(size=11),
                legend.title = element_text(size=11)) + 
          scale_fill_manual(values=c(OceansBlue1,SeagrassGreen1)) + 
          scale_x_discrete(c("2006","","2008","","2010","","2012","","2014","","2016","","2018",""),
                           labels=c("2006","","2008","","2010","","2012","","2014","","2016","","2018","")))
  dev.off()
}

myplotfun("eGOA")
myplotfun("cGOA")
