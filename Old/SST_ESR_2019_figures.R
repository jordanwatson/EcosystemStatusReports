library(tidyverse)
library(viridis)
library(rgdal)
library(forcats)
library(gridExtra)

OceansBlue3='#00467F'
WavesTeal1='#1ECAD3'
OceansBlue1='#0093D0'

bering <- as.character(c(514,524,508,512,516,509,513,517,521))

data2 <- mydata %>% 
  filter(!is.na(sst.mean)) %>% 
  mutate(year=as.numeric(format(date,"%Y")),
         month=as.numeric(format(date,"%m")),
         julian=as.POSIXlt(date)$yday+1,
         week=as.numeric(format(date,"%U"))+1,
         newyr=ifelse(month==12,year+1,year),
         season=ifelse(month%in%c(9:11),"Sept-Nov",
                       ifelse(month%in%c(12,1,2),"Dec-Feb",
                              ifelse(month%in%c(3:5),"Mar-May","Jun-Aug")))) %>% 
  filter(newyr>2002)

saveRDS(data2,file="Data/SST_Workspace_for_2019_ESR.RDS")

p2 <- data2 %>% 
  mutate(drop.flag=ifelse((year==2002 & month<12) | (year==2019 & month>8),"Drop","Keep")) %>% 
  filter(NMFSAREA%in%c(bering) & !is.na(NMFSAREA) & drop.flag=="Keep") %>% 
  mutate(NMFSAREA=ifelse(maxlat>60.1 & maxlat<65.6," NBS "," EBS "),
         season=fct_relevel(season,
                            "Sept-Nov",
                            "Dec-Feb",
                            "Mar-May",
                            "Jun-Aug"),
         region=ifelse(m.depth<50,"Inner",
                       ifelse(m.depth>50 & m.depth<100,"Middle",
                              ifelse(m.depth>100 & m.depth<200,"Outer")))) %>% 
  group_by(NMFSAREA,season,newyr) %>% 
  summarise(meantemp=mean(sst.mean,na.rm=TRUE)) %>% 
  ungroup %>% 
  group_by(NMFSAREA,season) %>% 
  mutate(tempanom=(meantemp-mean(meantemp,na.rm=TRUE))) %>% 
  #mutate(tempanom=(meantemp-mean(meantemp,na.rm=TRUE))/sd(meantemp,na.rm=TRUE)) %>% 
  ggplot(aes(x = newyr, y = tempanom, fill=factor(NMFSAREA),width=0.75)) +
  geom_bar(stat="identity",position="dodge") + 
  theme_bw() + 
  scale_fill_manual(values=c(OceansBlue3,WavesTeal1),name="NMFS Area") + 
  geom_hline(yintercept=c(-0.5,0.5),linetype=2) +
  facet_wrap(~season) + 
  xlab("Year") + 
  ylab("Temperature Anomaly (?C)") + 
  theme(legend.position=c(0.15,0.95),
        legend.title=element_blank(),
        legend.text=element_text(size=13),
        legend.background = element_blank(),
        #legend.title=element_text(size=13),
        axis.text.y = element_text(size=11),
        axis.text.x = element_text(size=11),
        axis.title = element_text(size=13),
        plot.title = element_text(size=15),
        strip.text = element_text(size=13),
        strip.background = element_rect(fill = OceansBlue1),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size=0.25)) + 
  scale_x_continuous(breaks = 2003:2019, labels = c("2003","","","2006","","","2009","","","2012","","","2015","","","2018",""),expand=c(0.01,0.01)) + 
  guides(fill=guide_legend(ncol=2))



pdf("Figures/SST_EBS_2019_EBS.pdf",width=7.5,height=6)
p2
dev.off()

png("Figures/SST_EBS_2019_EBS.png",width=7.5,height=6,units="in",res=300)
p2
dev.off()

p3 <- data2 %>% 
  mutate(mygoa=ifelse(NMFSAREA%in%c(620,630,649) | 
                        (NMFSAREA==610 & as.numeric(STAT_AREA)<635000) | 
                        (NMFSAREA==640 & as.numeric(STAT_AREA)>445500),"WGOA",
                      ifelse(NMFSAREA==650 | 
                               (NMFSAREA==640 & STAT_AREA<445600),"EGOA",
                             ifelse(NMFSAREA==659,"SEAK Inside",NA))),
         mygoa=fct_relevel(mygoa,"WGOA","EGOA","SEAK Inside"),
         season=fct_relevel(season,
                            "Sept-Nov",
                            "Dec-Feb",
                            "Mar-May",
                            "Jun-Aug"),
         drop.flag=ifelse((year==2002 & month<12) | (year==2019 & month>8),"Drop","Keep")) %>% 
  filter(!is.na(mygoa) & drop.flag=="Keep") %>% 
  group_by(mygoa,season,newyr) %>% 
  summarise(meantemp=mean(sst.mean,na.rm=TRUE)) %>% 
  ungroup %>% 
  group_by(mygoa,season) %>% 
  mutate(tempanom=(meantemp-mean(meantemp,na.rm=TRUE))) %>% 
  #mutate(tempanom=(meantemp-mean(meantemp,na.rm=TRUE))/sd(meantemp,na.rm=TRUE)) %>% 
  ggplot(aes(x = newyr, y = tempanom, fill=factor(mygoa),width=0.75)) +
  geom_bar(stat="identity",position="dodge") + 
  theme_bw() + 
  scale_fill_manual(values=c(OceansBlue3,WavesTeal1,OceansBlue1),name="GOA Region") + 
  geom_hline(yintercept=c(-0.5,0.5),linetype=2) +
  facet_wrap(~season) + 
  xlab("Year") + 
  ylab("Temperature Anomaly (?C)") + 
  theme(legend.position=c(0.15,0.925),
        legend.title=element_blank(),
        legend.text=element_text(size=13),
        legend.background = element_blank(),
        #legend.title=element_text(size=13),
        axis.text.y = element_text(size=11),
        axis.text.x = element_text(size=11),
        axis.title = element_text(size=13),
        plot.title = element_text(size=15),
        strip.text = element_text(size=13),
        strip.background = element_rect(fill = OceansBlue1),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size=0.25)) + 
  scale_x_continuous(breaks = 2003:2019, labels = c("2003","","","2006","","","2009","","","2012","","","2015","","","2018",""),expand=c(0.01,0.01)) + 
  guides(fill=guide_legend(ncol=1))

pdf("Figures/SST_GOA_2019.pdf",width=7.5,height=6)
p3
dev.off()

png("Figures/SST_GOA_2019.png",width=7.5,height=6,units="in",res=300)
p3
dev.off()


#-----------------------------------------------------------------------------------------------
# Create Maps
#-----------------------------------------------------------------------------------------------

library(maps)
library(PBSmapping)
library(rgdal)
library(rgeos)
library(tidyverse)

#-----------------------------------------------------------------------------------------------
# Bering Sea
#-----------------------------------------------------------------------------------------------

#  Create map layers
xmin <- 160
xmax <- 210
ymin <- 50
ymax <- 68

myworld <- map_data("world2")
names(myworld) <- c("X","Y","PID","POS","region","subregion")

#  You can learn more about clipPolys via ?clipPolys. We need to specify our x and y boundaries so that it can clip the dataset.
myworld <- clipPolys(myworld, xlim=c(xmin,xmax),ylim=c(ymin,ymax), keepExtra=TRUE)

ggplot() + 
  geom_polygon(data=myworld,aes(x=X,y=Y,group=factor(PID))) + 
  coord_map(xlim=c(xmin,xmax),ylim=c(ymin,ymax)) + 
  theme_bw()

bering <- as.character(c(514,524,508,512,516,509,513,517,521))

simplenmfs <- readOGR(dsn="Data",layer="simplenmfs")  %>% 
  fortify(region="REP_AREA") %>% 
  mutate(bering=ifelse(id%in%bering & lat<60.1,"EBS",
                       ifelse(id%in%bering & lat>60.1 & lat<66.6,"NBS","Other")))


pdf("Map_Bering.pdf",width=6,height=6)
ggplot() + 
  geom_polygon(data=simplenmfs ,aes(x=long,y=lat,group=factor(group)),fill=NA,color="black")  +
  geom_polygon(data=simplenmfs %>% filter(bering!="Other"),aes(x=long,y=lat,group=factor(group),fill=bering),color="black")  +
  geom_polygon(data=myworld,aes(x=X,y=Y,group=factor(PID))) +
  coord_map(xlim=c(xmin,xmax),ylim=c(ymin,ymax)) + 
  theme_bw() + 
  scale_fill_manual(values=c(OceansBlue3,WavesTeal1),name="") +
  xlab("Longitude") + 
  ylab("Latitude") + 
  theme(legend.position=c(0.2,0.35),
        legend.background = element_blank())
dev.off()






#-----------------------------------------------------------------------------------------------
# GOA
#-----------------------------------------------------------------------------------------------

#  Create map layers
xmin <- 180
xmax <- 235
ymin <- 45
ymax <- 65

myworld <- map_data("world2")
names(myworld) <- c("X","Y","PID","POS","region","subregion")

#  You can learn more about clipPolys via ?clipPolys. We need to specify our x and y boundaries so that it can clip the dataset.
myworld <- clipPolys(myworld, xlim=c(xmin,xmax),ylim=c(ymin,ymax), keepExtra=TRUE)

ggplot() + 
  geom_polygon(data=myworld,aes(x=X,y=Y,group=factor(PID))) + 
  coord_map(xlim=c(xmin,xmax),ylim=c(ymin,ymax)) + 
  theme_bw()

#  Management areas
simplenmfs <- readOGR(dsn="Data",layer="simplenmfs")  %>% 
  fortify(region="REP_AREA") %>% 
  mutate(goa=ifelse(id=="610","wgoa",
              ifelse(id%in%c("640","650"),"egoa",
                     ifelse(id%in%c("620","630"),"cgoa","other"))))

#ESR areas
simplenmfs <- readOGR(dsn="Data",layer="simplenmfs")  %>% 
  fortify(region="REP_AREA") %>% 
  mutate(goa=ifelse(id=="610","wgoa",
                    ifelse(id%in%c("640","650"),"egoa",
                           ifelse(id%in%c("620","630"),"cgoa","other"))))

ggplot() + 
  geom_polygon(data=simplenmfs ,aes(x=long,y=lat,group=factor(group)),fill=NA,color="black")  +
  geom_polygon(data=simplenmfs %>% filter(goa!="other"),aes(x=long,y=lat,group=factor(group),fill=goa),color="black")  +
  geom_polygon(data=myworld,aes(x=X,y=Y,group=factor(PID))) +
  geom_vline(xintercept=c()) +
  coord_map(xlim=c(xmin,xmax),ylim=c(ymin,ymax)) + 
  theme_bw()


pdf("Map_GOA.pdf",width=6,height=6)
ggplot() + 
  geom_polygon(data=simplenmfs ,aes(x=long,y=lat,group=factor(group)),fill=NA,color="black")  +
  geom_polygon(data=simplenmfs %>% filter(goa!="other"),aes(x=long,y=lat,group=factor(group),fill=goa),color="black")  +
  geom_polygon(data=myworld,aes(x=X,y=Y,group=factor(PID))) +
  geom_vline(xintercept=c()) +
  coord_map(xlim=c(xmin,xmax),ylim=c(ymin,ymax)) + 
  theme_bw() + 
  scale_fill_manual(values=c(OceansBlue3,WavesTeal1,OceansBlue1),name="") +
  xlab("Longitude") + 
  ylab("Latitude") + 
  theme(legend.position=c(0.55,0.2),
        legend.background = element_blank())
dev.off()



#  GOA plot 2
xmin <- -168
xmax <- -125
ymin <- 45
ymax <- 65

myworld <- map_data("world")
names(myworld) <- c("X","Y","PID","POS","region","subregion")

#  You can learn more about clipPolys via ?clipPolys. We need to specify our x and y boundaries so that it can clip the dataset.
myworld <- clipPolys(myworld, xlim=c(xmin,xmax),ylim=c(ymin,ymax), keepExtra=TRUE)

#  Management areas
simplenmfs <- readOGR(dsn="Data",layer="simplenmfs")  %>% 
  fortify(region="REP_AREA") %>% 
  mutate(goa=ifelse(id=="610","wgoa",
                    ifelse(id%in%c("640","650"),"egoa",
                           ifelse(id%in%c("620","630"),"cgoa","other"))))

#ifelse(NMFSAREA%in%c(620,630,649) | 
#         (NMFSAREA==610 & as.numeric(STAT_AREA)<635000) | 
#         (NMFSAREA==640 & as.numeric(STAT_AREA)>445500),"WGOA",
#       ifelse(NMFSAREA==650 | 
##                (NMFSAREA==640 & STAT_AREA<445600),"EGOA",
#              ifelse(NMFSAREA==659,"SEAK Inside",NA))),
#mygoa=fct_relevel(mygoa,"WGOA","EGOA","SEAK")

#ESR areas
simplenmfs <- readOGR(dsn="Data",layer="simplenmfs")  %>% 
  fortify(region="REP_AREA") %>% 
  mutate(goa=ifelse((id%in%c(620,630,649,610) & long>(-163)) | 
                      (id==640 & long<(-144)),"WGOA",
                    ifelse(id==650 | 
                             (id==640 & long>(-144)),"EGOA",
                           ifelse(id==659,"SEAK Inside",NA))),
         goa=fct_relevel(goa,"WGOA","EGOA","SEAK Inside"))

simplenmfs <- readOGR(dsn="Data",layer="simplenmfs")  %>% 
  fortify(region="REP_AREA") %>% 
  mutate(goa=ifelse((id%in%c(620,630,649,610) & long>197) | (id==640 & long<216),"WGOA",
                    ifelse(id==c(650,640) & long>216,"EGOA",
                           ifelse(id==659,"SEAK Inside",NA))),
         goa=fct_relevel(goa,"WGOA","EGOA","SEAK Inside"))

pdf("Map_GOA_ESR.pdf",width=6,height=6)
ggplot() + 
  geom_polygon(data=simplenmfs %>% filter(!is.na(goa)),aes(x=long,y=lat,group=factor(group),fill=goa),color=NA)  +
  geom_polygon(data=simplenmfs %>% filter(goa=="EGOA"),aes(x=long,y=lat,group=factor(group)),fill=OceansBlue3,color=NA)  +
  geom_polygon(data=simplenmfs %>% filter(goa=="WGOA"),aes(x=long,y=lat,group=factor(group)),fill=OceansBlue1,color=NA)  +
  geom_polygon(data=simplenmfs ,aes(x=long,y=lat,group=factor(group)),fill=NA,color="black")  +
  geom_polygon(data=myworld,aes(x=X,y=Y,group=factor(PID))) +
  geom_vline(xintercept=c()) +
  coord_map(xlim=c(xmin,xmax),ylim=c(ymin,ymax)) + 
  theme_bw()  +
  coord_map(xlim=c(xmin,xmax),ylim=c(ymin,ymax)) + 
  theme_bw() + 
  scale_fill_manual(values=c(OceansBlue1,OceansBlue3,WavesTeal1),name="") +
  theme(legend.position=c(0.55,0.2),
        legend.background = element_blank(),
        axis.text=element_text(size=15),
        axis.title=element_blank(),
        legend.text = element_text(size=15))
dev.off()
