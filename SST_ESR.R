library(tidyverse)
library(viridis)
library(rgdal)
library(forcats)
library(gridExtra)
 
#  Read in data from January 1, 2003
mydata <- readRDS("Data/mur_SST_stat6_all_columns.RDS") %>% 
  dplyr::select(sst.mean,date,everything())

spatialdat <- mydata %>% 
  dplyr::select(-c(sst.mean,date,sst.sd,year,month,julian,week)) %>% 
  distinct()

test <- bind_cols(readRDS("Data/myyear_2002_october.RDS")[1],
                  readRDS("Data/myyear_2002_november.RDS")[1],
                  readRDS("Data/myyear_2002_december.RDS")[1],
                  readRDS("Data/myyear_2018_may.RDS")[1],
                  readRDS("Data/myyear_2018_june.RDS")[1],
                  readRDS("Data/myyear_2018_july.RDS")[1],
                  readRDS("Data/myyear_2018_august.RDS")[1],
                  readRDS("Data/myyear_2018_september.RDS")[1]) %>% 
  dplyr::select(STAT_AREA,contains("mean")) %>% 
  gather(date,sst.mean,-STAT_AREA) %>% 
  mutate(date=substr(date,1,10)) %>% 
  inner_join(
    bind_cols(readRDS("Data/myyear_2002_october.RDS")[1],
              readRDS("Data/myyear_2002_november.RDS")[1],
              readRDS("Data/myyear_2002_december.RDS")[1],
              readRDS("Data/myyear_2018_may.RDS")[1],
              readRDS("Data/myyear_2018_june.RDS")[1],
              readRDS("Data/myyear_2018_july.RDS")[1],
              readRDS("Data/myyear_2018_august.RDS")[1],
              readRDS("Data/myyear_2018_september.RDS")[1]) %>% 
      dplyr::select(STAT_AREA,contains("sd")) %>% 
      gather(date,sst.sd,-STAT_AREA) %>% 
      mutate(date=substr(date,1,10))) %>% 
  mutate(date=as.Date(date),
         year=as.numeric(format(date,"%Y")),
         month=as.numeric(format(date,"%m"))) %>% 
  inner_join(spatialdat)


#  If you need to omit certain months due to incompleteness, modify the ifelse in "flag"
#data2 <- mydata %>% 
#  mutate(newyr=ifelse(month<4,year-1,year),
#         season=ifelse(month%in%c(4:9),"Summer (Apr - Sept)","Winter (Oct - Mar)"),
#         monthname=month.name[month],
#         flag=ifelse(newyr==2002 & month<4,1,
#                     ifelse(newyr==2018 & month>3,1,0)))


#  Join in the summer 2018 and winter 2002 data with the previous dataset.
data2 <- mydata %>% 
  bind_rows(test) %>% 
  filter(!is.na(sst.mean)) %>% 
  mutate(newyr=ifelse(month<4,year-1,year),
         season=ifelse(month%in%c(4:9),"Summer (Apr - Sept)","Winter (Oct - Mar)"),
         monthname=month.name[month],
         flag=0)



#data2 <- mydata %>% 
#  mutate(newyr=ifelse(month<4,year-1,year),
#         month2=ifelse(month<4,month+12,month),
#         season=ifelse(month2%in%c(4:9),"Summer (Apr - Sept)","Winter (Oct - Mar)"),
#         GOA=ifelse(is.na(GOA),0,GOA),
#         monthname=month.name[month],
#         flag=ifelse(newyr==2003 & month<4,1,
#                     ifelse(newyr==2018 & month>3,1,0))) 

#----------------------------------------------------
# Bering Sea
#----------------------------------------------------

bering <- unique(data2$NMFSAREA[data2$FMP_AREA_C=="BSAI"])
ebs514 <- unique(data2$STAT_AREA[data2$NMFSAREA=="514" & data2$maxlat<60.1])

bering <- as.character(c(514,524,508,512,516,509,513,517))

#  The flag field allows us to easily filter out winter 2003 which is incomplete and 
#  summer 2018 which is incomplete

p1 <- data2 %>% 
  filter(NMFSAREA%in%c(bering) & flag==0) %>% 
  mutate(NMFSAREA=ifelse(maxlat>60.1 & maxlat<65.6,"NBS",NMFSAREA),
         season=fct_relevel(season,"Winter (Oct - Mar)")) %>% 
      group_by(NMFSAREA,season,newyr) %>% 
      summarise(meantemp=mean(sst.mean,na.rm=TRUE)) %>% 
      ungroup %>% 
      group_by(NMFSAREA,season) %>% 
      mutate(tempanom=(meantemp-mean(meantemp,na.rm=TRUE))/sd(meantemp,na.rm=TRUE)) %>% 
  ggplot(aes(x = newyr, y = tempanom, fill=factor(NMFSAREA),width=0.65)) +
      geom_bar(stat="identity",position="dodge") + 
      theme_bw() + 
      scale_fill_viridis(name="NMFS Area",discrete=TRUE) + 
      geom_hline(yintercept=c(-0.5,0.5),linetype=2) +
  facet_wrap(~season) + 
      xlab("Year") + 
      ylab("Temperature Anomaly") + 
      theme(legend.position="top",
            legend.text=element_text(size=14),
            legend.title=element_text(size=14),
            axis.text.y = element_text(size=13),
            axis.text.x = element_text(size=13,angle=45,vjust=0.5),
            axis.title = element_text(size=14),
            plot.title = element_text(size=16),
            strip.text = element_text(size=14),
            panel.grid.minor = element_blank()) + 
      scale_x_continuous(breaks = 2002:2018, labels = c("2002","","2004","","2006","","2008","","2010","","2012","","2014","","2016","","2018")) + 
      guides(fill=guide_legend(ncol=6))

pdf("Figures/SST_EBS_2018.pdf",width=7.5,height=6)
p1
dev.off()

png("Figures/SST_EBS_2018.png",width=7.5,height=6,units="in",res=300)
p1
dev.off()


p2 <- data2 %>% 
  filter(NMFSAREA%in%c(bering) & flag==0 & maxlat<60.1) %>% 
  group_by(NMFSAREA,year) %>% 
  summarise(meantemp=mean(sst.mean,na.rm=TRUE)) %>% 
  ungroup %>% 
  group_by(NMFSAREA) %>% 
  mutate(tempanom=(meantemp-mean(meantemp,na.rm=TRUE))/sd(meantemp,na.rm=TRUE)) %>% 
  ggplot(aes(x = year, y = tempanom, fill=factor(NMFSAREA))) +
  geom_bar(stat="identity",position="dodge") + 
  theme_bw() + 
  scale_fill_viridis(name="NMFS Area",discrete=TRUE) + 
  geom_hline(yintercept=c(-0.5,0.5),linetype=2) +
  xlab("Year") + 
  ylab("Temperature Anomaly") + 
  theme(axis.text.y = element_text(size=13),
        axis.text.x = element_text(size=13,angle=45,vjust=0.5),
        axis.title = element_text(size=14),
        plot.title = element_text(size=16),
        panel.grid.minor = element_blank()) + 
  scale_x_continuous(breaks = 2002:2018, labels = c("2002","","2004","","2006","","2008","","2010","","2012","","2014","","2016","","2018")) 

p2

#----------------------------------------------------
# GOA
#----------------------------------------------------


#  The flag field allows us to easily filter out winter 2003 which is incomplete and 
#  summer 2018 which is incomplete

p3 <- data2 %>% 
  mutate(mygoa=ifelse(NMFSAREA%in%c(620,630,649) | 
                        (NMFSAREA==610 & as.numeric(STAT_AREA)<635000) | 
                        (NMFSAREA==640 & as.numeric(STAT_AREA)>445500),"WGOA",
                      ifelse(NMFSAREA==650 | 
                               (NMFSAREA==640 & STAT_AREA<445600),"EGOA",
                             ifelse(NMFSAREA==659,"SEAK",NA))),
         mygoa=fct_relevel(mygoa,"WGOA","EGOA","SEAK"),
         season=fct_relevel(season,"Winter (Oct - Mar)")) %>% 
  filter(!is.na(mygoa) & flag==0) %>% 
    group_by(mygoa,season,newyr) %>% 
    summarise(meantemp=mean(sst.mean,na.rm=TRUE)) %>% 
    ungroup %>% 
    group_by(mygoa,season) %>% 
    mutate(tempanom=(meantemp-mean(meantemp,na.rm=TRUE))/sd(meantemp,na.rm=TRUE)) %>% 
    ggplot(aes(x = newyr, y = tempanom, fill=factor(mygoa))) +
    geom_bar(stat="identity",position="dodge") + 
    theme_bw() + 
    scale_fill_viridis(name="GOA Region",discrete=TRUE) + 
    geom_hline(yintercept=c(-0.5,0.5),linetype=2) +
    facet_wrap(~season) + 
    xlab("Year") + 
    ylab("Temperature Anomaly") + 
    theme(legend.position="top",
          legend.text=element_text(size=14),
          legend.title=element_text(size=14),
          axis.text.y = element_text(size=13),
          axis.text.x = element_text(size=13,angle=45,vjust=0.5),
          axis.title = element_text(size=14),
          plot.title = element_text(size=16),
          strip.text = element_text(size=14),
          panel.grid.minor = element_blank()) + 
  scale_x_continuous(breaks = 2002:2018, labels = c("2002","","2004","","2006","","2008","","2010","","2012","","2014","","2016","","2018")) 

pdf("Figures/SST_GOA_2018.pdf",width=7.5,height=6)
p3
dev.off()

png("Figures/SST_GOA_2018.png",width=7.5,height=6,units="in",res=300)
p3
dev.off()

#----------------------------------------------------
# Aleutians
#----------------------------------------------------

#  Save the Aleutian data query for Ivonne Ortiz
p4 <- data2 %>% 
  mutate(myai=ifelse((NMFSAREA==610 & as.numeric(STAT_AREA)>635000) | NMFSAREA%in%c(518,519),"EAI",
                     ifelse(NMFSAREA%in%c(543),"WAI",
                            ifelse(NMFSAREA%in%c(541,542),"CAI",NA)))) %>% 
  filter(!is.na(myai) & flag==0) %>% 
  group_by(myai,season,newyr) %>% 
  summarise(meantemp=mean(sst.mean,na.rm=TRUE)) %>% 
  ungroup %>% 
  group_by(myai,season) %>% 
  mutate(tempanom=(meantemp-mean(meantemp,na.rm=TRUE))/sd(meantemp,na.rm=TRUE))

saveRDS(p4,"Data/AI_Anomalies.RDS")

#  The flag field allows us to easily filter out winter 2003 which is incomplete and 
#  summer 2018 which is incomplete

p4 <- data2 %>% 
  mutate(myai=ifelse((NMFSAREA==610 & as.numeric(STAT_AREA)>635000) | NMFSAREA%in%c(518,519),"EAI",
                     ifelse(NMFSAREA%in%c(543),"WAI",
                            ifelse(NMFSAREA%in%c(541,542),"CAI",NA))),
         myai=fct_relevel(myai,"WAI","CAI","EAI"),
         season=fct_relevel(season,"Winter (Oct - Mar)")) %>% 
  filter(!is.na(myai) & flag==0) %>% 
  group_by(myai,season,newyr) %>% 
  summarise(meantemp=mean(sst.mean,na.rm=TRUE)) %>% 
  ungroup %>% 
  group_by(myai,season) %>% 
  mutate(tempanom=(meantemp-mean(meantemp,na.rm=TRUE))/sd(meantemp,na.rm=TRUE)) %>% 
  ggplot(aes(x = newyr, y = tempanom, fill=(myai))) +
  geom_bar(stat="identity",position="dodge",width=0.65) + 
  theme_bw() + 
  scale_fill_viridis(name="Aleutian Region",discrete=TRUE) + 
  geom_hline(yintercept=c(-0.5,0.5),linetype=2) +
  facet_wrap(~season) + 
  xlab("Year") + 
  ylab("Temperature Anomaly") + 
  theme(legend.position="top",
        legend.text=element_text(size=14),
        legend.title=element_text(size=14),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12,angle=45,vjust=0.5),
        axis.title = element_text(size=14),
        plot.title = element_text(size=16),
        strip.text = element_text(size=14),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size=0.25)) + 
  scale_x_continuous(breaks = 2002:2018, labels = c("2002","","2004","","2006","","2008","","2010","","2012","","2014","","2016","","2018")) 

pdf("Figures/SST_AI_2018.pdf",width=7.5,height=6)
p4
dev.off()

png("Figures/SST_AI_2018.png",width=7.5,height=6,units="in",res=600)
p4
dev.off()


#----------------------------------------------------
# Combined figure
#----------------------------------------------------

data3 <- mydata %>% 
  mutate(newyr=ifelse(month<4,year-1,year),
         season=ifelse(month%in%c(4:9),"Summer (Apr - Sept)","Winter (Oct - Mar)"),
         monthname=month.name[month],
         flag=ifelse(newyr==2002 & month<4,1,
                     ifelse(newyr==2018 & month>3,1,0)),
         myregion=ifelse(NMFSAREA==610 & as.numeric(STAT_AREA)>635000,"EAI",
                         ifelse(NMFSAREA%in%c(542,543),"WAI",
                                ifelse(NMFSAREA==541,"CAI",
                                       ifelse(NMFSAREA%in%c(620,630,649) | 
                                                (NMFSAREA==610 & as.numeric(STAT_AREA)<635000) | (NMFSAREA==640 & as.numeric(STAT_AREA)>445500),"WGOA",
                                              ifelse(NMFSAREA==650 | (NMFSAREA==640 & STAT_AREA<445600),"EGOA",
                                                     ifelse(NMFSAREA==659,"SEAK Inside",
                                                            ifelse(NMFSAREA%in%c(514,524,508,512,516,509,513,517,518,519) & maxlat<60.1,"SEBS",
                                                                   ifelse(NMFSAREA%in%c(514) & maxlat>60.1,"NBS",NA)))))))))


p5 <- data3 %>% 
  mutate(myregion=fct_relevel(myregion,"NBS","SEBS","WAI","CAI","EAI","WGOA","EGOA","SEAK")) %>% 
  filter(flag==0 & !is.na(myregion)) %>% 
  group_by(myregion,season,newyr) %>% 
  summarise(meantemp=mean(sst.mean,na.rm=TRUE)) %>% 
  ungroup %>% 
  group_by(myregion,season) %>% 
  mutate(tempanom=(meantemp-mean(meantemp,na.rm=TRUE))/sd(meantemp,na.rm=TRUE)) %>% 
  ggplot(aes(x = newyr, y = tempanom, fill=factor(myregion),width=0.65)) +
  geom_bar(stat="identity",position="dodge") + 
  theme_bw() + 
  scale_fill_viridis(name="Ecosystem Region",discrete=TRUE) + 
  geom_hline(yintercept=c(-0.5,0.5),linetype=2) +
  facet_wrap(~season) + 
  xlab("Year") + 
  ylab("Temperature Anomaly") + 
  theme(legend.position="top",
        legend.text=element_text(size=14),
        legend.title=element_text(size=14),
        axis.text.y = element_text(size=13),
        axis.text.x = element_text(size=13,angle=45,vjust=0.5),
        axis.title = element_text(size=14),
        plot.title = element_text(size=16),
        strip.text = element_text(size=14),
        panel.grid = element_blank()) + 
  scale_x_continuous(breaks = 2003:2017, labels = c("2003","","2005","","2007","","2009","","2011","","2013","","2015","","2017"))

pdf("Figures/SST_Combined_2018.pdf",width=7.5,height=5)
p5
dev.off()

png("Figures/SST_Combined_2018.png",width=7.5,height=5,units="in",res=300)
p5
dev.off()




bering <- as.character(c(514,524,508,512,516,509,513,517,518,519))

p1c <- data2 %>% 
  filter(NMFSAREA%in%c(bering) & flag==0 & maxlat<60.1) %>% 
  group_by(NMFSAREA,season,newyr) %>% 
  summarise(meantemp=mean(sst.mean,na.rm=TRUE)) %>% 
  ungroup %>% 
  group_by(NMFSAREA,season) %>% 
  mutate(tempanom=(meantemp-mean(meantemp,na.rm=TRUE))/sd(meantemp,na.rm=TRUE)) %>% 
  ggplot(aes(x = newyr, y = tempanom, fill=factor(NMFSAREA))) +
  geom_bar(stat="identity",position="dodge") + 
  theme_bw() + 
  scale_fill_viridis(name="Bering Sea NMFS Areas",discrete=TRUE) + 
  ylab("") +
  geom_hline(yintercept=c(-0.5,0.5),linetype=2) +
  facet_wrap(~season) + 
  theme(legend.position=c(0.2,0.85),
        legend.background = element_rect(fill=NA),
        legend.key.size = unit(1,"line"),
        axis.text.y = element_text(size=13),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=14),
        plot.title = element_text(size=16),
        strip.text = element_text(size=14),
        panel.grid = element_blank()) + 
  scale_x_continuous(breaks = 2003:2017, labels = rep("",length(2003:2017))) + 
  guides(fill=guide_legend(ncol=5))

p3c <- data2 %>% 
  mutate(mygoa=ifelse(NMFSAREA%in%c(620,630,649) | 
                        (NMFSAREA==610 & as.numeric(STAT_AREA)<635000) | 
                        (NMFSAREA==640 & as.numeric(STAT_AREA)>445500),"WGOA",
                      ifelse(NMFSAREA==650 | 
                               (NMFSAREA==640 & STAT_AREA<445600),"EGOA",
                             ifelse(NMFSAREA==659,"SEAK",NA))),
         mygoa=fct_relevel(mygoa,"WGOA","EGOA","SEAK")) %>% 
  filter(!is.na(mygoa) & flag==0) %>% 
  group_by(mygoa,season,newyr) %>% 
  summarise(meantemp=mean(sst.mean,na.rm=TRUE)) %>% 
  ungroup %>% 
  group_by(mygoa,season) %>% 
  mutate(tempanom=(meantemp-mean(meantemp,na.rm=TRUE))/sd(meantemp,na.rm=TRUE)) %>% 
  ggplot(aes(x = newyr, y = tempanom, fill=factor(mygoa))) +
  geom_bar(stat="identity",position="dodge") + 
  theme_bw() + 
  scale_fill_viridis(name="GOA Region",discrete=TRUE) + 
  geom_hline(yintercept=c(-0.5,0.5),linetype=2) +
  facet_wrap(~season) + 
  ylab("Temperature Anomaly") + 
  theme(legend.position=c(0.18,0.85),
        legend.background = element_rect(fill=NA),
        legend.key.size = unit(1,"line"),        
        axis.text.y = element_text(size=13),
        axis.text.x = element_text(size=13,angle=45,vjust=0.5),
        axis.title.y = element_text(size=14),
        axis.title.x = element_blank(),
        plot.title = element_text(size=16),
        strip.text = element_text(size=14),
        panel.grid = element_blank()) + 
  scale_x_continuous(breaks = 2003:2017, labels = rep("",length(2003:2017))) + 
  guides(fill=guide_legend(ncol=3))


p4c <- data2 %>% 
  mutate(myai=ifelse(NMFSAREA==610 & as.numeric(STAT_AREA)>635000,"EAI",
                     ifelse(NMFSAREA%in%c(542,543),"WAI",
                            ifelse(NMFSAREA==541,"CAI",NA)))) %>% 
  filter(!is.na(myai) & flag==0) %>% 
  group_by(myai,season,newyr) %>% 
  summarise(meantemp=mean(sst.mean,na.rm=TRUE)) %>% 
  ungroup %>% 
  group_by(myai,season) %>% 
  mutate(tempanom=(meantemp-mean(meantemp,na.rm=TRUE))/sd(meantemp,na.rm=TRUE)) %>% 
  ggplot(aes(x = newyr, y = tempanom, fill=factor(myai))) +
  geom_bar(stat="identity",position="dodge") + 
  theme_bw() + 
  scale_fill_viridis(name="Aleutian Region",discrete=TRUE) + 
  geom_hline(yintercept=c(-0.5,0.5),linetype=2) +
  facet_wrap(~season) +
  ylab("") + 
  xlab("Year") + 
  theme(legend.position=c(0.15,0.85),
        legend.background = element_rect(fill=NA),
        legend.key.size = unit(1,"line"),
        axis.text.y = element_text(size=13),
        axis.text.x = element_text(size=13,angle=45,vjust=0.5),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14),
        plot.title = element_text(size=16),
        strip.text = element_text(size=14),
        panel.grid = element_blank()) + 
  scale_x_continuous(breaks = 2003:2017, labels = c("2003","","2005","","2007","","2009","","2011","","2013","","2015","","2017")) + 
  guides(fill=guide_legend(ncol=3))

pdf("Figures/SST_Combined_threepanel_2018.pdf",width=7.5,height=11)
grid.arrange(p1c,p3c,p4c,ncol=1)
dev.off()

png("Figures/SST_Combined_threepanel_2018.png",width=7.5,height=11,units="in",res=300)
grid.arrange(p1c,p3c,p4c,ncol=1)
dev.off()


