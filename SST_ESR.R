library(tidyverse)
library(viridis)
library(rgdal)
 
mydata <- readRDS("mur_SST_stat6_all_columns.RDS") %>% 
  dplyr::select(sst.mean,date,everything())


data2 <- mydata %>% 
  mutate(newyr=ifelse(month<4,year-1,year),
         season=ifelse(month%in%c(4:9),"Summer (Apr - Sept)","Winter (Oct - Mar)"),
         monthname=month.name[month],
         flag=ifelse(newyr==2002 & month<4,1,
                     ifelse(newyr==2018 & month>3,1,0)))

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

bering <- as.character(c(514,524,508,512,516,509,513,517,518,519))

#  The flag field allows us to easily filter out winter 2003 which is incomplete and 
#  summer 2018 which is incomplete

p1 <- data2 %>% 
      filter(NMFSAREA%in%c(bering) & flag==0 & maxlat<60.1) %>% 
      group_by(NMFSAREA,season,newyr) %>% 
      summarise(meantemp=mean(sst.mean,na.rm=TRUE)) %>% 
      ungroup %>% 
      group_by(NMFSAREA,season) %>% 
      mutate(tempanom=(meantemp-mean(meantemp,na.rm=TRUE))/sd(meantemp,na.rm=TRUE)) %>% 
  ggplot(aes(x = newyr, y = tempanom, fill=factor(NMFSAREA))) +
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
      scale_x_continuous(breaks = 2003:2017, labels = c("2003","","2005","","2007","","2009","","2011","","2013","","2015","","2017"))

pdf("SST_EBS_2018.pdf",width=7.5,height=6)
p1
dev.off()

png("SST_EBS_2018.png",width=7.5,height=6,units="in",res=300)
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
  scale_x_continuous(breaks = 2003:2017, labels = c("2003","","2005","","2007","","2009","","2011","","2013","","2015","","2017"))

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
                             ifelse(NMFSAREA==659,"SEAK",NA)))) %>% 
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
    scale_x_continuous(breaks = 2003:2017, labels = c("2003","","2005","","2007","","2009","","2011","","2013","","2015","","2017"))
  
pdf("SST_GOA_2018.pdf",width=7.5,height=6)
p3
dev.off()

png("SST_GOA_2018.png",width=7.5,height=6,units="in",res=300)
p3
dev.off()

#----------------------------------------------------
# Aleutians
#----------------------------------------------------


#  The flag field allows us to easily filter out winter 2003 which is incomplete and 
#  summer 2018 which is incomplete

p4 <- data2 %>% 
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
  scale_x_continuous(breaks = 2003:2017, labels = c("2003","","2005","","2007","","2009","","2011","","2013","","2015","","2017"))

pdf("SST_AI_2018.pdf",width=7.5,height=6)
p4
dev.off()

png("SST_AI_2018.png",width=7.5,height=6,units="in",res=300)
p4
dev.off()
