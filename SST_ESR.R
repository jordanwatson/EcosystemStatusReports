library(tidyverse)
library(viridis)
library(rgdal)
library(forcats)
library(gridExtra)
 

#  **** Jordan - for SSTs, we are going to keep GOA and BSAI separate, but
#  look at the seasons that Nick Bond uses and divide the data into those seasons instead of the 
#  winter/summer seasons that we have now.
#  Autumn (top left) Sept to Nov
#  Winter (top right) Dec to Feb of next year
#  Spring (bottom left) Mar to May
#  Summer (bottom right) Jun to August

#  First load the existing data and see what the last date in the dataset is.
mydata <- readRDS("Data/temperature_data.RDS") %>% 
  left_join(readRDS("Data/date_data.RDS")) %>% 
  left_join(readRDS("Data/spatial_data.RDS")) %>% 
  data.frame 

max(mydata$date)

#-------------------------------------------------------------------------------------------------------------
#  Updating the data
library(DBI)
library(odbc)

#-------------------------------------------------------------------------------------------------------------

# To update the data with more recent dates use the following query. Change the timestamp condition in the SQL
#  based on the value of max(mydata$date) above.
con <- dbConnect(odbc::odbc(), "akfin", UID="jwatson", PWD= rstudioapi::askForPassword("Enter AKFIN Password"))
my_tbl <- dbSendQuery(con,"SELECT * FROM afsc.erddap_sst_stat_area where read_date> timestamp '2019-06-20 09:00:00';")
data <- dbFetch(my_tbl)
#saveRDS(data,file="Data/akfin_query_since_03022019.rds")
dbDisconnect(con)

#  Make the data conform with the current data (which go through May 10, 2018)
newdata <- data %>% 
  dplyr::select(-c(AREA_INDEX,USERNAME,AKFIN_LOAD_DATE,POINTS_COUNT)) %>% 
  rename(date=READ_DATE,sst.mean=TEMP,sst.sd=S_DEV) %>% 
  mutate(date=as.Date(date))

new_date <- newdata %>% 
  dplyr::select(date) %>% 
  distinct() %>% 
  mutate(year=as.numeric(format(date,"%Y")),
         month=as.numeric(format(date,"%m")),
         julian=as.POSIXlt(date)$yday+1,
         week=as.numeric(format(date,"%U"))+1) %>% 
  arrange(date)

#  The following lines are commented out to avoid accidentally over-writing the data files
#saveRDS(bind_rows(readRDS("Data/temperature_data.RDS"),newdata) %>% distinct(),file="Data/temperature_data.RDS")
#saveRDS(bind_rows(readRDS("Data/date_data.RDS"),new_date) %>% distinct(),file="Data/date_data.RDS")
#-------------------------------------------------------------------------------------------------------------

mydata <- readRDS("Data/temperature_data.RDS") %>% 
  left_join(readRDS("Data/date_data.RDS")) %>% 
  left_join(readRDS("Data/spatial_data.RDS")) %>% 
  data.frame 

#  If you need to omit certain months due to incompleteness, modify the ifelse in "flag"
#data2 <- mydata %>% 
#  mutate(newyr=ifelse(month<4,year-1,year),
#         season=ifelse(month%in%c(4:9),"Summer (Apr - Sept)","Winter (Oct - Mar)"),
#         monthname=month.name[month],
#         flag=ifelse(newyr==2002 & month<4,1,
#                     ifelse(newyr==2018 & month>3,1,0)))


data2 <- mydata %>% 
  filter(!is.na(sst.mean)) %>% 
  mutate(newyr=ifelse(month==12,year+1,year),
         season=ifelse(month%in%c(9:11),"Sept-Nov",
                       ifelse(month%in%c(12,1,2),"Dec-Feb",
                              ifelse(month%in%c(3:5),"Mar-May","Jun-Aug"))))


#data2 <- mydata %>% 
#  filter(!is.na(sst.mean)) %>% 
#  mutate(newyr=ifelse(month<4,year-1,year),
#         season=ifelse(month%in%c(4:9),"Summer (Apr - Sept)","Winter (Oct - Mar)"),
#         monthname=month.name[month],
#         flag=0)



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

bering <- as.character(c(514,524,508,512,516,509,513,517,521))

p1 <- data2 %>% 
  filter(NMFSAREA%in%c(bering) & !is.na(NMFSAREA)) %>% 
  mutate(NMFSAREA=ifelse(maxlat>60.1 & maxlat<65.6,"NBS",NMFSAREA),
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
  scale_fill_viridis(name="NMFS Area",discrete=TRUE) + 
  geom_hline(yintercept=c(-0.5,0.5),linetype=2) +
  facet_wrap(~season) + 
  xlab("Year") + 
  ylab("Temperature Anomaly (C)") + 
  theme(legend.position="top",
        legend.text=element_text(size=13),
        legend.title=element_text(size=13),
        axis.text.y = element_text(size=12),
        #axis.text.x = element_text(size=12,angle=45,vjust=0.5),
        axis.title = element_text(size=13),
        plot.title = element_text(size=15),
        strip.text = element_text(size=13),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size=0.25)) + 
  scale_x_continuous(breaks = 2002:2019, labels = c("2002","","2004","","2006","","2008","","2010","","2012","","2014","","2016","","2018",""),expand=c(0.01,0.01)) + 
  guides(fill=guide_legend(ncol=6))

x11();p1
x11();p2

p2 <- data2 %>% 
  filter(NMFSAREA%in%c(bering) & !is.na(NMFSAREA)) %>% 
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
  scale_fill_viridis(name="NMFS Area",discrete=TRUE) + 
  geom_hline(yintercept=c(-0.5,0.5),linetype=2) +
  facet_wrap(~season) + 
  xlab("Year") + 
  ylab("Temperature Anomaly (C)") + 
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
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size=0.25)) + 
  scale_x_continuous(breaks = 2003:2019, labels = c("2003","","","2006","","","2009","","","2012","","","2015","","","2018",""),expand=c(0.01,0.01)) + 
  guides(fill=guide_legend(ncol=2))



p3 <- data2 %>% 
  filter(NMFSAREA%in%c(bering) & !is.na(NMFSAREA)) %>% 
  mutate(NMFSAREA=ifelse(maxlat>60.1 & maxlat<65.6,"NBS","EBS"),
         season=fct_relevel(season,
                            "Sept-Nov",
                            "Dec-Feb",
                            "Mar-May",
                            "Jun-Aug"),
         m.depth=abs(m.depth),
         region=ifelse(m.depth<50,"Inner",
                       ifelse(m.depth>=50 & m.depth<100,"Middle",
                              ifelse(m.depth>100 & m.depth<200,"Outer",NA))),
         mynew=paste0(NMFSAREA,"_",region)) %>% 
  filter(m.depth<200) %>% 
  group_by(mynew,season,newyr) %>% 
  summarise(meantemp=mean(sst.mean,na.rm=TRUE)) %>% 
  ungroup %>% 
  group_by(mynew,season) %>% 
  mutate(tempanom=(meantemp-mean(meantemp,na.rm=TRUE))) %>% 
  #mutate(tempanom=(meantemp-mean(meantemp,na.rm=TRUE))/sd(meantemp,na.rm=TRUE)) %>% 
  ggplot(aes(x = newyr, y = tempanom, fill=factor(mynew),width=0.75)) +
  geom_bar(stat="identity",position="dodge") + 
  theme_bw() + 
  scale_fill_viridis(name="Area",discrete=TRUE) + 
  geom_hline(yintercept=c(-0.5,0.5),linetype=2) +
  facet_wrap(~season) + 
  xlab("Year") + 
  ylab("Temperature Anomaly (C)") + 
  theme(legend.position="top",
        legend.text=element_text(size=13),
        legend.title=element_text(size=13),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12,angle=45,vjust=0.5),
        axis.title = element_text(size=13),
        plot.title = element_text(size=15),
        strip.text = element_text(size=13),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size=0.25)) + 
  scale_x_continuous(breaks = 2003:2019, labels = c("2003","","","2006","","","2009","","","2012","","","2015","","","2018",""),expand=c(0.01,0.01)) + 
  #scale_x_continuous(breaks = 2002:2019, labels = c("2002","","2004","","2006","","2008","","2010","","2012","","2014","","2016","","2018",""),expand=c(0.01,0.01)) + 
  guides(fill=guide_legend(ncol=6))

pdf("Figures/SST_EBS_2019_EBS.pdf",width=7.5,height=6)
p2
dev.off()

pdf("Figures/SST_EBS_2019_domains.pdf",width=7.5,height=6)
p3
dev.off()

pdf("Figures/SST_EBS_2019.pdf",width=7.5,height=6)
p1
dev.off()

png("Figures/SST_EBS_2019.png",width=7.5,height=6,units="in",res=300)
p1
dev.off()

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
            panel.grid.minor = element_blank(),
            panel.grid.major = element_line(size=0.25)) + 
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
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size=0.25)) + 
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
                             ifelse(NMFSAREA==659,"SEAK Inside",NA))),
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
  mutate(myai=ifelse((NMFSAREA==610 & as.numeric(STAT_AREA)>= 645000) | NMFSAREA%in%c(518,519),"EAI",
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


