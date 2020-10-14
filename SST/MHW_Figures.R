
#------------------------------------------------------------------------------------------
#  Setup
#------------------------------------------------------------------------------------------
library(tidyverse)
library(doParallel) 
library(heatwaveR)
library(lubridate)
library(viridis)

#  Load 508 compliant NOAA colors
OceansBlue1='#0093D0'
OceansBlue2='#0055A4' # rebecca dark blue
CoralRed1='#FF4438'
Crustacean1='#FF8300'
SeagrassGreen1='#93D500'
SeagrassGreen4='#D0D0D0' # This is just grey
UrchinPurple1='#7F7FFF'
WavesTeal1='#1ECAD3'

# Set line colours
lineColCat <- c(
  "Temperature" = "black",
  "Climatology" = "gray20",
  "Moderate" = "darkgreen",
  "Strong" = "darkgreen",
  "Severe" = "darkgreen",
  "Extreme" = "darkgreen"
)

# Set category fill colours
fillColCat <- c(
  "Moderate" = "#ffc866",
  "Strong" = "#ff6900",
  "Severe" = "#9e0000",
  "Extreme" = "#2d0000"
)
mytheme <- theme(strip.text = element_text(size=10,color="white",family="sans",face="bold"),
                 strip.background = element_rect(fill=OceansBlue2),
                 axis.title = element_text(size=10,family="sans"),
                 axis.text = element_text(size=10,family="sans"),
                 panel.border=element_rect(colour="black",fill=NA,size=0.5),
                 panel.background = element_blank(),
                 legend.position=c(0.6,0.7),
                 legend.background = element_blank(),
                 legend.box.background = element_blank(),
                 legend.key.size = unit(1,"line"))

#------------------------------------------------------------------------------------------
#  End Setup
#------------------------------------------------------------------------------------------

mhw_cat <- category(detect_event(ts2clm(readRDS("Data/crwsst_goa_19850401_through_Sept.RDS") %>%
                                  filter(Ecosystem_sub=="Western Gulf of Alaska") %>% 
                                  rename(t=date,temp=meansst) %>% 
                                  arrange(t), climatologyPeriod = c("1986-01-01", "2015-12-31"))),S=FALSE) %>% 
  mutate(region="Western Gulf of Alaska") %>% 
  bind_rows(category(detect_event(ts2clm(readRDS("Data/crwsst_goa_19850401_through_Sept.RDS") %>%
                                   filter(Ecosystem_sub=="Eastern Gulf of Alaska") %>% 
                                   rename(t=date,temp=meansst) %>% 
                                   arrange(t), climatologyPeriod = c("1986-01-01", "2015-12-31"))),S=FALSE) %>% 
              mutate(region="Eastern Gulf of Alaska"))

eventsum <- mhw_cat %>%
  mutate(year=year(peak_date),
         mycat=case_when(
           category=="I Moderate" ~ 1,
           category=="II Strong" ~ 2,
           category=="III Severe" ~ 3,
           category=="IV Extreme" ~ 4)) %>%
  group_by(year,region) %>%
  summarise(category=weighted.mean(mycat, duration),
            duration=sum(duration))

annualevents <- eventsum %>% 
  right_join(data.frame(year=1985:2020)) %>% 
  filter(!is.na(region)) %>% 
  arrange(year) %>% 
  replace_na(list(events=0,duration=0)) 

mycolvec <- c("#ffc866","#ff6900","#9e0000","#2d0000")

# Create figure that summarizes history of marine heatwaves
png("SST_ESR/2020/GOA/MHW_barplot.png",width=7,height=5,units="in",res=300)
annualevents %>% 
  mutate(region=fct_rev(region)) %>% 
  ggplot(aes(year,duration,fill=category)) +
  geom_bar(stat="identity") + 
  scale_fill_gradientn(colors=mycolvec, breaks=c(1,2,3,4),limits=c(0,4),labels=c("Moderate","Strong","Severe","Extreme"),name="") +
  #scale_fill_gradient(name = "Intensity", low="#ffc866",high="#2d0000") +
  mytheme + 
  xlab("Year") + 
  ylab("Total number of MHW days") + 
  facet_wrap(~region,ncol=1) + 
  #scale_x_continuous(limits=c(1985,2020)) + 
  scale_y_continuous(expand=c(0,0),limits=c(0,max(annualevents$duration*1.05))) + 
  theme(legend.position=c(0.1,0.8),
        axis.title.x=element_blank(),
        plot.margin=unit(c(0.1,0,0.1,0),"cm"))
dev.off()

#  Create figure that shows MHW status
mhw <- (detect_event(ts2clm(readRDS("Data/crwsst_goa_19850401_through_Sept.RDS") %>%
                             filter(Ecosystem_sub=="Western Gulf of Alaska") %>% 
                             rename(t=date,temp=meansst) %>% 
                             arrange(t), climatologyPeriod = c("1986-01-01", "2015-12-31"))))$clim %>% 
  mutate(region="Western Gulf of Alaska") %>% 
  bind_rows((detect_event(ts2clm(readRDS("Data/crwsst_goa_19850401_through_Sept.RDS") %>%
                                  filter(Ecosystem_sub=="Eastern Gulf of Alaska") %>% 
                                  rename(t=date,temp=meansst) %>% 
                                  arrange(t), climatologyPeriod = c("1986-01-01", "2015-12-31"))))$clim %>% 
              mutate(region="Eastern Gulf of Alaska"))


clim_cat <- mhw %>%
  mutate(region=fct_relevel(region,"Western Gulf of Alaska")) %>% 
  group_by(region) %>% 
  dplyr::mutate(diff = thresh - seas,
                thresh_2x = thresh + diff,
                thresh_3x = thresh_2x + diff,
                thresh_4x = thresh_3x + diff,
                year=year(t))

# Set line colours
lineColCat <- c(
  "Temperature" = "black",
  "Climatology" = "gray20",
  "Moderate" = "gray60",
  "Strong" = "gray60",
  "Severe" = "gray60",
  "Extreme" = "gray60"
)

png("SST_ESR/2020/GOA/MHW_status.png",width=7,height=5,units="in",res=300)
ggplot(data = clim_cat %>% filter(t>=as.Date("2017-12-01")), aes(x = t, y = temp)) +
  geom_flame(aes(y2 = thresh, fill = "Moderate")) +
  geom_flame(aes(y2 = thresh_2x, fill = "Strong")) +
  geom_flame(aes(y2 = thresh_3x, fill = "Severe")) +
  geom_flame(aes(y2 = thresh_4x, fill = "Extreme")) +
  geom_line(aes(y = thresh_2x, col = "Strong"), size = 0.5, linetype = "dotted") +
  geom_line(aes(y = thresh_3x, col = "Severe"), size = 0.5, linetype = "dotted") +
  geom_line(aes(y = thresh_4x, col = "Extreme"), size = 0.5, linetype = "dotted") +
  geom_line(aes(y = seas, col = "Climatology"), size = 0.5) +
  geom_line(aes(y = thresh, col = "Moderate"), size = 0.5,linetype= "dotted") +
  geom_line(aes(y = temp, col = "Temperature"), size = 0.5) +
  scale_colour_manual(name = NULL, values = lineColCat,
                      breaks = c("Temperature", "Climatology", "Moderate",
                                 "Strong", "Severe", "Extreme")) +
  scale_fill_manual(name = NULL, values = fillColCat, guide = FALSE) +
  scale_x_date(date_labels = "%b %Y",expand=c(0.01,0)) +
  #guides(colour = guide_legend(override.aes = list(linetype = c("dashed", "dotted", "solid"),
  #                                                 size = c(0.6, 0.7, 0.7)),
  #                             ncol=3)) +
  guides(colour = guide_legend(override.aes = list(linetype = c("solid", "solid", "dotted",
                                                                "dotted", "dotted", "dotted"),
                                                   size = c(0.6, 0.7, 0.7, 0.7, 0.7, 0.7)),
                               ncol=6)) +
  labs(y = "Sea Surface Temperature (°C)", x = NULL) + 
  theme(legend.position="none") +
  facet_wrap(~region,ncol=1,scales="free_y") +
  mytheme + 
  theme(#legend.position="top",
        legend.key=element_blank(),
        legend.text = element_text(size=9),
        axis.title.x=element_blank(),
        legend.margin=margin(l=-2.75,t = -8.5, unit='cm'),
        plot.margin=unit(c(0.65,0,0.0,0),"cm"))
dev.off()




#----------------------------------------------------------------------------------------------------------
#Bering Sea
#----------------------------------------------------------------------------------------------------------

mhw_cat <- category(detect_event(ts2clm(readRDS("Data/crwsst_bering_19850401_through_Sept.RDS") %>%
                                          filter(Ecosystem_sub=="Northern Bering Sea") %>% 
                                          rename(t=date,temp=meansst) %>% 
                                          arrange(t), climatologyPeriod = c("1986-01-01", "2015-12-31"))),S=FALSE) %>% 
  mutate(region="Northern Bering Sea") %>% 
  bind_rows(category(detect_event(ts2clm(readRDS("Data/crwsst_bering_19850401_through_Sept.RDS") %>%
                                           filter(Ecosystem_sub=="Southeastern Bering Sea") %>% 
                                           rename(t=date,temp=meansst) %>% 
                                           arrange(t), climatologyPeriod = c("1986-01-01", "2015-12-31"))),S=FALSE) %>% 
              mutate(region="Southeastern Bering Sea"))

eventsum <- mhw_cat %>%
  mutate(year=year(peak_date),
         mycat=case_when(
           category=="I Moderate" ~ 1,
           category=="II Strong" ~ 2,
           category=="III Severe" ~ 3,
           category=="IV Extreme" ~ 4)) %>%
  group_by(year,region) %>%
  summarise(category=weighted.mean(mycat, duration),
            duration=sum(duration))

annualevents <- eventsum %>% 
  right_join(data.frame(year=1985:2020)) %>% 
  filter(!is.na(region)) %>% 
  arrange(year) %>% 
  replace_na(list(events=0,duration=0)) 

mycolvec <- c("#ffc866","#ff6900","#9e0000","#2d0000")

# Create figure that summarizes history of marine heatwaves
png("SST_ESR/2020/EBS/MHW_barplot.png",width=7,height=5,units="in",res=300)
annualevents %>% 
  #mutate(region=fct_rev(region)) %>% 
  ggplot(aes(year,duration,fill=category)) +
  geom_bar(stat="identity") + 
  scale_fill_gradientn(colors=mycolvec, breaks=c(1,2,3,4),limits=c(0,4),labels=c("Moderate","Strong","Severe","Extreme"),name="") +
  #scale_fill_gradient(name = "Intensity", low="#ffc866",high="#2d0000") +
  mytheme + 
  xlab("Year") + 
  ylab("Total number of MHW days") + 
  facet_wrap(~region,ncol=1) + 
  #scale_x_continuous(limits=c(1985,2020)) + 
  scale_y_continuous(expand=c(0,0),limits=c(0,max(annualevents$duration*1.05))) + 
  theme(legend.position=c(0.1,0.8),
        axis.title.x=element_blank())
dev.off()

#  Create figure that shows MHW status
mhw <- (detect_event(ts2clm(readRDS("Data/crwsst_bering_19850401_through_Sept.RDS") %>%
                              filter(Ecosystem_sub=="Northern Bering Sea") %>% 
                              rename(t=date,temp=meansst) %>% 
                              arrange(t), climatologyPeriod = c("1986-01-01", "2015-12-31"))))$clim %>% 
  mutate(region="Northern Bering Sea") %>% 
  bind_rows((detect_event(ts2clm(readRDS("Data/crwsst_bering_19850401_through_Sept.RDS") %>%
                                   filter(Ecosystem_sub=="Southeastern Bering Sea") %>% 
                                   rename(t=date,temp=meansst) %>% 
                                   arrange(t), climatologyPeriod = c("1986-01-01", "2015-12-31"))))$clim %>% 
              mutate(region="Southeastern Bering Sea"))


clim_cat <- mhw %>%
  #mutate(region=fct_relevel(region,"Western Gulf of Alaska")) %>% 
  group_by(region) %>% 
  dplyr::mutate(diff = thresh - seas,
                thresh_2x = thresh + diff,
                thresh_3x = thresh_2x + diff,
                thresh_4x = thresh_3x + diff,
                year=year(t))


png("SST_ESR/2020/EBS/MHW_status.png",width=7,height=5,units="in",res=300)
ggplot(data = clim_cat %>% filter(t>=as.Date("2017-09-01")), aes(x = t, y = temp)) +
  geom_flame(aes(y2 = thresh, fill = "Moderate")) +
  geom_flame(aes(y2 = thresh_2x, fill = "Strong")) +
  geom_flame(aes(y2 = thresh_3x, fill = "Severe")) +
  geom_flame(aes(y2 = thresh_4x, fill = "Extreme")) +
  geom_line(aes(y = thresh_2x, col = "Strong"), size = 0.5, linetype = "dashed") +
  geom_line(aes(y = thresh_3x, col = "Severe"), size = 0.5, linetype = "dotdash") +
  geom_line(aes(y = thresh_4x, col = "Extreme"), size = 0.5, linetype = "dotted") +
  geom_line(aes(y = seas, col = "Climatology"), size = 0.5) +
  geom_line(aes(y = thresh, col = "Moderate"), size = 0.5) +
  geom_line(aes(y = temp, col = "Temperature"), size = 0.6) +
  scale_colour_manual(name = NULL, values = lineColCat,
                      breaks = c("Temperature", "Climatology", "Moderate",
                                 "Strong", "Severe", "Extreme")) +
  #scale_colour_manual(name = NULL, values = lineColCat,
  #                    breaks = c("Temperature", "Climatology", "Threshold",
  #                               "2x Threshold", "3x Threshold", "4x Threshold")) +
  scale_fill_manual(name = NULL, values = fillColCat, guide = FALSE) +
  scale_x_date(date_labels = "%b %Y",expand=c(0.01,0)) +
  #scale_y_continuous(limits=c(min(temp)*0.95,max(temp)*1.05)) +
  guides(colour = guide_legend(override.aes = list(linetype = c("solid", "solid", "solid",
                                                                "dashed", "dotdash", "dotted"),
                                                   size = c(0.6, 0.7, 0.7, 0.7, 0.7, 0.7)),
                               ncol=6)) +
  labs(y = "Temperature [°C]", x = NULL) + 
  theme(legend.position="none") +
  facet_wrap(~region,ncol=1,scales="free_y") +
  mytheme + 
  theme(#legend.position="top",
    legend.key=element_blank(),
    legend.text = element_text(size=9),
    axis.title.x=element_blank(),
    legend.margin=margin(l=-2.75,t = -8.5, unit='cm'),
    plot.margin=unit(c(0.65,0,0.0,0),"cm"))
dev.off()

#----------------------------------------------------------------------------------------------------------
# Aleutian Islands
#----------------------------------------------------------------------------------------------------------

mhw_cat <- category(detect_event(ts2clm(readRDS("Data/crwsst_ai_19850401_through_Sept.RDS") %>%
                                          filter(Ecosystem_sub=="Western Aleutians") %>% 
                                          rename(t=date,temp=meansst) %>% 
                                          arrange(t), climatologyPeriod = c("1986-01-01", "2015-12-31"))),S=FALSE) %>% 
  mutate(region="Western Aleutians") %>% 
  bind_rows(category(detect_event(ts2clm(readRDS("Data/crwsst_ai_19850401_through_Sept.RDS") %>%
                                           filter(Ecosystem_sub=="Central Aleutians") %>% 
                                           rename(t=date,temp=meansst) %>% 
                                           arrange(t), climatologyPeriod = c("1986-01-01", "2015-12-31"))),S=FALSE) %>% 
              mutate(region="Central Aleutians")) %>% 
  bind_rows(category(detect_event(ts2clm(readRDS("Data/crwsst_ai_19850401_through_Sept.RDS") %>%
                                           filter(Ecosystem_sub=="Eastern Aleutians") %>% 
                                           rename(t=date,temp=meansst) %>% 
                                           arrange(t), climatologyPeriod = c("1986-01-01", "2015-12-31"))),S=FALSE) %>% 
              mutate(region="Eastern Aleutians"))

eventsum <- mhw_cat %>%
  mutate(year=year(peak_date),
         mycat=case_when(
           category=="I Moderate" ~ 1,
           category=="II Strong" ~ 2,
           category=="III Severe" ~ 3,
           category=="IV Extreme" ~ 4)) %>%
  group_by(year,region) %>%
  summarise(category=weighted.mean(mycat, duration),
            duration=sum(duration))

annualevents <- eventsum %>% 
  right_join(data.frame(year=1985:2020)) %>% 
  filter(!is.na(region)) %>% 
  arrange(year) %>% 
  replace_na(list(events=0,duration=0)) %>% 
  data.frame

mycolvec <- c("#ffc866","#ff6900","#9e0000","#2d0000")

mytheme3 <- theme(strip.text = element_text(size=10,color="white",family="sans",face="bold"),
                 strip.background = element_rect(fill=OceansBlue2),
                 axis.title = element_text(size=10,family="sans"),
                 axis.text = element_text(size=10,family="sans"),
                 panel.border=element_rect(colour="black",fill=NA,size=0.5),
                 panel.background = element_blank(),
                 #plot.margin=unit(c(0.1,0,0.1,0),"cm"),
                 legend.background = element_blank(),
                 legend.box.background = element_blank(),
                 legend.key.size = unit(0.75,"line"))

# Create figure that summarizes history of marine heatwaves
png("SST_ESR/2020/AI/MHW_barplot.png",width=7,height=5,units="in",res=300)
annualevents %>% 
  mutate(region=fct_relevel(region,"Western Aleutians","Central Aleutians")) %>% 
  ggplot(aes(year,duration,fill=category)) +
  geom_bar(stat="identity") + 
  scale_fill_gradientn(colors=mycolvec, breaks=c(1,2,3,4),limits=c(0,4),labels=c("Moderate","Strong","Severe","Extreme"),name="") +
  #scale_fill_gradient(name = "Intensity", low="#ffc866",high="#2d0000") +
  mytheme3 + 
  xlab("Year") + 
  ylab("Total number of MHW days") + 
  facet_wrap(~region,ncol=1) + 
  #scale_x_continuous(limits=c(1985,2020)) + 
  scale_y_continuous(expand=c(0,0),limits=c(0,max(annualevents$duration*1.05))) + 
  theme(legend.position=c(0.1,0.9),
        axis.title.x=element_blank())
dev.off()



mhw <- (detect_event(ts2clm(readRDS("Data/crwsst_ai_19850401_through_Sept.RDS") %>%
                                          filter(Ecosystem_sub=="Western Aleutians") %>% 
                                          rename(t=date,temp=meansst) %>% 
                                          arrange(t), climatologyPeriod = c("1986-01-01", "2015-12-31"))))$clim %>% 
  mutate(region="Western Aleutians") %>% 
  bind_rows((detect_event(ts2clm(readRDS("Data/crwsst_ai_19850401_through_Sept.RDS") %>%
                                           filter(Ecosystem_sub=="Central Aleutians") %>% 
                                           rename(t=date,temp=meansst) %>% 
                                           arrange(t), climatologyPeriod = c("1986-01-01", "2015-12-31"))))$clim %>% 
              mutate(region="Central Aleutians")) %>% 
  bind_rows((detect_event(ts2clm(readRDS("Data/crwsst_ai_19850401_through_Sept.RDS") %>%
                                           filter(Ecosystem_sub=="Eastern Aleutians") %>% 
                                           rename(t=date,temp=meansst) %>% 
                                           arrange(t), climatologyPeriod = c("1986-01-01", "2015-12-31"))))$clim %>% 
              mutate(region="Eastern Aleutians"))

clim_cat <- mhw %>%
  mutate(region=fct_relevel(region,"Western Aleutians","Central Aleutians")) %>% 
  group_by(region) %>% 
  dplyr::mutate(diff = thresh - seas,
                thresh_2x = thresh + diff,
                thresh_3x = thresh_2x + diff,
                thresh_4x = thresh_3x + diff,
                year=year(t))


png("SST_ESR/2020/AI/MHW_status.png",width=7,height=5,units="in",res=300)
ggplot(data = clim_cat %>% filter(t>=as.Date("2017-12-01")), aes(x = t, y = temp)) +
  geom_flame(aes(y2 = thresh, fill = "Moderate")) +
  geom_flame(aes(y2 = thresh_2x, fill = "Strong")) +
  geom_flame(aes(y2 = thresh_3x, fill = "Severe")) +
  geom_flame(aes(y2 = thresh_4x, fill = "Extreme")) +
  geom_line(aes(y = thresh_2x, col = "Strong"), size = 0.35, linetype = "dashed") +
  geom_line(aes(y = thresh_3x, col = "Severe"), size = 0.35, linetype = "dotdash") +
  geom_line(aes(y = thresh_4x, col = "Extreme"), size = 0.35, linetype = "dotted") +
  geom_line(aes(y = seas, col = "Climatology"), size = 0.35) +
  geom_line(aes(y = thresh, col = "Moderate"), size = 0.35) +
  geom_line(aes(y = temp, col = "Temperature"), size = 0.35) +
  scale_colour_manual(name = NULL, values = lineColCat,
                      breaks = c("Temperature", "Climatology", "Moderate",
                                 "Strong", "Severe", "Extreme")) +
  scale_fill_manual(name = NULL, values = fillColCat, guide = FALSE) +
  scale_x_date(date_labels = "%b %Y",expand=c(0.01,0)) +
  #scale_y_continuous(limits=c(min(temp)*0.95,max(temp)*1.05)) +
  guides(colour = guide_legend(override.aes = list(linetype = c("solid", "solid", "solid",
                                                                "dashed", "dotdash", "dotted"),
                                                   size = c(0.6, 0.7, 0.7, 0.7, 0.7, 0.7)),
                               ncol=6)) +
  labs(y = "Temperature [°C]", x = NULL) + 
  theme(legend.position="none") +
  facet_wrap(~region,ncol=1,scales="free_y") +
  mytheme + 
  theme(#legend.position="top",
    legend.key=element_blank(),
    legend.text = element_text(size=9),
    axis.title.x=element_blank(),
    legend.margin=margin(l=-2.75,t = -8.5, unit='cm'),
    plot.margin = unit(c(0.5,0,0,0),"cm"))
dev.off()
