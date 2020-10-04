#  Create daily SST plot (ESR Figure 1) and time series decomposition (trend) (ESR Figure 2) for Ecosystem Status Report
#  The example here is for the GOA but the only change for the Bering Sea and the Aleutian Islands
#  is the data file and the input names for the function.

#  Author: Jordan Watson

#  Data sources: NOAA Coral Reef Watch SST, courtesy of CoastWatch West Coast Node and NESDIS

#  Load packages
library(tidyverse)
library(lubridate)
library(cowplot)
library(magick)

#  Load 508 compliant NOAA colors
OceansBlue1='#0093D0'
OceansBlue2='#0055A4' # dark blue
Crustacean1='#FF8300'
SeagrassGreen4='#D0D0D0' # This is just grey

#  Assign colors to different time series.
current.year.color <- Crustacean1
last.year.color <- OceansBlue1
mean.color <- "black"

#  Set default plot theme
theme_set(theme_cowplot())

#  Specify legend position coordinates
mylegx <- 0.525
mylegy <- 0.865

#  Read in and reformat the temperature time series for the GOA. These data will eventually be queryable from a webAPI but 
#  meanwhile, contact the author and he will share them.
data <- readRDS("Data/crwsst_goa_19850401_through_Sept.RDS") %>% 
  data.frame %>% 
  rename_all(tolower) %>% 
  mutate(read_date=as_date(date),
         ecosystem_sub=factor(ecosystem_sub),
         esr_region=fct_rev(ecosystem_sub),
         month=month(read_date),
         day=day(read_date),
         year=year(read_date),
         newdate=as.Date(ifelse(month==12,as.character(as.Date(paste("1999",month,day,sep="-"),format="%Y-%m-%d")),
                                as.character(as.Date(paste("2000",month,day,sep="-"),format="%Y-%m-%d"))),format("%Y-%m-%d")),
         year2=ifelse(month==12,year+1,year)) %>% 
  arrange(read_date) 

#  Set year criteria to automatically identify the current and previous years
current.year <- max(data$year)
last.year <- current.year-1
mean.years <- 1986:2015
mean.lab <- "Mean 1986-2015"


#  Create plotting function that will allow selection of 2 ESR regions
myplotfun <- function(region1,region2){
  mylines_base <- ggplot() +
    geom_line(data=data %>% filter(year2<last.year & esr_region%in%(c(region1,region2))),
              aes(newdate,meansst,group=factor(year2),col='mygrey'),size=0.3) +
    geom_line(data=data %>% filter(year2==last.year & esr_region%in%(c(region1,region2))),
              aes(newdate,meansst,color='last.year.color'),size=0.75) +
    geom_line(data=data %>% 
                filter(year%in%mean.years & esr_region%in%(c(region1,region2))) %>% 
                group_by(esr_region,newdate) %>% 
                summarise(meantemp=mean(meansst,na.rm=TRUE)),
              aes(newdate,meantemp,col='mean.color'),size=0.5) +
    geom_line(data=data %>% filter(year2==current.year & esr_region%in%(c(region1,region2))),
              aes(newdate,meansst,color='current.year.color'),size=0.95) +
    facet_wrap(~esr_region,ncol=2) + 
    scale_color_manual(name="",
                       breaks=c('current.year.color','last.year.color','mygrey','mean.color'),
                       values=c('current.year.color'=current.year.color,'last.year.color'=last.year.color,'mygrey'=SeagrassGreen4,'mean.color'=mean.color),
                       labels=c(current.year,last.year,paste0('1985-',last.year-1),mean.lab)) +
    ylab("Mean Sea Surface Temperature (C)") + 
    xlab("") +
    scale_x_date(date_breaks="1 month",
                 date_labels = "%b",
                 expand = c(0.025,0.025)) + 
    theme(legend.position=c(mylegx,mylegy),
          legend.text = element_text(size=8,family="sans"),
          legend.background = element_blank(),
          legend.title = element_blank(),
          strip.text = element_text(size=10,color="white",family="sans",face="bold"),
          strip.background = element_rect(fill=OceansBlue2),
          axis.title = element_text(size=10,family="sans"),
          axis.text = element_text(size=10,family="sans"),
          panel.border=element_rect(colour="black",size=0.75),
          axis.text.x=element_text(color=c("black",NA,NA,"black",NA,NA,"black",NA,NA,"black",NA,NA,NA)),
          legend.key.size = unit(0.35,"cm"),
          plot.margin=unit(c(0.65,0,0.65,0),"cm")) 
  
  ggdraw(mylines_base) #+
    #draw_image("fisheries_header_logo_jul2019.png",scale=0.2,x=mylogox,y=mylogoy,hjust=0.35) +
    #annotate("text",x=0.115,y=0.085,label=paste0("Contact: Jordan.Watson@noaa.gov, Alaska Fisheries Science Center, NOAA Fisheries (data: JPL MUR SST, ",format(Sys.Date(),"%m-%d-%Y"),")"),
    #         hjust=0.1,size=2.59,family="sans",fontface=2,color=OceansBlue2)
    #annotate("text",x=0.14,y=0.055,label=paste0("                                          Data: NOAA Coral Reef Watch SST, courtesy of PacIOOS and NESDIS\n           Contact: Jordan.Watson@noaa.gov, Alaska Fisheries Science Center, NOAA Fisheries (Updated: ",format(Sys.Date(),"%m-%d-%Y"),")\n             Data are modeled satellite products and periodic discrepancies may exist across sensors and products."),
    #         hjust=0.1,size=2.57,family="sans",fontface=2,color=OceansBlue2)
}


#  Generate figure 1 output 
#jpeg(paste0("SST_ESR/2020/GOA/SST_GOA_ESR_",format(Sys.Date(),"%Y_%m_%d"),".jpeg"),width=6,height=4,units="in",quality=100,res=300)
myplotfun("Western Gulf of Alaska","Eastern Gulf of Alaska")
#myplotfun("Western Aleutians","Central Aleutians","Eastern Aleutians")
#myplotfun("Northering Bering Sea","Southeastern Bering Sea")

#dev.off()


# -------------------------------------------------------------------------------
#  Perform time series decomposition and create figure 2 from ESR
# -------------------------------------------------------------------------------

#devtools::install_github("brisneve/ggplottimeseries")
library(ggplottimeseries)

#  The following could all be combined but I have left it separated out to be more transparent.
egoa <- data %>% 
  filter(ecosystem_sub=="Eastern Gulf of Alaska")
  

#  Perform the time series decomposition for the EGOA, setting the frequency as 365.25 because we have daily data with leap years.
dfegoa <- dts1(egoa$read_date,egoa$meansst,365.25, type = "additive") %>% 
  mutate(ecosystem_sub="Eastern Gulf of Alaska",
         year=year(date))

#  Repeat for the wgoa
wgoa <- data %>% 
  filter(ecosystem_sub=="Western Gulf of Alaska")

dfwgoa <- dts1(wgoa$read_date,wgoa$meansst,365.25, type = "additive") %>% 
  mutate(ecosystem_sub="Western Gulf of Alaska",
         year=year(date))

#  Combine the time series decompositions for each area and reorder the factors.
df <- dfegoa %>% 
  bind_rows(dfwgoa) %>% 
  mutate(ecosystem_sub=fct_relevel(ecosystem_sub,"Western Gulf of Alaska"))

#  Create the horizontal mean and sd lines for the 30 year baseline period.
dfmean <- df %>% 
  group_by(ecosystem_sub) %>% 
  summarise(meantrend=mean(trend[between(year,1986,2015)],na.rm=TRUE),
            sdtrend=sd(trend[between(year,1986,2015)],na.rm=TRUE))


#jpeg(paste0("SST_ESR/2020/GOA/SST_GOA_ESR_TimeSeriesTrend",format(Sys.Date(),"%Y_%m_%d"),".jpeg"),width=6,height=4,units="in",quality=100,res=300)
df %>% 
  ggplot(aes(x = date, y = trend)) + 
  geom_line() + 
  geom_hline(data=dfmean,aes(yintercept=meantrend),linetype=2) +
  geom_hline(data=dfmean,aes(yintercept=meantrend+sdtrend),linetype=2,color="red") +
  geom_hline(data=dfmean,aes(yintercept=meantrend-sdtrend),linetype=2,color="red") +
  facet_wrap(~ecosystem_sub) + 
  theme(strip.text = element_text(size=10,color="white",family="sans",face="bold"),
        strip.background = element_rect(fill=OceansBlue2),
        axis.title = element_text(size=10,family="sans"),
        axis.text = element_text(size=10,family="sans"),
        panel.border=element_rect(colour="black",size=0.5),
        plot.margin=unit(c(0.65,0,0.65,0),"cm")) + 
  ylab("Sea surface temperature (C)") + 
  xlab("Date")
#dev.off()

#  Explore time series in different ways (not used)
ggdecompose(df)+
  xlab("Date")+
  ylab("SST")

ggtrend(df)+
  xlab("Date")+
  ylab("SST") +
  geom_hline(yintercept=mean(df$observation)) +
  geom_hline(yintercept=mean(df$trend,na.rm=TRUE),linetype=2)

#-----------------------------------------------------------------------------------------------------------
#  Below is the calculation of anomaly time series, which I did not use in the final version.
#-----------------------------------------------------------------------------------------------------------

anomdat <- data %>% 
  group_by(newdate,esr_region) %>% 
  mutate(anomlong=meansst-mean(meansst[year2>=1986 & year2<=2015]),
         anomrecent=meansst-mean(meansst[year2>=2014 & year2<=2019]))

anommean <- anomdat %>% 
  filter(year==2020) %>% 
  group_by(ecosystem_sub) %>% 
  summarise(longmean=mean(anomlong),
            recentmean=mean(anomrecent))

jpeg(paste0("SST_ESR/2020/GOA/SST_GOA_ESR_Anomalies",format(Sys.Date(),"%Y_%m_%d"),".jpeg"),width=6,height=4,units="in",quality=100,res=300)
anomdat %>% 
  filter(year2==2020) %>% 
  ggplot() + 
  geom_line(aes(newdate,anomlong)) + 
  #geom_line(aes(newdate,anomrecent),linetype=2,col=OceansBlue2) +
  #geom_hline(data=anommean,aes(yintercept=longmean)) + 
  #geom_hline(data=anommean,aes(yintercept=recentmean),col=OceansBlue2) + 
  facet_wrap(~esr_region) + 
  ylab("Sea surface temperature anomaly") + 
  xlab("Date") +
  theme(legend.position=c(mylegx,mylegy),
        legend.text = element_text(size=8,family="sans"),
        legend.background = element_blank(),
        legend.title = element_blank(),
        strip.text = element_text(size=10,color="white",family="sans",face="bold"),
        strip.background = element_rect(fill=OceansBlue2),
        axis.title = element_text(size=10,family="sans"),
        axis.text = element_text(size=10,family="sans"),
        panel.border=element_rect(colour="black",size=0.75),
        #axis.text.x=element_text(color=c("black",NA,NA,"black",NA,NA,"black",NA,NA,"black",NA,NA,NA)),
        legend.key.size = unit(0.35,"cm"),
        plot.margin=unit(c(0.65,0,0.65,0),"cm"))
dev.off() 

