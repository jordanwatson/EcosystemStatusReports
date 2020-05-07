library(tidyverse)
library(DBI)
library(odbc)
library(cowplot)
library(magick)

con <- dbConnect(odbc::odbc(), "akfin", 
                 UID=rstudioapi::askForPassword("Enter AKFIN Username"), 
                 PWD= rstudioapi::askForPassword("Enter AKFIN Password"))

data <- dbFetch(dbSendQuery(con,"select tt.*,
    extract(YEAR FROM tt.read_date) as year,
    to_char(tt.read_date,'DDD') as julian
    from(
    select round(avg(t.temp),2) as meansst,
                 t.esr_region,
                 t.read_date
    from(select a.read_date,
           a.temp,
           b.esr_region
        from AFSC.erddap_sst_stat_area a
        join (select b.stat_area,
           b.nmfsarea,
            (case 
            when b.nmfsarea IN ('514','524','508','512','516','509','513','517','521') and b.max_lat>60.1 and b.max_lat < 65.6 then 'NBS' 
            when b.nmfsarea IN ('514','524','508','512','516','509','513','517','521') and b.max_lat<=60.1 then 'EBS'
            when b.nmfsarea IN ('659') then 'SEAK Inside'
            when b.nmfsarea='650' or (b.nmfsarea='640' and b.stat_area<445600) then 'EGOA'
            when (b.nmfsarea IN ('620','630','649')) 
                or (b.nmfsarea='610' and b.stat_area<635000) 
                or (b.nmfsarea='640' and b.stat_area>445500) then 'WGOA'
            else 'Other' end) esr_region from AFSC.erddap_stat_area_supplemental b) b
        on a.stat_area=b.stat_area) t
    group by t.esr_region,t.read_date
    order by t.read_date,t.esr_region
    ) tt")) %>% 
  rename_all(tolower) %>% 
  mutate(julian=as.numeric(julian),
         esr_region2=case_when(
           esr_region=="EBS" ~ "Eastern Bering Sea",
           esr_region=="NBS" ~ "Nothern Bering Sea",
           esr_region=="EGOA" ~ "Eastern Gulf of Alaska",
           esr_region=="WGOA" ~ "Western Gulf of Alaska",
           esr_region=="CGOA" ~ "Central Gulf of Alaska"))

OceansBlue1='#0093D0'
CoralRed1='#FF4438'
SeagrassGreen1='#93D500'
UrchinPurple1='#7F7FFF'

current.year <- max(data$year)
last.year <- current.year-1
median.years <- 2003:2013
median.lab <- "Median 2003-2013"

current.year.color <- CoralRed1
last.year.color <- OceansBlue1
median.color <- "black"

theme_set(theme_cowplot())

png("SST_esr_update.png",width=5,height=5,units="in",res=200)
ggplot() +
  geom_line(data=data %>% filter(year<last.year & esr_region%in%(c("NBS","EBS"))),aes(julian,meansst,group=factor(year),col='mygrey')) +
  geom_line(data=data %>% filter(year==last.year & esr_region%in%(c("NBS","EBS"))),aes(julian,meansst,color='last.year.color'),size=1.15) +
  geom_line(data=data %>% 
              filter(year%in%median.years & esr_region%in%(c("NBS","EBS"))) %>% 
              group_by(esr_region2,julian) %>% 
              summarise(mediantemp=median(meansst)),aes(julian,mediantemp,col='median.color'),size=1) +
  geom_line(data=data %>% filter(year==current.year & esr_region%in%(c("NBS","EBS"))),aes(julian,meansst,color='current.year.color'),size=1.15) +
  facet_wrap(~esr_region2,ncol=1) + 
  scale_color_manual(name="",
                     breaks=c('current.year.color','last.year.color','mygrey','median.color'),
                     values=c('current.year.color'=current.year.color,'last.year.color'=last.year.color,'mygrey'='grey70','median.color'=median.color),
                     labels=c(current.year,last.year,paste0('2002-',last.year-1),median.lab)) +
  ylab("Mean Sea Surface Temperature (C)") + 
  xlab("Date") +
  theme(legend.position=c(0.045,0.91),
        legend.text = element_text(size=10))
dev.off()

ggdraw() +
  draw_image("") +
  draw_plot(my_plot)
