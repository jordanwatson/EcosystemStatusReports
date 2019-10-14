library(tidyverse)
library(ncdf4)
library(RCurl)
library(forcats)

OceansBlue3='#00467F'
WavesTeal1='#1ECAD3'
OceansBlue1='#0093D0'
CrustaceanOrange2='#D65F00'

data <- read_csv(paste0("eGOA_Chlorophyll_Spring.csv")) %>%
  mutate(region="eGOA") %>% 
  bind_rows(read_csv(paste0("cGOA_Chlorophyll_Spring.csv")) %>%
              mutate(region="cGOA")) %>% 
  bind_rows(read_csv(paste0("wGOA_Chlorophyll_Spring.csv")) %>%
              mutate(region="wGOA")) %>% 
  mutate(Month=fct_relevel(Month,"March","April","May","June","July"),
         Season=ifelse(Month%in%c("March","April","May"),"Mar-May","Jun-Aug"),
         Season=fct_relevel(Season,"Mar-May","Jun-Aug"))

myfont <- 16

png("Chlorophyll_Monthly_by_GOA_region.png",width=10.5,height=7.5,units="in",res=300)
data %>%
  ggplot(aes(Year,chl,color=region)) + 
  geom_line(size=1) + 
  geom_point(size=2) + 
  theme_bw() + 
  facet_wrap(~Month) + 
  ylab("Chlorophyll") + 
  theme(legend.position=c(0.1,0.85),
        axis.text=element_text(size=myfont),
        axis.title=element_text(size=myfont),
        strip.text=element_text(size=myfont),
        legend.text = element_text(size=myfont),
        legend.title = element_blank(),
        legend.background = NULL,
        panel.grid.minor = element_blank())  + 
  scale_color_manual(values=c(OceansBlue3,WavesTeal1,CrustaceanOrange2),name="GOA Region") + 
  scale_x_continuous(breaks = 2006:2019, labels = c("","2007","","","2010","","","2013","","","2016","","","2019"),expand=c(0.05,0.01))
dev.off()


png("Seasonal_Chlorophyll_GOA_Standardized_Anomaly.png",width=10,height=7.5,units="in",res=300)
data %>%
  group_by(Season,Year,region) %>% 
  summarise(meanchl=mean(chl,na.rm=TRUE)) %>% 
  ungroup %>% 
  group_by(Season,region) %>% 
  mutate(chlanom=(meanchl-mean(meanchl))/sd(meanchl)) %>% 
  ggplot(aes(factor(Year),chlanom)) + 
  geom_bar(stat="identity",position="dodge") + 
  theme_bw() + 
  theme(axis.text=element_text(size=myfont),
        axis.title=element_text(size=myfont),
        strip.text=element_text(size=myfont),
        panel.grid.minor = element_blank()) +
  xlab("Year") + 
  ylab("Chlorophyll") + 
  facet_grid(region~Season) +
  scale_x_discrete(breaks = 2006:2019, labels = c("","2007","","","2010","","","2013","","","2016","","","2019"),expand=c(0.05,0.01))
dev.off()
