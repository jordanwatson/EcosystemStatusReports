library(tidyverse)
library(readxl)
data <- read_excel("Data/AgeSurvival.xlsx",sheet="Unuk")

p1 <- data %>% 
  filter(brood_year<2012) %>% 
  group_by(brood_year) %>% 
  summarise(mysum=sum(survival_percent)) %>% 
  ggplot(aes(brood_year,mysum)) + 
  geom_line() + 
  geom_point() + 
  geom_smooth(method="lm",linetype=2,color="red") + 
  theme_bw() +
  ylab("Survival (%)") + 
  xlab("Brood Year") + 
  scale_x_continuous(breaks=seq(1975,2015,by=5))

png("LPW_Survival.png",width=7.5,height=6,units="in",res=300)
p1
dev.off()