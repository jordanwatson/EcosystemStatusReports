library(tidyverse)
library(readxl)
library(viridis)

bycatch <- read_xlsx("GeneticsData.xlsx",sheet="Sheet1") %>% mutate(Year=factor(Year))
head(bycatch)

levels(bycatch$Stock) <- c("West Coast US",
                           "BC",
                           "Coast SE AK",
                           "Copper","NE GOA",
                           "NW GOA",
                           "N AK Pen",
                           "Coast W AK",
                           "Mid Yukon",
                           "Up Yukon",
                           "Russia")

bycatch <- bycatch %>% mutate(stock2=fct_relevel(Stock,"West Coast US",
                           "BC",
                           "Coast SE AK",
                           "Copper","NE GOA",
                           "NW GOA",
                           "N AK Pen",
                           "Coast W AK",
                           "Mid Yukon",
                           "Up Yukon",
                           "Russia"))


bycatch %>% ggplot(aes(x=Year,y=BayesMean,fill=stock2)) + 
  geom_bar(stat="identity",position="dodge") + 
  geom_errorbar(aes(x=Year,ymin=BayesMean-StdDev,ymax=BayesMean+StdDev),position="dodge") + 
  theme_bw() 


bycatch %>% arrange(stock2) %>% data.frame

# Show only those groupings that account for at least 0.01 (proportion) of bycatch in any of the years.
bycatch %>% dplyr::select(-Stock) %>% filter(BayesMean>0.01) %>%  rename(Stock=stock2) %>% ggplot(aes(x=Year,y=BayesMean,fill=Stock)) + 
  geom_bar(stat="identity",position="dodge") + 
  geom_errorbar(aes(x=Year,ymin=BayesMean-StdDev,ymax=BayesMean+StdDev),position="dodge") + 
  theme_bw() + 
  ylab("Proportion")




#-------------------------------------------------------------
#  Chum bycatch 
bycatch <- read_xlsx("Data/Chum multi-yr stock estimates.xlsx",sheet="for_R_esr") %>% 
  mutate(Year=factor(Year))
head(bycatch)

levels(bycatch$Region) <- c("SE Asia",
                           "NE Asia",
                           "W AlasKa",
                           "Up/Mid Yukon",
                           "SW Alaska",
                           "E GOA/PNW")

bycatch <- bycatch %>% 
  mutate(stock2=fct_relevel(Region,"SE Asia",
                            "NE Asia",
                            "W Alaska",
                            "Up/Mid Yukon",
                            "SW Alaska",
                            "E GOA/PNW"),
         `Reporting Group`=fct_recode(stock2,
                                      "SE Asia"="SE Asia",
                                      "NE Asia"="NE Asia",
                                      "W Alaska"="W Alaska",
                                      "Up-Mid Yukon"="Up/Mid Yukon",
                                      "SW Alaska"="SW Alaska",
                                      "E GOA / PNW"="E GOA/PNW"))


p1 <- bycatch %>% 
  ggplot(aes(x=Year,y=Mean,fill=`Reporting Group`)) + 
  geom_bar(stat="identity") +
  theme_bw() +
  scale_fill_viridis(discrete="TRUE") + 
  theme(legend.position="top",
        axis.text = element_text(size=12,color="black"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        axis.title=element_text(size=12)) + 
  ylab("Proportion") + 
  geom_text(data=. %>% 
              dplyr::select(Year,n) %>% 
              distinct() %>% 
              mutate(n=prettyNum(n,big.mark = ",")),
            aes(x=Year,y=0.1,label=(n)),inherit.aes = FALSE)

pdf("Bering_Chum.pdf",width=6,height=6)
p1
dev.off()

png("Bering_Chum.png",width=6,height=6,units="in",res=300)
p1
dev.off()

