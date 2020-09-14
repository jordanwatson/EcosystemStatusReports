library(tidyverse)
library(readxl)
library(forcats)
library(viridis)

#------------------------------------------------------------------------------
## GOA Chinook bycatch
#------------------------------------------------------------------------------

#bycatch <- read_xlsx("//nmfs.local/AKC-ABL/Users2/jordan.watson/Desktop/SOEBA/EcosysCon/Bycatch/2018/GeneticsData.xlsx",sheet="Sheet1") %>% mutate(Year=factor(Year))
bycatch <- read_xlsx("Data/GeneticsData_GOA.xlsx",sheet="Sheet1") %>% 
  mutate(Year=factor(Year))
head(bycatch)

levels(bycatch$Region) <- c("West Coast US",
                           "BC",
                           "Coast SE AK",
                           "Copper",
                           "NE GOA",
                           "NW GOA",
                           "N AK Pen",
                           "Coast W AK",
                           "Mid Yukon",
                           "Up Yukon",
                           "Russia")

bycatch <- bycatch %>% 
  mutate(stock2=fct_relevel(Region,
                            "West Coast US",
                            "BC",
                            "Coast SE AK",
                            "Copper",
                            "NE GOA",
                            "NW GOA",
                            "N AK Pen",
                            "Coast W AK",
                            "Mid Yukon",
                            "Up Yukon",
                            "Russia"),
         `Reporting Group`=stock2)


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
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5)) +
  ylab("Proportion") + 
  geom_text(data=. %>% 
              dplyr::select(Year,n) %>% 
              distinct() %>% 
              mutate(n=prettyNum(n,big.mark = ",")),
            aes(x=Year,y=0.1,label=(n)),inherit.aes = FALSE,color="white")

pdf("../Chinook_GOA/GOA_Chinook_2018.pdf",width=6,height=6)
p1
dev.off()

png("../Chinook_GOA/GOA_Chinook_2018.png",width=6,height=6,units="in",res=300)
p1
dev.off()


#------------------------------------------------------------------------------
## Bering Chinook bycatch
#------------------------------------------------------------------------------



bycatch <- read_xlsx("//nmfs.local/AKC-ABL/Users2/jordan.watson/Desktop/SOEBA/EcosysCon/Bycatch/2018/GeneticsData_BSAI.xlsx",sheet="Sheet1") %>% 
  mutate(Year=factor(Year))

bycatch <- read_xlsx("Data/GeneticsData_BSAI.xlsx",sheet="Sheet1") %>% 
  filter(Year>2010) %>% 
  mutate(Year=factor(Year),
         stock2=fct_relevel(Region,
                            "West Coast US",
                            "BC",
                            "Coast SE AK",
                            "Copper",
                            "NE GOA",
                            "NW GOA",
                            "N AK Pen",
                            "Coast W AK",
                            "Mid Yukon",
                            "Up Yukon",
                            "Russia"),
         `Reporting Group`=stock2)

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
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5)) +
  ylab("Proportion") + 
  geom_text(data=. %>% 
              dplyr::select(Year,n) %>% 
              distinct() %>% 
              mutate(n=prettyNum(n,big.mark = ",")),
            aes(x=Year,y=0.1,label=(n)),inherit.aes = FALSE)

pdf("../Chinook_BSAI/BSAI_Chinook_2018.pdf",width=6,height=6)
p1
dev.off()

png("../Chinook_BSAI/BSAI_Chinook_2018.png",width=6,height=6,units="in",res=300)
p1
dev.off()

