#-----------------------------------------------------------------------------------------------------------------
# This script generates the figure that is included in the Ecosystem Considerations Report each year for SECM.
# There are two data files that need to be updated each year - one for juvenile and one for adult salmon. 
# Emily Fergusson can run these queries, which include CPUE for each of the species and stages. 
# Each year, you'll need to update two things:
# (1) this.year should be updated to your current year. 
# (2) In the figure, you need to tweak the number of "" after 2017 in the labels argument to match the number of years in the breaks argument.

# Read in R packages
library(dplyr)
library(ggplot2)
library(readxl)
library(forcats)

#  Change this.year to the current year
this.year <- 2019

#  Load data. Assuming the same naming convention is used each year, the data should read-in correctly if you have a 2019 file to match this.year=2019.
asalmon <- read_xlsx(paste0("Data/SECM adult salmon catch data_",this.year,".xlsx"),sheet = "Sheet1")
juv <- read_xlsx(paste0("Data/SECM juvenile catch data_",this.year,".xlsx"),sheet = "Sheet1")

#  Get the data in shape.
hauls <- asalmon %>% 
  filter(Species==0) %>% 
  dplyr::select(Year,Month,Station,Haul,`Julian date`,AdjCatch) %>% 
  group_by(Year) %>% 
  summarise(Denom=sum(AdjCatch))

#  I multiple the catch rate by 3 so that it is in units of fish/hour instead of fish/(20min).
cpue <- asalmon %>% 
  right_join(data.frame(expand.grid(Year=unique(asalmon$Year),Species=unique(asalmon$Species)))) %>% 
  filter(Species!=0) %>% 
  right_join(hauls) %>% 
  replace_na(list(AdjCatch=0)) %>% 
  group_by(Species,Year) %>% 
  summarise(cpue=3*sum(AdjCatch)/Denom[1])

cpuejuv <- juv %>% 
  dplyr::select(Year,Pink=Adj_raw_Pink,Chum=Adj_raw_Chum,Sockeye=Adj_raw_Sockeye,Chinook=Adj_raw_Chinook,Coho=Adj_raw_Coho) %>% 
  gather(Species,AdjCatch,-Year) %>% 
  right_join(data.frame(expand.grid(Year=unique(juv$Year),Species=c("Pink","Chum","Sockeye","Chinook","Coho")))) %>% 
  filter(Species!=0) %>% 
  right_join(hauls) %>% 
  replace_na(list(AdjCatch=0)) %>% 
  group_by(Species,Year) %>% 
  summarise(cpue=3*sum(AdjCatch)/Denom[1])

cpuesalmon <- bind_rows(cpue,cpuejuv) %>% 
  mutate(Species2=as.character(fct_recode(Species,`Chinook (Juv)`="Chinook",
                            `Chum (Juv)`="Chum",
                            `Coho (Juv)`="Coho",
                            `Pink (Juv)`="Pink",
                            `Sockeye (Juv)`="Sockeye",
                            `Chum (Adult)`="A-Chum",
                            `Coho (Adult)`="A-Coho",
                            `Pink (Adult)`="A-Pink",
                            `Sockeye (Adult)`="A-Sockeye",
                            `Chinook (Immature)`="Im-Chinook"))) %>% 
  data.frame

#  Create and automatically output the figure of CPUE over time. 
#  You will need to edit the labels argument in scale_x_continuous to format as you like. The breaks will automatically update with "this.year"
#  The current labels argument has two sets of empty quotes after 2017, which is formatted for 2019. If you change this.year but not these quotes
#  you will get an error.
png(paste0("SECM_Salmon_",this.year,".png"),width=9,height=7.2,units="in",res=300)
cpuesalmon %>% 
  filter(!Species2%in%c("Coho (Adult)","Sockeye (Adult)")) %>% 
  mutate(Species2=fct_relevel(Species2,"Coho (Juv)",after=Inf)) %>% 
  ggplot(aes(x=Year,y=cpue)) + 
  geom_line() + 
  facet_wrap(~Species2,scales="free",ncol=2) + 
  scale_x_continuous(breaks=c(1997:this.year),labels = c(1997,"","","","","","","","","",2007,"","","","","","","","","",2017,"","")) + 
  theme_bw() + 
  ylab("CPUE (Number / hour)") + 
  theme(axis.title.x = element_blank(),
        axis.text=element_text(size=12),
        strip.text=element_text(size=14),
        axis.title = element_text(size=14))
dev.off()

#-------------------------------------------------------------------------------------
#  The below code is for some optional figures
#-------------------------------------------------------------------------------------

#  Same plot as above but different x-axis labels
#  I have the first line commented out to avoid accidentally over-writing the above figure.
#png(paste0("SECM_Salmon_",this.year,".png"),width=9,height=7.2,units="in",res=300)
cpuesalmon %>% 
  filter(!Species2%in%c("Coho (Adult)","Sockeye (Adult)")) %>% 
  mutate(Species2=fct_relevel(Species2,"Coho (Juv)",after=Inf)) %>% 
  ggplot(aes(x=Year,y=cpue)) + 
  geom_line() + 
  facet_wrap(~Species2,scales="free",ncol=2) + 
  scale_x_continuous(breaks=c(1997,2002,2007,2012,2017),labels = c(1997,"",2007,"",2017)) + 
  theme_bw() + 
  ylab("CPUE (Number / hour)") + 
  theme(axis.title.x = element_blank(),
        axis.text=element_text(size=12),
        strip.text=element_text(size=14),
        axis.title = element_text(size=14))
dev.off()


#  In 2017, we also included a figure of forage fish. Note that if you do this you'll need another data file that contains 
#  forage fish catches and rates. See the other R scripts in the 2017 folder. 

#juv <- read_xlsx("Data/SECM juvenile catch data_2017.xlsx",sheet = "Sheet1")

cpueforage <- forage %>% 
  right_join(data.frame(expand.grid(Year=unique(forage$Year),Species=unique(forage$Species)))) %>% 
  filter(Species!=0) %>% 
  right_join(hauls) %>% 
  replace_na(list(AdjCatch=0)) %>% 
  group_by(Species,Year) %>% 
  summarise(cpue=3*sum(AdjCatch)/Denom[1])

png("SECM_ForageFish.png",width=9,height=7.2,units="in",res=300)
cpueforage %>% 
  mutate(Species2=as.character(fct_recode(Species,
                             "Pollock (Adult)"="A-Pollock",
                             "Pollock (Larval)"="L-Pollock"))) %>% 
  ggplot(aes(x=Year,y=cpue)) + 
  geom_line() + 
  facet_wrap(~Species2,scales="free") + 
  scale_x_continuous(breaks=c(1997,2002,2007,2012,2017),labels = c(1997,"",2007,"",2017)) + 
  theme_bw() + 
  ylab("CPUE (Number / hour)") + 
  theme(axis.title.x = element_blank(),
        axis.text=element_text(size=12),
        strip.text=element_text(size=14),
        axis.title = element_text(size=14))
dev.off()
  