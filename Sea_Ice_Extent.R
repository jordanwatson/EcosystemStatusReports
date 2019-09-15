library(tidyverse)
library(readxl)

# Pull the data the NSIDC data.
#ftp://sidads.colorado.edu/DATASETS/NOAA/G02135/seaice_analysis/
#ftp://sidads.colorado.edu/DATASETS/NOAA/G02135/seaice_analysis/Sea_Ice_Index_Regional_Daily_Data_G02135_v3.0.xlsx
data <- read_excel("Sea_Ice_Index_Regional_Daily_Data_G02135_v3.0.xlsx",sheet="Bering-Extent-km^2") %>% 
  mutate(month=ifelse(row_number()<32,"01",
                      ifelse(between(row_number(),32,60),"02",
                             ifelse(between(row_number(),61,91),"03",
                                    ifelse(between(row_number(),92,121),"04",
                                           ifelse(between(row_number(),122,152),"05",
                                                  ifelse(between(row_number(),153,182),"06",
                                                         ifelse(between(row_number(),183,213),"07",
                                                                ifelse(between(row_number(),214,244),"08",
                                                                       ifelse(between(row_number(),245,274),"09",
                                                                              ifelse(between(row_number(),275,305),"10",
                                                                                     ifelse(between(row_number(),306,335),"11","12"))))))))))),
         month.n=as.numeric(month)) %>% 
  gather(year,extent,-c(month,day,month.n))


out <- data %>% 
  filter(year>1978) %>% 
  filter(month.n%in%c(3,4)) %>% 
  group_by(year) %>% 
  summarise(Sea_Ice_Extent=sum(extent,na.rm = TRUE)) 

out %>% 
  write.csv("Sea_Ice_Extent.csv",row.names = FALSE)


out %>% 
  ggplot(aes(as.numeric(year),Sea_Ice_Extent)) + 
  geom_point() + 
  geom_line()
