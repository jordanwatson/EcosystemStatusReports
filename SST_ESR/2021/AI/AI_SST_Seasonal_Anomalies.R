# Download SST data for the Aleutian Islands ecosystem regions and calculate anomalies for the AI ESR.
#  Date created: 8/18/2021
#  Jordan.Watson@noaa.gov

# Load libraries
library(tidyverse)
library(lubridate)
library(httr)

#  Download and summarise the data via seasonal anomalies by region.
data <- httr::content(httr::GET('https://apex.psmfc.org/akfin/data_marts/akmp/ecosystem_sub_crw_avg_sst?ecosystem_sub=Western%20Aleutians,Central%20Aleutians,Eastern%20Aleutians&start_date=19850401&end_date=20211231'), type = "application/json") %>% 
  bind_rows %>% # Combines all the downloaded data
  mutate(date=as_date(READ_DATE)) %>% # reformat the date
  data.frame %>% 
  dplyr::select(date,meansst=MEANSST,esr_region=ECOSYSTEM_SUB) %>% #streamline the fields
  mutate(year=year(date), #extract the date
         month=month(date), # extract the month
         year2=ifelse(month>=10,year+1,year), #because the seasons span multiple years, create a new year that aligns with the seasons
         season=ifelse(month>=10 | month<=3,"Winter","Summer"), # Define the seasons
         esr_region=fct_relevel(esr_region,"Western Aleutians")) %>%  #Reorder the seasons. 
  filter(year2>1985) %>% # Because the winter of 1985 is truncated; it only includes Jan-Mar. So exclude.
  group_by(esr_region,year2) %>% 
  summarise(meansst=mean(meansst)) %>% # Calculate the region-year average sst
  ungroup %>% 
  group_by(esr_region) %>% 
  mutate(anomaly=meansst-mean(meansst)) # Calculate the anomalies

# Save the data file
data %>% 
  write.csv("SST_ESR/2021/AI/Aleutians_sst_anomaly_08_18_21.csv",row.names = F)

# Plot it to see how it looks
data %>% 
  ggplot(aes(year2,anomaly)) + 
  geom_bar(stat="identity") + 
  facet_wrap(~esr_region,ncol=1)


