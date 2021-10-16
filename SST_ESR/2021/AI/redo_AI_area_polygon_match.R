library(tidyverse)
library(lubridate)

lkp <- readRDS("Data/crwsst/crwsst_spatial_lookup_table.RDS") %>% 
  #filter(Ecosystem!="Eastern Bering Sea") %>% 
  mutate(ecosub=as.character(Ecosystem_sub),
         ecosub=case_when(longitude<(-164) & as.character(Ecosystem_sub)=="Western Gulf of Alaska"~"Eastern Aleutians",
                          (longitude<(-170) & as.character(Ecosystem_sub)=="Eastern Aleutians")~"Central Aleutians",
                          TRUE~as.character(ecosub))) %>% 
  filter(ecosub%in%c("Eastern Aleutians","Central Aleutians","Western Aleutians")) %>% 
  dplyr::select(Ecosystem_sub=ecosub,id)

lkp %>% 
  mutate(longitude=ifelse(longitude<0,longitude+360,longitude)) %>% 
  ggplot(aes(longitude,latitude,color=ecosub)) +
  geom_point()



#  To recreate the full time series from the raw data you can rerun the commented out code. Takes 10 minutes or so.
data <- lapply(1985:2021,function(x) readRDS(paste0("Data/crwsst/crw_sst_matched/crw_sst_matched_",x,".RDS")) %>%
                inner_join(lkp) %>%
                dplyr::select(-id) %>%
                mutate(date=as_date(date)) %>%
                group_by(date,Ecosystem_sub) %>%
                summarise(meansst=mean(CRW_SST,na.rm=TRUE))) %>%
 bind_rows()

#  To recreate the full time series from the raw data you can rerun the commented out code. Takes 10 minutes or so.
data <- readRDS("Data/crwsst/crw_sst_matched/crw_sst_matched_2021.RDS") %>%
                 inner_join(lkp) %>%
                 dplyr::select(-id) %>%
                 mutate(date=as_date(date)) %>%
                 group_by(date,Ecosystem_sub) %>%
                 summarise(meansst=mean(CRW_SST,na.rm=TRUE)) %>%
  bind_rows(readRDS("Data/crwsst_ai_new_1985_through_20210901.RDS")) %>% 
  distinct()

data %>%
  arrange(date) %>% 
  saveRDS("Data/crwsst_ai_new_1985_through_20211011.RDS")


test <- readRDS(paste0("Data/crwsst/crw_sst_matched/crw_sst_matched_2021.RDS"))
# readRDS("Data/crwsst/crw_sst_matched/crw_sst_matched_2020.RDS") %>%
#   inner_join(lkp) %>%
#   dplyr::select(-id) %>%
#   mutate(date=as_date(date)) %>% 
#   group_by(date,Ecosystem_sub) %>%
#   summarise(meansst=mean(CRW_SST,na.rm=TRUE)) %>%
#   bind_rows(readRDS("Data/crwsst_ai_19850401_through_2019.RDS")) %>% 
#   distinct() %>% 
#   arrange(date) %>% 
#   saveRDS("Data/crwsst_ai_19850401_through_2020.RDS")

data %>% 
  ggplot(aes(date,meansst)) + 
  geom_line() + 
  facet_wrap(~Ecosystem_sub)

readRDS("Data/crwsst/crw_sst_matched/crw_sst_matched_2021_through_011121.RDS") %>%
  inner_join(lkp) %>%
  dplyr::select(-id) %>%
  mutate(date=as_date(date)) %>% 
  group_by(date,Ecosystem_sub) %>%
  summarise(meansst=mean(CRW_SST,na.rm=TRUE)) %>%
  bind_rows(readRDS("Data/crwsst_ai_19850401_through_2020.RDS")) %>% 
  distinct() %>% 
  arrange(date) %>% 
  saveRDS("Data/crwsst_ai_19850401_through_011121.RDS")
