
library(usdarnass) # negates necessity for API
library(tidyverse) # ggplot() and gather()

currentYear <- as.numeric(format(as.Date(Sys.Date(), format="%d/%m/%Y"),"%Y"))

nass_set_key(key = "53D17844-D92E-3CF5-8520-6E1E45CA2A49", overwrite = FALSE) #replace key 

peanutPegging0 <- nass_data(year=">=2000", agg_level_desc = "STATE", source_desc = "SURVEY",
                         short_desc = "PEANUTS - PROGRESS, MEASURED IN PCT PEGGING")

peggingDat <- peanutPegging0 %>%
  mutate(state = str_to_title(state_name)) %>%
  filter(state_name!="US TOTAL") %>% 
  filter(year <= currentYear) %>%
  group_by(state, end_code) %>%
  mutate(percent = as.numeric(Value)) %>%
  summarize(percent = mean(percent)) %>%
  rename(week = end_code) %>%
  mutate(stat = "pegging") 

allPeg <- peanutPegging0 %>%
  mutate(state = str_to_title(state_name)) %>%
  filter(state_name!="US TOTAL") %>% 
  filter(year <= currentYear) %>%
  rename(week = end_code) %>%
  mutate(percent = as.numeric(Value)) %>%
  mutate(year = as.numeric(year)) %>%
  select(state, week, percent, year) %>%
  mutate(stat = "AllPegging")

peanutPlantProg0 <- nass_data(year = ">=2000", short_desc="PEANUTS - PROGRESS, MEASURED IN PCT PLANTED")

peanutPlantProg <- peanutPlantProg0 %>%
  mutate(state = str_to_title(state_name)) %>%
  filter(state_name!="US TOTAL") %>% 
  filter(year <= currentYear) %>%
  group_by(state, end_code) %>%
  mutate(percent = as.numeric(Value)) %>%
  summarize(percent = mean(percent)) %>%
  rename(week = end_code) %>%
  mutate(stat = "planting")


peanutHarvProg0 <- nass_data(year = ">=2000", short_desc= "PEANUTS - PROGRESS, MEASURED IN PCT HARVESTED")

peanutHarvProg <- peanutHarvProg0 %>%
  mutate(state = str_to_title(state_name)) %>%
  filter(state_name!="US TOTAL") %>% 
  filter(year <= currentYear) %>%
  group_by(state, end_code) %>%
  mutate(percent = as.numeric(Value)) %>%
  summarize(percent = mean(percent)) %>%
  rename(week = end_code) %>%
  mutate(stat = "harvest")


peggingDat
peanutPlantProg
peanutHarvProg

peanutProgDat <- bind_rows(peggingDat,peanutPlantProg,peanutHarvProg)

peanutProgDat$stat <- factor(peanutProgDat$stat, levels=c("planting", "pegging", "harvest"))

allPegGA <- allPeg %>% filter(state == "Georgia")
peanutProgDatGA <- peanutProgDat %>% filter(state == "Georgia")
#peanutProgDat %>%
 # filter(state == "Georgia") %>%
  ggplot() +
    geom_line(data = allPegGA, aes(x=as.numeric(week), y=percent, group=year), col="grey") +
    geom_line(data = peanutProgDatGA, aes(x=as.numeric(week), y=percent, group=stat, colour=stat), lwd=2) +
    xlab("week of year") + ylab("crop progress percentage (%)") + theme_bw() +
  theme(legend.position="bottom", legend.title =  element_blank()) 
ggsave("cropProgGA.png", height=3, width=8, units="in", dpi = "retina")

