
library(usdarnass) # negates necessity for API
library(tidyverse) # ggplot() and gather()

currentYear <- as.numeric(format(as.Date(Sys.Date(), format="%d/%m/%Y"),"%Y"))

nass_set_key(key = "53D17844-D92E-3CF5-8520-6E1E45CA2A49", overwrite = FALSE) #replace key 

peanutCondition000 <- nass_data(year=">=2019", 
                             agg_level_desc = "STATE", 
                             source_desc = "SURVEY",
                             commodity_desc = "PEANUTS",
                             statisticcat_desc = "CONDITION",
                          freq_desc = "WEEKLY") 

peanutCondition5yr <- nass_data(year="2023", 
                                agg_level_desc = "STATE", 
                                source_desc = "SURVEY",
                                commodity_desc = "PEANUTS",
                                statisticcat_desc = "CONDITION, 5 YEAR AVG",
                                freq_desc = "WEEKLY") 

peanutCondition5yr <- peanutCondition5yr %>%
  filter(state_name == "GEORGIA") %>%
  mutate(percent = as.numeric(Value)) %>%
  mutate(condition = str_remove(short_desc , "PEANUTS - CONDITION, 5 YEAR AVG, MEASURED IN PCT ")) %>%
  mutate(condition = tolower(condition)) %>% 
  rename(week = end_code) %>%
  mutate(state = str_to_title(state_name))

peanutCondition5yr$condition <- factor(peanutCondition5yr$condition, 
                                      levels=c("excellent","good", "fair","poor","very poor"))


peanutCondition00 <- peanutCondition000 %>%
  mutate(percent = as.numeric(Value)) %>%
  mutate(condition = str_remove(short_desc , "PEANUTS - CONDITION, MEASURED IN PCT ")) %>%
  rename(week = end_code) %>%
  mutate(condition = tolower(condition)) %>%
  filter(state_name == "GEORGIA") %>%
  filter(year == 2023)

peanutCondition00$condition <- factor(peanutCondition00$condition, 
                                      levels=c("excellent","good", "fair","poor","very poor"))

peanutCondition00 %>%
  filter(state_name == "GEORGIA") %>%
  filter(year == 2023) %>%
ggplot() +
  geom_col(aes(x= week, y=percent, group=condition , fill=condition )) +
  theme_bw() + labs(x="week of year") +
  theme(legend.position = "bottom", legend.title = element_blank()) 
ggsave("peanutConditionGA2023.png", height=5, width=7, units = "in", dpi = "retina")
  

ggplot() +
    geom_line(data= peanutCondition5yr, aes(x=week, y=percent, group=condition, col=condition)) +
    geom_line(data= peanutCondition00, aes(x=week, y=percent, group=condition, col=condition), lwd=2) +
    theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  labs(x="week of year", title="2023 versus 5-year average crop condition, Georgia") 
ggsave("peanutConditionGA2023v5yr.png", height=3, width=7, units = "in", dpi = "retina")

