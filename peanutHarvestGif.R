library(ggplot2)
library(tibble)
library(ggfortify)
library(spdep)
library(tidyverse)
library(RColorBrewer)
library(tigris)
library(magick)
library(usdarnass)

options(scipen=999)

currentYear <- as.numeric(format(as.Date(Sys.Date(), format="%d/%m/%Y"),"%Y"))

nass_set_key(key = "53D17844-D92E-3CF5-8520-6E1E45CA2A49", overwrite = FALSE) 

peanutHarvest4county <- nass_data(
  source_desc="SURVEY",
                           agg_level_desc = "COUNTY",
          short_desc = "PEANUTS - ACRES HARVESTED")


usaCounties <- counties( cb = T, resolution = "20m")
states <- states(cb = T, resolution = "20m")
states <- states %>% 
  filter(STATEFP < "60") %>% # keep only states omits territories
  filter(!STATEFP %in% c("02", "15")) # AK HI

usaCounties <- usaCounties %>% 
  filter(STATEFP < "60") %>% # keep only states omits territories out
  filter(!STATEFP %in% c("02", "15")) # AK HI

usaCounties <- usaCounties %>%
  mutate(FIPS = paste(STATEFP, COUNTYFP, sep=""))

us_counties_peanuts <- peanutHarvest4county %>%
  mutate(FIPS = paste(state_ansi, county_ansi, sep="")) %>%
  mutate(acres = as.numeric(gsub(",","",Value))/1000) %>%
  filter(year > 1979) %>%
  mutate(peanuts.cut = cut_number(acres, n = 6, 
                                 dig.lab = 0, 
    labels = c("< 2", "2 to 4", "4 to 8", "8 to 13", "13 to 20", "> 20")))

years <- levels(factor(us_counties_peanuts$year))
year.range <- min(as.numeric(years)):max(as.numeric(years))



peanutMaps <- lapply(min(as.numeric(years)):max(as.numeric(years)), function(yr) {
  peanutMap <- 
   ggplot() + 
     geom_sf(data = usaCounties, fill = "grey99", colour = "grey74", lwd = 0.01) +
     geom_sf(data = usaCounties %>% 
            left_join(us_counties_peanuts, by="FIPS") %>%
            filter(year == yr) , 
            aes(fill = peanuts.cut),
            colour = "grey37",
            lwd = 0) + 
  geom_sf(data = states, fill = NA, color = "grey6", lwd = 0.4) +
    coord_sf(default_crs = sf::st_crs(4326)) +
    labs(title=paste(' United States harvested peanut acreage,', yr),
              caption='Terry Griffin, Kansas State University. Data source: USDA National Agricultural Statistics Service (NASS)') +
    scale_fill_brewer(palette = "Greens", name = "acres \n(000s)", drop = FALSE) +
    theme_bw() +
    theme(text=element_text(family="Times New Roman", size=16))
  
 ggsave(paste("peanutProductionGif", yr, ".png", sep = ""), width=13.33, height=6.42, units='in', dpi="print")

 return(peanutMap)
})

imglayers <- sapply(year.range, function(yr) {
  image_read(paste('peanutProductionGif', yr, '.png', sep=''))
})

imganim4 <- image_animate(image_join(imglayers), fps = 4, optimize = T, dispose = "none")
image_write(imganim4, 'peanutProductionGif4.gif')

imganim2 <- image_animate(image_join(imglayers), fps = 2, optimize = T, dispose = "previous")
image_write(imganim2, 'peanutProductionGif2.gif')


