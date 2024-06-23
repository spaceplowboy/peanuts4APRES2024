library(usdarnass) 
library(tidyverse) 
library(magrittr)
library(strucchange)

min.prob<-0.15 # lower bound DSFW, a "bad" year,  FYI 0.50 is median
max.prob<-0.85 # upper bound DSFW, a "good" year

currentYear<-year(Sys.Date())-1 # ignores current year during production
begin=15 # percent progress of beginning of most active dates
end=85 # percent progress of ending of most active dates

nass_set_key(key = "53D17844-D92E-3CF5-8520-6E1E45CA2A49", overwrite = FALSE) #replace key 

peanutHarvest <- nass_data(year=">=2014", agg_level_desc = "STATE", 
               short_desc = "PEANUTS - ACRES HARVESTED",
               reference_period_desc = "YEAR")

peanutStatesTab <- peanutHarvest %>%
  filter(year==currentYear) %>%
  mutate(value = as.numeric(gsub(",","", Value))) %>%
  mutate(percent = round(value/sum(value)*100,1)) %>%
  dplyr::select(state_name, value, percent, state_fips_code) %>%
  rename(state = state_name) %>%
  rename(acres = value) %>%
  mutate(acres = acres/1000) %>%
  rename(STATE_FIPS = state_fips_code) %>%
  as.data.frame() %>%
  filter(state != "OTHER STATES") %>%
  arrange(-acres) %>%
  mutate(state = str_to_title(state)) %>%
  dplyr::select(-STATE_FIPS)

write.csv(peanutStatesTab,"peanutStatesTab.csv")

######

DSFW0 <- nass_data(agg_level_desc = "STATE",
                   short_desc = "FIELDWORK - DAYS SUITABLE, MEASURED IN DAYS / WEEK ")

DSFW <- DSFW0 %>% 
  filter(state_name != "US TOTAL") %>%
  filter(year <= currentYear)

peanutPlantProg0 <- nass_data(year = ">=2005", short_desc="PEANUTS - PROGRESS, MEASURED IN PCT PLANTED")

peanutPlantProg <- peanutPlantProg0 %>%
filter(state_name!="US TOTAL") %>%
  filter(year <= currentYear) 

peanutHarvProg0 <- nass_data(year = ">=2015", short_desc= "PEANUTS - PROGRESS, MEASURED IN PCT HARVESTED")

peanutHarvProg <- peanutHarvProg0 %>%
  filter(state_name!="US TOTAL") %>% 
  filter(year <= currentYear) 

tab4desc<-matrix("NA", nrow=nlevels(factor(peanutPlantProg$state_name)), ncol=5)
mat4lm<-matrix(NA, nrow=nlevels(factor(peanutPlantProg$state_name)), ncol = 9)
mat4DSFWp<-matrix(NA, nrow=nlevels(factor(peanutPlantProg$state_name)), ncol = 6)
mat4DSFWh<-matrix(NA, nrow=nlevels(factor(peanutPlantProg$state_name)), ncol = 6)

for(i in 1:nlevels(factor(peanutPlantProg$state_name))){
  state<-levels(factor(peanutPlantProg$state_name))[i]
  DSFWdat<-subset(DSFW, state_name==state)
  plantdat<-subset(peanutPlantProg, state_name==state)
  last5yearsp<-subset(plantdat, year>=2015) #actually last 4 years
  last5yearsp$value<-as.numeric(as.character(last5yearsp$Value))
  last5yearsp$begin_code1<-as.numeric(as.character(last5yearsp$begin_code))
  dat4graph5yrsp<-aggregate(value~begin_code1, data=last5yearsp, FUN = "mean")
  
  numRows<-as.numeric(max(levels(factor(DSFWdat$begin_code))))#nlevels(factor(DSFWdat$begin_code))-1
  mat4longterm<-matrix("NA", nrow = numRows, ncol = 4)
  
  for(k in as.numeric(min(levels(factor(DSFWdat$begin_code)))):as.numeric(max(levels(factor(DSFWdat$begin_code)))))      {
    WOY<-k
    j<-k-as.numeric(min(levels(factor(DSFWdat$begin_code))))
    dat4longtermDSFW<-subset(DSFWdat, begin_code==WOY)
    q4chart<-quantile(as.numeric(as.character(dat4longtermDSFW$Value)), probs=c(min.prob, .5, max.prob))
    
    if (length(dat4longtermDSFW[,1])>=4)
    {
      mat4longterm[j,1]<-k
      mat4longterm[j,2]<-q4chart[1]
      mat4longterm[j,3]<-q4chart[2]
      mat4longterm[j,4]<-q4chart[3]
    }
  }
  
  colnames(mat4longterm)<-c("WOY", "Bad15th", "Median50th", "Good85th")
  mat4LT<-as.data.frame(mat4longterm)
  mat4LT$Bad15th<-   as.numeric(as.character(mat4LT$Bad15th))
  mat4LT$Median50th<-as.numeric(as.character(mat4LT$Median50th))
  mat4LT$Good85th<-  as.numeric(as.character(mat4LT$Good85th)) 
  
  mat4LT2<-mat4LT[complete.cases(mat4LT[ , 2]),]
  
  colnames(mat4LT2)<-c("WOY", "15th", "50th", "85th")
  dat4DSFW<-gather(mat4LT2, variable, value, c("15th", "50th", "85th"), factor_key=TRUE)
  
  dat4DSFW$WOY <- as.numeric(levels(factor(dat4DSFW$WOY)))#[dat4DSFW$WOY]
  dat4DSFW<-dat4DSFW[complete.cases(dat4DSFW[ , 3]),]
  
    ggplot() + 
    geom_line(aes(x=dat4DSFW$WOY, y= dat4DSFW$value, group=dat4DSFW$variable, 
                  linetype=dat4DSFW$variable, 
                  color=dat4DSFW$variable), size=1.) +
    scale_y_continuous(breaks = round(seq(0, 7, by = 1),1), limits=c(0,7)) +
    xlim(0, 52) + guides(fill=guide_legend(title=NULL)) +
    labs(y="days per week", x="week of year", caption="source: USDA NASS") +
    labs(colour = "Percentile") +
    scale_color_manual("", values=c("darkgreen", "darkred", "black")) +
    scale_linetype_manual("", values=c("dotted", "twodash", "solid"))+
    theme_bw()
  ggsave(paste("1DSFW", state, "graph.png", sep=""), width=6, height=4, units="in", dpi = 600)
  
  beginPeriodp<-min(which(abs(dat4graph5yrsp$value-begin)==min(abs(dat4graph5yrsp$value-begin))))
  begin15p<-dat4graph5yrsp$begin_code1[beginPeriodp]+1
  endPeriodp<-min(which(abs(dat4graph5yrsp$value-end)==min(abs(dat4graph5yrsp$value-end))))
  end85p<-dat4graph5yrsp$begin_code1[endPeriodp]
  
  tab4desc[i,1]<-state
  tab4desc[i,2]<-begin15p
  tab4desc[i,3]<-end85p
  
  numWeeks<-end85p-begin15p+1
  
  pDSFWdat<-subset(DSFWdat, begin_code>=begin15p & begin_code<=end85p)
  pDSFWdat$value<-as.numeric(as.character(pDSFWdat$Value))
  pDSFWdat$year<-as.numeric(as.character(pDSFWdat$year))
  DSFWdatptest<-aggregate(value~year, data=pDSFWdat, FUN = "length")
  DSFWdatp<-aggregate(value~year, data=pDSFWdat, FUN = "sum")
  DSFWdat2<-merge(DSFWdatp,DSFWdatptest,by="year")
  DSFWdat3<-subset(DSFWdat2, value.y==numWeeks)
  
  numYearsp<-length(DSFWdat3$year)
  
  mat4lm[i,1]<-state
  mat4lm[i,2:4]<-round(summary(lm(DSFWdat3$value.x~DSFWdat3$year))$coefficients[2,c(1,2,4)],2)
  if (length(DSFWdat3$year)>5)
  {
    chow4JSC<-sctest(formula=value.x~year, data = DSFWdat3, type = "Chow") # chow test
    mat4lm[i,8]<-round(chow4JSC$p.value, 4)
  } 
  
  ggplot(DSFWdat3, aes(x=year, y=value.x)) + geom_point() +
    geom_smooth(method=lm, se=T) + ylim(0,50) +xlim(1995, 2020) +
    ylab("number days") + xlab(NULL) + theme_bw()
  ggsave(paste("6figure10",state, "4slope.png", sep=""))
  
  hdat<-subset(peanutHarvProg, state_name==state)
  
  hlast5years<-subset(hdat, year>=2015) #last 4 years
  hlast5years$value<-as.numeric(as.character(hlast5years$Value))
  hdat4graph5yrs<-aggregate(value~begin_code, data=hlast5years, FUN = "mean")
  
  dat4graph5yrsp<-data.frame(dat4graph5yrsp)
  hdat4graph5yrs<-data.frame(hdat4graph5yrs)
  dat4graph5yrsp$begin_code1<-as.numeric(as.character(dat4graph5yrsp$begin_code1))
  hdat4graph5yrs$begin_code<-as.numeric(as.character(hdat4graph5yrs$begin_code))
  
  colnames(dat4graph5yrsp)<-c("WOY", "perc")
  colnames(hdat4graph5yrs)<-c("WOY", "perc")
  dat4graph5yrsp$Progress<-"planting"  
  hdat4graph5yrs$Progress<-"harvest"
  peanutProgress<-rbind(dat4graph5yrsp, hdat4graph5yrs)
  
  ggplot() +
    geom_line(data=peanutProgress, aes(x=WOY, y=perc, group=Progress, color=Progress, linetype=Progress),
              size=1.1) +
    labs(y="percentile", x="week of year", caption="source: USDA NASS") +
    xlim(0, 52) +
    geom_hline(yintercept=begin, linetype="dotted", color="lightgrey", size=.8) +
    geom_hline(yintercept=end, linetype="dotted", color="lightgrey",   size=.8) +
    scale_color_manual("", values=c("darkgoldenrod4", "darkgreen")) +
    scale_linetype_manual("", values=c("twodash", "solid"))+
    theme_bw() +
    theme(legend.position = "bottom")
  ggsave(paste("2progress", state, "graph.png", sep=""), width=5, height=3, units="in", dpi="retina")
  
  beginPeriod<-min(which(abs(hdat4graph5yrs$perc-begin)==min(abs(hdat4graph5yrs$perc-begin))))
  hbegin15<-as.numeric(hdat4graph5yrs$WOY[beginPeriod])+1
  endPeriod<-min(which(abs(hdat4graph5yrs$perc-end)==min(abs(hdat4graph5yrs$perc-end))))
  hend85<-as.numeric(hdat4graph5yrs$WOY[endPeriod])
  
  tab4desc[i,4]<-as.numeric(as.character(hbegin15))
  tab4desc[i,5]<-as.numeric(as.character(hend85))
  
  hDSFWdat<-subset(DSFWdat, begin_code>=hbegin15 & begin_code<=hend85)
  hDSFWdat$value<-as.numeric(as.character(hDSFWdat$Value))
  hDSFWdat$year<-as.numeric(as.character(hDSFWdat$year))
  
  hDSFWdattest<-aggregate(value~year, data=hDSFWdat, FUN = "length")
  
  hDSFWdath<-aggregate(value~year, data=hDSFWdat, FUN = "sum")
  hnumWeeks<-hend85-hbegin15+1
  hDSFWdat2<-merge(hDSFWdath,hDSFWdattest,by="year")
  hDSFWdat3<-subset(hDSFWdat2, value.x==hnumWeeks)
  
  mat4lm[i,5:7]<-round(summary(lm(hDSFWdat2$value.x~hDSFWdat2$year))$coefficients[2,c(1,2,4)],2)
  if (length(hDSFWdat2$year)>5)
  {
    chow4JSC<-sctest(formula=value.x~year, data = hDSFWdat2, type = "Chow") # chow test
    mat4lm[i,9]<-round(chow4JSC$p.value, 2)
  } 
  
  numYearsh<-length(hDSFWdat2$year)
  
  ggplot(DSFWdat3, aes(value.x)) +
    geom_histogram(color="black", fill="darkgrey", binwidth=2) +
    ylab(paste("count: n= ", numYearsp, " years", sep="")) +
    xlab("fieldwork days") +
    xlim(0,60) + 
    theme_bw()
  ggsave(paste("3hist4", state, "Planting.png", sep=""), width=5, height=4, dpi=600)
  
  ggplot(hDSFWdat2, aes(value.x)) +
    geom_histogram(color="black", fill="darkgrey", binwidth=2) +
    ylab(paste("count: n= ", numYearsp, " years", sep="")) +
    xlab("fieldwork days") +
    xlim(0,60) + 
    theme_bw()
  ggsave(paste("4hist4", state, "harvest.png", sep=""), width=5, height=4, dpi=600)
  
  mat4DSFWp[i,1]<-state
  mat4DSFWp[i,2]<-round(min(DSFWdat3$value.x),2)
  mat4DSFWp[i,3]<-round(quantile(as.numeric(as.character(DSFWdat3$value.x)), probs=min.prob),2)
  mat4DSFWp[i,4]<-round(quantile(as.numeric(as.character(DSFWdat3$value.x)), probs=.5),2)
  mat4DSFWp[i,5]<-round(quantile(as.numeric(as.character(DSFWdat3$value.x)), probs=max.prob),2)
  mat4DSFWp[i,6]<-round(max(DSFWdat3$value.x),2)
  
  mat4DSFWh[i,1]<-state
  mat4DSFWh[i,2]<-round(min(hDSFWdat2$value.x),2)
  mat4DSFWh[i,3]<-round(quantile(as.numeric(as.character(hDSFWdat2$value.x)), probs=min.prob),2)
  mat4DSFWh[i,4]<-round(quantile(as.numeric(as.character(hDSFWdat2$value.x)), probs=.5),2)
  mat4DSFWh[i,5]<-round(quantile(as.numeric(as.character(hDSFWdat2$value.x)), probs=max.prob),2)
  mat4DSFWh[i,6]<-round(max(hDSFWdat2$value.x),2)
}

colnames(mat4DSFWp)<-c("state", "minPdays", "days15p", "days50p", "days85p", "maxPdays")
colnames(mat4DSFWh)<-c("state", "minHdays", "days15h", "days50h", "days85h", "maxHdays")

mat4DSFWp <- as.data.frame(mat4DSFWp) %>% mutate(state = str_to_title(state))
mat4DSFWh <-as.data.frame(mat4DSFWh) %>% mutate(state = str_to_title(state))

write.csv(mat4DSFWp, "plantingTable.csv")
write.csv(mat4DSFWh, "harvestTable.csv")

colnames(tab4desc)<-c("state", "begin planting", "end planting", "begin harvest", "end harvest")
tab4desc <- tab4desc %>% as.data.frame() %>% mutate(state = str_to_title(state))
write.table(tab4desc, "tab4desc.txt", sep=",")
write.csv(tab4desc, "tab4desc.csv")

table1<-merge(peanutStatesTab,tab4desc, by="state")
write.csv(table1, "table1.csv")

colnames(mat4lm)<-c("state", "Planting Time Slope", "Planting Time SE", "Planting Time p-value","Harvest Time Slope", "Harvest Time SE", "Harvest Time p-value", "ChowPlant", "ChowHarvest")
mat4lm<-as.data.frame(mat4lm)
mat4lm$state<-str_to_title(mat4lm$state)
write.csv(mat4lm, "slopestats.csv")
