library(tidyverse)
library(haven)
library(stargazer)
history_full <- data.frame(read_dta("Data/jacobson_house_elexs_1952_2016.dta")) %>%
  mutate(year = as.numeric(year),
         incumb = ifelse(inc %in% c(0,7,9),-1,
                         ifelse(inc %in% c(1,6,8),1,0)),
         freshman= ifelse(fr == 0,0,
                          ifelse(pwin==1,1,-1))) 

# get 2x previous for uncontested elections -- what happens?

for(i in seq(1954,2016,2)){
  temp <- history_full %>% filter(year == i)
  templag <- history_full %>% filter(year == i-2)
  
  
  seats <- data.frame(stcd = temp[is.na(temp$dvp),]$stcd)
  replacement <-  templag[templag$stcd %in% seats$stcd,] %>% select(stcd, dvp)
  seats <- left_join(seats,replacement,by="stcd")
  
  temp[is.na(temp$dvp),]$dvp <- seats$dvp
  history_full[history_full$year == i,]<-temp
}


# continue 

all_elections <- data.frame(NULL)
for (YEARRUN in seq(1956,2016,2)){
  history <- data.frame(read_dta("Data/jacobson_house_elexs_1952_2016.dta")) %>%
    mutate(year = as.numeric(year),
           incumb = ifelse(inc %in% c(0,7,9),-1,
                           ifelse(inc %in% c(1,6,8),1,0)),
           freshman= ifelse(fr == 0,0,
                            ifelse(pwin==1,1,-1))) 
  
  history[is.na(history)] <- NA
  
  # get 2x previous for uncontested elections -- what happens?
  
  for(i in seq(1954,2016,2)){
    temp <- history %>% filter(year == i)
    templag <- history %>% filter(year == i-2)
    
    
    seats <- data.frame(stcd = temp[is.na(temp$dvp),]$stcd)
    replacement <-  templag[templag$stcd %in% seats$stcd,] %>% select(stcd, dvp)
    seats <- left_join(seats,replacement,by="stcd")
    
    temp[is.na(temp$dvp),]$dvp <- seats$dvp
    history[history$year == i,]<-temp
  }
  
  
  # continue 
  
  swings <- read.csv("Data/historical.csv") %>%
    dplyr::rename("year" = Year) %>%
    mutate(year=as.numeric(year),
           DemPctVotes = DemPctVotes/(DemPctVotes + RepPctVotes)*100,
           Dswing=ifelse(DirectionVotesChange == "D",1,-1)*ChangePctVotes)
  
  history <- left_join(history,swings,by="year") %>% filter(year%in% (YEARRUN-2):(YEARRUN-2))
  
  fit.open <- lm(dv ~  dpres , data=history %>% filter(incumb==0))
  fit.incumb <- lm(dv ~ dvp + dpres + incumb + freshman, data=history %>% filter(incumb!=0))
  print(paste("YEAR =",YEARRUN))
  print(summary(fit.open)$r.squared)
  print(summary(fit.incumb)$r.squared)
  
  
  # predict YEARRUN
  
  temp <- history_full %>% filter(year == YEARRUN)%>% mutate(predict=NA)
  temp[temp$incumb==0,]$predict <- predict(fit.open, temp[temp$incumb==0,]) + unique(swings[swings$year == YEARRUN,]$Dswing)
  temp[temp$incumb!=0,]$predict <- predict(fit.incumb, temp[temp$incumb!=0,])+ unique(swings[swings$year == YEARRUN,]$Dswing)
  
  temp$error <- temp$dv-temp$predict
  
  all_elections <- bind_rows(all_elections,temp)
}

h.16 <- all_elections %>% filter(year>2000)

rmse_lm.open <- rmse(h.16[h.16$incumb==0,]$error);rmse_lm.open
rmse_lm.incumb <- rmse(h.16[h.16$incumb!=0,]$error);rmse_lm.incumb

gg <- ggplot(h.16,aes(predict,dv)) + 
  geom_vline(xintercept = 50,linetype=2,col="gray") +
  geom_hline(yintercept = 50,linetype=2,col="gray") +
  geom_point(aes(col=as.character(year)),alpha=0.5,size=1.5) + 
  geom_smooth(method="lm",col="black") + 
  theme_elliott() + 
  labs(title = "Accuracy of US House Election Forecasts",
       subtitle = "Explained variance: 94%, wrong calls: 6% overall* (1% in 2016)\nEach point is an individual district in an individual year",
       x = "Predicted Democratic Vote* (Two-Party %)",
       y="Actual Democratic Vote (Two-Party %)",
       caption = "*Vote = previous vote + presidential vote + national Democratic swing + incumbency status + freshman status\n*Although the model regresses every election since 1952, jus those since 2000 are shwon here") + 
  theme(legend.position = c(.95, .3)) +
  scale_color_brewer("",palette = "GnBu")


png("Tracker/Graphics/AccuracyOverTime.png",height=6,width=8,unit="in",res=350)
grid.arrange(gg,my_g,heights=c(9, .5))
dev.off()


