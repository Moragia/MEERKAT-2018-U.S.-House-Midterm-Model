rm(list=ls())
source("Tracker/functions.R")
# would be nice to have a graphic for each year, just for kicks  -----------
library(tidyverse)
library(lubridate)
library(zoo)
source("~/theme_elliott.R")

# R-Squared, error over time ------------

# get polls
gbpolls <- read_csv("Tracker/Data/gbpolls.csv")  %>%
  mutate(pollsum =Democrats + Republicans,
         votesum =DemVote + RepVote,
         #Democrats = Democrats/pollsum*100,
         #Republicans = Republicans/pollsum*100,
         #DemVote = DemVote/votesum*100,
         #RepVote = RepVote/votesum*100,
         Pres.PollMargin = ifelse(PartyPres== 1,Democrats-Republicans,Republicans-Democrats),
         Pres.VoteMargin = ifelse(PartyPres== 1,DemVote-RepVote,RepVote-DemVote),
         DemPollMargin = Democrats-Republicans,
         DemVoteMargin = DemVote-RepVote) #%>% filter(ElecYear > 1999)

# interpolate days in between
newgbpolls <- data.frame()

for (i in unique(gbpolls$ElecYear)){
  print(i)
  temp <- gbpolls %>%
    filter(ElecYear == i) %>%
    dplyr::select(StartDate,EndDate,Democrats,Pres.PollMargin,Pres.VoteMargin,ElecYear,ElecDay,DaysTilED,DemVote,PartyPres,DemPollMargin,DemVoteMargin)
  
  # group polls from same days
  temp <- temp %>%
    dplyr::group_by(StartDate,EndDate) %>%
    dplyr::summarise(Democrats = mean(Democrats),
                     Pres.PollMargin = mean(Pres.PollMargin),
                     DemPollMargin = mean(DemPollMargin),
                     ElecYear = unique(ElecYear),
                     ElecDay = unique(ElecDay),
                     DaysTilED = unique(DaysTilED),
                     DemVote = unique(DemVote),
                     Pres.VoteMargin = unique(Pres.VoteMargin),
                     DemVoteMargin = mean(DemVoteMargin),
                     PartyPres = unique(PartyPres))
  
  # formate dates
  dates <- data.frame("EndDate" = seq(min(mdy(temp$EndDate)), max(mdy(temp$EndDate)), by="days"))
  temp$EndDate <- mdy(temp$EndDate)
  
  temp <- left_join(dates, temp, by = "EndDate")

  #interpolated vars
  temp$Pres.PollMargin <- na.approx(temp$Pres.PollMargin)
  temp$DemPollMargin <- na.approx(temp$DemPollMargin)
  temp$Democrats <- na.approx(temp$Democrats)
  temp$DaysTilED <- na.approx(temp$DaysTilED)
  temp$StartDate <- temp$EndDate - 3
  
  # fill in some vars
  temp$ElecYear <- na.locf(temp$ElecYear)
  temp$ElecDay <- na.locf(temp$ElecDay)
  temp$DemVote <- na.locf(temp$DemVote)
  temp$Pres.VoteMargin <- na.locf(temp$Pres.VoteMargin)
  temp$DemVoteMargin <- na.locf(temp$DemVoteMargin)
  temp$PartyPres <- na.locf(temp$PartyPres)
  
  #bind
  newgbpolls <- rbind(newgbpolls, temp)
  
}


# get r2 by day votes = polls 
r2df.polls <- data.frame(days = seq(365:1),
                  r2=NA,
                  corr = NA)

for(day in r2df.polls$days){
  temp <- newgbpolls %>% filter(DaysTilED == day)
  if(nrow(temp)==0){next}
  r2df.polls[r2df.polls$days==day,]$r2 <- summary(lm(temp$DemVoteMargin ~ temp$DemPollMargin + temp$PartyPres))$r.squared
  
  r2df.polls[r2df.polls$days==day,]$corr <- cor(temp$Pres.PollMargin,temp$Pres.VoteMargin,use="complete.obs")
}


gg <- ggplot(data=r2df.polls,aes(x=days,y=r2)) + 
  geom_point(shape=1) +
  geom_smooth(method="loess",span=.2) +
  labs(title="Generic Congressional Ballot Polls are Very\nPredictive, Even Early On",
       subtitle="Daily r-squared of predictions of the White House party's popular vote margin using\npolls asking the generic congressional ballot in years since 1960",
       x="Days Until Election Day",
       y="Predictiveness (R-Squaared)") +
  theme_elliott() +
  scale_x_reverse() +
  scale_y_continuous(limits = c(0,1)) +
  geom_vline(xintercept=difftime(ymd("2018-11-06"),ymd(Sys.Date()),unit="days")) +
  geom_text(aes(x=as.numeric(difftime(ymd("2018-11-06"),ymd(Sys.Date()),unit="days"))+10,y=.2,label="Today"),angle=90) +
  geom_vline(xintercept=0) 


grid.arrange(gg,my_g,heights=c(9,0.5))

dev.copy(png,"Tracker/Graphics/timeline_style_GBpolls-R2.png",width=8,height=6,unit="in",res=350)
dev.off()



# proj to final day --------

newgbpolls <- read.csv("Tracker/Data/daily_ts_avg.csv") %>%
  filter(ElecYear>1970) %>%
  mutate(DemPollMargin=Democrat.Avg.fill - Republican.Avg.fill,
         DemVoteMargin = DemVote-RepVote)

# get r2 by day votes = polls 
r2df.polls <- data.frame(days = seq(365:1),
                         r2=NA,
                         corr = NA)

for(daytilEDummy in r2df.polls$days){
  temp <- newgbpolls %>% filter(DaysTilED == daytilEDummy)
  if(nrow(temp)==0){next}
  r2df.polls[r2df.polls$days==daytilEDummy,]$r2 <- summary(lm(temp$Final.Dem.Margin ~ temp$DemPollMargin + temp$PartyPres))$r.squared
  
  r2df.polls[r2df.polls$days==daytilEDummy,]$corr <- cor(temp$DemPollMargin,temp$Final.Dem.Margin,use="complete.obs")
}

gg <- ggplot(data=r2df.polls,aes(x=days,y=r2)) + 
  geom_point(shape=1) +
  geom_smooth(method="loess",span=.2) +
  labs(title="Daily Predictions of Movement in Generic Ballot Polls\nFrom Now Until Election Day",
       subtitle="Daily r-squared of predictions of the Democrat's popular vote margin using\npolls asking the generic congressional ballot in years since 1970",
       x="Days Until Election Day",
       y="Predictiveness (R-Squaared)") +
  theme_elliott() +
  scale_x_reverse() +
  scale_y_continuous(limits = c(0,1)) +
  geom_vline(xintercept=difftime(ymd("2018-11-06"),ymd(Sys.Date()),unit="days")) +
  geom_text(aes(x=as.numeric(difftime(ymd("2018-11-06"),ymd(Sys.Date()),unit="days"))+10,y=.2,label="Today"),angle=90) +
  geom_vline(xintercept=0) 


grid.arrange(gg,my_g,heights=c(9,0.5))

dev.copy(png,"Tracker/Graphics/timeline_style_GB_Movement-R2.png",width=8,height=6,unit="in",res=350)
dev.off()


