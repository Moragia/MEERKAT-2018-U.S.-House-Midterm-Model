


library(tidyverse)
library(haven)
suppressWarnings(suppressMessages(library(stargazer)))
library(lubridate)
library(MASS)
source("~/theme_elliott.R")


rmse <- function(error){
  sqrt(mean(error^2,na.rm=TRUE))
}

#RUN_DATE <- Sys.Date() # ymd("2018-11-06") # 

DaysTilEDDUMMY <- round(as.numeric(difftime(ymd("2018-11-06"),ymd(RUN_DATE),"days")));print(DaysTilEDDUMMY)

# data cleaning ----------
# import polls
gbpolls <- suppressMessages(read_csv("Tracker/Data/gbpolls.csv"))

# filter to todays day in each cycle
gbpolls <- gbpolls %>% 
  filter(DaysTilED >= DaysTilEDDUMMY) %>% # no polls younger than today's day till ED
  mutate(Days =  DaysTilED - DaysTilEDDUMMY) %>%  # need the date for each indiv poll
  filter(DaysTilED <= case_when(DaysTilEDDUMMY>=300 ~ 550,# dynamic filtering
                                DaysTilEDDUMMY<=300 & DaysTilEDDUMMY>240 ~ 300,
                                DaysTilEDDUMMY<=240 & DaysTilEDDUMMY>180 ~ 240,
                                DaysTilEDDUMMY<=180 & DaysTilEDDUMMY>120 ~ 180,
                                DaysTilEDDUMMY<=120 & DaysTilEDDUMMY>61 ~ 120,
                                DaysTilEDDUMMY<=61 & DaysTilEDDUMMY>30 ~ 61,
                                DaysTilEDDUMMY<=30 & DaysTilEDDUMMY>14 ~ 30,
                                DaysTilEDDUMMY<=14 & DaysTilEDDUMMY>7 ~ 14,
                                DaysTilEDDUMMY<=7 ~ 7)) 

# create poll and vote share to two-party
gbpolls <- gbpolls %>%
  mutate(pollsum =Democrats + Republicans,
         votesum =DemVote + RepVote,
         Democrats = Democrats/pollsum*100,
         Republicans = Republicans/pollsum*100,
         DemVote = DemVote/votesum*100,
         RepVote = RepVote/votesum*100)

# interpolate days in between
newgbpolls <- data.frame()

for (i in unique(gbpolls$ElecYear)){
  #print(i)
  temp <- gbpolls %>%
    filter(ElecYear == i) %>%
    dplyr::select(StartDate,EndDate,Democrats,ElecYear,ElecDay,DaysTilED,DemVote,PartyPres)
  
  # group polls from same days
  temp <- temp %>%
    dplyr::group_by(StartDate,EndDate) %>%
    dplyr::summarise(Democrats = mean(Democrats),
                     ElecYear = unique(ElecYear),
                     ElecDay = unique(ElecDay),
                     DaysTilED = unique(DaysTilED),
                     DemVote = unique(DemVote),
                     PartyPres = unique(PartyPres))
  
  # formate dates
  dates <- data.frame("EndDate" = seq(min(mdy(temp$EndDate)), max(mdy(temp$EndDate)), by="days"))
  temp$EndDate <- mdy(temp$EndDate)
  
  temp <- left_join(dates, temp, by = "EndDate")
  
  #interpolated vars
  temp$Democrats <- na.approx(temp$Democrats)
  temp$DaysTilED <- na.approx(temp$DaysTilED)
  temp$StartDate <- temp$EndDate - 3
  
  # fill in some vars
  temp$ElecYear <- na.locf(temp$ElecYear)
  temp$ElecDay <- na.locf(temp$ElecDay)
  temp$DemVote <- na.locf(temp$DemVote)
  temp$PartyPres <- na.locf(temp$PartyPres)
  
  #bind
  newgbpolls <- rbind(newgbpolls, temp)
  
}

gbpolls <- newgbpolls

# create weighted average for each cycle
#setup DF
DemAvg <- NULL
for(i in unique(gbpolls$ElecYear)){
  #print(i)
  # metadata
  temp <- gbpolls %>% filter(ElecYear==i)
  temp <- temp[1,]
  
  # average
  tempavg <- gbpolls %>% 
    filter(ElecYear == i) %>%
    mutate(weight = -0.1)
  
  avg <- weighted.mean(tempavg$Democrats, tempavg$weight,na.rm=TRUE)
  
  DemAvg <- bind_rows(DemAvg,
                      data.frame("Year" = i,
                                 "Democrats" = avg,
                                 "DemVote" = temp$DemVote,
                                 "PartyPres" = temp$PartyPres))
}

# do an LM, actual vote = pollavg + presdummy

fit <- lm(DemVote ~ Democrats + PartyPres, DemAvg)
#stargazer(fit,type="text")
r2 <- summary(fit)$r.squared

# predict and evaluate
DemAvg$Predicted <- predict(fit,DemAvg)
#DemAvg$Error <- DemAvg$Predicted - DemAvg$DemVote
#fitdistr(DemAvg$Error,"normal")

#ggplot(DemAvg,aes(x=Error))+geom_dotplot(binwidth=0.5)
NationalLMError <- round(rmse(DemAvg$Predicted - DemAvg$DemVote),1);print(NationalLMError)
MOE <- round(NationalLMError*1.96,1);print(MOE)

# add 2016, special colors for good ID
DemAvg<- rbind(DemAvg,data.frame("Year" = 2018,
                                 "Democrats" = average$Democrat,
                                 "DemVote" = predict(fit,data.frame("Democrats" =average$Democrat,
                                                                    "PartyPres"=-1)),
                                 "PartyPres"=-1,
                                 "Predicted"= predict(fit,data.frame("Democrats" =average$Democrat,
                                                                     "PartyPres"=-1))))

gg <- ggplot(DemAvg,aes(Predicted,DemVote)) +
  geom_smooth(method="lm",se=F) +
  geom_vline(xintercept=50,linetype=2) +
  geom_hline(yintercept=50,linetype=2) +
  geom_point(aes(col=Year==2018),alpha=0.5)+
  geom_point(shape=1,aes(col=Year==2018))+
  geom_label_repel(aes(label=Year,col=Year==2018)) +
  theme_elliott()+
  labs(title="Generic Ballot Polls Predict House Elections Well",
       subtitle=paste0("Evaluating predicted Democratic vote share with generic ballot polls since 1946.\nEstimated error from our predictions* is ",NationalLMError,"%. Margin of error is roughly ",MOE," percentage points."),
       caption= "Error is the RMSE from a linear model setting\nDemocratic vote share = Democratic poll share +  dummy for party in the White House",
       x= "Predicted Democratic Vote Share (Two-Party, %)",
       y= "Actual Democratic Vote Share (Two-Party, %)") +
  coord_cartesian(xlim=c(42,58),ylim=c(42,58)) +
  scale_color_manual(values=c("TRUE"="red","FALSE"="blue"))

grid.arrange(gg,my_g,heights=c(9, .5))
dev.copy(png,"Tracker/Graphics/GBPollsAccuracy.png",height=6,width=8,unit="in",res=350)
dev.off()

detach("package:MASS", character.only = TRUE)
