library(tidyverse)
library(haven)
suppressWarnings(suppressMessages(library(stargazer)))
library(lubridate)
library(zoo)
library(MASS)
source("~/theme_elliott.R")
source("Tracker/functions.R")


rmse <- function(error){
  sqrt(mean(error^2,na.rm=TRUE))
}

RUN_DATE <- Sys.Date() # 

DaysTilEDDUMMY <- round(as.numeric(difftime(ymd("2018-11-06"),ymd(RUN_DATE),"days")));print(DaysTilEDDUMMY)

# data cleaning + prediction ----------

# filter to todays day in each cycle
gbpolls <- suppressMessages(read_csv("Tracker/Data/gbpolls.csv"))  %>% 
  mutate(#pollsum =Democrats + Republicans,
         #votesum =DemVote + RepVote,
         #Democrats = Democrats/pollsum*100,
         #Republicans = Republicans/pollsum*100,
         #DemVote = DemVote/votesum*100,
         #RepVote = RepVote/votesum*100,
         Pres.Poll.Margin = ifelse(PartyPres== 1,Democrats-Republicans,Republicans-Democrats),
         Pres.Vote.Margin = ifelse(PartyPres== 1,DemVote-RepVote,RepVote-DemVote),
         Dem.Vote.Margin = DemVote-RepVote
  )


# daily averages -------------
# (1) for each cycle (2) for every day (3) get window lag (4) get average (5) interpolate missing days (6) grab average for today
#setup DF
DemAvg <- NULL
daily_averages.list <- vector("list",length=length(unique(gbpolls$ElecYear)))

years <- unique(as.numeric(gbpolls$ElecYear))

for(i in 1:length(years)){
  cycle<-years[[i]]
  print(cycle)
  temp <- gbpolls %>% 
    filter(ElecYear==cycle) %>%
    mutate(EndDate = mdy(EndDate))
  
  # dates 
  dates <- as.character(seq.Date(from = min(temp$EndDate),
                                 to = unique(mdy(temp$ElecDay)),by="days"))
  # simple average
  tempavg <-data.frame(NULL)
  pb <- txtProgressBar(min = 0, max = length(dates), style = 3)
  for(j in 1:length(dates)){
    date<-dates[[j]]
    
    days_til_temp <- as.numeric(difftime(mdy(temp[1,]$ElecDay),ymd(date),unit="days"))
    
    # filter with window function
    temp_polls <- temp %>% 
      filter(EndDate >= ymd(date)-get_window(days_til_temp) & EndDate<=ymd(date))
    
    # get days since  
    temp_polls$Days_Since = as.numeric(difftime(ymd(date),temp_polls$EndDate,units="days"))
    
    
    # get weight with window function
    if(nrow(temp_polls)>0){temp_polls$weight <- 1}
    
    # bind average to df
    tempavg <- tempavg %>% 
      bind_rows(data.frame("EndDate" = ymd(date),
                           "Democrat.Avg" = weighted.mean(
                             temp_polls$Democrats,temp_polls$weight),
                           "Republican.Avg" = weighted.mean(
                             temp_polls$Republicans,temp_polls$weight)) )
    
    tempavg
    
    # update progress bar
    setTxtProgressBar(pb, j)
  }
  
  # replace NAN with na
  tempavg[2:length(tempavg)]<-lapply(FUN=function(x){return(ifelse(is.nan(x),NA,x))},tempavg[2:length(tempavg)])
  
  # interpolate averagpbe for missing days
  tempavg$Democrat.Avg.fill <- na.approx(tempavg$Democrat.Avg,na.rm = FALSE)
  tempavg$Republican.Avg.fill <- na.approx(tempavg$Republican.Avg,na.rm = FALSE)
  
  # join interpolated DF with polls DF
  tempavg <- left_join(tempavg,temp,by="EndDate") 
  
  # fill in  missing variables
  tempavg[nrow(tempavg),]$DaysTilED <- 0
  tempavg$DaysTilED <- na.approx(tempavg$DaysTilED,na.rm = FALSE)
  
  tempavg$Midterm <- na.locf(tempavg$Midterm,na.rm = FALSE)
  tempavg$ElecYear <- na.locf(tempavg$ElecYear,na.rm = FALSE)
  tempavg$PartyPres <- na.locf(tempavg$PartyPres,na.rm = FALSE)
  
  tempavg$DemVote <- na.locf(tempavg$DemVote,na.rm = FALSE)
  tempavg$RepVote <- na.locf(tempavg$RepVote,na.rm = FALSE)
  
  tempavg$Democrat.Avg.fill <- na.locf(tempavg$Democrat.Avg.fill,na.rm = FALSE)
  tempavg$Republican.Avg.fill <- na.locf(tempavg$Republican.Avg.fill,na.rm = FALSE)
  
  tempavg$PresApprove <- na.locf(tempavg$PresApprove,na.rm = FALSE)
  tempavg$PresDisapprove <- na.locf(tempavg$PresDisapprove,na.rm = FALSE)
  tempavg$PresApproveNet <- na.locf(tempavg$PresApproveNet,na.rm = FALSE)
  
  tempavg$Dem.Vote.Margin <- na.locf(tempavg$Pres.Vote.Margin,na.rm = FALSE)
  tempavg$Pres.Vote.Margin <- na.locf(tempavg$Pres.Vote.Margin,na.rm = FALSE)
  
  # create Dem/Pres Party margins
  tempavg <- tempavg %>% 
    mutate("Dem.Poll.Margin.Avg" = Democrat.Avg-Republican.Avg,
           "Pres.Poll.Margin.Avg" = ifelse(PartyPres== 1,
                                           Democrat.Avg-Republican.Avg,
                                           Republican.Avg-Democrat.Avg),
           "Dem.Poll.Margin.I" = Democrat.Avg.fill-Republican.Avg.fill,
           "Pres.Poll.Margin.I" = ifelse(PartyPres== 1,
                                  Democrat.Avg.fill-Republican.Avg.fill,
                                  Republican.Avg.fill-Democrat.Avg.fill))
  
  # look
  tail(tempavg)
  
  # some plots
  gg <- ggplot(tempavg,aes(x=DaysTilED))+
    geom_hline(yintercept=0,linetype=2,col="#696969") +
    geom_point(aes(y=Pres.Poll.Margin,shape="individual polls")) + 
    geom_point(aes(y=Pres.Poll.Margin.Avg,shape="average"))+
    geom_line(aes(y=Pres.Poll.Margin.I,linetype="interpolated average"))+
    labs(title=sprintf("Daily average of generic ballot polls in %s",cycle),
         subtitle="Averages for days with missing data (no polls within the past month) are interpolated",
         x="Days Until Election Day",
         y="White House Party's Poll Margin\n(Two-party %)") + 
    theme_elliott() +
    coord_cartesian(xlim=c(0,365))+
    scale_x_reverse()+
    theme(legend.position="bottom",
          legend.title = element_blank()) +
    scale_linetype_manual(values=c("interpolated average" = 2))+
    scale_shape_tremmel() 
  
  print(gg)
  
  png(sprintf("~/Dropbox/2018MidtermForecast/Tracker/Graphics/yearly_averages/gbpollsaverage%s.png",cycle),
      height=6,width=8,units="in",res=350)
  grid.arrange(gg,my_g,heights=c(9,0.5))
  dev.off()
  
  
  # add to list of daily averages
  daily_averages.list[[i]] <- tempavg
}

# bind rows
daily_averages <- do.call("rbind",daily_averages.list)


# attach last-day averages
final_day_polls <- daily_averages %>% 
  mutate(Final.Dem.Margin = Dem.Poll.Margin.I,
         Final.Pres.Margin = Pres.Poll.Margin.I) %>%
    dplyr::select(ElecYear,DaysTilED,Final.Dem.Margin,Final.Pres.Margin) %>% 
    group_by(ElecYear) %>%  
    arrange(DaysTilED) %>% 
    filter(DaysTilED == min(DaysTilED)) %>%
    dplyr::select(-c(DaysTilED))

daily_averages <- daily_averages %>%
  left_join(final_day_polls,by = "ElecYear")


# save dataset
write.csv(daily_averages,"~/Dropbox/2018MidtermForecast/Tracker/Data/daily_ts_avg.csv",row.names = F)

