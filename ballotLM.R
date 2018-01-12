# this file forecasts movement in the polls from today until election day and measures 
# it also gets the forecast error for the actual election day vote share
# NOTE: THE MIDTERM VARIABLE HAS NO SIGNIFICANCE AFTER 1970 ! 

# setup ------------
suppressWarnings(suppressMessages(library(tidyverse)))
suppressWarnings(suppressMessages(library(haven)))
suppressWarnings(suppressMessages(library(stargazer)))
suppressWarnings(suppressMessages(library(lubridate)))
suppressWarnings(suppressMessages(library(zoo)))
suppressWarnings(suppressMessages(library(MASS)))
suppressWarnings(suppressMessages(library(broom)))
source("~/theme_elliott.R")

rmse <- function(error){
  sqrt(mean(error^2,na.rm=TRUE))
}

yearlim <- 1946 #since 1946 yields best stability in prediction (but a 30% MOE)
yearlim.final <- 1946
#RUN_DATE<- ymd("2018-10-01") # RUN_DATE <- Sys.Date() # 

DaysTilEDDUMMY <- round(as.numeric(difftime(ymd("2018-11-06"),ymd(RUN_DATE),"days")))

# a prelim LM ----------

# filter to todays day in each cycle
gbpolls <- suppressMessages(read_csv("Tracker/Data/gbpolls.csv"))  %>% 
  filter(ElecYear >= yearlim) %>% filter(Midterm == 1) %>%
  mutate(Days =  DaysTilED - DaysTilEDDUMMY) %>%  # need the date for each indiv poll
  filter(Days <= get_window(DaysTilEDDUMMY)/2 & Days >= -get_window(DaysTilEDDUMMY)/2) %>%
  mutate(#pollsum =Democrats + Republicans,
         #votesum =DemVote + RepVote,
         #Democrats = Democrats/pollsum*100,
         #Republicans = Republicans/pollsum*100,
         #DemVote = DemVote/votesum*100,
         #RepVote = RepVote/votesum*100,
         Pres.PollMargin = ifelse(PartyPres== 1,Democrats-Republicans,Republicans-Democrats),
         Pres.VoteMargin = ifelse(PartyPres== 1,DemVote-RepVote,RepVote-DemVote),
         DemPollMargin = Democrats-Republicans,
         DemVoteMargin = DemVote-RepVote
         ) %>% 
  group_by(ElecYear) %>%
  summarise(Pres.PollMargin = mean(Pres.PollMargin),
            Pres.VoteMargin = unique(Pres.VoteMargin),
            DemPollMargin = mean(DemPollMargin), 
            DemVoteMargin = unique(DemVoteMargin),
            PresApproveNet = mean(PresApproveNet))


fitPrelim <- lm(Pres.VoteMargin ~ Pres.PollMargin, gbpolls)
summary(fitPrelim)
predict(fitPrelim,
        data.frame("Pres.PollMargin" = average$Republican - average$Democrat))

# get averages from daily averages, then filter to today  ------------
#source("~/Dropbox/2018MidtermForecast/Tracker/create_ts_interp_avg.R")
daily_averages <- read.csv("~/Dropbox/2018MidtermForecast/Tracker/Data/daily_ts_avg.csv",stringsAsFactors = FALSE) %>% 
  filter(ElecYear >= yearlim) 

if(DaysTilEDDUMMY > 7){
  daily_averages  <- daily_averages %>% 
    filter(DaysTilED >= DaysTilEDDUMMY-get_window(DaysTilEDDUMMY)/2  & DaysTilED <= DaysTilEDDUMMY+get_window(DaysTilEDDUMMY)/2)
}else{
  daily_averages  <- daily_averages %>% 
    filter(DaysTilED <=7) # if 7 days out or less, use pollling not projection
}

  
DemAvg <- daily_averages %>%
  dplyr::select(ElecYear,
                PartyPres,
                PresApproveNet,
                Dem.Poll.Margin.Avg,
                Pres.Poll.Margin.Avg,
                Dem.Poll.Margin.I,
                Pres.Poll.Margin.I,
                Final.Pres.Margin,
                Final.Dem.Margin,
                Dem.Vote.Margin,
                Pres.Vote.Margin,
                Midterm) %>%
  group_by(ElecYear) %>%
  summarise(PartyPres = unique(PartyPres),
            PresApproveNet = unique(PresApproveNet),
            Midterm = unique(Midterm),

            Dem.Poll.Margin.Avg = mean(Dem.Poll.Margin.Avg),
            Dem.Poll.Margin.I = mean(Dem.Poll.Margin.I),
            Final.Dem.Margin = mean(Final.Dem.Margin),
            Dem.Vote.Margin = mean(Dem.Vote.Margin),
            
            Pres.Poll.Margin.Avg = mean(Pres.Poll.Margin.Avg),
            Pres.Poll.Margin.I = mean(Pres.Poll.Margin.I),
            Final.Pres.Margin = mean(Final.Pres.Margin),
            Pres.Vote.Margin = mean(Pres.Vote.Margin)
)

DemAvg

# do an LM, final poll = poll avg today

fit <- lm(Final.Pres.Margin ~ Pres.Poll.Margin.I + Midterm, DemAvg)
summary(fit)


# predict and evaluate
DemAvg$Predicted <- predict(fit,DemAvg)

LMErrorUntilEDay <- round(rmse(DemAvg$Predicted - DemAvg$Pres.Poll.Margin.I),1);LMErrorUntilEDay
MOEErrorUntilEDay <- round(LMErrorUntilEDay*1.96,1);MOEErrorUntilEDay # 95% CI
MOEErrorUntilEDay.5 <- round(LMErrorUntilEDay*0.85,1) # 60% CI


# add 2018, then graph ---------
# add prediction 
DemPredictions <- rbind(DemAvg %>% 
                 dplyr::select(ElecYear,Pres.Poll.Margin.I,Final.Pres.Margin,Predicted,Midterm),
               data.frame("ElecYear" = 2018,
                          "Pres.Poll.Margin.I" = average$Republican - average$Democrat,
                          "Final.Pres.Margin" = predict(fit,data.frame("Pres.Poll.Margin.I" =average$Republican-average$Democrat,"Midterm"=1)),
                          "Predicted"= predict(fit,data.frame("Pres.Poll.Margin.I" =average$Republican-average$Democra,"Midterm"=1)),
                          "Midterm"=1)
               ) %>% 
  mutate(Error = Final.Pres.Margin - Predicted)

tail(DemPredictions)



# graph
gg <- ggplot(DemPredictions,aes(Final.Pres.Margin,Predicted)) +
  geom_linerange(data=tail(DemPredictions,1),
                 aes(ymin = Final.Pres.Margin -MOEErrorUntilEDay,
                     ymax = Final.Pres.Margin+MOEErrorUntilEDay),
                 col="black",alpha=0.4,size=2) +
  geom_linerange(data=tail(DemPredictions,1),
                 aes(ymin = Final.Pres.Margin -MOEErrorUntilEDay.5,
                     ymax = Final.Pres.Margin+MOEErrorUntilEDay.5),
                 col="black",alpha=0.8,size=2) +
  geom_abline(intercept=0,slope=1,col="blue",linetype=2)+
  geom_smooth(method="lm",se=F,color="blue",linetype=1) +
  geom_vline(xintercept=0,linetype=2) +
  geom_hline(yintercept=0,linetype=2) +
  geom_point(aes(col=ElecYear==2018),alpha=0.5) +
  geom_point(shape=1,aes(col=ElecYear==2018),size=2)+
  geom_label_repel(aes(label=ElecYear,col=ElecYear==2018)) +
  theme_elliott()+
  labs(title="Predicting Movement in Generic Ballot Polls",
       subtitle=sprintf("The margin of error for our prediction of where the polls well end up on election day, based on\npolls taken %s days out, is +/- %s%% percentage points.",DaysTilEDDUMMY,MOEErrorUntilEDay),
       x= "Predicted White House Party Margin \nIn Polls On Election Day (%)",
       y= "Actual White House Party Margin \nIn Polls On Election Day (%)",
       caption= "The dashed line is a hypothetical perfect fit whereas the solid line is the fit for today") +
  scale_color_manual(values=c("TRUE"="red","FALSE"="blue"))


png("Tracker/Graphics/Predicting-E-Day-Polls_With-Today.png",height=6,width=8,unit="in",res=350)
grid.arrange(gg,my_g,heights=c(9, .5))
dev.off()

detach("package:MASS", character.only = TRUE)


predicted2018 <- DemPredictions[nrow(DemPredictions),]$Predicted
predicted2018


# what is the national error on election day? ------------

final_averages <- read.csv("Tracker/Data/daily_ts_avg.csv") %>%
  filter(ElecYear >= yearlim.final) %>% 
  dplyr::select(ElecYear,Final.Pres.Margin,Pres.Vote.Margin,PartyPres,PresApproveNet,Midterm) %>% 
  group_by(ElecYear) %>%  
  summarise(Final.Pres.Margin = median(Final.Pres.Margin,na.rm=TRUE),
            Pres.Vote.Margin = median(Pres.Vote.Margin,na.rm=TRUE),
            PartyPres = median(PartyPres),
            PresApproveNet = median(PresApproveNet)) 

fitEDay <- lm(Pres.Vote.Margin ~ Final.Pres.Margin, final_averages)
summary(fitEDay)

final_averages <- final_averages %>%
  mutate(error = Final.Pres.Margin - Pres.Vote.Margin,
         Prediction = predict(fitEDay,final_averages),
         LMError = Prediction - Pres.Vote.Margin)


NationalLMError <- rmse(final_averages$error)
NationalLMError*1.96

# sample equations for days 500 to 0 from ED -------
if(FALSE){
  looped_projections.list <- vector('list',366)

  for(i in 0:523){
    print(i)
    
    old_polls <- read.csv("~/Dropbox/2018MidtermForecast/Tracker/Data/daily_ts_avg.csv",stringsAsFactors = FALSE) %>% 
      filter(ElecYear >= yearlim) %>% 
      filter(Midterm == 1)
    
    if(i > 7){
      old_polls  <- old_polls %>% 
        filter(DaysTilED >= i-get_window(i)/2  & DaysTilED <= i+get_window(i)/2)
    }else{
      old_polls  <- old_polls %>% 
        filter(DaysTilED==0) # if 7 days out or less, use pollling not projection
    }
    

    old_polls <-  old_polls %>%
      dplyr::select(ElecYear,
                    PartyPres,
                    PresApproveNet,
                    Dem.Poll.Margin.I,
                    Pres.Poll.Margin.I,
                    Final.Pres.Margin,
                    Final.Dem.Margin,
                    Dem.Vote.Margin,
                    Pres.Vote.Margin) %>%
      group_by(ElecYear) %>%
      summarise(PartyPres = unique(PartyPres),
                
                Dem.Poll.Margin.I = mean(Dem.Poll.Margin.I),
                Final.Dem.Margin = mean(Final.Dem.Margin),
                Dem.Vote.Margin = mean(Dem.Vote.Margin),
                
                Pres.Poll.Margin.I = mean(Pres.Poll.Margin.I),
                Final.Pres.Margin = mean(Final.Pres.Margin),
                Pres.Vote.Margin = mean(Pres.Vote.Margin)
      )
    
      
    fit_loop <- lm(Final.Pres.Margin ~ Pres.Poll.Margin.I, old_polls)
    summary(fit_loop)
    
    # get error
    old_polls$prediction <- predict(fit_loop,old_polls)
    error_2day <- rmse(old_polls$Final.Pres.Margin - old_polls$prediction)
  
    # predict votes, get error
    fit_votes <- lm(Pres.Vote.Margin ~ Pres.Poll.Margin.I, old_polls)
    summary(fit_votes)
    
    # get error
    old_polls$prediction.vote <- predict(fit_votes,old_polls)
    error_2day.votes <- rmse(old_polls$Pres.Vote.Margin - old_polls$prediction.vote)
    
    
    # today's numbers
    today <- data.frame("DaysTilED"=i,
                        "Proj.Pres.Minus.Ten" = predict(fit_loop,data.frame("Pres.Poll.Margin.I" =average$Republican - average$Democrat)),
                        "Proj.Votes"= predict(fit_votes,data.frame("Pres.Poll.Margin.I" =average$Republican - average$Democrat))) %>%
      mutate(RMSE_today = error_2day,
             RMSE_today.votes = error_2day.votes)
                                                       
    looped_projections.list[[i+1]] <- today
    
  }
  
  looped_projections <- do.call("rbind",looped_projections.list)
  
  write.csv(looped_projections,"Tracker/Data/ED_Poll_Forecast_At_Day_X.csv",row.names = FALSE)

  
  # graph 
  gg <- ggplot(looped_projections,aes(x=DaysTilED,y=Proj.Pres.Minus.Ten)) +
    geom_ribbon(aes(ymin=Proj.Pres.Minus.Ten-(RMSE_today*1.96),
                    ymax=Proj.Pres.Minus.Ten+(RMSE_today*1.96)),
                fill="blue",alpha=0.1) + 
    geom_ribbon(aes(ymin=Proj.Pres.Minus.Ten-(RMSE_today*.85),
                    ymax=Proj.Pres.Minus.Ten+(RMSE_today*.85)),
                fill="blue",alpha=0.4) + 
    geom_line(size=1.4,col="blue") +
    geom_hline(yintercept=average$Republican - average$Democrat,linetype=2,col="gray") +
    geom_vline(xintercept = DaysTilEDDUMMY,col="black",linetype=2) +
    geom_text(aes(x=DaysTilEDDUMMY+5,
                  y=looped_projections[looped_projections$DaysTilED == DaysTilEDDUMMY,]$Proj.Pres.Minus.Ten -looped_projections[looped_projections$DaysTilED == DaysTilEDDUMMY,]$RMSE_today*1.96 - 5,label="Today"),angle=90) +
    labs(title = "Predicting Eventual White House Party Poll Margin\nFrom One Year Out Until Election Day",
         subtitle = sprintf("What would we expect to happen on Election Day if the party controlling the\nWhite House is %s %s%% in generic ballot polls at day ___ ?",ifelse(average$Republican - average$Democrat>0,"up","down"),round(average$Republican - average$Democrat,1)),
         x = "Days Until Election Day",
         y = "Predicted White House Margin\nIn Elec. Day Polls (Two-party %)",
         caption = sprintf("The shaded regions represent the 60%% and 95%% confidence intervals\nfor our predictions of Election Day polls using polls at this point in midterm cycles since %s",yearlim)) +
    theme_elliott() +
    scale_x_reverse()
  
  grid.arrange(gg,my_g,heights=c(9,0.5))
  
  png("Tracker/Graphics/TS.Pres.At.10.Margin.png",height=6,width=8,unit="in",res=350)
  grid.arrange(gg,my_g,heights=c(9,0.5))
  dev.off()
  
  
  # projected vote share
  
  
  gg <- ggplot(looped_projections,aes(x=DaysTilED,y=Proj.Votes)) +
    geom_ribbon(aes(ymin=Proj.Votes-(RMSE_today.votes*1.96),
                    ymax=Proj.Votes+(RMSE_today.votes*1.96)),
                fill="blue",alpha=0.1) + 
    geom_ribbon(aes(ymin=Proj.Votes-(RMSE_today.votes*.85),
                    ymax=Proj.Votes+(RMSE_today.votes*.85)),
                fill="blue",alpha=0.4) + 
    geom_line(size=1.4,col="blue") +
    geom_vline(xintercept = DaysTilEDDUMMY,col="black",linetype=2) +
    geom_text(aes(x=DaysTilEDDUMMY+5,
                  y=looped_projections[looped_projections$DaysTilED == DaysTilEDDUMMY,]$Proj.Votes -looped_projections[looped_projections$DaysTilED == DaysTilEDDUMMY,]$RMSE_today.votes*1.96 - 5,label="Today"),angle=90) +
    labs(title = "Predicting White House Party Vote Margin\nFrom One Year Out Until Election Day",
         subtitle = "What votes should we expect on Election Day if the party controlling the\nWhite House is up 10% in generic ballot polls at day ___ ?",
         x = "Days Until Election Day",
         y = "Predicted White House Vote Margin\n(Two-party %)",
         caption = sprintf("The shaded regions represent the 60%% and 95%% confidence intervals\nfor our predictions of Election Day polls using polls at this point in midterm cycles since %s",yearlim)) +
    theme_elliott() +
    scale_x_reverse()
  
  grid.arrange(gg,my_g,heights=c(9,0.5))
  
  png("Tracker/Graphics/TS.Pres.At.10.Margin.VOTES.png",height=6,width=8,unit="in",res=350)
  grid.arrange(gg,my_g,heights=c(9,0.5))
  dev.off()
  
  
}
