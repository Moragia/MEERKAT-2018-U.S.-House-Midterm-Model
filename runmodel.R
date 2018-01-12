# setup  -----
Sys.setenv(TZ="US/Central")
rm(list = ls())
source('~/theme_elliott.R')
source('Tracker/functions.R')

# !! INPUT VARIABLES !!  -----------------

RUN_DATE <- Sys.Date()
#RUN_DATE <- as.Date("2018-01-05")   # can be switched out to run on any day you like 

print("-------------------------------------",quote=FALSE)
print(sprintf("      Today is %s           ",RUN_DATE),quote=FALSE)

# data wrangling ----------
# days until election day 
DaysTilEDMaster <- as.numeric(difftime(ymd("2018-11-06"),RUN_DATE,units = "days"))

# import house data 
suppressMessages(my_sheets <- gs_ls())
suppressMessages(gsheet <- gs_title("2018 Midterm Benchmarks"))
suppressMessages(gseats <- data.frame(gs_read(gsheet,ws = "Main Data",range="A3:AR441")))
gseats[is.na(gseats)] <- NA

# retirement data
suppressMessages(retirements <- data.frame(gs_read(gsheet,ws="Retirements")))
retirements <- retirements %>%filter(mdy(Date)<=RUN_DATE)

gseats <- gseats %>%
  mutate(Open.2018. = ifelse(Code %in% retirements$District,"YES",NA))

# download polls
url <- "http://elections.huffingtonpost.com/pollster/api/v2/questions/18-US-House/poll-responses-clean.tsv" # 538: https://projects.fivethirtyeight.com/generic-ballot-data/generic_polllist.csv

suppressMessages(suppressWarnings(download.file(url=url,destfile="Tracker/Data/polls.tsv")))

raw_polls <- as.data.frame(suppressMessages(read_tsv("Tracker/Data/polls.tsv")))

raw_polls <- raw_polls %>%
  mutate(Dem2P = Democrat/(Democrat + Republican)*100,
         GOP2P = Republican/(Democrat + Republican)*100) %>%
  dplyr::select(Democrat,
                Republican,
                Other,
                Undecided,
                "Firm" = survey_house,
                "Start"=start_date,
                "End" =end_date,
                "Population"=sample_subpopulation,
                "Sample Size" =observations,
                "Mode" = mode,
                Dem2P,
                GOP2P) %>%
  mutate(
    "Median" = ymd(End)-difftime(ymd(End),ymd(Start)),
    "Days_Till_ED" = difftime(ymd("2018-11-06"),ymd(Median)),
    `Sample Size` = ifelse(is.na(`Sample Size`),800,`Sample Size`)
    
  ) 


write.csv(raw_polls,"Tracker/Data/polls_formatted.csv",row.names = F)


# !! SETUP LOOP !!  ---------
#for(i in rev(as.character(seq.Date(ymd("2018-01-05"),ymd("2018-01-06"),by="days")))){RUN_DATE <- ymd(i);print(sprintf("Today is %s",i))

SIM_NUMS <- 50000  # these are (obviously) the number of simulations you want to run. 

# average the polls ---------------
# get bounds
polls <- raw_polls %>% filter(ymd(End) <= RUN_DATE) 

# simple average
avg <-data.frame(NULL)

for(i in as.character(seq.Date(from = ymd("2017-06-01"),
                               to = ymd(RUN_DATE),by=1))){
  days_til_temp <- as.numeric(difftime(ymd("2018-11-06"),ymd(i),unit="days"))
  
  # filter with window function
  temp_polls <- polls %>% 
    filter(ymd(Median) >= ymd(i)-get_window(days_til_temp) & ymd(Median)<=ymd(i))
  
  # get days since  
  temp_polls$Days_Since = as.numeric(difftime(ymd(i),temp_polls$Median,units="days"))
  
  
  # get weight with window function
  temp_polls$weight = 1#exponent_weight(temp_polls$Days_Since,get_weight(days_til_temp))
  
  # average
  avg <- avg %>% 
    bind_rows(data.frame("Median" = ymd(i),
                         "Democrat.Avg" = weighted.mean(
                           temp_polls$Democrat,temp_polls$weight,na.rm=TRUE),
                         "Republican.Avg" = weighted.mean(
                           temp_polls$Republican,temp_polls$weight),
                         "Other.Avg" = weighted.mean(
                           temp_polls$Other,temp_polls$weight,na.rm=TRUE),
                         "Undecided.Avg" = weighted.mean(
                           temp_polls$Undecided,temp_polls$weight,na.rm=TRUE),
                         "Num.Obs" = sum(temp_polls$`Sample Size`,na.rm=TRUE)) %>% 
                mutate("Dem.margin" = Democrat.Avg-Republican.Avg))
  
  avg
}

# interpolate average for missing days
avg$Democrat.Avg <- na.locf(avg$Democrat.Avg)
avg$Republican.Avg <- na.locf(avg$Republican.Avg)

avg <- left_join(avg,polls,by="Median") 

gg <- ggplot() + 
  geom_line(data=avg,aes(x=Median,y=Democrat.Avg-Republican.Avg)) +
  geom_point(data=avg,aes(x=Median,y=Democrat-Republican)) +
  geom_smooth(data=avg,aes(x=Median,y=Democrat-Republican),span=0.2)

print(gg)


gg <- ggplot() + 
  geom_line(data=avg,aes(x=Median,y=Democrat.Avg),col="blue") +
  geom_line(data=avg,aes(x=Median,y=Republican.Avg),col="red") +
  geom_line(data=avg,aes(x=Median,y=Other.Avg),col="green") +
  geom_line(data=avg,aes(x=Median,y=Undecided.Avg),col="gray40") +
  geom_point(data=avg,aes(x=Median,y=Democrat),col="blue",alpha=0.5) +
  geom_point(data=avg,aes(x=Median,y=Republican),col="red",alpha=0.5) +
  geom_point(data=avg,aes(x=Median,y=Other),col="green",alpha=0.5) +
  geom_point(data=avg,aes(x=Median,y=Undecided),col="gray40",alpha=0.5) 

print(gg)


# get today's average
average <- avg[nrow(avg),] %>% 
  dplyr::select("Democrat" = Democrat.Avg,"Republican"=Republican.Avg,"Other" = Other.Avg) %>%
  mutate(Undecided = 100-(Democrat + Republican + Other))

num_obs <- avg[nrow(avg),]$Num.Obs
# pooled polls average
#average <- POOL_POLLS(pollDF = polls ,todaysDate = RUN_DATE)


# forecast national polls and votes  ---------------

#natl_dem_pct <- (-1.62 + (0.44 *(average$Democrat-50)) + (-2.65 *-1)) + 50 # coefs from 2014 Bafumi/Wlezien paper, February polling

source("Tracker/ballotLM.R")
# first, predict final polls, then use final polls to predict final votes
natl_pres_votes <- predict(fitEDay,
                           data.frame("Final.Pres.Margin" = 
                                        predict(fit,
                                                data.frame("Pres.Poll.Margin.I"= average$Republican - average$Democrat,
                                                           "Midterm"=1))))

natl_pres_votes

# then, just predict the final polls
natl_pres_margin <- predict(fit,
                            data.frame("Pres.Poll.Margin.I"= average$Republican - average$Democrat, 
                                       "Midterm"=1))

natl_pres_margin

# use predicted VOTES (not polls?) for this
natl_dem_margin <- natl_pres_margin * -1 # -1 because Dems are out of pwr
natl_rep_margin <- natl_pres_margin

# apply to average share
natl_dem_pct <- average$Democrat + (natl_dem_margin - (average$Democrat- average$Republican)) /2 # dem forecast pct = average + the half difference between today's margin and the predicted margin

natl_rep_pct <- average$Republican + (natl_rep_margin - (average$Republican- average$Democrat)) /2 # dem forecast pct is the reverse

natl_other_pct <- average$Other + (natl_dem_margin - (average$Democrat- average$Republican)) /2 # dem forecast pct is the reverse

natl_und_pct = 100 - (natl_dem_pct + natl_rep_pct + natl_other_pct)

# swing is this yr's dem margin minus last year's
NatlDSwingMargin =  natl_dem_margin - -1.1 # the 2p-% dems earned in 2016

# print some info
cat(sprintf("
current POLL margin is -------------- D + %s%%
projected e-day POLL margin is ------ D + %s%% 
projected e-day VOTE margin is ------ D + %s%% 
swing in votes via polls is --------- D + %s%%
polled vote shares: 
          D ------------------------- D: %s%%
          R ------------------------- D: %s%%
          O ------------------------- D: %s%%
          Und/ ---------------------- D: %s%%
projected vote shares: 
          D ------------------------- D: %s%%
          R ------------------------- D: %s%%
          O ------------------------- D: %s%%
          Und/ ---------------------- D: %s%%
",round(average$Democrat-average$Republican,1), round(natl_pres_margin*-1,1),round(natl_pres_votes,1)*-1,round(NatlDSwingMargin,1),round(average$Democrat,1),round(average$Republican,1),round(average$Other,1),round(average$Undecided,1),round(natl_dem_pct,1),round(natl_rep_pct,1),round(natl_other_pct,1),round(natl_und_pct,1)))

# add proj to graph
avg.proj <- avg %>%
  mutate(pres.poll = Republican.Avg - Democrat.Avg,
         linetype="1",
         upper=NA,
         lower=NA) %>%
  dplyr::select(Median,pres.poll,linetype,upper,lower) %>%
  bind_rows(data.frame(Median=ymd("2018-11-06"),
                       pres.poll = natl_pres_margin,
                       linetype="2",
                       upper = natl_pres_margin+MOEErrorUntilEDay,
                       lower = natl_pres_margin-MOEErrorUntilEDay,
                       upper.5 = natl_pres_margin+MOEErrorUntilEDay.5,
                       lower.5 = natl_pres_margin-MOEErrorUntilEDay.5))

avg.proj[(nrow(avg.proj)-1),][,4:7] <- avg.proj[(nrow(avg.proj)-1),]$pres.poll
avg.proj[(nrow(avg.proj)-1):nrow(avg.proj),]$linetype="2"



# agg votes -> seats? yeah, but with error --------------------
# difference between dem vote share & seat share

if(TRUE){
  seats_time <- suppressMessages(read_csv("Data/historical.csv")) %>% 
    mutate(DSeats = 435 * (DemPctSeats/100))
  
  # make a lm
  fit <- lm(DemPctSeats ~ DemPctVotes + DemLagSeats,data=seats_time)
  summary(fit)
  coefs <- broom::tidy(fit)
  
  # project aggregate for this year
  proj_seats <- coefs[1,]$estimate + (coefs[2,]$estimate*(natl_dem_pct/(natl_dem_pct+natl_rep_pct)*100)) + coefs[3,]$estimate * 191 # <- that's the 2016 value
  
  seats_time[seats_time$Year==2018,]$DemPctVotes <- natl_dem_pct/(natl_dem_pct+natl_rep_pct)*100 # add vairables for predict() function
  
  seats_time$PredictedDSeats <- c(predict(fit,seats_time))
  seats_time[seats_time$Year==2018,]$DemPctSeats <- seats_time[seats_time$Year==2018,]$PredictedDSeats
  
  seats_time$color <- seats_time$Year==2018
  prediction <- as.numeric(predict(fit,seats_time)[[37]])
  interval <- as.numeric(predict(fit,seats_time, interval="predict")[37,][3] - predict(fit,seats_time, interval="predict")[37,][2]) / 2
  
  pred.gg <- ggplot(seats_time,aes(PredictedDSeats,DemPctSeats))+
    geom_hline(yintercept = 50,linetype=2) +
    geom_vline(xintercept = 50,linetype=2) +
    geom_smooth(method="lm",se=F,col="black")  +
    geom_point(size=2,alpha=0.4,aes(col=color))+
    geom_label_repel(data=seats_time %>% filter(Year %in% c(2016,2014,2012,2008,2018)),aes(col=color,label=Year),show.legend = FALSE) + 
    geom_errorbar(aes(x=prediction,ymax=prediction+interval,ymin=prediction-interval,col=Year==2018),show.legend = FALSE)+
    scale_color_manual(values=c("FALSE"="blue","TRUE"="red")) +
    theme_elliott()+
    labs(title="House Popular Vote Predicts Seat Share Pretty Well",
         subtitle="It is the nature of the United States House that representation is not always equally\nproportioned, but majority-minority rule has never occurred in modern America",
         x="Predicted Democratic Seats\nBased on Votes & Previous Seats",
         y="Actual Democratic Seats",
         caption="Predicted Seats = Two-party Democratic vote share + Democratic seats last election.\nI predict House Democratic vote share with a separate equation, simply\nD Vote = D Poll + (var that punishes WH party)") +
    scale_x_continuous(breaks=c(40,45,50,55,60,65,70),
                       labels=c(174,196,218,239,261,283,305)) +
    scale_y_continuous(breaks=c(40,45,50,55,60,65,70),
                       labels=c(174,196,218,239,261,283,305)) +
    geom_text(inherit.aes = FALSE,aes(x=60,y=52.8,label="*Based on\nAverage of\nGeneric Ballot Polls"),col="red")
  

  png("Tracker/Graphics/PredictedSeatsVsActual.png",height=6,width=8,unit="in",res=350)
  grid.arrange(pred.gg,my_g,heights=c(9, .5))
  #dev.copy(png,"Graphics/PredictedSeatsVsActual.png",height=6,width=8,unit="in",res=350)
  dev.off()
  
  rm(fit)
}
# so instead, do indiv-lvl seat projections -----------

gseats$Name <- sapply(strsplit(gseats$Incumbent,","), `[`, 1)

seat_proj <- gseats %>% 
  mutate("Obama.2012.Margin" = Obama.2012 - Romney.2012,
         "Clinton.Margin" = Clinton..2016 - Trump..2016, 
         "Dem.House.Margin.Prev.2" = X2014.Dem - X2014.GOP, 
         "Dem.House.Margin.Prev" = X2016.Dem - X2016.GOP) %>%
  dplyr::select(Code,
                Name,
                Obama.2012.Margin,
                Clinton.Margin,
                Dem.House.Margin.Prev.2,
                Dem.House.Margin.Prev,
                "PartisanLean" = PVI.2018.20,
                "Open" = Open.2018, 
                "Freshman" = Freshman.2018,
                Uncontested.Prev = Uncontested.2016,
                Uncontested = Uncontested.2018) %>% # load in data from Gsheet
  mutate(Party = ifelse(Dem.House.Margin.Prev>0,1,-1),
         Open = ifelse(is.na(as.character(Open)),0,1),
         Uncontested.Prev = ifelse(is.na(as.character(Uncontested.Prev)),0,Uncontested.Prev),
         Uncontested = ifelse(is.na(as.character(Uncontested)),0,Uncontested),
         Freshman = ifelse(is.na(as.character(Freshman)),0,Party),
         Incumbent = ifelse(Open == 1,0,Party),
         year=2018) %>% 
  mutate( # If prev race was uncontested, use margin lagged x2
    Dem.House.Margin.Prev = ifelse(Uncontested.Prev!=0,Dem.House.Margin.Prev.2,Dem.House.Margin.Prev),
    forecast_dem_margin = NA)

# regression coefs !! FOR 2016:2016
source("Tracker/seatsLM.R")

# make a precition!
#seat_proj[seat_proj$Incumbent==0,]$forecast_dem_margin <- predict(fit.open, seat_proj[seat_proj$Incumbent==0,]) + NatlDSwingMargin

#seat_proj[seat_proj$Incumbent!=0,]$forecast_dem_margin <- predict(fit.incumb, seat_proj[seat_proj$Incumbent!=0,]) + NatlDSwingMargin

seat_proj$forecast_dem_margin <- predict(fit, seat_proj) + NatlDSwingMargin # the above lines use two diff linear models for projection -- this uses one

# correct uncontested seats
if(nrow(seat_proj[seat_proj$Uncontested ==1,])>0){
  seat_proj[seat_proj$Uncontested ==1,]$forecast_dem_margin <- 100
}
if(nrow(seat_proj[seat_proj$Uncontested ==-1,])>0){
  seat_proj[seat_proj$Uncontested ==-1,]$forecast_dem_margin <- 100
}

# correct for > 100 or < -100
if(nrow(seat_proj[seat_proj$forecast_dem_margin > 100,])>0){
  seat_proj[seat_proj$forecast_dem_margin > 100,]$forecast_dem_margin <- 100
}
if(nrow(seat_proj[seat_proj$forecast_dem_margin < -100,])>0){
  seat_proj[seat_proj$forecast_dem_margin < -100,]$forecast_dem_margin <- -100
}


# investigate
seat_proj <- seat_proj %>% mutate(Dem_Win = forecast_dem_margin > 0)

table(seat_proj$Dem_Win)
seat_proj %>% filter(Dem.House.Margin.Prev>0,forecast_dem_margin<0) # seats dems lose
seat_proj %>% filter(Dem.House.Margin.Prev<0,forecast_dem_margin>0) # seats reps lose

# output the file
write.csv(seat_proj,'Tracker/Data/indiv_seats.csv',row.names = FALSE)

# graph
ggplot(seat_proj,aes(x=Dem.House.Margin.Prev,y=forecast_dem_margin)) + 
  geom_abline(intercept = 0,slope=1) +
  geom_point(aes(col=as.character(Uncontested)))


# simulate --------------

# commence
SIMULATE(seatsDF = seat_proj,
         sims = SIM_NUMS,
         ErrorPollsTodayToEDay = LMErrorUntilEDay,
         NationalErrorSD = NationalLMError,
         DemNatlMargin = natl_dem_margin,
         IncumbRMSE = RMSEincumb,
         OpenRMSE = RMSEopen,
         UncontestedRMSE = RMSEuncontested)

# save simulation output to existing seat DF
seat_proj$DemWinProb <- seat_probs$DemWinProb 
seat_proj$simulated_dem_share <- seat_probs$DemAvgShare # set vote margin as avg sim share
seat_proj$Sim_Dem_Win<-NA
seat_proj$Sim_Dem_Win <- ifelse(seat_proj$forecast_dem_margin>0,TRUE,FALSE)


# print  info
print(paste("baseline sum of Dem seats:",nrow(seat_proj%>%filter(forecast_dem_margin>0))),quote=FALSE)

print(paste("simulation sum  of Dem seats:",nrow(seat_proj%>%filter(Sim_Dem_Win>0))),quote=FALSE)

print(paste("median number of simulated Dem seats:",median(SIMULATIONS$Dem_Seats)),quote=FALSE)
print(paste("probability of Dem victory is",
            round(nrow(SIMULATIONS %>% filter(Dem_Seats>217))/nrow(SIMULATIONS)*100,2),"%"),quote=FALSE)


# graph it 
hist(SIMULATIONS$Dem_Seats)
credible.interval = quantile(SIMULATIONS$Dem_Seats, prob=c(.025, .975))
abline(v=credible.interval, col=2)
abline(v=median(SIMULATIONS$Dem_Seats), col=3, lwd=3)
abline(v=218,col=1,lwd=2)


# write polls, probs, text etc. sims over time --------------

### write file 

vital_stats <- data.frame("Date" = RUN_DATE,
                          "DemPoll" = average$Democrat,
                          "RepPoll" = average$Republican,
                          "OtherPoll" = average$Other,
                          "UndecidedPoll" = average$Undecided,
                          "DemProject" = natl_dem_pct,
                          "RepProject" = natl_rep_pct,
                          "OtherProject" = natl_other_pct,
                          "UndecidedProject" = natl_und_pct,
                          "ModelDemSeats" = nrow(seat_proj%>%filter(Dem_Win)),
                          "MedSimulatedDemSeats" = median(SIMULATIONS$Dem_Seats),
                          "DemProb" = nrow(SIMULATIONS %>% filter(Dem_Seats>217))/nrow(SIMULATIONS),
                          "NumObs" = num_obs)

write.csv(vital_stats,"Tracker/Data/forecast_data.csv",row.names = FALSE)


# add the average to a file of averages
sims_over_time <- suppressMessages(read_csv("Tracker/Data/sims_over_time.csv")) # read the existing time series data

rownames(sims_over_time) <- NULL # remove row names
sims_over_time$Date <- as.Date(sims_over_time$Date) # make sure date is in the correct format

if(RUN_DATE %in% sims_over_time$Date){
  sims_over_time[sims_over_time$Date == RUN_DATE,] <- vital_stats
}else{
  sims_over_time <- bind_rows(sims_over_time,vital_stats)
}

write.csv(sims_over_time,"Tracker/Data/sims_over_time.csv",row.names = FALSE) # write the file!

# graphics output -----------

### histogram
hist.gg <- ggplot(data=SIMULATIONS,aes(x=Dem_Seats))+
  geom_histogram(binwidth = 2,aes(fill=Dem_Seats>217),alpha=0.5) +
  scale_fill_manual(values=c("FALSE"="red","TRUE"="blue"))+
  theme_elliott()+
  labs(title="50,000 Simulations of the 2018 House Elections",
       subtitle="What is the most likely outcome for Democrats?",
       x="Number of Democratic Seats Won",
       y="Likelihood\n(Increasing →)",
       caption = paste0("The number of expected seats according to our linear model is ",nrow(seat_proj %>% filter(forecast_dem_margin>0)),".\nThe number of expected seats according to the median of our simulations is ",median(SIMULATIONS$Dem_Seats),".\nThe mode is ",getmode(SIMULATIONS$Dem_Seats)," and the average is ",round(mean(SIMULATIONS$Dem_Seats))))+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+
  geom_vline(xintercept = 218) + 
  #geom_vline(xintercept = 194,col="gray",linetype=2) +  
  #annotate("text",x=193,y=.07*max(table(SIMULATIONS$Dem_Seats)),label="2016:\n194 Dem",hjust=1,col="black") +
  geom_vline(xintercept = median(SIMULATIONS$Dem_Seats),col="black",linetype=2) +
  annotate("text",x=median(SIMULATIONS$Dem_Seats)+1,y=.07*max(table(SIMULATIONS$Dem_Seats)),label=paste0("Median\n2018:\n",median(SIMULATIONS$Dem_Seats)," Dem"),hjust=0,col="black") +
  
  geom_vline(xintercept = getmode(SIMULATIONS$Dem_Seats),col="blue",linetype=2) +
  annotate("text",x=getmode(SIMULATIONS$Dem_Seats)-1,y=.07*max(table(SIMULATIONS$Dem_Seats)),label=paste0("Most likely\n2018:\n",getmode(SIMULATIONS$Dem_Seats)," Dem"),hjust=1,col="blue") +
  
  annotate("text",x=219,y=.85*max(table(SIMULATIONS$Dem_Seats)),label="Dem→\nMajority",hjust=0) 


png("Tracker/Graphics/simulation_hist.png",height=6,width=8,unit="in",res=350)
grid.arrange(hist.gg,my_g, heights=c(9,0.5))
#dev.copy(png,"Tracker/Graphics/simulation_hist.png",height=6,width=8,unit="in",res=350)
dev.off()


### Vote histogram
SIMULATIONS <- SIMULATIONS %>% 
  mutate(category = 
           case_when(Dem_Vote>0 & Dem_Seats>=218 ~ "D Win Pop. Vote & Win Maj.", 
                     Dem_Vote>0 & Dem_Seats<218 ~ "D Win Pop. Vote & Lose Maj.",
                     Dem_Vote<0 & Dem_Seats>=218 ~ "D Lose Pop. Vote & Win Maj.", 
                     Dem_Vote<0 & Dem_Seats<218 ~ "D Lose Pop. Vote & Lose Maj."))

scenarios <- NULL
categories <- c("D Win Pop. Vote & Win Maj.","D Win Pop. Vote & Lose Maj.","D Lose Pop. Vote & Win Maj.", "D Lose Pop. Vote & Lose Maj.")
ypos = nrow(SIMULATIONS)*c(0.04,0.035,0.03,0.025)

for(i in 1:length(categories)){
  chance = round(nrow(SIMULATIONS %>% 
                  filter(category == categories[[i]])) / nrow(SIMULATIONS) *100,1)
  
  scenarios <- scenarios %>% 
    bind_rows(data.frame("category" = categories[[i]],
                         "chance" = chance,
                         "ypos" = ypos[[i]]))
}

seat_votes_hist.gg <- ggplot(SIMULATIONS,aes(x=Dem_Vote,fill=category))+
  geom_vline(xintercept = 0,linetype=2,col="black")+
  geom_histogram(binwidth=1,col="white")+
  geom_label(data=scenarios,aes(x=-10,y=ypos,label=sprintf("%s: %s%%",category,chance)),col="white",show.legend = FALSE) +
  theme_elliott() +
  labs(title="A Range of Outcomes Are Possible for the 2018 House Midterms",
       subtitle=" ",
       x="Democratic Popular Vote Margin (%)",
       y="Probability") +
  scale_y_continuous(breaks=seq(250,2500,250),
                     labels=paste0((seq(250,2500,250)/nrow(SIMULATIONS)*100),"%")) +
  scale_x_continuous(breaks=seq(-50,50,10)) +
  scale_fill_manual("",values=c("D Win Pop. Vote & Win Maj."="#3498DB",
                                "D Win Pop. Vote & Lose Maj."="#F5B7B1",
                                "D Lose Pop. Vote & Win Maj."="#AED6F1",
                                "D Lose Pop. Vote & Lose Maj."="#E74C3C")) +
  coord_cartesian(ylim=c(0,nrow(SIMULATIONS)*0.05))

print(seat_votes_hist.gg)

png("Tracker/Graphics/outcome_votes_seats_hist.png",height=6,width=8,unit="in",res=350)
grid.arrange(seat_votes_hist.gg,my_g, heights=c(9,0.5))
#dev.copy(png,"Tracker/Graphics/simulation_hist.png",height=6,width=8,unit="in",res=350)
dev.off()

### TS averages chart
pollsTS.gg <- ggplot(avg.proj,aes(x=Median, y=pres.poll*-1, linetype=linetype)) + 
  geom_hline(yintercept=-1.1,linetype=2,col="#F5B7B1") + 
  geom_text(aes(x=RUN_DATE,y=-2,label="2016 D: -1.1%"),col="#F5B7B1") +
  geom_hline(yintercept=-5.7,linetype=2,col="#FADBD8") + 
  geom_text(aes(x=RUN_DATE,y=-6.6,label="2014 D: -5.7%"),col="#FADBD8") +
  geom_hline(yintercept=0,linetype=1,col="black") +
  geom_line() +
  geom_point(data=polls,inherit.aes=FALSE,
             aes(x=Median,y=Democrat-Republican,col=Democrat-Republican>0),alpha=0.3) +
  geom_point(data=polls,inherit.aes=FALSE,
             aes(x=Median,y=Democrat-Republican,col=Democrat-Republican>0),shape=1) +
  geom_linerange(inherit.aes = FALSE,
                 aes(x=Median,ymax=upper*-1,ymin=lower*-1),size=2,col="gray70") +
  geom_linerange(inherit.aes = FALSE,
                 aes(x=Median,ymax=upper.5*-1,ymin=lower.5*-1),size=2,col="gray40") +
  labs(title="What Will U.S. House Polls Say on Election Day 2018?",
       subtitle="Our estimate of election-day polls compared to those up to this point, based on movement in past cycles",
       x="Date",
       y= "Democratic Margin In Generic Ballot Polls (%)",
       caption = sprintf("The shaded regions represent the 60%% and 95%% confidence intervals for our predictions\n of Election Day polls using polls at this point in midterm cycles since %s",yearlim))  +
  theme_elliott()  +
  scale_color_manual(values=c("FALSE"="#E74C3C","TRUE"="#3498DB")) +
  scale_x_date(limits = c(ymd("2017-06-01"),ymd("2018-12-01")),date_labels = "%b '%y",date_breaks = "3 months") +
  coord_cartesian(ylim=c(-10,natl_dem_margin+MOEErrorUntilEDay))


png("Tracker/Graphics/avg_time.png",height=6,width=8,unit="in",res=350)
grid.arrange(pollsTS.gg,my_g, heights=c(9,0.5))
#dev.copy(png,"Tracker/Graphics/simulation_hist.png",height=6,width=8,unit="in",res=350)
dev.off()


# visualizing probability 
win.prob <- nrow(SIMULATIONS %>% filter(Dem_Seats>217))/nrow(SIMULATIONS)
heatmap <- matrix(ifelse(runif(400, min = 0, max = 1)<=win.prob,1,0), nrow = 20)
heatmap.m <- melt(heatmap) %>% mutate("Majority" = ifelse(value>0.5,"Democratic","Republican"))

## probability diagram
probs.gg <- ggplot(heatmap.m, aes(x = X1, y = X2,fill=Majority)) +
  geom_tile(color = "black",alpha=0.6) +
  scale_fill_manual(values=c("Democratic" = "blue","Republican"="red")) +
  labs(title = "Chance that Democrats Win a House Majority in 2018",
       subtitle = paste0("The Result of 20,000 simulations of the 2018 midterm elections.\nDemocrats have a ",round(win.prob*100,2),"% chance of flipping the House")) +
  theme_elliott() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks  = element_blank(),
        legend.position = "bottom") +
  geom_text(aes(x=10,y=21,label="Each Blue Rectangle is a Democratic Majority"))

png("Tracker/Graphics/dem_win_prob.png",height=6,width=8,unit="in",res=350)
grid.arrange(probs.gg,my_g, heights=c(9,0.5))
#dev.copy(png,"Tracker/Graphics/simulation_hist.png",height=6,width=8,unit="in",res=350)
dev.off()

#} # for looping ####

#### WRITE FILES TO OUTPUT FOLDER -------

write.csv(polls,"Tracker/Output/polls_formatted.csv",row.names = F)

write.csv(avg.proj,"Tracker/Output/poll_avg_proj.csv",row.names = F)

write.csv(seat_proj,'Tracker/Output/indiv_seats.csv',row.names = FALSE)

write.csv(sims_over_time,"Tracker/Output/sims_over_time.csv",row.names = FALSE) 

write.csv(SIMULATIONS,"Tracker/Output/SIMULATIONS.csv",row.names = FALSE)

write.csv(tipping_probs,"Tracker/Output/tipping_points.csv",row.names = FALSE)

# find the files that you want
list.of.files <- paste0("Tracker/Output/",list.files("Tracker/Output"))

# copy the files to the new folder
file.copy(list.of.files, "~/Dropbox/Blog/Website/content/data/forecast-2018/",overwrite = TRUE)


# print because done
print("Done!",quote=F)


# empty environment
#rm(list=ls())

