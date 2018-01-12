library(MASS)
library(tidyverse)
library(lubridate)
library(curl)
library(haven)
library(broom)
library(googlesheets)
library(ggrepel)
library(reshape)
library(zoo)
library(dplyr)
library(data.table)


# a functuion to weight polls exponentially by day, scaling for proximity to election day
exponent_weight <- function(i,scale) {
  
  exp(scale*i)
}

weight_data <- data.frame("Days" = c(1:30),"Weights" = exponent_weight(c(1:30),-0.1))

ggplot(weight_data,aes(x=Days,y=Weights,fill=Weights))+geom_bar(stat="identity")

# Mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Function that returns Root Mean Squared Error
rmse <- function(error){
  sqrt(mean(error^2,na.rm=TRUE))
}

# window function for poll average
get_window <- function(days_until_ed){
  if(days_until_ed<=7){
    return(7)
  }
  # max is 45, less is 7
  start = 50 # on june 1; 523 days til
  stop = 7
  iter =  (start - stop) / 523
  window = stop + (days_until_ed*iter)
  return(window)
}

get_weight <- function(days_until_ed){
  if(days_until_ed<=7){
    return(-0.15)
  }
  # max is -0.15
  weight = exponent_weight(days_until_ed,-0.005)
  weight = weight * -0.15
  return(weight)
}

# function to pool polls
POOL_POLLS <- function(pollDF,todaysDate){
  # init vars
  
  daystilelec <- as.numeric(difftime(ymd("2018-11-06"),todaysDate,unit="days"))
  # get weights, scaled by eday
  scale <- case_when(daystilelec >= 309 ~ -0.01,
                     daystilelec < 309 & daystilelec >= 250 ~ -0.025,
                     daystilelec < 250 & daystilelec >= 250 ~ -0.05,
                     daystilelec < 250 & daystilelec >= 158 ~ -0.1,
                     daystilelec < 158 & daystilelec >= 66 ~ -0.15,
                     daystilelec < 66 ~ -0.2)
  
  # trim by day
  dayLIMIT <- case_when(daystilelec >= 300 ~ 28,
                        daystilelec < 300 & daystilelec >= 250 ~ 21,
                        daystilelec < 250 & daystilelec >= 158 ~ 14,
                        daystilelec < 158 & daystilelec >= 66 ~ 7,
                        daystilelec < 66 ~ 7)
  
  
  ##########################.
  # SPLIT UP POLLS 
  ###########################.
  
  # make a new data.frame for the divided polls 
  split_polls <- pollDF[0,]
  
  # iterate through polls, split them up by date 
  for (i in 1:nrow(pollDF)){
    # get the row
    sub_temp <-pollDF[i,]
    
    # get the number of days fielded. Add one since we count days differently than numbers
    days_fielded <- as.numeric(sub_temp$End - sub_temp$Start) + 1 
    
    # get the number of observations per day
    new_obs <- sub_temp$`Sample Size`/days_fielded
    if (isTRUE(new_obs > 1500)){new_obs <- 1500} # limit sample size
    
    ##### duplicate the column [day_fielded] times with the new number of observations
    
    # first, make the new data set variable
    sub_temp_new <- sub_temp[0,]
    
    # then duplicate the row and inputnew_obs
    for(j in 1:days_fielded){
      sub_temp_new_idx <- sub_temp
      # new obs number
      sub_temp_new_idx$`Sample Size` <- new_obs
      # new date
      sub_temp_new_idx$Date <- sub_temp_new_idx$End  - j + 1
      
      
      sub_temp_new <- rbind(sub_temp_new, sub_temp_new_idx)
    }
    
    # new days variable 
    
    sub_temp_new <- sub_temp_new %>%
      mutate(Days = as.numeric(difftime(todaysDate,Date,units="days"))) 
    
    # add this divided poll to the new dataframe (susbet_new)
    split_polls <- rbind(split_polls,sub_temp_new)
  }
  
  # put it in a real var
  pooled_polls<- split_polls
  
  num_obs <- sum(pooled_polls %>% filter(Days <= dayLIMIT) %>% pull(`Sample Size`),na.rm=TRUE)
  
  ############################.
  # 1) POOL BY DAY, process:
  # 2)     setting each candidate % to: 
  # 3)     ( (N[a] * %[a]) + (N[b] * %[b]) ) / (N[a] + N[b])
  ############################.
  # prelim vars
  pooled_polls <- pooled_polls[!is.na(pooled_polls$`Sample Size`),]
  pooled_polls <- pooled_polls[order(pooled_polls$Days),]
  subset <- pooled_polls
  dates <- unique(subset$Days)
  
  pooled_polls_final <- data.frame("Days_Till_Election" = NA,"Democrat" = NA,"Republican" = NA)[0,]
  
  for (i in 1:(length(dates))){
    date <- dates[i]
    temp <- subset[subset$Days == date,]
    
    
    # Dem Number for day x
    new_Dem_temp <- 0
    for (j in 1:nrow(temp)){
      new_Dem_temp <- new_Dem_temp + (temp$`Sample Size`[j] *
                                        temp$Democrat[j])
    }
    new_Dem_temp <- new_Dem_temp / sum(temp$`Sample Size`)
    
    # GOP Number for day x
    new_Rep_temp <- 0
    for (j in 1:nrow(temp)){
      new_Rep_temp <- new_Rep_temp + (temp$`Sample Size`[j] *
                                        temp$Republican[j])
    }
    new_Rep_temp <- new_Rep_temp / sum(temp$`Sample Size`)
    
    
    pooled_polls_final <- rbind(pooled_polls_final,
                                data.frame("Days" = date, 
                                           "Democrat" = new_Dem_temp,
                                           "Republican" = new_Rep_temp,
                                           "Days_Till_Election" = date+daystilelec
                                ))
  }
  
  # average it for every day ------------
  
  pooled_polls_final$Median <- ymd("2018-11-06") - pooled_polls_final$Days_Till_Election
  
  # simple average
  avg <-data.frame(NULL)
  
  for(i in as.character(seq.Date(from = ymd("2017-06-01"),
                                 to = ymd(Sys.Date()),by=1))){
    
    temp_polls <- pooled_polls_final %>% 
      filter(ymd(Median) >= ymd(i)-14 & ymd(Median)<=ymd(i))
    
    temp_polls$Days_Since = as.numeric(difftime(ymd(i),temp_polls$Median,units="days"))
    temp_polls$weight = exponent_weight(temp_polls$Days_Since,-0.01)
    
    avg <- avg %>% 
      bind_rows(data.frame("Median" = ymd(i),
                           "Democrat.Avg" = weighted.mean(
                             temp_polls$Democrat,temp_polls$weight),
                           "Republican.Avg" = weighted.mean(
                             temp_polls$Republican,temp_polls$weight)) %>% 
                  mutate("Dem.margin" = Democrat.Avg-Republican.Avg))
    
    avg
  }
  
  # interpolate average for missing days
  avg$Democrat.Avg <- na.locf(avg$Democrat.Avg)
  avg$Republican.Avg <- na.locf(avg$Republican.Avg)
  
  avg <- left_join(avg,pooled_polls_final,by="Median") 

  ggplot() + 
    geom_line(data=avg,aes(x=Median,y=Democrat.Avg-Republican.Avg)) +
    geom_point(data=avg,aes(x=Median,y=Democrat-Republican)) +
    geom_smooth(data=avg,aes(x=Median,y=Democrat-Republican),span=0.2)
  

  # get today's average
  averages <- avg[nrow(avg),] %>% 
    dplyr::select("Democrat" = Democrat.Avg,"Republican"=Republican.Avg) 
  
  averages <- cbind("Date"= todaysDate,data.frame(t(unlist(averages)))) # stick it in a data frame with today's date
  
  assign(x="num_obs",value=num_obs, envir = globalenv())
  return(averages)
}

# corrrelation matrix
source("Tracker/gen_corr_matrix.R")

# seatsDF <- seat_proj;sims<-2000;NationalErrorSD<-5;ErrorPollsTodayToEDay<-LMErrorUntilEDay;DemNatlMargin<-natl_dem_margin; IncumbRMSE<-RMSEincumb; OpenRMSE<-RMSEopen; UncontestedRMSE<-RMSEuncontested

SIMULATE <- function(seatsDF,sims,ErrorPollsTodayToEDay,NationalErrorSD,DemNatlMargin,IncumbRMSE,OpenRMSE,UncontestedRMSE){
  # create list for the simualted Dem number of seats
  Dem_seats<- rep(NA,sims) 
  
  # vector and df for seat probabilities
  seat_probs.list <- vector("list",sims) # list version
  seat_probs <- as.data.frame(matrix(NA, ncol = 435, nrow = sims)) # data frame
  names(seat_probs) <- seatsDF$Code
  
  # error from today's polls to final polls
  ErrorPollsTodayToEDay.list <- rnorm(n=sims,mean=0,sd=ErrorPollsTodayToEDay) # gen national errors
  
  # error from final polls to votes
  natlError.list <- rnorm(n=sims,mean=0,sd=NationalErrorSD) 
  
  # list of popular votes 
  Dem_Pop_Vote_List <- natl_dem_margin + natlError.list + ErrorPollsTodayToEDay.list

  # lists for tipping points 
  tipping_points <-  vector("list",sims) # list version
  
  # simulate
  print(sprintf("Simulating %s trials",sims))
  pb <- txtProgressBar(min = 0, max = sims, style = 3)
  for(i in 1:sims){ # for every simulation, grab national error, gen district-specific errors
    
    # national-level error
    natlError <- natlError.list[[i]] + ErrorPollsTodayToEDay.list[[i]]
    
    # seat-level errors
    seatsDF$randomOPEN <- CorrelatedRnorm()*OpenRMSE # the RMSE from our open seat lm, in file seatsLM.R
    seatsDF$randomINCUMB <- CorrelatedRnorm()*IncumbRMSE # RMSE for incumbents lm
    seatsDF$randomUNCONTESTED <- CorrelatedRnorm()*UncontestedRMSE # RMSE for uncontested lm
    
    # for every seat, randomly vary it based on 1 natl move and 2.5 error
    seatsDF <- seatsDF %>% mutate(
      forecast_mod = forecast_dem_margin + natlError + case_when(Incumbent == 0 ~ randomOPEN,
                                                              Incumbent != 0 ~ randomINCUMB,
                                                              Uncontested != 0~ randomUNCONTESTED),
      Dem_Win_Sim = forecast_mod>0
    )
    
    natlError
    #mean(seatsDF$forecast_mod - seatsDF$forecast_dem_margin) 
    
    dwin <- nrow(seatsDF %>% filter(Dem_Win_Sim)) # add number of seats 1 to the vector
    dwin
    
    Dem_seats[[i]] <- as.numeric(dwin)
    #hist(seatsDF$forecast_mod - seatsDF$forecast_dem_pct)
    #nrow(seatsDF %>% filter(!Dem_Win,Dem_Win_Sim))
    #nrow(seatsDF %>% filter(Dem_Win,!Dem_Win_Sim))
    
    # add simulated district win to the DF
    #seat_probs[i,] <- seatsDF$forecast_mod
    seat_probs.list[[i]] <- seatsDF$forecast_mod
    
    # for tipping point
    if(dwin>=218){
      tipping_points[[i]] <- seatsDF %>%
        arrange(desc(forecast_mod)) %>%
        filter(row_number() ==218) %>%
        pull(Code)
    }else{
      tipping_points[[i]] <- seatsDF %>%
        arrange(forecast_mod) %>%
        filter(row_number() == 218) %>%
        pull(Code)
    }

    
    # update progress bar
    setTxtProgressBar(pb, i)
  }
  
  # combine output
  seat_probs <- do.call("cbind",seat_probs.list)
  
  # inspect
  max(Dem_seats)
  min(Dem_seats)
  median(Dem_seats)
  #hist(Dem_seats)
  
  # setup probabilities for each seat
  seat_probs.s <- data.frame("Seat" = seatsDF$Code,
                            "DemAvgShare" = rep(NA,435),
                           "DemWinProb" = rep(NA,435))
  
  # get probabilities for each seat
  print("Seat Probs")
  pb <- txtProgressBar(min = 0, max = nrow(seat_probs.s), style = 3)
  for(i in 1:nrow(seat_probs.s)){
    # get shares for that seat
    shares <- as.numeric( seat_probs[i,])
    
    # get average share
    avg_share <- mean(shares)
    
    # get prob
    seat_D_win_prob <- (length(shares[shares>0])/length(shares))*100
    
    # correct prob
    prob <- case_when(seat_D_win_prob >= 99 ~99, 
                      seat_D_win_prob <= 1 ~ 1,
                      TRUE ~ seat_D_win_prob)
    
    # write
    seat_probs.s[i,]$DemAvgShare <- avg_share
    
    seat_probs.s[i,]$DemWinProb <- prob

    # update progress bar
    setTxtProgressBar(pb, i)
  }
  
  seat_probs <- seat_probs.s
  
  # setup for tipping point calculator 
  tipping_points <- unlist(tipping_points)
  tipping_points.df <- data.frame(tp_seat = tipping_points,
                                  demseats = Dem_seats)
  tipping_points.dem <- tipping_points.df %>% 
    filter(demseats>=218) %>%
    pull(tp_seat)
  
  tipping_points.gop <- tipping_points.df %>% 
    filter(demseats<218) %>%
    pull(tp_seat)
  
  tipping_probs  <- vector('list',nrow(seatsDF))
  
  # calc tipping points
  print("Tipping Points")
  pb <- txtProgressBar(min = 0, max = nrow(seatsDF), style = 3)
  for(i in 1:nrow(seatsDF)){
    # get seat
    seat<-seatsDF[i,]$Code
    
    # get a df of seat shares and averages
    seat_shares.df <- data.frame(
      forecast_mod = lapply(seat_probs.list,
                           FUN = function(x){return(x[[i]])}) %>% 
        as.numeric,
      seats_won = Dem_seats
    )
    
    # classic tipping point thing
    tipping_prob <- length(tipping_points[tipping_points == seat])/length(tipping_points)* 100
    
    # if d/gop values more
    dem_tipping_prob <- length(tipping_points.dem[tipping_points.dem == seat])/length(tipping_points.dem)* 100
    gop_tipping_prob <- length(tipping_points.gop[tipping_points.gop == seat])/length(tipping_points.gop)* 100
    

    # residaul probability that Ds win the House if they win the seat MINUS residual prob that they win the House if the lose the seat
    seat_shares.df %>% filter(forecast_mod > 50) %>% nrow() / sims
    
    MajInd <- 
      (
        (seat_shares.df %>% 
         filter(forecast_mod > 0) %>%
         filter(seats_won>=218) %>% 
         nrow() - 
          (seat_shares.df %>% 
             filter(forecast_mod > 0) %>%
             filter(seats_won<=217) %>%
             nrow()) )  -
       (seat_shares.df %>% 
         filter(forecast_mod < 0) %>%
         filter(seats_won>=218) %>% 
         nrow() - 
          (seat_shares.df %>% 
             filter(forecast_mod < 0) %>%
             filter(seats_won<=217) %>%
             nrow()) )
       ) / sims * 100
    
    MajInd
    
    
    tipping_probs[[i]] <- data.frame("Code" = seat, 
                           "Prev.D,vote" = seatsDF %>% filter(Code==seat) %>% pull(Dem.House.Margin.Prev),
                           "D.Forecast"= seat_probs %>% filter(Seat==seat) %>% pull(DemAvgShare),
                           "D.Win.Prob" = seat_probs %>% filter(Seat==seat) %>% pull(DemWinProb)) %>%
                  mutate(
                           "Dem.Residual.Prob" = D.Win.Prob - ((length(Dem_seats[Dem_seats>=218])/length(Dem_seats)*100)),
                           "Tipping.Point" = tipping_prob,
                           "D.Tipping.Point" = dem_tipping_prob,
                           "R.Tipping.Point" = gop_tipping_prob,
                           "MPI" = MajInd)
    
    # update progress bar
    setTxtProgressBar(pb, i)
  }
  
  # inspect
  tipping_probs <- rbindlist(tipping_probs)
  
  #View(tipping_probs)
  
  # !! assign global values then done !! 
  assign("SIMULATIONS", data.frame("Dem_Seats" = Dem_seats,"Dem_Vote"=Dem_Pop_Vote_List),envir = .GlobalEnv)
  assign("seat_probs", seat_probs,envir = .GlobalEnv)
  assign("tipping_probs", tipping_probs,envir = .GlobalEnv)
  
}


