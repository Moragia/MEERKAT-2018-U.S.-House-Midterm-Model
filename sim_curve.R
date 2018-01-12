# libraries  -----------
rm(list=ls())
library(tidyverse)
library(gridExtra)
library(googlesheets)
source("~/Desktop/theme_elliott.R")
source("Tracker/seatsLM.R")
source("Tracker/functions.R")

# house stuff 
my_sheets <- gs_ls()
suppressMessages(gsheet <- gs_title("2018 Midterm Benchmarks"))
suppressMessages(gseats <- data.frame(gs_read(gsheet,ws = "Main Data",range="A3:X441")))
gseats[is.na(gseats)] <- NA


# setup -------------
SIM_NUMS <- 2000

vital_stats <- data.frame("Projection" = 50,"DemSeats" = 194,"Med" = 193,"DemProb" = 0)

write.csv(vital_stats,"Tracker/Data/sims_for_curve.csv",row.names = FALSE)

seatsequence <- seq(35,65,by = 1)
# do the looping -----------
for (dpctseq in seatsequence){
  print(dpctseq)
  natl_dem_pct <- dpctseq 
  natl_rep_pct <- 100 - natl_dem_pct
  
  swing =  natl_dem_pct - 49.4 # the 2p% dems earned in 2016

  # predict based on incumbency advantage
  seats <- gseats %>% select(Code,"Open" = Open.2018., "Clinton" = Clinton..2016, "Trump" = Trump..2016, "Dem" = X2016.Dem, "GOP" = X2016.GOP,"Freshman" = Freshman.2018,"D2014"=X2014.Dem,"R2014" = X2014.GOP) %>% # load in data from Gsheet
    mutate(Party = ifelse(GOP>Dem,-1,1),
           Trump=as.numeric(Trump),
           Clinton=as.numeric(Clinton),
           Dem=as.numeric(Dem),
           GOP=as.numeric(GOP)) %>%
    mutate(Open=ifelse(is.na(Open),0,Open),
           Open = ifelse(as.character(Open) == "YES",1,0),
           Freshman=ifelse(is.na(Freshman),0,Freshman),
           Freshman = ifelse(as.character(Freshman) == "YES",Party,0),
           psum = Trump + Clinton,
           csum = Dem + GOP,
           Dem = (Dem/csum)*100,
           GOP = (GOP/csum)*100,
           Trump=(Trump/psum)*100,
           Clinton=(Clinton/psum)*100,
           # get the lagged dvp for empty districts
           csumlag = D2014 + R2014,
           Dlag = (D2014/csumlag)*100,
           Dem = ifelse(Dem %in% c(0,100),Dlag,Dem)) %>% 
    select(-c(psum,csum,csumlag,Dlag)) %>% 
    mutate(incumbent = ifelse(as.numeric(Open) == 1,0,Party)) 
  
  seat_proj <- seats %>%  # dem = if 2 candidates, lm; if GOP, auto lose; if dem, auto win
    mutate(dvp=Dem,
           dpres=Clinton,
           Dswing=swing,
           incumb = incumbent,
           freshman = Freshman) %>%
    select(Code,dvp,dpres,Dswing,incumb,freshman)
  
  seat_proj <- seat_proj %>% mutate(forecast_dem_pct=NA)
  seat_proj[seat_proj$incumb==0,]$forecast_dem_pct <- predict(fit.open, seat_proj[seat_proj$incumb==0,]) + swing
  seat_proj[seat_proj$incumb!=0,]$forecast_dem_pct <- predict(fit.incumb, seat_proj[seat_proj$incumb!=0,]) + swing
  
  
  seat_proj <- seat_proj %>% mutate(Dem_Win = forecast_dem_pct > 50)
  
  table(seat_proj$Dem_Win)
  seat_proj %>% filter(dvp>50,forecast_dem_pct<50) # seats dems lose
  seat_proj %>% filter(dvp<50,forecast_dem_pct>50) # seats reps lose
  # simulate--------------
  
  SIMULATE(seat_proj,SIM_NUMS,2.5)
  
  
  # print  info
  print(paste("dem share of polling is",round(natl_dem_pct,2), "| swing=",round(swing,2)),quote=FALSE)
  print(paste("baseline sum of Dem seats:",nrow(seat_proj%>%filter(Dem_Win))),quote=FALSE)
  
  # what is # of seats according to sim?
  seat_proj$forecast_dem_pct <- seat_probs$DemAvgShare # makes output easier
  seat_proj <- seat_proj %>% mutate(Dem_Win = forecast_dem_pct>50)
  
  print(paste("simulation sum  of Dem seats:",nrow(seat_proj%>%filter(Dem_Win))),quote=FALSE)
  
  # proabbilitites:
  print(paste("probability of Dem victory is",
              round(nrow(SIMULATIONS %>% filter(Dem_Seats>217))/nrow(SIMULATIONS)*100,2),"%"),quote=FALSE)
  print(paste("median number of simulated Dem seats:",median(SIMULATIONS$Dem_Seats)),quote=FALSE)
  print(paste("mode number of simulated Dem seats:",getmode(SIMULATIONS$Dem_Seats)),quote=FALSE)
  
  
  # write polls, probs, text etc. --------------
  
  ### write file 
  # Date  Dem % Poll    Dem 2p% Poll   Dem seats     Dem prob seats
  
  vital_stats <- data.frame("Projection" = natl_dem_pct,
                            "DemSeats" = nrow(seat_proj%>%filter(Dem_Win)),
                            "Med" = median(SIMULATIONS$Dem_Seats),
                            "DemProb" = nrow(SIMULATIONS %>% filter(Dem_Seats>217))/nrow(SIMULATIONS))
  
  #write.csv(vital_stats,"Tracker/Data/sims_for_curve.csv",row.names = FALSE)
  
  
  # add the average to a file of averages
  sims_over_time <- suppressMessages(read_csv("Tracker/Data/sims_for_curve.csv")) # read the existing time series data
  
  rownames(sims_over_time) <- NULL # remove row names
  
  if(natl_dem_pct %in% sims_over_time$Projection){
    sims_over_time[sims_over_time$Projection == natl_dem_pct,] <- vital_stats
  }else{
    sims_over_time <- bind_rows(sims_over_time,vital_stats)
  }
  
  write.csv(sims_over_time,"Tracker/Data/sims_for_curve.csv",row.names = FALSE) # write the file!

}


# visualize, analyze ----------
sims <- suppressMessages(read_csv("Tracker/Data/sims_for_curve.csv")) # read the existing time series data

#names(sims)<-c("Dem. Share of Vote (%)","Dem. Number of Seats","Med","Probability of Dem. Win" )
#sims <- sims %>%
#  select(-Med) %>%
#  mutate(`Probability of Dem. Win` = 100 *`Probability of Dem. Win`) #%>% gather(variable,value,2:3)

gg <- ggplot(sims,aes(x=Projection)) +
  geom_abline(intercept=0,slope = 4.3,col="gray") +
  annotate("text",x=40,y=180,label="Perfect Proportionality",angle=17.6,col="gray") +
  geom_line(aes(y=DemSeats,col="Modeled Dem. Seats")) +
  geom_line(aes(y=Med,col="Simulated Dem. Seats"),linetype=4) +
  geom_line(aes(y=(DemProb*(max(DemSeats)-min(DemSeats)))+min(DemSeats),
                col="Dem. Chance of Winning"),linetype=2)+
  scale_y_continuous(sec.axis=
                       sec_axis(~(.-min(sims$DemSeats))/(max(sims$DemSeats) - min(sims$DemSeats)), 
                                name = "Democratic Win Probability (%)",
                                labels = scales::percent_format())) +
  geom_vline(xintercept = 50,linetype=2,col="gray") +
  theme_elliott() +
  theme(legend.position = "top") +
  scale_color_manual("",values=c("Simulated Dem. Seats" = "purple",
                                 "Modeled Dem. Seats" = "blue",
                                 "Dem. Chance of Winning" = "red")) + 
  labs(labs(x="Democratic Share of the\nNational Vote (Two-Party %)",
            y="Democratic Seats",
            title="How Many Votes Do Democrats Need to\nTake Back the House?",
            subtitle="Forecasting hypothetical seat shares and win probabilities with\nvarying national popular votes (two-party %)"))

gg

png("Tracker/Graphics/simulate_curve.png",height=6,width=8,unit="in",res=350)
grid.arrange(gg,my_g, heights=c(9,0.5))
#dev.copy(png,"Tracker/Graphics/simulation_hist.png",height=6,width=8,unit="in",res=350)
dev.off()

