### run after run_model

SIM_NUMS <- 10000

for (dpctseq in seq(40,60,by = 0.5)){
  
  natl_dem_pct <- dpctseq #(0.58 + (0.62 *(dpctseq-50)) - (0.74 *1)) + 50 # coefs from 2014 paper, oct polling
  natl_rep_pct <- 100 - natl_dem_pct
  
  swing =  natl_dem_pct - 50.06 # the 2p-% dems earned in 2016
  swing = swing #- 0.49 # correction, in technical appendix of bafumi et al
  
  
  # predict based on incumbency advantage
  seats <- gseats %>% select(Code,"Open" = Open.2014., "Clinton" = Obama.2012, "Trump" = Romney.2012, "Dem" = X2012.Dem, "GOP" = X2012.GOP,"Freshman" = Freshman.2014) %>%
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
           Clinton=(Clinton/psum)*100) %>% 
    select(-c(psum,csum)) %>% 
    mutate(incumbent = ifelse(as.numeric(Open) == 1,0,Party)) 
  
  # regression coefs 
  
  source("Tracker/seatsLM.R")
  
  seat_proj <- seats %>%  # dem = if 2 candidates, lm; if GOP, auto lose; if dem, auto win
    mutate(dvp=Dem,
           dpres=Clinton,
           Dswing=swing,
           incumb = incumbent,
           freshman = Freshman) %>%
    select(Code,dvp,dpres,Dswing,incumb,freshman)
  
  seat_proj <- seat_proj %>%
    mutate(forecast_dem_pct = predict(fit,seat_proj),
           forecast_dem_pct = ifelse(dvp == 0,0,
                                    ifelse(dvp == 100,100,forecast_dem_pct)),
           Dem_Win = forecast_dem_pct > 50)
  
  table(seat_proj$Dem_Win)
  # simulate--------------
  
  SIMULATIONS <- data.frame("Dem_Seats" = SIMULATE(seat_proj,SIM_NUMS))
  
  print(paste("dem share of polling is",natl_dem_pct),quote=FALSE)
  print(paste("baseline number of Dem seats:",nrow(seat_proj%>%filter(Dem_Win))),quote=FALSE)
  print(paste("probability of Dem victory is",
              nrow(SIMULATIONS %>% filter(Dem_Seats>217))/nrow(SIMULATIONS)),quote=FALSE)
  print(paste("median number of simulated Dem seats:",median(SIMULATIONS$Dem_Seats)),quote=FALSE)
  
  
  # write polls, probs, text etc. --------------
  
  ### write file 
  # Date  Dem % Poll    Dem 2p% Poll   Dem seats     Dem prob seats
  
  vital_stats <- data.frame("Projection" = natl_dem_pct,
                            "DemSeats" = nrow(seat_proj%>%filter(Dem_Win)),
                            "DemProb" = nrow(SIMULATIONS %>% filter(Dem_Seats>217))/nrow(SIMULATIONS))
  
  #write.csv(vital_stats,"Tracker/Data/sims_for_curve14.csv",row.names = FALSE)
  
  
  # add the average to a file of averages
  sims_over_time <- suppressMessages(read_csv("Tracker/Data/sims_for_curve14.csv")) # read the existing time series data
  
  rownames(sims_over_time) <- NULL # remove row names
  
  if(natl_dem_pct %in% sims_over_time$Projection){
    sims_over_time[sims_over_time$Projection == natl_dem_pct,] <- vital_stats
  }else{
    sims_over_time <- bind_rows(sims_over_time,vital_stats)
  }
  
  write.csv(sims_over_time,"Tracker/Data/sims_for_curve14.csv",row.names = FALSE) # write the file!
  
}



sims <- suppressMessages(read_csv("Tracker/Data/sims_for_curve14.csv")) # read the existing time series data

gg <- ggplot(sims_over_time,aes(x=Projection)) +
  geom_line(aes(y=DemSeats,col="Democratic Seats")) +
  geom_line(aes(y=(DemProb*(max(DemSeats)-min(DemSeats)))+min(DemSeats),
                col="Democratic Win Probability"),linetype=2)+
  scale_y_continuous(sec.axis=
                       sec_axis(~(.-min(sims$DemSeats))/(max(sims$DemSeats) - min(sims$DemSeats)), 
                                name = "Democratic Win Probability (%)",
                                labels = scales::percent_format())) +
  geom_vline(xintercept = 50,linetype=2,col="gray") +
  theme_elliott() +
  theme(legend.position = "top") +
  scale_color_manual("",values=c("Democratic Seats" = "blue",
                                 "Democratic Win Probability" = "red")) + 
  labs(labs(x="Democratic Share of the\nNational Vote (Two-Party %)",
            y="Democratic Seats",
            title="How Many Votes Do Democrats Need to\nTake Back the House?",
            subtitle="Forecasting hypothetical seat shares and win probabilities with\nvarying national popular votes (two-party %)"))

png("Tracker/Graphics/simulate_curve14.png",height=6,width=8,unit="in",res=350)
grid.arrange(gg,my_g, heights=c(9,0.5))
#dev.copy(png,"Tracker/Graphics/simulation_hist.png",height=6,width=8,unit="in",res=350)
dev.off()
