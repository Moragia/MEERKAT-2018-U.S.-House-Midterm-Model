# this file runs our prediction model on the 2016, election 
library(tidyverse)
library(haven)
suppressMessages(suppressWarnings(library(stargazer)))

seats_lm.df <- gseats %>% 
  mutate("Obama.2012.Margin" = Obama.2012 - Romney.2012,
         "Clinton.Margin" = Clinton..2016 - Trump..2016, 
         "Dem.House.Margin.Prev.2" = X2012.Dem - X2012.GOP, 
         "Dem.House.Margin.Prev" = X2014.Dem - X2014.GOP, 
         "Dem.House.Margin" = X2016.Dem - X2016.GOP) %>%
  dplyr::select(Code,
                Obama.2012.Margin,
                Clinton.Margin,
                Dem.House.Margin.Prev.2,
                Dem.House.Margin.Prev,
                Dem.House.Margin,
                "PartisanLean" = PVI.2014.16,
                "Open" = Open.2016, 
                "Freshman" = Freshman.2016,
                Uncontested.Prev = Uncontested.2014,
                Uncontested = Uncontested.2016) %>% # load in data from Gsheet
  mutate(Party = ifelse(Dem.House.Margin.Prev>0,1,-1),
         Open = ifelse(is.na(as.character(Open)),0,1),
         Uncontested.Prev = ifelse(is.na(as.character(Uncontested.Prev)),0,Uncontested.Prev),
         Uncontested = ifelse(is.na(as.character(Uncontested)),0,Uncontested),
         Freshman = ifelse(is.na(as.character(Freshman)),0,Party),
         Incumbent = ifelse(Open == 1,0,Party),
         year=2016) %>% 
  mutate( # If prev race was uncontested, use margin lagged x2
    Dem.House.Margin.Prev = ifelse(Uncontested.Prev!=0,Dem.House.Margin.Prev.2,Dem.House.Margin.Prev) 
    )


seats_lm.df[is.na(seats_lm.df)] <- NA

# add swing
Swing.2016 <- 4.6 #shift in D margin from 2014 to 2016


# make the models ---------
fit <- lm(Dem.House.Margin ~ Dem.House.Margin.Prev + PartisanLean + Incumbent, data=seats_lm.df %>% filter(Uncontested==0))
summary(fit)

fit.open <- lm(Dem.House.Margin ~ PartisanLean, data=seats_lm.df %>% filter(Incumbent==0,Uncontested==0))
summary(fit.open)

fit.incumb <- lm(Dem.House.Margin ~  Dem.House.Margin.Prev + PartisanLean + Incumbent + Freshman, data=seats_lm.df %>% filter(Incumbent!=0,Uncontested==0))
summary(fit.incumb)


#stargazer(fit.open,fit.incumb,type="text")

# predict and eval ------------
seats_lm.df$Prediction <- NA

# open seats
#seats_lm.df[seats_lm.df$Incumbent==0,]$Prediction <- predict(fit.open, seats_lm.df[seats_lm.df$Incumbent==0,]) + Swing.2016

# incumbent seats
#seats_lm.df[seats_lm.df$Incumbent!=0,]$Prediction <- predict(fit.incumb,seats_lm.df[seats_lm.df$Incumbent!=0,]) + Swing.2016

seats_lm.df$Prediction <- predict(fit,seats_lm.df) + Swing.2016 # lets' just project using one linear model

# uncontested seats
seats_lm.df[seats_lm.df$Uncontested==1,]$Prediction <- 100
seats_lm.df[seats_lm.df$Uncontested==-1,]$Prediction <- -100


# error
seats_lm.df$error <- seats_lm.df$Dem.House.Margin - seats_lm.df$Prediction

ggplot(seats_lm.df,aes(x=Prediction,y=Dem.House.Margin)) + 
  geom_abline(intercept = 0,slope=1) +
  geom_point(aes(col=as.character(Uncontested))) +
  theme(legend.position  = "bottom")


# rmse for simulations
RMSEopen <- rmse(seats_lm.df[seats_lm.df$Incumbent==0,]$error);RMSEopen 

RMSEincumb <- rmse(seats_lm.df[seats_lm.df$Incumbent!=0,]$error);RMSEincumb

RMSEuncontested<- rmse(seats_lm.df[seats_lm.df$Uncontested!=0,]$error);RMSEuncontested


# wrong calls
nrow(seats_lm.df %>% filter((Prediction>0)))
nrow(seats_lm.df %>% filter((Dem.House.Margin>0)))
nrow(seats_lm.df %>% filter((Prediction>0 & Dem.House.Margin<0) | (Prediction<0 & Dem.House.Margin>0))) 

