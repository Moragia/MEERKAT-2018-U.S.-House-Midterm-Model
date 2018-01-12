# Many thanks to the folks at Huffpost Pollster for this data

if(FALSE){
  library(googlesheets)
  library(dplyr)
  my_sheets <- gs_ls()
  suppressMessages(gsheet <- gs_title("2018 Midterm Benchmarks"))
  suppressMessages(gseats <- data.frame(gs_read(gsheet,ws = "Main Data",range="A3:Y439")))
  gseats[is.na(gseats)] <- NA
  demog <- data.frame(gs_read(gsheet,ws = "Demographics"))
  
  # wrangle
  merged <- gseats %>% 
    select(Code,Obama.2008,Obama.2012,Clinton..2016,X2012.Dem,X2016.Dem) %>%
    left_join(demog %>% 
                select("Code" = District,White.,Black.,Latino.,BA.,Income),
              by="Code") %>%
    select(-Code) 
  
  merged$Income <- gsub("[$]","",merged$Income)
  merged$Income <- gsub("[,]","",merged$Income)
  
  merged<- lapply(FUN=as.numeric,merged) %>% 
    as.data.frame() %>%
    mutate(Income = Income/1000)
  
  # correlations for simulation
  seat_cor <- as.data.frame(t(merged))
  
  names(seat_cor) <- gsub("-","",gseats$Code)
  corr <- cor(seat_cor, use="complete.obs")
  row.names(corr) <-  gsub("-","",gseats$Code)
  
  head(corr)
  
  write.csv(corr,"Tracker/Data/corr_matrix.csv",row.names=FALSE)
  
}
library('Matrix')

buildCorrelationConstant <- function() {
  # Read correlations from file
  frame <- read.csv("Tracker/Data/corr_matrix.csv")
  rawMatrix <- as.matrix(frame)
  
  # Make the matrix the nearest positive definite, but still correlated,
  # because CorrelatedRnorm() won't work with the negative eigenvalues the
  # original generates.
  pdMatrix <- as.matrix(nearPD(rawMatrix, corr=TRUE)$mat)
  
  ev <- eigen(pdMatrix, symmetric=TRUE)
  
  return(ev$vectors %*% diag(sqrt(ev$values)) %*% t(ev$vectors))
}

kCorrelationConstant <- buildCorrelationConstant()

# Returns an array of normal-distribution doubles which are correlated.
CorrelatedRnorm <- function() {
  n <- nrow(kCorrelationConstant)
  random <- rnorm(n)
  ret <- kCorrelationConstant %*% random
  return(drop(ret))
}



