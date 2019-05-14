
###############
### PURPOSE ###
###############
This code is part of the Mortgage PD model (bottom-up) 
add functions to smooth out coxph baseline function

########################
### raw data location###
########################


setwd("~Intermediate DT")
setwd("~Capital Plan 2019")

########################
# output data location #
########################

setwd("~Capital Plan 2019")
setwd("~Intermediate DT")
setwd("~Fit Model & Forecast")
###################
###output files###
##################

#D04_MTG_LGD_ALL.RData  
#Actual loss data

#D04_For_ModelFit_LGD.RData###
#Adjusted loss rate after extreme values were removed, only variables used in LGD model were kept 


########################
####### load data ######
########################

#data.directory
load('D04_For_ModelFit_PD.RData')  #for.pd.model.fit
load('D04_For_ModelFit_LGD.RData') #LGD.data

setwd("~Fit Model & Forecast")
load('D04_For_Idio_DT.RData')
load('D04_For_unfavorable_DT.RData' )
load('D04_For_Severe_DT.RData')

#####################################
####### SMOOTH Baseline Hazard ######
#####################################
######------Step 1----- Calculate Cumulative H0------#######
#centered=FALSE returns a non-parametric estimate of the cumulative baseline hazard
H0 <- basehaz(cox.fit, centered=FALSE) 

hhat_d<-H0 %>%
  mutate(hhat_d=hazard-lag(hazard))

hhat_d$hhat_d[which(is.na(hhat_d$hhat_d) )]<- 0 #obs 1 is NA, replaced with 0

###########################################################################################
######     smooth MOB from 0-121, 121-267 exponential decline, >267 SET TO BE 0      ######
###########################################################################################
### when MOB equals to 0, hazard rate is always 0
s.hazard = c(0,smooth.spline(hhat_d$hhat_d[2:121],cv=TRUE)$y)
s.hazard1 = exp(seq(log(s.hazard[length(s.hazard)]+1),log(1),length.out=dim(hhat_d)[1]-length(s.hazard)))-1

smoothedH0 = cbind(MOB=seq(0,372,length.out=373),s.hazard121 =c(s.hazard,s.hazard1))
smoothedH0<-data.frame(smoothedH0)

smoothedH0$s.hazard121[which(smoothedH0$MOB>=121&smoothedH0$MOB<=267)] = 
  10^(seq(log(smoothedH0$s.hazard121[which(smoothedH0$MOB==121)]+1,base=10),0,length.out=147))-1
smoothedH0$s.hazard121[which(smoothedH0$MOB>267)] = 0

write.csv(smoothedH0,"smoothH0.csv")
plot(smoothedH0$s.hazard121)


#cumHR
smoothedH0<-smoothedH0 %>%
  mutate(cumHR = cumsum(s.hazard121))

##########################
######  smooth end  ######
##########################

################################################
####### Function predict.PD.s  predict.pd#######
################################################
predict.PD.s <- function(newdata,date.seq, input.list.month.pd = NULL){
  PD.actual <- c()
  PD.pred <- c()
  list.month.pd <- list()
  for(month in date.seq){                                                  ### for each month, extract all the loan data that have records in that month
    print(month)
    subset.all.data <- subset(newdata, Tape.Cut.off.date == month)                      ### given one month, subset the newdata
    subset.all.data <- data.table(subset.all.data)
    if(length(input.list.month.pd) == 0){ 
      pred.pd <- predict.pd(subset.all.data)                      ### if not given PD, then use PD model to obtain the predicted PD probability                        
      list.month.pd[[month]] <- pred.pd
    }else{
      pred.pd <- input.list.month.pd[[month]]                              ### if given PD, then use that directly       
    }                                                                      ### calculate the default probability for each loan
    total.balance <- sum(subset.all.data$Current.Principal.Balance)                 ### sum the current principal balance of all loans to obtain the total balance
    default.balance <- sum(subset.all.data$Current.Principal.Balance[which(subset.all.data$If_Default==1)])      ### sum the current principal balance of the defaulted loans
    PD.actual <- c(PD.actual, default.balance/total.balance)                                      
    PD.pred <- c(PD.pred, weighted.mean(pred.pd, w = subset.all.data$Current.Principal.Balance, na.rm = T)) ### obtain a weighted prediction default probability for that month based on the default probability of individual loan    
  }  
  PD.ts <- data.frame(date = as.character(date.seq), PD.actual = PD.actual, PD.pred = PD.pred)  ### output the date, actual PD and predictive PD
  return(list(PD.ts, cache = list.month.pd))
}


predict.pd <- function(newdata){
  newdata <- data.table(newdata)
  if(dim(newdata)[1]>0){
    
    newdata<-left_join(newdata, smoothedH0, by = c("start" = "MOB"))
    newdata<-left_join(newdata, smoothedH0, by = c("MOB" = "MOB"))
    k <- length(cox.fit$coefficients)
    cov_coef_mat <- vector()
    newdata <- as.data.frame(newdata)
    for (i in 1:k){
      index <- i+3
      a <- newdata[,index]
      b <- unname(cox.fit$coefficients[i])
      cov_coef_mat <- cbind(cov_coef_mat,a*b)
    } 
    
    cov_coef_df <- as.data.frame(cov_coef_mat)
    
    colnames(cov_coef_df) <- c("Unemployment_rate","Real_GDP_growth.1qtr.lag","FICO.Score..Current.", "Current.Principal.Balance","refresh.ltv","Single.Family","Purpose.Refi")
    
    df<-cbind(newdata[,1:3],cov_coef_df,newdata[,11:16])
    
    newdata<-df %>% 
      mutate(Ht_start =exp(rowSums(.[4:10]))*cumHR.x,
             St_start=exp(-(Ht_start)),
             Ht_MOB =exp(rowSums(.[4:10]))*cumHR.y,
             St_MOB=exp(-(Ht_MOB)), 
             pred.pd=1-St_MOB/St_start)
    
    return(newdata$pred.pd)                                                     ### output the probability of default for each loan 
  }else{
    return(rep(NA, dim(newdata)[1]))                                              ### if the input data has no loan, then output missing data
  }
}

#############################
####### Function end ########
#############################
date.seq <- as.Date(as.yearmon(seq(as.Date("2006/1/1"), as.Date("2017/9/1"), by = "month")),frac=1)        ### the sequence of historical date 

last.month <- date.seq[length(date.seq)]                                                                 ### last date of the historical date series

# date.seq <- as.character(as.yearmon(seq(as.Date("2006/1/1"), as.Date("2017/6/1"), by = "month"))) 
PD.ts.hist <- predict.PD.s(for.pd.model.fit, date.seq)


write.csv(PD.ts.hist[[1]], "MTG-PD_history 2019.csv", quote = F, row.names = F)

# forecast Idio,sev and adv
date.seq.for <- as.Date(as.yearmon(seq(as.Date("2017/8/1"), as.Date("2020/12/1"), by = "month")),frac=1) 

PD.ts.hist <- predict.PD.s(all.data, date.seq)


PD.hat.idio<- predict.PD.s(forecast.base, date.seq.for)
#base.month.map.all <- PD.hat.idio$cache
PD.hat.idio <- PD.hat.idio[[1]]
write.csv(PD.hat.idio, "MTG-PD_idio 2019.csv", quote = F, row.names = F)

PD.hat.adv <- predict.PD.s(forecast.ad,date.seq.for)
#ad.month.map.all <- PD.hat.adv$cache
PD.hat.adv <- PD.hat.adv[[1]]

write.csv(PD.hat.adv, "MTG-PD_adv 2019.csv", quote = F, row.names = F)

### output the actual PD probability and prediction PD probability for future month under severely adverse condition 
PD.hat.sev <- predict.PD.s(forecast.sead, date.seq.for)
#sead.month.map.all <- PD.hat.sev$cache
PD.hat.sev <- PD.hat.sev[[1]]

write.csv(PD.hat.sev, "MTG-PD_sev 2019.csv", quote = F, row.names = F)



######################################################
######## lossrate forecast ###########################
######################################################

#########################
####### Function ########
#########################
### output the predicted lossrate for given month
predict.lossrate <- function(newdata, lossrate.logit.linear, date.seq.for, mixture = F){
  Lossrate.pred <- c()
  for(month in date.seq.for){
    print(month)
    subset.all.data <- subset(newdata, DATE == month)          ### for each month in the dataset, first subset all the data from that month
    subset.all.data <- data.table(subset.all.data)
    total.balance <- sum(subset.all.data$Current.Principal.Balance)      ### calculate the current balance total and current default balance total
    default.balance <- sum(subset.all.data$Current.Principal.Balance[which(subset.all.data$If_Default==1)])
    pred.lossrate.logit <- predict(lossrate.logit.linear, newdata = subset.all.data)  ### obtained the predicted loss rate for the subset loan from the estimated LGD model.
    if(mixture){
      pred.lossrate <- one.probability + (1 - one.probability)*inv.logit(pred.lossrate.logit)
    }else{
      pred.lossrate <- inv.logit(pred.lossrate.logit)
    }
    Lossrate.pred<-c(Lossrate.pred,weighted.mean(pred.lossrate, w = subset.all.data$Current.Principal.Balance, na.rm = T))            ### output the weighted average of the loserate
  }  
  return(Lossrate.pred)
}

#############################
####### Function end ########
#############################

lossrate.modelfit<- predict.lossrate(all.data.snapshot, lgd.fit, date.seq)

lossrate.hist <- data.frame(date = date.seq, 
                            LossRate.modelfit= lossrate.modelfit)
write.csv(lossrate.hist, "pred_history_lgd.csv", quote = F, row.names = F)


### output the prediction loss for future month under base condition 

lossrate.base <- predict.lossrate(forecast.base, lgd.fit, date.seq.for)

### output the prediction loss for future month under adverse condition

lossrate.ad <- predict.lossrate(forecast.ad, lgd.fit, date.seq.for)

### output the prediction loss for future month under severely adverse condition 

lossrate.sead <- predict.lossrate(forecast.sead, lgd.fit, date.seq.for)

### output the aggregated result, including the date, predicted loss under 3 scenarios

lossrate.output.base <- data.frame(date = date.seq.for, Idiosyncratic = lossrate.base)
lossrate.output.ad<- data.frame(date = date.seq.for, Unfavorable = lossrate.ad)
lossrate.output.sead <- data.frame(date = date.seq.for, Severe = lossrate.sead)

write.csv(lossrate.output.base)
write.csv(lossrate.output.ad...)
write.csv(lossrate.output.sead...)




