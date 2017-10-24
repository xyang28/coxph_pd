###############################################
###   Self_defined model ######################
###############################################

### load the required libraries
require(data.table)
require(survival)
require(zoo)
require(dplyr)
require(sas7bdat)
require(rCharts)
require(glmnet)
require(ROCR)
require(fitdistrplus)
require(car)
require(betareg)
require(LiblineaR)
require(pec)
require(boot)

### data cleaning 
### new feature :             Loan.Type.Or.Product.Type.Has.Arm, Jumbo.Indicator, Original.Interest.Only.Flag, Documentation.Type.Full
###                           Owner.Occupied, Purpose.Refi, Single.Family, asset.class, median.current.FICO, median.original.DTI, MOB
### feature transformation:   FICO.Score..Current., Front.End.DTI..Original., Original.Appraisal.Amount, Date
read.data <- function(data.directory, sample = T, key = "Loan.Number", sample.size = 5000){
  original.data <- fread(data.directory, header = T)                                                  ### read the local file into original.data dataset
  setnames(original.data, old = names(original.data), new = make.names(colnames(original.data)))      ### make syntactically valid names for original.data
  if(sample){
    keep.keys <- sample(unique(original.data[[key]]), size = sample.size)                             ### if the sample option is TRUE, sample certain number of key variables from the original.data without replacement
    keep.index <- which(original.data[[key]] %in% keep.keys)                                          ### the default of the key variable is 'Loan.Number'
    original.data <- original.data[keep.index, ]                                                      
  }
  original.data$MOB <-  mondf(original.data$Origination.Date, original.data$Tape.Cut.off.date)       # add new variable MOB: MOB= Tape.Cut.Off.date - Origination.Date
  original.data <- original.data[ which(original.data$MOB >= 0), ]                                   # remove the abnormal rows with MOB smaller than 0
  original.data$FICO.Score..Current. <- as.numeric(original.data$FICO.Score..Current.)               # transform the FICO.Score..Current. into numeric variable 
  median.table <- original.data[, list(median.current.FICO = median(FICO.Score..Current.[which(FICO.Score..Current. >= 300)], na.rm = T)), 
                                by = c("Loan.Type")]                                                 # For each loan.type, find the median of the FICO score after removing the FICO score lower than 300 and the missing values 
  original.data <- left_join(original.data, median.table, by = "Loan.Type")                          # join the median FICO score table with the original.data  
  original.data <- data.table(original.data)                                                         # transform the original.data into data.table so that every loan has its corresponding median FICO
  original.data$Front.End.DTI..Original. <- as.numeric(original.data$Front.End.DTI..Original.)       # transform the Front.End.DTI..Original.  into numeric variable 
  median.table <- original.data[, list(median.original.DTI = median(Front.End.DTI..Original.[which(Front.End.DTI..Original. > 0)], na.rm = T)), 
                                by = c("Interest.Rate.Type")]                                        # For each interest rate type, find the median of the original DTI after removing the DTI lower than 0 and the missing values
  original.data <- left_join(original.data, median.table, by = "Interest.Rate.Type")                 # join the median DTI table with the original.data so that every loan has its corresponding median DTI       
  original.data <- data.table(original.data)                                                        
  original.data$FICO.Score..Current.[which(original.data$FICO.Score..Current.<300)] <- original.data$median.current.FICO[which(original.data$FICO.Score..Current.<300)]          # Dealing with abnormal FICO values: replace the FICO score smaller than 300 with the median FICO 
  original.data$Front.End.DTI..Original.[which(original.data$Front.End.DTI..Original.<= 0 | is.na(original.data$Front.End.DTI..Original.))] <- original.data$median.original.DTI[which(original.data$Front.End.DTI..Original.<= 0 | is.na(original.data$Front.End.DTI..Original.))]
  arm.index <- which(grepl(pattern = "ARM", x = original.data$Loan.Type, ignore.case = T)|grepl(pattern = "ARM", x = original.data$Product.Type, ignore.case = T))              # Dealing with abnormal DTI values: replace the DTI values smaller than 0 with the median DTI
  arm.na.index <- which((original.data$Loan.Type) == "" | original.data$Product.Type =="")   ### ARM index: record the indices of the loans with loan.type or product type is 'ARM'
  if(length(arm.na.index) > 0){
    original.data <- original.data[-arm.na.index, ]                     ### remove the data with missing Loan.type or Product.Type
  }
  original.data$Loan.Type.Or.Product.Type.Has.Arm <- 0                     # create new variable called Loan.Type.Or.Product.Type.Has.Arm with initial value 0
  original.data$Loan.Type.Or.Product.Type.Has.Arm[arm.index] <- 1          #  Loan.Type.Or.Product.Type.Has.Arm flag equals to 1 if the loan is in the ARM index
  original.data$Jumbo.Indicator <- 0                                       # create new variable called Jumbo.Indicator with initial value 0
  jumbo.index <- which(grepl(pattern = "jumbo", x = original.data$Loan.Type, ignore.case = T)|grepl(pattern = "jumbo", x = original.data$Product.Type, ignore.case = T))  ### Jumbo index: record the indices of the loans with loan.type or product type is 'jumbo'
  original.data$Jumbo.Indicator[jumbo.index] <- 1                         # Jumbo.Indicator flag equals to 1 if the loan is in the Jumbo index
  original.data$Original.Interest.Only.Flag <- 0                          # create new variable called Original.Interest.Only.Flag with initial value 0
  original.data$Original.Interest.Only.Flag[which(original.data$Interest.Only.Flag %in% c("FX02","FX03", "FX10", "NNZI", "NNUI", "ZIII"))] <- 1   # Original.Interest.Only.Flag equals to 1 if the loan's Interest is in the group ("FX02","FX03", "FX10", "NNZI", "NNUI", "ZIII")
  doc.full.index <- which(grepl(pattern = "full", x = original.data$Documentation.Type, ignore.case = T))      # doc.full.index: record the indices of the loans if the Documentation type is "full"
  original.data$Documentation.Type.Full <- 0                                          # create new variable called Documentation.Type.Full with initial value 0
  original.data$Documentation.Type.Full[doc.full.index] <- 1                          #  Documentation.Type.Full flag equals to 1 if the loan is in the doc.full.index
  original.data <- original.data[which(original.data$Occupancy.Type != ""), ]        # remove the data with Occupancy.Type missing
  original.data$Owner.Occupied <- 0                                                  # create new variable called Owner.Occupied with initial value 0
  original.data$Owner.Occupied[which(original.data$Occupancy.Type == "OO")] <- 1     # Owner.Occupied flag equals to 1 if the loans' occupancy type is "OO"
  original.data$Purpose.Refi <- 0                                                   # create new variable called Purpose.Refi with initial value 0
  original.data$Purpose.Refi[which(original.data$Purpose == "Refinance")] <- 1       # Purpose.Refi flag equals to 1 if the purpose of the loan is Refinance
  original.data$Single.Family <- 0                                                    # create new variable called Single.Family with inital value 0
  original.data$Single.Family[which(original.data$Property.Type == "1-Fam")] <- 1     # Single.Family flag equals to 1 if the Property type of the loan is "1-Fam"
  original.data$Original.Appraisal.Amount[which(is.na(original.data$Original.Appraisal.Amount))] <- original.data$Sales.Purchase.Price[which(is.na(original.data$Original.Appraisal.Amount))] # for each loan, if the original appraisal amount is missing, use its sales purchase price to replace that
  original.data$DATE <- as.yearmon(as.Date(original.data$Tape.Cut.off.date))          # transform the DATE variable into monthly data format
  asset.classes <- read.csv(file.path("J:/Finance/Treasury/Statistical Modeling/Credit Loss Models/First Mortgage/SFFCU_First Mortgage_Final_Code_SFFCU/SFFCU First Mortage/01. Data Cleaning/01. Raw_data/01. Loan Data", "asset_class.csv"))                # read the file containing asset class
  Fixed.25.30.40.Year.index <- which(original.data$Interest.Rate.Type == "Fixed" & original.data$Product.Type %in% original.data$Product.Type[which(asset.classes$Category == "25/30/40")])  # Fixed.25.30.40.Year.index: record the indices of loans with both interest rate type equals to 'Fixed' and asset class is "25/30/40" 
  Fixed.20.index <-  which(original.data$Interest.Rate.Type == "Fixed" & original.data$Product.Type %in% original.data$Product.Type[which(asset.classes$Category == "20 and Below")])  # Fixed.20.index : record the indices of the loans with both interest rate type equals to 'Fixed' and asset class is "20 and below" 
  Adjustable.index <- which(original.data$Interest.Rate.Type == "Adjustable")        # Adjustable.index: record the indices of the loans with interest rate type equals to "Adjustable"
  original.data$asset.class <- NA
  original.data$asset.class[Fixed.25.30.40.Year.index] <- "Fixed.25.30.40.Year"    # create a new variable called asset.class and assign asset class levels to this variable based on Fixed.25.30.40.Year.index,  Fixed.20.index and Adjustable.index
  original.data$asset.class[Fixed.20.index] <- "Fixed.20.Below"
  original.data$asset.class[Adjustable.index] <- "Adjustable"
  unique(original.data, by=NULL)                                          # remove the duplicates
}

### merge the original.data with the base macro dataset by Date, and then remove the duplicates.
transform.data <- function(original.data, predictor.names, admin.names, base){
  setnames(x = original.data, old = names(original.data), new = gsub(" ", "_", names(original.data), fixed =T))           # reconstruct the column name: replace the " " in the column with "_" in the column name
  setnames(x = original.data, old = names(original.data), new = gsub("-", "_", names(original.data), fixed =T))           # reconstruct the column name: replace the "-" in the column with "_" in the column name
  transformed.data <- original.data[, which(names(original.data) %in% c(predictor.names, admin.names)), with = FALSE]    ## subset the original dataset
  transformed.data <- data.table(transformed.data)                                       # transform the transformed.data into data table
  transformed.data$DATE <- as.character(transformed.data$DATE)                           # transform the DATE variable into character format
  base$DATE <- as.character(base$DATE)                                                   # transform the DATE variable in the base macro dataset into character format
  base <- data.table(base)                                                               # transform the base macro dataset into data table
  transformed.data <- left_join(transformed.data, base, by = "DATE")           # merge the original.data with the base macro dataset by Date
  unique(transformed.data, by=NULL)                                            # remove the duplicates.
}


### clean.data function's sub functions: get.start, get.start.2, get.refresh.ltv, prepare.ltv 

### based on MOB, generate a series of start, default loans are not new loans  
### for each new loan, START variable starts from 0
### for each old loan, START variable starts from (first element of MOB -1)
### for each loan, calculate the refresh ltv value
clean.data <- function(transformed.data, newloans = F){
  transformed.data$DATE <- as.yearmon(as.Date(transformed.data$Tape.Cut.off.date))                # tranform the Date variable in transformed.data into monthly format
  all.data <- data.table(transformed.data)                                                        # transform the transformed.data into data table
  toEvaluateString <- "all.data <- all.data[, list("                                              # The below will construct a command, and this line starts the command 
  for(name in setdiff(names(all.data), c("Loan.Number"))) 
    toEvaluateString <- paste0(toEvaluateString,paste0(name , " = ", name, " ,\n"))                    # keep all the variables in all.data except Loan Number
  if(newloans){
    toEvaluateString <- paste0(toEvaluateString, "start = get.start.2(MOB, If_Default) ,\n")             # create start varible if it is a new loan
    
  }else{
    toEvaluateString <- paste0(toEvaluateString, "start = get.start(MOB, If_Default) ,\n")               # create start variable if it is a historic loan
  }
  toEvaluateString <- paste0(toEvaluateString, "refresh.ltv = get.refresh.ltv(Current.Principal.Balance, Current.AVM.Value, AVM.Date, Original.Appraisal.Amount, Original.Appraisal.Date, Origination.Date,  House_Price_Index, OC_HPI, LA_HPI, Property.County)")
  toEvaluateString <- paste0(toEvaluateString, "), by = c('Loan.Number')]")                              #  for each loan, calculate the refresh ltv value
  eval(parse(text=toEvaluateString))                                                               #  combine the pieces of the command together and run it to obtain the new all.data
  all.data <- all.data[which(!(all.data$MOB==0 & all.data$start ==0)), ]    ### remove the rows with both MOB and start equal to 0  
  all.data
}

### calculate the start variable for historic loan
get.start <- function(endtime, status){
  tmp <- endtime[-length(endtime)]       # remove last element of MOB vector
  tmp <- c(0, tmp)                       # append 0 into the front of MOB vector
  tmp
}

### calculate the start variable for new loan
get.start.2 <- function(endtime, status){
  tmp <- endtime[-length(endtime)]      # remove last element of MOB vector
  tmp <- c(endtime[1] - 1, tmp)         # append endtime[1] - 1 to the front (first element decrease 1)
  tmp
}

### calculate the refresh ltv 
get.refresh.ltv <- function(Current.Principal.Balance, Current.AVM.Value, AVM.Date, Original.Appraisal.Amount, Original.Appraisal.Date, Origination.Date, House_Price_Index, OC_HPI, LA_HPI, Property.County){
  AVM.Date[which(AVM.Date == "")] <- NA                                                   ### mark the AVM.Date as missing if it equals to ""
  Original.Appraisal.Date[which(Original.Appraisal.Date == "")] <- NA                     ### mark the original appraisal date as missing if it equals to ""
  index.avm <- which(Current.AVM.Value > 0 & !is.na(AVM.Date) & max(as.Date(AVM.Date), na.rm = T) > min(as.Date(base$DATE)))  ### index.avm: record the indices of loans with current AVM value >0 and maximum AVM date > minimum appraisal date
  index.app <- which(Original.Appraisal.Amount > 0 & !is.na(Original.Appraisal.Date) & max(as.Date(Original.Appraisal.Date), na.rm = T) > min(as.Date(base$DATE))) # index.app: record the indices of loans with original appraisal amount >0 and maximum origination date > minimum of date under base scenario
  index.avm.ori <- which(Current.AVM.Value > 0 & Origination.Date!= "" & max(as.Date(Origination.Date), na.rm = T) > min(as.Date(base$DATE)))  # index.avm.ori: record the indices of loans with current AVM value >0 and maximum origination date > minimum of date under base scenario
  index.app.ori <- which(Original.Appraisal.Amount > 0 & Origination.Date!= "" & max(as.Date(Origination.Date), na.rm = T) > min(as.Date(base$DATE)))  # index.app.ori: record the indices of loans with original appraisal amount >0 and maximum of origination date > minium of date under base scenario
  if(length(index.avm) > 0){                                                              
    result <- prepare.ltv(Current.Principal.Balance[index.avm], Current.AVM.Value[index.avm], AVM.Date[index.avm], Property.County[index.avm])               ### this series of ifelse calculates the ltv before adjustment, 
  }else if(length(index.app) > 0){                                                                                                                           ### if index.avm is not empty, then use the indices in the index.avm to calculate
    result <- prepare.ltv(Current.Principal.Balance[index.app], Original.Appraisal.Amount[index.app], Original.Appraisal.Date[index.app], Property.County[index.app])      # if the above is not satisfied and index.app is not empty, then use the indices in the index.app to calculate 
  }else if(length(index.avm.ori)>0){                       
    result <- prepare.ltv(Current.Principal.Balance[index.avm.ori], Current.AVM.Value[index.avm.ori], Origination.Date[index.avm.ori], Property.County[index.avm.ori])      # if the above is not satisfied and index.avm.ori is not empty, then use the indices in the index.avm.ori to calculate    
  }else if(length(index.app.ori)>0){
    result <- prepare.ltv(Current.Principal.Balance[index.app.ori], Original.Appraisal.Amount[index.app.ori], Origination.Date[index.app.ori], Property.County[index.app.ori])  # if the above is not satisfied and index.app.ori is not empty, then use the indices in the index.app.ori to calculate
  }else{
    result <- NA
  }                               ### based on different mortgage information, obtain different balance*hpi/value
  if(length(na.omit(Property.County)) == 0){
    hpi <- House_Price_Index                                      ### if the county information is missing, then use the normal HPI    
  }else if(na.omit(Property.County)[1] == 59){
    hpi <- OC_HPI                                                 ### if the county is orange county, use orange county's HPI
  }else if(na.omit(Property.County)[1] == 37){
    hpi <- LA_HPI                                                 ### if the county is los angeles, use los angeles's HPI
  }else{
    hpi <- House_Price_Index                                      ### else use the normal HPI
  }                              
  result / hpi                   ### ltv= balance/value
}

### based on the recent value of the house (adjusted for hpi) and most recent balance, calculate the balance*hpi/value
### as the name suggests, preparation for the ltv calculation
prepare.ltv <- function(balance, value, date, Property.County){
  prepare <- data.frame(balance = balance, value = value, date = date)               ### construct a data frame with three columns: balance, value and date
  prepare <- na.omit(prepare)                                                        ### delete missing values
  most.recent.date <- max(as.Date(prepare$date))                                     ### find the most recent date
  balance.most.recent <- balance[which(as.Date(prepare$date) == most.recent.date)][1]   ### find the most recent balance and most recent value
  value.most.recent <- value[which(as.Date(prepare$date) == most.recent.date)][1]       ### find the value of the mortgage at the most recent date
  if(length(na.omit(Property.County)) == 0){
    hpi <- base$House_Price_Index[which(base$DATE == as.yearmon(most.recent.date))]        # if the county information is missing, use the regular HPI at the most recent date          
  }else if(na.omit(Property.County)[1] == 59){
    hpi <- base$OC_HPI[which(base$DATE == as.yearmon(most.recent.date))]                   # if it is in orange county, use orange county's HPI at the most recent date
  }else if(na.omit(Property.County)[1] == 37){
    hpi <- base$LA_HPI[which(base$DATE == as.yearmon(most.recent.date))]                   # if it is in Los Angeles, use Los Angeles's HPI at the most recent date
  }else{
    hpi <- base$House_Price_Index[which(base$DATE == as.yearmon(most.recent.date))]       # else use the regular HPI at the most recent date  
  }                                                                                    ### considering the geo information, there are 4 different hpi overall
  value <- value.most.recent / hpi                                                     ### recent value of the mortgage adjusted for corresponding hpi
  balance.most.recent / value                                                          ### calculate the ltv value
}

### New loan data has only one month record, this function serves to extend the new loan data to future dates and make preparation for forecast
### The output will be a list of datasets under 3 econ scenarios 
### 1. fill in fixed features for future data (features in fixed.dt.last vector, for example: Original.Interest.Only.Flag, Owner.Occupied etc)
### 2. merge the new loan data with the macro datasets under 3 scenarios
### 3. Combine the time-changing features (features in other.col: start, MOB) 
### 4. update the refresh.ltv based on geo information
prepare.forecast.PD <- function(all.data, date.seq.for, last.month){
  base$DATE <- as.character(as.yearmon(as.Date(base_monthly$DATE)))                     ### transform the DATE in macro datasets to month format
  ad$DATE <- as.character(as.yearmon(as.Date(ad_monthly$DATE)))
  sead$DATE <- as.character(as.yearmon(as.Date(sead_monthly$DATE)))  
  subset.all.data <- as.data.frame(subset(all.data, DATE == last.month))                ### subset new loan data (only new loan data has record in 2014 Sep)
  forecast.base <- data.frame()                                                         ### create empty forecast dataset for 3 scenarios
  forecast.ad <- data.frame()
  forecast.sead <- data.frame()
  for(loan in subset.all.data$Loan.Number){                                             ### for each of the loan in the subset dataset
    loan.data <- subset(all.data, Loan.Number == loan)                                  ### 1. fill in fixed features in fixed.dt.last vector (unchanged with time), then we obtain extend.dt.frame 
    fixed.cols <- setdiff(names(subset.all.data), c("DATE", "MOB", "Tape.Cut.off.date", "start", names(base)))   ### columns except DATE MOB Tape.Cut.off.date and start are all regarded as fixed columns
    fixed.dt <- loan.data[, names(loan.data)[which(names(loan.data) %in% fixed.cols)], with = F]                 ### extract loan data from fixed.columns
    fixed.dt.last <- as.vector(fixed.dt[dim(fixed.dt)[1],])
    extend.dt.frame <- as.data.frame(matrix(NA, nrow = 0, ncol = length(fixed.cols)))                            ### since all the data in fixed.columns are not time variant, then replicate it for each time period     
    names(extend.dt.frame) <- names(fixed.dt.last)
    for(i in 1:(length(date.seq.for) - 1)){
      extend.dt.frame <- rbind(extend.dt.frame, fixed.dt.last)
    }
    tmp.frame <- data.frame("DATE" = as.character(date.seq.for[-1]))                    ### 2. merge it with the macro variable datasets under 3 scenarios with the extend.dt.frame by DATE 
    macro.base <- merge(tmp.frame, base, by = "DATE", all.x = T)                            ### merge it with base macro dataset by date
    macro.base <- macro.base[order(as.yearmon(macro.base$DATE)), ]
    macro.ad <- merge(tmp.frame, ad, by = "DATE", all.x = T)                                ### merge it with adverse macro dataset by date
    macro.ad <- macro.ad[order(as.yearmon(macro.ad$DATE)), ]
    macro.sead <- merge(tmp.frame, sead, by = "DATE", all.x = T)                            ### merge it with severely macro dataset by date 
    macro.sead <- macro.sead[order(as.yearmon(macro.sead$DATE)), ]                      ### 3. Combine the time-changing features (features in other.col: start, MOB) 
    other.col <- data.frame(Tape.Cut.off.date = as.character(as.Date(as.yearmon(macro.base$DATE))), MOB = mondf(fixed.dt.last$Origination.Date , as.character(as.Date(as.yearmon(macro.base$DATE)))))  ### construct a dataframe called other.col containing Tape.Cut.off.date and MOB
    other.col$start <- get.start.2(other.col$MOB)                                            ###   add start variable to data.frame other.col                            
    
    result.base <- cbind(extend.dt.frame, macro.base, other.col)                             ### combine the datasets from above three steps together                  
    result.ad <- cbind(extend.dt.frame, macro.ad, other.col)
    result.sead <- cbind(extend.dt.frame, macro.sead, other.col)
    
    if(length(na.omit(result.base$Property.County)) == 0){                              ### 4. update the refresh.ltv based on geo information
      result.base$refresh.ltv <- result.base$refresh.ltv[1]*result.base$House_Price_Index[1]/result.base$House_Price_Index
      result.ad$refresh.ltv <- result.ad$refresh.ltv[1]*result.ad$House_Price_Index[1]/result.ad$House_Price_Index          ### if the property county information is missing then use the regular HPI value to adjust refresh ltv
      result.sead$refresh.ltv <- result.sead$refresh.ltv[1]*result.sead$House_Price_Index[1]/result.sead$House_Price_Index
    }else if(result.base$Property.County %in% c("59", 59)){
      result.base$refresh.ltv <- result.base$refresh.ltv[1]*result.base$OC_HPI[1]/result.base$OC_HPI
      result.ad$refresh.ltv <- result.ad$refresh.ltv[1]*result.ad$OC_HPI[1]/result.ad$OC_HPI                               ### if the county is orange county, then use orange county's HPI value to adjust refresh ltv
      result.sead$refresh.ltv <- result.sead$refresh.ltv[1]*result.sead$OC_HPI[1]/result.sead$OC_HPI      
    }else if(result.base$Property.County %in% c("37", 37)){
      result.base$refresh.ltv <- result.base$refresh.ltv[1]*result.base$LA_HPI[1]/result.base$LA_HPI 
      result.ad$refresh.ltv <- result.ad$refresh.ltv[1]*result.ad$LA_HPI[1]/result.ad$LA_HPI                              ### if the county is Los Angeles, then use Los Angeles's HPI value to adjust refresh ltv
      result.sead$refresh.ltv <- result.sead$refresh.ltv[1]*result.sead$LA_HPI[1]/result.sead$LA_HPI
    }else{
      result.base$refresh.ltv <- result.base$refresh.ltv[1]*result.base$House_Price_Index[1]/result.base$House_Price_Index
      result.ad$refresh.ltv <- result.ad$refresh.ltv[1]*result.ad$House_Price_Index[1]/result.ad$House_Price_Index            ### else, use the regular HPI to adjust refresh ltv 
      result.sead$refresh.ltv <- result.sead$refresh.ltv[1]*result.sead$House_Price_Index[1]/result.sead$House_Price_Index
    }
    
    forecast.base <- rbind(forecast.base, result.base)                                ### combine the forecast and the historic macro data under 3 scenarios
    forecast.ad <- rbind(forecast.ad, result.ad)
    forecast.sead <- rbind(forecast.sead, result.sead)
    cat(paste0(format(which(subset.all.data$Loan.Number == loan) *100/ length(subset.all.data$Loan.Number), digits = 1), "%\n"))   ### display the progress of the loop
  }
  list(forecast.base = forecast.base, forecast.ad = forecast.ad, forecast.sead = forecast.sead)                  ### output a list of three datasets ready for prediction under different scenarios 
}



### predict.PD function's sub functions: predict.pd

### output the actuall PD probability and weighted prediction PD probability for each historical month
predict.PD <- function(newdata, cox.fit, date.seq, input.list.month.pd = NULL){
  PD.actual <- c()
  PD.pred <- c()
  list.month.pd <- list()
  for(month in date.seq){                                                  ### for each month, extract all the loan data that have records in that month
    print(month)
    subset.all.data <- subset(newdata, DATE == month)                      ### given one month, subset the newdata
    subset.all.data <- data.table(subset.all.data)
    if(length(input.list.month.pd) == 0){ 
      pred.pd <- predict.pd(subset.all.data, cox.fit)                      ### if not given PD, then use PD model to obtain the predicted PD probability                        
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


### output the probability of default for eacl loan for given month
predict.pd <- function(newdata, cox.fit){
  newdata <- data.table(newdata)
  if(dim(newdata)[1]>0){
    surv.fit <- survfit(cox.fit, newdata = newdata, individual = F, se.fit = F)
    tmp <- newdata[, list(index = which.min(abs(surv.fit$time - start))), by = c("Loan.Number")]     ### find the time in surv.fit to represent 'start' 
    row.index <- tmp$index
    surv.prob.start <- diag(surv.fit$surv[row.index,])                                            ### output the survival probability of that time
    tmp <- newdata[, list(index = which.min(abs(surv.fit$time - MOB))), by = c("Loan.Number")]      ### find the time in surv.fit to represent 'MOB'
    row.index <- tmp$index
    surv.prob.end <- diag(surv.fit$surv[row.index,])                                               ### output the survival probability of that time
    return(1 - surv.prob.end/surv.prob.start)                                                     ### output the probability of default for each loan 
  }else{
    return(rep(NA, dim(newdata)[1]))                                              ### if the input data has no loan, then output missing data
  }
}

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

### Although the function name is forecast.LGD, the output is LOSS
### for each month:
### 1. obtain the predicted PD probability from the estimated PD model
### 2. calculate the total principal balance
### 3. obtain the predicted LGD lossrate from the estimated LGD model
### 4. multiply the above three items, and obtain the estimated loss
forecast.LGD <- function(newdata, cox.fit, lossrate.logit.linear, date.seq.for, input.list.month.pd = NULL, mixture = F){
  LGD.pred <- c()
  for(month in date.seq.for){                  
    subset.all.data <- subset(newdata, DATE == month)          ### for each month in the dataset, first subset all the data from that month
    subset.all.data <- data.table(subset.all.data)
    if(length(input.list.month.pd) == 0){                      ### obtained the predicted PD probability for the subset loan from the estimated PD model.
      pred.pd <- predict.pd(subset.all.data, cox.fit)            ### if not given PD, then use PD model to obtain the predicted PD probability   
    }else{
      pred.pd <- input.list.month.pd[[month]]                   ### if given PD, then use that directly
    }
    total.balance <- sum(subset.all.data$Current.Principal.Balance)      ###  ### sum the current principal balance of the all loans
    default.balance <- sum(subset.all.data$Current.Principal.Balance[which(subset.all.data$If_Default==1)])   ### sum the current principal balance of the defaulted loans
    pred.lossrate.logit <- predict(lossrate.logit.linear, newdata = subset.all.data)  ### obtained the predicted loss rate for the subset loan from the estimated LGD model.
    if(mixture){
      pred.lossrate <- one.probability + (1 - one.probability)*inv.logit(pred.lossrate.logit)   ### if the lossrate is mixtured, then adjust it with one.probability
    }else{
      pred.lossrate <- inv.logit(pred.lossrate.logit)                                   ### if the lossrate is not mixtured, then do a inverse logit transformation on pred.lossrate.logit directly.
    }
    LGD <- pred.pd* subset.all.data$Current.Principal.Balance* pred.lossrate    ### LOSS would be the multiplication of estimated PD, LGD and Current.Principal.Balance
    LGD.pred <- c(LGD.pred, sum(LGD, na.rm = T))
  }
  LGD.output<-list(LGD.pred, total.balance)               ### output the LGD result and total balance
  return(LGD.output)
}

