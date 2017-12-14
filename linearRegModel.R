###########################################################
#### Load a "library" dataset of predictor variables  #####
###########################################################

load("predictors.rda")

############################
#### Examine the data  #####
############################

## Clean Data / Remove Extreme Outliers ##
predictors = data.frame(predictors)
predictors$HOU_windmax[predictors$HOU_windmax > 51] = NA
predictors$HOU_precip[predictors$HOU_precip > 1.7] = NA
predictors$HOU_hummax[predictors$HOU_hummax == 37] = NA
predictors$SEA_windmax[predictors$SEA_windmax == 76] = NA
predictors$SEA_tmax[predictors$SEA_tmax == 129] = NA
predictors$SEA_precip[predictors$SEA_precip > 1.3] = NA
predictors$SEA_windmean[predictors$SEA_windmean == 43] = NA
predictors$SAN_precip[predictors$SAN_precip == 2.51] = NA
predictors$DEN_tmax[predictors$DEN_tmax > 50000] = NA
predictors$DEN_tmax[predictors$DEN_tmax < 0] = NA
predictors$DEN_tmin[predictors$DEN_tmin > 460000] = NA
predictors$DEN_windmax[predictors$DEN_windmax > 60] = NA
predictors$DEN_precip[predictors$DEN_precip > 1.7] = NA
predictors$MIA_tmax[predictors$MIA_tmax == 48] = NA
predictors$MIA_tmean[predictors$MIA_tmean == 42] = NA
predictors$MIA_precip[predictors$MIA_precip > 1.4] = NA
predictors$MIA_windmax[predictors$MIA_windmax > 55] = NA
predictors$MIA_windmean[predictors$MIA_windmean == 30] = NA
predictors$ANC_windmax[predictors$ANC_windmax > 41] = NA
predictors$ANC_windmean[predictors$ANC_windmean > 27.5] = NA
predictors$ANC_precip[predictors$ANC_precip == 10.4] = NA
predictors$ANC_hummin[predictors$ANC_hummin == 5] = NA
predictors$SFO_tmax[predictors$SFO_tmax == 107] = NA
predictors$SFO_tmean[predictors$SFO_tmean == 85] = NA
predictors$SFO_precip[predictors$SFO_precip == 3.43] = NA
predictors$SFO_windmax[predictors$SFO_windmax == 132] = NA
predictors$SLC_precip[predictors$SLC_precip > 100] = NA
predictors$SLC_tmax[predictors$SLC_tmax > 600000] = NA
predictors$SLC_windmax[predictors$SLC_windmax == 62] = NA
predictors$IAD_windmax[predictors$IAD_windmax == 59] = NA
predictors$IAD_precip[predictors$IAD_precip > 1.4] = NA
predictors$LGA_windmax[predictors$LGA_windmax > 50] = NA
predictors$LGA_windmean[predictors$LGA_windmean > 60] = NA
predictors$LGA_hummin[predictors$LGA_hummin == 0] = NA
predictors$LGA_precip[predictors$LGA_precip > 1.5] = NA
predictors$MSP_windmax[predictors$MSP_windmax > 50] = NA
predictors$MSP_windmean[predictors$MSP_windmean > 25] = NA
predictors$MSP_precip[predictors$MSP_precip > 2] = NA
predictors$MSP_hummin[predictors$MSP_hummin == 100] = NA
predictors$OKC_precip[predictors$OKC_precip > 2.6] = NA
predictors$OKC_windmax[predictors$OKC_windmax == 241] = NA
predictors$OKC_windmean[predictors$OKC_windmean > 30] = NA
predictors$OKC_tmax[predictors$OKC_tmax < 16] = NA
predictors$BNA_precip[predictors$BNA_precip > 1.2] = NA
predictors$BNA_windmax[predictors$BNA_windmax > 50] = NA
predictors$BNA_windmean[predictors$BNA_windmean == 22] = NA
predictors$DTW_precip[predictors$DTW_precip > 1.5] = NA
predictors$DTW_windmax[predictors$DTW_windmax > 80] = NA
predictors$ORD_windmax[predictors$ORD_windmax == 55] = NA
predictors$ORD_precip[predictors$ORD_precip > 2.4] = NA
predictors = as.matrix(predictors)

###########################################################
#### Build a "library" dataset of response variables  #####
###########################################################

responsecodes = c("ANC", "HOU", "IAD", "SEA", "MSP", "DTW", "SFO", "ORD", "OKC", "SAN", "BNA", "LGA", "DEN", "MIA", "SLC")
responsevariables = c("tmax", "tmin", "precip")
responsecolumnnames = as.vector(t(outer(responsecodes, responsevariables, paste, sep="_"))) 
responses = predictors[, responsecolumnnames]

## Convert into Celsius and Millimeters ##
for (city in responsecodes)
{
  responses[,as.vector(t(outer(city, "tmax", paste, sep="_")))] = ((responses[,as.vector(t(outer(city, "tmax", paste, sep="_")))]) - 32)*(5/9)
  responses[,as.vector(t(outer(city, "tmin", paste, sep="_")))] = ((responses[,as.vector(t(outer(city, "tmin", paste, sep="_")))]) - 32)*(5/9)
  responses[,as.vector(t(outer(city, "precip", paste, sep="_")))] = (responses[,as.vector(t(outer(city, "precip", paste, sep="_")))])*25.4
}


##################################
#### Build Model Predictors  #####
##################################

emptylist = rep(list(NA), 5) 
names(emptylist) = paste(2:6, "days") 
models = rep(list(emptylist), ncol(responses)) 
names(models) = colnames(responses) 

cities = responsecodes
predictorvariables = c("tmax", "tmean", "tmin", "hummax", "hummean", "hummin", "windmax", "windmean", "precip")

for (predictorcity in cities)
{
  predictorcolumnnames = as.vector(t(outer(predictorcity, predictorvariables, paste, sep="_")))
  citycolumnnames = as.vector(t(outer(predictorcity, responsevariables, paste, sep="_")))
  cityresponses = responses[, citycolumnnames]
  for (response in colnames(cityresponses))
  {
    for (forecast in 2:6)
    {
      startday = 8
      endday = nrow(predictors) - 6
      days = startday:endday
      n = length(days)
      X = matrix(NA, n, 24)
      Y = matrix(NA, n, 1)
      for (i in 1:n)
      {
        Y[i,1] = responses[days[i] + forecast, response]
        X[i,1:9] = predictors[days[i]-1, as.vector(t(outer(predictorcity, predictorvariables, paste, sep="_")))]
        X[i,10:18] = predictors[days[i]-2, as.vector(t(outer(predictorcity, predictorvariables, paste, sep="_")))]
        X[i,19] = mean(predictors[(days[i]-7):(days[i]-1), as.vector(t(outer(predictorcity, "tmax", paste, sep="_")))])
        X[i,20] = mean(predictors[(days[i]-7):(days[i]-1), as.vector(t(outer(predictorcity, "tmean", paste, sep="_")))])
        X[i,21] = mean(predictors[(days[i]-7):(days[i]-1), as.vector(t(outer(predictorcity, "tmin", paste, sep="_")))])
        X[i,22] = mean(predictors[(days[i]-7):(days[i]-1), as.vector(t(outer(predictorcity, "hummean", paste, sep="_")))])
        X[i,23] = mean(predictors[(days[i]-7):(days[i]-1), as.vector(t(outer(predictorcity, "windmean", paste, sep="_")))])
        X[i,24] = mean(predictors[(days[i]-7):(days[i]-1), as.vector(t(outer(predictorcity, "precip", paste, sep="_")))])
      }
      models[[response]][[paste(forecast, "days")]] = lm(Y~X)
    }
  }
}

###########################################
#### Save Model Coefficients to disk  #####
###########################################

emptylist = rep(list(NA), 5) 
names(emptylist) = paste(2:6, "days") 
modelcoefficients = rep(list(emptylist), ncol(responses)) 
names(modelcoefficients) = colnames(responses)  

for (response in colnames(responses))
{
  for (forecast in 2:6)
  {
    modelcoefficients[[response]][[paste(forecast, "days")]] = coefficients(models[[response]][[paste(forecast, "days")]])
  }
}
save(modelcoefficients, file="modelcoefficients.rda")

####################################
#### Make Today's Predictions  #####
####################################

load("newpredictors.rda")
newpredictors = rbind(predictors, newpredictors)
newpredictors = newpredictors[-(1:(nrow(newpredictors)-7)),]

# Next we will make the predictions (for each response variable, and for each forecast period)
# First create a vector to store the predictions
todayspredictions = rep(NA, 45*5)  #15 * 3 * 5
names(todayspredictions) = as.vector(t(outer(colnames(responses), 2:6, paste, sep="_")))

for (predictorcity in cities)
{
  predictorcolumnnames = as.vector(t(outer(predictorcity, predictorvariables, paste, sep="_")))
  citycolumnnames = as.vector(t(outer(predictorcity, responsevariables, paste, sep="_")))
  cityresponses = responses[, citycolumnnames]
  # Now cycle through and make the predictions.  
  for (response in colnames(cityresponses))
  {
    for (forecast in 2:6)
    {
      # We have to make a "new X" to feed into the model
      X = matrix(NA, 1, 24) # The new X just has 1 row -- corresponding to today's data
      X[1,1:9] = newpredictors[7, as.vector(t(outer(predictorcity, predictorvariables, paste, sep="_")))]
      X[1,10:18] = newpredictors[6, as.vector(t(outer(predictorcity, predictorvariables, paste, sep="_")))]
      X[1,19] = mean(newpredictors[1:7, as.vector(t(outer(predictorcity, "tmax", paste, sep="_")))])
      X[1,20] = mean(newpredictors[1:7, as.vector(t(outer(predictorcity, "tmean", paste, sep="_")))])
      X[1,21] = mean(newpredictors[1:7, as.vector(t(outer(predictorcity, "tmin", paste, sep="_")))])
      X[1,22] = mean(newpredictors[1:7, as.vector(t(outer(predictorcity, "hummean", paste, sep="_")))])
      X[1,23] = mean(newpredictors[1:7, as.vector(t(outer(predictorcity, "windmean", paste, sep="_")))])
      X[1,24] = mean(newpredictors[1:7, as.vector(t(outer(predictorcity, "precip", paste, sep="_")))])
      
      # Make the prediction
      predictedresponse = predict(models[[response]][[paste(forecast, "days")]], as.data.frame(X))
      # Save the prediction
      todayspredictions[paste(response, forecast, sep="_")] = predictedresponse
    }
  }
}

######################################
########## Estimate Errors  ##########
######################################
rmse = rep(NA, 15*3*5)  
names(rmse) = as.vector(t(outer(colnames(responses), 2:6, paste, sep="_"))) 
for (response in colnames(responses))
{
  for (forecast in 2:6)
  {
    # Use the residual standard error
    rmse[paste(response, forecast, sep="_")] = summary(models[[response]][[paste(forecast, "days")]])$sigma
  }
}

######################################
#### Write the output to a file  #####
######################################

# The order of the cities is:

# Anchorage
# Houston
# Washington DC
# Seattle
# Minneapolis
# Detroit
# San Francisco
# Chicago
# Oklahoma City
# San Diego
# Nashville
# New York
# Denver
# Miami
# Salt Lake City

# We will output a similar file to disk, except that it doesn't include all of the cities, 
# and doesn't include the precipitation info.
outputvector = c(todayspredictions, rmse)
uniqname = "dorcheng"
todaysdate = as.character(Sys.Date())
filename = paste(uniqname, "_", todaysdate, ".txt", sep="")
cat(outputvector, file=filename, sep="\n")