################################################################################
# import data
################################################################################

library(dplyr)
library(ggplot2)
library(ggpubr)

options(dplyr.summarise.inform = FALSE)

setwd("C:/Users/yijdi/OneDrive/Desktop/trips_data")

load("mobileTrips.RData")
load("obd2Trips.RData")

################################################################################
# Convert mobile and obd list to data frame
################################################################################

mobileData <- do.call(
  rbind,
  mobileTripsTrain
)

obdData <- do.call(
  rbind,
  obd2TripsTrain
)

################################################################################
# back up data set used to join in the end by 'trip_id' so that column formats
# are kept original
################################################################################

mobileDataCopy <- mobileData %>%
  mutate(
    trip_id1 = cumsum(!duplicated(trip_id))
  )

obdDataCopy <-obdData  %>%
  mutate(
    trip_id1 = cumsum(!duplicated(trip_id))
  )

################################################################################
# convert time stamp to time format
################################################################################

mobileData <- mobileData %>%
  mutate(
    trip_id1 = cumsum(!duplicated(trip_id)),
    timestamp = as.POSIXct(timestamp, origin="1970-01-01"),
    created_at = as.POSIXct(created_at, origin="1970-01-01"),
    speed = speed * 3.6 # covnert m/s to km/s based on observation
  ) 

obdData <- obdData %>%
  mutate(
    trip_id1 = cumsum(!duplicated(trip_id))
  )


################################################################################
# sample plot to explore trip data
########################


########################################################

plot_trip<- function(data, id, type){
  df_sub <- data %>% filter(
    trip_id1 == id
  ) 
  
  p <- ggplot(df_sub, aes(x=timestamp, y=speed)) +
    labs(
      x =  "time",
      y = "speed in km/h",
      title = paste0("trip from ", type, " : id = ", id)
    ) + 
    geom_line(col = "red")
  
  return(p )
  
}

plot_trip(mobileData, 1, "mobile")
plot_trip(obdData, 1, "obd")

################################################################################
# function: smooth function reduce trip noise
################################################################################
smooth <- function(tripData){
  fit.smooth <- loess(
    speed ~ as.numeric(timestamp),
    data = tripData,
    span= 0.03
  )
  
  tripData$speedSmooth <- predict(fit.smooth, as.numeric(tripData$timestamp))
  
  return(tripData)
}

################################################################################
# funtion: generate a unique id to each interval where interval period is
# a continuous data points with speed > 20
################################################################################

divideInterval <- function(tripData){
  tripData <- tripData %>%
    distinct()  %>%
    mutate(
      intervalID = ifelse(speedSmooth < 20, 1 ,0)
    ) %>%
    mutate(
      intervalID = cumsum(intervalID) + 1
    ) %>%
    filter(
      speedSmooth >= 20
    )
  
  return(tripData)
}

################################################################################
# funtion: get various statistics for each interval that are used to 
# match with the other trips
################################################################################

intervalStat <- function(tripData){
  
  tripData  <- divideInterval(tripData)
  
  # get percentile speed and length of interval, these two are used to
  # determine if two intervals are from same trip
  tripStat <- tripData %>% 
    group_by(intervalID) %>%
    summarise(
      trip_id1 = max(trip_id1),
      interval  = as.numeric(max(timestamp)) - as.numeric(min(timestamp)),
      intervalBeginTime = min(timestamp),
      intervalEndTime   =  max(timestamp),
      speed_01 = quantile(speedSmooth, probs = 0.1),
      speed_02 = quantile(speedSmooth, probs = 0.2),
      speed_03 = quantile(speedSmooth, probs = 0.3),
      speed_04 = quantile(speedSmooth, probs = 0.4),
      speed_05 = quantile(speedSmooth, probs = 0.5),
      speed_06 = quantile(speedSmooth, probs = 0.6),
      speed_07 = quantile(speedSmooth, probs = 0.7),
      speed_08 = quantile(speedSmooth, probs = 0.8),
      speed_09 = quantile(speedSmooth, probs = 0.9),
      maxSpeed = quantile(speedSmooth, probs = 1)
    ) %>%
    # filters to reduce measurement noise
    filter(
      maxSpeed > 25,
      interval > 40
    )
  
  return(tripStat)
}

################################################################################
# funtion: generate matching score for given pair of trips
################################################################################

getTripMatchScore <- function(mobileStat, obdStat){
  
  # cross join and get every combination of intervals from two sources
  tripMatchScore <- merge(
    x = mobileStat, y = obdStat, by = NULL
  ) %>%  
    mutate(
      lengthDif = abs(interval.x - interval.y) / interval.x 
    ) %>%
    mutate(
      lengthMatch = ifelse(lengthDif > 0.05, 0, 1 )
    ) %>%
    mutate(
      # speed similarity is average difference in 5 points
      speedMatchScore = (
        abs(speed_01.x - speed_01.y) +
          abs(speed_03.x - speed_03.y) +
          abs(speed_05.x - speed_05.y) +
          abs(speed_07.x - speed_07.y) +
          abs(speed_09.x - speed_09.y)
      ) / 5 
    ) %>%
    mutate(  
      # matching score is formula of both similarity in speed distribution 
      # and interval length
      intervalMatchScore = exp(
        5 - speedMatchScore - 5 * lengthDif
      ) * lengthMatch
    ) %>%
    group_by(
      intervalID.x,
      intervalID.y
    ) %>%
    arrange(-intervalMatchScore)
  
  # Based on matching scores, use a loop to generate best matching combination
  # and also make sure each interval is only used once in calculating trip match
  # score
  I <- min(
    length(unique(tripMatchScore$intervalID.x)), 
    length(unique(tripMatchScore$intervalID.y))
  )
  
  tripMatchScore <- tripMatchScore[
    ,c(
      "intervalID.x", 
      "intervalID.y", 
      "intervalMatchScore",
      "trip_id1.x",
      "trip_id1.y",
      "intervalBeginTime.x",
      "intervalEndTime.x",
      "intervalBeginTime.y",
      "intervalEndTime.y",
      "lengthDif"
    )
  ]
  
  for(i in (1:I)){
    
    # the next best pair is the first row as the data is sorted by descending
    # interval match score
    move <- data.frame(tripMatchScore[1, ])
    
    if(is.na(bestTripMatchAll)){
      bestTripMatchAll <<- move
    }else{
      bestTripMatchAll <<- rbind(
        bestTripMatchAll,
        move
      )
    }
    
    bestTripMatchTemp <- bestTripMatchAll %>%
      filter(
        trip_id1.x == max(mobileStat$trip_id1),
        trip_id1.y == max(obdStat$trip_id1)
      )
    
    # remove the selected pair from candidate pool
    tripMatchScore <- tripMatchScore %>% filter(
      intervalID.x != move$intervalID.x,
      intervalID.y != move$intervalID.y
    )
  }
  
  result <- data.frame(
    mobileStat$trip_id1[1],
    obdStat$trip_id1[1], 
    # trip similarity score is sum of interval scores
    sum(bestTripMatchTemp$intervalMatchScore)
  )
  
  return(result)
}

################################################################################
# Loop to calculate trip matching score for each combination
################################################################################

bestTripMatchAll <<- NA
tripMatchResult <- NA

for(i in (1:length(mobileTripsTrain))){
  
  mobileTripData <- mobileData[
    mobileData$trip_id1 == i,
  ]
  
  print(paste("matching mobile trip", as.character(i)))
  
  mobileTripData <- smooth(mobileTripData)
  
  # remove bad data 
  if(max(mobileTripData$speedSmooth) < 10){
    print(paste0("skipped mobile trip ", i))
    next
  }
  
  mobileStat <- intervalStat(mobileTripData) 
  
  for(j in (1:length(obd2TripsTrain))){
    
    obdTripData <-  obdData[
      obdData$trip_id1 == j,
    ]
    
    obdTripData <- smooth(obdTripData)
    
    # remove bad data 
    if(max(obdTripData$speedSmooth) < 10){
      print(paste0("skipped obd trip ", j))      
      next
    }
    
    obdStat <- intervalStat(obdTripData)
    
    if(is.na(tripMatchResult)){
      tripMatchResult <- getTripMatchScore(mobileStat , obdStat)
    }else{
      tripMatchResult <- bind_rows(
        tripMatchResult, 
        getTripMatchScore(mobileStat , obdStat)
      )
    }
  }
}

colnames(tripMatchResult) <- c(
  'mobileTripID',
  'obdTripID',
  'score'
)

################################################################################
# filter by trip match score to get final result 
################################################################################

tripMatchFinal <- tripMatchResult %>% 
  group_by(mobileTripID) %>%
  summarise(
    score = max(score, na.rm = TRUE)
  ) %>% 
  inner_join(
    tripMatchResult,
    by = c("mobileTripID", "score")
  ) %>%
  select(
    mobileTripID,
    obdTripID,
    score
  ) %>% 
  filter(score > 25)


tripScorePlotDf <- tripMatchFinal %>%
  mutate(
    name = paste(mobileTripID, " vs ", obdTripID)
  )

ggplot(
  tripScorePlotDf,
  aes(reorder(name, -score)) 
) + geom_col(aes(y = score), fill = 'orange' ) +
  theme(axis.text.x = element_text(angle = 90))

################################################################################
# funtion: get start and end timestamp for each trip used to find out which 
# begin and end point to slice for common trips
################################################################################

tripStat <- function(sourceData){
  
  tripStat <- sourceData %>% 
    group_by(trip_id1) %>%
    summarise(
      tripBeginTime = min(timestamp),
      tripEndTime   =  max(timestamp)
    )
  return(tripStat)
}

tripStatMobile <- tripStat(mobileData)
tripStatObd <- tripStat(obdData)

################################################################################
# calculate begin time and end time of common trips from two sources by
# comparing gap length between interval start/end time and trip start/end time
################################################################################

commonTripTimeframe <- tripMatchFinal %>%
  inner_join(
    bestTripMatchAll,
    by = c(
      "mobileTripID" = "trip_id1.x",
      "obdTripID"    ="trip_id1.y"
    )
  ) %>% 
  inner_join(
    tripStatMobile,
    by = c("mobileTripID" = "trip_id1")
  ) %>% 
  inner_join(
    tripStatObd,
    by = c("obdTripID" = "trip_id1")
  ) %>%
  group_by(
    mobileTripID
  ) %>%
  filter(
    intervalMatchScore == max(intervalMatchScore)
  ) %>%
  mutate(
    subTripBeginTimeMobile =
      intervalBeginTime.x - min(
        as.numeric(intervalBeginTime.x) - as.numeric(tripBeginTime.x),
        intervalBeginTime.y - tripBeginTime.y
      ),
    subTripBeginTimeObd =
      intervalBeginTime.y - min(
        as.numeric(intervalBeginTime.x) - as.numeric(tripBeginTime.x),
        intervalBeginTime.y - tripBeginTime.y
      ),
    subTripEndTimeMobile =
      intervalEndTime.x + min(
        as.numeric(tripEndTime.x) - as.numeric(intervalEndTime.x),
        tripEndTime.y - intervalEndTime.y 
      ),
    subTripEndTimeObd =
      intervalEndTime.y + min(
        as.numeric(tripEndTime.x) - as.numeric(intervalEndTime.x),
        tripEndTime.y - intervalEndTime.y 
      ) 
  )


commonTripFull <- tripMatchFinal %>%
  inner_join(
    bestTripMatchAll,
    by = c(
      "mobileTripID" = "trip_id1.x",
      "obdTripID"    ="trip_id1.y"
    )
  ) %>% 
  inner_join(
    tripStatMobile,
    by = c("mobileTripID" = "trip_id1")
  ) %>% 
  inner_join(
    tripStatObd,
    by = c("obdTripID" = "trip_id1")
  ) %>%
  group_by(
    mobileTripID
  ) %>%
  filter(
    intervalMatchScore == max(intervalMatchScore)
  )
################################################################################
# assign row number before slicing to lock down the index with respect to 
# original data frame
################################################################################

mobileData$rownumber <- seq(1:nrow(mobileData))
obdData$rownumber <- seq(1:nrow(obdData))

# slice entire trip data set by common trip timeframe 
mobileSubset <- mobileData %>%
  inner_join(
    commonTripTimeframe,
    by = c("trip_id1" = "mobileTripID")
  ) %>%
  filter(
    timestamp >= subTripBeginTimeMobile,
    timestamp <= subTripEndTimeMobile
  ) 

obdSubset <- obdData %>%
  inner_join(
    commonTripTimeframe,
    by = c("trip_id1" = "obdTripID")
  ) %>%
  filter(
    timestamp >= subTripBeginTimeObd,
    timestamp <= subTripEndTimeObd
  ) 

################################################################################
# look up common trips from back-up data using row number
################################################################################
mobileDataCopySub <- mobileDataCopy[
  mobileSubset$rownumber,
]

obdDataCopySub <- obdDataCopy[
  obdSubset$rownumber,
]

################################################################################
# split output to list like raw input.
################################################################################
mobileTripsTrainCommon <- split(
  mobileDataCopySub,
  f = mobileDataCopySub$trip_id1
)

obd2TripsTrainCommon <- split(
  obdDataCopySub, 
  f = obdDataCopySub$trip_id1
)

subColumnMobile <- function(obj){obj[, c(1:5)]}

mobileTripsTrainCommon <- lapply(
  mobileTripsTrainCommon, 
  subColumnMobile
)

subColumnObd <- function(obj){obj[, c(1:3)]}

obd2TripsTrainCommon <- lapply(
  obd2TripsTrainCommon, 
  subColumnObd
)

output <- list(
  obd2TripsTrainCommon,
  mobileTripsTrainCommon
)

################################################################################
# Combined trip data plot
################################################################################

combinedTripPlot <- function(commonTripTimeframe, mobileData, obdData, mobileTripID){
  df1 = commonTripTimeframe %>% 
    mutate(
      mobileTimestampMove = (intervalBeginTime.y - tripBeginTime.y) -
        difftime(intervalBeginTime.x, tripBeginTime.x, units = 'secs')
      
    ) %>% data.frame()
  
  sample = df1[df1$mobileTripID == mobileTripID , ]
  df2 <- mobileData %>% 
    filter(trip_id1 == sample$mobileTripID) %>%
    inner_join(
      df1 %>% select(mobileTripID, mobileTimestampMove, tripBeginTime.x),
      by = c("trip_id1"  = "mobileTripID")
    ) %>% mutate(
      newTime = as.numeric(
        round(difftime(timestamp, tripBeginTime.x) + mobileTimestampMove)
      )
      
    )
  
  df3 <- obdData %>% 
    filter(trip_id1 == sample$obdTripID) %>%
    mutate(timestamp = round(timestamp)) %>%
    group_by(
      timestamp
    ) %>% summarise(
      speed = mean(speed) 
    )
  
  df4 <- full_join(
    df2,
    df3,
    by = c("newTime" = "timestamp")
  ) 
  
  p <- ggplot(df4, aes(x  = newTime) ) + 
    geom_line(aes( y = speed.y, color = "obd"), size = 1.5 ) +
    geom_line(aes( y = speed.x, color = "mobile"), size = 1) +
    labs(
      x =  "timestamp",
      y = "speed in km/h",
      title = "matched trip",
      subtitle = paste0(
        "mobile trip ",
        sample$mobileTripID,
        " with ",
        "obd trip ",
        sample$obdTripID
        
      ),
      caption = "note: mobile time stamp converted to obd timestamp"
    )
  
  print(p)
  
}

combinedTripPlot(commonTripTimeframe, mobileData, obdData, 1)
combinedTripPlot(commonTripTimeframe, mobileData, obdData, 24)
