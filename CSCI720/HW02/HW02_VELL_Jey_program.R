# Initially the file will be loaded into a dataframe and bins are being created based on following rule
# Here we quantized the speed in 1mph bins and we use the floor function to round down the decimal points
# to nearest integer value. 
# pathOfThefile : Path for the input file
# This function returns a dataframe with binned speeds and counts

createBins <- function(pathOfThefile){
  df <- read.csv(pathOfThefile, header = FALSE)
  old.par <- par(mfrow=c(1, 2))
  newDataFrame <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(df) <- c("Speed")
  colnames(newDataFrame) <- c("Speeds", "Counts")
  numberOfRows = nrow(df)
  counter2 <- 1
  
  for (counter in 1:numberOfRows){
    binSpeed = floor(df[counter, "Speed"])
    if(binSpeed %in% newDataFrame$Speeds){
      rowIndex = which(newDataFrame$Speeds == binSpeed)
      newDataFrame[rowIndex, "Counts"] = newDataFrame[rowIndex, "Counts"] + 1
    }else {
      newDataFrame[counter2,] <- c(binSpeed,1)
      counter2 <- counter2 + 1
    }
  }
  return (newDataFrame)
}

# This is a step needed for mean calculation, basically in this step we multiply the bin  
# speed with it's frequency and add those values together
# Find the sum of x*f values for a given dataframe where first column has values of x and 
# df : input dataframe
# The output will be the total sum of speeds into frequencies

findSumValFreq <- function(df){
  n <- nrow(df)
  theCols <- colnames(df)
  param <- theCols[1]
  counter <- theCols[2]
  totalSum <- 0
  for(i in 1: n){
    totalSum <- totalSum + (df[i, param] * df[i, counter] )
  }
  return (totalSum)
}

# Find the mean value for a data fram where first column is parameter, second column is counter
# First we need to find the column names, 
# Here we use the findSumValFreq funciton to calculate the Summation of speed times frequencies
# Finally we divide that by total frequency for all the bins 
# dataFrame : Input dataFrame for mean calculation
# The output value will be the mean value

findMean <- function(dataFrame){
  newNumberOfRows = nrow(dataFrame)
  totalSum <- findSumValFreq (dataFrame)
  #print(c( "total Sum from Function : ", totalSum,  " total count is : ", sum(dataFrame[,2])))
  
  meanVal <- totalSum/sum(dataFrame[,2])
  return (meanVal)
  
}

# Find the variance of a dataframe
# We use variance = frequency *(targetVariable - calculated mean)^2 / N If N is greater than 100
# if N is < 100 then variance = frequency *(targetVariable - calculated mean)^2 / (N -1)

findVariance <- function(dataFrame, meanVal){
  theCols <- colnames(dataFrame)
  param <- theCols[1]
  counter <- theCols[2]
  newNumberOfRows = nrow(dataFrame)
  theNVal <- 0
  tSum <- 0
  theVariance <- 0
  if(newNumberOfRows> 0){
    theNVal <- newNumberOfRows
  }else{
    theNVal  <- newNumberOfRows - 1
  }
  for(i in 1: newNumberOfRows){
    tSum <- tSum + (((dataFrame[i, param] - meanVal)^2) * dataFrame[i, counter] )
  }
  
  theVariance <- tSum/theNVal
  standDeviation <- sqrt(theVariance)
  #print(c("The standard deviation is : ",standDeviation) )
  return (theVariance)
}

# Find the median value for a dataframe where first column is parameter, second column is counter
# First we need to find the column names, second we need to multiple relevant rows for each column
# Add the total and divide that by total count of primary parameter

findMedian <- function(dataFram){
  theCols <- colnames(dataFram)
  param <- theCols[1]
  counter <- theCols[2]
  newNumberOfRows = nrow(dataFram)
  centerPoint1 <- 0
  centerPoint2 <- 0
  medianVal <- 0
  if(newNumberOfRows%%2 == 0){
    
    centerPoint1 = floor((newNumberOfRows+1)/2)
    centerPoint2 = ceiling(floor((newNumberOfRows+1)/2))
  }else{
    centerPoint1 = (newNumberOfRows+1)/2
    medianVal <- antherDF[centerPoint1,0]
  }
  antherDF <- dataFram[order(param),] 
  
  
  medianVal <- antherDF[centerPoint1]
  return (medianVal)
  
}

# In the following funciton we implemented the OTUSU's method for objective function
# the ObjectFunction is weighted variance, our goal for this object function is to 
# minimize the total weighted variance 
# minimize [(w * σ^2) for Left Side  + (w * σ^2 )Right Side]
# df: Is the input data frame for this function
# The output will be dataframe with speeds and their relevant total weighted variances

tryThemAll <- function(df){
  otusuDataFrame <- data.frame(matrix(ncol = 2, nrow = 0))
  lDF <- data.frame(matrix(ncol = 2, nrow = 0))
  rDF <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(otusuDataFrame) <- c("Val", "Total")
  colnames(lDF) <- c("Speeds", "Counts")
  colnames(rDF) <- c("Speeds", "Counts")
  numRows = nrow(df)
  theCols <- colnames(df)

  for(i in 1: numRows){
    # Here we split the data frame based on every speed value
    lDF <- df[df$Speeds< df[i,1],]
    rDF <- df[df$Speeds >= df[i,1],]
    sumlDFtimesVar <- 0
    sumrDFtimesVar <-0
    if(nrow(lDF)>0){
      totalSumlDF <- findSumValFreq(lDF)
      meanlDF <- findMean(lDF)
      varlDF <- findVariance(lDF, meanlDF)
      sumlDFtimesVar <- totalSumlDF*varlDF

    }
    if(nrow(rDF)>0){
      totalSumrDF <- findSumValFreq(rDF)
      meanrDF <- findMean(rDF)
      varrDF <- findVariance(rDF, meanrDF)
      sumrDFtimesVar = totalSumrDF*varrDF
      
    }

    # Total weighted variance for the speed df[i,1]
    totalSumvarlDFplusvarrDF = sumlDFtimesVar + sumrDFtimesVar
    
    otusuDataFrame[i, "Val"] <-df[i,1]
    otusuDataFrame[i,"Total"] <- totalSumvarlDFplusvarrDF
    rm(totalSumvarlDFplusvarrDF)
    rm(lDF)
    rm(rDF)
    
  }
  
  return(otusuDataFrame)
}

# Plot the bin speeds vs frequency  
# df : data frame with speed and frequencies, 
# meanVal : Calculated mean value for that data frame
# The output will be a bar chart with mean line

plotSpeedFreqWithMean <- function(df, meanVal){
  p2<-ggplot(data=df, aes(x=(df$Speeds), y=Counts)) +
    geom_bar(stat="identity" ) 
  p2<- p2+ geom_vline(xintercept = meanVal, linetype=1, 
                      color = "green", size=1.5) +
    annotate("text", x=meanVal, y=40, label="Mean", size=4)
  p2 <- p2+xlab("Speed (mph)") + ylab("Counts") + 
  ggtitle("Speed vs Frequency") +
    theme(plot.title = element_text(family="Menlo-Bold", color = "black", size=14, face="bold", hjust=0.5)) 
    
  p2<- p2 + scale_x_continuous(breaks=seq(min(df$Speeds),max(df$Speeds),1))
  return (p2)
}

# This is a line plot with lowest weighted variance identified for the relevant speed
# df : Data frame with speeds and weighted variances
# p: output plot

plotWeightedVarianceWithSpeed <- function(df, clusterCenter){
  p <- ggplot(data=df, aes(x=df$Val, y=df$Total, group=1)) +
    geom_line(color="red")+
    geom_point() +
    geom_vline(xintercept = clusterCenter, linetype=1, 
               color = "green", size=1.5) +
    annotate("text", x=clusterCenter, y=max(df$Total), label="Cluster Center Line", size=4)
  p = p+xlab("Speed (mph)") + ylab("Weighted Variance") +
    ggtitle("Speed vs Weighted Variance") +
    theme(plot.title = element_text(family="Menlo-Bold", color = "black", size=14, face="bold", hjust=0.5 ))
  p <- p  + scale_x_continuous(breaks=seq(min(df$Val),max(df$Val),1)) 
  return (p)
}

plotWeightedVarianceWithSpeedBarChart <- function(df,clusterCenter ){
  p<-ggplot(data=df, aes(x=(df$Val), y=df$Total)) +
    geom_bar(stat="identity" ) 
  
  p <- p+ geom_vline(xintercept = clusterCenter, linetype=1, 
                     color = "green", size=1.5)+
    annotate("text", x=clusterCenter, y=max(df$Total), label="Cluster Center Line", size=4)
  p = p+xlab("Speed (mph)") + ylab("Weighted Variance") +
    ggtitle("Speed vs Weighted Variance") +
    theme(plot.title = element_text(family="Menlo-Bold", color = "black", size=14, face="bold", hjust=0.5))
  p <- p + scale_x_continuous(breaks=seq(min(df$Val),max(df$Val),1))   
  return (p)
}


# This is the main function which as cluster center, and all the important functions
# are getting called

main <- function(){
  library(ggplot2)
  ###
  ##  IMPORTANT  ####
  ### PLEASE CHANGE THE FOLLOWING FILE PATH TO THE RIGHT LOCATION
  ####
  file_path <- "/Users/jeyvell/BigDataCertification/CSCI720/HW02/DATA_v2175_FOR_CLUSTERING_using_Otsu.csv"
  newDataFrame <- createBins(file_path)
  meanVal = findMean(newDataFrame)
  print (newDataFrame)
  print(c("The mean value is : ", meanVal))
  
  theVar = findVariance (newDataFrame, meanVal)
  print(c("The variance value is : ", theVar))
  print(c("The standard diviation  is : ", sqrt(theVar)))

  d <- tryThemAll(newDataFrame)
  
  # Here the assumption is lower the speed higher the safety therefore if two speeds get the
  # same lowest weight variance then we pick the lower speed from them
  rowIn <- which(d$Total ==min(d$Total))
  clusterCenter <- 0
  if(length(rowIn) >1){
    #Finding the lowest value
    for (i in rowIn){
      if(clusterCenter==0){
        clusterCenter <- d[i, "Val"]
      }else if(clusterCenter > d[i, "Val"]){
        clusterCenter <- d[i, "Val"]
      }
    }
  }else{
    #The minimum weighted variance happened only once
    clusterCenter <- d[rowIn, "Val"]
  }
  

  leftCluster <- d[d$Val< clusterCenter,]
  rightCluster <- d[d$Val >= clusterCenter,]
  print(c("The cluster center point is : ", clusterCenter))
  
  library(ggplot2)
  library(grid)
  library(gridExtra)
  require(gridExtra)
  p1 <- plotSpeedFreqWithMean (newDataFrame,meanVal )
  p2 <- plotWeightedVarianceWithSpeed(d, clusterCenter)
  p3 <- plotWeightedVarianceWithSpeedBarChart(d, clusterCenter)

  grid.arrange(p1, p2, p3, ncol = 3 )
  #plot(p1)
}

# Executing the main function
call <- main()






