# Initially the file will be loaded into a dataframe and bins are being created based on following rule
# Here we quantized the speed in 1mph bins and we use the floor function to round down the decimal points
# to nearest integer value. 
# pathOfThefile : Path for the input file
# This function returns a dataframe with binned speeds and counts

createBins <- function(pathOfThefile){
  # This file doesn't have any header
  df <- read.csv(pathOfThefile, header = FALSE)
  #old.par <- par(mfrow=c(1, 2))
  newDataFrame <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(df) <- c("Speeds", "Counts")
  
  df <- df[order(df$Speeds),] 
  colnames(newDataFrame) <- c("Speeds", "RightCounts", "WrongCounts")
  numberOfRows = nrow(df)

  counter2 <- 1
  
  for (counter in 1:numberOfRows){
    binSpeed = floor(df[counter, "Speeds"])
    if(binSpeed %in% newDataFrame$Speeds){
      rowIndex = which(newDataFrame$Speeds == binSpeed)
      if(df[counter, "Counts"] > 0){
        newDataFrame[rowIndex, "RightCounts"] = newDataFrame[rowIndex, "RightCounts"] + 1
      }else{
        newDataFrame[rowIndex, "WrongCounts"] = newDataFrame[rowIndex, "WrongCounts"] + 1
      }
      
    }else {
      if(df[counter, "Counts"] > 0){
        newDataFrame[counter2,] <- c(binSpeed,1, 0 ) 
      }else{
        newDataFrame[counter2,] <- c(binSpeed,0, 1) 
      }
      
      counter2 <- counter2 + 1
    }
  }
  return (newDataFrame)
}

# This function take the input data and build a dataframe that can be used by plot
# basially here we are colums to inlclude the counts for each "True" and "False" values
# For the speeds and counts provided in the data set
# newDataFrame : Input data from with actual speeds their right counts and wrong coutns
# This returns a dataframe that will have each speed relevant classificaiton and counts

buildActualClassifiedDF <- function(newDataFrame){
  classifiedDF <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(classifiedDF) <- c("Speeds", "Class", "Counts")
  rc = nrow(newDataFrame)

  counter <- 1
  for(i in 1: rc){
    classifiedDF [counter,"Speeds"] <- newDataFrame [i,"Speeds"]
    classifiedDF [counter,"Class"] <- 'True'
    classifiedDF [counter,"Counts"] <- newDataFrame[i,"RightCounts"] 
    counter <- counter +1
    classifiedDF [counter,"Speeds"] <- newDataFrame [i,"Speeds"]
    classifiedDF [counter,"Class"] <- "False"
    classifiedDF [counter,"Counts"] <- newDataFrame[i,"WrongCounts"] 

    counter <- counter +1
  }
  return(classifiedDF)
}

findCulsterTotalCounts <- function(df){
  count <- 0
  totalRows = nrow(df)
  if(totalRows>0){
    for(j in 1: totalRows){
      count <- count + df[j,"RightCounts"] + df[j,"WrongCounts"]
    }
  }
 
  return(count)
}

# Build Expected Classified Dataframe
# In this case we move the threshold value from lowest speed to highest speed
# and finding the expected classification values. 
# newDataFram: is the input where we have different speeds avaialble. 
# This will return a dataframe with speeds, expected classification, and counts

buildExpectedClassifiedDF <- function(newDataFrame){
  classifiedDF <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(classifiedDF) <- c("Speeds", "Class", "Counts")
  rc = nrow(newDataFrame)
  clusterCenter <- 0 
  counter <- 1
  for(i in 1: rc){
    clusterCenter <- newDataFrame [i,"Speeds"]
    leftClass <- newDataFrame[newDataFrame$Speeds < clusterCenter,]
    rightClass <- newDataFrame[newDataFrame$Speeds >= clusterCenter,]
    
    leftCount <- findCulsterTotalCounts(leftClass)
    rightCount <- findCulsterTotalCounts(rightClass)
    classifiedDF [counter,"Speeds"] <- newDataFrame [i,"Speeds"]
    classifiedDF [counter,"Class"] <- 'True'
    classifiedDF [counter,"Counts"] <- rightCount 
    counter <- counter +1
    classifiedDF [counter,"Speeds"] <- newDataFrame [i,"Speeds"]
    classifiedDF [counter,"Class"] <- "False"
    classifiedDF [counter,"Counts"] <- leftCount
    
    counter <- counter +1
  }
  
  return(classifiedDF)
}

# Here we build a confusion matrix based on Expected dataframe values
# Actual True Counts, and Actual False counts. 
# aTCount : Actual True Count from the training dataset
# aFCount : Actual False Count from the training dataset
# dfExpected: Expected results dataframe
# The return value will be a confusion matrix with speeds


buildConfusionMatrix <- function(aTCount, aFCount, dfExpected){
  confusionDF <- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(confusionDF) <- c("Speeds", "TP", "FP", "TN", "FN")
  actualTrueCount = aTCount
  actualFalseCount = aFCount
  totalCount = aTCount + aFCount
  expectedTrueCount <- 0
  expectedFalseCount <- 0
  counter <- 1
  rowNumber <- nrow(dfExpected)
  tp <- 0
  fp <- 0
  tn <- 0
  fn <- 0
  for(i in 1: rowNumber){
    if(dfExpected[i, "Class"] == "True"){
      expectedTrueCount = dfExpected[i, "Counts"]
      expectedFalseCount = totalCount - expectedTrueCount
      if(actualTrueCount > expectedTrueCount){
        tp <- expectedTrueCount
        fp <- 0
      }else{
        tp <- actualTrueCount
        fp <- expectedTrueCount - actualTrueCount
      }
      
      if(actualFalseCount >expectedFalseCount){
        tn <- expectedFalseCount
        fn <- 0
      }else{
        tn <- actualFalseCount
        fn <- expectedFalseCount - actualFalseCount
      }
      
      confusionDF[counter, "Speeds"] <- dfExpected[i, "Speeds"]
      confusionDF[counter, "TP"] <- tp
      confusionDF[counter, "FP"] <- fp
      confusionDF[counter, "TN"] <- tn
      confusionDF[counter, "FN"] <- fn
      counter <- counter + 1
    }
    
    
    
  }
  print(confusionDF)
  return (confusionDF);
  
}

# Here we calculate the missclassification rate based on confusion matrix
# The missclassification rate is [FP + FN]/[TP + TN + FP + FN]
# confusionMatrx: Input dataframe with 
# The output will be speed vs Missclassification rate
calculateMissClassificationRate <- function(confusionMatrx) {
  totalRows = nrow(confusionMatrx)
  dfMissClassification <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(dfMissClassification) <- c("Speeds", "MissClassRate")
  for (i in 1: totalRows){
    dfMissClassification[i,"Speeds"] = confusionMatrx[i, "Speeds"]
    dfMissClassification[i,"MissClassRate"] = (confusionMatrx[i, "FP"] + confusionMatrx[i, "FN"])/(confusionMatrx[i, "FP"] +confusionMatrx[i, "FN"] + confusionMatrx[i, "TP"] + confusionMatrx[i, "TN"])
  }
  #print(dfMissClassification)
  return (dfMissClassification)
}

# Here we build a Reciver Operator curve (ROC)
# This requires Trus Postive Rate = TP/(FN + TP) and False Positive Rate = FP/(FP+TN)
# For each speed we measure those parameters and put them into a dataframe
# confusionMatrix: The confusionMatrix is the input for this function
# This will return datafrom with TPR and FPR
buildROCdf <- function(confusionMatrx){
  totalRows = nrow(confusionMatrx)
  dfROC <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(dfROC) <- c("Speeds", "TruePositiveRate", "FalsePositiveRate")
  for (i in 1: totalRows){
    dfROC[i,"Speeds"] = confusionMatrx[i, "Speeds"]
    dfROC[i,"TruePositiveRate"] = (confusionMatrx[i, "TP"])/(confusionMatrx[i, "FN"] + confusionMatrx[i, "TP"])
    dfROC[i,"FalsePositiveRate"] = (confusionMatrx[i, "FP"])/(confusionMatrx[i, "FP"] + confusionMatrx[i, "TN"])
  }
  return (dfROC)
}

# Plot the missclassification rate vs Speed
# missClassificationMatrix: Input matrix with missclassification rate and speed
# Here I also plot a point where the lowest miss classificaiton happens

plotMissClssification <- function(missClassificationMatrix){
  minMissClassRate = min(missClassificationMatrix$MissClassRate)
  rowNum = which(missClassificationMatrix$MissClassRate == minMissClassRate)
  
  p <- ggplot(data=missClassificationMatrix, aes(x=missClassificationMatrix$Speeds, y=missClassificationMatrix$MissClassRate, group=1)) + 
    geom_point() + geom_path(color="darkgreen") + geom_point(x=missClassificationMatrix[rowNum, "Speeds"],y=minMissClassRate,shape = 5, colour = "Red", fill = "white", size = 2, stroke = 1) + geom_text(aes(label=Speeds),hjust=2, vjust=1, size=2)
  
  p = p+xlab("Speed (mph)") + ylab("Miss Classification Rate") +
    ggtitle("Speed Vs Miss Classification Curve") +
    theme(plot.title = element_text(color = "black", size=14, face="bold", hjust=0.5 ))
  

 return (p)
}

# Plot the actual data we got from the dataset
# The input will be  dataframe with actual speeds and counts

plotActualData <- function(dfActual)
{
  
  p <- ggplot(data=dfActual, aes(x=Speeds, y=Counts, fill=Class)) + 
    geom_bar(stat="identity") + scale_fill_brewer(palette = "Set1") 
   return (p)
}


# Plot the ROC Curve
# Here we determined the distance from 1,0 to find the ideal point
# dfROC  = In put ROC matrix


plotROC <- function(dfROC){
  # Find the closest point to (0,1)
  # Basic math SQRT [(x1-x2)^2 + (y1-y2)^2]
  dfROCDist <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(dfROCDist) <- c("Speeds", "Distance")
  numRows = nrow(dfROC)
  for (i in 1: numRows){
    distance = sqrt((0- dfROC[i, "FalsePositiveRate"])^2 + (1-dfROC[i, "TruePositiveRate"])^2)
    dfROCDist[i,"Speeds"] = dfROC[i,"Speeds"]
    dfROCDist[i,"Distance"] = distance
    
  }
  
  minDistance = min(dfROCDist$Distance)
  rowNumMDMatrix = which(dfROCDist$Distance == minDistance)
  rowNumROCMatrix = which(dfROC$Speeds == dfROCDist[rowNumMDMatrix,"Speeds"])
  xROC = dfROC [rowNumROCMatrix, "FalsePositiveRate"]
  yROC = dfROC [rowNumROCMatrix, "TruePositiveRate"]
  print(c(" Minimum ROC Distance is : ", minDistance, " The Speed is for that distance is ", dfROCDist[rowNumMDMatrix,"Speeds"]))
  p <- ggplot(data=dfROC, aes(x=dfROC$FalsePositiveRate, y=dfROC$TruePositiveRate, group=1,col = Speeds)) + 
    geom_point() + geom_path(color="darkgreen") +
    geom_point(x=xROC,y=yROC,shape = 5, colour = "Red", fill = "white", size = 2, stroke = 1) + geom_text(aes(label=Speeds),hjust=2, vjust=1, size=2)
  p = p+xlab("False Positive Rate") + ylab("True Positive Rate") +
    ggtitle("ROC Curve by Threshold") +
    theme(plot.title = element_text(color = "black", size=14, face="bold", hjust=0.5 ))
  return (p)
}

# Here we call the functions defined above to the do the different operations
# Here also we defined the file path

main <- function(){
  library(ggplot2)
  library(grid)
  library(gridExtra)
  require(gridExtra)
  ###
  ##  IMPORTANT  ####
  ### PLEASE CHANGE THE FOLLOWING FILE PATH TO THE RIGHT LOCATION
  ####
  file_path <- "/Users/jeyvell/BigDataCertification/CSCI720/HW03/DATA_v2175_FOR_CLASSIFICATION_using_Threshold.csv"
  newDataFrame <- createBins(file_path)

  actualTrueCounts <- sum(newDataFrame$RightCounts)
  actualFalseCounts <- sum(newDataFrame$WrongCounts)
  dfActual <- buildActualClassifiedDF(newDataFrame)
  
  expectedDF <- buildExpectedClassifiedDF(newDataFrame)
  dfConfusion <- buildConfusionMatrix (actualTrueCounts, actualFalseCounts, expectedDF)
  
  missClassificationDF <- calculateMissClassificationRate(dfConfusion)
  rowIn <- which(missClassificationDF$MissClassRate ==min(missClassificationDF$MissClassRate))
  print(c( "SPEED and MISSCLASSIFICATION RATE: ", missClassificationDF))

  dfROC <- buildROCdf(dfConfusion)

  p1 <- plotActualData(dfActual)
  p2 <- plotMissClssification(missClassificationDF)
  p3 <- plotROC(dfROC)
  
  grid.arrange(p1, p2, p3, ncol = 3 )
  #plot(p1)
}

# Executing the main function
call <- main()
