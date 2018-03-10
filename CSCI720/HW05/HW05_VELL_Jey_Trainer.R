
# Global variable of Decision Tree, this tree keep track of all the splits we are making with left Node
# Right Node and other important variables 

dfDecisionTree <<- data.frame(matrix(ncol = 1, nrow = 0))

# Load the file and do a small process for empty columns and commas
# take the file path as input

loadAndProcess <- function(pathOfThefile){
  # This file doesn't have any header
  df <- read.csv(pathOfThefile, header = TRUE)
  df <- df[,colSums(is.na(df)) == 0]
  totNumberOfRows <- ncol(df)
  df$Totals <- rowSums(df[,c(2:totNumberOfRows)]) 
  return (df)
}

# When processing the Decision Tree list the following function help to find the next unused 
# index. This refers to the dfDecisionTree global variable

findNextUnusedRowIndex <- function(){
  rowCount <- length(dfDecisionTree)
  #print(c(" Decision Tree $$$%%^^^", rowCount))
  for (i in 1: rowCount){
    if(dfDecisionTree[[i]][["Used"]] == "F")
      return(i)
  }
  return(0)
}

# this accuracy is for each branch, Here I calculate the accuracy for each branch and use this value later
# as a stopping condtion 
# This funciton takes 
# df - Input datafram with all the nodes
# type - what type exactly we are looking in this dataframe

getAccuracy <- function(df, type){
  df <- as.data.frame(df)
  #print(c("Accuracy Type : ", type))
  predicatedCount <- nrow(df[tolower(substr(df[,1],1,1)) == substr(type,1,1),])
  totalCount <- nrow(df)
  accuracy <- (predicatedCount/totalCount) *100
  return(accuracy)
}

# This is a recursive function: It calls itself for further spliting 
# The following function do major funciton for tree induction
# it basically find the best attribute for split using weighted Gini Index
# Here we look for all the attributes and find the attribute with lowest Gini Index
# for split. This funciton also refers to global variable of dfDecisionTree 


findTheBestAttributeForParentNode <- function()
  {
  
  nextIndex <- findNextUnusedRowIndex()
  if(as.numeric(nextIndex) > 0){
    df <- as.data.frame(dfDecisionTree[[nextIndex]][["NodeDF"]])
  }
  noOfCols <- ncol(df) - 1
  outputdf <<- data.frame(matrix(ncol=6,nrow=0))
  
  colMedians <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(colMedians) <-  c("ColIndex", "Median")
  counter <- 1
  previousWeightedGini <- 1
  for(i in 2: noOfCols){
    medianVal <- median(df[,i])
    colMedians[counter,"ColIndex"] <- i
    colMedians[counter, "Median"] <- medianVal
    # Here we define how to split the left node using "<"
    leftNode <- df[df[,i] < medianVal,]
    #Here we define how to split the right node using ">="
    rightNode <- df[df[,i] >= medianVal,]
    
    totalLCount <- nrow(leftNode)
    # Here we do some string processing to avoid any spelling mistake for cupcake 
    # basically we make the type variable to lower case and take the first character
    # and compare that against "c" for cupcake 
    
    lCupCakeCount <- nrow(leftNode[tolower(substr(leftNode[,1],1,1)) == "c",])
    totalRCount <- nrow(rightNode)
    rCupCakeCount <- nrow(rightNode[tolower(substr(rightNode[,1],1,1)) == "c",])
    if(lCupCakeCount > (totalLCount-lCupCakeCount)){
        lNodeName <- "cupcake"
        rNodeName <- "muffin"
    }else if(rCupCakeCount > (totalRCount-rCupCakeCount)) {
        lNodeName <- "muffin"
        rNodeName <- "cupcake"
    } else{
      lNodeName <- "cupckage"
      rNodeName <- "muffin"      
    }
    lGiniIndex <- 1 - (lCupCakeCount/totalLCount)^2 - ((totalLCount-lCupCakeCount)/totalLCount)^2
    rGiniIndex <- 1 - (rCupCakeCount/totalRCount)^2 - ((totalRCount-rCupCakeCount)/totalRCount)^2
    weightGiniIndex <- (totalLCount/(totalLCount + totalRCount))*lGiniIndex + 
                       (totalRCount/(totalLCount + totalRCount))*rGiniIndex
   
    if(totalLCount > 0 && totalRCount > 0) {
      if(previousWeightedGini > weightGiniIndex){
        outputdf<<- c("colIndex"=i, "leftNodeName"=lNodeName, "rightNodeName"=rNodeName, "leftNode"=list(leftNode), "rightNode"=list(rightNode), "medVal"=medianVal)
        previousWeightedGini <- weightGiniIndex
      }
    }
    counter <- counter + 1
  }
  
  # Fill the information for the present node
  dfDecisionTree[[nextIndex]][["Used"]] <<- "T"
  dfDecisionTree[[nextIndex]][["Median"]] <<- outputdf[["medVal"]]
  dfDecisionTree[[nextIndex]][["sCond"]] <<- outputdf[["colIndex"]]   
  leftNodeAccuracy <- getAccuracy(outputdf[["leftNode"]], outputdf[["leftNodeName"]])
  rightNodeAccuracy <- getAccuracy(outputdf[["rightNode"]], outputdf[["rightNodeName"]])
  
  
  noOfIfs <- length(dfDecisionTree)
  #Define Left Node 
  if(noOfIfs <8){
    if(leftNodeAccuracy >= 98){
      leftNode <- c(NodeType ="F", Class=outputdf[["leftNodeName"]], Used="T", Median=-1,sCond=-1 , NodeDir="Left",NodeDF=list(outputdf[["leftNode"]]))
    }else{
      leftNode <- c(NodeType ="B", Class=outputdf[["leftNodeName"]], Used="F", Median=-1,sCond=-1 , NodeDir="Left",NodeDF=list(outputdf[["leftNode"]]))
    }
    dfDecisionTree[[length(dfDecisionTree) + 1]]<<- leftNode
  }else{
    return(0)
  }

 #Define the Right Node
 if(noOfIfs <8){
    if(rightNodeAccuracy >= 98){
      rightNode <- c(NodeType ="F", Class=outputdf[["rightNodeName"]], Used="T", Median=-1,sCond=-1 , NodeDir="Right",NodeDF=list(outputdf[["rightNode"]]))
    }else{
     rightNode <- c(NodeType ="B", Class=outputdf[["rightNodeName"]], Used="F", Median=-1,sCond=-1 , NodeDir="Right",NodeDF=list(outputdf[["rightNode"]]))
    }
    dfDecisionTree[[length(dfDecisionTree) + 1]]<<- rightNode
 }else{
    return (0)
  }
  #Calling itself recursively 
  findTheBestAttributeForParentNode()
  return (0)
  
}


# This is the step we spent a lot of time. Here we remove the duplicated data..
# we also applied some statistics principles to remove the outliers. Here we used
# outer fense to identify the extreme outliers. Also we applied some domain knowledge 
# in the data to find the accuracy. For an example, if we find upperfense as 0 then we 
# defaulted that value to 5 to capture more training data. 
# This function take the data frame as input and return the cleansed data frame


dataCleaning <- function(df){
  noOfCols <- ncol(df)
  outlierRecords  <- c()
  index <- which(duplicated(df))
  df <- unique(df)
  for(i in 2: noOfCols){
    
    quantVals <- quantile(df[,i])
    iqrVal <- IQR(df[,i])
    q1 <- quantVals[2]
    q3 <- quantVals[4]

    upperInnerFence <- q1 - 3*iqrVal
    lowerInnerFence <- q1 - 1.5*iqrVal
    lowerOuterFence <- q3 + 1.5*iqrVal
    upperOuterFence <- q3 + 3*iqrVal
    if(upperOuterFence ==0){
      upperOuterFence = 5
    }
    belowLowerInnerFenceRowIndexes <- which(df[,i] < lowerInnerFence)
    belowUpperInnerFenceRowIndexes <- which(df[,i] < upperInnerFence)
    aboveLowerOuterFenceRowIndexes <- which(df[,i] > lowerOuterFence)
    aboveUpperOuterFenceRowIndexes <- which(df[,i] > upperOuterFence)
    outlierRecords <- c(outlierRecords, as.numeric(belowUpperInnerFenceRowIndexes), as.numeric(aboveUpperOuterFenceRowIndexes))
  }

  if(length(outlierRecords) >0){
    df <- df[-outlierRecords,]
  }
  

  #index <- which(duplicated(df))
  #deduped.data <- unique(df)
  
  return (df)
}

# This is the emiter header where we setup initial file load and other parameters

emitHeader <-function(){
  # install.packages("rstudioapi") # run this if it's your first time using it to install
  library(rstudioapi) # load it
  # the following line is for getting the path of your current open file
  current_path <- getActiveDocumentContext()$path 
  # The next line set the working directory to the relevant one:
  setwd(dirname(current_path ))
  
  #Creating an R source Code
  sink('HW_05_Vell_Jey_Classifie.R')
  
  cat("#Function to load the validaiton file \n")
  cat("loadFile <- function(pathOfThefile) { \n")
  cat("\tdf <- read.csv(pathOfThefile, header = TRUE)\n")
  cat("\tdf <- df[,colSums(is.na(df)) == 0]\n")
  cat("\ttotNumberOfRows <- ncol(df)\n")
  cat("\treturn(df)\n")
  cat("}\n")
  sink()
  
}


#Generate Code to Generate DecisionTree

emitClassifier <- function(){
  # install.packages("rstudioapi") # run this if it's your first time using it to install
  library(rstudioapi) # load it
  # the following line is for getting the path of your current open file
  current_path <- getActiveDocumentContext()$path 
  # The next line set the working directory to the relevant one:
  setwd(dirname(current_path ))
  
  sink('HW_05_Vell_Jey_Classifie.R', append=TRUE)
  cat("#Function for classification\n")
  cat("classifier <-function(inputVal, id){ \n")
  cat("\tif(inputVal[2] < 40.285){ \n")
  cat("\t\tprint(c(id, \": classification is \", \"cupcake\", \" -LN\")) \n")
  cat("\t}else if(inputVal[2] < 46.26){ \n")
  cat("\t\tif(inputVal[6]<8){ \n")
  cat("\t\t\tprint(c(id, \": classification is \", \"muffin\", \" - RN-LN-LN\")) \n")
  cat("\t\t}else { \n")
  cat("\t\t\tprint(c(id, \": classification is \", \"cupcake\", \" - RN-LN-RN\")) \n")
  cat("\t\t}\n")
  cat("\t}else{\n")
  cat("\t\tif (inputVal[6] < 5){ # RN-RN\n")
  cat("\t\t\tprint(c(id, \": classification is \", \"cupcake\", \" - RN-RN-LN\")) \n")
  cat("\t\t}else{ \n")
  cat("\t\t\tprint(c(id, \": classification is \", \"muffin\", \" - RN-RN-RN\")) \n")
  cat("\t\t} \n")
  cat("\t} \n")
  cat("}\n")
  sink()
  
  

  
}

# This where we call the main function and define the actual trianing file name

emitFooter <- function(){
  # install.packages("rstudioapi") # run this if it's your first time using it to install
  library(rstudioapi) # load it
  # the following line is for getting the path of your current open file
  current_path <- getActiveDocumentContext()$path 
  # The next line set the working directory to the relevant one:
  setwd(dirname(current_path ))
  
  sink('HW_05_Vell_Jey_Classifie.R', append=TRUE)
  cat("#Main Function \n")
  cat("main <- function() { \n")
  cat("############################################################################################# \n\n")
  cat("### VERY IMPORTANT : Please change the following file location to validate file location #### \n\n")
  cat("############################################################################################# \n")
  cat("\tfile_path <-\"/Users/jeyvell/BigDataCertification/CSCI720/HW04/Recipes_For_VALIDATION_2175_RELEASED_v201.csv\"\n")
  cat("\tdf <- loadFile(file_path) \n")
  cat("\ttotRows <- nrow(df)\n")
  cat("\tfor( i in 1:totRows) { \n")
  cat("\t\t classifier(df[i,], i)\n")
  cat("\t} \n")
  cat("}\n")
  # Stop writing to the file
  cat("call <- main() \n")
  sink()
}

# This is the final function which combines all other functions to generate code for 
# Decision Tree Classifier

emitDecisionTree <- function(){
  emitHeader()
  emitClassifier()
  emitFooter()
}

# This is the function where all other function are getting called also the trianing data
# file location defined

main <- function(){
  library(ggplot2)
  library(grid)
  library(gridExtra)
  require(gridExtra)


  #######################################################################
  
 # IMPORTANT: PLEASE CHANGE THE FOLLOWING FILE PATH TO THE RIGHT LOCATION
   
  ########################################################################
  file_path <- "/Users/jeyvell/BigDataCertification/CSCI720/HW04/Recipes_For_Release_2175_v201.csv"
  newDataFrame <- loadAndProcess(file_path)

  afterCleaningDF <- dataCleaning (newDataFrame)

  dfCupcake <- afterCleaningDF[tolower(substr(afterCleaningDF$Type,1,1)) == "c",]
  dfMuffin <- afterCleaningDF[tolower(substr(afterCleaningDF$Type,1,1 ))== "m",]
  rootNode <<- c(NodeType ="T", Class="NA", Used="F", Median=-1,sCond=-1 , NodeDir="Root",NodeDF=list(afterCleaningDF))

  dfDecisionTree<<- c(list(rootNode))
  findTheBestAttributeForParentNode()
  emitDecisionTree()

}



# Executing the main function
call <- main()
