
file_path <- "/Users/jeyvell/BigDataCertification/CSCI720/HW02/DATA_v2175_FOR_CLUSTERING_using_Otsu.csv"

SpeedProcessing <- function(file_path){
  df <- read.csv(file_path)
  a <- table(df)
  dfSpeedFreq <- as.data.frame(a)
  names(dfSpeedFreq)[1] <- "Speed"
  names(dfSpeedFreq)[2] <- "Freq"
  
  dfSpeedFreq
  return (a)
  
}

b <- SpeedProcessing(file_path)
bplot <- barplot(b)




