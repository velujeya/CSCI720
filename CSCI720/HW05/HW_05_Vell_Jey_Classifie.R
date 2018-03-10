#Function to load the validaiton file 
loadFile <- function(pathOfThefile) { 
	df <- read.csv(pathOfThefile, header = TRUE)
	df <- df[,colSums(is.na(df)) == 0]
	totNumberOfRows <- ncol(df)
	return(df)
}
#Function for classification
classifier <-function(inputVal, id){ 
	if(inputVal[2] < 40.285){ 
		print(c(id, ": classification is ", "cupcake", " -LN")) 
	}else if(inputVal[2] < 46.26){ 
		if(inputVal[6]<8){ 
			print(c(id, ": classification is ", "muffin", " - RN-LN-LN")) 
		}else { 
			print(c(id, ": classification is ", "cupcake", " - RN-LN-RN")) 
		}
	}else{
		if (inputVal[6] < 5){ # RN-RN
			print(c(id, ": classification is ", "cupcake", " - RN-RN-LN")) 
		}else{ 
			print(c(id, ": classification is ", "muffin", " - RN-RN-RN")) 
		} 
	} 
}
#Main Function 
main <- function() { 
############################################################################################# 

### VERY IMPORTANT : Please change the following file location to validate file location #### 

############################################################################################# 
	file_path <-"/Users/jeyvell/BigDataCertification/CSCI720/HW04/Recipes_For_VALIDATION_2175_RELEASED_v201.csv"
	df <- loadFile(file_path) 
	totRows <- nrow(df)
	for( i in 1:totRows) { 
		 classifier(df[i,], i)
	} 
}
call <- main() 
