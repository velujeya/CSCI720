# install.packages("rstudioapi") # run this if it's your first time using it to install
library(rstudioapi) # load it
# the following line is for getting the path of your current open file
current_path <- getActiveDocumentContext()$path 
# The next line set the working directory to the relevant one:
setwd(dirname(current_path ))
# you can make sure you are in the right directory
print( getwd() )


sink("header.R")
cat("print (\"I am header \")\n")
sink()


sink('analysis-output.R')
cat("for (i in 1: 100) {")
cat("if(i > 50) {")
cat("print (c(\"I am bigger than 50 \", i))")
cat("}\n")
cat("print (\"T-test between x and y\")\n")
cat("}\n")
# Stop writing to the file
sink()
# Append to the file
sink('analysis-output.R', append=TRUE)
cat("print (\"Some more stuff here...\")\n")
sink()

sink("footer.R")
cat("print (\"I am footer \")\n")
sink()

x = 1
source("header.R")
if(x==1){
  source("analysis-output.R")
}else{
  source("program_B.R")
}
source("footer.R")