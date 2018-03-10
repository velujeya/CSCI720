

classifier <-function(inputVal)
{
  if(inputVal[2] < 40.285){
    #LN  (Leaf)
    print(c(inputVal, " classification is ", "cupcake")) 
  }else if(inputVal[2] < 46.26){ 
    #  RN
    if(inputVal[6]<8){ 
      #RN - LN
      
      # RN - LN - LN
      print(c(inputVal, " classification is ", "cupcake")) 
      
    }else {
      # RN - LN - RN (Leaf)
      print(c(inputVal, " classification is ", "cupcake")) 
    }
  }else{
    if (inputVal[6] < 5){ # RN-RN
      #RN-RN- LN (Leaf)
      print(c(inputVal, " classification is ", "cupcake")) 
    }else{
      #RN-RN-RN (Leaf)
      print(c(inputVal, " classification is ", "cupcake")) 
    }
  }
  
}


check <- function(val, nodeID){
  

  nodeAttributeID <- dfDecisionTree[[nodeID]][["sCond"]]
  nodeThreshold <- dfDecisionTree[[nodeID]][["Median"]]
  nodeType <- dfDecisionTree[[nodeID]][["NodeType"]]
  if(val[nodeAttributeID] < nodeThreshold){
    if()
    check (val, nodeID +1 )
    
  }else{
    
  }
  
  
  
  
  
  if(nodeType == "F") {
    
  }else{
    leftNodeID <- nodeID*2
    rightNodeID <- nodeID*2 +1
    check(leftNodeID)
    check(rightNodeID)
  }
  
  
  
  if(leftNodeType == "F"){
    
  }
  leftNode <- c(NodeType ="F", Class=outputdf[["leftNodeName"]], Used="T", Median=-1,sCond=-1 , NodeDir="Left",NodeDF=list(outputdf[["leftNode"]]))
}