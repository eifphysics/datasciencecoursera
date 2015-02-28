## This function ranks hospital in a state 
## based on outcome, and returns the hospital
## name with the specified rank.

rankhospital <- function(state, outcome, num = "best"){
      ## read outcome data
      outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
      
      ## check that state and outcome are valid
      if (!(state %in% state.abb))
            stop("invalid state")
      if (!(outcome %in% c("heart attack","heart failure","pneumonia")))
            stop("invalid outcome")
      
      ## rank the hospitals based on input
      if( outcome == "heart attack" ) 
            outcomeCol <- 11
      else if( outcome == "heart failure" ) 
            outcomeCol <- 17
      else if( outcome == "pneumonia" ) 
            outcomeCol <- 23
      
      outcomeData <- subset(outcomeData, State == state)
      
      outcomeData[ , outcomeCol] <- as.numeric(outcomeData[, outcomeCol])
      if(num == "worst" )
            outcomeSort <- outcomeData[order(outcomeData[outcomeCol],outcomeData[2], na.last=FALSE) , ]
      else 
            outcomeSort <- outcomeData[order(outcomeData[outcomeCol],outcomeData[2]) , ]
      
      ## return hospital name in that state
      ## with the given rank 30-day death rate
      maxNum <- nrow(outcomeSort)
      
      if (num == "best")
            outcomeData[1, 2]
      else if (num == "worst")
            outcomeSort[maxNum, 2]
      else
            outcomeSort[num, 2]
}
