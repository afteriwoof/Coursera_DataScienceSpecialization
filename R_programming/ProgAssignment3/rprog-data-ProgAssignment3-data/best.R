best <- function(state, outcome){

        ## Read outcome data

	data<-read.csv("outcome-of-care-measures.csv",colClasses="character")

	## Check that state and outcome are valid
	
	if (!(state %in% data[[7]])){ 
		stop("Invalid state")
	}
	if (!(outcome %in% c("heart attack","heart failure","pneumonia"))){
		stop("Invalid outcome")
	}
		
	## Return hospital name in that state with lowest 30-day death rate

	if (outcome=="heart attack"){
		ind <- which(as.numeric(data[data[[7]]==state,11])==min(as.numeric(data[data[[7]]==state,11]),na.rm=TRUE))
	}
	
	if (outcome=="heart failure"){
                ind <- which(as.numeric(data[data[[7]]==state,17])==min(as.numeric(data[data[[7]]==state,17]),na.rm=TRUE))
        }

	if (outcome=="pneumonia"){
                ind <- which(as.numeric(data[data[[7]]==state,23])==min(as.numeric(data[data[[7]]==state,23]),na.rm=TRUE))
        }

	hosps <- data[data[[7]]==state,2]
	best <- hosps[ind]

	best
}
