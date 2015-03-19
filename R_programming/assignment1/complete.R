complete <- function(directory, id=1:332){

	## 'directory' is a character vector of length 1 indicating
	## the location of the CSV files

	## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used

	## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases


	id_i <- c(0)
	nobs_i <- c(0)

	for (i in 1:length(id)){
		filename <- paste(directory,"/",sprintf("%03d",id[i]),".csv",sep="")
		data <- read.csv(filename, header=TRUE)

		vals <- !is.na(data[,2]) & !is.na(data[,3])

		id_i <- c(id_i,id[i])
		nobs_i <- c(nobs_i,length(vals[vals==TRUE]))
	

	}

	complete <- data.frame(id=id_i[2:length(id_i)],nobs=nobs_i[2:length(nobs_i)])
	
}
