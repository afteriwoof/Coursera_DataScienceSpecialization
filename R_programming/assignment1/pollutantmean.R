pollutantmean <- function(directory, pollutant, id=1:332){

	## 'directory' is a character vector of length 1 indicating
	## the location of the CSV files

	## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".

	## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used

        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)


	if(pollutant == "sulfate"){ind<-2} else{ind<-3}
	
	comb_data <- c(0)

	for (i in 1:length(id)){
		filename <- paste(directory,"/",sprintf("%03d",id[i]),".csv",sep="")
		data <- read.csv(filename, header=TRUE)

		vals <- !is.na(data[,ind])

		comb_data <- c(comb_data,data[vals,ind])

	}
	
	mean_pol <- mean(comb_data[2:length(comb_data)])

	print(mean_pol)
}
