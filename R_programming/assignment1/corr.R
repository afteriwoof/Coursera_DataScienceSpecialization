corr <- function(directory, threshold=0){

	## 'directory' is a character vector of length 1 indicating
	## the location of the CSV files

	## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations


	## Compute the list of complete cases
	df <- complete("specdata")
	ind <- df$nobs > threshold

	if (length(ind[ind==TRUE])==0){
		return(numeric(0))
	}

	id <- df$id[ind]

	comb_sulfate <- c(0)
	comb_nitrate <- c(0)
	corl <- c(0)

	for (i in 1:length(id)){
		filename <- paste(directory,"/",sprintf("%03d",id[i]),".csv",sep="")
		data <- read.csv(filename, header=TRUE)

		vals <- !is.na(data[,2]) & !is.na(data[,3])
		
		comb_sulfate <- c(comb_sulfate, data$sulfate[vals])
		comb_nitrate <- c(comb_nitrate, data$nitrate[vals])
		corl <- c(corl, cor(data$sulfate[vals], data$nitrate[vals]))
	}

	corr <- corl[2:length(corl)]
}
