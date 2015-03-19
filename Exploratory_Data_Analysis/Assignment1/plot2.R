data<-read.table("household_power_consumption.txt",sep=";",header=TRUE,colClasses=c("character","character",rep("numeric",7)),na="?")

# data<-subset(data,data[[1]] %in% c("1/2/2007","2/2/2007"),drop=TRUE)

data$Time<-strptime(paste(data$Date,data$Time),"%d/%m/%Y %H:%M:%S")
data$Date<-as.Date(data$Date,"%d/%m/%Y")

subdates<-as.Date(c("1/2/2007","2/2/2007"),"%d/%m/%Y")
subdata<-subset(data,Date %in% subdates)

# plot2.R
png("plot2.png", width=480, height=480, units="px", bg="white")
par(mar=c(4,4,2,1))
plot(subdata$Time,subdata$Global_active_power,xlab="",ylab="Global Active Power (kilowatts)",type="l")
dev.off()



