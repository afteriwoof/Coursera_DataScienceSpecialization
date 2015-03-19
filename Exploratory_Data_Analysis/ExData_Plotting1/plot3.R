data<-read.table("household_power_consumption.txt",sep=";",header=TRUE,colClasses=c("character","character",rep("numeric",7)),na="?")

# data<-subset(data,data[[1]] %in% c("1/2/2007","2/2/2007"),drop=TRUE)

data$Time<-strptime(paste(data$Date,data$Time),"%d/%m/%Y %H:%M:%S")
data$Date<-as.Date(data$Date,"%d/%m/%Y")

subdates<-as.Date(c("1/2/2007","2/2/2007"),"%d/%m/%Y")
subdata<-subset(data,Date %in% subdates)

# plot3.R
png("plot3.png", width=480, height=480, units="px", bg="white")
plot(subdata$Time,subdata$Sub_metering_1,type="l",xlab="",ylab="Enery sub metering")
lines(subdata$Time,subdata$Sub_metering_2,col="red")
lines(subdata$Time,subdata$Sub_metering_3,col="blue")
legend("topright",col=c("black","red","blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),lty=1)
dev.off()



