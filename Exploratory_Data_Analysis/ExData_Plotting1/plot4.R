data<-read.table("household_power_consumption.txt",sep=";",header=TRUE,colClasses=c("character","character",rep("numeric",7)),na="?")

# data<-subset(data,data[[1]] %in% c("1/2/2007","2/2/2007"),drop=TRUE)

data$Time<-strptime(paste(data$Date,data$Time),"%d/%m/%Y %H:%M:%S")
data$Date<-as.Date(data$Date,"%d/%m/%Y")

subdates<-as.Date(c("1/2/2007","2/2/2007"),"%d/%m/%Y")
subdata<-subset(data,Date %in% subdates)

# plot4.R
png("plot4.png",width=480,height=480,units="px",bg="white")
par(mfrow=c(2,2))
plot(subdata$Time,subdata$Global_active_power,xlab="",ylab="Global Active Power",type="l")
plot(subdata$Time,subdata$Voltage,xlab="datetime",ylab="Voltage",type="l")
plot(subdata$Time,subdata$Sub_metering_1,type="l",xlab="",ylab="Enery sub metering")
lines(subdata$Time,subdata$Sub_metering_2,col="red")
lines(subdata$Time,subdata$Sub_metering_3,col="blue")
legend("topright",col=c("black","red","blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),lty=1,bty="n")
plot(subdata$Time,subdata$Global_reactive_power,xlab="datetime",ylab="Global_reactive_power",type="l")
dev.off()


