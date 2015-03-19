data<-read.table("household_power_consumption.txt",sep=";",header=TRUE,colClasses=c("character","character",rep("numeric",7)),na="?")

# data<-subset(data,data[[1]] %in% c("1/2/2007","2/2/2007"),drop=TRUE)

data$Time<-strptime(paste(data$Date,data$Time),"%d/%m/%Y %H:%M:%S")
data$Date<-as.Date(data$Date,"%d/%m/%Y")

subdates<-as.Date(c("1/2/2007","2/2/2007"),"%d/%m/%Y")
subdata<-subset(data,Date %in% subdates)

# plot1.R
png("plot1.png", width=480, height=480, units="px", bg="white")
hist(subdata$Global_active_power,xlab="Global Active Power (kilowatts)",col="red",main="Global Active Power")
#dev.copy(png,filename="plot1.png",width=480,height=480,units="px",bg="white")
dev.off()


