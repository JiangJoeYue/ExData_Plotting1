#load data
data<-read.csv('household_power_consumption.txt',header=TRUE, sep=";", na.strings = "?")
head(data)

#preprocess data
data$Date <- as.Date(data$Date, "%d/%m/%Y")
data2<-data[which(data$Date>=as.Date("2007-02-01")&data$Date<=as.Date("2007-02-02")),]
DT <- paste(data2$Date, data2$Time)
DT <- setNames(DT,"DT")
data3 <- cbind(DT,data2)
data3$DT <- as.POSIXct(DT)

#plot1
hist(data3$Global_active_power, main="Global Active Power", xlab = "Global Active Power (kilowatts)", col="red")
png(filename = 'D:/dataset/plot1.png',width=480,height = 480)

#plot2
plot(data3$Global_active_power~data3$DT, type="l", xlab="",ylab="Global Active Power (kilowatts)")
png(filename = 'D:/dataset/plot2.png',width=480,height = 480)

#plot3
with(data3, {
  plot(Sub_metering_1~DT, type="l",xlab="",ylab="Global Active Power (kilowatts)")
  lines(Sub_metering_2~DT,col='Red')
  lines(Sub_metering_3~DT,col='Blue')})
legend("topright", lwd=c(1,1,1),col=c("black", "red", "blue"),c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

#plot4
par(mfrow=c(2,2), mar=c(5,4,2,1))
with(data3, {plot(Global_active_power~DT, type="l", xlab="",ylab="Global Active Power (kilowatts)")
  plot(Voltage~DT, type="l", xlab="",ylab="Voltage (volt)")
  plot(Sub_metering_1~DT, type="l", col="black",xlab="",ylab="Global Active Power (kilowatts)")
  lines(Sub_metering_2~DT,col="Red")
  lines(Sub_metering_3~DT,col="Blue")
  legend("topright", col=c("black", "red", "blue"), lty=1,legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),cex=0.5)
  plot(Global_reactive_power~DT, type="l",xlab="",ylab="Global Rective Power (kilowatts)") })