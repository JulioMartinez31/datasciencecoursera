

library(dplyr)
library(tidyr)

household_pc <- read.csv(".../household_power_consumption.txt",sep=';')

# filtering data
df_01 <- household_pc%>%
  mutate(Date = as.Date(Date,"%d/%m/%Y"),
         DateTime = paste(Date,Time,sep=" "),
         DateTime2 = as.POSIXct(DateTime, "%Y-%m-%d %H:%M:%S", tz=Sys.timezone())) %>%
  filter(Date >= '2007-02-01' & Date <= '2007-02-02') %>%
  filter(!is.na(Global_active_power)) %>%
  mutate(Global_active_power = as.numeric(Global_active_power))


############### plot3

png(filename=".../plot3.png",width = 480, height = 480)

plot(df_01$DateTime2,df_01$Sub_metering_1,type="l",col="black",xlab="",ylab="Energy sub metering")
lines(df_01$DateTime2,df_01$Sub_metering_2,type="l",col="red")
lines(df_01$DateTime2,df_01$Sub_metering_3,type="l",col="blue")
legend("topright",legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),col=c("black","red","blue"),lty=1)

dev.off()

