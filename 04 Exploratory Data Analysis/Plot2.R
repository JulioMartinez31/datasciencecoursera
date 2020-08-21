

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

################ plot2
png(filename=".../plot2.png",width = 480, height = 480)

plot(df_01$DateTime2 ,df_01$Global_active_power, type = "l", ylab="Global Active Power (kilowatts)",xlab="")

dev.off()

