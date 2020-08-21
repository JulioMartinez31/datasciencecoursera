

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

####   Plot1
png(filename=".../plot1.png",width = 480, height = 480)

hist(df_01$Global_active_power,
               main="Global Active Power",
               xlab="Global Active Power (kilowatts)",
               col="red")  

dev.off()

