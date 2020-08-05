library(dplyr)
library(tidyr)


################################################################################
##########                     pollutantmean function            ###############
################################################################################
pollutantmean <- function(file_name,column,id=1:332) {
  num <- sapply(id, function(i) ifelse(i<10, paste('00',i,sep=''),ifelse(i<100, paste('0',i,sep=''),  paste(i,sep=''))))
  data2 <- data.frame()
  for(i in 1:length(num)){
    dir <- paste('.../rprog_data_specdata/',file_name,'/',num[i],'.csv',sep="")
    if(i == 1){
      data <- read.csv(dir,sep=',')
    }else{
      data2 <- read.csv(dir,sep=',')
      data <- rbind(data2,data)  
    }
  }
  result <- data%>%
    filter(!is.na(sulfate))%>%
    filter(!is.na(nitrate))%>%
    mutate(sulfate=as.numeric(sulfate),
           nitrate=as.numeric(nitrate))%>%
    select(column)%>%
    colMeans()%>%
    as.vector()
  
  return(result)
} 


################################################################################
##########                    complete function                  ###############
################################################################################
complete <- function(file_name,id = 1:332) {
  num <- sapply(id, function(i) ifelse(i<10, paste('00',i,sep=''),ifelse(i<100, paste('0',i,sep=''),  paste(i,sep=''))))
  
  for(i in 1:length(num)){
    dir <- paste('.../rprog_data_specdata/',file_name,'/',num[i],'.csv',sep="")
    if(i <= 1){
      data <- read.csv(dir,sep=',')
    }else {
      data2 <- read.csv(dir,sep=',')
      data <- rbind(data,data2)  
    }
    
  }
  
  result <- data%>%
    filter(!is.na(sulfate))%>%
    filter(!is.na(nitrate))%>%
    mutate(sulfate=as.numeric(sulfate),
           nitrate=as.numeric(nitrate),
           id = as.character(ID))%>%
    select(-ID)%>%
    group_by(id)%>%
    summarise(nobs=n())%>%
    ungroup()%>%
    data.frame()%>%
    mutate(id = as.numeric(id))%>%
    arrange(id)
    
  return(result)
} 


###############################################################################
##########                    complete function                  ###############
################################################################################
corr <- function(file_name,threshold = 0) {
  num <- sapply(1:332, function(i) ifelse(i<10, paste('00',i,sep=''),ifelse(i<100, paste('0',i,sep=''),  paste(i,sep=''))))
  
  for(i in 1:length(num)){
    dir <- paste('.../rprog_data_specdata/',file_name,'/',num[i],'.csv',sep="")
    if(i <= 1){
      data <- read.csv(dir,sep=',')
    }else {
      data2 <- read.csv(dir,sep=',')
      data <- rbind(data,data2)  
    }
    
  }
  
  n_comp_obs <- data%>%
    filter(!is.na(sulfate))%>%
    filter(!is.na(nitrate))%>%
    mutate(sulfate=as.numeric(sulfate),
           nitrate=as.numeric(nitrate),
           id = ID)%>%
    select(-ID)%>%
    group_by(id)%>%
    summarise(nobs=n())%>%
    ungroup()%>%
    data.frame()%>%
    filter(nobs > threshold)%>%
    select(id)
    
  result <- data%>%
    filter(!is.na(sulfate))%>%
    filter(!is.na(nitrate))%>%
    mutate(sulfate=as.numeric(sulfate),
           nitrate=as.numeric(nitrate),
           id = as.character(ID))%>%
    select(-ID)%>%
    filter(id %in% n_comp_obs$id)%>%
    group_by(id)%>%
    summarise(cor = cor(sulfate,nitrate))%>%
    ungroup()%>%
    data.frame()%>%
    mutate(id = as.numeric(id))%>%
    arrange(id)
  
  
  return(result$cor)
} 


################################################################################
##################              quiz            #######################
################################################################################

## 1)
pollutantmean("specdata", "sulfate", 1:10)
## 2)
pollutantmean("specdata", "nitrate", 70:72)
## 3)
pollutantmean("specdata", "sulfate", 34)
## 4)
pollutantmean("specdata", "nitrate")
## 5)
cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)
## 6)
cc <- complete("specdata", 54)
print(cc$nobs)
## 7)
RNGversion("3.5.1")  
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])
## 8)
cr <- corr("specdata")                
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)
## 9)
cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)
## 10)
cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
