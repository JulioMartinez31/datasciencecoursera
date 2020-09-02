library(data.table)

# reading features
cnames <- read.csv(".../UCI HAR Dataset/features.txt",header = FALSE,sep="")

# reading trainx and trainy
trainx <- read.csv(".../UCI HAR Dataset/train/X_train.txt",header = FALSE,sep="")
trainy <- read.csv(".../UCI HAR Dataset/train/Y_train.txt",header = FALSE,sep="")
trainsub <- read.csv(".../UCI HAR Dataset/train/subject_train.txt",header = FALSE,sep="")

# reading testx and testy
testx <- read.csv(".../UCI HAR Dataset/test/X_test.txt",header = FALSE,sep="")
testy <- read.csv(".../UCI HAR Dataset/test/Y_test.txt",header = FALSE,sep="")
testsub <- read.csv(".../UCI HAR Dataset/test/subject_test.txt",header = FALSE,sep="")

# merge two datasets x and y
x <- rbind(trainx,testx)  
y <- rbind(trainy,testy)
sub <- rbind(trainsub,testsub)

# colnames: x dataset and y dataset
colnames(x) <- cnames$V2
colnames(y) <- 'y'
colnames(sub) <- 'Subject'

# pattern mean and std  
col <- grep("mean\\()|std()",cnames$V2,value=TRUE)

# columns selected from patters
x_selected <- x[,col]

# merge x and y 
df_full <- cbind(x_selected,y,sub)

# create a tidy data with avg for each variable and each activity 
df <- df_full %>%
  gather(key='variable',value='value',-y,-Subject) %>%
  mutate(Activity = ifelse(y == 1,"Walking",
                           ifelse(y == 2,"Walking_Upstairs",
                                  ifelse(y == 3, "Walking_Downstairs",
                                         ifelse(y == 4, "Sitting",
                                                ifelse(y == 5,"Standing",
                                                       ifelse(y == 6, "Laying",""))))))) %>%
  group_by(Activity,Subject,variable) %>%
  summarise(avg = mean(value)) %>%
  ungroup() %>%
  data.frame() %>%
  arrange(desc(avg)) %>%
  spread(Activity,avg)

df
write.csv(df,".../03 DataCleaning/week4/df_tidy.csv",row.names = FALSE)

