#test - retest
#see how consistent participants were with their rating of stimuli
#Import Libraries
library(reshape2)
library(ggplot2)
library(plyr)

#read all files from directory
subj <- dir(path = "/Volumes/mainland/Projects/TMA blocker/Split Nostril Study/Setup2-Olfactometer/Code/SplitNostrilSetup_07_2016/DataPilot2/", pattern="\\.txt$", full.names=TRUE)# creating a list of all file names
names(subj)  <- basename(subj)
df  <- ldply(subj, read.delim, stringsAsFactors=FALSE)
#check number of participants
numParticipants <- length(unique(df$Subject))
#subset to get the test data
testData <- subset(df, TestMode == "No" & Procedure.SubTrial. == "ExperimentProc" & Subject !=1 & Subject != 2 & Subject !=57, select = c ("Subject", "Running.Block.", "Trial", "IntensityRatingAntag", "IntensityRatingFishy", "RightType", "LeftType"))
#convert scales (these scales are on a different scale than the previous scales)
#580 is zero on the scale and 80 is the highest
testData$FishyRating  <- 580 - as.integer(testData$IntensityRatingFishy)
testData$AntagRating  <- 580 - as.integer(testData$IntensityRatingAntag)
#make sure the conversion worked - there should be no items coming up in this test
checkConversion <- testData[which(testData$FishyRating < 0 | testData$AntagRating <0),]
#tidy up data
testData.sub <- subset(testData, select = c("Subject", "Running.Block.", "Trial","RightType", "LeftType", "FishyRating", "AntagRating"))
testData.melt <- melt(testData.sub, c("Subject", "Running.Block.", "Trial","RightType", "LeftType"), factorsAsStrings = FALSE)
testData.melt$value <- as.double(testData.melt$value)
testData.melt <- rename(testData.melt, c("variable" = "IntensityRating")) #rename variable
#name the variables the other way
nostriltyperelabel <- read.csv("/Volumes/mainland/Projects/TMA\ blocker/Split\ Nostril\ Study/Setup2-Olfactometer/Raw\ Data\ 3-2016/NostrilTypeRelabel.csv") 
testDataRename <- merge (testData.melt, nostriltyperelabel)


# #try plotting subject 28
# subj28 <- subset(testDataRename, Subject == 28)
# ggplot(subj28, aes(x = OdorType, y = value )) +
#   geom_point() +
#   facet_grid(Running.Block. ~ IntensityRating)
# #I want to correlate the first answers versus the second answers
# subj28.sub <- subset(subj28, Running.Block. == "Test1" & OdorType == "TMAAlone" & IntensityRating == "FishyRating")
#to get test-retest we are going to have to split the database into trials 1-9 and 10-18 for the first and second rating of the same stimulus. 
testDataRename$Trial <- as.numeric(testDataRename$Trial)
testDataRename1 <- subset(testDataRename, Trial < 10)
testDataRename2 <- subset(testDataRename, Trial >  9)
testDataRename1 <- testDataRename1[,-5]
testDataRename2 <- testDataRename2[,-5]
names(testDataRename1) <- c("RightType", "LeftType", "Subject", "Test", "IntensityRating", "trial1", "OdorType", "RightNostril", "LeftNostril")
names(testDataRename2) <- c("RightType", "LeftType", "Subject", "Test", "IntensityRating", "trial2", "OdorType", "RightNostril", "LeftNostril")
testData.trials <- merge(testDataRename1, testDataRename2)

ggplot(testData.trials, aes(x = trial1, y = trial2, colour = factor(Subject))) +
  geom_point() +
  facet_grid(IntensityRating ~Test)

#find the correlation by person and test and which scale they are rating on.
testData.corr <- ddply(testData.trials, .variables = c("Subject", "IntensityRating", "Test"), function(x) trial.cor = cor(x$trial1, x$trial2))

#look at graphs to see if the correlation data makes sense
ggplot(subset(testData.trials, Subject %in% c(28,29,30) & Test == "Test1"), aes(x = trial1, y = trial2, colour = factor(Subject))) +
  geom_point() +
  facet_grid(IntensityRating ~.)
#this graph suggests the correlation calculation is working becuase the datapoints seem to look like what the numbers are telling us. 

#so I probably want to make a graph that shows the absolute value of correlation for each person for each test
testData.corr$trial.corr <- abs(testData.corr$V1)
ggplot(testData.corr, aes(x = Test, y = trial.corr, colour = IntensityRating))+ 
  geom_point() + 
  facet_wrap(~Subject) +
  geom_hline(yintercept= .5)


#does the pattern look good for the few people who can actually do the task?
goodPeople <- subset(testDataBlock1.melt, Subject %in% c(29, 31, 37, 42, 50, 56))
ggplot(data = subset(goodPeople, IntensityRating %in% "FishyRating"), aes(x = variable, y = value))+
geom_point(position = position_jitter(w = .1)) +
  facet_grid(Subject~.)
#nope, not really except for a few of them. 
#I should go back and look at this for my previous studies.