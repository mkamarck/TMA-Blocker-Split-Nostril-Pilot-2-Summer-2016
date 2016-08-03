#Setup3 analysis
#This is setup of the split nostril experiment was run by MK and SV during MSAP summer 2016. The machine was fully cleaned, lines were shortened, and the odors are being refreshed more often in order to reduce variability
#Analysis will follow here. 
#Subject 1 =SV (data can't be used)
#Subject 2 = YI (in lab - data can't be used)

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
#get the training data from the scale
scaleTrainingData <- subset(df, Procedure.Block. == "ScaleTrainingProc", select = c("Subject", "Procedure.Block.", "ScaleTraining", "Question.Trial.", "IntensityRatingTraining.Trial."))
#get the training data from the smell training (quiz and such)
smellTrainingQuiz <- subset(df, Procedure.Trial. == "training" | Procedure.Trial. == "Quiz", select = c("Subject", "Procedure.Block.", "IntensityRatingTraining.Trial.", "OdorType", "Procedure.Trial.", "Descriptor", "QuizPage.RESP"))


#convert scales (these scales are on a different scale than the previous scales)
#580 is zero on the scale and 80 is the highest
testData$FishyRating  <- 580 - as.integer(testData$IntensityRatingFishy)
testData$AntagRating  <- 580 - as.integer(testData$IntensityRatingAntag)
#make sure the conversion worked - there should be no items coming up in this test
checkConversion <- testData[which(testData$FishyRating < 0 | testData$AntagRating <0),]

#make new labels and averages
testData.sub <- subset(testData, select = c("Subject", "Running.Block.", "Trial","RightType", "LeftType", "FishyRating", "AntagRating"))
#write.csv(testData.sub, "/Volumes/storage/mainland/Projects/TMA\ blocker/Split\ Nostril\ Study/Setup2-Olfactometer/Code/SplitNostrilSetup_07_2016/SplitNostrilDataSummer2016.csv") #This outputs a file that Sianneh was using to analyze data for MSAP project
# testData.excel <- merge(testData.sub, nostriltyperelabel)
# #this doesn't work #testData.excel.fishy <- subset(testData.excel, select = c("Subject", "Running.Block.", "OdorType", "FishyRating"))
# testData.excel.antagcontrol <- subset(testData.excel, select = c("Subject", "Running.Block.", "OdorType", "FishyRating", "AntagRating"))
# write.csv(testData.excel.antagcontrol, "/Volumes/mainland/Projects/TMA\ blocker/Split\ Nostril\ Study/Setup2-Olfactometer/Code/SplitNostrilSetup_07_2016/SplitNostrilDataSummer2016_FinalDataset.csv")

testData.melt <- melt(testData.sub, c("Subject", "Running.Block.", "Trial","RightType", "LeftType"), factorsAsStrings = FALSE)
testData.melt$value <- as.double(testData.melt$value)
testData.melt <- rename(testData.melt, c("variable" = "IntensityRating")) #rename variable

testDataBlock1 <- ddply(.data = subset(testData.melt, Running.Block. == "Test1"), .variables = c("Subject", "IntensityRating", "Running.Block."), function(x) c(AntagAlone = mean(subset(x, (RightType == "Nonenol" & LeftType == "Blank") | (RightType == "Blank" & LeftType == "Nonenol"))$value), TMAAlone = mean(subset(x, (RightType == "TMA" & LeftType == "Blank") | (RightType == "Blank" & LeftType == "TMA"))$value), DifferentNostrils = mean(subset(x, (RightType == "TMA" & LeftType == "Nonenol") | (RightType == "Nonenol" & LeftType == "TMA"))$value), SameNostril= mean(subset(x, (RightType == "TMA+Nonenol" | LeftType == "TMA+Nonenol"))$value), Blank= (mean(subset(x, (RightType == "Blank" & LeftType == "Blank"))$value))))
testDataBlock2 <- ddply(.data = subset(testData.melt, Running.Block. == "Test2"), .variables = c("Subject", "IntensityRating", "Running.Block."), function(x) c(AntagAlone = mean(subset(x, (RightType == "Linalool" & LeftType == "Blank") | (RightType == "Blank" & LeftType == "Linalool"))$value), TMAAlone = mean(subset(x, (RightType == "TMA" & LeftType == "Blank") | (RightType == "Blank" & LeftType == "TMA"))$value), DifferentNostrils = mean(subset(x, (RightType == "TMA" & LeftType == "Linalool") | (RightType == "Linalool" & LeftType == "TMA"))$value), SameNostril= mean(subset(x, (RightType == "TMA+Linalool" | LeftType == "TMA+Linalool"))$value), Blank= (mean(subset(x, (RightType == "Blank" & LeftType == "Blank"))$value))))
testDataBlock1.melt <- melt(testDataBlock1, c("Subject", "IntensityRating", "Running.Block."))
testDataBlock2.melt <- melt(testDataBlock2, c("Subject", "IntensityRating", "Running.Block."))



#make some graphs - then later make some graphs with normalized data - we want to make sure everything looks good first
# ggplot(data = subset(testDataBlock1.melt, IntensityRating %in% "FishyRating"), aes(x = variable, y = value))+
#   geom_point(position = position_jitter(w = .1)) +
#   facet_grid(Subject~.)
# 
# ggplot(data = subset(testDataBlock2.melt, IntensityRating %in% "FishyRating"), aes(x = variable, y = value))+
#   geom_point(position = position_jitter(w = .1)) +
#   facet_grid(Subject~.)


#The other way to relabel so we can see all the points from one person
nostriltyperelabel <- read.csv("/Volumes/mainland/Projects/TMA\ blocker/Split\ Nostril\ Study/Setup2-Olfactometer/Raw\ Data\ 3-2016/NostrilTypeRelabel.csv") 
testDataRename <- merge (testData.melt, nostriltyperelabel)

#graph for individuals with all datapoints
ggplot(data = subset(testDataRename, IntensityRating == "FishyRating" & Running.Block. == "Test1"), aes(x = OdorType, y = value)) +
  geom_point(position = position_jitter(w = .1)) +
  facet_grid(.~Subject)

ggplot(data = subset(testDataRename, IntensityRating == "FishyRating" & Running.Block. == "Test1"), aes(x = OdorType, y = value)) +
  geom_boxplot() +
  facet_grid(.~Subject)
#just the second half of the participants
# ggplot(data = subset(testDataRename, IntensityRating == "FishyRating" & Running.Block. == "Test1" & Subject >=40), aes(x = OdorType, y = value)) +
#   geom_boxplot() +
#   facet_grid(.~Subject)


#normalizedData <- ddply(dfSubset, .variables = c("Subject", "Running.Block."), function(x) c(FishyRatingNorm = scale01(x$FishyRating), AntagRatingNorm = scale01(x$AntagRating)))
#try normalization with and without by "Running.Block."
#we could also normalize it just to the fishy scale and by block or not by block - this would show us the biggest difference in fishy values. 
#####
#DECIDE HOW WE WANT TO NORMALIZE THE DATA####
#I'm scaling against other odors, we can also try normalizing to the initial scale training
#scale01 <- function(x){(x-min(x))/(max(x)-min(x))}
scale01 <- function(value, minValue, maxValue) {
  (value - minValue)/(maxValue - minValue)
}

#plan: Find min and max for each participant and then calculate the value using the scale function. 
#this is normalizing by scale usage during the experiment, rather than overall scale usage during the trial
#later I can try normalizing by the scale usage from the beginning of the data
#work with testDataRename
subjMinMax <- ddply(testDataRename, .variables = c("Subject"), .fun = summarize, minValue = min(value), maxValue = max(value))
testDataMinMax <- merge(testDataRename, subjMinMax)
testDataMinMax$normValue <- scale01(testDataMinMax$value, testDataMinMax$minValue, testDataMinMax$maxValue)


testDataMinMax$Running.Block. <- factor(testDataMinMax$Running.Block., levels = c("Test1", "Test2"), labels = c("Nonenol", "Linalool"))
testDataMinMax$OdorType <- factor(testDataMinMax$OdorType, levels = c("TMAAlone", "DifferentNostrils", "SameNostril", "Blank", "AntagAlone"))

# #graph normalized individuals
# ggplot(data = subset(testDataMinMax, IntensityRating == "FishyRating" & Running.Block. == "Nonenol"), aes(x = OdorType, y = normValue)) +
#   geom_point(position = position_jitter(w = .1)) +
#   facet_grid(Subject~.)
# #graph normalized subjects together
# ggplot(data = subset(testDataMinMax, IntensityRating == "FishyRating"), aes(x = OdorType, y = normValue)) +
#   geom_point(position = position_jitter(w = .1))+
#   facet_grid(.~Running.Block.)
# #together violin plot
# ggplot(data = subset(testDataMinMax, IntensityRating == "FishyRating"), aes(x = OdorType, y = normValue)) +
#   geom_violin() +
#   facet_grid(.~Running.Block.)
#everything together
ggplot(testDataMinMax, aes(x = OdorType, y = normValue)) +
  geom_violin() +
  facet_grid(IntensityRating ~ Running.Block.) +
  xlab("Normalized Intensity Rating") +
  ylab("Nostril Type")

ggplot(testDataMinMax, aes(x = OdorType, y = normValue)) +
  geom_boxplot() +
  facet_grid(IntensityRating ~ Running.Block.) +
  xlab("Normalized Intensity Rating") +
  ylab("Nostril Type")
#graph to match Sianneh's 
#Find average for each type
testDataAverages <- ddply(testDataMinMax, .variables = c("Running.Block.", "OdorType", "IntensityRating"), .fun = summarize, average = mean(normValue), stdev = sd(normValue), stderror= stdev/sqrt(length(normValue)))


ggplot(testDataAverages, aes(x = OdorType, y = average, fill = OdorType)) +
  geom_bar(stat = "identity") +
  facet_grid(IntensityRating ~ Running.Block.) +
  xlab("Nostril Type") +
  ylab("Normalized Intensity Rating") +
  geom_errorbar(aes(ymin = average-stderror, ymax = average+stderror), position = "dodge")


###########################################
#why is TMA Alone stronger on average with the nonenol than with the linalool
#it isn't really now that we've run a whole bunch more people
testDataBlock1 <- ddply(.data = subset(testData.melt, Running.Block. == "Test1"), .variables = c("Subject", "IntensityRating", "Running.Block."), function(x) c(AntagAlone = mean(subset(x, (RightType == "Nonenol" & LeftType == "Blank") | (RightType == "Blank" & LeftType == "Nonenol"))$value), TMAAlone = mean(subset(x, (RightType == "TMA" & LeftType == "Blank") | (RightType == "Blank" & LeftType == "TMA"))$value), DifferentNostrils = mean(subset(x, (RightType == "TMA" & LeftType == "Nonenol") | (RightType == "Nonenol" & LeftType == "TMA"))$value), SameNostril= mean(subset(x, (RightType == "TMA+Nonenol" | LeftType == "TMA+Nonenol"))$value), Blank= (mean(subset(x, (RightType == "Blank" & LeftType == "Blank"))$value))))
testDataBlock2 <- ddply(.data = subset(testData.melt, Running.Block. == "Test2"), .variables = c("Subject", "IntensityRating", "Running.Block."), function(x) c(AntagAlone = mean(subset(x, (RightType == "Linalool" & LeftType == "Blank") | (RightType == "Blank" & LeftType == "Linalool"))$value), TMAAlone = mean(subset(x, (RightType == "TMA" & LeftType == "Blank") | (RightType == "Blank" & LeftType == "TMA"))$value), DifferentNostrils = mean(subset(x, (RightType == "TMA" & LeftType == "Linalool") | (RightType == "Linalool" & LeftType == "TMA"))$value), SameNostril= mean(subset(x, (RightType == "TMA+Linalool" | LeftType == "TMA+Linalool"))$value), Blank= (mean(subset(x, (RightType == "Blank" & LeftType == "Blank"))$value))))
testDataBlockmerge <- rbind(testDataBlock1, testDataBlock2)
testDataBlock.sub <- subset(testDataBlockmerge, IntensityRating == "FishyRating", select = c("Subject", "Running.Block.", "TMAAlone"))

testData.cast <- dcast(testDataBlock.sub, ...~Running.Block., value.var = "TMAAlone")
ggplot(testData.cast, aes(x = Test1, y = Test2)) +
  geom_point()+
  xlim(0,200) +
  ylim(0,200) +
  geom_abline(a=1, b=1)
#I should make sure that this wasn't an effect of when I changed the TMA odor and which block was run first 
#pick out the few participants that fall far from the line and see
testData.cast$difference <- abs(testData.cast$Test1 - testData.cast$Test2)
testData.cast$Subject[which(testData.cast$difference > 90)]
#37 and 52
#52 was one after refreshing - which block did they get first? they got block 1 first but rated TMA higher in 2 - my guess is that they were confusing the green and fishy odors
# ~check this to see if they need to be eliminated
#31 was one after refreshing - block 2 first, but rated TMA about the same in both blocks
#37 seems random... but which block did they get first? they got block 1 first and found that the TMA from that block is higher. 

###########################################
#I want to try different normalizations of this data to see whether we can fix the issue of the blanks being high
#There may also be a cutoff I need to make for whether or not people can tell apart the fishy and the green odors. 

#try normalizing to the blank
#for all fishy intensity data subtract the average blank fishy intensity and for all antagonist intensity subtract the average blank green or control intensity
#I think it makes sense to do this for each block rather than combining them to rule out the possibility of just confusing the two odors

BlankAvg <- ddply(testDataRename, .variables = c("Subject","Running.Block.", "IntensityRating"), function(x) c(BlankAvg = mean(subset(x, OdorType == "Blank")$value)))
testData_blankAvg <- merge(testDataRename, BlankAvg)
testData_blankAvg$norm_toBlank <- testData_blankAvg$value - testData_blankAvg$BlankAvg

#okay, now with the blank normalized data we need to normalize the data for scale usage
subjMinMax_normToBlank <- ddply(testData_blankAvg, .variables = c("Subject"), .fun = summarize, minValue = min(norm_toBlank), maxValue = max(norm_toBlank))
testDataMinMax_normToBlank <- merge(testData_blankAvg, subjMinMax_normToBlank)
testDataMinMax_normToBlank$normValue <- scale01(testDataMinMax_normToBlank$norm_toBlank, testDataMinMax_normToBlank$minValue, testDataMinMax_normToBlank$maxValue)

testDataMinMax_normToBlank$Running.Block. <- factor(testDataMinMax_normToBlank$Running.Block., levels = c("Test1", "Test2"), labels = c("Nonenol", "Linalool"))
testDataMinMax_normToBlank$OdorType <- factor(testDataMinMax_normToBlank$OdorType, levels = c("TMAAlone", "DifferentNostrils", "SameNostril", "Blank", "AntagAlone"))

#graph it!
ggplot(testDataMinMax_normToBlank, aes(x = OdorType, y = normValue)) +
  geom_boxplot() +
  facet_grid(IntensityRating ~ Running.Block.) +
  xlab("Normalized Intensity Rating") +
  ylab("Nostril Type")

#graph to match Sianneh's 
#Find average for each type
testDataAverages <- ddply(testDataMinMax, .variables = c("Running.Block.", "OdorType", "IntensityRating"), .fun = summarize, average = mean(normValue), stdev = sd(normValue), stderror= stdev/sqrt(length(normValue)))


ggplot(testDataAverages, aes(x = OdorType, y = average, fill = OdorType)) +
  geom_bar(stat = "identity") +
  facet_grid(IntensityRating ~ Running.Block.) +
  xlab("Nostril Type") +
  ylab("Normalized Intensity Rating") +
  geom_errorbar(aes(ymin = average-stderror, ymax = average+stderror), position = "dodge")


#this did nothing, all I did was shift the data and then renormalize it so it looks exactly the same - I am dumb. 
###########################################
#unity line plot

ggplot(data = subset(testDataBlock1, IntensityRating == "FishyRating"), aes(x = DifferentNostrils, y = SameNostril, colour = Subject)) +
  geom_point() +
  xlim(0,200) +
  ylim(0,200) +
  geom_abline(a=1, b=1)

ggplot(data = subset(testDataBlock2, IntensityRating == "FishyRating"), aes(x = DifferentNostrils, y = SameNostril, colour = Subject)) +
  geom_point() +
  xlim(0,200) +
  ylim(0,200) +
  geom_abline(a=1, b=1)
  

testDataBlock1.unityline <- ddply(.data = subset(testData.melt, Running.Block. == "Test1"), .variables = c("Subject", "IntensityRating", "Running.Block."), function(x) c(DifferentNostrils = mean(subset(x, (RightType == "TMA" & LeftType == "Nonenol") | (RightType == "Nonenol" & LeftType == "TMA"))$value), DifferentNostrilsSE = sd(subset(x, (RightType == "TMA" & LeftType == "Nonenol") | (RightType == "Nonenol" & LeftType == "TMA"))$value/2), SameNostril= mean(subset(x, (RightType == "TMA+Nonenol" | LeftType == "TMA+Nonenol"))$value), SameNostrilSE= mean(subset(x, (RightType == "TMA+Nonenol" | LeftType == "TMA+Nonenol"))$value/2)))
testDataBlock2.unityline <- ddply(.data = subset(testData.melt, Running.Block. == "Test2"), .variables = c("Subject", "IntensityRating", "Running.Block."), function(x) c(DifferentNostrils = mean(subset(x, (RightType == "TMA" & LeftType == "Linalool") | (RightType == "Linalool" & LeftType == "TMA"))$value), SameNostril= mean(subset(x, (RightType == "TMA+Linalool" | LeftType == "TMA+Linalool"))$value)))

ggplot(data = subset(testDataBlock1.unityline, IntensityRating == "FishyRating"), aes(x = DifferentNostrils, y = SameNostril, colour = Subject)) +
  geom_point() +
  xlim(0,200) +
  ylim(0,200) +
  geom_abline(a=1, b=1) +
  geom_errorbar(aes(ymin = SameNostril - SameNostrilSE, ymax = SameNostril + SameNostrilSE)) +
  geom_text(aes(label = Subject))
#the participant who smelled TMA as being stronger tended to rate different nostrils as being stronger than same nostrils and visa versa
#possibly those who smelled TMA weakly are also mixing it up with green more and are therefore more likely to rate it as being stronger when in the same nostril

###########################################
#does the data improve if we compare right nostril to right nostril and left nostril to left nostril?
#start with testDataRename
right.unityline <- ddply(.data = subset(testDataRename, Running.Block. == "Test1" & RightNostril == 1), .variables = c("Subject", "IntensityRating", "Running.Block."), function(x) c(DifferentNostrils = mean(subset(x, (RightType == "TMA" & LeftType == "Nonenol") | (RightType == "Nonenol" & LeftType == "TMA"))$value), DifferentNostrilsSE = sd(subset(x, (RightType == "TMA" & LeftType == "Nonenol") | (RightType == "Nonenol" & LeftType == "TMA"))$value/2), SameNostril= mean(subset(x, (RightType == "TMA+Nonenol" | LeftType == "TMA+Nonenol"))$value), SameNostrilSE= mean(subset(x, (RightType == "TMA+Nonenol" | LeftType == "TMA+Nonenol"))$value/2)))
left.unityline <- ddply(.data = subset(testDataRename, Running.Block. == "Test1" & LeftNostril == 1), .variables = c("Subject", "IntensityRating", "Running.Block."), function(x) c(DifferentNostrils = mean(subset(x, (RightType == "TMA" & LeftType == "Nonenol") | (RightType == "Nonenol" & LeftType == "TMA"))$value), DifferentNostrilsSE = sd(subset(x, (RightType == "TMA" & LeftType == "Nonenol") | (RightType == "Nonenol" & LeftType == "TMA"))$value/2), SameNostril= mean(subset(x, (RightType == "TMA+Nonenol" | LeftType == "TMA+Nonenol"))$value), SameNostrilSE= mean(subset(x, (RightType == "TMA+Nonenol" | LeftType == "TMA+Nonenol"))$value/2)))

ggplot(data = subset(right.unityline, IntensityRating == "FishyRating"), aes(x = DifferentNostrils, y = SameNostril, colour = Subject)) +
  geom_point() +
  xlim(0,200) +
  ylim(0,200) +
  geom_abline(a=1, b=1) +
  geom_errorbar(aes(ymin = SameNostril - SameNostrilSE, ymax = SameNostril + SameNostrilSE)) +
  geom_text(aes(label = Subject))
#right looks better and the variability is lower

ggplot(data = subset(left.unityline, IntensityRating == "FishyRating"), aes(x = DifferentNostrils, y = SameNostril, colour = Subject)) +
  geom_point() +
  xlim(0,200) +
  ylim(0,200) +
  geom_abline(a=1, b=1) +
  geom_errorbar(aes(ymin = SameNostril - SameNostrilSE, ymax = SameNostril + SameNostrilSE)) +
  geom_text(aes(label = Subject))

#boxplot
ggplot(subset(testDataMinMax, RightNostril == 1), aes(x = OdorType, y = normValue)) +
  geom_boxplot() +
  facet_grid(IntensityRating ~ Running.Block.) +
  xlab("Normalized Intensity Rating") +
  ylab("Nostril Type")
#that looks better
ggplot(subset(testDataMinMax, LeftNostril == 1), aes(x = OdorType, y = normValue)) +
  geom_boxplot() +
  facet_grid(IntensityRating ~ Running.Block.) +
  xlab("Normalized Intensity Rating") +
  ylab("Nostril Type")
#this does not follow the pattern

#check for ratings of TMA right versus left - is TMA stronger out of one nostril consistently
TMA.rightVleft <- subset(testDataMinMax, IntensityRating == "FishyRating" & OdorType == "TMAAlone", select = c("Subject", "RightType", "value"))
TMA.rightVleft_cast <- dcast(TMA.rightVleft, Subject~RightType, fun.aggregate = mean, value.var = "value")
names(TMA.rightVleft_cast) <- c("Subject", "LeftNostril", "RightNostril") 

ggplot(TMA.rightVleft_cast, aes(x = LeftNostril, y = RightNostril)) +
  geom_point()+
  xlim(0,200) +
  ylim(0,200) +
  geom_abline(a=1, b=1) +
  geom_text(aes(label = Subject))
#not consistently stronger TMA left or right, but those that find TMA the strongest are from the left nostril - this probably means that its not the olfactometer
#as much as it is which nostril is more open for the participant
#this data includes TMA alone for both blocks

###########################################
#does order make a difference? in other words are there any trends for those who got odor block 1 first as opposed to second
#first I need to make a database of which subject got which block first

###########################################

#Going to try a new method of normalization
#top scale usage is going to be gathered from the top value participant picked on the first part of the test - assuming that they used the scales correctly
#bottom scale value will be the average of the fishy or antagonist ratings of the blank, this will be split up by block. 
#what if the blank smells like TMA becuase of contamination and the antagonist alone is blocking the contamination by fishy? just a thought - is there a way to look at this
  #i could maybe look at that by doing a unity graph and comparing blank to antag alone
  #I should look at this as a unity graph anyawys


#other goals
#look at the data split by nostril
#look at plot of same versus different nostril for each person and then try looking at that normalized by blank or antagonist. 
#or what was the graph I looked at before was it the difference between TMA alone and same or different? I can't remember 
#and maybe I should try this as a median and as a mean


       