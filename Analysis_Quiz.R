# Looking at the quiz data to see if anyone should be eliminated
#Import Libraries
library(reshape2)
library(ggplot2)
library(plyr)

#read all files from directory
subj <- dir(path = "/Volumes/mainland/Projects/TMA blocker/Split Nostril Study/Setup2-Olfactometer/Code/SplitNostrilSetup_07_2016/DataPilot2/", pattern="\\.txt$", full.names=TRUE)# creating a list of all file names
names(subj)  <- basename(subj)
df  <- ldply(subj, read.delim, stringsAsFactors=FALSE)

#get the training data from the smell training (quiz and such)
smellTrainingQuiz <- subset(df, Procedure.Trial. == "Quiz"& Subject !=1 & Subject != 2 & Subject !=57, select = c("Subject", "Procedure.Block.", "IntensityRatingTraining.Trial.", "OdorType", "Procedure.Trial.", "QuizPage.RESP", "Running.Block.", "CorrectAnswer.Trial."))

quizAntag <- subset(smellTrainingQuiz, Running.Block. == "Train1")
quizControl <- subset(smellTrainingQuiz, Running.Block. == "Train2")

#Pull out participants that had to take the antag quiz more than once
numquizAntag <- ddply(quizAntag, .variables = c("Subject"), .fun = summarize, n = length(Subject))
remove <- numquizAntag$Subject[which(numquizAntag$n>8)]

#Lets just try eliminating these guys and running the same graphs from the other script and see if anything happens
df <- df[!(df$Subject %in% remove),] #eliminates these subjects
