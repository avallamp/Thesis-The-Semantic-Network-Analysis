### MY SCRIPT LEGEND FOR FUTURE REFERENCE ###

##############################################################
#############  PACKAGES WITH USEFUL FUNCTIONS ################
##############################################################

# Packages that are useful for formatting & processing data
library(plyr)
library(tidyr)

# Packages that are useful for graphing
library(sciplot)
library(ggplot2)

# Packages that are useful for conducting analyses
library(lme4)
library(lmerTest)
library(car)

##############################################################
#################  FUNCTIONS USED IN SCRIPT ##################
##############################################################

# This section defines some custom functions that will be used in the
# script.

# This creates a function called extract.le that will be used to extract 
# the data from the .csv files that will be useful, such as participant ID, 
# variables that identify the condition for each trial, and accuracy for each trial
extract.le <- function(file) {
  sheet <- read.csv(file=file)
  if(any(grepl("Acc", colnames(sheet)))) {
    data <- data.frame(as.character(sheet$participant[sheet$LE_Mouse.midButton == 0]),
                       sheet$expName[sheet$LE_Mouse.midButton == 0],
                       sheet$Order[sheet$LE_Mouse.midButton == 0],
                       sheet$Distance[sheet$LE_Mouse.midButton == 0],
                       sheet$Triad[sheet$LE_Mouse.midButton == 0],
                       sheet$Acc[sheet$LE_Mouse.midButton == 0])
  } else {
    data <- data.frame(as.character(sheet$participant[sheet$LE_Mouse.midButton == 0]),
                       sheet$expName[sheet$LE_Mouse.midButton == 0],
                       sheet$Order[sheet$LE_Mouse.midButton == 0],
                       sheet$Distance[sheet$LE_Mouse.midButton == 0],
                       sheet$Triad[sheet$LE_Mouse.midButton == 0],
                       sheet$LE_Resp.corr[sheet$LE_Mouse.midButton == 0])
  }
  colnames(data) <- c("ID", "Age", "Order", "Distance", "Triad", "Acc")
  data$Age <- gsub("McNeillET_test_","", data$Age)
  data$Age <- tolower(data$Age)
  data$Distance <- factor(data$Distance, levels=c("Ident","Near","Far"))
  data <- na.omit(data)
  return(data)
}

# These are functions that will be used in the analysis section to take a subset
# of the data (e.g., the data for Direct Formation trials in children),
# and return the results of an analysis of whether accuracy in that subset of
# data was above chance.
return.intercept <- function(input.model) {
  model.summary <- summary(input.model)
  model.intercept <- model.summary$coefficients
  model.intercept[4] <- round(model.intercept[4], digits=4)
  return(model.intercept)
}

return.ttest <- function(input.ttest) {
  stats <- data.frame(input.ttest$statistic, input.ttest$parameter, input.ttest$p.value)
  names(stats) <- c("t.value", "df", "p")
  stats$p <- round(stats$p, digits=4)
  return(stats)
}
##############################################################
##################  EXTRACT & FORMAT DATA ####################
##############################################################

# This section extracts the data from the spreadsheets, formats it
# to be tidy and clear, and removes data from anyone with abnormally low
# accuracy

# Change this to be the location of the .csv data files on my computer
setwd("C:/Users/Layla/Desktop/Research/1st & 2nd Order/McNeill - Words/McNeill - ET/McNeillET - LE/data")

# Get a list of the .csv data file names
files <- list.files(pattern=".csv")

# Extract the data of interest from each file
# This creates a dataframe in which a row is a trial for a participant
# Columns provide information about each row:
# Participant ID
# Participant Age
# "Order": 1=direct, 2=shared
# "Distance": Ident = apple/horse, Near = apple-like fruit/horse-like animal, Far = other fruits/other animals
le.raw <- ldply(files, extract.le)
head(le.raw)

# Add some columns that re-code the columns in le.raw to be more transparent

# Create new "Cond.Link" column that identifies whether the trial was a formation ("Ident") or generalization ("Near" or "Far") trial
le.raw$Cond.Link <- ifelse(le.raw$Distance=="Ident","Formation","Generalization")
# Create a new "Cond.Type" column that re-codes the Order column as "Direct" and "Shared"
le.raw$Cond.Type <- ifelse(le.raw$Order==1,"Direct","Shared")
# Create a new "Target" column that re-codes the Triad column as apple and horse
le.raw$Target <- ifelse(le.raw$Triad==1, "apple","horse")

# Re-order the Age column so that "child" is the first level of the age factor, and
# adult is the second. This will ensure that graphs of child data will appear before 
# graphs of adult data below.
le.raw$Age <- factor(le.raw$Age, levels=c("child","adult"))

# Identify participants with abnormally low accuracy (i.e., "outliers") and remove from dataset
le.outlier <- ddply(le.raw, .(Age,ID),summarise, Acc.Count=sum(Acc)) #count number of acc trials for each participant
plot(table(le.outlier$Acc.Count), xlim=c(0,48), ylab="Number of participants", xlab="Number of Acc Trials") #visualize numbers of acc trials across participants
le.raw <- le.raw[!(le.raw$ID %in% as.character(le.outlier$ID[le.outlier$Acc.Count<15])),] #remove data from participants with fewer than 15 acc trials

##############################################################
#####################  DATA PROCESSING #######################
##############################################################

# This section uses the raw data to generate a dataframe of mean 
# accuracies for each participant in each Cond.Link and Cond.Type 
# condition. The dataframe also indicates the each of each participant
le.acc.means <- ddply(le.raw, .(Age, ID, Cond.Link, Cond.Type), summarise, Acc.mean=mean(Acc))
head(le.acc.means)

#Convert to wide for predicting Shared from Direct (I will not need this for thesis but just in case)
le.predict <- pivot_wider(le.summ, names_from=c(Cond.Link,Cond.Type), values_from=Acc.mean)

##############################################################
#########################  GRAPHS ############################
##############################################################

# This creates nice labels that are used in the graphs
age.labs <- c("4-Year-Olds","Adults")
names(age.labs) <- c("child","adult")

# This makes graphs of data from the Link Formation trials, and saves
# it to an object called le.form
le.form.plot <- ggplot(le.acc.means[le.acc.means$Cond.Link=="Formation",], aes(x=Cond.Type, y=Acc.mean)) +
  facet_wrap(~Age, labeller = labeller(Age = age.labs)) + theme_bw() +
  geom_hline(yintercept=.5,linetype="dashed", color="black") +
  geom_boxplot(aes(group=Cond.Type,fill=Cond.Type), position=position_dodge(), outlier.shape=NA, show.legend=F) +
  geom_point(aes(x=Cond.Type, y=Acc.mean, group=Cond.Type, fill=Cond.Type), alpha=.3, shape=16, position=position_jitterdodge(jitter.width=.1), show.legend=F) +
  # The line below sets the colors for the boxplots in terms of red, green, blue, and transparency. There are two values for red, green, etc for the two 
  # colored boxplots. Can fiddle with these to change colors
  scale_fill_manual(name="",values = rgb(c(1,0),c(0,1),c(1,1),c(.6,.6))) + 
  scale_y_continuous(expand = expansion(mult = c(.05, .25))) +
  labs( y="Accuracy", x="") +
  theme(axis.text.x = element_text( size = 8, color="black" ),
        axis.text.y = element_text( size = 6, color="black" ),
        axis.title = element_text( size = 8),
        strip.text = element_text(size = 7),
        panel.grid=element_blank(),
        #plot.title = element_text(size=8, hjust = 0.5, face = "bold")
  )

# Same as above, but for Link Generalization
le.gen.plot <- ggplot(le.acc.means[le.acc.means$Cond.Link=="Generalization",], aes(x=Cond.Type, y=Acc.mean)) +
  facet_wrap(~Age, labeller = labeller(Age = age.labs)) + theme_bw() +
  geom_hline(yintercept=.5,linetype="dashed", color="black") +
  geom_boxplot(aes(group=Cond.Type,fill=Cond.Type), position=position_dodge(), outlier.shape=NA, show.legend=F) +
  geom_point(aes(x=Cond.Type, y=Acc.mean, group=Cond.Type, fill=Cond.Type), alpha=.3, shape=16, position=position_jitterdodge(jitter.width=.1), show.legend=F) +
  # The line below sets the colors for the boxplots in terms of red, green, blue, and transparency. There are two values for red, green, etc for the two 
  # colored boxplots. Can fiddle with these to change colors
  scale_fill_manual(name="",values = rgb(c(1,0),c(0,1),c(1,1),c(.6,.6))) +
  scale_y_continuous(expand = expansion(mult = c(.05, .25))) +
  labs(y="Accuracy", x="") + 
  theme(axis.text.x = element_text( size = 8, color="black" ),
        axis.text.y = element_blank(),
        axis.title.x = element_text( size = 8),
        axis.title.y = element_blank(),
        strip.text = element_text(size = 7),
        panel.grid=element_blank(),
        #plot.title = element_text(size=8, hjust = 0.5, face = "bold")
        )

# This plots the Formation and Generalization graphs side-by-side
plot_grid(le.form.plot, le.gen.plot, ncol=2, rel_widths=c(1.1,1))
#Can also look at the Formation and Generalization graphs separately
le.form.plot
le.gen.plot

# This saves the plots to a png file on Lab laptop
png(file="C:/Users/Layla/Desktop/Research/1st & 2nd Order/McNeill - Words/McNeill - ET/McNeillET - LE/data/McNeillET - LE - Boxplots.png",
    width=640, height=220, res=120)
plot_grid(le.form, le.gen, ncol=2, rel_widths=c(1.1,1))
dev.off()

##############################################################
########################  ANALYSES ###########################
##############################################################

# For thesis, two sets of analyses:

# (1) Comparisons to chance: First, test whether accuracy
# in each condition in each age group was above chance (i.e., .5).
# For example, we want to test whether children were above chance
# on trials that assessed the Formation of Direct links, trials that
# assessed the Generalization of Shared links, and so on.

# (2) Comparisons of age groups and conditions: Second, we want to test
# whether accuracy varied across age and conditions. Here, instead of
# comparing accuracy to chance, we will be comparing accuracy across
# different age groups, conditions, etc.

#tldr: two options for conducting these analyses. Option 1 keeps the data in its raw form
#- i.e., accuracies on each trial that are either 0 (incorrect) or 1 (correct). 
#Thus, these analyses preserve all the data that I collected. However, these analyses will
# also be harder to write about in paper

# Analyses in Option 2 use data that has been "summarized": I.e., instead of
# accuracies of 0 and 1, for each condition in each participant, we calculate the mean
# accuracy across trials in that participant/condition. 
# These means were calculated in the Data Processing section above, and are stored
# in the object called le.acc.means.
# In this object, for each participant, we have calculated four mean accuracies:
# One for Direct-Formation trials, another for Direct-Generalization trials, 
# a third for Shared-Formation trials, and a fourth for Shared-Generalization trials. 
# Because the means are summaries of accuracy across trials, this does not 
# capture all the information recorded in the study.

######################### OPTION 1 ###########################

################### COMPARISON TO CHANCE #####################

# This divides the dataset into chunks that are analyzed, in which each chunk is data
# from one age group for one set of Cond.Type and Cond.Link conditions
# such as Direct Formation trials in children. 
# There are a total of 8 chunks (2 age groups x 2 Cond.Type conditions x 2 Cond.Link conditions)
# For each chunk, this analyzes whether accuracy across trials in that chunk 
# was above chance.
chance.mods.raw <- dlply(le.raw, .(Age, Cond.Type, Cond.Link), glmer, 
      formula = Acc ~ 1 + (1|ID), family="binomial")

# The two lines below extract the relevant results from the analysis conducted
# in the line above.
chance.mods.raw.coeff = ldply(chance.models, return.intercept)
chance.mods.raw.coeff$p.adjusted <- p.adjust(chance.mods.raw.coeff$"Pr(>|z|", method="holm")

# Look at the chance.mods.raw.coeff object
chance.mods.raw.coeff

# This object, chance.mods.raw.coeff, contains the results that you can report 
# if you use these analyses. These are the results of comparing each chunk of data
# to chance. In other words, each row of this object contains the results of comparing
# data from one age group (e.g., children) in one Cond.Type condition (e.g., Direct) in
# one Cond.Link condition (e.g., Formation) to chance. The first three columns identify
# the data chunk (e.g., age = child, Cond.Type = Direct, Cond.Link = Formation).
# The "Estimate" column is essentially an estimate of how much above chance the accuracy
# in the chunk was. The larger this "Estimate" is above 0, the more above chance the accuracy
# was. 
# The Std.Error column quantifies the amount of variability around the Estimate (similar to the
# variance/standard deviation/standard error around a mean)
# Ignore the z value.
# The Pr(>|z|) column gives the p-value. In other words, the Estimate tells how much
# above chance the accuracy was, and the p-value tells you whether this was significant.
# BUT, this analyses contains many (8) comparisons to chance. Conducting this many comparisons
# "inflates" the possibility that some of these comparisons will be "significant" even though
# they are not. So, we need to correct for this issue.
# Therefore, get the corrected p-value from p.adjusted column.

########### COMPARISON BETWEEN AGES & CONDITIONS #############

# This submits the full dataset to an analysis that tests whether accuracy varied
# across age groups, Cond.Type conditions, Cond.Link conditions, and interactions
# between them
omn.mod.raw <- glmer(Acc ~ Age*Cond.Type*Cond.Link + (1|ID), data=le.raw, family="binomial")

# From Layla: This reports the results of the analysis in a format that gives you information
# that is conventionally reported in psychology papers.
Anova(omn.mod.raw)

# The first column  refers to a main effect or interaction that
# could affect accuracy, such as age. 
# The second column quantifies an estimate of how much accuracy varied with the
# main effect or interaction.
# The third column is the degrees of freedom.
# The last column is the p-value.

# To interpret the effects of age and Cond.Type, we can look at how accuracy
# differed between ages and between Cond.Type conditions.

age.effect <- ddply(le.raw, .(Age), summarise, 
                    Acc.mean = mean(Acc),
                    Acc.sd = sd(Acc),
                    N = length(Acc),
                    Acc.se = Acc.sd/sqrt(N))
cond.type.effect <- ddply(le.raw, .(Cond.Type), summarise, 
                          Acc.mean = mean(Acc),
                          Acc.sd = sd(Acc),
                          N = length(Acc),
                          Acc.se = Acc.sd/sqrt(N))
# These objects give the means, standard deviations, and standard errors 
# of accuracy for each age (the age.effect object) and each cond.type
# the cond.type.effect object.
# can see that accuracy was higher in adults than children,
# and in Direct vs Shared.
age.effect
cond.type.effect

######################### OPTION 2 ###########################

################### COMPARISON TO CHANCE #####################

# The steps in this analysis are very similar to Option 1. The only differences
# are that the specific analyses used are ones that compare mean accuracies 
# rather than raw accuracies

# This divides the dataset into chunks that are analyzed, in which each chunk is data
# from one age group for one set of Cond.Type and Cond.Link conditions
# such as Direct Formation trials in children. 
# There are a total of 8 chunks (2 age groups x 2 Cond.Type conditions x 2 Cond.Link conditions)
# For each chunk, this uses t-tests to analyzes whether mean accuracies across
# participants in that chunk were above chance (i.e., above .5).
chance.ttest.means <- dlply(le.acc.means, .(Age, Cond.Type, Cond.Link), function(x) t.test(x$Acc.mean, mu = .5))

# The two lines below extract the relevant results from the analysis conducted
# in the line above.
chance.ttest.means.stats = ldply(chance.ttest.means, return.ttest)
chance.ttest.means.stats$p.adjusted <- p.adjust(chance.ttest.means.stats$p, method="holm")

# Look at the chance.ttest.means.stats object
chance.ttest.means.stats

# The info contained in this object is very similar to the info in the chance.mods.raw.coeff
# object from Option 1. The only difference is that the results are from t-tests
# that compare mean accuracies to a chance level of .5
# The t.value provies the estimate of how different mean accuracy was from .5, the
# df provides the degrees of freedom, the p value is the significance value, and again,
# adjust this value to correct for multiple comparisons.

########### COMPARISON BETWEEN AGES & CONDITIONS #############

# This submits the full dataset to an analysis that tests whether accuracy varied
# across age groups, Cond.Type conditions, Cond.Link conditions, and interactions
# between them
omn.mod.means <- lm(Acc.mean ~ Age*Cond.Type*Cond.Link, data=le.acc.means)

# From Layla: this reports the results of the analysis in a format that gives you information
# that is conventionally reported in psychology papers.
Anova(omn.mod.means)

# The format of these results is the same as in Option 1. As in Option 1,
# we're seeing significant main effects of Age and Cond.Type.


# To interpret the effects of age and Cond.Type, just as in Option 1, 
# look at how accuracy differed between ages and between Cond.Type conditions.

age.effect <- ddply(le.raw, .(Age), summarise, 
                    Acc.mean = mean(Acc),
                    Acc.sd = sd(Acc),
                    N = length(Acc),
                    Acc.se = Acc.sd/sqrt(N))
cond.type.effect <- ddply(le.raw, .(Cond.Type), summarise, 
                          Acc.mean = mean(Acc),
                          Acc.sd = sd(Acc),
                          N = length(Acc),
                          Acc.se = Acc.sd/sqrt(N))
# These objects give the means, standard deviations, and standard errors 
# of accuracy for each age (the age.effect object) and each cond.type
# the cond.type.effect object.
# From these values, we can see that accuracy was higher in adults than children,
# and in Direct vs Shared.
age.effect
cond.type.effect