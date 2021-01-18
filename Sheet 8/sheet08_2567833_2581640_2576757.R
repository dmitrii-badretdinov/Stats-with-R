### Stats with R Exercise sheet 8

##########################
#Linear Mixed Effects Models
##########################

## Please fill in the exam survey posted on Teams.
##https://teams.microsoft.com/l/entity/81fef3a6-72aa-4648-a763-de824aeafb7d/_djb2_msteams_prefix_835713882?context=%7B%22subEntityId%22%3Anull%2C%22channelId%22%3A%2219%3Af7da5fa2071d48a99900f18e4eaec8f5%40thread.tacv2%22%7D&groupId=c1d382ae-bcc1-4f35-b1ba-8e312dd7bde0&tenantId=67610027-1ac3-49b6-8641-ccd83ce1b01f

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Monday, January 18. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.

## You need to provide a serious attempt to each exercise in order to have
## the assignment graded as complete.


## Please write below your (and your teammates) name, matriculation number. 
## Name: Farzaneh Khojasteh
## Matriculation number: 2567833
## Name: Meghana Srinath
## Matriculation number: 2581640
## Name: Dmitrii Badretdinov
## Matriculation number: 2576757

## Change the name of the file by adding your matriculation numbers
## (exercise0N_firstID_secondID_thirdID.R)

###########################################################################################
###########################################################################################
package_list <- c("lme4", "lattice", "Matrix")
warn_level <- getOption("warn")
options(warn = -1)

for (val in package_list)
{
  if(!require(val, character.only = TRUE))
  {
    install.packages(val)
    library(val, character.only = TRUE)
  }
}

options(warn = warn_level)
rm(package_list, val, warn_level)

# a)There is (gender.Rdata) datasets on moodle.
#   Read in the data file (gender.Rdata) 
#   and assign it to a variable called "dat". 
#   See a description of the items in the datasets below.

# The files contain data from an experiment where people were reading sentences, 
# and pressed the space bar to see the next word. The duration for which a word was 
# viewed before pressing the space bar again is the reading time of the word, and is 
# stored in the file as "WORD_TIME". The experiment had 24 items (given as "ITEM_ID") 
# and 24 subjects (given as "PARTICIPANT"). The order in which the different sentences 
# were presented in the experiment is given in the variable "itemOrder". 

dat = read.csv("gender.Rdata", TRUE, ' ')


# b) Inspect "dat" and provide 2 plots. 
#    The first plot should provide insights about the relationship between WORD_TIME 
#    and ITEM_TYPE. 
#    For the second plot you should first subset the data using only RELWDINDEX == 0 and
#    then plot the WORD_TIME for the different conditions (ITEM_TYPE).

str(dat)

unique(dat$ITEM_TYPE)
dat$ITEM_TYPE = as.factor(dat$ITEM_TYPE)

bwplot(WORD_TIME~ITEM_TYPE, data = dat)
# The average time to read the word seems almost equal in both item types along with similar distribution

dat_relwdindex0 = subset(dat, RELWDINDEX == 0)
bwplot(WORD_TIME~ITEM_TYPE, data = dat_relwdindex0)
# On considering only one relative word in the index, we see a difference in avg word reading times 


# c) Decide whether you want to exclude any data points (provide not only the code,
#    but also a detailed (!) explanation). 
#    Note that we are evaluating WORD_TIME as our response variable. 
#    What time intervals make sense for such an experiment?

dat[order(dat$WORD_TIME, decreasing=TRUE)[1:10],]
dat[order(dat$WORD_TIME, decreasing=FALSE)[1:100],]
dat_relwdindex0[order(dat_relwdindex0$WORD_TIME, decreasing=TRUE)[1:10],]
dat_relwdindex0[order(dat_relwdindex0$WORD_TIME, decreasing=FALSE)[1:100],]

mean(dat_relwdindex0$WORD_TIME)

## Considering WORD_TIME is counted in milliseconds:
##
## On the upper bound, 
## There are two outliers: Titel and See.
## All other words took no more than 4 seconds to be processed when these two
##  fairly short words took 6 and 5 seconds respectively. 
##  Therefore, we remove two data points of the upper bound.
## 
## On the lower bound, 
## when pressing the spacebar as fast as possible, I was able to push it 
##  35 times in 4.17 seconds.
## Keeping in mind that the participants weren't focused on the spacebar but on
##  reading words, I think it's reasonable to consider the lower bound as at
##  least 130ms.

dat_sanity_checked = subset(dat, WORD_TIME < 5000 & WORD_TIME > 130)


# d) Make a scatter plot where for each index word as the sentence progresses (RELWDINDEX),
#    the average reading time is shown for each of the two conditions (ITEM_TYPE).
#    Please use two different colours for the different conditions.

dat_sanity_checked$WORD_TIME_avg_by_RELWDINDEX = ave(dat_sanity_checked$WORD_TIME, 
                                                     dat_sanity_checked$ITEM_TYPE, dat_sanity_checked$RELWDINDEX)

print(xyplot(RELWDINDEX ~ WORD_TIME_avg_by_RELWDINDEX, group = ITEM_TYPE, dat_sanity_checked))


# e) You do not need to use ggplot here, just follow the example below.
#    The code is a plot for the dataset 'sleepstudy' from the package 'lme4'.
#    The figure shows relationships between days without sleeping and reaction 
#    time for each participant (subject) separately.

summary(sleepstudy)
print(xyplot(Reaction ~ Days | Subject, sleepstudy, aspect = "xy",
             layout = c(9,2), type = c("g", "p", "r"),
             index.cond = function(x,y) coef(lm(y ~ x))[1],
             xlab = "Days of sleep deprivation",
             ylab = "Average reaction time (ms)"))

#    Your task is to figure out how to adapt this plot for our data. What do you 
#    conclude regarding the reading sentences experiment?

##
length(unique(dat_sanity_checked$PARTICIPANT))

print(xyplot(WORD_TIME ~ RELWDINDEX | PARTICIPANT, dat_sanity_checked, aspect = "xy",
             layout = c(8,3), type = c("g", "p", "r"),
             index.cond = function(x,y) coef(lm(y ~ x))[1],
             xlab = "Word's position in a sentence",
             ylab = "Reading speed"))

print(xyplot(WORD_TIME ~ ITEM_TYPE | PARTICIPANT, dat_sanity_checked, aspect = "xy",
             layout = c(24,1), type = c("g", "p", "r"),
             index.cond = function(x,y) coef(lm(y ~ x))[1],
             xlab = "Word type",
             ylab = "Reading speed"))

## As we can see on the graphs, the slopes of a linear model vary significantly.
## For some participants, the slope is zero when for some others it has either
##  visibly positive or negative value.
##
## Based on this observation, I'd conclude that the reading speed is noticeably
##  person-dependent.


# f)  Explain the main need for switching to Linear mixed effect model for the study.
#     In addition, report what could be the fixed and random effect structure.

## The main reason to switch is the data fails to meet the independence assumption that
## is conditional for normal linear regression.
## The data is collected for different words from same set of people i.e it is both
## multiple observations from each subject and multiple observations from each item,
## hence the observations are likely not independent of each other.
## Some participants might be good readers while some might be slow generally in that language
## and hence each have their own reading speed.

## fixed effect: ITEM_TYPE, RELWDINDEX, itemOrder are fixed effect as they are items of limited type
## random effect: PARTICIPANT and EXPWORD would most likely be the random effect structures 
## as they are chosen at random and on addition of different person from the population or 
## a new word from the dictionary will cause a change
#(The interpretation for fixed and random effect could also be as follows:)
## Fixed effects are constant across individuals. Random effects aren't.
## The slopes of WORD_TIME ~ RELWDINDEX | PARTICIPANT and WORD_TIME ~ ITEM_TYPE | PARTICIPANT
## vary significantly. Therefore, they cannot be the fixed effects.
## A fixed effect would be something that had the same slope for all participants.


# g) Experiment with calculating a linear mixed effects model for this study, 
#    and draw the appropriate conclusions 

model_item = lmer(WORD_TIME ~ ITEM_TYPE + (ITEM_TYPE|PARTICIPANT), dat_sanity_checked)
print(dotplot(ranef(model,condVar=TRUE),  scales = list(x = list(relation = 'free')))
      [["PARTICIPANT"]])
model_relw = lmer(WORD_TIME ~ RELWDINDEX + (RELWDINDEX|PARTICIPANT), dat_sanity_checked)
print(dotplot(ranef(model,condVar=TRUE),  scales = list(x = list(relation = 'free')))
      [["PARTICIPANT"]])

model_all = lmer(WORD_TIME ~ RELWDINDEX + (1|PARTICIPANT) + (1|EXPWORD) + (RELWDINDEX|ITEM_TYPE), dat_sanity_checked)
print(dotplot(ranef(model,condVar=TRUE),  scales = list(x = list(relation = 'free')))
      [["PARTICIPANT"]])
coef(model_all)

## Based on the graphs, we can see that the dependence ITEM_TYPE and RELWDINDEX to
##  PARTICIPANT is not of a linear nature.

# h) Describe how you would report and write up the analysis giving a detailed explanation for each model 

## I would report the analysis starting from the general overview of the data and graphs given in b).
## Then, I would introduce the dependence of the features on the participants and that it brakes
##  the default assumption of the feature independence.
## With that said, I would introduce the mixed effects linear model and how it handles the broken
##  assumption.
## After presenting the plots of ITEM_TYPE|PARTICIPANT and RELWDINDEX|PARTICIPANT,
##  I would clarify that the slope heavily depends on the participant with most
##  of the slopes being RELWDINDEX positive and ITEM_TYPE being negative.
## Overall, I'd conclude that the results are heavily person-dependent, and no
##  overarching trend could be drawn on such a small number of participants.
sessionInfo()
citation("lme4")
## We used R core team (2020) and lme4_1.1-23 (Bates D, Mächler M, Bolker B, Walker S (2015))
## to perform linear mixed effect analysis to understand the relationship between 
## relative word in the index and the word reading time.
## For model_item, as fixed effect we used ITEM_TYPE and as random effect, intercept for PARTICIPANT was added.
## For model_relw, as fixed effect we used RELWDINDEX and as random effect, intercept for PARTICIPANT was added.
## For model_all, as fixed effect we used RELWDINDEX. As random effect, intercepts for 
## PARTICIPANT,ITEM_TYPE and EXPWORD was added along with by ITEM_TYPE slope for effect of RELWDINDEX. 

# i) Let's get back to the dataset 'sleepstudy'. The following plot shows 
#    subject-specific intercepts and slopes. Adapt this plot for our study 
#    and draw conclusions.

model = lmer(Reaction ~ Days + (Days|Subject), sleepstudy)
print(dotplot(ranef(model,condVar=TRUE),  scales = list(x = list(relation = 'free')))
      [["Subject"]])

##
model_sentence = lmer(WORD_TIME ~ RELWDINDEX + (RELWDINDEX|ITEM_TYPE), dat_sanity_checked)
print(dotplot(ranef(model,condVar=TRUE),  scales = list(x = list(relation = 'free')))
      [["PARTICIPANT"]])

## As described in h), the following graph shows the dependence between ITEM_TYPE
##  and PARTICIPANT. The information about it was included in h).


