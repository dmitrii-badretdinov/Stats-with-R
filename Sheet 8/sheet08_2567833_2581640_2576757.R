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

dat_relwdindex0 = subset(dat, RELWDINDEX == 0)
bwplot(WORD_TIME~ITEM_TYPE, data = dat_relwdindex0)


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

# f)  Explain the main need for switching to Linear mixed effect model for the study.
#     In addition, report what could be the fixed and random effect structure.

# g) Experiment with calculating a linear mixed effects model for this study, 
#    and draw the appropriate conclusions 

# h} Describe how would you report and write up the analysis giving a detailed explanation for each model 

# i) Let's get back to the dataset 'sleepstudy'. The following plot shows 
#    subject-specific intercepts and slopes. Adapt this plot for our study 
#    and draw conclusions.

model = lmer(Reaction ~ Days + (Days|Subject), sleepstudy)
print(dotplot(ranef(model,condVar=TRUE),  scales = list(x = list(relation = 'free')))
      [["Subject"]])

