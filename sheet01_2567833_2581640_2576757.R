### Stats with R Exercise sheet 1 

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Monday, November 16. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are required to work together in groups of three students, but everybody 
## needs to submit the group version of the homework via moodle individually.
## You need to provide a serious attempt at solving each exercise in order to have
## the assignment graded as complete. 

## Please write below your (and your teammates') name and matriculation number. 
## Name: Farzaneh Khojasteh
## Matriculation number: 2567833
## Name: Meghana Srinath
## Matriculation number: 2581640
## Name: Dmitrii Badretdinov
## Matriculation number: 2576757

## Change the name of the file by adding your matriculation numbers
## (sheet01_firstID_secondID_thirdID.R)



## Many of the things on this exercise sheet have not been discussed in class. 
## The answers will therefore not be on  the slides. You are expected to find 
## the answers using the help function in R, in the textbooks and online. If you 
## get stuck on these exercises, remember: Google is your friend.
## If you have any questions, you can ask these during the tutorial, or use the 
## moodle discussion board for the course.

###############
### Exercise 1: Getting started
###############
## a) Look at your current working directory.

getwd()

## b) Get help with this function.

?getwd

## c) Change your working directory to another directory.

ifelse(!dir.exists("~/R-course-files"), dir.create("~/R-course-files"), FALSE)
setwd("~/R-course-files")


###############
### Exercise 2: Participants' age & boxplots
###############
## In this exercise, we will deal with data from a package.

## a) Install the package "languageR" and load it.

install.packages("languageR")
library(languageR)

## b) Specifically, we will deal with the dataset 'dutchSpeakersDistMeta'. 
##    This dataset should be available to you once you've loaded languageR.
##    The dataset contains information on the speakers included in the Spoken 
##    Dutch Corpus. Inspect 'dutchSpeakersDistMeta'. Look at the head, tail, 
##    and summary. What do head and tail show you?

data(dutchSpeakersDistMeta)
head(dutchSpeakersDistMeta)
tail(dutchSpeakersDistMeta)
summary(dutchSpeakersDistMeta)

## c) Each line in this file provides information on a single speaker. How many 
##    speakers are included in this dataset? In other words, use a function to 
##    retrieve the number of rows for this dataset.

nrow(dutchSpeakersDistMeta)

## d) Let's say we're interested in the age of the speakers included in the 
##    corpus, to see whether males and females are distributed equally. 
##    Create a boxplot for Sex and AgeYear.

boxplot(AgeYear~Sex, data=dutchSpeakersDistMeta)

## e) Does it seem as if either of the two groups has more variability in age?

##      According to the plot, the female group has more variability: 
##      their minimum (Q1 - 1.5 interquartile range) goes lower than 1930 when
##      the male group has only one outlier in this range.
##      The maximum of the female group is also farther the the male one.

## f) Do you see any outliers in either of the two groups?

##      There are two outliers in the male group on the lower side of the
##      spectrum. According to the plot, the female group has no outliers.


## g) Now calculate the mean and standard deviation of the AgeYear per group. 
##    Do this by creating a subset for each group.
##    Do the groups seem to differ much in age?

maleSubset <- subset(dutchSpeakersDistMeta, Sex == "male")
femaleSubset <- subset(dutchSpeakersDistMeta, Sex == "female")
(mean(maleSubset$AgeYear))
(mean(femaleSubset$AgeYear))
(sd(maleSubset$AgeYear))
(sd(femaleSubset$AgeYear))

##      If we were to consider only the mean and standard deviation,
##      it could be said that the groups don't differ much in age.
##      The difference in the mean is minuscule.
##      The difference in the standard deviation is relatively larger, but
##      still not enough to say that the groups differ in age noticeably.

## h) What do the whiskers of a boxplot mean?

##      The whiskers are the range between the quartiles (Q1, Q3) and the
##      minimum or maximum respectively. 
##      Minimum being Q1 - 1.5 interquartile range, when maximum being
##      Q1 + 1.5 interquartile range.

## i) What is the inter-quartile range in the boxplot?

##      The inter-quartile range is the range between the first and third
##      (Q1 and Q3) quartiles. That is, Q3 - Q1.

## j) Is the plot positively or negatively skewed?

##      The plot being skewed means that most of its values are on one of the 
##      tails of the distribution.
##      Based on the boxplot, we can see that both male and female subsets are
##      negatively skewed. That is, most of their values are on the left side
##      from the mean.



###############
### Exercise 3: Children's stories & dataframes
###############
# A researcher is interested in the way children tell stories. More specifically,
# she wants to know how often children use 'and then'. She asks 25 children to
# tell her a story, and counts the number of times they use 'and then'.
# The data follow:

# 18 15 22 19 18 17 18 20 17 12 16 16 17 21 25 18 20 21 20 20 15 18 17 19 20 


## a) What measurement scale is this data? Is it discrete or continuous? Explain
##    in one sentence why? (remember, comment out written answers)
ratio/discrete/because when we have the number of something it means it should be interval or ratio and because in this case 
we can have the zero number it is ratio since in interval we cannot have zero. and ratio can be continuous and discrete. Here the scale is 
discrete because between 16 and 17 we cannot have another number.


## b) In the next questions (c-e), you will create a dataframe of this data, 
##    which will also include participant IDs.
##    Why is a dataframe better suited to store this data than a matrix?
Because data in matrix should have the same data type. But columns in dataframe can have different data type like numeric,factor,character


## c) First create a vector with participant IDs. Your vector should be named 
##    'pps', and your participants should be labeled from 1 to 25
pps <- sprintf("User % d", 1:25)  

## d) Next, create a vector containing all the observations. Name this vector 'obs'.
obs <- c(18,15,22,19,18,17,18,20,17,12,16,16,17,21,25,18,20,21,20,20,15,18,17,19,20) 

## e) Create a dataframe for this data. Assign this to 'stories'. 
stories <- data.frame(pps,obs) 

## f) Take a look at the summary of your dataframe, and at the classes of your 
##    columns. What class is the variable 'pps'?
stories
summary(stories)

## g) Change the class of 'pps' to factor. Why is factor a better class for this
##    variable?
as.factor((pps))


## h) Plot a histogram (using hist()) for these data. Set the number of breaks 
##    to 8.
hist(obs, breaks=8, col="red")



## i) Create a kernel density plot using density().
d <- density(obs) 
plot(d)

## j) What is the difference between a histogram and a kernel density plot?
ensity plot has advantage over histogram because in density plot, the plot does not affected by the number of bars.A histogram with 20 
bars will produce a better distinguishable enough shape of distribution than a histogram with only 4 bars.Density plot does not affected 
with this problem
## This is a difficult one, remember you just need to provide a serious attempt at solving each 
## exercise in order to pass. 
## k) Overlay the histogram with the kernel density plot 
##    (hint: the area under the curve should be equal for overlaying the graphs 
##    correctly.)

install.packages("ggplot2")                          
library("ggplot2")
ggplot(stories, aes(obs)) +                                
  geom_histogram(aes(y = stat(density))) +
 geom_density(col = "red")



###############
### Exercise 4: Normal distributions
###############
## In this exercise, we will plot normal distributions.

## a) First, use seq() (?seq) to select the x-values to plot the range for
##    (will become the x-axis in the plot).
##    Get R to generate the range from -5 to 5, by 0.1. Assign this to the 
##    variable x.
x <- seq(-5, 5, by = 0.1)

## b) Now we need to obtain the y-values of the plot (the density). We do this 
##    using the density function for the normal distribution. 
##    Use "help(dnorm)" to find out about the standard functions for the normal 
##    distribution.
y <- dnorm(x, mean = mean(x), sd = sd(x))

## c) Now use plot() to plot the normal distribution for z values of "x". 
plot(x,y)

## d) The plot now has a relatively short y-range, and it contains circles 
##    instead of a line. 
##    Using plot(), specify the y axis to range from 0 to 0.8, and plot a line 
##    instead of the circles.
plot(x,y, ylim=c(0,0.8), type = "l")

## e) We want to have a vertical line to represent the mean of our distribution.
##    'abline()' can do this for us. Look up help for abline(). 
##    Use abline() to create the vertical line. Specify the median of x using
##    the argument 'v'.
##    In order to get a dashed line, set the argument 'lty' to 2.
#abline(v=c(mean(x),median(x)),lty = 2). 
#The question also mentions mean and hence added the above code although mean ~ median=0
abline(v=(median(x)),lty = 2)

## f) Take a look at the beaver1 dataset. (You can see it by typing "beaver1".) 
##    Then select only the temperature part and store it in a variable "b1temp".
beaver1
b1temp <- beaver1$temp

## g) Calculate the mean and standard deviation of this dataset and plot a normal
##    distribution with these parameters.
b1_mean <- (mean(b1temp))
b1_sd <- (sd(b1temp))
temp_normal <- dnorm(b1temp, mean = b1_mean, sd = b1_sd)
plot(b1temp, temp_normal)

## h) We observe two temparatures (36.91 and 38.13). What's the likelihood that
##    these temperatures (or more extreme ones) respectively come 
##    from the normal distribution from g)?
1 -pnorm((36.91-b1_mean)/b1_sd)
1 -pnorm((38.13-b1_mean)/b1_sd)
#The probabilities are as obtained above. 
#36.91 is more likely to come from normal distribution at around 40% while 38.13 is mostly not.

## i) Use the random sampling function in R to generate 20 random samples from
##    the normal distribution from g), and draw a histogram based on this sample.
##    Repeat 5 times. What do you observe?

temp_random_sample = sample (temp_normal, size=20)
hist(temp_random_sample)
#The selection is random and hence it is not normally distributed anymore
#Using any of the individual samples, the overall data distribution cannot be interpretted
#and the samples selected each time varies hence the histogram varies accordingly
