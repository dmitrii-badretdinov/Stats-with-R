### Stats with R Exercise sheet 6

##########################
# ANOVA
##########################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Monday, January 04. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are required to work together in groups of three students.
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



#######################
### Exercise 1: Preparation
#######################

package_list <- c("boot", "ggplot2", "reshape")
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

# This time we will be working with the "amis" data frame (package 'boot') that has 
# 8437 rows and 4 columns.

# In a study into the effect that warning signs have on speeding patterns, 
# Cambridgeshire County Council considered 14 pairs of locations. The locations were 
# paired to account for factors such as traffic, volume and type of road. One site in 
# each pair had a sign erected warning of the dangers of speeding and asking drivers 
# to slow down. No action was taken at the second site. Three sets of measurements 
# were taken at each site. 
# Each set of measurements was nominally of the speeds of 100 cars but not all sites 
# have exactly 100 measurements. These speed measurements were taken before the 
# erection of the sign, shortly after the erection of the sign, and again after 
# the sign had been in place for some time.

# a) For the further reference please use ?amis. 
# It may take some time to understand the dataset. 

?amis


# b) Load the dataset, store it into a variable called "data", and briefly inspect it. 
# Feel free to make some plots and calculate some statistics in order to understand 
# the data.

data <- amis

ggplot(data, aes(x = speed)) +
  geom_bar()

ggplot(data, aes(x = speed)) +
  geom_histogram(bins = 40)

ggplot(data, aes(x = period, y = speed, group = period)) +
  geom_boxplot()


# c) All our columns have numeric type. Convert the categorical columns to factors.

data$period <- as.factor(data$period)
data$warning <- as.factor(data$warning)
data$pair <- as.factor(data$pair)


# d) Plot boxplots for the distribution of `speed` for each of the `period` values 
# (before, immediately after and after some time). Build 2 plots (each containing 3 
# boxplots) side by side depending on the `warning` variable.
# (For all plots here and below please use ggplot)

ggplot(data, aes(x = period, y = speed, fill = warning)) +
  geom_boxplot(position = "dodge")


# e) What can you conclude looking at the plots? What can you say about people's 
# behaviour in different periods: before, immediately after and after some time?

## Compared to the road with a warning sign, it's visible that whiskers, 
##  quartiles, and means are higher when there's no warning sign.
##
## The mean takes a noticeable dip right after the erection of the sign,
##  but returns to the value that was before erecting the sign after some time 
##  and even exceeds it.


# f) What are your ideas about why the data with warning==2 (sites where no sign was 
# erected) was collected?

## Likely it was collected to serve as a control group: to assess the volatility
## of the parameters on the road that was not affected by the signs.


#######################
### Exercise 2: 1-way ANOVA
#######################

# a) First let's create a new data frame which will be used for all exercise 2.
# For the 1-way ANOVA we will be working with a subset of `amis` using only the 
# data for sites where warning signs were erected, which corresponds to warning==1. 
# Therefore first subset your data to filter out warning==2 and then apply cast() 
# to average "speed" over each "pair" and "period". 
# Assign this new data frame to the variable casted_data.

casted_data <- cast(data[data$warning == 1,], period+pair~., fun.aggregate = mean, value = "speed", na.rm = T)


# b) Build boxplots of the average speed depending on "period".

ggplot(casted_data, aes(x = period, y = casted_data$'(all)')) +
  geom_boxplot()

# c) Looking at the boxplots, is there a difference between the periods?

## There is a visible difference between the periods.
## If we were to consider this plot without any prior information, 
##  it could be said that the sign had effect right after installation
##  but the situation has become even worse than without sign after some time.


# Now, let's check the ANOVA assumptions and whether they are violated or not 
# and why.

# d) Independence assumption
# (Figure out the best way to check this assumption and give a detailed justified 
# answer to whether it is violated or not.)



# e) Normality of residuals
# (Figure out the best way to check this assumption and give a detailed justified 
# answer to whether it is violated or not.)



# f) Homogeneity of variance of residuals
# (Figure out the best way to check this assumption and give a detailed justified 
# answer to whether it is violated or not.)



# g) Now we are ready to perform 1-way ANOVA: please use the function aov() on the 
# speed depending on the period, report p-value and interpret the result in details.

(aov_test <- aov(casted_data$`(all)` ~ casted_data$period, casted_data))
summary(aov_test)

## The p-value equals 0.382.
## As the p-value is higher than 0.05, the test states that there is no significant
##  difference between the groups.


# h) What were the degrees of freedom from the result in part g)

## The degrees of freedom were 2 for period and 39 for the residuals.


# i) Calculate the effect size and interpret the results. 

# j) Please do pairwise t-tests of the same variables as in g) using pairwise.t.test().


# k) Report the pairwise p-values and interpret the result in detail.


# l) Try to use no adjustment for pairwise testing and then the Bonferroni correction.
# Does the result change? 

# m) If the results change why do they? What does Bonferroni correction do?


# n) If the assumption of Normality does not hold, which test would you be using in this scenario.

#######################
### Exercise 3: 2-way ANOVA
#######################
# a) Now we want to analyze the influence of 2 categorical variables 
# (period and warning) on the speed.
# So let's turn back to our initial dataset amis (not its subset with warning==1).
# First, we need to average the speed over each `pair`, `warning` and `period
# Cast your data again and assign the results to casted_data2.

casted_data2 <- cast(data, period+warning+pair~., fun.aggregate = mean, value = "speed", na.rm = T)


# b) Calculate the mean for each of the 6 possible pairs of `period` and `warning`.
pair_mean <- aggregate(AverageSpeed ~ period + warning, casted_data2, mean)
pair_mean

# c) Do you think there is a significant difference between some of the groups?
#Second element of each pair is greater then first element. i.e. P1: W1 = AvgSpeed (36.51000) < P1:W2 = AvgSpeed (38.21857)
#There is no significant difference between some of the groups

# d) State the main difference between the applicability of 1-way and 2-way ANOVA.
#one way ANOVA: when we have one factor with two levels. we need to compute 1 effect. On the other side
#when we have two factors with two levels we use two-way ANOVA.We should compute two main effects and there might be an interract of two factors
# e) Now apply the 2-way ANOVA: please use the function aov() on the speed depending 
# on the period and warning.
# Report the p-value and interpret the result in detail. Properly formulate the findings!


# f) What do you conclude about the behaviour of drivers based on the 2-way ANOVA?

