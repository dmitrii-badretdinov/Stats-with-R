###############
### Cleaning Data - Exercise 1
###############

# Please do the "Cleaning Data with R" exercise that was assigned in DataCamp.
# We recommend that you take notes during the DataCamp tutorial, so you're able to use the commands 
# you learned there in the exercises below.
# This week, the exercise will be about getting data into a good enough shape to start analysing. 
# Next week, there will be a DataCamp tutorial on how to further work with this data.
## You need to provide a serious attempt to each exercise in order to have
## the assignment graded as complete. 

# 1. Download the data file "digsym.csv" from the moodle and save it in your working directory. 

## I hope the task doesn't require to download the file with a command from R
##  as we would have to deal with authorization.
##
## Checking if the file is in the working directory:

if(!file.exists("digsym.csv"))
  print("The file digsym.csv is missing.")


# 2. Read in the data into a variable called "dat".

dat <- read.csv(file = "digsym.csv")


# 3. Load the libraries languageR, stringr, dplyr and tidyr.

## Installing packages only if missing.
## I was unable to suppress the warning messages coming from require() 
##  by wrapping it in suppressMessages().
##  Therefore, I had to disable warnings globally during the installation.

package_list <- c("languageR", "dplyr", "tidyr", "stringr")
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


# 4. How many rows, how many columns does that data have?

nrow(dat)
ncol(dat)

## The data has 3700 rows and 11 columns.


# 5. Take a look at the structure of the data frame using "glimpse".

glimpse(dat)


# 6. View the first 20 rows, view the last 20 rows.

head(dat, n = 20)
tail(dat, n = 20)


# 7. Is there any missing data in any of the columns?

## Yes, there is missing data.
## In what head() has shown, the first ten observations don't have information
##  in the columns StimulDS1.*


# 8. Get rid of the row number column.

dat <- dat %>% select(-X)


# 9. Put the Sub_Age column second.

dat <- dat[,c(1,10,2:9)]


# 10. Replace the values of the "ExperimentName" column with something shorter, more legible.

## The column has only one unique value. Therefore, it's problematic to 
##  figure out what this column is about.

unique(dat$ExperimentName)

colnames(dat)[1] <- "Study_Name"


# 11. Keep only experimental trials (encoded as "Trial:2" in List), get rid of practice trials 
# (encoded as "Trial:1"). When you do this, assign the subset of the data to a variable "data2", 
# then assign data2 to dat and finally remove data2.

data2 <- filter(dat, List == "Trial:2")
dat <- data2
rm(data2)


# 12. Separate Sub_Age column to two columns, "Subject" and "Age", using the function "separate".

dat <- separate(dat, Sub_Age, c("Subject", "Age"))


# 13. Make subject a factor.

dat$Subject <- as.factor(dat$Subject)


# 14. Extract experimental condition ("right" vs. "wrong") from the "File" column:
# i.e. we want to get rid of digit underscore before and the digit after the "right" and "wrong".

dat <- mutate(dat, Right_or_wrong = 
        ifelse(grepl("right", File), "right",
                ifelse(grepl("wrong", File), "wrong", "NA")
               )
      )


# 15. Using str_pad to make values in the File column 8 chars long, by putting 0 at the end  (i.e., 
# same number of characters, such that "1_right" should be replaced by "1_right0" etc).

str_pad(dat$File, 8, "right", pad = "0")


# 16. Remove the column "List".

dat <- select(dat, -List)


# 17. Change the data type of "Age" to integer.

dat$Age <- as.integer(dat$Age)


# 18. Missing values, outliers:
# Do we have any NAs in the data, and if so, how many and where are they?

## The original dataset had NAs, but as we selected only Trial:2, there
## are no NAs left.

fresh_dat <- read.csv(file = "digsym.csv")

for (val in names(fresh_dat))
{
  if(any(is.na(fresh_dat[val])))
  {
    print(val)
  }
}

rm(fresh_dat)

for (val in names(dat))
{
  if(any(is.na(dat[val])))
  {
    print(val)
  }
}


# 19. Create an "accuracy" column using ifelse-statement.
# If actual response (StimulDS1.RESP) is the same as the correct response (StimulDS1.CRESP), put 
# in value 1, otherwise put 0.

dat <- mutate(dat, Accuracy = ifelse(StimulDS1.RESP == StimulDS1.CRESP, 1, 0))


# 20. How many wrong answers do we have in total?

count_result <- count(dat, Accuracy)

## We have 185 wrong answers, if we define "wrong" as Accurasy being 0.


# 21. What's the percentage of wrong responses?

mutate(count_result, ratio = n / sum(n))

## The percentage of wrong responses is 5.6%


# 22. Create a subset "correctResponses" that only contains those data points where subjects 
# responded correctly. 

correctResponses <- filter(dat, Accuracy == 1)


# 23. Create a boxplot of StimulDS1.RT - any outliers?

boxplot(correctResponses$StimulDS1.RT)

## According to the boxplot, most of the results are located between 0 and 2500.
## There are numerous outliers, one of which reaches above 12000.


# 24. Create a histogram of StimulDS1.RT with bins set to 50.

hist(correctResponses$StimulDS1.RT, breaks = 50)


# 25. Describe the two plots - any tails? any suspiciously large values?

## Both plots show a right tail, although the second plot clarifies that
##  there are relatively few observations above 3000.
## The distribution itself looks like a Power Lognormal distribution with
##  p ~ 9.
## However, even with this distribution, the values above 6000 are very rare.
##  Therefore, the values above 6000 are suspicious.


# 26. View summary of correct_RT.

summary(correctResponses)


# 27. There is a single very far outlier. Remove it and save the result in a new dataframe named 
# "cleaned".

## Based on summary() in the previous step, we know the exact value that we need to remove.

cleaned <- filter(correctResponses, StimulDS1.RT < 13852)



###############
### Exercise 2: Deriving sampling distributions
###############
## In this exercise, we're going to derive sampling distributions with 
## different sizes.

## a) Load the package languageR. We're going to work with the dataset 'dative'. 
## Look at the help and summary for this dataset.
#install.packages('languageR')
library(languageR)
summary(dative)
?dative


## The term dative alternation is used to refer to the alternation between 
## a prepositional indirect-object construction
## (The girl gave milk (NP) to the cat (PP)) and a double-object construction 
## (The girl gave the cat (NP) milk (NP)).
## The variable 'LenghtOfTheme' codes the number of words comprising the theme.

## b) Create a contingency table of 'LenghtOfTheme' using table(). 
##    What does this table show you?
table(dative$LengthOfTheme)
dative$LengthOfTheme
# The table gives the frequency distribution of the 'LenghtOfTheme' variable.

## c) Look at the distribution of 'LenghtOfTheme' by plotting a histogram and a boxplot. 
##    Do there appear to be outliers? Is the data skewed?
hist(dative$LengthOfTheme)
#The data is positively skewed 
boxplot(dative$LengthOfTheme)
#There are outliers as there multiple data points located outside the whiskers of the box plot 


## d) Now we're going to derive sampling distributions of means for different 
##    sample sizes. 
##    What's the difference between a distribution and a sampling distribution?
#Distribution is the general frequency and values from the entire data for that particular variable
#Sampling distribution is the distribution across several samples. 
#It includes statistics from repetition of survey for all possible samples of the population.


## e) We are going to need a random sample of the variable 'LengthOfTheme'. 
##    First create a random sample of 5 numbers using sample(). 
##    Assign the outcome to 'randomsampleoflengths'
randomsampleoflengths <- sample(dative$LengthOfTheme, size = 5)


## f) Do this again, but assign the outcome to 'randomsampleoflengths2'. 
randomsampleoflengths2 <- sample(dative$LengthOfTheme, size = 5)


## g) Now calculate the mean of both vectors, and combine these means 
##    into another vector called 'means5'.
mean(randomsampleoflengths)
mean(randomsampleoflengths2)
means5 <- c(mean(randomsampleoflengths),mean(randomsampleoflengths2))


## h) In order to draw a distribution of such a sample, we want means of 
##    1000 samples. However, we don't want to repeat question e and f 
##    1000 times. We can do this in an easier way: 
##    by using a for-loop. See dataCamp or the course books for 
##    how to write loops in R.
means5 <- vector()
for ( i in 1:1000) {
  randomsample<-sample(dative$LengthOfTheme, size = 5) 
  # here random sample of 5 numbers is chosen
  means5 <- append(means5, mean(randomsample))
  # here the mean of the random sample is calculated and stored it the vector
}


## i) Repeat the for-loop in question h, but use a sample size of 50. 
##    Assign this to 'means50' instead of 'means5'.
means50  <- vector()
for ( i in 1:1000) {
  randomsample<-sample(dative$LengthOfTheme, size = 50) 
  # here random sample of 50 numbers is chosen
  means50 <- append(means50, mean(randomsample))
}


## j) Explain in your own words what 'means5' and 'means50' now contain. 
##    How do they differ?
#means5 and means50 contain the sampling distribution of the mean. 
#lower sample size giving means5 and a higher, generally used sample size of 50 giving means50


## k) Look at the histograms for means5 and means50. Set the number of breaks to 15.
##    Does means5 have a positive or negative skew?
hist(means5, breaks = 15)
hist(means50, breaks = 15)
#means5 has positive skew
#while, means50 is almost normal

## l) What causes this skew? In other words, why does means5 have bigger 
##    maximum numbers than means50?
#Very low sample size such as 5, can lead to bias in selection 
#and the mean obtained is similar to individual frequency rather than general distribution.
#Since the complete data frequency distribution is positively skewed, same trend is seen.


###############
### Exercise 3: Confidence interval
###############

## A confidence interval is a range of values that is likely to contain an 
## unknown population parameter.
## The population parameter is what we're trying to find out. 
## Navarro discusses this in more depth in chapter 10.

## a) What does a confidence interval mean from the perspective of experiment replication?
##Degree of certainty in our estimates,it also says how stable the estimate is

## b) Let's calculate the confidence interval for our means from the previous 
##    exercise.
##    First, install and load the packages 'lsr' and 'sciplot'
library(lsr)
library(sciplot)

## c) Look at the description of the function ciMean to see which arguments it takes.
?ciMean

## d) Use ciMean to calculate the confidence interval of the dataset dative from
##    the previous exercise.
##    Also calculate the mean for the variable LengthOfTheme.
ciMean(dative)
conf.Lot<-ciMean(dative$LengthOfTheme)

## e) Does the mean of the sample fall within the obtained interval? 
##    What does this mean?
#YES
# no:if mean does not conclude the interval, we say that particular value is not true population mean.But here YES:Yet 
#we cannot conclude  that the population mean equals that specific value.

## f) As the description of dative mentions, the dataset describes the 
##    realization of the dative as NP or PP in two corpora.
##    The dative case is a grammatical case used in some languages 
##    (like German) to indicate the noun to which something is given.
##    This dataset shows us, among other things, how often the theme is 
##    animate (AnimacyOfTheme) and how long the theme is (LengthOfTheme).
##    Plot this using the function bargraph.CI(). Look at the help for this function. 
##    Use the arguments 'x.factor' and 'response'.
bargraph.CI(x.factor=conf.Lot$LengthOfTheme ,response=,conf.Lot$AnimacyofTheme, data=data)


## g) Expand the plot from question f with the ci.fun argument 
##    (this argument takes 'ciMean'). 
##    Why does the ci differ in this new plot compared to the previous plot?
bargraph.CI(x.factor=conf.Lot$LengthOfTheme ,response=,conf.Lot$AnimacyofTheme, data=data,ci.fun = ciMean)
###because ci.fun returns a vector of length 2 defining the lower and upper limit of CI. Defaults to the mean +/- standard error with NA values 
#removed.


