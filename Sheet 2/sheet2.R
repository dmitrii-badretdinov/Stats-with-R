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


# 2. Read in the data into a variable called "dat".


# 3. Load the libraries languageR, stringr, dplyr and tidyr.


# 4. How many rows, how many columns does that data have?


# 5. Take a look at the structure of the data frame using "glimpse".


# 6. View the first 20 rows, view the last 20 rows.


# 7. Is there any missing data in any of the columns?


# 8. Get rid of the row number column.


# 9. Put the Sub_Age column second.


# 10. Replace the values of the "ExperimentName" column with something shorter, more legible.


# 11. Keep only experimental trials (encoded as "Trial:2" in List), get rid of practice trials 
# (encoded as "Trial:1"). When you do this, assign the subset of the data to a variable "data2", 
# then assign data2 to dat and finally remove data2.


# 12. Separate Sub_Age column to two columns, "Subject" and "Age", using the function "separate".


# 13. Make subject a factor.


# 14. Extract experimental condition ("right" vs. "wrong") from the "File" column:
# i.e. we want to get rid of digit underscore before and the digit after the "right" and "wrong".



# 15. Using str_pad to make values in the File column 8 chars long, by putting 0 at the end  (i.e., 
# same number of characters, such that "1_right" should be replaced by "1_right0" etc).


# 16. Remove the column "List".


# 17. Change the data type of "Age" to integer.


# 18. Missing values, outliers:
# Do we have any NAs in the data, and if so, how many and where are they?


# 19. Create an "accuracy" column using ifelse-statement.
# If actual response (StimulDS1.RESP) is the same as the correct response (StimulDS1.CRESP), put 
# in value 1, otherwise put 0.


# 20. How many wrong answers do we have in total?


# 21. What's the percentage of wrong responses?



# 22. Create a subset "correctResponses" that only contains those data points where subjects 
# responded correctly. 



# 23. Create a boxplot of StimulDS1.RT - any outliers?


# 24. Create a histogram of StimulDS1.RT with bins set to 50.


# 25. Describe the two plots - any tails? any suspiciously large values?


# 26. View summary of correct_RT.


# 27. There is a single very far outlier. Remove it and save the result in a new dataframe named 
# "cleaned".


###############
### Exercise 2: Deriving sampling distributions
###############
## In this exercise, we're going to derive sampling distributions with 
## different sizes.

## a) Load the package languageR. We're going to work with the dataset 'dative'. 
## Look at the help and summary for this dataset.


## The term dative alternation is used to refer to the alternation between 
## a prepositional indirect-object construction
## (The girl gave milk (NP) to the cat (PP)) and a double-object construction 
## (The girl gave the cat (NP) milk (NP)).
## The variable 'LenghtOfTheme' codes the number of words comprising the theme.

## b) Create a contingency table of 'LenghtOfTheme' using table(). 
##    What does this table show you?


## c) Look at the distribution of 'LenghtOfTheme' by plotting a histogram and a boxplot. 
##    Do there appear to be outliers? Is the data skewed?


## d) Now we're going to derive sampling distributions of means for different 
##    sample sizes. 
##    What's the difference between a distribution and a sampling distribution?


## e) We are going to need a random sample of the variable 'LengthOfTheme'. 
##    First create a random sample of 5 numbers using sample(). 
##    Assign the outcome to 'randomsampleoflengths'


## f) Do this again, but assign the outcome to 'randomsampleoflengths2'. 


## g) Now calculate the mean of both vectors, and combine these means 
##    into another vector called 'means5'.


## h) In order to draw a distribution of such a sample, we want means of 
##    1000 samples. However, we don't want to repeat question e and f 
##    1000 times. We can do this in an easier way: 
##    by using a for-loop. See dataCamp or the course books for 
##    how to write loops in R.


## i) Repeat the for-loop in question h, but use a sample size of 50. 
##    Assign this to 'means50' instead of 'means5'.


## j) Explain in your own words what 'means5' and 'means50' now contain. 
##    How do they differ?


## k) Look at the histograms for means5 and means50. Set the number of breaks to 15.
##    Does means5 have a positive or negative skew?


## l) What causes this skew? In other words, why does means5 have bigger 
##    maximum numbers than means50?


###############
### Exercise 3: Confidence interval
###############

## A confidence interval is a range of values that is likely to contain an 
## unknown population parameter.
## The population parameter is what we're trying to find out. 
## Navarro discusses this in more depth in chapter 10.

## a) What does a confidence interval mean from the perspective of experiment replication?


## b) Let's calculate the confidence interval for our means from the previous 
##    exercise.
##    First, install and load the packages 'lsr' and 'sciplot'


## c) Look at the description of the function ciMean to see which arguments it takes.


## d) Use ciMean to calculate the confidence interval of the dataset dative from
##    the previous exercise.
##    Also calculate the mean for the variable LengthOfTheme.


## e) Does the mean of the sample fall within the obtained interval? 
##    What does this mean?


## f) As the description of dative mentions, the dataset describes the 
##    realization of the dative as NP or PP in two corpora.
##    The dative case is a grammatical case used in some languages 
##    (like German) to indicate the noun to which something is given.
##    This dataset shows us, among other things, how often the theme is 
##    animate (AnimacyOfTheme) and how long the theme is (LengthOfTheme).
##    Plot this using the function bargraph.CI(). Look at the help for this function. 
##    Use the arguments 'x.factor' and 'response'.


## g) Expand the plot from question f with the ci.fun argument 
##    (this argument takes 'ciMean'). 
##    Why does the ci differ in this new plot compared to the previous plot?