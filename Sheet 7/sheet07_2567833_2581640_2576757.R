### Stats with R Exercise sheet 7

##########################
#Week8: Checking Assumptions underlying ANOVA and linear regression
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Monday, January 11. Write the code below the questions. 
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

###############################################################################
###############################################################################

########
### Exercise 1
########

########
### Please, use ggplot to make plots in all exercises below!
########


# a) Read in the data kidiq.txt (available in the Moodle) and take a look
#    at the data summary. It contains information about the mum's iq and 
#    their child's iq. 
#    mom_hs indicates whether the mother has a high school degree
#    1 = high school education, 0 = no high school degree.

if(!require(ggplot2))
{
  install.packages(ggplot2)
  library(ggplot2)
}

kidiq = read.csv("kidiq.txt", TRUE, ' ')
str(data)


# b) Plot kid_score against mom_iq in a scatter plot, and add a regression line 
#    (kid_score should be the response variable and mom_iq the predictor 
#    variable) to the plot. 
#    Name the plot and the axis in sensible ways.

ggplot(kidiq, aes(x = mom_iq, y = kid_score)) +
  geom_point() +
  xlab("Mom's IQ") +
  ylab("Child's IQ")


# c) State the main difference between correlation and regression. Calculate a simple regression model 
#    for kid_score with mom_hs as a predictor and interpret the results.

## Essentially, correlation cannot explain the cause and effect.
## If two features correlate, the relationship between the features remains unknown:
## it is still undefined which feature influences the other one.

model_kid_momHs = lm(kid_score ~ mom_hs, kidiq)
print(model_kid_momHs)

## Intercept: 77.55
## mom_hs: 11.77
## kid_score = 77.55 + 11.77 * mom_hs;
##
## The linear model predicts that if the mother has a high school degree,
## the IQ of their kid would be higher by roughly 12 points.

# d) Next, fit a regression model with two predictors: mom_hs and mom_iq. 
#    Interpret the model 
#    Then compare this regression model to the previous model and state which has a better model fit.

model_kid_momHs_momIq = lm(kid_score ~ mom_hs + mom_iq, kidiq)
print(model_kid_momHs_momIq)

## Intercept: 25.73
## mom_hs: 5.95       
## mom_iq: 0.56
## kid_score = 25.73 + 5.95 * mom_hs + 0.56 * mom_iq;
##
## The second model is a better fit due to considering more information.
## The first linear model had one binary feature as a predictor, therefore was
##  unable to give any precise results.
## The second model includes a numerical feature as a predictor, therefore it has
##  significantly more data to calibrate the prediction.

# e) Now plot a model where both predictors are shown. Do this by plotting 
#    data points for mothers with high school degree==1 in one color and those 
#    without degree in another color. Then also fit two separate regression lines 
#    such that these lines reflect the model results.
#	   HINT: One solution in ggplot is to calculate fitted values of the regression model 
#    and then plot them along with the original data points:
#    pred = data.frame(mom_iq=kidiq$mom_iq, mom_hs=kidiq$mom_hs, 
#    kid_score_pred=fitted(your_model))



# f) Next, we will proceed to a model including an interaction between mom_hs
#    and mom_iq. Fit the model and interpret your results.



# g) Next, let's plot the results of this model.



# h) Next, let's explore the "predict.lm" function. Please first generate
#    a new dataframe with one datapoint (a mother with high school degree
#    and iq of 100). Then, use the predict function to predict the corresponding
#    child's iq. 
#    Please specify the predict function to also give you the 0.95 confidence 
#    interval.



# i) Meaning of confidence intervals for regression line.
#    Let's go back to exercise b) and plot again the data points with the 
#    regression line. By default, there should also be displayed the borders of 
#    the confidence interval. What is the meaning of this confidence interval?



# j) Finally, do model checking on your model from f), i.e. inspect 
#    the standard model plots provided by R, and interpret what you see.


