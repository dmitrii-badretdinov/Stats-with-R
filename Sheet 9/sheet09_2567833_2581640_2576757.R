##########################
#Week 11: Model Families and Logistic Regression
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Monday. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are allowed to work together in group up to three students


## Please write below your (and your teammates) name, matriculation number. 
## Name: Farzaneh Khojasteh
## Matriculation number: 2567833
## Name: Meghana Srinath
## Matriculation number: 2581640
## Name: Dmitrii Badretdinov
## Matriculation number: 2576757

## Change the name of the file by adding your matriculation numbers
## (exercise0N_firstID_secondID_thirdID.R)

######################################################################################################################


####
#Part 1
####
# The folder speed.dating
# contains data from an experiment on a few hundred students that randomly assigned
# each participant to 10 short dates with participants of the opposite sex
# (Fisman et al., 2006). For each date, each person recorded several subjective
# numerical ratings of the other person (attractiveness, compatibility, and some 
# other characteristics) and also wrote down whether he or she would like to meet
# the other person again. Label and rij1, . . . , rij6 as person i’s numerical ratings of person j 
# on the dimensions of attractiveness, compatibility, and so forth.



#(1) Fit a classical logistic regression predicting Pr(yij = 1) given person i's 
#    ratings of person j. For ratings, use the features attr, sinc, intel, fun; see the documentation for what exactly these
#    abbreviations stand for.
#    Also, please plot the data in order to inspect it, and discuss the importance of attractiveness, compatibility, and so 
#    forth in this predictive model.


#(2) Expand this model to allow varying intercepts for the persons making the
#    evaluation; that is, some people are more likely than others to want to meet
#    someone again. Discuss the fitted model. 

#(3) Expand further to allow varying intercepts for the persons being rated. Discuss
#    the fitted model.Interpret the model outcome and explain what the varying Intercepts are.

#(4) Now fit some models that allow the coefficients for attractiveness, compatibility, and the 
#    other attributes to vary by person.  Fit a multilevel model, allowing the intercept and the 
#    coefficients for the 6 ratings to vary by the rater i. (Hint: The model will not converge when you 
#    include many predictors as random slopes; see with how many predictors you can get the model to converge;
#    and try out some of the tricks we have seen to see whether they affect convergence for this dataset.)


#(5) compare the output for the different models that you calculated - did the model design affect your conclusions?


####
#Part 2
####

# In this example, num_awards is the outcome variable and indicates the number of awards earned by students at
# a high school in a year, math is a continuous predictor variable and represents students' scores on their 
# math final exam, and prog is a categorical predictor variable with three levels indicating the type of program 
# in which the students were enrolled. It is coded as 1 = "General", 2 = "Academic" and 3 = "Vocational". 
# Let's start with loading the data and looking at some descriptive statistics.

p = read.csv("poisson_sim.csv", sep=";")
p <- within(p, {
  prog <- factor(prog, levels=1:3, labels=c("General", "Academic", "Vocational"))
  id <- factor(ï..id) #The name of 1st column was loaded diffently hence the correction
  #id <- factor(id)
})
summary(p)


#(6) Plot the data to see whether program type and math final exam score seem to affect the number of awards.
boxplot(num_awards~prog, data = p)
#program type seems to have some relation on number of awards 
#as Academic shows to have higher number of awards compared to the other two types
ggplot(p,aes(x=math, y=num_awards))+
  geom_smooth(method="lm") +
  geom_point()
#there appears to be a trend of increased awards with increase in math score

#merging both we have,
ggplot(p,aes(x=math, y=num_awards, color=prog))+
  geom_smooth(method="lm") +
  geom_point()

#(7) what model family is used and explain the reason for using it.
#Run a generalized linear model to test for significance of effects.
hist(p$num_awards)
# It is positively skewed
mod_all <- glm(num_awards ~ math * prog, data = p, family = poisson)
# The predicter variables: math and prog are independent of each other as the data is collected from different students
# prog variable is categorical and math variable is continuous
#qqnorm(mod_gaussian$residuals)
#qqline(mod_gaussian$residuals)
# residuals are not normally distributed hence it cannot use gaussian
# hence the most ideal model would be gamma 

mod_math <- lm(num_awards ~ math, data = p)
mod_prog <- lm(num_awards ~ prog, data = p)

#(8) Do model comparisons to find out whether the predictors significantly improve model fit.
anova(mod_math,mod_all)
anova(mod_prog,mod_all)
AIC(mod_all,mod_math,mod_prog)
# The 2 predictors do significantly improve the model fit

#(9) Compare to a model that uses a gaussian distribution (normal lm model) for this data.
mod_gaussian <- lm(num_awards ~ math * prog, data = p)
anova(mod_gaussian,mod_all)
AIC(mod_all,mod_gaussian)
# In this case chosen poisson model seems to suit the data better than normal with lower AIC value of 377.15

##Task 3

## Please explain within and between subject experimental design.
##How does the design affect the random effect structure during analysis.
