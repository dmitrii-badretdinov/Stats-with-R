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

## Loading required libraries.

package_list <- c("lme4", "ggplot2")
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

## Reading data

data <- read.csv('Speed Dating Data.csv')

## The fields attr, sinc, intel, and fun represent the person's opinion on their partner's
##  characteristics after the date.
##  attr:  attractiveness,
##  sinc:  sincerity,
##  intel: intelligence,
##  fun:   whether they were fun.

## As the response variable "match" is binary, its error distribution is binomial.
##  Therefore, we will use glm() function with the binomial family.

model_glm <- glm(match ~ attr + sinc + intel + fun, family = "binomial", data)
summary(model_glm)

## Based on the probabilities shown in the "Pr(>|z|)" column, we see that "sinc" and "intel"
##  do not influence the result significantly due to p >> 0.05.
##  At the same time, "attr" and "fun" have clean influence on the result.

## Drawing model predictions together with the data:
## In our understanding, one can get information about the features by comparing 
##  the models fit for one feature at a time.

range(data$attr, na.rm = TRUE)
range(data$sinc, na.rm = TRUE)
range(data$intel, na.rm = TRUE)
range(data$fun, na.rm = TRUE)

model_glm_attr <-glm(match ~ attr, family = "binomial", data)
model_glm_sinc <-glm(match ~ sinc, family = "binomial", data)
model_glm_intel <-glm(match ~ intel, family = "binomial", data)
model_glm_fun <-glm(match ~ fun, family = "binomial", data)

x_all <- seq(0, 10, 0.05)
y_attr <- predict.glm(model_glm_attr, list(attr = x_all), type = "response")
plot(data$attr, data$match, pch = 16, xlab = "attr", ylab = "match")
lines(x_all, y_attr)

y_sinc <- predict.glm(model_glm_sinc, list(sinc = x_all), type = "response")
plot(data$sinc, data$match, pch = 16, xlab = "sinc", ylab = "match")
lines(x_all, y_sinc)

y_intel <- predict.glm(model_glm_intel, list(intel = x_all), type = "response")
plot(data$intel, data$match, pch = 16, xlab = "intel", ylab = "match")
lines(x_all, y_intel)

y_fun <- predict.glm(model_glm_fun, list(fun = x_all), type = "response")
plot(data$fun, data$match, pch = 16, xlab = "fun", ylab = "match")
lines(x_all, y_fun)

summary(model_glm_attr)
summary(model_glm_sinc)
summary(model_glm_intel)
summary(model_glm_fun)

## Even though the plot difference is noticeable mostly for "fun", AIC shows the
##  situation similar to the probabilities of model_glm: sinc and intel models have
##  significantly higher AIC values when compared to attr and fun.
## This fact shows that the models based on attr or fun would be a better fit.


#(2) Expand this model to allow varying intercepts for the persons making the
#    evaluation; that is, some people are more likely than others to want to meet
#    someone again. Discuss the fitted model.

model_glmer <- glmer(match ~ attr + sinc + intel + fun + (1|iid), family = "binomial", data)
summary(model_glm)
summary(model_glmer)

## model_glmer shows lower AIC when compared to model_glm, which signifies 
##  that the model_glmer is more parsimonious and fits the data that it was generated on better.

## Aside from that, the significance of fixed effects has changed noticeably.
## Intelligence is no longer insignificant as it went from p 0.3 to 0.06.
## Sincerity also increased in significance, even though not to the usually required
##  level: from 0.6 to 0.07.

## Overall, the introduction of the random effect helped us to discover the increased
##  importance of the features previously considered statistically insignificant.

#(3) Expand further to allow varying intercepts for the persons being rated. Discuss
#    the fitted model.Interpret the model outcome and explain what the varying Intercepts are.

model_glmerer <- glmer(match ~ attr + sinc + intel + fun + (1|iid) + (1|pid), family = "binomial", data)
summary(model_glmer)
summary(model_glmerer)

## Judging by AIC, the introduction of the random intercept to the person being rated has
##  improved the model's ability to predict. However, the AIC change is not as drastic as
##  it was when comparing model_glm and model_glmer: the AIC difference has dropped
##  from 7% to 2%.

## It was also said that the recent model has failed to converge with the 
##  default max|grad| method and had to use the Nelder_Mead one
## The situation of non-convergence with the default method is slightly concerning.
## One of the likely reasons why this happened is because we use only random
##  intercepts and not slopes. This practice was disapproved in Lecture 9 page 17.  

## Answering the last question of (3), according to Lecture 9 page 18, 
##  the varying Intercepts are the consideration of the differences in
##  how, in our case, subjects should be evaluated due to their natural variability.
## With the different baseline for each subject, it's possible to filter out
##  the intrinsic differences and focus on the actual insights that one wants to discover.


#(4) Now fit some models that allow the coefficients for attractiveness, compatibility, and the 
#    other attributes to vary by person.  Fit a multilevel model, allowing the intercept and the 
#    coefficients for the 6 ratings to vary by the rater i. (Hint: The model will not converge when you 
#    include many predictors as random slopes; see with how many predictors you can get the model to converge;
#    and try out some of the tricks we have seen to see whether they affect convergence for this dataset.)

## The trick that was mentioned in Lecture 9 was to base the random effect structure on how the data was acquired.
## The trick from Lecture 11 was to create proxy variables and construct a model based on them.

## As promised, the following model did not converge after several minutes:
##  model_glmerest <- glmer(match ~ attr + sinc + intel + fun + amb + shar + 
##   (1+attr|iid) + (1+sinc|iid) + (1+intel|iid) + (1+fun|iid) + (1+amb|iid) + (1+shar|iid), family = "binomial", data)
## "Warning messages:
## 1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
##  unable to evaluate scaled gradient
## 2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
##  Model failed to converge: degenerate  Hessian with 1 negative eigenvalues"

## From the previous models, we got insight that sinc and intel don't affect
##  the result much. Let's check it with random slopes and intercepts when compared
##  to the definitely significant "fun".

model_glmer_sinc <- glmer(match ~ sinc + (1+sinc|iid), family = "binomial", data)
model_glmer_fun <- glmer(match ~ fun + (1+fun|iid), family = "binomial", data)

summary(model_glmer_sinc)
## AIC 6896; BIC 6931

summary(model_glmer_fun)
## AIC 6359; BIC 6394

## Fun still wins significantly in terms of the impact.

## Let's compare the AICs and BICs of two new contenders:

model_glmer_amb <- glmer(match ~ amb + (1+amb|iid), family = "binomial", data)
model_glmer_shar <- glmer(match ~ shar + (1+shar|iid), family = "binomial", data)

summary(model_glmer_amb)
## Failed to converge. Likely minuscule impact.

summary(model_glmer_shar)
## AIC 5901; BIC 5936; Lower than "fun". We take this feature to the final model.

## Checking attr just in case we miss something:
model_glmer_attr <- glmer(match ~ attr + (1+attr|iid), family = "binomial", data)

summary(model_glmer_attr)
## AIC 6480; BIC 6515; Worse than "fun" but will do.

## Constructing the model with the most impactful features: shar, fun, and attr.
##  Will add other fixed effects back if this model converges.
## model_glmerest <- glmer(match ~ attr + fun + shar + (1+attr|iid) + (1+fun|iid) + (1+shar|iid), family = "binomial", data)

## It didn't converge.

## Checking the basic one with at least random shar.
model_glmerest <- glmer(match ~ attr + fun + shar + amb + sinc + intel + (1+shar|iid), family = "binomial", data)

## Didn't converge

summary(model_glmerest)
## Still, it's AIC 5345; 
## Sincerity is not important as always.
## Ambitiousness is surprisingly significant.

## Ideally, we'd want something like the thing below, but it doesn't converge.
## model_glmerest <- glmer(match ~ attr + fun + shar + amb + intel + (1|iid) + (1+shar|iid) + (1+shar|iid), family = "binomial", data)
## We'd want to know most about the two most influential features and their randomness 
##  while also considering fixed effects of other features, not forgetting about the important basic subject randomness. 

#(5) compare the output for the different models that you calculated - did the model design affect your conclusions?

## The outputs, that is AIC and BIC, were compared in (4) when choosing the model.

## Regarding the design affecting the conclusions:
## Based on our research while doing the task, the model design is very important. 
## It was mentioned in many papers and is specific to different fields.
## Even the "keep maximal" idea was debated because it applied not to all situations.

## Therefore, the model design definitely affected our conclusions.
## Considering the start when we did not accounted for the random effects, the results
##  are drastically different in terms of feature importance, which in turn would have
##  long-lasting consequences if this model would have actually been used somewhere.


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

# The data collected is essential in understanding and deciding on method to be used for analysis and interpretation
# Within subject experimental design is when same group of individuals are used in the study for multiple observations
# or test conditions
# Between subject experimental design refers to different individuals testing or providing observations
# so each variable is unique

# The design affects the choice of method as in case of within subject design, 
# there will be random effects that we need to take into consideration.
