library(car)
library(ggplot2)
library(sandwich)
library(lmtest)

load("twoyear.RData")
desc

#Question1
#Analyze some of the variables

# ??Do we need to clean any vars??
data$mrate.clean = data$mrate*100
data$mrate.clean[data$mrate.clean>100]=NA
summary(data$mrate.clean)

summary(data$jc)
print(quantile(data$jc, probs = c(.01, .05, .10, .25, .50, .75, .90, .95, .99, 1)))
hist(data$jc, 50)

summary(data$univ)
print(quantile(data$univ, probs = c(.01, .05, .10, .25, .50, .75, .90, .95, .99, 1)))
hist(data$univ, 50)

summary(data$exper)
print(quantile(data$exper, probs = c(.01, .05, .10, .25, .50, .75, .90, .95, .99, 1)))
hist(data$exper, 50)

summary(data$black)
print(quantile(data$black, probs = c(.01, .05, .10, .25, .50, .75, .90, .95, .99, 1)))
hist(data$black, 50)

summary(data$hispanic)
print(quantile(data$hispanic, probs = c(.01, .05, .10, .25, .50, .75, .90, .95, .99, 1)))
hist(data$hispanic, 50)

summary(data$AA)
print(quantile(data$AA, probs = c(.01, .05, .10, .25, .50, .75, .90, .95, .99, 1)))
hist(data$AA, 50)

summary(data$BA)
print(quantile(data$BA, probs = c(.01, .05, .10, .25, .50, .75, .90, .95, .99, 1)))
hist(data$BA, 50)

#Question 2
#Interpret the coefficients B4 (black) and B8 (exper*black).
data$experXblack = data$exper*data$black
ols.lwage.8ind = lm(lwage~jc+univ+exper+black+hispanic+AA+BA+experXblack, data=data)
summary(ols.lwage.8ind)

#Question3
# ??are all the assumptions met to do this?  what are the std err like?
# With this model, test that the return to university education is 7%.
# H0:  B2=.07
# t = (B2-H0)/(se/sqrt(n-k-1))
(.0733 - .07) / ((.0031) / sqrt(6763-8-1))
# t = 87.48483

#Question 4
#With this model, test that the return to junior college education is equal for black
#and non-black.

#Question 5
#With this model, test whether the return to university education is equal to the
#return to 1 year of working experience.

#Question 6
#Test the overall signiffcance of this regression.
#Residual standard error: 0.4287 on 6754 degrees of freedom
#Multiple R-squared:  0.2282,	Adjusted R-squared:  0.2272
#F-statistic: 249.6 on 8 and 6754 DF,  p-value: < 2.2e-16
R=sqrt(0.2282)
R
#1. Our model null hypothesis is that there is no relationship among any of the independent variables and
#lwage variable. We are able to reject the null hypothesis since our p-value of the f-statistic of the
#model is significant at < 2.2e-16.
#2. Practical significance: we have an R-squared value of 0.2282, indicating that 22.82% of the variation in
#lwage is explained by our model. An R value of 0.478 indicates a ?? effect size.

#Question 7
#Including a square term of working experience to the regression model built above,
#estimate the linear regression model again. What is the estimated return to work
#experience in this model?
data$experXexper = data$exper*data$exper
ols.lwage.9ind = lm(lwage~jc+univ+exper+black+hispanic+AA+BA+experXblack+experXexper, data=data)
summary(ols.lwage.9ind)

#Question 8
#Provide the diagnosis of the homoskedasticity assumption. Does this assumption
#hold? If so, how does it affect the testing of no effect of university education on
#salary change? If not, what potential remedies are available?
plot(ols.lwage.9ind)

#Beush-Pagan test is not run because of large sample size
#Based on the violation of homoskedasticity, we must run robus standard errors.
coeftest(ols.lwage.9ind, vcov=vcovHC)

#??not sure if we need to run this
waldtest(ols.lwage.9ind, vcov=vcovHC)
