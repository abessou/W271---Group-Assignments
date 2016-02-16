library(car)
library(ggplot2)
library(sandwich)
library(lmtest)

load("twoyear.RData")
desc

#Question1
#Analyze some of the variables

summary(data$lwage)
print(quantile(data$lwage, probs = c(.01, .05, .10, .25, .50, .75, .90, .95, .99, 1)))
hist(data$lwage, 50, ylim=c(0,350))

summary(data$jc)
print(quantile(data$jc, probs = c(.01, .05, .10, .25, .50, .75, .90, .95, .99, 1)))
hist(data$jc, 50)

summary(data$univ)
print(quantile(data$univ, probs = c(.01, .05, .10, .25, .50, .75, .90, .95, .99, 1)))
hist(data$univ, 50, xlim=c(0,8))

summary(data$exper)
print(quantile(data$exper, probs = c(.01, .05, .10, .25, .50, .75, .90, .95, .99, 1)))
hist(data$exper, 50, xlim=c(0,200))

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
hist(data$BA, 50, ylim=c(0,5000))

#Question 2
#Interpret the coefficients B4 (black) and B8 (exper*black).
data$experXblack = data$exper*data$black
ols.lwage.8ind = lm(lwage~jc+univ+exper+black+hispanic+AA+BA+experXblack, data=data)
summary(ols.lwage.8ind)
plot(ols.lwage.8ind)
#zero-conditional mean seems to be met
#homoskedasticity seems to be met
#assuming random sample
#assuming linear relationship

#Question3
# ??are all the assumptions met to do this?  what are the std err like?
# With this model, test that the return to university education is 7%.
# H0:  B2=.07
# H1:  B2!=.07 (two-tailed)
# t = (B2-H0)/(se)
(.0733 - .07) / (.0031)
2*(1-.8554)
# t = 1.064516, p-value=2*(1-.8554)=0.2892
# can't reject the null at 0.05% level
             
#Question 4
#With this model, test that the return to junior college education is equal for black
#and non-black.
# could just say that black var is not stat significant, so there is no impact on jc
data$jcXblack = data$jc*data$black
ols.lwage.jcblack.diff = lm(lwage~jc+univ+exper+black+jcXblack+hispanic+AA+BA+experXblack, data=data)
summary(ols.lwage.jcblack.diff)
ols.lwage.black0 = lm(lwage~jc+univ+exper+hispanic+AA+BA, data=data)
summary(ols.lwage.black0)
# jc slope when black = 0 is 0.0661197,  H0:  b1=0.0661197
summary(ols.lwage.8ind)
# t = (B2-H0)/(se)
(0.0637926 - 0.0661197) / (0.0079034)
# t = -0.2944429
# can't reject the null at 0.05% level

#Question 5
#With this model, test whether the return to university education is equal to the
#return to 1 year (or 12 months?) of working experience.
# or do we need to scale everything by 12??
data$experYr = data$exper/12
data$univ_plus_experYr = data$univ + data$experYr
summary(data$univ_plus_experYr)
hist(data$univ_plus_experYr, breaks=50, xlim=c(0,25))
# don't forget to take out the exper var otherwise it's a perfect linear combination
ols.lwage.univ.experYr = lm(lwage~jc+univ+univ_plus_experYr+black+hispanic+AA+BA+experXblack, data=data)
summary(ols.lwage.univ.experYr)
#univ represents the difference between univ and exper12.
#univ coeff is significant, so we can reject the null hypothesis that they are the same.

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
summary(ols.lwage.8ind)

#Question 7
#Including a square term of working experience to the regression model built above,
#estimate the linear regression model again. What is the estimated return to work
#experience in this model?
data$experXexper = data$exper*data$exper
ols.lwage.9ind = lm(lwage~jc+univ+exper+black+hispanic+AA+BA+experXblack+experXexper, data=data)
summary(ols.lwage.9ind)
#dlwage/dexper = (.004301 - .001239*black + 2*.000003379*exper)
#dwage = exp((.004301 - .001239*black + 2*.000003379*exper))??
#exper coeff = 4.301e-03= .004301
#black coeff = .001239
#experXexper coeff = 000003379

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
