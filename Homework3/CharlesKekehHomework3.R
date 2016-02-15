library(car)
library(ggplot2)
library(sandwich)
library(lmtest)

load(file.path("./Data/", "twoyear.RData"))
desc

#Question1
summary(data$lwage)
print(quantile(data$lwage, probs = c(.01, .05, .10, .25, .50, .75, .90, .95, .99, 1)))

# Plot the histogram of lwage at 15 bins
lwage.hist <- ggplot(data, aes(lwage)) + 
    theme(legend.position = "none") + 
    geom_histogram(fill = "Blue", colour = "Black", binwidth = (range(data$lwage)[2] - range(data$lwage)[1])/15) +
    labs(title = "Distribution of lwage", x = "Log of wage", y = "Frequency")

plot(lwage.hist)

# Plot the histogram of lwage at 30 bins
lwage.hist <- ggplot(data, aes(lwage)) + 
    theme(legend.position = "none") + 
    geom_histogram(fill = "Blue", colour = "Black", binwidth = (range(data$lwage)[2] - range(data$lwage)[1])/30) +
    labs(title = "Distribution of lwage", x = "Log of wage", y = "Frequency")

plot(lwage.hist)

# Plot the histogram of lwage at 60 bins
lwage.hist <- ggplot(data, aes(lwage)) + 
    theme(legend.position = "none") + 
    geom_histogram(fill = "Blue", colour = "Black", binwidth = (range(data$lwage)[2] - range(data$lwage)[1])/60) +
    labs(title = "Distribution of lwage", x = "Log of wage", y = "Frequency")

plot(lwage.hist)

# The log of wage appears to be close to normally distributed. There are no concerning 
# outlier points in the data from visual inspection of the different histograms plotted.

summary(data$univ)
print(quantile(data$univ, probs = c(.01, .05, .10, .25, .50, .75, .90, .95, .99, 1)))

# Plot the histogram of univ at 15 bins
univ.hist <- ggplot(data, aes(univ)) + 
    theme(legend.position = "none") + 
    geom_histogram(fill = "Blue", colour = "Black", binwidth = (range(data$univ)[2] - range(data$univ)[1])/15) +
    labs(title = "Distribution of univ", x = "University Years", y = "Frequency")

plot(univ.hist)

# Plot the histogram of univ at 30 bins
univ.hist <- ggplot(data, aes(univ)) + 
    theme(legend.position = "none") + 
    geom_histogram(fill = "Blue", colour = "Black", binwidth = (range(data$univ)[2] - range(data$univ)[1])/30) +
    labs(title = "Distribution of univ", x = "University Years", y = "Frequency")

plot(univ.hist)

# Plot the histogram of univ at 60 bins
univ.hist <- ggplot(data, aes(univ)) + 
    theme(legend.position = "none") + 
    geom_histogram(fill = "Blue", colour = "Black", binwidth = (range(data$univ)[2] - range(data$univ)[1])/60) +
    labs(title = "Distribution of univ", x = "University Years", y = "Frequency")

plot(univ.hist)

# The distribution of years at university appear to be very positively skewed with the
# majority of individuals having zero year of university education. There are no signs
# of bad data in the set for this variable.

summary(data$jc)
print(quantile(data$jc, probs = c(.01, .05, .10, .25, .50, .75, .90, .95, .99, 1)))

# Plot the histogram of exper at 15 bins
jc.hist <- ggplot(data, aes(jc)) + 
    theme(legend.position = "none") + 
    geom_histogram(fill = "Blue", colour = "Black", binwidth = (range(data$jc)[2] - range(data$jc)[1])/15) +
    labs(title = "Distribution of jc", x = "Junior College Years", y = "Frequency")

plot(jc.hist)

# Plot the histogram of jc at 30 bins
jc.hist <- ggplot(data, aes(jc)) + 
    theme(legend.position = "none") + 
    geom_histogram(fill = "Blue", colour = "Black", binwidth = (range(data$jc)[2] - range(data$jc)[1])/30) +
    labs(title = "Distribution of jc", x = "Junior College Years", y = "Frequency")

plot(jc.hist)

# Plot the histogram of jc at 60 bins
jc.hist <- ggplot(data, aes(jc)) + 
    theme(legend.position = "none") + 
    geom_histogram(fill = "Blue", colour = "Black", binwidth = (range(data$jc)[2] - range(data$jc)[1])/60) +
    labs(title = "Distribution of jc", x = "Junior College Years", y = "Frequency")

plot(jc.hist)

# The distribution of junior college years shows the same characteristics as the university
# years one. It's is positively skewed with the majority of individuals having zero
# years of junior college. There are no signs of bad data in the set for this variable.

summary(data$exper)
print(quantile(data$exper, probs = c(.01, .05, .10, .25, .50, .75, .90, .95, .99, 1)))

# Plot the histogram of exper at 15 bins
exper.hist <- ggplot(data, aes(exper)) + 
    theme(legend.position = "none") + 
    geom_histogram(fill = "Blue", colour = "Black", binwidth = (range(data$exper)[2] - range(data$exper)[1])/15) +
    labs(title = "Distribution of exper", x = "Work Experience", y = "Frequency")

plot(exper.hist)

# Plot the histogram of exper at 30 bins
exper.hist <- ggplot(data, aes(exper)) + 
    theme(legend.position = "none") + 
    geom_histogram(fill = "Blue", colour = "Black", binwidth = (range(data$exper)[2] - range(data$exper)[1])/30) +
    labs(title = "Distribution of exper", x = "Work Experience", y = "Frequency")

plot(exper.hist)

# Plot the histogram of exper at 60 bins
exper.hist <- ggplot(data, aes(exper)) + 
    theme(legend.position = "none") + 
    geom_histogram(fill = "Blue", colour = "Black", binwidth = (range(data$exper)[2] - range(data$exper)[1])/60) +
    labs(title = "Distribution of exper", x = "Work Experience", y = "Frequency")

plot(exper.hist)

# The distribution of years of experience is negatively skewed with a long tail 
# of individuals showing little work experience and a larger proportion showing larger
# work experience.

#Question2
# The $\beta_{4}$ coefficient captures the effect of being black on the log of wage, 
# holding all other variables in the model fix. It provides an indication of how much
# percentage point change to expect in the wage of an individual when they move from 
# the baseline (white) to being black. Hence the coefficient captures the change at the
# intersect of the wage vs experience plot between black and white respondants.

# The $\beta_{8}$ coefficient captures the effect of being black on the impact of 
# experience over the years on wage.
# It is better explained in terms of derivatives with:
# $\frac{\delta log(wage)}{\delta exper} = \beta_{3} + \beta_{8}*black$
# In the previous formulation, we can see that the $\beta_{8}$ coefficient captures the
# impact of being back on the slope of the wage vs experience curve. In other words, the
# coefficient describes how much more or less experience impacts the log of wage for
# back people over the years, vs the baseline (while people).
# And because the outcome variable is the log oif wage, the impact above can actually be 
# formulated in terms of impact of ethnicity on percentage point changes on the actual 
# wage of individuals.

#Question3
data$exper.by.black <- data$exper * data$black
lwage.model = lm(lwage ~ jc + univ + exper + black + hispanic + AA + BA + exper.by.black, data = data)
summary(lwage.model)

# We note that the F-statistic for the test is highly significant and that the model 
# explains close to 23% of the variance in the log of wage variable.
# We validate from the coefficients in this model that the value of the parameter 
# $\beta_{2}$ is actual .0732, indicating that for every additional year of university
# the log of wage of an individual increases by about 7%. Meaning that the wage itself
# increases approximatively by about 7% of a percentage point.
# We note that the coeffcient is significant from the t-statistic, indicating that we
# must reject the null hypothesis that $\beta_{1} is zero.

# We perform a t-test for the mean of teh return to university education with the null
# hypothesis that the mean is .007
# Our p value is:
print (2* (1- pt ((lwage.exper.squared.model$coefficients["univ"] - 0.07)/
        coef(summary(lwage.exper.squared.model))[, "Std. Error"]["univ"], 
        df = summary(lwage.exper.squared.model)$df[1])))
# Can't reject the null at 0.05% level

#Question4
# Holding university years and experience equal (dropping them from the model), the 
# model becomes: 
# $log(wage) = \beta_{0} + \beta_{1}*jc + \beta_{4}*black + \beta_{5}*hispanic + \beta_{6}*AA + \beta_{7}*BA + \epsilon$
# From the linear model, we derive:
# $\frac{\delta log(wage)}{\delta jc} = \beta_{1}$
# The model is specified in a way that the return to junior college education, 
# is $\beta_{1}$, and is independent of ethnicity.
# Therefore without additional computation, we can immediately answer that the return
# on junior college education is the same for all ethinicities, 

#Question5
data$BA.minus.year.exper = data$BA - data$exper
lwage.BA.minus.year.exper.model = lm(lwage ~ BA.minus.year.exper, data = data)
summary(lwage.BA.minus.year.exper.model)

# We want to test that all other factors being held equal, the return to university 
# education (BA obtained) equals that of 1 year of experience. In order to test this 
# condition, we hold all other conditions (ethnicity, years in junior college) equal 
# by dropping them from the model and testing the significance of the coefficient of
# a new variable that deducts the years of experience from the obtention of a university
# degree. If the returns are equal, then we would expect the coefficient on this new 
# variable to be null.

# The test shows that the p-value for the coefficient of the new variable is highly 
# significant, indicating that we cannot accept the null hypothesis and that the 
# returns are different for university and an additional year of experience.

#Question6
# The F statistic for this regression is highly significant above 0.05.
# The R squared for this statistic indicates that less than 9% of the variance in the
# model is explained by this regression. However, the practical significance of this 
# regression is not intuitive. The regression was used as a tool to test a condition 
# about the coefficients of a previous regression and cannot necessarily be interpreted
# practically beyond the hypothesis testing that it allowed.

#Question7
data$exper.squared = data$exper^2
data$exper.by.year = data$exper/12.0
lwage.exper.squared.model = lm(lwage ~ jc + univ + exper.by.year + black + hispanic + AA + BA + exper.by.black + exper.squared, data = data)
summary(lwage.model)

# The new model can be described as:
# $log(wage) = \beta_{0}+\beta_{1}jc+\beta_{2}univ+\beta_{3}exper+\beta_{4}black+\beta_{5}hispanic+\beta_{6}AA+\beta_{7}BA+\beta_{8}exper*black+\beta_{9}exper^{2}$
# We obtain the return on a year of experience by evaluating
# $\frac{\delta lwage.model}{exper} = \beta_{3} +\beta_{8}black+2*\beta_{9}exper$

# From a regression of this model on our data, we have:
# $\beta_{3} = 5.161e-02$
# $\beta_{8} = -1.239e-03$
# $\beta_{9} = 3.379e-06
# Thus for blacks, the return to work experience is: 5 percentage points of wage increase
# for every additional year of experience, where as the same return is 5.2 percentage 
# points of wage increase for non-black individuals.

# Question8
#plot(lwage.exper.squared.model)

# The plots residual vs fittel values plot and the scale-location plot show a very small
# amount of heterosketdasticity of the residuals.
# We conclude that robust methods should be used to evaluate the variance of the 
# $\beta_{j}$ coefficients and test the significance of teh coefficients.

# To determine that there is no effect of university education on salary changes, we 
# must test that the coefficient $\beta_{2}$ is null in the linear model. Our null 
# hypothesis is that it it. Our alternative is that it is not.
# Assuming heteroskedasticity, we run robust coefficient tests on the model to obtain 
# the corrected variables and statistics:
coeftest(lwage.exper.squared.model, vcov=vcovHC)

# The coefficient is highly significant and we conclude that we must reject the null
# hypothesis that it's value is null.
