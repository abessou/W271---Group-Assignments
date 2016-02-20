library(car)
library(ggplot2)
library(sandwich)
library(lmtest)
library(pastecs)

# Load the dataset and print descriptions
load("athletics.RData")
desc

#Question1
#==========
# How many observations and variablea are in the dataset
str(data)
# There are 116 observations of 14 variables in the data set

# apps variable
print(quantile(data$apps, probs = c(.01, .05, .10, .25, .50, .75, .90, .95, .99, 1)))

# Plot the histogram of at 15 bins
apps.hist <- ggplot(data, aes(apps)) + 
    theme(legend.position = "none") + 
    geom_histogram(fill = "Blue", colour = "Black", binwidth = (range(data$apps)[2] - range(data$apps)[1])/15) +
    labs(title = "Distribution of Applications", x = "Number of applications for admissions", y = "Frequency")

plot(apps.hist)
#The histogram shows a data distribution that's positevely skewed with most universities 
#showing between 3000 and 10000 applications for the years 1992 and 1993

# bowl variable
print(quantile(data$bowl, probs = c(.01, .05, .10, .25, .50, .75, .90, .95, .99, 1)))

# Plot the histogram of at 15 bins
bowl.hist <- ggplot(data, aes(bowl)) + 
    theme(legend.position = "none") + 
    geom_histogram(fill = "Blue", colour = "Black", binwidth = (range(data$bowl)[2] - range(data$bowl)[1])/15) +
    labs(title = "Distribution of Bowl Game Participation", x = "Bowl game participation", y = "Frequency")

plot(bowl.hist)

#The bowl variable is a binary categorical variable. We note that over the 2 year period
#considered there are less universities that appear in bowl games than universities 
#that do.

# btitle variable
print(quantile(data$btitle, probs = c(.01, .05, .10, .25, .50, .75, .90, .95, .99, 1)))

# Plot the histogram of at 15 bins
btitle.hist <- ggplot(data, aes(btitle)) + 
    theme(legend.position = "none") + 
    geom_histogram(fill = "Blue", colour = "Black", binwidth = (range(data$btitle)[2] - range(data$btitle)[1])/15) +
    labs(title = "Distribution of btitle variable", x = "Conf Champ title Won", y = "Frequency")

plot(btitle.hist)

#The btitle variable is also a binary categorical variable. The histogram, (as one would 
#expect) shows that there are significantly less universities that have been won men title 
#over the 2 year period considered than universities that have.

# finfour variable
print(quantile(data$finfour, probs = c(.01, .05, .10, .25, .50, .75, .90, .95, .99, 1)))

# Plot the histogram of at 15 bins
finfour.hist <- ggplot(data, aes(finfour)) + 
    theme(legend.position = "none") + 
    geom_histogram(fill = "Blue", colour = "Black", binwidth = (range(data$finfour)[2] - range(data$finfour)[1])/15) +
    labs(title = "Distribution of finfour variable", x = "finfour game participation", y = "Frequency")

plot(finfour.hist)

#The finfour variable is also a binary categorical variable. The histogram, (as one would
#expect) shows that there are significantly less universities that have participated in men's
#final four games over the 2 year period considered than universities that have.

#Question2
#============
reshaped.data <- reshape(data, v.names=c("apps","top25","ver500","mth500","stufac",
                                         "bowl","btitle","finfour","lapps","avg500","bball",
                                         "perf"),
                         timevar="year",idvar="school", direction="wide")
#Check the layout of the reshaped data
str(reshaped.data)

# Create the new variable for the change in the log of the number of applications
reshaped.data$chlapps <- reshaped.data$lapps.1993-reshaped.data$lapps.1992

#examine the new variable
# finfour variable
print(quantile(reshaped.data$chlapps, probs = c(.01, .05, .10, .25, .50, .75, .90, .95, .99, 1)))

# Plot the histogram of at 20 bins
chlapps.hist <- ggplot(reshaped.data, aes(chlapps)) + 
    theme(legend.position = "none") + 
    geom_histogram(fill = "Blue", colour = "Black", binwidth = (range(reshaped.data$chlapps)[2] - range(reshaped.data$chlapps)[1])/20) +
    labs(title = "Distribution of chlapps variable", x = "Change in number of log aplications", y = "Frequency")

plot(chlapps.hist)

#The distribution of the change of log of application number has the appearance of a
#normal(ish) distribution. There are 2 outlier points with a change of -.1 and +.4
#that correspond to Arizona University and Florida State University. There are however
#no indications that these outliers would affect the regression at this point.

#Which schools had the greatest increase in number of log applications
head(reshaped.data[order(reshaped.data$chlapps, decreasing= TRUE), c("school", "chlapps")])

#Which schools had the greatest decrease in number of log applications
tail(reshaped.data[order(reshaped.data$chlapps, decreasing= TRUE), c("school", "chlapps")])

#Question3
#==========
#Create the additional variables
reshaped.data$cperf <- reshaped.data$perf.1993-reshaped.data$perf.1992
print(quantile(reshaped.data$cperf, probs = c(.01, .05, .10, .25, .50, .75, .90, .95, .99, 1)))
print(stat.desc(reshaped.data$cperf))

reshaped.data$cbball <- reshaped.data$bball.1993-reshaped.data$bball.1992
print(quantile(reshaped.data$cbball, probs = c(.01, .05, .10, .25, .50, .75, .90, .95, .99, 1)))
print(stat.desc(reshaped.data$cbball))

reshaped.data$cbowl <- reshaped.data$bowl.1993-reshaped.data$bowl.1992
print(quantile(reshaped.data$cbowl, probs = c(.01, .05, .10, .25, .50, .75, .90, .95, .99, 1)))
print(stat.desc(reshaped.data$cbowl))

reshaped.data$cbtitle <- reshaped.data$btitle.1993-reshaped.data$btitle.1992
print(quantile(reshaped.data$cbtitle, probs = c(.01, .05, .10, .25, .50, .75, .90, .95, .99, 1)))
print(stat.desc(reshaped.data$cbtitle))

reshaped.data$cfinfour <- reshaped.data$finfour.1993-reshaped.data$finfour.1992
print(quantile(reshaped.data$cfinfour, probs = c(.01, .05, .10, .25, .50, .75, .90, .95, .99, 1)))
print(stat.desc(reshaped.data$cfinfour))

#cperf is the variable with the greatest variance, with a value of 0.82425892

#Question4
#==========
#Create the model.
change.lapps.model <- lm(chlapps ~ cbowl + cbtitle + cfinfour, data = reshaped.data)

#Additional assumptions needed for the model to be causal
#---------------------------------------------------------
#One important assumption needed for the model to be causal is the exogeneity of the 
#dependent variables in the model with the error term in the model. Exogeneity can be
#defined as the requirement that the dependent variables are not related to the error
#term.
#Assuming the population model presented:
#$$claps_{i}=\beta_{0} + \beta_{1}cbowl_{i} + \beta_{2}cbtitle_{i} + \beta_{3}cfinfour_{i} + a_{i} + cu_{i}$$
#Exogeneity of the variables in the model can be formulated as:
#$$Cov(cbowl_{i}, cu_{i})=Cov(cbtitle_{i}, cu_{i})=Cov(cfinfour_{i}, cu_{i})=0$$
 

#Additional assumpotion needed for OLS to consistently estimate the first-difference model
#-------------------------------------------------------------------------------------
#For OLS to consistently estimate the first difference model, we need to assume that
#assumptions MLR1-4' hold.
#We also need to assume that the model isn't affected by omitted variable bias, meaning
#that all the explanatory variables are included in the model. The presence of omitted
#variables that are correlated with any of the variables in the model creates a bias in
#the estimation of the coefficient of the variables selected in the model.

#In the  abscence of a guanrantee that we have not left unmeasured effect in the model,
#a manipulation is needed to make a control variable truly random so that we can 
#estalish the that the bias introduced by any unmeasured variables is null.

#Question5
#===========
#Estimate the model
print(summary(change.lapps.model))

#The F statistic for the model has a p-value of 0.03855, which is significant at the
#0.05 level.
#The coefficient for the intercept is 0.01684 indicating that the year over year inncease
#in application is 1.6% from 1992 to 1993.
#However, the t-statistic for that coefficient has a value of 0.1932 and is not 
#significant at the 0.05 level.
#The coefficient for the cbowl variable is .057, indicating that a win in a bowl the year
#prior, contributes to a 5.7% increase in applications the subsequent year.
#The t-statistic for the coefficient is significant at the 0.05 level, with a value of
#0.0236
#The coefficient for the cbtitle variable is .041, indicating that a win in the men's
#conference championship in the previous year, tranlates into an increase of 4.1% year
#over year from 1992 to 1993.
#The t-statistic for the coefficient is 0.1950 and is not significant at the 0.05 level.
#The coefficient for the cfinfour variable is -0.06961 and seems to indicate that an
#appearance in the men's final four the year prior is related to a decrease of 6.9% of
#applications.


#Question6
#==============
#The F statistic obtained from the model is the test of overall significance of the 
#model. We have already establish that it has a p-value of 0.03855, which is 
#significant at the 0.05 level.
#We can obtain the same statistic with the linearHypothesis function.
linearHypothesis(change.lapps.model, c("cbowl", "cbtitle", "cfinfour"))

#Because the model is significant, we do not want to read too much in the t-statistics
#of each of the coefficients of the model. We had noted that 3 out of 4 coefficients
#had t-values that were not significant at the 0.05 level.
#It appears that the combined explanatory power of the model is still relevant, 
#as evidenced by the F-statistic