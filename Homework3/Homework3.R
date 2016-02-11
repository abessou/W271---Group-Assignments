library(car)
library(ggplot2)
library(sandwich)
library(lmtest)

load("401k_w271.RData")
desc

#Question1
summary(data$prate)
print(quantile(data$prate, probs = c(.01, .05, .10, .25, .50, .75, .90, .95, .99, 1)))

# Plot the histogram of bwght at 15 bins
prate.hist <- ggplot(data, aes(prate)) + 
  theme(legend.position = "none") + 
  geom_histogram(fill = "Blue", colour = "Black", binwidth = (range(data$prate)[2] - range(data$prate)[1])/15) +
  labs(title = "Distribution of Prate", x = "Participation Rate", y = "Frequency")

plot(prate.hist)

# Plot the histogram of bwght at 30 bins
prate.hist <- ggplot(data, aes(prate)) + 
  theme(legend.position = "none") + 
  geom_histogram(fill = "Blue", colour = "Black", binwidth = (range(data$prate)[2] - range(data$prate)[1])/30) +
  labs(title = "Distribution of Prate", x = "Participation Rate", y = "Frequency")

plot(prate.hist)

# Plot the histogram of bwght at 60 bins
prate.hist <- ggplot(data, aes(prate)) + 
  theme(legend.position = "none") + 
  geom_histogram(fill = "Blue", colour = "Black", binwidth = (range(data$prate)[2] - range(data$prate)[1])/60) +
  labs(title = "Distribution of Prate", x = "Participation Rate", y = "Frequency")

plot(prate.hist)

#The variable has higher frequency at higher values of participation rates with a particularly large
#spike at 100% participation, indicating that most companies have all employees participating in the 401k.
#There also seem to be erroneous values of 200% which we will code as NA.

data$prate.clean = data$prate
data$prate.clean[data$prate.clean>100]=NA
summary(data$prate.clean)

#Question 2
summary(data$mrate)
print(quantile(data$mrate, probs = c(.01, .05, .10, .25, .50, .75, .90, .95, .99, 1)))

# Plot the histogram of bwght at 15 bins
mrate.hist <- ggplot(data, aes(mrate)) + 
  theme(legend.position = "none") + 
  geom_histogram(fill = "Blue", colour = "Black", binwidth = (range(data$mrate)[2] - range(data$mrate)[1])/15) +
  labs(title = "Distribution of mrate", x = "Matching Rate", y = "Frequency")

plot(mrate.hist)

# Plot the histogram of bwght at 30 bins
mrate.hist <- ggplot(data, aes(mrate)) + 
  theme(legend.position = "none") + 
  geom_histogram(fill = "Blue", colour = "Black", binwidth = (range(data$mrate)[2] - range(data$mrate)[1])/30) +
  labs(title = "Distribution of mrate", x = "Matching Rate", y = "Frequency")

plot(mrate.hist)

# Plot the histogram of bwght at 60 bins
mrate.hist <- ggplot(data, aes(mrate)) + 
  theme(legend.position = "none") + 
  geom_histogram(fill = "Blue", colour = "Black", binwidth = (range(data$mrate)[2] - range(data$mrate)[1])/60) +
  labs(title = "Distribution of mrate", x = "Matching Rate", y = "Frequency")

plot(mrate.hist)

#mrate is heavily positively skewed, with most companies matching between 30% and 83%.
#Though the variable has a mean of 73%, the median here is a better measure of central tendency.

#We will ignore variables above 100% as they are not practical real matching values for corporation.
#We will also do an arithmetic transformation and multiply the values by 100 in order to keep it consistent with the prate variable.

data$mrate.clean = data$mrate*100
data$mrate.clean[data$mrate.clean>100]=NA
summary(data$mrate.clean)

#Question3
# Create a scatterplot of prate vs mrate
scatter.prate.mrate <- ggplot(data, aes(prate.clean, mrate.clean)) +
  geom_point(colour = "Blue", position = "jitter") +
  geom_smooth(method = "lm", colour = "Red") +
  labs(x = "401k Participation Rate", 
       y = "Employer Matching Rate", 
       title = "Participation Rate Vs Matching Rate")
plot(scatter.prate.mrate)

scatter.prate.mrate <- ggplot(data, aes(prate.clean, mrate)) +
  geom_point(colour = "Blue", position = "jitter") +
  geom_smooth(method = "lm", colour = "Red") +
  labs(x = "401k Participation Rate", 
       y = "Employer Matching Rate", 
       title = "Participation Rate Vs Matching Rate")
plot(scatter.prate.mrate)

model = lm(prate.clean~mrate.clean, data = data)
summary(model)
#We get a slope coefficient of 0.17


#Question 4
#From residuals vs fitted vals, it seems like there is a very small violation of zero-cond mean, as we see the smoothing curve slope downwards slightly.
#Nonetheless, since we have a large sample size, we do not need this assumption since we can use the assumption of exogeneity.


#Question 5
plot(model)
#There seems to be a violation of homoskedasticity, albeit not too large
#1 - We can see from the residuals vs fitted plot that the variance seems to narrow as we move to higher fitted values.
#2 - The same story is told by the scale-location plot where we see that there is a downward trend in the standardized residuals band,
#where there should be a horizontal band if homoskedasticity was met.
#We do not look at the Breusch Pagan test since we have a large number of observations.
#The implication of heteroskedasticity in the data is that it might cause our standard error to become biased.
#This may lead to our estimate not being BLUE, or a false negative in the hypothesis test.

#Question 6
#Plotting Histogram of errors
errors.hist <- ggplot(model, aes(model$residuals)) + 
  theme(legend.position = "none") + 
  geom_histogram(fill = "Blue", colour = "Black", binwidth = (range(model$residuals)[2] - range(model$residuals)[1])/60) +
  labs(title = "Distribution of Errors", x = "Errors", y = "Frequency")
plot(errors.hist)
#From inspection of plots, we can see a violation of normality
#1 - When we plot the histogram of errors, we see the negative skew.
#2 - The negative skew is also apparent in the Q-Q plot of the standardized residuals.
#We do not conduct the Shapiro Wilk test because knowing that we have a very large sample size, we know almost certainly that we will obtain significance.

#In terms of implications, despite non-normality from the plots, we can use OLS asymptotics to keep our assumption.
#Since there is a version of the central limit theorem that tells us that the sampling distribution of coefficient estimates
#approaches normality with large sample sizes, we do not need the normality assumption of our error.
#Therefore, the finding that our errors do not follow a normal distribution does not have much of an impact on our regression.

#Question 7
#Based on the violation of homoskedasticity, we must run robus standard errors.
coeftest(model, vcov=vcovHC)


#Question 8
waldtest(model, vcov=vcovHC)
#Stat signif, pract signif