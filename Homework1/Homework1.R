library(ggplot2)
library(car)
library(boot)
library(pastecs)
library(car)


# Load the data from file
load.data <- function(){
    load(file.path("./data", "birthweight_w271.rdata"))
    return (data)
}

data <- load.data()

# Display summary information about the dataframe
print(str(data))
print(summary(data))
print(stat.desc(data))

############
# Question 3
############
# Get summary statistics about the birthweight variable.
print(summary(data$bwght))
print(sum(is.nan(data$bwght)))
print(quantile(data$bwght, probs = c(.1, .5, .10, .25, .50, .75, .90, .95, .99, 1)))

# Plot the histogram of bwght at 15 bins
bwght.hist <- ggplot(data, aes(bwght)) + 
    theme(legend.position = "none") + 
    geom_histogram(fill = "Blue", colour = "Black", binwidth = (range(data$bwght)[2] - range(data$bwght)[1])/15) +
    labs(title = "Distribution of birthweight", x = "Birth Weight", y = "Fequency")

plot(bwght.hist)

# Plot the histogram of bwght at 30 bins
bwght.hist <- ggplot(data, aes(bwght)) + 
    theme(legend.position = "none") + 
    geom_histogram(fill = "Blue", colour = "Black") +
    labs(title = "Distribution of birthweight", x = "Birth Weight", y = "Fequency")

plot(bwght.hist)

# Plot the histogram of bwght at 60 bins
bwght.hist <- ggplot(data, aes(bwght)) + 
    theme(legend.position = "none") + 
    geom_histogram(fill = "Blue", colour = "Black", binwidth = (range(data$bwght)[2] - range(data$bwght)[1])/60) +
    labs(title = "Distribution of birthweight", x = "Birth Weight", y = "Fequency")

plot(bwght.hist)

# Strange observations:
# These are the outlier observations. There are babies with birthweights of 0 and over 200 onces
print(sum(data$bwght == 0))
print(data$bwght[data$bwght > 200])
# We should remove the zero baby weights from the data. They probably correspond to data entry issues.
# We can probably keep the single 271 pounds baby observation in the data set.

############
# Question 4
############
# Get summary statistics about the cigarettes smoked variable.
print(summary(data$cigs))
print(sum(is.nan(data$cigs)))
print(quantile(data$cigs, probs = c(.1, .5, .10, .25, .50, .75, .90, .95, .99, 1)))

# Plot the histogram of cigs at 15 bins
cigs.hist <- ggplot(data, aes(cigs)) + 
    theme(legend.position = "none") + 
    geom_histogram(fill = "Blue", colour = "Black", binwidth = (range(data$cigs)[2] - range(data$cigs)[1])/15) +
    labs(title = "Distribution of birthweight", x = "Birth Weight", y = "Fequency")

plot(cigs.hist)

# Plot the histogram of cigs at 30 bins
cigs.hist <- ggplot(data, aes(cigs)) + 
    theme(legend.position = "none") + 
    geom_histogram(fill = "Blue", colour = "Black") +
    labs(title = "Distribution of birthweight", x = "Birth Weight", y = "Fequency")

plot(cigs.hist)

# Plot the histogram of cigs at 60 bins
cigs.hist <- ggplot(data, aes(cigs)) + 
    theme(legend.position = "none") + 
    geom_histogram(fill = "Blue", colour = "Black", binwidth = (range(data$cigs)[2] - range(data$cigs)[1])/60) +
    labs(title = "Distribution of birthweight", x = "Birth Weight", y = "Fequency")

plot(cigs.hist)

# Strange observations
# The histogram for the number of cigarettes smoked is negatively skewed with a very high proportion of
# individuals smoking zero cigarettes per day during heir pregnancy.
# There are no visible signs of other anomalies in the data.

############
# Question 5
############
# Create a scatterplot of cigarettes smoked per day vs baby birthweight
scatter.bwght.cigs <- ggplot(data, aes(cigs, bwght)) +
    geom_point(colour = "Blue", position = "jitter") +
    geom_smooth(method = "lm", colour = "Red") +
    labs(x = "Cigarettes Smoked Per Day", 
         y = "Infant Birthweight", 
         title = "Cigarettes Smoked Per Day Vs Infant Birthweight")
plot(scatter.bwght.cigs)
# Based on the scatterplot and the fitted lm curve on it, it appears that only a very small amount 
# of the variation of bwght will be explained by cigs. That's because the variation explained in the 
# graph appears to be much lower than the variation of birthweights at any level of daily cigarette 
# consumption

############
# Question 6
############
# Clean up the data based on previous observations
data <- data[data$bwght != 0, ]

# Now perform the OLS regression
simple.ols.cigs.bwght <- lm(bwght ~ cigs, data = data)
print(summary.lm(simple.ols.cigs.bwght))
plot(simple.ols.cigs.bwght)

# our null hypothesis is that there is no relationship between the cigs variable and the bwght variable.
# The p-value of the f-statistic of the model is significant at 1.711e-08, indicating that we should 
# reject the null hypothesis.
# The model accounts for 2.28% of the variability in the variable bwght
# That value is givent by a R^2 value of 0.2285 and a R value of 0.151 indicating a relatively small 
# effect size
# 
# The model shows a negative coefficient for the variable cigs with a value of -0.5147.
# The coefficents of the model are statistically significant.

############
# Question 7
############
# Obtain descriptive statistics for the new variable
print(summary(data$faminc))
print(sum(is.nan(data$faminc)))
print(quantile(data$faminc, probs = c(.1, .5, .10, .25, .50, .75, .90, .95, .99, 1)))
print(stat.desc(data$faminc, basic=FALSE, norm = TRUE))

# Plot the histogram of faminc at 30 bins
faminc.hist <- ggplot(data, aes(faminc)) + 
    theme(legend.position = "none") + 
    geom_histogram(fill = "Blue", colour = "Black") +
    labs(title = "Distribution of Family Income", x = "Birth Weight", y = "Fequency")

plot(faminc.hist)

# Produce a scatterplot of bwght, cigs and faminc
scatterplotMatrix(~ bwght + cigs + faminc, data = data)

############
# Question 8
############
#Introduce a new independent variable to the model
multiple.ols.cigs.faminc.bwght <- lm(bwght ~ cigs + faminc, data = data)
print(summary.lm(multiple.ols.cigs.faminc.bwght))
plot(multiple.ols.cigs.faminc.bwght)

# our null hypothesis is that there is no relationship between the cigs variable and the bwght and 
# faminc variables.
# The p-value of the f-statistic of the model is significant at 8.109e-10, indicating that we should 
# reject the null hypothesis.
# The model accounts for 2.99% of the variability in the variable bwght
# That value is givent by a R^2 value of 0.299 and a R value of 0.173 indicating a relatively small 
# effect size
# 
# The model shows a negative coefficient for the variable cigs with a value of -0.4640.
# It shows a positive coefficient for the variable faminc of 0.093.
# The coefficents of the model are statistically significant.

###############
# Question 9/10
###############
# The coefficient on cigs in the multiple regression means that for every additional
# cigarette smoked per day by the pregnant mother during their maternity, and leaving
# the income variable constant, the body weight of the infant children decreases by 0.4640 pound.

# In the simple regression, we saw that this coefficient was also negative and had a value
# of -0.5147.

# In the simple regression case, the coefficient on cigs meant that for every addiotional
# cigarette smoked per day, indepedent of any other condition of the mother, the birth
# weight of the infant was educed by -0.5147 lbs.

# In the multiple regression, the variance explained by the famic variable was captured in
# the residuals of the simple model and partially also in the coefficent of the cigs 
# variable as we can suspect there is some correlation between the two variables.

# Therefore the introduction of the additional variable faminc has reduced the contribution
# of the cigs variable to the infant body weight. We can hypothesis that there is some 
# correlation between the numberof cigarettes smoked per day and the family income, where
# mothers at higher family income have better health habits and therefore smoke less.
# And introducing the family income variable thus takes away some of the variance
# explanation previously captured by the cigs variable.

