
###########################################################
###########################################################

## Calculating Basic Descriptive Statistics in R

###########################################################
###########################################################


###########################################################
## Task One: Getting Started
## In this task, you will learn set and check your current
## working directory
###########################################################

## Set and get the working directory

## setwd()

## Get the working directory
getwd()

###########################################################
## Task Two: Import data set and explore
## In this task, we will import two data sets and explore them
###########################################################

## 2.1: Import the msleep.csv and mpg.csv data sets
df <- read.csv(file = "msleep.csv")
df1 <- read.csv(file = "mpg.csv")

## 2.2: View and check the dimension of the data sets
View(df)
View(df1)
dim(df)
dim(df1)

## 2.3: Take a peek at df using the head and tail functions
head(df)
head(df1)
tail(df)
tail(df1)

## 2.4: Check the internal structure of the data frame
str(df)
str(df1)

## 2.5: Count missing values in the variables
sum(is.na(df))
sum(is.na(df1))

sapply(df,function(x) sum(is.na(x)))
sapply(df1,function(x) sum(is.na(x)))

## 2.6: Check the column names for the df data frame
names(df)
names(df1)


###########################################################
## Task Three: Frequency of categorical variables
## In this task, we will learn how to count the frequency 
## and percentage of a categorical variable
###########################################################

## Let us view the df data frame

View(df)

dim(df)

## 3.1: Drop the first two columns of df
df <- df[, -c(1,2)]
dim(df)
names(df)
df1 <- df1[, -c(1)]
dim(df1)
names(df1)
# 3.2: Get the frequency of a categorical variable

str(df)
str(df1)

## Get the frequency of the feature "order"
table(df$order)
table(df1$manufacturer)

## 3.3: Calculate the percentage from the frequency
total <- length(df$order)
percentage <- (table(df$order)/total) * 100
percentage

total1 <- length(df1$manufacturer)
percentage1 <- (table(df1$manufacturer)/total1) * 100
percentage1
## Round to 2 decimal places
percentage_r <- round(percentage1, 2)
percentage_r

percentage1_r <- round(percentage1, 2)
percentage1_r

###########################################################
## Task Four: Univariate statistics for univariate variables - Part I
## In this task, we will perform different descriptive statistics
## for a quantitative variable
###########################################################

### Univariate statistics for univariate variables

## 4.1: Calculate the mean of total sleep time variable
mean(df$Total.Sleep.Time)

## 4.2: Calculate the median of total sleep time variable
median(df$Total.Sleep.Time)

## 4.3: Assign a single variable to a new variable
sleeptime <- df$Total.Sleep.Time

## 4.4: Calculate the mode of total sleep time
which.max(sleeptime)
which.max(table(sleeptime))
#### The spread of a quantitative variable

## 4.5: Get the minimum value for total sleep time
min(sleeptime)

## 4.6: Get the maximum value for total sleep time
max(sleeptime)

## 4.7: Get the range for total sleep time
range(sleeptime)

## 4.8: Get the range difference for total sleep time
diff(range(sleeptime))

## 4.9: Sort the total sleep time variable
sort(sleeptime)

## In descending order
sort(sleeptime, decreasing = TRUE)

###########################################################
## Task Five: Univariate statistics for univariate variables - Part II
## In this task, we will continue to perform different 
## descriptive statistics for a quantitative variable
###########################################################

## 5.1: Calculate the quartile of the total sleep time variable

## Returns all the quartiles
quantile(sleeptime)

## Return the 1st, 2nd and 3rd quartiles
quantile(sleeptime, c(0.23, 0.5, 0.75))

## Return the 90th percentile
quantile(sleeptime, c(0.90))

## 5.2: Calculate the inter-quartile range 
IQR(sleeptime)

## 5.3: Calculate the variance
var(sleeptime)

## 5.4: Calculate the standard deviation
sqrt(var(sleeptime))
sd(sleeptime)

## 5.5: Get the five number summary 
fivenum(sleeptime)

## 5.6: Summarize the total sleep time variable
summary(sleeptime)

###########################################################
## Task Six: Distribution of Quantitative variable
## In this task, we will learn how to check the distribution
## of a quantitative variable
###########################################################

### Analyze the spread of the quantitative variable

## 6.1: Install the e1071 package
install.packages("e1071")

## 6.2: Importing a library
library(e1071)

## 6.3: Calculate the skewness of the total sleep time variable
skewness(sleeptime)

## 6.4: Calculate the kurtosis of the total sleep time variable
kurtosis(sleeptime)

## 6.5: Plot the density of the total sleep time variable
plot(density(sleeptime))

## 6.6: Plot an histogram of the total sleep time variable
hist(sleeptime)

## 6.7: Plot the histogram using proportions or probabilities
hist(sleeptime, probability = TRUE)

## Give the histogram a title
hist(sleeptime, probability = TRUE,
     col = grey(0.9),
     main = "Histogram of Total Sleep Time",
     xlab = "Total Sleep Time")

## Add a density line on the histogram
               # superimpose a normal curve
lines(density(sleeptime), col="blue", lw=2)

###########################################################
## Task Seven: Bivariate statistics for variables
## In this task, we will learn how to perform bivariate
## statistics for both qualitative and quantitative variables
###########################################################

## 7.1: Bivariate statistics for two qualitative variables

View(df1)    ## View the data set
table(df1$class, df1$drv)


### Bivariate statistics for two quantitative variables

## 7.2: Calculate covariances
cov(df$Total.Sleep.Time, df$sleep_rem)
cov(df$awake, df$brainwt)

## 7.3: Calculate correlation coefficients
cor(df$Total.Sleep.Time, df$sleep_rem)
cor(df$awake, df$brainwt)


### Bivariate statistics for both a quantitative and qualitative variable

## 7.4: Calculate the mean total sleep time for different orders
tapply(df$Total.Sleep.Time, df$order, mean)

## 7.5: Calculate the summary of all variables in the df data frame
summary(df)
