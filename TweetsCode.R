
### Assignment 1 R Tutorial code

### 1. Importing the Data
## read in the tweets dataset
tweets <- read.csv('tweets.csv')

## look at first five entries for day.of.week
tweets$day.of.week[1:5]

### 2. Frequency Tables

## summarize day.of.week
table(tweets$day.of.week)

## reorder levels of day.of.week factor
tweets$day.of.week <- factor(tweets$day.of.week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
## check reordering has worked
table(tweets$day.of.week)

### 3. Bar Plots

## bar plot of day.of.week
barplot(table(tweets$day.of.week),
              xlab = "Day of Week", ylab = "Frequency", las = 1,
              main = "Bar plot of day.of.week", 
              col = c("red", "orange", "yellow", "forestgreen", "dodgerblue3", "darkorchid2", "darkorchid4"),
              ylim = c(0, 200), density = 25, angle = c(30, 60, 90, 120, 150, 180, 210), cex.names = 0.75)

### 4. Summarizing Across a Binary Variate**

## summarize day.of.week and media.binary
table(tweets$day.of.week, tweets$media.binary)

## grouped bar plot
barplot(table(tweets$media.binary, tweets$day.of.week), beside = T,
              xlab = "Day of Week", ylab = "Frequency", las = 1,
              main = "Bar plot of day.of.week and media.binary", 
              col = c("forestgreen", "dodgerblue3"),
              ylim = c(0, 100), density = 50, angle = c(45, 135))
legend('topright', legend = c("No media", "Media"), fill = c("forestgreen", "dodgerblue3"), density = 50, angle = c(45, 135))

### 5. Calculations with Tables**

## finding row/column proportions
# create mytable for easier reference
mytable <- table(tweets$day.of.week, tweets$media.binary)
prop.table(mytable, 1)
prop.table(mytable, 2)

### 6. Numerical Summaries for Continuous Variates**

## numerical summaries of time.of.day
mean(tweets$time.of.day)
sd(tweets$time.of.day)

## create time.of.day.hour variate
tweets$time.of.day.hour <- tweets$time.of.day/3600
## numerical summaries of time.of.day.hour
mean(tweets$time.of.day.hour)
sd(tweets$time.of.day.hour)

## store numerical summaries to use later
time.mean <- mean(tweets$time.of.day.hour)
time.sd <- sd(tweets$time.of.day.hour)

## summarize time.of.day.hour using summary()
summary(tweets$time.of.day.hour)

## define skewness and kurtosis functions
skewness <- function(x) {(sum((x - mean(x))^3)/length(x))/(sum((x - mean(x))^2)/length(x))^(3/2)}
kurtosis <- function(x) {(sum((x - mean(x))^4)/length(x))/(sum((x - mean(x))^2)/length(x))^2}

## find sample skewness and kurtosis of time.of.day.hour
skewness(tweets$time.of.day.hour)
kurtosis(tweets$time.of.day.hour)


### 7. Relative Frequency Histogram**

## load MASS package
library(MASS)

## relative frequency histogram with Gaussian pdf
truehist(tweets$time.of.day.hour, xlab = "Publication time (hours after midnight)",
           ylab = "Relative Frequency",
           main = "Relative frequency histogram of publication time", 
           xlim = c(0, 25), ylim = c(0, 0.1), las = 1, col = "dodgerblue3", density = 25, angle = 45)
## add pdf: note use of stored time.mean and time.sd values!
curve(dnorm(x, time.mean, time.sd), col = "red", add = TRUE, lwd = 1.5)

## changing the number of bins using nbins
truehist(tweets$time.of.day.hour, xlab = "Publication time (hours after midnight)",
           ylab = "Relative Frequency",
           main = "Relative frequency histogram of publication time", 
           xlim = c(0, 25), ylim = c(0, 0.2), las = 1, col = "dodgerblue3", density = 25, angle = 45, nbins = 100)
curve(dnorm(x, time.mean, time.sd), col = "red", add = TRUE, lwd = 1.5)
### 8. Empirical Cumulative Distribution Function Plot**

## plot ecdf with cdf curve
plot(ecdf(tweets$time.of.day.hour), xlab = "Publication time (hours after midnight)",
     main = "e.c.d.f. of publication time",
     las = 1, lwd = 2, pch = NA)
curve(pnorm(x, time.mean, time.sd), col = "red", add = TRUE, lwd = 1.5, lty = 2)

## sample quantile
quantile(tweets$time.of.day.hour, prob = 0.5)

### 9. Scatterplots

## scatterplot of likes and retweets
plot(tweets$likes, tweets$retweets, xlab = "Likes", ylab = "Retweets",
     main = "Likes and Retweets", pch = 1, cex = 0.5, col = "navy", las = 1, cex.axis = 0.5)

## create subtweets dataset, excluding tweets with 40000+ retweets
subtweets <- subset(tweets, subset = (retweets < 40000))
## plot subset with points excluded
plot(subtweets$likes, subtweets$retweets, xlab = "Likes", ylab = "Retweets",
     main = "Likes and Retweets (Subset)", pch = 1, cex = 0.5, col = "navy", las = 1, cex.axis = 0.5)

## create log-transformed variates of likes and reweets
tweets$likes.log <- log(tweets$likes + 1)
tweets$retweets.log <- log(tweets$retweets + 1)

## plot the transformed variates
plot(tweets$likes.log, tweets$retweets.log, xlab = "likes.log", ylab = "retweets.log",
     main = "Likes and Retweets (log-transformed)", pch = 1, cex = 0.5, col = "navy", las = 1)

## sample correlation of likes vs. retweets, and log-transformed likes vs. retweets
cor(tweets$likes, tweets$retweets)
cor(tweets$likes.log, tweets$retweets.log)
