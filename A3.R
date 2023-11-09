mydata <- read.csv("D:/桌面/2A Term/STAT 231/R Code/stat231f23dataset20995558.csv")

#q1
mle <- mean(mydata$subject.age)
sample_size <- length(mydata$subject.age)
sample_size

# Define the Poisson relative likelihood function
PoissonRLF <- function(lambda, n, lambda_hat) {
  ((lambda/lambda_hat)^(n*lambda_hat)) * exp(n*(lambda_hat - lambda))
}


lambda_hat <- mle
lambda_hat
n <- sample_size

# Create a sequence of lambda values around your MLE for plotting
lambda_seq <- seq(from = 36.5, to = 38.5, by = 0.001)

# Plot the relative likelihood function
plot(lambda_seq, PoissonRLF(lambda_seq, n, lambda_hat), main = "Poission relative likelihood function",
     xlab = expression(lambda), ylab = expression(paste("R(", lambda, ")")), type = "l",
     lwd = 2, las = 1)
abline(h = 0.15, col = "red", lwd = 2)


#c
uniroot(function(x) PoissonRLF(x, n, lambda_hat) - 0.15, lower = 36.7, upper = 37.3)$root
uniroot(function(x) PoissonRLF(x, n, lambda_hat) - 0.15, lower = 37.5, upper = 38)$root

#d
lambda_hat <- mean(mydata$subject.age)
n <- length(mydata$subject.age)
z_15 <- qnorm((1 + 0.15)/2)
z_90 <- qnorm((1 + 0.9)/2)
z_95 <- qnorm((1 + 0.95)/2)

CI_15 <- c(lambda_hat - z_15 * sqrt(lambda_hat/n), lambda_hat + z_15 * sqrt(lambda_hat/n))
CI_90 <- c(lambda_hat - z_90 * sqrt(lambda_hat/n), lambda_hat + z_90 * sqrt(lambda_hat/n))
CI_95 <- c(lambda_hat - z_95 * sqrt(lambda_hat/n), lambda_hat + z_95 * sqrt(lambda_hat/n))
CI_15
CI_90
CI_95

q <- pchisq(-2 * log(0.15), 1)
a <- qnorm((1 + q)/2)
lambda_hat - a * lambda_hat/sqrt(n)
lambda_hat + a * lambda_hat/sqrt(n)

#Q2
chicago <- subset(mydata, subset = (city == "chicago"))
n <- length(chicago$subject.sex)
n
female_stops <- sum(chicago$subject.sex == 'female')
theta_hat <- female_stops / n
female_stops
theta_hat

ExpRLF <- function(theta, n, y, thetahat) {
  (theta/thetahat)^y * ((1-theta)/(1-thetahat))^(n-y)
}
theta_hat <- female_stops/n
lower_bound <- uniroot(function(x) ExpRLF(x, n, female_stops, theta_hat) - 0.05, lower = 0, upper = theta_hat)$root
upper_bound <- uniroot(function(x) ExpRLF(x, n, female_stops, theta_hat) - 0.05, lower = theta_hat, upper = 1)$root
lower_bound
upper_bound


#2d
n <- length(chicago$subject.sex)
theta_hat <- female_stops / n
sd_binomial <- sqrt(theta_hat * (1 - theta_hat) * n)
q <- pchisq(-2 * log(0.95), 1)
q
a <- qnorm((1+q)/2)
theta_hat - a * sd_binomial/sqrt(n)
theta_hat + a * sd_binomial/sqrt(n)



#q3
chicago <- subset(mydata, subset = (city == "chicago"))
summary(chicago$lat)
quantile(chicago$lat, probs = c(0.025, 0.975))
mean(chicago$lat)
quantile(chicago$lng, probs = c(0.025, 0.975))
mean(chicago$lng)


#c
# Calculate q for 95% confidence (0.05 significance level)
mean_lat <- mean(chicago$lat)
sd_lat <- sd(chicago$lat)
n <- length(chicago$lat)

# Compute 95% confidence interval for latitude
CI_lower_lat <- mean_lat - 1.96 * (sd_lat / sqrt(n))
CI_upper_lat <- mean_lat + 1.96 * (sd_lat / sqrt(n))
c(CI_lower_lat, CI_upper_lat)



mean_lng <- mean(chicago$lng)
sd_lng <- sd(chicago$lng)
n <- length(chicago$lng)
CI_lower_lng <- mean_lng - 1.96 * (sd_lng / sqrt(n))
CI_upper_lng <- mean_lng + 1.96 * (sd_lng / sqrt(n))
c(CI_lower_lng, CI_upper_lng)


#f
a <- qnorm((1 + 0.95)/2)
sigma_t_hat <- sd(chicago$lat)
CI_sigma_t_lower <- sigma_t_hat - a * sigma_t_hat/sqrt(n)
CI_sigma_t_upper <- sigma_t_hat + a * sigma_t_hat/sqrt(n)
CI_sigma_t_lower
CI_sigma_t_upper

a <- qnorm((1 + 0.95)/2)
sigma_g_hat <- sd(chicago$lng)
CI_sigma_g_lower <- sigma_g_hat - a * sigma_g_hat/sqrt(n)
CI_sigma_g_upper <- sigma_g_hat + a * sigma_g_hat/sqrt(n)
CI_sigma_g_lower
CI_sigma_g_upper

#f
s2 <- var(chicago$lat)
df = n - 1
a <- 0.05
lower <- qchisq(a / 2, df)
upper <- qchisq(1 - a / 2, df)
variance_lower <- df * s2 / upper
variance_upper <- df * s2 / lower
sd_lower <- sqrt(variance_lower)
sd_upper <- sqrt(variance_upper)
sd_lower
sd_upper
