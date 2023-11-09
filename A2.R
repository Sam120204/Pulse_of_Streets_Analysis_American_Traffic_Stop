mydata <- read.csv("D:/桌面/2A Term/STAT 231/R Code/stat231f23dataset20995558.csv")

#q1
chicago <- subset(mydata, subset = (city == "chicago"))
female_data <- subset(chicago, subject.sex == "female")
Fc <- nrow(female_data)
n <- nrow(chicago)
thetahat_c <- Fc / n
thetahat_c

sf <- subset(mydata, subset = (city == "sf"))
female_data_sf <- subset(chicago, subject.sex == "female")
Fc_sf <- nrow(female_data_sf)
n <- nrow(sf)
thetahat_c_sf <- Fc_sf / n
thetahat_c_sf

BinomRLF <- function(theta, n, x, thetahat) {
  L_theta = dbinom(x, size = n, prob = theta)
  L_thetahat = dbinom(x, size = n, prob = thetahat)
  return (L_theta / L_thetahat)
}

theta = seq(0, 0.7, 0.01)

R_theta_c = BinomRLF(theta, n, Fc, thetahat_c)
R_theta_s = BinomRLF(theta, n, Fc_sf, thetahat_c_sf)

# Plotting for Chicago
plot(theta, R_theta_c, type = "l", col = "red", ylim = c(0, 2),
     xlab = expression(theta), ylab = expression(paste("R(", theta, ")")),
     main = "Relative Likelihood Functions for Binomial Model in Chicago", lwd = 2, las = 1)

# Plotting for sf
plot(theta, R_theta_s, type = "l", col = "red", ylim = c(0, 1.2),
     xlab = expression(theta), ylab = expression(paste("R(", theta, ")")),
     main = "Relative Likelihood Functions for Binomial Model in San Francisco", lwd = 2, las = 1)


#q2
sample_size <- nrow(mydata)
sample_size
mean_age <- mean(mydata$subject.age)
mean_age
median_age <- median(mydata$subject.age)
median_age
sd_age <- sd(mydata$subject.age)
sd_age


library(MASS)

truehist(mydata$subject.age, main = "Relative Frequency Histogram of subject.age with Exponential PDF", 
         xlab = "subject.age", ylab = "Density", col = "grey")


lambda <- 1/mean(mydata$subject.age)

curve(dexp(x, rate = lambda), from = 0, to = max(mydata$subject.age), add = TRUE, col = "blue", lwd = 2)


plot(ecdf(mydata$subject.age), main = "Empirical CDF of subject.age with Exponential CDF", 
     xlab = "subject.age", ylab = "Probability", xlim = c(0, max(mydata$subject.age)), verticals = TRUE, do.points = FALSE)

lambda <- 1/mean(mydata$subject.age)

curve(pexp(x, rate = lambda), from = 0, to = max(mydata$subject.age), add = TRUE, col = "blue", lwd = 2)


qqnorm(mydata$subject.age, pch = 1, cex = 0.5)
qqline(mydata$subject.age, col = "red", lwd = 2, lty = 2)

#q3
mle <- mean(mydata$subject.age)
probability_30_or_younger <- ppois(30, lambda = mle)
probability_30_or_younger

PoisRLF <- function(theta, n, thetahat) {
  exp(n * thetahat * log(theta/thetahat) + n * (thetahat -
                                                  theta))
}
n <- length(mydata$subject.age)
thetahat <- mean(mydata$subject.age)
R_39_value <- PoisRLF(39, n, thetahat)
R_39_value

#q4
chicago <- subset(mydata, subset = (city == "chicago"))
table(chicago$vehicle.make)


observed <- c(chevrolet = 60, ford = 43, honda = 35, toyota = 49, other = 256)
s <- sum(observed)
observed


vehicle_makes <- c("Chevrolet", "Ford", "Honda", "Toyota", "Other")
observed_counts <- c(60, 43, 35, 49, 256)


total_vehicles <- sum(observed_counts)

expected_proportions <- c(Chevrolet = 0.101, Ford = 0.128, Honda = 0.093, Toyota = 0.138, Other = 0.54)

expected <- expected_proportions * total_vehicles
expected

barplot(
  height = data_matrix, 
  beside = TRUE,
  main = "Observed vs Expected Frequencies for vehicle make in Chicago",
  xlab = "Vehicle Make",
  ylab = "Frequency",
  col = c("orange", "darkred"),
  names.arg = vehicle_makes,
  ylim = c(0, max(c(observed, expected)) + 55),
  legend.text = c("Observed", "Expected"),
  args.legend = list(x = "topright", bty = "n")
)

chicago$vehicle.make <- factor(chicago$vehicle.make, levels = c("chevrolet", "ford", "honda", "toyota", "other"))
boxplot(subject.age ~ vehicle.make, data = chicago, 
        order = c("Chevrolet", "Ford", "Honda", "Toyota", "Other"),
        main = "Distribution of Subject Age by Vehicle Make in Chicago",
        xlab = "Vehicle Make",
        ylab = "Subject Age",
        col = c("dodgerblue3", "forestgreen", "orange", "red", "yellow"))





