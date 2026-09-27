#DISTRIBUTIONS
i <- 10000
# 1. Standard Normal: mean = 0, standard deviation = 1
normal_data <- rnorm(i, mean = 0, sd = 1)

# 2. Continuous Uniform: from 0 to 1
uniform_data <- runif(i = N_obs, min = 0, max = 1)

# 3. Six-sided die: values 1 through 6
die_data <- sample(x = 1:6, size = i, replace = TRUE)

# 4. Exponential: rate lambda = 1
exponential_data <- rexp(i, rate = 1)

# 5. Poisson: lambda = 1
poisson_data <- rpois(i, lambda = 1)

# 6. Small Binomial: n = 5, p = 0.10
binomial_small_data <- rbinom(i, size = 5, prob = 0.10)

# 7. Large Binomial: n = 20, p = 0.50
binomial_large_data <- rbinom(i, size = 20, prob = 0.50)

# 8. Cauchy: location = 0, scale = 1
cauchy_data <- rcauchy(i, location = 0, scale = 1)

# 9. Non-identical Bernoulli observations w/ each observation has a different probability p
p_values <- seq(from = 0.5, to = 0.9, length.out = i)
nonidentical_bernoulli_data <- rbinom(i, size = 1, prob = p_values)


#===============================================================================
#GRAPHICAL TESTS
#1. Histogram

#2. Q-Q plot (quantile-quantile plot)
qqnorm(x, main = paste("Q-Q Plot: n ="))

#3. P-P Plots (Probability-Probability Plots)

#4 Convergence of mean, median, skewness -> 0, and kurtosis -> 3


#===============================================================================
#STATISTICAL TESTS
#1. Shapiro-Wilk test: small to medium samples (<a few hundred observations)
#---> R actually only accepts up to 5000 so skipped

#1. Kolmogorov-Smirnov test: Not as reliable as 1, for quick general test only

#2. Anderson-Darling test: KS test but better check for tail deviation, best with larger samples, where skewness and kurtosis estimates are reliable.

#4. D'Agostino-Pearson test: Check for skewness and kutosis

#5. Mean = median = mode

#6a. Fisher-Pearson Skewness = E[(X - mean)^3] / sd^3 -> 0
#6b. SE = sqrt(6/N)
#6c. z = Skewness / SE

#7a Kurtosis = E[(X - mean)^4] / sd^4 -> 3
#7b. SE = sqrt(24/N)
#7c. z = Kurtosis / SE





