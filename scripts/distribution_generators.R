## Dependent Exponential Process: Modeling Machine Failures ====================
n <- 1000
failure_time <- numeric(n)
failure_rate <- numeric(n)

# Model parameters
baseline_rate <- 1
beta <- 1.25
reference_time <- 1

# Generate the first operating time
failure_rate[1] <- baseline_rate
failure_time[1] <- rexp(1, rate = failure_rate[1])

# Generate the remaining dependent operating times
for (t in 2:n) {
  failure_rate[t] <- baseline_rate *
    exp(beta * (failure_time[t - 1] - reference_time))
  
  failure_time[t] <- rexp(1, rate = failure_rate[t])
}

# Creates the distribution data table
dependent_distribution <- data.frame(
  failure = 1:n,
  operating_time = failure_time,
  failure_rate = failure_rate
)


## Basketball: Non-Identically Distributed Observations ========================
# Linear sequence to decrease the probability from 0.90 at shot #1 to 0.50 at shot #20 
n_games <- 1000
n_shots <- 20
shot_number <- 1:n_shots

p <- seq(from = 0.90, to = 0.50, length.out = n_shots) 

# Create the probability data table
data.frame(
  shot = shot_number,
  probability = round(p, 3)
)

# Simulate 1 game using binomial w/ the custom probability: whether or not he made each shot in the game
game_simulation <- function() {
  made <- rbinom(n = n_shots, size = 1, prob = p)

  one_game <- data.frame(
  shot = shot_number,
  probability = round(p, 3),
  made = made
  )

  return(sum(made))              #return the total # shot made in the game
}

## Custom Age-at-Death Distribution ============================================
# Define age:
age <- 0:100

# Construct components
early_component <- 0.045 * exp(-age / 5)                     #exponential decay
adult_component <- 0.030 * exp(-0.5 * ((age - 65) / 10)^2)   #normal density curve

# Combine and normalize the components
weight <- early_component + adult_component                  #Add the two components
probability <- weight / sum(weight)                          #Convert the weights into P
percent <- 100 * probability                                 #Convert probabilities into %

sum(probability)                                             #Sanity check
sum(percent)

# Create the distribution data frame
death_distribution <- data.frame(
  age = age,
  probability = probability,
  percent = percent
)