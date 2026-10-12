source("scripts/distribution_generators.R")

# 1. Standard Normal: mean = 0, standard deviation = 1
dist_normal <- list(
  normal = function(m) {
    rnorm(m, mean = 0, sd = 1)
  }
)


# 2. Continuous Uniform: from 0 to 1
dist_uniform <- list(
  uniform = function(m) {
    runif(n = m, min = 0, max = 1)
  }
)


# 3. Six-sided die: values 1 through 6
dist_die <-list(
  die = function(m) {
    sample(x = 1:6, size = m, replace = TRUE)
  }
)


# 4. Exponential: rate lambda = 1
dist_exponential <- list(
  exponential = function(m) {
    rexp(m, rate = 1)
  }
)


# 5. Poisson: lambda = 1
dist_poisson <- list (
  poisson = function(m) {
    rpois(m, lambda = 1)
  }
)


# 6. Small Binomial: n = 5, p = 0.10
dist_binom_small <- list (
  binomial_small = function(m) {
    rbinom(m, size = 5, prob = 0.10)
  }
)


# 7. Large Binomial: n = 20, p = 0.50
dist_binom_large <- list (
  binomial_large = function(m) {
    rbinom(m, size = 20, prob = 0.50)
  }
)


# 8. Cauchy: location = 0, scale = 1
dist_cauchy <- list (
  cauchy = function(m) {
    rcauchy(m, location = 0, scale = 1)
  }
)


#9. Machine Failure: Dependent distribution
dist_dependent <- list(
  rdependent = function(n) {
    
    # Result array
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
    if (n >= 2) {
      for (t in 2:n) {
        failure_rate[t] <- baseline_rate *
          exp(beta * (failure_time[t - 1] - reference_time))
        
        failure_time[t] <- rexp(
          1,
          rate = failure_rate[t]
        )
      }
    }
    
    # Return the observations to be averaged
    return(failure_time)
  }
)


# 10. Basketball: Non-identical Bernoulli distributed observations
dist_nonidentical <- list(
  rnonidentical = function(n) {
    
    shot_results <- matrix(
      rbinom(
        n = n * n_shots,
        size = 1,
        prob = rep(p, times = n)
      ),
      nrow = n_shots,
      ncol = n
    )
    
    colSums(shot_results)
  }
)

#11. A Custom Age-at-Death Distribution
dist_death <- list (
    rdeath = function(n) {
      sample(
        x = death_distribution$age,
        size = n,
        replace = TRUE,
        prob = death_distribution$probability
      )
    }
)


# #9. Dependent distribution
# dist_dependent <- list (
#   rdependent = function(n) {
#     sample(
#       x = dependent_distribution$failure,
#       size = n,
#       replace = TRUE,
#       prob = dependent_distribution$failure_rate
#     )
#   }
# )


# distributions_6 <- list (
#   #10. Basketball - Non-Identically Distributed Observations
#   rnonidentical = function(n) {
#     replicate(n, game_simulation())
#   }
# )


# # 10. Non-identical Bernoulli observations w/ each observation has a different probability p
# nonidentical_bernoulli = function(m) {
#   p_values <- seq(0.5, 0.9, length.out = m)
#   rbinom(m, size = 1, prob = p_values)
# }
