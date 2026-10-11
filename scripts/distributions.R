source("scripts/distribution_generators.R")

distributions_1 <- list(

  # 1. Standard Normal: mean = 0, standard deviation = 1
  normal = function(m) {
    rnorm(m, mean = 0, sd = 1)
  },

  # 2. Continuous Uniform: from 0 to 1
  uniform = function(m) {
    runif(n = m, min = 0, max = 1)
  },

  # 3. Six-sided die: values 1 through 6
  die = function(m) {
    sample(x = 1:6, size = m, replace = TRUE)
  }

)

distributions_2 <- list(
  # 4. Exponential: rate lambda = 1
  exponential = function(m) {
    rexp(m, rate = 1)
  }
)

distributions_3 <- list (
  # 5. Poisson: lambda = 1
  poisson = function(m) {
    rpois(m, lambda = 1)
  },

  # 6. Small Binomial: n = 5, p = 0.10
  binomial_small = function(m) {
    rbinom(m, size = 5, prob = 0.10)
  },

  # 7. Large Binomial: n = 20, p = 0.50
  binomial_large = function(m) {
    rbinom(m, size = 20, prob = 0.50)
  },

  # 9. Non-identical Bernoulli observations w/ each observation has a different probability p
  nonidentical_bernoulli = function(m) {
    p_values <- seq(0.5, 0.9, length.out = m)
    rbinom(m, size = 1, prob = p_values)
  }
)

distributions_4 <- list (
  # 8. Cauchy: location = 0, scale = 1
  cauchy = function(m) {
    rcauchy(m, location = 0, scale = 1)
  }
)

distributions_5 <- list (
  #9. Dependent distribution
  rdependent = function(n) {
    sample(
      x = dependent_distribution$failure,
      size = n,
      replace = TRUE,
      prob = dependent_distribution$failure_rate
    )
  }
)


distributions_6 <- list (
  #10. Basketball - Non-Identically Distributed Observations
  rnonidentical = function(n) {
    replicate(n, game_simulation())
  }
)

#11. A Custom Age-at-Death Distribution
distributions_7 <- list (
    rdeath = function(n) {
      sample(
        x = death_distribution$age,
        size = n,
        replace = TRUE,
        prob = death_distribution$probability
      )
    }
)
  
