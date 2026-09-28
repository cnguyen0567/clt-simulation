## Library =====================================================================
library(moments)
library(nortest)
library(fBasics)
source("scripts/distributions.R")


## Setting seeds ===============================================================
student_id <- 020810348
set.seed(student_id)


## Constants and Settings ======================================================
# Simulation parameters:
R <- 10000                    #Num of Monte Carlo repetition

# Theoretical values:
skew_norm <- 0                #skewness of normal dist = 0
kurtosis_norm <- 3            #proper kurtosis of norm dist = 3

# Stats to calculate:
stats_col = c("n",
              "Mean",
              "Median",
              "SD",
              "Skewness",
              "SE_s",
              "Z_s",
              "Kurtosis",
              "SE_k",
              "Z_k",
              "SW_W",
              "SW_p",
              "Lillie_D",
              "Lillie_p",
              "AD_A",
              "AD_p",
              "DP_Chi2",
              "DP_p"
)


## Simulation Function =========================================================
run_simulation <- function(
    r_generator,
    distribution_name,
    n,
    R,
    stats_col
) {
  
  #Data files and table: -------------------------------------------------------
  pdf_path <- file.path(
    "results",
    paste0("normality_plots_", distribution_name, ".pdf")
  )
  
  pdf(pdf_path)
  on.exit(dev.off(), add = TRUE)
  
  
  csv_path <- file.path(
    "results",
    paste0("stats_results_", distribution_name, ".csv")
  )
  
  stats <- matrix(
    NA_real_,
    nrow = length(n),
    ncol = length(stats_col),
    dimnames = list(NULL, stats_col)
  )
  
  #Monte Carlo (MC) Simulation: ------------------------------------------------
  par(mfrow = c(3, 2))
  
  for (j in seq_along(n)) {
  
    i <- n[j]
    sample_means <- replicate(
      R,
      {
        one_sample <- r_generator(i)
        
        if (length(one_sample) != i) {
          stop(
            paste(distribution_name, "generator returned", length(one_sample), "points instead of", i)
          )
        }
        mean(one_sample)
      }
    )
    
    #Calculate Simulation Statistics -------------------------------------------
    # 1 Descriptive
    mc_mean <- mean(sample_means)
    mc_median <- median(sample_means)
    mc_sd <- sd(sample_means)
    
    # 2. Skewness
    mc_skew <- moments::skewness(sample_means)
    mc_skew_se <- sqrt(6 / R)
    mc_skew_z <- (mc_skew - skew_norm) / mc_skew_se
    
    # 3. Pearson kurtosis
    mc_kurtosis <- moments::kurtosis(sample_means)
    mc_kurtosis_se <- sqrt(24 / R)
    mc_kurtosis_z <- (mc_kurtosis - kurtosis_norm) / mc_kurtosis_se
    
    # 4. Shapiro-Wilk: R accepts a maximum of 5,000 observations
    sw_data <- sample(sample_means, size = min(length(sample_means), 5000), replace = FALSE)
    shapiro_result <- stats::shapiro.test(sw_data)
    sw_stat <- unname(shapiro_result$statistic)
    sw_p <- shapiro_result$p.value
    
    # 5. Lilliefors-corrected Kolmogorov-Smirnov test
    lillie_result <- nortest::lillie.test(sample_means)
    lillie_stat <- unname(lillie_result$statistic)
    lillie_p <- lillie_result$p.value
    
    # 6. Anderson-Darling test
    ad_result <- nortest::ad.test(sample_means)
    ad_stat <- unname(ad_result$statistic)
    ad_p <- ad_result$p.value
    
    # 7. D'Agostino-Pearson omnibus test
    dp_result <- fBasics::dagoTest(sample_means)
    dp_stat <- unname(dp_result@test$statistic[1])
    dp_p <- dp_result@test$p.value[1]
    
    #Store the statistics results ----------------------------------------------
    stats[j, ] <- c(
      i,
      round(mc_mean, 5),
      round(mc_median, 5),
      round(mc_sd, 5),
      round(mc_skew, 3),
      round(mc_skew_se, 3),
      round(mc_skew_z,3),
      round(mc_kurtosis, 3),
      round(mc_kurtosis_se, 3),
      round(mc_kurtosis_z, 3),
      round(sw_stat, 5),
      sw_p,
      round(lillie_stat, 5),
      lillie_p,
      round(ad_stat, 5),
      ad_p,
      round(dp_stat, 5),
      dp_p
    )
    
    #Draw graphical results ----------------------------------------------------
    # Histogram
    hist(
      sample_means,                                          #The data
      breaks=20,                                             #Num of bins
      probability = TRUE,                                    #Density hist rather than freq hist
      col = "steelblue",                                     #Fill color
      border = "white",                                      #Border color
      main = paste("Sampling Distribution of Mean: n =", i), #Title
      xlab = "Sample mean"                                  #x-axis label
      #x-axis limit (min, max)
    )
    
    curve(dnorm(x, mean = mc_mean, sd = mc_sd), add = TRUE, col = "firebrick", lwd = 2)
    
    # Q-Q
    qqnorm(
      sample_means,
      main = paste("Q-Q Plot: n =", i)
    )
    qqline(sample_means, col = "firebrick")
    
  }
  
  # Summary of Statistics Plots ================================================
  # Reset layout  
  par(mfrow = c(3, 1))                  
  
  # Mean, Median
  matplot(                                                          #Std R plot tool
    x = stats[, "n"],                                               #--> Select the values in the "n" col for x-axis
    y = stats[, c("Mean", "Median")],                   #--> Select the stats cols for y-axis
    type = "o",                                                     #--> plot point + line
    pch = 1:2,                                                      #--> plotting char = 4 symbols for each stats
    lty = 1,                                                        #--> line type = 1 line type
    lwd = 2,                                                        #--> line width
    col = c("steelblue", "darkgreen"),                 #--> color
    xlab = "Sample size, n",
    ylab = "Statistic value",
    main = "Mean and Median versus Sample Size (n)"
  )
  legend(
    "top",
    legend = c("Mean", "Median"),
    col = c("steelblue", "darkgreen"),
    pch = 1:2,
    lty = 1,
    lwd = 2
  )
  grid()                                                            #--> turn on grid
  abline(h = 0, lty = 2, col = "gold")                              #--> draw horizontal line
  
  # Plot Skewness
  plot(
    stats[, "n"],
    stats[, "Skewness"],
    type = "o",
    pch = 3,
    lwd = 2,
    col = "firebrick",
    xlab = "Sample size, n",
    ylab = "Skewness",
    main = "Skewness versus Sample Size (n)"
  )
  grid()
  abline(h = 0, lty = 2, col = "gold")
  
  # Plot Kurtosis
  plot(
    stats[, "n"],
    stats[, "Kurtosis"],
    type = "o",
    pch = 4,
    lwd = 2,
    col = "purple",
    xlab = "Sample size, n",
    ylab = "Kurtosis",
    main = "Kurtosis versus Sample Size (n)"
  )
  grid()
  abline(h = 3, lty = 2, col = "gold")
  
  # Export the statistics table ------------------------------------------------
  write.csv(stats, file = csv_path, row.names = FALSE)
  
  invisible(stats)
  
}

# Run every distribution -------------------------------------------------------
all_stats <- list()

n <- c(
  seq(1, 60, by = 2)         #Num of sample size needed to satisfy CLT
) 

for (distribution_name in names(distributions_2)) {
  
  all_stats[[distribution_name]] <- run_simulation(
    r_generator = distributions_2[[distribution_name]],
    distribution_name = distribution_name,
    n = n,
    R = R,
    stats_col = stats_col
  )
}
