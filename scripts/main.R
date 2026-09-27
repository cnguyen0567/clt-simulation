#Library =======================================================================
install.packages(c("moments", "nortest", "fBasics"))
library(moments)
library(nortest)
library(fBasics)


#Setting seeds =================================================================
student_id <- 020810348
set.seed(student_id)

#Constants =====================================================================
R <- 10000                    #Number of Monte Carlo repetition
t1 <- 10^(-12)                #Tolerance 1
t1 <- 0.5                     #Tolerance 2

mu = 0                        #mean = 0
skew_norm <-0                 #skewness of normal dist = 0
kurtosis_norm <- 3            #kurtosis of norm dist = 0


pdf("normality_plots_rnorm.pdf")    #Start a pdf to save the plots: 3row x 2 col layout
par(mfrow = c(3, 2))   

#Simulation ====================================================================
# Parameters: number of sample size needed to satisfy CLT
n = seq(1,30, by=1)

#Statistic table to store the data
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
stats <- matrix(         
  NA,
  nrow = length(n),
  ncol = length(stats_col),
  dimnames = list(NULL, stats_col)
  )

# Monte Carlo (MC) Simulation
for (j in seq_along(n)) {
  
  # Simulation
  i <- n[j]
  sample_means <- replicate(R, mean(rnorm(i, mean = 0, sd = 1)))
  
  # Calculate Simulation Statistics
  ## 1. Mean Median SD
  mc_mean = mean(sample_means)
  mc_median = median(sample_means)
  mc_sd = sd(sample_means)
  
  ## 2. Skewness
  mc_skew <- skewness(sample_means)
  mc_skew_se <- sqrt(6/R)
  mc_skew_z <- (mc_skew - skew_norm) / mc_skew_se
  
  ## 3. Kurtosis
  mc_kurtosis <- kurtosis(sample_means)
  mc_kurtosis_se <- sqrt(24/R)
  mc_kurtosis_z <- (mc_kurtosis - kurtosis_norm) / mc_kurtosis_se
  
  ## 4. Shapiro-Wilk test: only accepts up to 5,000 observations, so cut off data
  sw_data <- sample_means[seq_len(min(length(sample_means), 5000))]   #grab the 1st 5000 data
  shapiro_result <- stats::shapiro.test(sw_data)
  sw_stat <- unname(shapiro_result$statistic)
  sw_p <- shapiro_result$p.value
  
  ## 5. Lilliefors-corrected Kolmogorov-Smirnov test
  lillie_result <- nortest::lillie.test(sample_means)
  lillie_stat <- unname(lillie_result$statistic)
  lillie_p <- lillie_result$p.value
  
  ## 6. Anderson-Darling test
  ad_result <- nortest::ad.test(sample_means)
  ad_stat <- unname(ad_result$statistic)
  ad_p <- ad_result$p.value
  
  ## 7. D'Agostino-Pearson omnibus normality test
  ## The first statistic and p-value are the omnibus results.
  dp_result <- fBasics::dagoTest(sample_means)
  dp_stat <- unname(dp_result@test$statistic[1])
  dp_p <- dp_result@test$p.value[1]
  
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
  
  # Graphic: Histogram
  hist(
    sample_means,                                          #The data
    breaks=50,                                             #Num of bins
    probability = TRUE,                                    #Density hist rather than freq hist
    col = "steelblue",                                     #Fill color
    border = "white",                                      #Border color
    main = paste("Sampling Distribution of Mean: n =", i), #Title
    xlab = "Sample mean",                                  #x-axis label
    xlim = c(-2,2)                                         #x-axis limit (min, max)
  )

  curve(dnorm(x, mean = mc_mean, sd = mc_sd), add = TRUE, col = "firebrick", lwd = 2)
  
  # Graphic: Q-Q
  qqnorm(
    sample_means,
    main = paste("Q-Q Plot: n =", i)
    )
  qqline(sample_means, col = "firebrick")
  
}

#Graphic: Mean, Median, Skewness, Kurtosis =====================================
# Reset layout  
par(mfrow = c(2, 1))                  

# Plot Mean, Median, Skewness
matplot(                                                          #Std R plot tool
  x = stats[, "n"],                                               #--> Select the values in the "n" col for x-axis
  y = stats[, c("Mean", "Median", "Skewness")],                   #--> Select the stats cols for y-axis
  type = "o",                                                     #--> plot point + line
  pch = 1:3,                                                      #--> plotting char = 4 symbols for each stats
  lty = 1,                                                        #--> line type = 1 line type
  lwd = 2,                                                        #--> line width
  col = c("steelblue", "darkgreen", "firebrick"),                 #--> color
  xlab = "Sample size, n",
  ylab = "Statistic value",
  main = "Mean, Median, and Skewness versus Sample Size (n)"
)
legend(
  "top",
  legend = c("Mean", "Median", "Skewness"),
  col = c("steelblue", "darkgreen", "firebrick"),
  pch = 1:4,
  lty = 1,
  lwd = 2
)
grid()                                                            #--> turn on grid
abline(h = 0, lty = 2, col = "gold")                              #--> draw horizontal line

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
  main = "Kurtosis"
)
grid()
abline(h = 0, lty = 2, col = "gold")


#Export statistic table =========================================================
write.csv(
  stats,
  file = "stats_results_rnorm.csv",
  row.names = FALSE
)

#Close graphical device ========================================================
dev.off()