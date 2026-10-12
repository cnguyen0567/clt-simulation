## Library =====================================================================
library(moments)
library(nortest)
library(fBasics)
source("scripts/config.R")
set.seed(STUDENT_ID)
source("scripts/distributions.R")


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
              "Theo_SE",
              "Skewness",
              "SE_skew",
              "Z_skew",
              "Kurtosis",
              "SE_kurt",
              "Z_kurt",
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
    r_generator,                                            #random generator
    distribution_name,                                      #name for labels
    n,                                                      #no of sampling amount
    R,                                                      #no of MC simulation 
    stats_col                                               #columns to store the stats
) {
  
  #Data files and table to save results: ---------------------------------------
  fig_dir <- file.path("results", "figures")
  dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
  
  save_png <- function(name, kind, n, plot_fun) {
    png(file.path(fig_dir, sprintf("%s_%s_n%d.png", name, kind, n)),
        width = 1200, height = 900, res = 200)
    plot_fun()
    dev.off()
  }
  
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
  par(mfrow = c(3, 2))                                        #Set the subplot layout
  
  for (j in seq_along(n)) {
    
    #Run the MC R times for each n sample
    set.seed(STUDENT_ID)                                      #Reset seed for each simulation
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
    theo_se <- mc_sd / sqrt(i)
    
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
      round(theo_se, 5),
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
      probability = TRUE,                                    #Density hist rather than freq hist
      col = "steelblue",                                     #Fill color
      border = "white",                                      #Border color
      main = paste("Sampling Distribution of Mean: n =", i), #Title
      xlab = "Sample mean"                                   #x-axis label
    )
    
    curve(dnorm(x, mean = mc_mean, sd = mc_sd), add = TRUE, col = "firebrick", lwd = 2)
    
    # Q-Q
    qqnorm(
      sample_means,
      main = paste("Q-Q Plot: n =", i),
      pch = 46,
      cex = 0.5
    )
    qqline(sample_means, col = "firebrick")
    
    grid(nx = NULL, ny = NULL, lty = 3, col = "gray70")  #dotted grid line
    
    #Export the individual graphical results as png-------------------------------
    save_png(distribution_name, "hist", i, function() {
      hist(sample_means, probability = TRUE, col = "steelblue", border = "white",
           main = paste("Sampling Distribution of Mean: n =", i), xlab = "Sample mean")
      curve(dnorm(x, mean = mc_mean, sd = mc_sd), add = TRUE, col = "firebrick", lwd = 2)
    })
    save_png(distribution_name, "qq", i, function() {
      qqnorm(sample_means, main = paste("Q-Q Plot: n =", i), pch = 46, cex = 2)
      qqline(sample_means, col = "firebrick")
      grid(nx = NULL, ny = NULL, lty = 3, col = "gray70")  #dotted grid line
    })
    
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
run_distributions <- c(
  dist_death
)

n <- c(
  seq(1, 20, by = 1),        #Num of sample size needed to satisfy CLT
  seq(100, 800, by = 100)
  ) 

for (distribution_name in names(run_distributions)) {
  message("Running: ", distribution_name)
  
  all_stats[[distribution_name]] <- run_simulation(
    r_generator = run_distributions[[distribution_name]],
    distribution_name = distribution_name,
    n = n,
    R = R,
    stats_col = stats_col
  )
}
