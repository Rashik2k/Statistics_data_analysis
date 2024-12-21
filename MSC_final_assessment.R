# Load necessary packages, installing if not already installed
# Check if each package is installed, and if not, install it
define_packages <- c("ggplot2", "dplyr", "tidyr", "GGally", "viridis", "ggExtra")
sapply(define_packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
})

# Load the installed packages into the current R session
library(ggplot2)
library(dplyr)
library(tidyr)
library(GGally)
library(viridis)
library(ggExtra)

# Read and import the dataset from a specified file path
# The file path should point to a CSV file containing the dataset
# Ensure the file exists and the path is correct before running this code

data_set <- read.csv(file="E:\\masters\\dataset.csv")

# Select relevant columns that represent brain activity signals (x1 to x5)
# These columns are assumed to contain the input and output signals of interest for analysis
selected_data <- data_set %>% select(x1, x2, x3, x4, x5)

# Display the structure of the selected dataset
# This provides an overview of the data_set types and column information
str(selected_data)

# Display the first few rows of the selected dataset
# This gives a preview of the data_set for initial inspection
head(selected_data)

# Display summary statistics for each signal column
# Summary includes measures like mean, median, min, max, and quartiles
summary(selected_data)

# Check for missing values (NA) in each column of the dataset
# This ensures data_set quality by identifying incomplete entries
colSums(is.na(selected_data))

# Create a "time" column if not already present in the dataset
# Simulate a sequential time index from 1 to the number of rows in the dataset
selected_data$time <- 1:nrow(selected_data)

# Reshape the dataset from wide format to long format for visualization
# This format makes it easier to use ggplot for plotting grouped data_set
data_long <- selected_data %>%
  gather(key = "Signal", value = "BloodOxygenation", x1, x2, x3, x4, x5)

# Generate a list of unique signal identifiers for further processing
# This helps in creating individual plots for each signal
unique_signals <- unique(data_long$Signal)

# Task 1.1: Time Series Plots
# Create a combined time series plot for all signals
ggplot(data_long, aes(x = time, y = BloodOxygenation, color = Signal)) +
  geom_line() +
  labs(title = "Time Series of Brain Activity Signals",
       x = "Time",
       y = "Blood Oxygenation Level") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_color_manual(values = c("x1" = "red", "x2" = "blue", "x3" = "green", "x4" = "purple", "x5" = "black"))

# Create individual time series plots for each signal using facets
# This approach separates signals into distinct panels for clarity
ggplot(data_long, aes(x = time, y = BloodOxygenation, color = Signal)) +
  geom_line() +
  labs(title = "Time Series of Brain Activity Signals by Signal",
       x = "Time",
       y = "Blood Oxygenation Level") +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(~ Signal, ncol = 1) +
  scale_color_manual(values = rep("black", 5))

# Task 1.2: Distribution Plots
# Create a density plot to visualize the distribution of blood oxygenation levels for each signal
ggplot(data_long, aes(x = BloodOxygenation, fill = Signal)) +
  geom_density(alpha = 0.3) +
  labs(title = "Distribution of Blood Oxygenation Levels by Signal",
       x = "Blood Oxygenation Level",
       y = "Density") +
  theme_minimal() +
  theme(legend.title = element_blank())

# Generate individual distribution plots for each signal using histograms and density curves
# This provides a detailed view of the frequency distribution for each signal
for (signal in unique_signals) {
  signal_data <- data_long %>% filter(Signal == signal)
  p <- ggplot(signal_data, aes(x = BloodOxygenation, fill = Signal)) +
    geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.5, color = "black") +
    geom_density(alpha = 0.7) +
    labs(title = paste("Distribution of Blood Oxygenation Levels for Signal:", signal),
         x = "Blood Oxygenation Level",
         y = "Density") +
    theme_minimal() +
    theme(legend.position = "none")
  print(p)
}

# Task 1.3: Correlation and Scatter Plots
# Compute the correlation matrix for the selected signals
# Correlation values indicate relationships between pairs of signals
data_correlated <- selected_data %>% select(x1, x2, x3, x4, x5)
cor_matrix <- cor(data_correlated, use = "complete.obs")
print(cor_matrix)

# Add a grouping column for optional scatter plot coloring
# Groups are created based on whether signal x2 is above or below its median value
data_correlated$Group <- factor(ifelse(data_correlated$x2 > median(data_correlated$x2), "High", "Low"))

# Reshape the dataset for faceted scatter plots
# The input signals (x1, x3, x4, x5) are compared against x2 (assumed output)
data_long_scatter <- selected_data %>%
  select(x2, x1, x3, x4, x5) %>%
  pivot_longer(cols = c(x1, x3, x4, x5), names_to = "Input", values_to = "Value")

# Create scatter plots for each input signal against the output signal (x2)
# Each panel shows the relationship for a specific input-output pair
ggplot(data_long_scatter, aes(x = Value, y = x2)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  geom_smooth(method = "lm", se = FALSE, color = "yellow") +
  facet_wrap(~Input, scales = "free_x") +
  labs(title = "Scatter Plots of Inputs vs Output Signal x2",
       x = "Input Signal",
       y = "Output Signal (x2)") +
  theme_minimal()

# Generate pairwise scatter plots and correlations using ggpairs
# This creates a matrix of plots showing relationships between all pairs of signals
ggpairs(data_correlated,
        lower = list(continuous = wrap("smooth", color = "blue")),
        diag = list(continuous = wrap("densityDiag", fill = "lightblue")),
        upper = list(continuous = wrap("cor", size = 4, color = "red")),
        title = "Pairwise Relationships between Signals",
        axisLabels = "show")

# Task 2: Model Selection and Validation

# Step 1: Selecting input and output signals
y <- selected_data$x2  # Define the output variable (response) x2
x1 <- selected_data$x1  # Define the first input variable x1
x3 <- selected_data$x3  # Define the second input variable x3
x4 <- selected_data$x4  # Define the third input variable x4
x5 <- selected_data$x5  # Define the fourth input variable x5

# Define the bias term (intercept term, usually a column of 1s)
bias <- rep(1, length(y))

# Step 2: Adding Gaussian noise to the response variable
set.seed(42)  # Set seed for reproducibility of random numbers
noise_sd <- 0.3  # Standard deviation of the noise
noise <- rnorm(length(y), mean = 0, sd = noise_sd)  # Generate Gaussian noise
y_noisy <- y + noise  # Add noise to the true response variable

# Step 3: Define the models_list with corresponding design matrices
models_list <- list(
  list(X = cbind(x4, x3^2, bias), name = "Model 1"),  # Model 1: x4, x3^2, bias
  list(X = cbind(x4, x3^2, x5, bias), name = "Model 2"),  # Model 2: x4, x3^2, x5, bias
  list(X = cbind(x3, x4, x5^3), name = "Model 3"),  # Model 3: x3, x4, x5^3
  list(X = cbind(x4, x3^2, x5^3, bias), name = "Model 4"),  # Model 4: x4, x3^2, x5^3, bias
  list(X = cbind(x4, x1^2, x3^2, bias), name = "Model 5")  # Model 5: x4, x1^2, x3^2, bias
)

# Task 2.1: Compute parameters (theta) using Least Squares Estimation (LSE)
lse_model <- function(X, y) {
  theta <- solve(t(X) %*% X) %*% t(X) %*% y  # Calculate theta (LSE)
  return(list(theta = theta))
}

# Compute theta for each model using LSE
results_lse <- lapply(models_list, function(model) {
  lse_model(model$X, y_noisy)
})

# Print the results for LSE
for (i in 1:length(results_lse)) {
  cat(models_list[[i]]$name, "\n")  # Print model name
  cat("Thetahat:\n", results_lse[[i]]$theta, "\n")
}

# Task 2.2: Compute parameters (theta) and Residual Sum of Squares (RSS)
rss_model <- function(X, y) {
  theta <- solve(t(X) %*% X) %*% t(X) %*% y  # Calculate theta
  y_hat <- X %*% theta  # Predicted values from the model
  residuals <- y - y_hat  # Residuals (difference between actual and predicted values)
  rss <- sum(residuals^2)  # Compute RSS
  return(list(theta = theta, rss = rss, y_hat = y_hat, residuals = residuals))
}

# Compute theta and RSS for each model
results_rss <- lapply(models_list, function(model) {
  rss_model(model$X, y_noisy)
})

# Print the results for RSS
for (i in 1:length(results_rss)) {
  cat(models_list[[i]]$name, "\n")  # Print model name
  cat("Theta:\n", results_rss[[i]]$theta, "\n")
  cat("RSS:", results_rss[[i]]$rss, "\n\n")
}

# Task 2.3: Compute Log-Likelihood for each model
log_likelihood_model <- function(X, y) {
  theta <- solve(t(X) %*% X) %*% t(X) %*% y  # Calculate theta (LSE)
  y_hat <- X %*% theta  # Predicted values
  residuals <- y - y_hat  # Residuals
  rss <- sum(residuals^2)  # Residual sum of squares
  
  n <- length(y)  # Number of observations
  sigma_squared <- rss / (n - 1)  # Estimate variance from residuals
  
  # Calculate the log-likelihood
  log_likelihood <- -n / 2 * log(2 * pi * sigma_squared) - rss / (2 * sigma_squared)
  
  return(list(theta = theta, rss = rss, log_likelihood = log_likelihood, sigma_squared = sigma_squared))
}

# Compute Log-Likelihood for each model
log_likelihood_results <- lapply(models_list, function(model) {
  log_likelihood_model(model$X, y_noisy)
})

# Print the Log-Likelihood results
cat("Task 2.3: Log-Likelihood for each model\n")
for (i in 1:length(log_likelihood_results)) {
  cat(models_list[[i]]$name, "\n")
  cat("Theta Hat:\n", log_likelihood_results[[i]]$theta, "\n")
  cat("Log-Likelihood:", log_likelihood_results[[i]]$log_likelihood, "\n")
  cat("Estimated Sigma^2:", log_likelihood_results[[i]]$sigma_squared, "\n\n")
}

# Function to compute AIC and BIC
calculate_aic_bic_values <- function(log_likelihood_value, num_params, num_obs) {
  # AIC = 2k - 2 * log-likelihood
  aic_value <- 2 * num_params - 2 * log_likelihood_value
  
  # BIC = k * log(n) - 2 * log-likelihood
  bic_value <- num_params * log(num_obs) - 2 * log_likelihood_value
  
  return(list(aic_value = aic_value, bic_value = bic_value))
}
# Number of observations (n)
num_obs <- length(y)

# Define the number of parameters (k) for each model
params_count <- c(3, 4, 3, 4, 4)  # Number of parameters for each model

# Compute AIC and BIC for each model (Task 2.4)
aic_bic_results <- mapply(function(log_likelihood_value, num_params) {
  calculate_aic_bic_values(log_likelihood_value, num_params, num_obs)
}, log_likelihood_value = sapply(log_likelihood_results, function(x) x$log_likelihood),
num_params = params_count, SIMPLIFY = FALSE)

# Print results for AIC and BIC
cat("Task 2.4: AIC and BIC for each model\n")
for (i in 1:length(aic_bic_results)) {
  cat(models_list[[i]]$model_name, "\n")
  cat("AIC:", aic_bic_results[[i]]$aic_value, "\n")
  cat("BIC:", aic_bic_results[[i]]$bic_value, "\n\n")
}


# Task 2.5: Plot residuals distribution for each model
plot_residuals_distribution <- function(residuals, model_name) {
  qqnorm(residuals, main = paste("Q-Q Plot for", model_name))  # Generate Q-Q plot
  qqline(residuals, col = "yellow", lwd = 2)  # Add reference line
}

# Reset plotting layout
par(mfrow = c(1, 1))

# Loop through each model and plot residuals
for (i in 1:length(results_rss)) {
  residuals <- results_rss[[i]]$residuals  # Extract residuals
  model_name <- models_list[[i]]$name  # Model name
  plot_residuals_distribution(residuals, model_name)  # Generate Q-Q plot
  
  # Print basic statistics of residuals
  cat(model_name, "Residuals Summary:\n")
  cat("Mean of residuals: ", mean(residuals), "\n")
  cat("Standard deviation of residuals: ", sd(residuals), "\n\n")
}

# Task 2.7: Train-Test Split and Model Validation (Model 2)
train_indices <- sample(1:nrow(selected_data), size = 0.7 * nrow(selected_data))  # Randomly select training indices
train_data <- selected_data[train_indices, ]  # Training dataset
test_data <- selected_data[-train_indices, ]  # Testing dataset

# Extract output variable and inputs for Model 2
train_y <- train_data$x2
train_X <- cbind(train_data$x4, train_data$x3^2, train_data$x5, rep(1, nrow(train_data)))

test_y <- test_data$x2
test_X <- cbind(test_data$x4, test_data$x3^2, test_data$x5, rep(1, nrow(test_data)))

# Function to compute theta using training data
compute_theta <- function(X, y) {
  solve(t(X) %*% X) %*% t(X) %*% y
}

# Train Model 2 on training data
theta_hat <- compute_theta(train_X, train_y)

# Predict output on test data
test_predictions <- test_X %*% theta_hat

# Compute residuals and variance on training data
train_y_hat <- train_X %*% theta_hat
train_residuals <- train_y - train_y_hat
sigma_squared <- sum(train_residuals^2) / (nrow(train_data) - ncol(train_X))

# Compute standard errors of predictions
pred_var <- rowSums((test_X %*% solve(t(train_X) %*% train_X)) * test_X)
pred_std_err <- sqrt(sigma_squared * (1 + pred_var))

# Compute 95% confidence intervals for predictions
alpha <- 0.05
z_value <- qnorm(1 - alpha / 2)  # Critical value for 95% CI
conf_lower <- test_predictions - z_value * pred_std_err
conf_upper <- test_predictions + z_value * pred_std_err

# Combine test results for visualization
test_results <- data.frame(
  Actual = test_y,
  Predicted = test_predictions,
  Lower_CI = conf_lower,
  Upper_CI = conf_upper
)

# Plot predictions and confidence intervals
library(ggplot2)

ggplot(test_results, aes(x = 1:nrow(test_results))) +
  geom_point(aes(y = Actual), color = "black", size = 2, alpha = 0.7, shape = 1) +
  geom_line(aes(y = Predicted), color = "blue", size = 1) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), alpha = 0.2, fill = "yellow") +
  labs(title = "Model 2: Predictions vs Actual Test Data",
       x = "Test Data Points",
       y = "Output (x2)") +
  theme_minimal()


# Task 3

# Task 3.1
# Least squares estimates from Task 2.1 (as per your code)
model2_theta_hat <- c(0.7471398, 0.03920352, 0.1803357, -0.0641826)  # Coefficients for Model 2
model2_rss <- 151.4747  # Residual sum of squares (RSS) for Model 2

# Calculate the absolute values of the parameters
theta_abs <- abs(model2_theta_hat)  # Take the absolute values of the coefficients

# Identify the indices of the two parameters with the largest absolute values
top_two_indices <- order(theta_abs, decreasing = TRUE)[1:2]  # Get the indices of the top two parameters

# Extract the parameter names for the largest two absolute values
top_two_params <- model2_theta_hat[top_two_indices]  # Extract the top two parameters by value

# Define prior distributions for theta1 and theta3 (assuming uniform priors)
# The prior range is based on the absolute values of the top two parameters
theta_prior_1 <- function() runif(1, min = -abs(top_two_params[1]), max = abs(top_two_params[1]))
theta_prior_3 <- function() runif(1, min = -abs(top_two_params[2]), max = abs(top_two_params[2]))

# Function to simulate the model with fixed parameters (fix theta2 and theta4, sample theta1 and theta3)
simulate_model <- function(theta1, theta3, X) {
  # Fixed parameters from Task 2.1
  theta2 <- 0.03920352  # Fixed value of theta2 from Task 2.1
  theta4 <- -0.0641826  # Fixed value of theta4 from Task 2.1
  
  # Recompute the fitted values based on the new parameter set
  y_sim <- theta1 * X[,1] + theta2 * X[,2] + theta3 * X[,3] + theta4 * X[,4]  # Linear model equation
  return(y_sim)  # Return simulated output
}

# ABC Rejection Sampling
set.seed(42)  # Set seed for reproducibility
n_simulations <- 1000  # Number of simulations to perform

# Define the epsilon level for distance (RSS multiplied by a factor for acceptance)
epsilon <- model2_rss * 4  # epsilon is scaled RSS
print(epsilon)  # Print the epsilon value for debugging

# Initialize a list to store accepted samples
samples_accepted <- list()

# Perform rejection sampling
for (i in 1:n_simulations) {
  # Sample theta1 and theta3 from their priors
  theta1_sample <- theta_prior_1()
  theta3_sample <- theta_prior_3()
  
  # Simulate the model output with the sampled parameters
  y_sim <- simulate_model(theta1_sample, theta3_sample, models_list[[2]]$X)  # Using Model 2 design matrix
  
  # Compute the distance (sum of squared residuals) between simulated and observed data
  residuals <- y_noisy - y_sim  # Residuals between observed and simulated data
  distance <- sum(residuals^2)  # Compute sum of squared residuals
  
  # Debugging output to check distance and residuals
  if (i %% 1000 == 0) {  # Print every 1000th iteration for clarity
    cat("Iteration:", i, "Distance:", distance, "\n")
  }
  
  # If the distance is within epsilon, accept the sample
  if (distance < epsilon) {
    samples_accepted[[length(samples_accepted) + 1]] <- c(theta1_sample, theta3_sample)  # Store accepted sample
  }
}

# Convert the accepted samples into a data frame for easier analysis
accepted_samples_df <- do.call(rbind, samples_accepted)  # Combine list into a data frame
accepted_samples_df <- as.data.frame(accepted_samples_df)  # Convert to data frame

# Plot the accepted samples for theta1 and theta3
library(ggplot2)  # Load ggplot2 for visualization
p <- ggplot(accepted_samples_df, aes(x = V1, y = V2)) +  # Define plot aesthetics
  geom_point(alpha = 0.5, color = "red") +  # Plot accepted samples as points
  labs(title = "Accepted Samples from ABC Rejection Sampling",
       x = "Theta 1", y = "Theta 3") +  # Add title and axis labels
  theme_minimal()  # Use minimal theme for the plot

# Add marginal histograms using ggExtra
# This adds histograms to the margins of the plot to show the distribution of theta1 and theta3
library(ggExtra)
ggExtra::ggMarginal(p, type = "histogram", fill = "lightblue", alpha = 0.5)

# Summary of accepted samples
cat("Number of accepted samples: ", nrow(accepted_samples_df), "\n")  # Print the number of accepted samples
summary(accepted_samples_df)  # Print summary statistics of accepted samples
