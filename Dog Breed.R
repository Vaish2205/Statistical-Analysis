load("/Users/vaish2205/Downloads/rehoming.Rdata") 
loaded_objects <- ls()  
print(loaded_objects)   
summary("rehoming.Rdata")    
head("rehoming.Rdata")      
ls()

createsample(201896641)
save(mysample, file = "mysample.RData")
table(mysample$Breed)
summary(mysample)
colnames(mysample)

# Data Cleaning
load("mysample.RData")
initial_count <- nrow(mysample)
missing_rehomed_count <- sum(mysample$Rehomed == 99999, na.rm = TRUE)
missing_breed_count <- sum(is.na(mysample$Breed))
percent_missing_rehomed <- (missing_rehomed_count / initial_count) * 100
percent_missing_breed <- (missing_breed_count / initial_count) * 100
mysample_clean <- mysample[mysample$Rehomed != 99999 & !is.na(mysample$Breed), ]
final_count <- nrow(mysample_clean)
total_removed <- initial_count - final_count
# Report results
cat("Initial number of observations:", initial_count, "\n")
cat("Number of rows removed due to missing rehoming time:", missing_rehomed_count, 
    sprintf("(%.2f%%)", percent_missing_rehomed), "\n")
cat("Number of rows removed due to missing breed:", missing_breed_count, 
    sprintf("(%.2f%%)", percent_missing_breed), "\n")
cat("Total number of rows removed:", total_removed, "\n")
cat("Final number of observations:", final_count, "\n")
cat("Breed distribution in cleaned data:\n")
print(table(mysample_clean$Breed))
save(mysample_clean, file = "mysample_clean.RData")


# Data Exploration
load("mysample_clean.RData")
breed_groups <- split(mysample_clean, mysample_clean$Breed)
summaries <- list()

for (breed in names(breed_groups)) {
  data <- breed_groups[[breed]]
  summaries[[breed]] <- list(
    Rehomed = summary(data$Rehomed),
    Health = summary(data$Health),
    Age = table(data$Age),
    Returned = table(data$Returned),
    Reason = table(data$Reason)
  )
}
print(summaries)
install.packages("ggplot2")
library(ggplot2)
# Boxplot for rehoming time by breed
ggplot(mysample_clean, aes(x = Breed, y = Rehomed, fill = Breed)) +
  geom_boxplot() +
  labs(title = "Rehoming Time by Breed", x = "Breed", y = "Rehoming Time (weeks)") +
  theme_minimal()
# Violin plot for health by breed
ggplot(mysample_clean, aes(x = Breed, y = Health, fill = Breed)) +
  geom_violin() +
  labs(title = "Health Distribution by Breed", x = "Breed", y = "Health Score") +
  theme_minimal()
# Bar plot for age distribution by breed
ggplot(mysample_clean, aes(x = Breed, fill = Age)) +
  geom_bar(position = "dodge") +
  labs(title = "Age Distribution by Breed", x = "Breed", y = "Count") +
  theme_minimal()
# Bar plot for returned status by breed
ggplot(mysample_clean, aes(x = Breed, fill = factor(Returned))) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Returned Dogs by Breed", x = "Breed", y = "Proportion", fill = "Returned") +
  theme_minimal()

install.packages("tidyverse")
library(tidyverse)
reason_data <- data.frame(
  Breed = c("Mixed Breed", "Shih Tzu", "Staffordshire Bull Terrier"),
  Dangerous = c(0.71, 0.00, 1.45),
  `Health Condition` = c(0.85, 0.00, 0.97),
  Neglect = c(61.16, 87.50, 44.44),
  Stray = c(33.05, 4.17, 46.86),
  Unwanted = c(4.24, 8.33, 6.28)
)
long_reason_data <- reason_data %>%
  pivot_longer(cols = Dangerous:Unwanted, names_to = "Reason", values_to = "Proportion")

# Bar plot for Reason for Rehoming Proportions by Breed
ggplot(long_reason_data, aes(x = Breed, y = Proportion, fill = Reason)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(
    title = "Proportion of Reasons for Rehoming by Breed",
    x = "Breed",
    y = "Proportion",
    fill = "Reason"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2")


# Modelling and Estimation
library(fitdistrplus)
breeds <- c("Mixed Breed", "Shih Tzu", "Staffordshire Bull Terrier")
par(mfrow = c(1, 3)) 
for (breed in breeds) {
  breed_data <- subset(mysample_clean, Breed == breed)
  breed_data_clean <- breed_data[!is.na(breed_data$Rehomed) & breed_data$Rehomed != 99999, ]
  fit_norm <- fitdist(breed_data_clean$Rehomed, "norm")
  hist(breed_data_clean$Rehomed, freq = FALSE, col = "lightblue", 
       main = paste("Histogram and Normal Fit for Rehoming Time\n(", breed, ")", sep = ""), 
       xlab = "Rehoming Time (weeks)", breaks = 20)
  curve(dnorm(x, mean = fit_norm$estimate[1], sd = fit_norm$estimate[2]), 
        col = "red", lwd = 2, add = TRUE)
}


# INFERENCE
load("mysample_clean.RData")
breeds <- unique(mysample_clean$Breed)
calc_mean_ci <- function(data, breed, alpha = 0.05) {
  rehomed <- data$Rehomed
  n <- length(rehomed)
  mean_rehomed <- mean(rehomed)
  sd_rehomed <- sd(rehomed)
  se <- sd_rehomed / sqrt(n)
  if (n > 30) {
    z_critical <- qnorm(1 - alpha / 2)
    margin_of_error <- z_critical * se
    test_type <- "Z-Test"
  } else {
    t_critical <- qt(1 - alpha / 2, df = n - 1)
    margin_of_error <- t_critical * se
    test_type <- "T-Test"
  }
  
  # Confidence Interval
  lower_bound <- mean_rehomed - margin_of_error
  upper_bound <- mean_rehomed + margin_of_error
  includes_27 <- lower_bound <= 27 && upper_bound >= 27
  
  # Print results
  cat("\n===== Confidence Interval for Breed:", breed, "=====\n")
  cat("Mean Rehoming Time:", round(mean_rehomed, 2), "weeks\n")
  cat("Standard Deviation:", round(sd_rehomed, 2), "\n")
  cat("Test Type:", test_type, "\n")
  cat("Confidence Interval for Mean (27 weeks):", round(lower_bound, 2), "to", round(upper_bound, 2), "\n")
  cat("Does the CI include 27?", includes_27, "\n")
  
  return(data.frame(
    Breed = breed,
    Test_Type = test_type,
    Mean = round(mean_rehomed, 2),
    SD = round(sd_rehomed, 2),
    Lower_CI = round(lower_bound, 2),
    Upper_CI = round(upper_bound, 2),
    Includes_27 = includes_27
  ))
}
breed_results <- do.call(rbind, lapply(breeds, function(breed) {
  breed_data <- mysample_clean[mysample_clean$Breed == breed, ]
  calc_mean_ci(breed_data, breed)
}))
print(breed_results)

    # Visualize the confidence intervals for each breed
library(ggplot2)
ggplot(breed_results, aes(x = Breed, y = Mean)) +
  geom_violin(aes(fill = Breed), alpha = 0.4) +
  geom_point(aes(color = "Mean"), size = 3) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, color = "blue") +
  geom_hline(yintercept = 27, linetype = "dashed", color = "red") +
  labs(title = "Confidence Intervals for Mean Rehoming Time for Each Breed",
       x = "Breed", y = "Rehoming Time (weeks)") +
  theme(
    panel.background = element_rect(fill = "white"),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_line(color = "grey", size = 0.5),  
    panel.grid.minor = element_line(color = "darkgrey", size = 0.25),  
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 12),  
    axis.text.y = element_text(color = "black", size = 12),  
    axis.title.x = element_text(color = "black", size = 14),  
    axis.title.y = element_text(color = "black", size = 14),  
    plot.title = element_text(color = "black", size = 16, face = "bold", hjust = 0.5),  
    plot.subtitle = element_text(color = "black", size = 14, hjust = 0.5),  
    legend.position = "none"
  ) +
  scale_fill_brewer(palette = "Set3") +
  scale_color_manual(values = c("Mean" = "red"))  

#COMPARISON
breed_groups <- split(mysample_clean, mysample_clean$Breed)
calc_difference_ci <- function(data1, data2, breed1, breed2, alpha = 0.05) {
  rehomed1 <- data1$Rehomed
  rehomed2 <- data2$Rehomed
  
  n1 <- length(rehomed1)
  n2 <- length(rehomed2)
  mean1 <- mean(rehomed1)
  mean2 <- mean(rehomed2)
  sd1 <- sd(rehomed1)
  sd2 <- sd(rehomed2)
  if (n1 > 30 & n2 > 30) {
    pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
    z_critical <- qnorm(1 - alpha / 2)
    margin_of_error <- z_critical * pooled_sd * sqrt(1 / n1 + 1 / n2)
    test_type <- "Z-Test (Assuming equal variance)"
  } else {
    t_critical <- qt(1 - alpha / 2, df = min(n1 - 1, n2 - 1))
    margin_of_error <- t_critical * sqrt((sd1^2 / n1) + (sd2^2 / n2))
    test_type <- "T-Test (Unequal variance)"
  }
  diff_mean <- mean1 - mean2
  lower_bound <- diff_mean - margin_of_error
  upper_bound <- diff_mean + margin_of_error
  includes_zero <- lower_bound <= 0 && upper_bound >= 0
  
  # Print results
  cat("\n===== Comparison Between", breed1, "and", breed2, "=====\n")
  cat("Mean Difference:", round(diff_mean, 2), "\n")
  cat("Standard Deviation Breed 1:", round(sd1, 2), "\n")
  cat("Standard Deviation Breed 2:", round(sd2, 2), "\n")
  cat("Test Type:", test_type, "\n")
  cat("Confidence Interval for Mean Difference:", round(lower_bound, 2), "to", round(upper_bound, 2), "\n")
  cat("Does the CI include 0?", includes_zero, "\n")
  
  return(data.frame(
    Breed1 = breed1,
    Breed2 = breed2,
    Test_Type = test_type,
    Mean_Difference = round(diff_mean, 2),
    SD_Breed1 = round(sd1, 2),
    SD_Breed2 = round(sd2, 2),
    Lower_CI = round(lower_bound, 2),
    Upper_CI = round(upper_bound, 2),
    Includes_Zero = includes_zero
  ))
}
     # Generate pairwise comparisons between all breeds
breed_combinations <- combn(names(breed_groups), 2, simplify = FALSE)
comparison_results <- do.call(rbind, lapply(breed_combinations, function(pair) {
  calc_difference_ci(breed_groups[[pair[1]]], breed_groups[[pair[2]]], pair[1], pair[2])
}))
print(comparison_results)
library(ggplot2)
ggplot(comparison_results, aes(x = interaction(Breed1, Breed2), y = Mean_Difference, ymin = Lower_CI, ymax = Upper_CI)) +
  geom_pointrange(color = "red", size = 1) +  # Set point range color to cyan
  geom_hline(yintercept = 0, linetype = "dashed", color = "yellow", size = 1) +  
  labs(title = "Confidence Intervals for Difference in Mean Rehoming Time Between Breeds",
       x = "Breed Comparison", y = "Mean Difference in Rehoming Time (weeks)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "black", color = "black"),  
    panel.background = element_rect(fill = "black"),  
    panel.grid.major = element_line(color = "gray", size = 0.7),  
    panel.grid.minor = element_line(color = "white", size = 0.4),  
    plot.title = element_text(color = "white", size = 16, face = "bold", hjust = 0.5),  
    axis.title = element_text(color = "white", size = 14, face = "bold"),  
    axis.text = element_text(color = "white", size = 12),  
    axis.ticks = element_line(color = "white"),  
    legend.text = element_text(color = "white", size = 12),  
    legend.title = element_text(color = "white", size = 14, face = "bold"),  
    plot.margin = margin(20, 20, 20, 20)  
  )
