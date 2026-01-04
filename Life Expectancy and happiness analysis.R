
# LIFE EXPECTANCY AND HAPPINESS: STATISTICAL ANALYSIS OF GLOBAL HEALTH DISPARITIES

# Author: Samidha Basrur
# Date: December 2025
# Description: Analysis of 2015 WHO Life Expectancy and World Happiness Report data
# examining global health disparities through hypothesis testing and regression

# LOAD LIBRARIES
library(tidyverse)
library(knitr)
library(gridExtra)
library(magrittr)
library(ggrepel)
library(flextable)
library(officer)
library(psych)
library(broom)

# SECTION 1: DATA LOADING AND PREPARATION

# Load the datasets
life <- read.csv("Life Expectancy Data.csv")
happiness <- read.csv("World Happiness Report (1).csv")

# Filter life expectancy data for 2015 only
life_2015 <- life %>%
  filter(Year == 2015) %>%
  select(
    Country,
    Life_expectancy = `Life.expectancy`,
    Status,
    GDP,
    Population,
    Income_composition = `Income.composition.of.resources`,
    Schooling,
    Adult_Mortality = `Adult.Mortality`,
    BMI
  )

# Merge with happiness data by country
combined_2015 <- happiness %>%
  left_join(life_2015, by = "Country")

# Data cleaning - remove countries without life expectancy data
combined_2015_clean <- combined_2015 %>%
  filter(!is.na(Life_expectancy))

# Check data cleaning results
raw_rows <- nrow(combined_2015)
clean_rows <- nrow(combined_2015_clean)
cat("Original rows:", raw_rows, "\n")
cat("After cleaning:", clean_rows, "\n")
cat("Rows removed:", raw_rows - clean_rows, "\n")

# SECTION 2: EXPLORATORY DATA ANALYSIS


# Calculate global statistics
global_mean_life <- mean(combined_2015_clean$Life_expectancy)
global_sd_life <- sd(combined_2015_clean$Life_expectancy)

cat("\nGlobal Life Expectancy Statistics:\n")
cat("Mean:", round(global_mean_life, 2), "years\n")
cat("SD:", round(global_sd_life, 2), "years\n")

# Primary outcome distribution - Life Expectancy
mean_l <- mean(combined_2015_clean$Life_expectancy)
median_l <- median(combined_2015_clean$Life_expectancy)

p_life <- ggplot(combined_2015_clean, aes(Life_expectancy)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 20, fill = "lightgreen", color = "black", alpha = 0.7) +
  geom_density(color = "darkblue", linewidth = 1.2) +
  geom_vline(aes(xintercept = mean_l, color = "Mean"), linewidth = 1) +
  geom_vline(aes(xintercept = median_l, color = "Median"),
             linewidth = 1, linetype = "dashed") +
  scale_color_manual(values = c("Mean" = "red", "Median" = "blue")) +
  labs(
    title = "Life Expectancy Distribution (Primary Outcome)",
    x = "Life Expectancy (years)",
    y = "Density"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

print(p_life)

# Descriptive statistics for all numeric variables
clean_numeric_stats <- combined_2015_clean %>%
  pivot_longer(cols = where(is.numeric),
               names_to = "Variable",
               values_to = "Value") %>%
  group_by(Variable) %>%
  summarise(
    Mean = round(mean(Value, na.rm = TRUE), 2),
    SD = round(sd(Value, na.rm = TRUE), 2),
    Median = round(median(Value, na.rm = TRUE), 2),
    Min = round(min(Value, na.rm = TRUE), 2),
    Max = round(max(Value, na.rm = TRUE), 2),
    .groups = "drop"
  )

print(clean_numeric_stats)

# Regional summary statistics
regional_summary <- combined_2015_clean %>%
  group_by(Region) %>%
  summarise(
    Count = n(),
    Mean_LifeExp = round(mean(Life_expectancy), 2),
    SD_LifeExp = round(sd(Life_expectancy), 2),
    Mean_Happiness = round(mean(Happiness.Score), 2),
    SD_Happiness = round(sd(Happiness.Score), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(Mean_LifeExp))

print(regional_summary)

# SECTION 3: HYPOTHESIS TESTING

# Create OECD classification
oecd_countries <- c(
  "Australia", "Austria", "Belgium", "Canada", "Chile", "Czech Republic",
  "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary",
  "Iceland", "Ireland", "Israel", "Italy", "Japan", "South Korea", "Luxembourg",
  "Mexico", "Netherlands", "New Zealand", "Norway", "Poland", "Portugal",
  "Slovak Republic", "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey",
  "United Kingdom", "United States"
)

combined_2015_clean <- combined_2015_clean %>%
  mutate(OECD_Status = ifelse(Country %in% oecd_countries, "OECD", "Non-OECD"))

# QUESTION 1: ONE-SAMPLE T-TEST
# Research Question: Is the mean life expectancy of African countries 
#                    significantly different from the global mean of 72 years?
# H₀: μ_Africa = 72 years
# H₁: μ_Africa ≠ 72 years


cat("\n============================================================================\n")
cat("QUESTION 1: ONE-SAMPLE T-TEST (Africa vs Global Mean)\n")
cat("============================================================================\n")

# Extract Africa data
africa_data <- combined_2015_clean %>%
  filter(Region == "Africa") %>%
  pull(Life_expectancy)

# Descriptive statistics
africa_stats <- data.frame(
  Region = "Africa",
  N = length(africa_data),
  Mean = round(mean(africa_data), 2),
  SD = round(sd(africa_data), 2),
  Median = round(median(africa_data), 2),
  Min = round(min(africa_data), 2),
  Max = round(max(africa_data), 2),
  SE = round(sd(africa_data) / sqrt(length(africa_data)), 2)
)

print(africa_stats)

# Check normality assumption
shapiro_africa <- shapiro.test(africa_data)
cat("\nShapiro-Wilk Test for Normality:\n")
cat("W =", round(shapiro_africa$statistic, 4), ", p =", round(shapiro_africa$p.value, 4), "\n")

# Conduct one-sample t-test
q1_test <- t.test(africa_data,
                  mu = 72,
                  alternative = "two.sided",
                  conf.level = 0.95)

# Calculate effect size (Cohen's d)
cohens_d_q1 <- abs((mean(africa_data) - 72) / sd(africa_data))

# Display results
q1_results <- data.frame(
  Test = "One-Sample t-test",
  Group = "Africa",
  Sample_Size = length(africa_data),
  Sample_Mean = round(mean(africa_data), 2),
  Test_Value = 72,
  t_statistic = round(q1_test$statistic, 4),
  df = q1_test$parameter,
  p_value = round(q1_test$p.value, 4),
  CI_Lower = round(q1_test$conf.int[1], 2),
  CI_Upper = round(q1_test$conf.int[2], 2),
  Cohens_d = round(cohens_d_q1, 3)
)

print(q1_results)

# Visualization
q1_plot <- ggplot(combined_2015_clean, aes(x = Life_expectancy)) +
  geom_histogram(data = filter(combined_2015_clean, Region == "Africa"),
                 aes(y = after_stat(density)),
                 bins = 15, fill = "coral", alpha = 0.7, color = "black") +
  geom_density(data = filter(combined_2015_clean, Region == "Africa"),
               color = "darkred", linewidth = 1.2) +
  geom_vline(xintercept = mean(africa_data), 
             color = "red", linewidth = 1.2, linetype = "solid") +
  geom_vline(xintercept = 72, 
             color = "blue", linewidth = 1.2, linetype = "dashed") +
  annotate("text", x = mean(africa_data) + 3, y = 0.08, 
           label = paste("Africa Mean =", round(mean(africa_data), 1)),
           color = "red", fontface = "bold", size = 4) +
  annotate("text", x = 72 + 3, y = 0.08, 
           label = "Global Mean = 72",
           color = "blue", fontface = "bold", size = 4) +
  labs(
    title = "Q1: Life Expectancy Distribution - Africa vs Global Mean",
    subtitle = paste("One-Sample t-test: t =", round(q1_test$statistic, 2), 
                     ", p =", round(q1_test$p.value, 4)),
    x = "Life Expectancy (years)",
    y = "Density"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

print(q1_plot)

# QUESTION 2: TWO-SAMPLE T-TEST (OECD vs NON-OECD)
# Research Question: Is there a significant difference in mean life expectancy
#                    between OECD and Non-OECD countries?
# H₀: μ_OECD = μ_Non-OECD
# H₁: μ_OECD ≠ μ_Non-OECD

cat("\n============================================================================\n")
cat("QUESTION 2: TWO-SAMPLE T-TEST (OECD vs Non-OECD)\n")
cat("============================================================================\n")

# Extract data by OECD Status
oecd_data <- combined_2015_clean %>%
  filter(OECD_Status == "OECD") %>%
  pull(Life_expectancy)

non_oecd_data <- combined_2015_clean %>%
  filter(OECD_Status == "Non-OECD") %>%
  pull(Life_expectancy)

# Descriptive statistics
oecd_stats <- combined_2015_clean %>%
  group_by(OECD_Status) %>%
  summarise(
    N = n(),
    Mean = round(mean(Life_expectancy), 2),
    SD = round(sd(Life_expectancy), 2),
    Median = round(median(Life_expectancy), 2),
    Min = round(min(Life_expectancy), 2),
    Max = round(max(Life_expectancy), 2),
    SE = round(sd(Life_expectancy) / sqrt(n()), 2),
    .groups = "drop"
  )

print(oecd_stats)

# Check normality
shapiro_oecd <- shapiro.test(oecd_data)
shapiro_non_oecd <- shapiro.test(non_oecd_data)

cat("\nShapiro-Wilk Tests for Normality:\n")
cat("OECD: W =", round(shapiro_oecd$statistic, 4), ", p =", round(shapiro_oecd$p.value, 4), "\n")
cat("Non-OECD: W =", round(shapiro_non_oecd$statistic, 4), ", p =", round(shapiro_non_oecd$p.value, 4), "\n")

# Conduct two-sample t-test (Welch's)
q2_test <- t.test(oecd_data, non_oecd_data,
                  alternative = "two.sided",
                  var.equal = FALSE,
                  conf.level = 0.95)

# Calculate effect size (Cohen's d)
pooled_sd <- sqrt(((length(oecd_data) - 1) * var(oecd_data) + 
                     (length(non_oecd_data) - 1) * var(non_oecd_data)) / 
                    (length(oecd_data) + length(non_oecd_data) - 2))
cohens_d_q2 <- abs((mean(oecd_data) - mean(non_oecd_data)) / pooled_sd)

# Display results
q2_results <- data.frame(
  Test = "Two-Sample t-test",
  Group1 = "OECD",
  Group2 = "Non-OECD",
  n1 = length(oecd_data),
  n2 = length(non_oecd_data),
  Mean1 = round(mean(oecd_data), 2),
  Mean2 = round(mean(non_oecd_data), 2),
  Mean_Diff = round(mean(oecd_data) - mean(non_oecd_data), 2),
  t_statistic = round(q2_test$statistic, 4),
  df = round(q2_test$parameter, 2),
  p_value = round(q2_test$p.value, 4),
  CI_Lower = round(q2_test$conf.int[1], 2),
  CI_Upper = round(q2_test$conf.int[2], 2),
  Cohens_d = round(cohens_d_q2, 3)
)

print(q2_results)

# Visualization - Jitter Plot
q2_plot <- ggplot(combined_2015_clean, aes(x = OECD_Status, y = Life_expectancy, color = OECD_Status)) +
  geom_jitter(width = 0.25, alpha = 0.7, size = 3) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 5, 
               color = "black") +
  stat_summary(fun = mean, geom = "text", aes(label = paste("Mean =", round(..y.., 1))),
               vjust = -1.5, color = "black", fontface = "bold") +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Q2: Life Expectancy - OECD vs Non-OECD Countries",
    subtitle = paste("Two-Sample t-test: t =", round(q2_test$statistic, 2), 
                     ", p =", format.pval(q2_test$p.value, digits = 3)),
    x = "OECD Status",
    y = "Life Expectancy (years)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "none"
  )

print(q2_plot)

# QUESTION 3: TWO-SAMPLE T-TEST (REGIONAL COMPARISON)
# Research Question: Is there a significant difference in mean life expectancy
#                    between Western Europe and Africa?
# H₀: μ_WesternEurope = μ_Africa
# H₁: μ_WesternEurope ≠ μ_Africa

cat("\n============================================================================\n")
cat("QUESTION 3: TWO-SAMPLE T-TEST (Western Europe vs Africa)\n")
cat("============================================================================\n")

# Extract data by Region
weurope <- combined_2015_clean %>%
  filter(Region == "Western Europe") %>%
  pull(Life_expectancy)

africa <- combined_2015_clean %>%
  filter(Region == "Africa") %>%
  pull(Life_expectancy)

# Descriptive statistics
region_stats <- combined_2015_clean %>%
  filter(Region %in% c("Western Europe", "Africa")) %>%
  group_by(Region) %>%
  summarise(
    N = n(),
    Mean = round(mean(Life_expectancy), 2),
    SD = round(sd(Life_expectancy), 2),
    Median = round(median(Life_expectancy), 2),
    Min = round(min(Life_expectancy), 2),
    Max = round(max(Life_expectancy), 2),
    SE = round(sd(Life_expectancy) / sqrt(n()), 2),
    .groups = "drop"
  )

print(region_stats)

# Check normality
shapiro_weurope <- shapiro.test(weurope)
shapiro_africa_q3 <- shapiro.test(africa)

cat("\nShapiro-Wilk Tests for Normality:\n")
cat("Western Europe: W =", round(shapiro_weurope$statistic, 4), ", p =", round(shapiro_weurope$p.value, 4), "\n")
cat("Africa: W =", round(shapiro_africa_q3$statistic, 4), ", p =", round(shapiro_africa_q3$p.value, 4), "\n")

# Conduct two-sample t-test (Welch's)
q3_test <- t.test(weurope, africa,
                  alternative = "two.sided",
                  var.equal = FALSE,
                  conf.level = 0.95)

# Calculate effect size (Cohen's d)
pooled_sd_q3 <- sqrt(((length(weurope) - 1) * var(weurope) + 
                        (length(africa) - 1) * var(africa)) / 
                       (length(weurope) + length(africa) - 2))
cohens_d_q3 <- abs((mean(weurope) - mean(africa)) / pooled_sd_q3)

# Display results
q3_results <- data.frame(
  Test = "Two-Sample t-test",
  Group1 = "Western Europe",
  Group2 = "Africa",
  n1 = length(weurope),
  n2 = length(africa),
  Mean1 = round(mean(weurope), 2),
  Mean2 = round(mean(africa), 2),
  Mean_Diff = round(mean(weurope) - mean(africa), 2),
  t_statistic = round(q3_test$statistic, 4),
  df = round(q3_test$parameter, 2),
  p_value = round(q3_test$p.value, 4),
  CI_Lower = round(q3_test$conf.int[1], 2),
  CI_Upper = round(q3_test$conf.int[2], 2),
  Cohens_d = round(cohens_d_q3, 3)
)

print(q3_results)

# Visualization - Jitter Plot
q3_plot <- ggplot(combined_2015_clean %>% 
                    filter(Region %in% c("Western Europe", "Africa")), 
                  aes(x = Region, y = Life_expectancy, color = Region)) +
  geom_jitter(width = 0.25, alpha = 0.7, size = 3) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 5, 
               color = "black") +
  stat_summary(fun = mean, geom = "text", aes(label = paste("Mean =", round(..y.., 1))),
               vjust = -1.5, color = "black", fontface = "bold") +
  scale_color_manual(values = c("Africa" = "coral", "Western Europe" = "lightblue")) +
  labs(
    title = "Q3: Life Expectancy - Western Europe vs Africa",
    subtitle = paste("Two-Sample t-test: t =", round(q3_test$statistic, 2), 
                     ", p =", format.pval(q3_test$p.value, digits = 3)),
    x = "Region",
    y = "Life Expectancy (years)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "none"
  )

print(q3_plot)

# SECTION 4: REGRESSION ANALYSIS

cat("\n============================================================================\n")
cat("REGRESSION ANALYSIS\n")
cat("============================================================================\n")

# Prepare data for regression
model_data <- combined_2015 %>%
  select(
    Life_expectancy, Economy, Happiness.Score, GDP, Population,
    Income_composition, Schooling, Adult_Mortality, BMI
  ) %>%
  drop_na()

# Simple linear regressions for individual predictors
get_col <- function(df, candidates) {
  candidates[candidates %in% names(df)][1]
}

life_col <- get_col(combined_2015_clean, c("Life_expectancy", "Life.expectancy", "Life expectancy"))
economy_col <- get_col(combined_2015_clean, c("Economy", "Economy..GDP.per.Capita.", "Economy (GDP per Capita)"))
happy_col   <- get_col(combined_2015_clean, c("Happiness.Score", "Happiness Score"))
income_col  <- get_col(combined_2015_clean, c("Income_composition", "Income.composition.of.resources"))
adult_col   <- get_col(combined_2015_clean, c("Adult.Mortality", "Adult_Mortality", "Adult Mortality"))

plot_df <- combined_2015_clean %>%
  transmute(
    Life_expectancy = .data[[life_col]],
    Economy = .data[[economy_col]],
    Happiness.Score = .data[[happy_col]],
    Income_composition = .data[[income_col]],
    Adult_Mortality = .data[[adult_col]],
    Country = Country
  ) %>%
  drop_na()

# Function to create correlation plots
corr_plot <- function(df, xvar, xlab) {
  ct <- cor.test(df[[xvar]], df[["Life_expectancy"]], use = "complete.obs")
  r  <- unname(ct$estimate)
  p  <- ct$p.value
  lab <- sprintf("r = %.2f, p = %.3g, R² = %.2f", r, p, r^2)
  
  ggplot(df, aes(x = .data[[xvar]], y = Life_expectancy)) +
    geom_point(alpha = 0.85, size = 2) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
    annotate(
      "text",
      x = min(df[[xvar]], na.rm = TRUE),
      y = max(df$Life_expectancy, na.rm = TRUE),
      hjust = 0, vjust = 1,
      label = lab,
      size = 4.5
    ) +
    labs(
      title = paste0("Life Expectancy vs ", xlab),
      x = xlab,
      y = "Life Expectancy (years)"
    ) +
    theme_minimal(base_size = 13)
}

# Create individual regression plots
p1 <- corr_plot(plot_df, "Economy", "Economy")
p2 <- corr_plot(plot_df, "Happiness.Score", "Happiness Score")
p3 <- corr_plot(plot_df, "Income_composition", "Income composition of resources")
p4 <- corr_plot(plot_df, "Adult_Mortality", "Adult Mortality")

print(p1); print(p2); print(p3); print(p4)

# Multiple regression with stepwise selection
full_model <- lm(
  Life_expectancy ~ Economy + Happiness.Score + GDP +
    Population + Income_composition + Schooling + Adult_Mortality + BMI,
  data = model_data
)

cat("\nPerforming backward stepwise selection...\n")
step_model <- step(full_model, direction = "backward", trace = 0)

cat("\nFinal Model Summary:\n")
summary(step_model)

# Final model with selected predictors
Final_model <- lm(
  Life_expectancy ~ Economy + Happiness.Score + Income_composition + Adult_Mortality,
  data = model_data
)

cat("\n============================================================================\n")
cat("FINAL REGRESSION MODEL\n")
cat("============================================================================\n")
summary(Final_model)

cat("\n============================================================================\n")
cat("ANALYSIS COMPLETE\n")
cat("============================================================================\n")

