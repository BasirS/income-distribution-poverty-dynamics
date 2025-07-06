####################################################################
# Global Poverty Analysis
# STA 3000 Final Project
####################################################################

####################################################################
# Research Hypotheses and Equations
####################################################################

# Primary Research Hypotheses:
# H1: Countries with lower income inequality (measured by mean-to-median ratio) 
#     will show greater poverty reduction over time
# H2: The relationship between economic growth and poverty reduction is moderated 
#     by income distribution patterns
# H3: Regional differences in poverty reduction success are statistically significant
# H4: The poorest decile's growth rate is a significant predictor of overall poverty reduction

# Key Equations:

# 1. Inequality Metrics
# Mean-to-Median Ratio = Mean_Income / Median_Income
# Richest-to-Poorest Ratio = Richest_Decile / Poorest_Decile
# Gini Coefficient = 1 - (2/n) * sum((n-i+0.5)/n * y_i)

# 2. Poverty Reduction Metrics
# Absolute Reduction = Initial_Poverty - Final_Poverty
# Relative Reduction = (Initial_Poverty - Final_Poverty) / Initial_Poverty * 100
# Annual Reduction Rate = Absolute_Reduction / Year_Range

# 3. Statistical Models
# Model 1: Linear relationship between income and poverty
# Extreme_Poverty_Share = b0 + b1*log(Mean_Income) + e

# Model 2: Inequality's effect on poverty
# Extreme_Poverty_Share = b0 + b1*Richest_to_Poorest_Ratio + e

# Model 3: Combined effects
# Extreme_Poverty_Share = b0 + b1*log(Mean_Income) + b2*Richest_to_Poorest_Ratio + e

# Model 4: Interaction effects
# Extreme_Poverty_Share = b0 + b1*log(Mean_Income) + b2*Richest_to_Poorest_Ratio + 
#                        b3*(log(Mean_Income)*Richest_to_Poorest_Ratio) + e

# 4. Time Series Components
# Y(t) = T(t) + S(t) + R(t)
# where:
# Y(t) = Observed poverty rate at time t
# T(t) = Trend component
# S(t) = Seasonal component (if any)
# R(t) = Random component

# 5. Growth Incidence Analysis
# Growth_Rate = (Final_Value - Initial_Value) / Initial_Value * 100
# Decile_Growth_Ratio = Growth_Rate_Poorest_Decile / Growth_Rate_Richest_Decile

# loading necessary packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(lmtest)      # For dwtest
library(car)         # For vif
library(forecast)    # For auto.arima and forecast
library(tseries)     # For time series functions
library(tibble)      # For rownames_to_column

####################################################################
# Part 1: Data Loading And Exploration
####################################################################

# setting directory
getwd()
# use relative paths instead of setwd
data_dir = "../data"
output_dir = "../output"
viz_dir = "../output/visualizations"

# reading the six datasets
mean_income = read.csv(file.path(data_dir, "mean-income-or-consumption-per-day.csv"))
median_income = read.csv(file.path(data_dir, "median-income-or-consumption-per-day.csv"))
poorest_decile = read.csv(file.path(data_dir, "the-poorest-decile.csv"))
richest_decile = read.csv(file.path(data_dir, "the-richest-decile.csv"))
num_below_poverty = read.csv(file.path(data_dir, "number-of-people-living-below-a-range-of-poverty-lines.csv"))
share_below_poverty = read.csv(file.path(data_dir, "share-of-population-living-below-a-range-of-poverty-lines.csv"))

# basic exploration of each dataset
head(mean_income)
summary(mean_income)
dim(mean_income)

head(median_income)
summary(median_income)
dim(median_income)

head(poorest_decile)
summary(poorest_decile)
dim(poorest_decile)

head(richest_decile)
summary(richest_decile)
dim(richest_decile)

head(num_below_poverty)
summary(num_below_poverty)
dim(num_below_poverty)

head(share_below_poverty)
summary(share_below_poverty)
dim(share_below_poverty)

# checking for missing values
sum(is.na(mean_income))
sum(is.na(median_income))
sum(is.na(poorest_decile))
sum(is.na(richest_decile))
sum(is.na(num_below_poverty))
sum(is.na(share_below_poverty))

####################################################################
# Part 2: Data Integration
####################################################################

# merging income distribution metrics (mean, median, poorest and richest deciles)
income_distribution = mean_income %>%
  inner_join(median_income, by = c("Country", "Year")) %>%
  inner_join(poorest_decile, by = c("Country", "Year")) %>%
  inner_join(richest_decile, by = c("Country", "Year"))

# renaming columns for clarity
names(income_distribution) = c("Country", "Year", "Mean_Income", "Median_Income", 
                               "Poorest_Decile", "Richest_Decile")

# checking the merged dataset
head(income_distribution)
dim(income_distribution)

# calculating inequality metrics
income_distribution = income_distribution %>%
  mutate(
    Mean_to_Median_Ratio = Mean_Income / Median_Income,
    Richest_to_Poorest_Ratio = Richest_Decile / Poorest_Decile,
    Top_Bottom_Gap = Richest_Decile - Poorest_Decile
  )

# checking our inequality metrics
summary(income_distribution$Mean_to_Median_Ratio)
summary(income_distribution$Richest_to_Poorest_Ratio)
summary(income_distribution$Top_Bottom_Gap)

####################################################################
# Part 3: Correlation Analysis
####################################################################

# step 1: prepare correlation data
# first merge poverty data with income distribution data
poverty_income = income_distribution %>%
  inner_join(share_below_poverty %>% 
              dplyr::select(Country, Year, "Share.below..2.15.a.day") %>%
              rename(Extreme_Poverty_Share = "Share.below..2.15.a.day"),
            by = c("Country", "Year"))

# now prepare correlation data
correlation_data = poverty_income %>%
  dplyr::select(
    Extreme_Poverty_Share,
    Mean_Income,
    Richest_to_Poorest_Ratio,
    Mean_to_Median_Ratio
  )

# step 2: calculate correlation matrix
correlation_matrix = cor(correlation_data, use = "complete.obs")
print("correlation matrix:")
print(correlation_matrix)

# step 3: correlation significance tests
correlation_tests = list()
for(i in 1:ncol(correlation_data)) {
  for(j in 1:ncol(correlation_data)) {
    if(i != j) {
      test = cor.test(correlation_data[,i], correlation_data[,j])
      correlation_tests[[paste(colnames(correlation_data)[i], 
                             colnames(correlation_data)[j], 
                             sep = "_")]] = test
    }
  }
}

# step 4: visualize correlations
# prepare correlation data for ggplot
correlation_long = as.data.frame(correlation_matrix) %>%
  rownames_to_column("Variable1") %>%
  pivot_longer(-Variable1, names_to = "Variable2", values_to = "Correlation") %>%
  mutate(
    Variable1 = factor(Variable1, levels = rev(rownames(correlation_matrix))),
    Variable2 = factor(Variable2, levels = colnames(correlation_matrix))
  )

# create correlation heatmap using ggplot2
correlation_heatmap = ggplot(correlation_long, aes(x = Variable2, y = Variable1, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                      midpoint = 0, limit = c(-1, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Matrix of Key Variables")

# save correlation heatmap
ggsave(file.path(viz_dir, "correlation_heatmap.png"), 
       correlation_heatmap, 
       width = 10, 
       height = 8, 
       dpi = 300)
print(correlation_heatmap)

# create scatter plot matrix using ggplot2
# prepare data for scatter plot matrix
scatter_data = correlation_data %>%
  pivot_longer(everything(), 
               names_to = "Variable", 
               values_to = "Value")

# create scatter plot matrix
scatter_matrix = ggplot(scatter_data, aes(x = Value)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  facet_wrap(~ Variable, scales = "free") +
  theme_minimal() +
  labs(title = "Distribution of Variables",
       x = "Value",
       y = "Count")

# save scatter matrix
ggsave(file.path(viz_dir, "variable_distributions.png"), 
       scatter_matrix, 
       width = 12, 
       height = 8, 
       dpi = 300)

# print the scatter matrix
print(scatter_matrix)

# step 5: partial correlations
# install and load ppcor if not already installed
if (!require("ppcor")) {
  install.packages("ppcor")
  library(ppcor)
}

partial_cor = pcor(correlation_data)
print("Partial Correlation Matrix:")
print(partial_cor$estimate)

# step 6: non-parametric correlations
spearman_cor = cor(correlation_data, method = "spearman")
print("spearman correlation matrix:")
print(spearman_cor)

# step 7: correlation by region
regional_correlations = list()
for(region in unique(poverty_income$Region)) {
  region_data = poverty_income %>%
    filter(Region == region) %>%
    dplyr::select(
      Extreme_Poverty_Share,
      Mean_Income,
      Richest_to_Poorest_Ratio,
      Mean_to_Median_Ratio
    )
  
  if(nrow(region_data) > 1) {
    regional_correlations[[region]] = cor(region_data, use = "complete.obs")
  }
}

# step 8: correlation stability analysis
# calculate rolling correlations
library(zoo)
rolling_cor = rollapply(correlation_data,
                       width = 10,
                       function(x) cor(x)[1,2],
                       by.column = FALSE)

# step 9: results summary
cat("\ncorrelation analysis results:\n")
cat("1. pearson correlations:\n")
for(i in 1:ncol(correlation_matrix)) {
  for(j in 1:ncol(correlation_matrix)) {
    if(i < j) {
      cat("   -", colnames(correlation_matrix)[i], "vs", 
          colnames(correlation_matrix)[j], ":", 
          round(correlation_matrix[i,j], 3), "\n")
    }
  }
}

cat("\n2. significant correlations (p < 0.05):\n")
for(test in names(correlation_tests)) {
  if(correlation_tests[[test]]$p.value < 0.05) {
    cat("   -", test, ":", 
        round(correlation_tests[[test]]$estimate, 3),
        "(p =", round(correlation_tests[[test]]$p.value, 3), ")\n")
  }
}

cat("\n3. regional correlation patterns:\n")
for(region in names(regional_correlations)) {
  cat("   -", region, ":\n")
  cat("     poverty-income correlation:", 
      round(regional_correlations[[region]][1,2], 3), "\n")
  cat("     poverty-inequality correlation:", 
      round(regional_correlations[[region]][1,3], 3), "\n")
}

cat("\n4. correlation stability:\n")
cat("   - rolling correlation range:", 
    round(min(rolling_cor, na.rm = TRUE), 3), "to",
    round(max(rolling_cor, na.rm = TRUE), 3), "\n")
cat("   - correlation volatility:", 
    round(sd(rolling_cor, na.rm = TRUE), 3), "\n")

####################################################################
# Part 4: Trend Analysis Of Income Distribution
####################################################################

# function to calculate global average for each year
calculate_yearly_avg = function(data, metric) {
  yearly_avg = data %>%
    group_by(Year) %>%
    summarize(
      Average = mean(!!sym(metric), na.rm = TRUE),
      Median = median(!!sym(metric), na.rm = TRUE),
      Count = n()
    )
  return(yearly_avg)
}

# calculating global trends for inequality metrics
mean_median_trend = calculate_yearly_avg(income_distribution, "Mean_to_Median_Ratio")
rich_poor_trend = calculate_yearly_avg(income_distribution, "Richest_to_Poorest_Ratio")

# let's look at the trends
head(mean_median_trend)
head(rich_poor_trend)

# plotting the trend of mean-to-median ratio
mean_median_plot = ggplot(mean_median_trend, aes(x = Year, y = Average)) +
  geom_line() +
  geom_point() +
  labs(title = "Global Trend in Mean-to-Median Income Ratio",
       x = "Year",
       y = "Average Mean-to-Median Ratio") +
  theme_minimal()

# save mean-median plot
ggsave(file.path(viz_dir, "mean_median_trend.png"), 
       mean_median_plot, 
       width = 10, 
       height = 6, 
       dpi = 300)
print(mean_median_plot)

# plotting the trend of rich-to-poor ratio
rich_poor_plot = ggplot(rich_poor_trend, aes(x = Year, y = Average)) +
  geom_line() +
  geom_point() +
  labs(title = "Global Trend in Richest-to-Poorest Decile Ratio",
       x = "Year",
       y = "Average Richest-to-Poorest Ratio") +
  theme_minimal()

# save rich-poor plot
ggsave(file.path(viz_dir, "rich_poor_trend.png"), 
       rich_poor_plot, 
       width = 10, 
       height = 6, 
       dpi = 300)
print(rich_poor_plot)

####################################################################
# Part 5: Country-Specific Trends Analysis
####################################################################

# analyzing income distribution trends for selected countries
selected_countries = c("United States", "China", "India", "Brazil", "Germany", "Nigeria")

# filtering data for selected countries
selected_data = income_distribution %>%
  filter(Country %in% selected_countries)

# plotting mean-to-median ratio trends for selected countries
country_mean_median_plot = ggplot(selected_data, aes(x = Year, y = Mean_to_Median_Ratio, color = Country)) +
  geom_line() +
  geom_point() +
  labs(title = "Mean-to-Median Income Ratio Trends",
       x = "Year",
       y = "Mean-to-Median Ratio") +
  theme_minimal() +
  theme(legend.position = "bottom")

# save country mean-median plot
ggsave(file.path(viz_dir, "country_mean_median_trends.png"), 
       country_mean_median_plot, 
       width = 12, 
       height = 8, 
       dpi = 300)
print(country_mean_median_plot)

# plotting richest-to-poorest ratio trends for selected countries
country_rich_poor_plot = ggplot(selected_data, aes(x = Year, y = Richest_to_Poorest_Ratio, color = Country)) +
  geom_line() +
  geom_point() +
  labs(title = "Richest-to-Poorest Decile Ratio Trends",
       x = "Year",
       y = "Richest-to-Poorest Ratio") +
  theme_minimal() +
  theme(legend.position = "bottom")

# save country rich-poor plot
ggsave(file.path(viz_dir, "country_rich_poor_trends.png"), 
       country_rich_poor_plot, 
       width = 12, 
       height = 8, 
       dpi = 300)
print(country_rich_poor_plot)

####################################################################
# Part 6: Poverty Analysis
####################################################################

# focusing on extreme poverty (below $2.15 a day)
# selecting just the relevant columns
extreme_poverty_share = share_below_poverty %>%
  dplyr::select(Country, Year, "Share.below..2.15.a.day")

# renaming for clarity
names(extreme_poverty_share)[3] = "Extreme_Poverty_Share"

# merging income distribution with poverty data
poverty_income = income_distribution %>%
  inner_join(extreme_poverty_share, by = c("Country", "Year"))

# checking the merged dataset
head(poverty_income)
dim(poverty_income)

# creating scatterplots to examine relationships
# 1. mean income vs. extreme poverty
income_poverty_plot = ggplot(poverty_income, aes(x = Mean_Income, y = Extreme_Poverty_Share)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE) +
  scale_x_log10(labels = dollar_format()) +
  labs(title = "Relationship Between Mean Income and Extreme Poverty",
       x = "Mean Income (log scale)",
       y = "Share Below $2.15/day (%)") +
  theme_minimal()

# save income-poverty plot
ggsave(file.path(viz_dir, "income_poverty_relationship.png"), 
       income_poverty_plot, 
       width = 10, 
       height = 6, 
       dpi = 300)
print(income_poverty_plot)

# 2. inequality vs. extreme poverty
inequality_poverty_plot = ggplot(poverty_income, aes(x = Richest_to_Poorest_Ratio, y = Extreme_Poverty_Share)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "Relationship Between Inequality and Extreme Poverty",
       x = "Richest-to-Poorest Decile Ratio",
       y = "Share Below $2.15/day (%)") +
  theme_minimal()

# save inequality-poverty plot
ggsave(file.path(viz_dir, "inequality_poverty_relationship.png"), 
       inequality_poverty_plot, 
       width = 10, 
       height = 6, 
       dpi = 300)
print(inequality_poverty_plot)

####################################################################
# Part 7: Poverty Reduction Success Analysis
####################################################################

# identifying countries with substantial data over time
country_years = poverty_income %>%
  group_by(Country) %>%
  summarize(
    Years = n(),
    Min_Year = min(Year),
    Max_Year = max(Year),
    Year_Range = Max_Year - Min_Year
  ) %>%
  filter(Years >= 5, Year_Range >= 10)

# analyzing poverty reduction for countries with sufficient data
poverty_reduction = poverty_income %>%
  filter(Country %in% country_years$Country) %>%
  group_by(Country) %>%
  arrange(Country, Year) %>%
  mutate(
    Initial_Poverty = first(Extreme_Poverty_Share),
    Final_Poverty = last(Extreme_Poverty_Share),
    Absolute_Reduction = Initial_Poverty - Final_Poverty,
    Relative_Reduction = (Initial_Poverty - Final_Poverty) / Initial_Poverty * 100,
    Initial_Year = first(Year),
    Final_Year = last(Year),
    Year_Range = Final_Year - Initial_Year,
    Annual_Reduction = Absolute_Reduction / Year_Range
  ) %>%
  dplyr::select(Country, Initial_Year, Final_Year, Initial_Poverty, Final_Poverty, 
                Absolute_Reduction, Relative_Reduction, Annual_Reduction) %>%
  distinct(Country, .keep_all = TRUE) %>%
  arrange(desc(Relative_Reduction))

# viewing the most successful countries in poverty reduction
head(poverty_reduction, 10)

# plotting top 10 countries by relative poverty reduction
top_reducers = head(poverty_reduction, 10)

poverty_reduction_plot = ggplot(top_reducers, aes(x = reorder(Country, Relative_Reduction), y = Relative_Reduction)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Countries by Relative Poverty Reduction",
       x = "",
       y = "Relative Reduction in Extreme Poverty (%)") +
  theme_minimal()

# save poverty reduction plot
ggsave(file.path(viz_dir, "top_poverty_reducers.png"), 
       poverty_reduction_plot, 
       width = 10, 
       height = 6, 
       dpi = 300)
print(poverty_reduction_plot)

####################################################################
# Part 8: Statistical Inference And Hypothesis Testing
####################################################################

# hypothesis testing for income inequality
# h0: mean-to-median ratio = 1 (perfect equality)
# h1: mean-to-median ratio > 1 (inequality exists)
t_test_inequality = t.test(income_distribution$Mean_to_Median_Ratio, mu = 1, alternative = "greater")
print(t_test_inequality)

# creating regional groupings first
poverty_income = poverty_income %>%
  mutate(Region = case_when(
    Country %in% c("China", "Japan", "South Korea", "Vietnam") ~ "East Asia",
    Country %in% c("India", "Pakistan", "Bangladesh", "Sri Lanka") ~ "South Asia",
    Country %in% c("Nigeria", "South Africa", "Kenya", "Ethiopia") ~ "Sub-Saharan Africa",
    Country %in% c("Brazil", "Mexico", "Argentina", "Colombia") ~ "Latin America",
    Country %in% c("Germany", "France", "United Kingdom", "Italy") ~ "Europe",
    TRUE ~ "Other"
  ))

# additional hypothesis tests
# test for difference in poverty rates between regions
region_test = aov(Extreme_Poverty_Share ~ Region, data = poverty_income)
summary(region_test)

# test for correlation between income and poverty
cor_test = cor.test(poverty_income$Mean_Income, poverty_income$Extreme_Poverty_Share)
print(cor_test)

# confidence intervals for poverty reduction
poverty_ci = poverty_income %>%
  group_by(Year) %>%
  filter(n() > 1) %>%
  summarize(
    mean_poverty = mean(Extreme_Poverty_Share, na.rm = TRUE),
    se = sd(Extreme_Poverty_Share, na.rm = TRUE) / sqrt(n()),
    ci_lower = ifelse(n() > 1, mean_poverty - qt(0.975, n()-1) * se, mean_poverty),
    ci_upper = ifelse(n() > 1, mean_poverty + qt(0.975, n()-1) * se, mean_poverty)
  )

# plotting poverty trends with confidence intervals
poverty_ci_plot = ggplot(poverty_ci, aes(x = Year, y = mean_poverty)) +
  geom_line() +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2) +
  labs(title = "Global Poverty Trends with 95% Confidence Intervals",
       x = "Year",
       y = "Mean Poverty Rate (%)") +
  theme_minimal()

# save poverty ci plot
ggsave(file.path(viz_dir, "poverty_trends_with_ci.png"), 
       poverty_ci_plot, 
       width = 10, 
       height = 6, 
       dpi = 300)
print(poverty_ci_plot)

####################################################################
# Part 9: Time Series Analysis
####################################################################

# step 1: data preparation and initial visualization
# calculate global averages over time
global_trends = poverty_income %>%
  group_by(Year) %>%
  summarize(
    mean_poverty = mean(Extreme_Poverty_Share, na.rm = TRUE),
    mean_income = mean(Mean_Income, na.rm = TRUE),
    mean_inequality = mean(Richest_to_Poorest_Ratio, na.rm = TRUE),
    n_countries = n()
  )

# visualize global trends
par(mfrow = c(2, 2))
plot(global_trends$Year, global_trends$mean_poverty, type = "l",
     main = "Global Poverty Trend", xlab = "Year", ylab = "Poverty Rate (%)")
plot(global_trends$Year, global_trends$mean_income, type = "l",
     main = "Global Income Trend", xlab = "Year", ylab = "Mean Income")
plot(global_trends$Year, global_trends$mean_inequality, type = "l",
     main = "Global Inequality Trend", xlab = "Year", ylab = "Inequality Ratio")
plot(global_trends$Year, global_trends$n_countries, type = "l",
     main = "Number of Countries Over Time", xlab = "Year", ylab = "Count")
par(mfrow = c(1, 1))

# step 2: time series decomposition
# ensure we have enough data points for decomposition
if(nrow(global_trends) >= 2) {
  # create time series object with appropriate frequency
  # for annual data, we'll use frequency = 1
  poverty_ts = ts(global_trends$mean_poverty, 
                 start = min(global_trends$Year),
                 end = max(global_trends$Year),
                 frequency = 1)
  
  # perform decomposition using a moving average approach
  # calculate trend using moving average
  trend = ma(poverty_ts, order = 3, centre = TRUE)
  
  # calculate detrended series
  detrended = poverty_ts - trend
  
  # prepare data for ggplot
  decomposition_data = data.frame(
    Year = global_trends$Year,
    Original = as.numeric(poverty_ts),
    Trend = as.numeric(trend),
    Random = as.numeric(detrended)
  )
  
  # create long format data for plotting
  decomposition_long = decomposition_data %>%
    pivot_longer(
      cols = c(Original, Trend, Random),
      names_to = "Component",
      values_to = "Value"
    )
  
  # create decomposition plot using ggplot2
  decomposition_plot = ggplot(decomposition_long, aes(x = Year, y = Value)) +
    geom_line() +
    facet_wrap(~ Component, ncol = 1, scales = "free_y") +
    labs(title = "Time Series Decomposition",
         x = "Year",
         y = "Value") +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 12, face = "bold"),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 10),
      panel.spacing = unit(1, "lines")
    )
  
  # print the plot
  print(decomposition_plot)
  
  # calculate and print trend statistics
  trend_stats = data.frame(
    Component = c("Original", "Trend", "Random"),
    Mean = c(mean(poverty_ts, na.rm = TRUE),
             mean(trend, na.rm = TRUE),
             mean(detrended, na.rm = TRUE)),
    SD = c(sd(poverty_ts, na.rm = TRUE),
           sd(trend, na.rm = TRUE),
           sd(detrended, na.rm = TRUE))
  )
  
  print("Time Series Decomposition Statistics:")
  print(trend_stats)
  
} else {
  cat("Insufficient data points for time series decomposition.\n")
}

# step 3: trend analysis
# fit linear and polynomial models to poverty trend
linear_trend = lm(mean_poverty ~ Year, data = global_trends)
poly_trend = lm(mean_poverty ~ poly(Year, 2), data = global_trends)

# compare models
trend_comparison = data.frame(
  model = c("Linear", "Polynomial"),
  aic = c(AIC(linear_trend), AIC(poly_trend)),
  adj_r_squared = c(summary(linear_trend)$adj.r.squared,
                   summary(poly_trend)$adj.r.squared)
)
print("Trend Model Comparison:")
print(trend_comparison)

# visualize trend fits
plot(global_trends$Year, global_trends$mean_poverty, 
     main = "Poverty Trend with Fitted Models",
     xlab = "Year", ylab = "Poverty Rate (%)")
lines(global_trends$Year, predict(linear_trend), col = "red")
lines(global_trends$Year, predict(poly_trend), col = "blue")
legend("topright", legend = c("Data", "Linear", "Polynomial"),
       col = c("black", "red", "blue"), lty = 1)

# step 4: regional analysis
# calculate regional trends
regional_trends = poverty_income %>%
  group_by(Region, Year) %>%
  summarize(
    mean_poverty = mean(Extreme_Poverty_Share, na.rm = TRUE),
    mean_income = mean(Mean_Income, na.rm = TRUE),
    mean_inequality = mean(Richest_to_Poorest_Ratio, na.rm = TRUE),
    n_countries = n()
  )

# visualize regional trends
ggplot(regional_trends, aes(x = Year, y = mean_poverty, color = Region)) +
  geom_line() +
  labs(title = "Poverty Trends by Region",
       x = "Year",
       y = "Poverty Rate (%)") +
  theme_minimal()

# step 5: statistical tests for regional differences
# anova test for regional differences
regional_anova = aov(Extreme_Poverty_Share ~ Region, data = poverty_income)
summary(regional_anova)

# tukey's hsd test for pairwise comparisons
tukey_test = TukeyHSD(regional_anova)
print("Regional Pairwise Comparisons:")
print(tukey_test)

# visualize tukey's hsd results
par(mar = c(5, 8, 4, 2))  # adjust margins for better label visibility
plot(tukey_test, las = 2)
title("Tukey's HSD Test Results", line = 1)
par(mar = c(5, 4, 4, 2))  # reset margins to default

# step 6: trend strength analysis
# install and load trend package if not already installed
if (!require("trend")) {
  install.packages("trend")
  library(trend)
}

# calculate trend strength using mann-kendall test
mk_test = mk.test(global_trends$mean_poverty)
print("Mann-Kendall Trend Test:")
print(mk_test)

# calculate trend magnitude using sen's slope
sen_slope = sens.slope(global_trends$mean_poverty)
print("Sen's Slope Estimate:")
print(sen_slope)

# calculate trend strength metrics
trend_strength = list(
  mk_test = mk_test,
  sen_slope = sen_slope,
  direction = ifelse(sen_slope$estimates < 0, "Decreasing", "Increasing"),
  magnitude = abs(sen_slope$estimates),
  significance = ifelse(mk_test$p.value < 0.05, "Significant", "Not Significant")
)

# visualize trend with confidence intervals
trend_plot = ggplot(global_trends, aes(x = Year, y = mean_poverty)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Poverty Rate Trend with 95% Confidence Interval",
       x = "Year",
       y = "Poverty Rate (%)") +
  theme_minimal()

# save trend plot
ggsave(file.path(viz_dir, "poverty_trend.png"), 
       trend_plot, 
       width = 10, 
       height = 6, 
       dpi = 300)
print(trend_plot)

# step 7: forecasting
# fit arima model for forecasting
library(forecast)
poverty_arima = auto.arima(poverty_ts)
print("ARIMA Model Summary:")
print(summary(poverty_arima))

# generate forecasts
forecast_poverty = forecast(poverty_arima, h = 5)

# save forecast plot
png(file.path(viz_dir, "poverty_forecast.png"), 
    width = 10, 
    height = 6, 
    units = "in", 
    res = 300)
plot(forecast_poverty, 
     main = "Poverty Rate Forecast",
     xlab = "Year", 
     ylab = "Poverty Rate (%)")
dev.off()

# print forecast plot
plot(forecast_poverty, 
     main = "Poverty Rate Forecast",
     xlab = "Year", 
     ylab = "Poverty Rate (%)")

# step 8: results summary
cat("\nTime Series Analysis Results:\n")
cat("1. Trend Analysis:\n")
cat("   - Best Trend Model:", 
    trend_comparison$model[which.min(trend_comparison$aic)], "\n")
cat("   - Trend Direction:", 
    ifelse(coef(linear_trend)[2] < 0, "Decreasing", "Increasing"), "\n")
cat("   - Trend Significance:", 
    ifelse(summary(linear_trend)$coefficients[2,4] < 0.05, "Significant", "Not Significant"), "\n\n")

cat("2. Regional Analysis:\n")
cat("   - Regional Differences:", 
    ifelse(summary(regional_anova)[[1]][1,5] < 0.05, "Significant", "Not Significant"), "\n")
cat("   - Number of Significant Pairwise Differences:", 
    sum(tukey_test$Region[,4] < 0.05), "\n\n")

cat("3. Trend Strength:\n")
cat("   - Mann-Kendall Test:", 
    ifelse(trend_strength$mk_test$p.value < 0.05, "Significant Trend", "No Significant Trend"), "\n")
cat("   - Sen's Slope:", round(trend_strength$sen_slope$estimates, 3), "Units Per Year\n")
cat("   - Trend Direction:", trend_strength$direction, "\n")
cat("   - Trend Magnitude:", round(trend_strength$magnitude, 3), "Units Per Year\n\n")

cat("4. Forecasting:\n")
cat("   - Best ARIMA Model:", poverty_arima$arma[1], ",", poverty_arima$arma[2], "\n")
cat("   - Forecast Accuracy (MAPE):", 
    round(accuracy(poverty_arima)[5], 2), "%\n")

####################################################################
# Part 10: Regional And Comparative Analysis
####################################################################

# calculating regional statistics
regional_stats = poverty_income %>%
  group_by(Region, Year) %>%
  summarize(
    mean_poverty = mean(Extreme_Poverty_Share, na.rm = TRUE),
    median_poverty = median(Extreme_Poverty_Share, na.rm = TRUE),
    mean_inequality = mean(Richest_to_Poorest_Ratio, na.rm = TRUE),
    median_inequality = median(Richest_to_Poorest_Ratio, na.rm = TRUE),
    .groups = "drop"
  )

# plotting regional trends
regional_trends_plot = ggplot(regional_stats, aes(x = Year, y = mean_poverty, color = Region)) +
  geom_line() +
  labs(title = "Regional Poverty Trends",
       x = "Year",
       y = "Mean Poverty Rate (%)") +
  theme_minimal() +
  theme(legend.position = "bottom")

# save regional poverty trends plot
ggsave(file.path(viz_dir, "regional_poverty_trends.png"), 
       regional_trends_plot, 
       width = 12, 
       height = 8, 
       dpi = 300)
print(regional_trends_plot)

# regional inequality trends
regional_inequality_plot = ggplot(regional_stats, aes(x = Year, y = mean_inequality, color = Region)) +
  geom_line() +
  labs(title = "Regional Inequality Trends",
       x = "Year",
       y = "Mean Richest-to-Poorest Ratio") +
  theme_minimal() +
  theme(legend.position = "bottom")

# save regional inequality trends plot
ggsave(file.path(viz_dir, "regional_inequality_trends.png"), 
       regional_inequality_plot, 
       width = 12, 
       height = 8, 
       dpi = 300)
print(regional_inequality_plot)

####################################################################
# Part 11: Advanced Statistical Analysis
####################################################################

# calculating correlation matrix with all relevant variables
correlation_matrix = poverty_income %>%
  dplyr::select(Mean_Income, Median_Income, Richest_to_Poorest_Ratio, 
         Mean_to_Median_Ratio, Extreme_Poverty_Share) %>%
  cor(use = "complete.obs")

# creating correlation heatmap
correlation_long = as.data.frame(correlation_matrix) %>%
  rownames_to_column("Variable1") %>%
  pivot_longer(-Variable1, names_to = "Variable2", values_to = "Correlation")

advanced_correlation_plot = ggplot(correlation_long, aes(x = Variable1, y = Variable2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                      midpoint = 0, limit = c(-1, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Matrix of Key Variables")

# save advanced correlation matrix plot
ggsave(file.path(viz_dir, "advanced_correlation_matrix.png"), 
       advanced_correlation_plot, 
       width = 10, 
       height = 8, 
       dpi = 300)
print(advanced_correlation_plot)

# multiple regression with interaction terms
advanced_model = lm(Extreme_Poverty_Share ~ log(Mean_Income) * Richest_to_Poorest_Ratio + 
                     Mean_to_Median_Ratio, data = poverty_income)
summary(advanced_model)

# display diagnostic plots
par(mfrow = c(2, 2))
plot(advanced_model)
par(mfrow = c(1, 1))

# save diagnostic plots for advanced model
png(file.path(viz_dir, "advanced_model_diagnostics.png"), 
    width = 10, 
    height = 8, 
    units = "in", 
    res = 300)
par(mfrow = c(2, 2))
plot(advanced_model)
par(mfrow = c(1, 1))
dev.off()

####################################################################
# Part 12: Results Export And Summary
####################################################################

# creating comprehensive summary statistics
summary_stats = poverty_income %>%
  group_by(Year) %>%
  summarize(
    mean_poverty = mean(Extreme_Poverty_Share, na.rm = TRUE),
    median_poverty = median(Extreme_Poverty_Share, na.rm = TRUE),
    sd_poverty = sd(Extreme_Poverty_Share, na.rm = TRUE),
    mean_inequality = mean(Richest_to_Poorest_Ratio, na.rm = TRUE),
    median_inequality = median(Richest_to_Poorest_Ratio, na.rm = TRUE),
    sd_inequality = sd(Richest_to_Poorest_Ratio, na.rm = TRUE)
  )

# saving all results
write.csv(summary_stats, file.path(output_dir, "summary_statistics.csv"), row.names = FALSE)
write.csv(correlation_matrix, file.path(output_dir, "correlation_matrix.csv"), row.names = TRUE)
write.csv(regional_stats, file.path(output_dir, "regional_analysis.csv"), row.names = FALSE)

# printing final summary
cat("\nAnalysis Complete!\n")
cat("Results have been saved to the output directory.\n")
cat("Visualizations have been saved to the output/visualizations directory.\n")
cat("Key findings:\n")
cat("1. Global poverty trends show", 
    ifelse(tail(summary_stats$mean_poverty, 1) < head(summary_stats$mean_poverty, 1), 
           "a decreasing trend", "an increasing trend"), 
    "over the study period.\n")
cat("2. The correlation between inequality and poverty is", 
    round(correlation_matrix["Richest_to_Poorest_Ratio", "Extreme_Poverty_Share"], 3), "\n")
cat("3. Regional analysis shows", 
    regional_stats$Region[which.min(regional_stats$mean_poverty[regional_stats$Year == max(regional_stats$Year)])],
    "has the lowest poverty rates in the most recent year.\n")
cat("4. Time series decomposition indicates", 
    trend_strength$direction, "trend with", 
    ifelse(trend_strength$significance == "Significant", "significant", "non-significant"), 
    "change.\n")
cat("5. Advanced regression analysis shows", 
    ifelse(summary(advanced_model)$coefficients["log(Mean_Income):Richest_to_Poorest_Ratio", "Pr(>|t|)"] < 0.05,
           "a significant interaction between income and inequality",
           "no significant interaction between income and inequality"), ".\n")

# create a function to save base R plots
save_base_plot = function(plot_func, filename, width = 10, height = 6) {
  png(file.path(viz_dir, filename), 
      width = width, 
      height = height, 
      units = "in", 
      res = 300)
  plot_func()
  dev.off()
}

# save global trends plots
save_base_plot(
  function() {
    par(mfrow = c(2, 2))
    plot(global_trends$Year, global_trends$mean_poverty, type = "l",
         main = "Global Poverty Trend", xlab = "Year", ylab = "Poverty Rate (%)")
    plot(global_trends$Year, global_trends$mean_income, type = "l",
         main = "Global Income Trend", xlab = "Year", ylab = "Mean Income")
    plot(global_trends$Year, global_trends$mean_inequality, type = "l",
         main = "Global Inequality Trend", xlab = "Year", ylab = "Inequality Ratio")
    plot(global_trends$Year, global_trends$n_countries, type = "l",
         main = "Number of Countries Over Time", xlab = "Year", ylab = "Count")
    par(mfrow = c(1, 1))
  },
  "global_trends.png",
  width = 12,
  height = 10
)

# save trend fits plot
save_base_plot(
  function() {
    plot(global_trends$Year, global_trends$mean_poverty, 
         main = "Poverty Trend with Fitted Models",
         xlab = "Year", ylab = "Poverty Rate (%)")
    lines(global_trends$Year, predict(linear_trend), col = "red")
    lines(global_trends$Year, predict(poly_trend), col = "blue")
    legend("topright", legend = c("Data", "Linear", "Polynomial"),
           col = c("black", "red", "blue"), lty = 1)
  },
  "trend_fits.png"
)

# save tukey's hsd plot
save_base_plot(
  function() {
    par(mar = c(5, 8, 4, 2))
    plot(tukey_test, las = 2)
    title("Tukey's HSD Test Results", line = 1)
    par(mar = c(5, 4, 4, 2))
  },
  "tukey_hsd_results.png",
  width = 10,
  height = 8
)

# (removed redundant save of regional_poverty_trends.png; already created in part 10)

# (removed redundant save of regional_inequality_trends.png; already created in part 10)

# (removed redundant save of advanced_correlation_matrix.png; already created in part 11)