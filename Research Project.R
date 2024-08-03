library(readxl)
library(ggplot2)
library(tidyverse)
library(rstatix)
library(ggpubr)
library(stats)
library(tibble)
library(kableExtra)
library(ARTool)

# Read Excel

data <- read_excel("~/Documents/All Scores.xlsx")
data$Difficulty <- factor(data$Difficulty)
data$Order <- factor(data$Order)

# Filter Data

IES1_data <- data |>
  filter(Difficulty == 1)
IES2_data <- data |>
  filter(Difficulty == 2)
IES3_data <- data |>
  filter(Difficulty == 3)

# Descriptive statistics table for sleep 

sleep_summary <- IES1_data |>
  summarise(
    Median_ESS = median(ESS),
    IQR_ESS = IQR(ESS),
    Median_SCI = median(SCI),
    IQR_SCI = IQR(SCI)
  )|>
  pivot_longer(
    cols = everything(), 
    names_to = "Metric", 
    values_to = "Value"
  ) |>
  mutate(
    Variable = case_when(
      str_detect(Metric, "ESS") ~ "ESS",
      str_detect(Metric, "SCI") ~ "SCI"
    ),
    Statistic = case_when(
      str_detect(Metric, "Median") ~ "Median",
      str_detect(Metric, "IQR") ~ "IQR"
    )
  ) |>
  select(Variable, Statistic, Value) |>
  pivot_wider(
    names_from = Statistic, 
    values_from = Value
  )


sleep_table <- sleep_summary |>
  kable("html", caption = "ESS and SCI Descriptive Statistics") |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

sleep_table

# Descriptive statistics table for change in IES by difficulty level

change_summary <- data |>
  group_by(Difficulty) |>
  summarise(
    Median = median(Change),
    IQR = IQR(Change)
  )

change_table <- change_summary |>
  kable("html", caption = "Change in IES by Task Difficulty") |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

change_table

# Descriptive statistics for IES change by task order 

order_summary <- data |>
  group_by(Order) |>
  summarise(
    Median = median(Change),
    IQR = IQR(Change)
  )

order_table <- order_summary |>
  kable("html", caption = "Change in IES by Task Order") |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

order_table


# Histogram for assessing distribution of IES change

ggplot(data, aes(x = Change)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightgrey", color = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(data$Change), sd = sd(data$Change)), color = "red", size = 1) +
  labs(title = "Distribution of Change in IES", 
       x = "Values", y = "Density") +
  theme_minimal()

# Sleep Data Distribution Plots

ggplot(data = IES1_data, aes(x = ESS)) +
  geom_density(fill = "lightgrey", color = "white") +
  geom_vline(xintercept = median(IES1_data$ESS), color = "red", linetype = "solid", size = 0.5) +
  labs(title = "Distribution of ESS Scores",
       x = "ESS Score",
       y = "Density") + 
  theme_minimal()

ggplot(data = IES1_data, aes(x = SCI)) +
  geom_density(fill = "lightgrey", color = "white") +
  geom_vline(xintercept = median(IES1_data$SCI), color = "red", linetype = "solid", size = 0.5) +
  labs(title = "Distribution of SCI Scores",
       x = "SCI Score",
       y = "Density") + 
  theme_minimal()


# IES scores by day data plots

data_long <- data %>%
  pivot_longer(cols = starts_with("IES"), names_to = "Day", values_to = "IES")

data_long$Day <- factor(data_long$Day, levels = c("IES1", "IES2"), labels = c("Day 1", "Day 2"))

long1_data <- data_long |>
  filter(Difficulty == 1)
long2_data <- data_long |>
  filter(Difficulty == 2)
long3_data <- data_long |>
  filter(Difficulty == 3)

# IES Difficulty 1 Plot
ggplot(long1_data, aes(x = Day, y = IES)) +
  geom_boxplot() + 
  geom_jitter(width = 0.2, alpha = 0.5) +  
  stat_summary(fun = median, geom = "point", aes(color = "Median"), size = 3) + 
  stat_summary(fun = median, geom = "errorbar", aes(ymin = ..y.., ymax = ..y.., color = "Median"), width = 0.3, size = 1) + 
  scale_color_manual(name = "Statistics", values = c(Median = "red")) +
  labs(title = "IES for Task Difficulty 1", x = "Day", y = "Inverse Efficiency Score") +
  theme_minimal()
  
# IES Difficulty 2 Plot     
ggplot(long2_data, aes(x = Day, y = IES)) +
  geom_boxplot() + 
  geom_jitter(width = 0.2, alpha = 0.5) +  
  stat_summary(fun = median, geom = "point", aes(color = "Median"), size = 3) + 
  stat_summary(fun = median, geom = "errorbar", aes(ymin = ..y.., ymax = ..y.., color = "Median"), width = 0.3, size = 1) + 
  scale_color_manual(name = "Statistics", values = c(Median = "red")) +
  labs(title = "IES for Task Difficulty 2", x = "Day", y = "Inverse Efficiency Score") +
  theme_minimal()

# IES Difficulty 3 Plot
ggplot(long3_data, aes(x = Day, y = IES)) +
  geom_boxplot() + 
  geom_jitter(width = 0.2, alpha = 0.5) +  
  stat_summary(fun = median, geom = "point", aes(color = "Median"), size = 3) + 
  stat_summary(fun = median, geom = "errorbar", aes(ymin = ..y.., ymax = ..y.., color = "Median"), width = 0.3, size = 1) + 
  scale_color_manual(name = "Statistics", values = c(Median = "red")) +
  labs(title = "IES for Task Difficulty 3", x = "Day", y = "Inverse Efficiency Score") +
  theme_minimal()



# Analysis of IES change and sleep scores

# Calculate overall IES change plus sleep data 

overall_change <- data |>
  group_by(PID) |>
  summarise(Overall = mean(Change),
            ESS = first(ESS),
            SCI = first(SCI)
  )

# Overall change and ESS correlation

ESS_cor <- cor.test(overall_change$ESS, overall_change$Overall, method = "spearman")
print(ESS_cor)

# Overall change and SCI correlation

SCI_cor <- cor.test(overall_change$SCI, overall_change$Overall, method = "spearman")
print(SCI_cor)

# Plot overall change correlations

ggplot(overall_change, aes(x = ESS, y = Overall)) +
  geom_point() +
  geom_line(stat = "smooth", method = "lm") +
  labs(title = "Correlation between ESS Scores and Overall Change in IES",
       x = "ESS Scores",
       y = "Overall Change in IES") +
  theme_minimal()

ggplot(overall_change, aes(x = SCI, y = Overall)) +
  geom_point() +
  geom_line(stat = "smooth", method = "lm") +
  labs(title = "Correlation between SCI Scores and Overall Change in IES",
       x = "SCI Scores",
       y = "Overall Change in IES") +
  theme_minimal()



# Analysis of Performance Change and Difficulty

# Plot to visualize data

ggplot(data, aes(x = Difficulty, y = Change, group = Difficulty)) +
  geom_boxplot(fill = "white", outlier.colour = "red", outlier.shape = 16, outlier.size = 3) +
  geom_point(size = 1) +
  labs(
    title = "Change in IES by Difficulty",
    y = "Change",
    x = "Difficulty"
  )

# Outlier Check

data |>
  identify_outliers(Change)

ggqqplot(data, "Change", facet.by = "Difficulty")


# Friedman test 

friedman_result <- friedman_test(data, Change ~ Difficulty | PID)
friedman_result

# Pairwise Comparison (post-hoc test)

pwc <- pairwise.wilcox.test(data$Change, data$Difficulty, p.adjust.method = "bonferroni", paired = TRUE)

pwc <- data |>
  wilcox_test(Change ~ Difficulty, paired = TRUE, p.adjust.method = "bonferroni")

pwc

# Effect Size

data |>
  friedman_effsize(Change ~ Difficulty | PID)

# Plot results

pwc <- pwc |> add_xy_position(x = "Difficulty")

ggboxplot(data, x = "Difficulty", y = "Change", add = "point") +
  stat_pvalue_manual(pwc, label = "p.adj", hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(friedman_result, detailed = TRUE),
    caption = get_pwc_label(pwc),
    y = "Change in IES",
    x = "Task Difficulty Level"
  ) +
  theme_minimal()



# Analysis of last task performed

# Plot to visualize data

ggplot(data, aes(x = Order, y = Change, group = Order)) +
  geom_boxplot(fill = "white", outlier.colour = "red", outlier.shape = 16, outlier.size = 3) +
  geom_point(size = 1) +
  labs(
    title = "Change in IES by Task Order",
    y = "Change",
    x = "Order"
  )

# Outlier Check

data |>
  identify_outliers(Change)

ggqqplot(data, "Change", facet.by = "Order")


# Friedman test 

friedman2_result <- friedman_test(data, Change ~ Order | PID)
friedman2_result

# Mean ranks to assess the difference between first and second task 

 ranked_data <- data |>
   group_by(PID) |>
   mutate(rank = rank(Change)) |>
   ungroup()
 
 mean_ranks <- ranked_data |>
   group_by(Order) |>
   summarize(mean_rank = mean(rank))
 
print(mean_ranks)

# Pairwise Comparison (post-hoc test)

pwc2 <- pairwise.wilcox.test(data$Change, data$Order, p.adjust.method = "bonferroni", paired = TRUE)

pwc2 <- data |>
  wilcox_test(Change ~ Order, paired = TRUE, p.adjust.method = "bonferroni")

pwc2

# Effect Size

data |>
  friedman_effsize(Change ~ Order | PID)

# Plot results

pwc2 <- pwc2 |> add_xy_position(x = "Difficulty")

ggboxplot(data, x = "Order", y = "Change", add = "point") +
  stat_pvalue_manual(pwc2, label = "p.adj", hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(friedman2_result, detailed = TRUE),
    caption = get_pwc_label(pwc2),
    y = "Change in IES",
    x = "Task Order"
  ) +
  theme_minimal()




