# ============================================================
# Data Analysis of 2013 New York City Flights
# Math 151: Fundamentals of Data Science
# Sections follow the report:
#   0. Setup & Background
#   1. Univariate Analyses (Q1-Q2)
#   2. Bivariate Analyses (Q3-Q9)
#   3. Multivariate Analyses (Q10-Q11)
#   4. Correlation Analysis (Q13)
#   5. Regression Analyses (simple, multiple, logistic)
#   6. Principal Component Analysis
#   7. Clustering (Hopkins, VAT, silhouette, hclust, k-means, k-medoids)
# ============================================================

# install.packages(c("nycflights13", "tidyverse", "corrplot", "car",
#                    "GGally", "factoextra", "cluster", "hopkins", "gridExtra"))

library(nycflights13)
library(tidyverse)
library(lubridate)
library(corrplot)
library(car)         # vif()
library(GGally)      # ggpairs()
library(factoextra)  # fviz_nbclust(), fviz_cluster(), fviz_dist()
library(cluster)     # pam(), silhouette()
library(hopkins)     # hopkins()
library(gridExtra)   # grid.arrange()

set.seed(151)

# ============================================================
# 0. Setup & Background
# ============================================================
data(flights)
dim(flights)            # 336,776 observations, 19 variables
head(flights)
str(flights)

# Missing values in the raw data
# (dep_time, dep_delay, arr_time, arr_delay, tailnum, air_time)
colSums(is.na(flights))

# Remove rows with missing departure/arrival delay (cancelled or incomplete flights)
flights_clean <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))

# After cleaning, no missing values should remain in any variable
colSums(is.na(flights_clean))
sum(is.na(flights_clean))

nrow(flights_clean)                                   # 327,346
round(nrow(flights_clean) / nrow(flights) * 100, 2)   # ~97.2% retained

str(flights_clean)

# Ten largest arrival delays
flights_clean %>%
  arrange(desc(arr_delay)) %>%
  head(10)

# Variable types
# Numerical: year, month, day, dep_time, sched_dep_time, dep_delay, arr_time,
#            sched_arr_time, arr_delay, air_time, distance, hour, minute, flight
# Categorical: carrier, tailnum, origin, dest, time_hour
# Response variable: arr_delay

# ============================================================
# 1. Univariate Analyses
# ============================================================

# ---------- Q1: Distribution of arrival delay ----------
ggplot(flights_clean, aes(x = arr_delay)) +
  geom_histogram(bins = 50, fill = "#A6C8E0", color = "black") +
  xlim(-20, 200) +
  labs(title = "Distribution of Arrival Delay",
       x = "Arrival Delay (minutes)", y = "Count")

ggplot(flights_clean, aes(x = "", y = arr_delay)) +
  geom_boxplot(fill = "#A8C5DA", outlier.alpha = 0.15, width = 0.35) +
  coord_cartesian(ylim = c(-50, 100)) +
  scale_y_continuous(breaks = seq(-50, 100, by = 5)) +
  labs(title = "Boxplot of Arrival Delay", x = "", y = "Arrival Delay (minutes)") +
  theme_minimal()

# Median ~ -5, IQR ~ (-17, 14), mean ~ 6.9  -> right-skewed (mean > median)
summary(flights_clean$arr_delay)
quantile(flights_clean$arr_delay, c(0.25, 0.5, 0.75))
sd(flights_clean$arr_delay)

# ---------- Q2: Distribution of departure delay ----------
ggplot(flights_clean, aes(x = dep_delay)) +
  geom_histogram(bins = 50, fill = "#A6C8E0", color = "black") +
  xlim(-20, 200) +
  labs(title = "Distribution of Departure Delay",
       x = "Departure Delay (minutes)", y = "Count")

ggplot(flights_clean, aes(x = "", y = dep_delay)) +
  geom_boxplot(fill = "#A8C5DA", outlier.alpha = 0.15, width = 0.35) +
  coord_cartesian(ylim = c(-50, 100)) +
  scale_y_continuous(breaks = seq(-50, 100, by = 5)) +
  labs(title = "Boxplot of Departure Delay", x = "", y = "Departure Delay (minutes)") +
  theme_minimal()

summary(flights_clean$dep_delay)
sd(flights_clean$dep_delay)   # compare spread with arr_delay (departure is smaller)
IQR(flights_clean$dep_delay); IQR(flights_clean$arr_delay)

# ============================================================
# 2. Bivariate Analyses
# ============================================================

# ---------- Q3: Departure delay vs arrival delay ----------
ggplot(flights_clean, aes(x = dep_delay, y = arr_delay)) +
  geom_point(alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Departure Delay vs Arrival Delay",
       x = "Departure Delay (minutes)", y = "Arrival Delay (minutes)")

cor(flights_clean$dep_delay, flights_clean$arr_delay)   # ~0.915

# Share of flights that "recover" time in the air (arrival delay < departure delay)
mean(flights_clean$arr_delay < flights_clean$dep_delay)

# ECDF plots
ggplot(flights_clean, aes(x = arr_delay)) +
  stat_ecdf(color = "#5A9BD5", linewidth = 1) +
  coord_cartesian(xlim = c(-60, 200)) +
  scale_x_continuous(breaks = seq(-60, 180, by = 30)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1)) +
  theme_minimal() +
  labs(title = "ECDF of Arrival Delay",
       x = "Arrival Delay (minutes)", y = "Cumulative Probability")

ggplot(flights_clean, aes(x = dep_delay)) +
  stat_ecdf(color = "#5A9BD5", linewidth = 1) +
  coord_cartesian(xlim = c(-60, 200)) +
  scale_x_continuous(breaks = seq(-60, 180, by = 30)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1)) +
  theme_minimal() +
  labs(title = "ECDF of Departure Delay",
       x = "Departure Delay (minutes)", y = "Cumulative Probability")

# Numbers quoted in the report (~60% little/no delay, 80-85% within 15-30 min)
mean(flights_clean$dep_delay <= 0)
mean(flights_clean$arr_delay <= 0)
mean(flights_clean$dep_delay <= 15); mean(flights_clean$dep_delay <= 30)
mean(flights_clean$arr_delay <= 15); mean(flights_clean$arr_delay <= 30)

# Normality was not formally tested: histogram and ECDF clearly show non-normality.

# ---------- Categorical variables ----------
# Tail number is only an identifier and is excluded.
unique(flights$origin)                 # "EWR" "LGA" "JFK"
length(unique(flights$origin))         # 3
length(unique(flights$dest))           # 105 destinations
length(unique(flights$carrier))        # 16 airlines

# ---------- Q4a: Origin airports with highest average delay ----------
origin_delay <- flights_clean %>%
  group_by(origin) %>%
  summarise(avg_dep_delay = mean(dep_delay),
            avg_arr_delay = mean(arr_delay),
            n = n()) %>%
  arrange(desc(avg_dep_delay))
origin_delay   # EWR ~15, JFK ~12, LGA ~10 (departure delay)

ggplot(origin_delay, aes(x = reorder(origin, avg_dep_delay), y = avg_dep_delay)) +
  geom_col(fill = "#91B9D8") +
  geom_text(aes(label = round(avg_dep_delay, 1)), vjust = -0.4) +
  labs(title = "Average Departure Delay by Origin Airport",
       x = "Origin", y = "Avg Departure Delay (minutes)") +
  theme_minimal()

# ---------- Q4b: Destination airports with highest average arrival delay ----------
dest_delay <- flights_clean %>%
  group_by(dest) %>%
  summarise(avg_dep_delay = mean(dep_delay),
            avg_arr_delay = mean(arr_delay),
            n = n()) %>%
  filter(n >= 500) %>%                 # only destinations with enough observations
  arrange(desc(avg_arr_delay))

head(dest_delay, 10)

ggplot(head(dest_delay, 15), aes(x = reorder(dest, avg_arr_delay), y = avg_arr_delay)) +
  geom_col(fill = "#91B9D8") +
  coord_flip() +
  labs(title = "Top 15 Destinations by Average Arrival Delay (n >= 500)",
       x = "Destination", y = "Avg Arrival Delay (minutes)") +
  theme_minimal()

# ---------- Q5: Airlines with highest average arrival delay ----------
carrier_delay <- flights_clean %>%
  group_by(carrier) %>%
  summarise(avg_dep_delay = mean(dep_delay),
            avg_arr_delay = mean(arr_delay),
            n = n()) %>%
  arrange(desc(avg_arr_delay))
carrier_delay   # F9, EV high; AS, HA low; note very different sample sizes (n)

ggplot(carrier_delay, aes(x = reorder(carrier, avg_dep_delay), y = avg_dep_delay)) +
  geom_col(fill = "#91B9D8") +
  coord_flip() +
  labs(title = "Average Departure Delay by Airline",
       x = "Carrier", y = "Avg Departure Delay (minutes)")

# NOTE: the original EDA script plotted avg_dep_delay on this chart (y mismatch);
# it is corrected here to avg_arr_delay.
ggplot(carrier_delay, aes(x = reorder(carrier, avg_arr_delay), y = avg_arr_delay)) +
  geom_col(fill = "#91B9D8") +
  coord_flip() +
  labs(title = "Average Arrival Delay by Airline",
       x = "Carrier", y = "Avg Arrival Delay (minutes)")

# Sample size per carrier (reliability of the comparison)
ggplot(carrier_delay, aes(x = reorder(carrier, n), y = n)) +
  geom_col(fill = "#C9ADA7") +
  coord_flip() +
  labs(title = "Number of Flights by Airline", x = "Carrier", y = "Flights")

# ---------- Q6: Variability of delays across origin / carrier / destination ----------
# Summaries of group means, computed for departure delay (reproduces the report's
# numbers: range 4.7 / 16.5 / 43.9 and SD 2.39 / 5.43 / 6.63) and for arrival delay.
group_variation <- function(data, delay_var) {
  summarise_group <- function(group_var, label) {
    data %>%
      group_by(.data[[group_var]]) %>%
      summarise(avg = mean(.data[[delay_var]]), .groups = "drop") %>%
      summarise(variable = label,
                groups   = n(),
                min_avg  = min(avg),
                max_avg  = max(avg),
                range_avg = max(avg) - min(avg),
                sd_avg   = sd(avg),
                iqr_avg  = IQR(avg))
  }
  bind_rows(summarise_group("origin",  "origin"),
            summarise_group("carrier", "carrier"),
            summarise_group("dest",    "destination"))
}

compare_group_variation_dep <- group_variation(flights_clean, "dep_delay")
compare_group_variation_arr <- group_variation(flights_clean, "arr_delay")
compare_group_variation_dep
compare_group_variation_arr

# ---------- Q7: Delay by time of day ----------
time_delay <- flights_clean %>%
  group_by(hour) %>%
  summarise(avg_dep_delay = mean(dep_delay),
            avg_arr_delay = mean(arr_delay),
            n = n())
time_delay

ggplot(time_delay, aes(x = hour, y = avg_arr_delay)) +
  geom_line() + geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_x_continuous(breaks = seq(6, 22, 2),
                     labels = paste0(seq(6, 22, 2), ":00")) +
  labs(title = "Average Arrival Delay by Time of Day",
       x = "Scheduled Departure Hour", y = "Average Arrival Delay (minutes)") +
  theme_minimal()

ggplot(time_delay, aes(x = hour, y = avg_dep_delay)) +
  geom_line() + geom_point() +
  scale_x_continuous(breaks = seq(6, 22, 2),
                     labels = paste0(seq(6, 22, 2), ":00")) +
  labs(title = "Average Departure Delay by Time of Day",
       x = "Scheduled Departure Hour", y = "Average Departure Delay (minutes)") +
  theme_minimal()

# ---------- Q8: Delay by season ----------
flights_clean <- flights_clean %>%
  mutate(
    season = case_when(
      month %in% c(12, 1, 2) ~ "Winter",
      month %in% c(3, 4, 5)  ~ "Spring",
      month %in% c(6, 7, 8)  ~ "Summer",
      month %in% c(9, 10, 11) ~ "Fall"
    ),
    season = factor(season, levels = c("Spring", "Summer", "Fall", "Winter"))
  )

season_delay <- flights_clean %>%
  group_by(season) %>%
  summarise(avg_dep_delay = mean(dep_delay),
            avg_arr_delay = mean(arr_delay),
            n = n()) %>%
  arrange(desc(avg_arr_delay))
season_delay   # Summer ~13.0 highest, Fall ~ -1.22 lowest

# Sample size balance across seasons (~77,000 to ~84,000)
season_sample_size <- flights_clean %>%
  count(season) %>%
  mutate(percent = n / sum(n) * 100)
season_sample_size

season_colors <- c("Spring" = "#A8D5BA", "Summer" = "#F4A261",
                   "Fall" = "#C9ADA7", "Winter" = "#A8C5DA")

ggplot(season_sample_size, aes(x = season, y = n, fill = season)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(n, " (", round(percent, 1), "%)")),
            vjust = -0.5, size = 4) +
  scale_fill_manual(values = season_colors) +
  labs(title = "Sample Size by Season", x = "Season", y = "Number of Flights") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(season_delay, aes(x = season, y = avg_arr_delay, fill = season)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = round(avg_arr_delay, 1)), vjust = -0.5, size = 5) +
  scale_fill_manual(values = season_colors) +
  labs(title = "Average Arrival Delay by Season",
       x = "Season", y = "Average Arrival Delay (minutes)") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(size = 16, face = "bold"))

# ---------- Q9: Delay vs distance (numerical variable) ----------
ggplot(flights_clean, aes(x = distance)) +
  geom_histogram(bins = 50, fill = "#4C78A8", color = "white") +
  labs(title = "Distribution of Flight Distance",
       x = "Distance (miles)", y = "Count") +
  theme_minimal()

ggplot(flights_clean, aes(x = distance, y = arr_delay)) +
  geom_point(alpha = 0.03, color = "#4C78A8") +
  geom_smooth(method = "lm", color = "#F4A261", linewidth = 1) +
  labs(title = "Arrival Delay vs Distance",
       x = "Distance (miles)", y = "Arrival Delay (minutes)") +
  theme_minimal()

cor(flights_clean$distance, flights_clean$arr_delay)
summary(lm(arr_delay ~ distance, data = flights_clean))   # nearly flat slope

# ============================================================
# 3. Multivariate Analyses
# ============================================================

# ---------- Q10: Arrival delay by airline and time of day ----------
plot_carrier_hour <- function(data, n_carriers) {
  top_carriers <- data %>%
    count(carrier, sort = TRUE) %>%
    slice_head(n = n_carriers) %>%
    pull(carrier)

  carrier_hour_delay <- data %>%
    filter(carrier %in% top_carriers) %>%
    group_by(carrier, hour) %>%
    summarise(avg_arr_delay = mean(arr_delay), n = n(), .groups = "drop") %>%
    filter(n >= 100)

  print(
    ggplot(carrier_hour_delay, aes(x = hour, y = avg_arr_delay, color = carrier)) +
      geom_line(linewidth = 1) +
      geom_point(size = 1.5) +
      scale_x_continuous(breaks = seq(6, 22, 2),
                         labels = paste0(seq(6, 22, 2), ":00")) +
      labs(title = paste0("Average Arrival Delay by Airline and Time of Day (top ",
                          n_carriers, " carriers)"),
           x = "Time of Day", y = "Average Arrival Delay (minutes)",
           color = "Carrier") +
      theme_minimal()
  )
  invisible(carrier_hour_delay)
}

# All 16 carriers first (check the pattern is not driven by a few airlines)
carrier_hour_all <- plot_carrier_hour(flights_clean, 16)
# Then the 8 largest carriers for a clearer picture
carrier_hour_top8 <- plot_carrier_hour(flights_clean, 8)

# Hour of peak delay for each carrier (e.g., UA ~18:00, US and EV ~17-19:00)
carrier_hour_all %>%
  group_by(carrier) %>%
  slice_max(avg_arr_delay, n = 1) %>%
  arrange(hour)

# ---------- Q11: Arrival delay by season and time of day ----------
season_hour_delay <- flights_clean %>%
  group_by(season, hour) %>%
  summarise(avg_arr_delay = mean(arr_delay), n = n(), .groups = "drop") %>%
  filter(n >= 100)
season_hour_delay

ggplot(season_hour_delay, aes(x = hour, y = avg_arr_delay, color = season)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  scale_color_manual(values = c("Spring" = "#2E8B57", "Summer" = "#F4A261",
                                "Fall" = "#8B6F6B", "Winter" = "#4C78A8")) +
  scale_x_continuous(breaks = seq(6, 22, 2),
                     labels = paste0(seq(6, 22, 2), ":00")) +
  labs(title = "Average Arrival Delay by Season and Time of Day",
       x = "Time of Day", y = "Average Arrival Delay (minutes)", color = "Season") +
  theme_minimal()

# Peak hour and peak delay in each season (Summer ~30 min; Fall stays low)
season_hour_delay %>%
  group_by(season) %>%
  slice_max(avg_arr_delay, n = 1)

# ============================================================
# 4. Correlation Analysis (Q13)
# ============================================================
num_data <- flights_clean %>%
  dplyr::select(dep_delay, arr_delay, month, day, dep_time, sched_dep_time,
                arr_time, sched_arr_time, flight, air_time, distance,
                hour, minute)

cor_matrix <- cor(num_data)
round(cor_matrix, 2)
corrplot(cor_matrix, method = "color", tl.col = "black")

# Strong pairs: dep_delay ~ arr_delay; time variables among themselves;
# air_time ~ distance. Weak: month, day, minute with delays.
cor_pairs <- as.data.frame(as.table(cor_matrix)) %>%
  filter(as.character(Var1) < as.character(Var2)) %>%
  arrange(desc(abs(Freq)))
head(cor_pairs, 15)

# ============================================================
# 5. Regression Analyses
# ============================================================

# ---------- 5.1 Simple linear regression: arr_delay ~ dep_delay ----------
slr <- lm(arr_delay ~ dep_delay, data = flights_clean)
summary(slr)
# intercept ~ -5.899, slope ~ 1.019, adjusted R^2 ~ 0.8369
coef(slr)
summary(slr)$adj.r.squared

ggplot(flights_clean, aes(x = dep_delay, y = arr_delay)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Simple Linear Regression: Arrival Delay ~ Departure Delay",
       x = "Departure Delay (minutes)", y = "Arrival Delay (minutes)") +
  theme_minimal()

# Residuals vs fitted values
slr_diag <- data.frame(fitted = fitted(slr), resid = resid(slr))
ggplot(slr_diag, aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.1) +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Residuals vs Fitted (Simple Linear Regression)",
       x = "Fitted values", y = "Residuals") +
  theme_minimal()

# Attempted remedies: square-root transform (after shifting to be positive)
# and polynomial regression
shift <- abs(min(flights_clean$arr_delay)) + 1
slr_sqrt <- lm(sqrt(arr_delay + shift) ~ dep_delay, data = flights_clean)
summary(slr_sqrt)$adj.r.squared

slr_poly2 <- lm(arr_delay ~ poly(dep_delay, 2), data = flights_clean)
slr_poly3 <- lm(arr_delay ~ poly(dep_delay, 3), data = flights_clean)
summary(slr_poly2)$adj.r.squared
summary(slr_poly3)$adj.r.squared

data.frame(
  model = c("Simple linear", "Sqrt(arr_delay + shift)", "Poly deg 2", "Poly deg 3"),
  adj_r2 = c(summary(slr)$adj.r.squared,
             summary(slr_sqrt)$adj.r.squared,
             summary(slr_poly2)$adj.r.squared,
             summary(slr_poly3)$adj.r.squared)
)
# R^2 values are very similar: transformations do not help much.

# ---------- 5.2 Multiple linear regression ----------
# Three models with an increasing number of predictors
mlr1 <- lm(arr_delay ~ dep_delay + distance, data = flights_clean)
mlr2 <- lm(arr_delay ~ dep_delay + distance + carrier + month, data = flights_clean)
mlr3 <- lm(arr_delay ~ distance + dep_delay + carrier + dest +
             sched_dep_time + month, data = flights_clean)   # best model

summary(mlr3)
summary(mlr3)$adj.r.squared      # ~0.8434

model_compare <- data.frame(
  model = c("mlr1: dep_delay + distance",
            "mlr2: + carrier + month",
            "mlr3: + dest + sched_dep_time"),
  n_params = c(length(coef(mlr1)), length(coef(mlr2)), length(coef(mlr3))),
  adj_r2 = c(summary(mlr1)$adj.r.squared,
             summary(mlr2)$adj.r.squared,
             summary(mlr3)$adj.r.squared),
  AIC = c(AIC(mlr1), AIC(mlr2), AIC(mlr3)),
  BIC = c(BIC(mlr1), BIC(mlr2), BIC(mlr3))
)
model_compare

# Nested F-tests between the models
anova(slr, mlr1, mlr2, mlr3)

# Significance of each predictor in the best model
Anova(mlr3, type = "II")

# Residuals vs fitted values (multiple regression)
mlr_diag <- data.frame(fitted = fitted(mlr3), resid = resid(mlr3))
ggplot(mlr_diag, aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.1) +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Residuals vs Fitted (Multiple Linear Regression)",
       x = "Fitted values", y = "Residuals") +
  theme_minimal()

# Collinearity: VIF
# car::vif() returns GVIF for factor terms (carrier, dest); the report's VIF for
# distance (~125) is flagged as problematic (distance is nearly determined by dest).
vif_values <- vif(mlr3)
vif_values

# Refit without dest to see how much distance's VIF drops
mlr3_no_dest <- lm(arr_delay ~ distance + dep_delay + carrier +
                     sched_dep_time + month, data = flights_clean)
vif(mlr3_no_dest)
summary(mlr3_no_dest)$adj.r.squared

# ---------- 5.3 Logistic regression ----------
# Binary response: 1 = arrived late, 0 = on time or early
flights_clean <- flights_clean %>%
  mutate(arr_delay_binary = ifelse(arr_delay > 0, 1, 0))
table(flights_clean$arr_delay_binary)
prop.table(table(flights_clean$arr_delay_binary))

# Null, simple and saturated models
logit_null   <- glm(arr_delay_binary ~ 1,
                    data = flights_clean, family = binomial(link = "logit"))
logit_simple <- glm(arr_delay_binary ~ dep_delay,
                    data = flights_clean, family = binomial(link = "logit"))
logit_sat    <- glm(arr_delay_binary ~ dep_delay + distance + carrier + dest +
                      sched_dep_time + month,
                    data = flights_clean, family = binomial(link = "logit"))

summary(logit_simple)
summary(logit_sat)

# Chi-squared (likelihood ratio) tests
anova(logit_null,   logit_simple, test = "Chisq")   # null vs simple
anova(logit_simple, logit_sat,    test = "Chisq")   # simple vs saturated
anova(logit_null,   logit_sat,    test = "Chisq")   # null vs saturated
anova(logit_null, logit_simple, logit_sat, test = "Chisq")  # sequential

# AIC comparison: null ~442238.2, simple ~298902.8, saturated ~293193.7
AIC(logit_null, logit_simple, logit_sat)

# Optional: classification accuracy of each model (0.5 cut-off)
accuracy <- function(model) {
  pred <- ifelse(predict(model, type = "response") > 0.5, 1, 0)
  mean(pred == flights_clean$arr_delay_binary)
}
data.frame(model = c("null", "simple", "saturated"),
           accuracy = c(accuracy(logit_null), accuracy(logit_simple), accuracy(logit_sat)))

# ============================================================
# 6. Principal Component Analysis
# ============================================================
# Variables: dep_delay, arr_delay, air_time, distance
pca_data <- flights_clean %>%
  dplyr::select(dep_delay, arr_delay, air_time, distance)

# Plot of paired data (sample for readability / speed)
pca_sample <- pca_data %>% slice_sample(n = 5000)
ggpairs(pca_sample,
        lower = list(continuous = wrap("points", alpha = 0.2, size = 0.4)),
        title = "Pairs Plot of dep_delay, arr_delay, air_time, distance")

# Mean and variance table (motivates scaling)
mean_var_table <- data.frame(
  variable = colnames(pca_data),
  mean     = sapply(pca_data, mean),
  variance = sapply(pca_data, var),
  row.names = NULL
)
mean_var_table

# Scale the data; eigen-decomposition of the correlation matrix R
pca_scaled <- scale(pca_data)
R <- cor(pca_data)            # equals cov(pca_scaled)
R
eig <- eigen(R)
eigenvalues  <- eig$values
eigenvectors <- eig$vectors
eigenvalues
eigenvectors

# Proportion of variance explained (PVE) and cumulative PVE
pve     <- eigenvalues / sum(eigenvalues)
cum_pve <- cumsum(pve)
pve_table <- data.frame(PC = paste0("PC", 1:4),
                        eigenvalue = eigenvalues,
                        PVE = pve,
                        cumulative_PVE = cum_pve)
pve_table      # PC1 ~51%, PC2 ~47%, cumulative ~98%

# Scree plot (PVE) and cumulative scree plot
p_scree <- ggplot(pve_table, aes(x = PC, y = PVE, group = 1)) +
  geom_line() + geom_point(size = 3) +
  labs(title = "Scree Plot (PVE)", x = "Principal Component",
       y = "Proportion of Variance Explained") +
  theme_minimal()

p_cum <- ggplot(pve_table, aes(x = PC, y = cumulative_PVE, group = 1)) +
  geom_line() + geom_point(size = 3) +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red") +
  labs(title = "Cumulative Scree Plot", x = "Principal Component",
       y = "Cumulative PVE") +
  theme_minimal()

grid.arrange(p_scree, p_cum, ncol = 2)

# Kaiser rule: keep components with eigenvalue > 1
sum(eigenvalues > 1)            # 2 components retained

# Loadings of the first two PCs
loadings_table <- data.frame(variable = colnames(pca_data),
                             PC1 = eigenvectors[, 1],
                             PC2 = eigenvectors[, 2])
loadings_table
# NOTE: eigenvector signs are arbitrary. The report shows PC1 loadings of air_time and
# distance as negative (~0.617, 0.623) and PC2 loadings of dep/arr delay positive
# (~0.624, 0.616). If your signs are flipped, multiply the PC by -1.
# PC1 ~ space-time scale of the flight; PC2 ~ overall delays.

# Cross-check with prcomp()
pca_fit <- prcomp(pca_data, scale. = TRUE)
summary(pca_fit)
pca_fit$rotation[, 1:2]
fviz_pca_var(pca_fit, repel = TRUE)
fviz_eig(pca_fit, addlabels = TRUE)

# Scores on PC1 / PC2
pca_scores <- as.data.frame(pca_fit$x[, 1:2])
ggplot(pca_scores %>% slice_sample(n = 5000), aes(PC1, PC2)) +
  geom_point(alpha = 0.3) +
  labs(title = "Flights on the First Two Principal Components") +
  theme_minimal()

# ============================================================
# 7. Clustering
# ============================================================
# 500 standardized observations, variables: dep_delay, arr_delay, air_time, distance
set.seed(151)
cluster_raw <- pca_data %>% slice_sample(n = 500)
cluster_x   <- as.data.frame(scale(cluster_raw))

# ---------- 7.1 Clustering tendency: Hopkins statistic ----------
# Close to 1 => high cluster tendency (report: ~0.99)
hopkins_values <- replicate(10, hopkins(cluster_x, m = ceiling(nrow(cluster_x) / 10)))
hopkins_values
mean(hopkins_values)

# ---------- VAT: flights data vs random uniform(0,1) data ----------
random_x <- as.data.frame(apply(cluster_x, 2, function(col) runif(length(col), 0, 1)))
colnames(random_x) <- colnames(cluster_x)

vat_flights <- fviz_dist(dist(cluster_x), show_labels = FALSE) +
  labs(title = "VAT: Flights Data")
vat_random  <- fviz_dist(dist(random_x), show_labels = FALSE) +
  labs(title = "VAT: Random Uniform(0,1) Data")
grid.arrange(vat_flights, vat_random, ncol = 2)

# ---------- 7.2 Choosing the number of clusters: average silhouette ----------
sil_kmeans <- fviz_nbclust(cluster_x, kmeans, method = "silhouette",
                           k.max = 10, nstart = 25) +
  labs(title = "Average Silhouette: K-means")
sil_pam    <- fviz_nbclust(cluster_x, pam, method = "silhouette", k.max = 10) +
  labs(title = "Average Silhouette: K-medoids (PAM)")
sil_hc     <- fviz_nbclust(cluster_x, hcut, method = "silhouette", k.max = 10,
                           hc_method = "average", hc_metric = "euclidean") +
  labs(title = "Average Silhouette: Agglomerative (average linkage)")
grid.arrange(sil_kmeans, sil_pam, sil_hc, ncol = 3)
# K-medoids suggests k = 2; K-means and agglomerative suggest k = 3 -> choose k = 3
k <- 3

cluster_palette <- c("#F4A261", "#4C78A8", "#E9C46A")  # orange, blue, yellow

# Helper: pair plot colored by cluster
pair_plot_cluster <- function(cluster_labels, title) {
  df <- cluster_x
  df$cluster <- factor(cluster_labels)
  ggpairs(df, columns = 1:4,
          aes(color = cluster, alpha = 0.6),
          upper = list(continuous = wrap("cor", size = 3)),
          title = title)
}

# ---------- 7.3 Agglomerative hierarchical clustering ----------
dist_euclid <- dist(cluster_x, method = "euclidean")
hc_avg <- hclust(dist_euclid, method = "average")
plot(hc_avg, labels = FALSE, hang = -1, main = "Agglomerative Clustering (average linkage)")
rect.hclust(hc_avg, k = k, border = "red")

hc_clusters <- cutree(hc_avg, k = k)
table(hc_clusters)

# Projection of clusters on a 2D plane
fviz_cluster(list(data = cluster_x, cluster = hc_clusters),
             palette = cluster_palette, ellipse.type = "convex",
             geom = "point", main = "Agglomerative: Projection of Clusters on a 2D Plane")

pair_plot_cluster(hc_clusters, "Paired Plots of Variables Colored by Agglomerative Clusters")

# ---------- 7.4 K-means ----------
set.seed(151)
km <- kmeans(cluster_x, centers = k, nstart = 25)
km$centers          # centers of clusters (standardized scale)
km$size             # K-means cluster sizes

# Centers on the original scale
aggregate(cluster_raw, by = list(cluster = km$cluster), FUN = mean)

p_km_circle <- fviz_cluster(km, data = cluster_x, palette = cluster_palette,
                            ellipse.type = "euclid", geom = "point",
                            main = "K-means with Geometric Circle")
p_km_shape  <- fviz_cluster(km, data = cluster_x, palette = cluster_palette,
                            ellipse.type = "convex", geom = "point",
                            main = "K-means: General Shape (no Geometric Circle)")
grid.arrange(p_km_circle, p_km_shape, ncol = 2)

pair_plot_cluster(km$cluster, "K-means Clustering Pair Plot")

# ---------- 7.5 K-medoids (PAM) ----------
set.seed(151)
pam_fit <- pam(cluster_x, k = k)
pam_fit$medoids         # cluster medoids
pam_fit$clusinfo        # cluster sizes
table(pam_fit$clustering)

p_pam_ellipse <- fviz_cluster(pam_fit, palette = cluster_palette,
                              ellipse.type = "norm", geom = "point",
                              main = "K-medoids with Ellipse Shape")
p_pam_none    <- fviz_cluster(pam_fit, palette = cluster_palette,
                              ellipse = FALSE, geom = "point",
                              main = "K-medoids without Ellipse")
grid.arrange(p_pam_ellipse, p_pam_none, ncol = 2)

pair_plot_cluster(pam_fit$clustering, "K-medoids Pair Plots")

# ---------- 7.6 Comparing the clusters ----------
# Side-by-side projections
p_hc <- fviz_cluster(list(data = cluster_x, cluster = hc_clusters),
                     palette = cluster_palette, geom = "point",
                     ellipse.type = "convex", main = "Agglomerative")
p_km <- fviz_cluster(km, data = cluster_x, palette = cluster_palette,
                     geom = "point", ellipse.type = "convex", main = "K-means")
p_pm <- fviz_cluster(pam_fit, palette = cluster_palette, geom = "point",
                     ellipse.type = "convex", main = "K-medoids")
grid.arrange(p_hc, p_km, p_pm, ncol = 3)

# Average silhouette width at k = 3 for each method
avg_sil <- function(cl) mean(silhouette(cl, dist_euclid)[, "sil_width"])
data.frame(method = c("Agglomerative", "K-means", "K-medoids"),
           avg_silhouette = c(avg_sil(hc_clusters), avg_sil(km$cluster),
                              avg_sil(pam_fit$clustering)))

# Cluster profiles on the original scale for each method
profile_table <- function(cl, method) {
  cluster_raw %>%
    mutate(cluster = cl) %>%
    group_by(cluster) %>%
    summarise(across(everything(), mean), n = n(), .groups = "drop") %>%
    mutate(method = method, .before = 1)
}
bind_rows(profile_table(hc_clusters, "Agglomerative"),
          profile_table(km$cluster, "K-means"),
          profile_table(pam_fit$clustering, "K-medoids"))

# Agreement between methods (cluster labels are arbitrary)
table(Agglomerative = hc_clusters, Kmeans = km$cluster)
table(Kmeans = km$cluster, Kmedoids = pam_fit$clustering)

# Conclusion (from the report): agglomerative and K-means separate both the
# space-time scale of flights and the level of delays; K-medoids mainly separates
# flight length only.
