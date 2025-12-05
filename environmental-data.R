# STREAM synoptic (Buonpane thesis)
# Re-analysis 
# Environmental data

# setup environment----
library(ggplot2); library(tidyverse)

## import data----
env_data <- read.csv("env_data_no-prelim.csv", header = T)

# Exploratory plots----
## all variables----
xvar <- names(env_data)[1]

env_long <- env_data %>%
  pivot_longer(
    cols = -all_of(xvar),   # everything except the first column
    names_to = "variable",
    values_to = "value"
  )

# ggplot(env_long, aes_string(x = xvar, y = "value")) +
#   geom_point(alpha = 0.6) +
#   geom_smooth(method = "lm", se = FALSE, color = "blue") +
#   facet_wrap(~ variable, scales = "free_y") +
#   theme_bw() +
#   labs(x = xvar, y = "Value")

# Redoing to only add line when significant relationship 
# Fit linear models and extract slope p-values for each variable
lm_results <- env_data %>%
  pivot_longer(cols = -all_of(xvar), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarize(
    p_value = summary(lm(value ~ .data[[xvar]]))$coefficients[2, 4],
    r_squared = summary(lm(value ~ .data[[xvar]]))$r.squared
  )

lm_results %>%
  mutate(p_value = format(p_value, scientific = FALSE)) %>%
  print(n = 23)

env_long <- env_data %>%
  pivot_longer(cols = -all_of(xvar), names_to = "variable", values_to = "value") %>%
  left_join(lm_results, by = "variable")

# only plot line when sig
ggplot() +
  # points for all variables
  geom_point(data = env_long,
             aes_string(x = xvar, y = "value"),
             alpha = 0.6) +
  
  # regression line only for significant relationships
  geom_smooth(
    data = env_long %>% filter(p_value < 0.05),
    aes_string(x = xvar, y = "value"),
    method = "lm",
    se = FALSE,
    color = "blue"
  ) +
  
  facet_wrap(~ variable, scales = "free_y") +
  theme_bw() +
  labs(x = xvar, y = "Value")

# To test inidvidually to see what value we get
mod <- aov(data = env_data, Cl_mgClL ~ allhumn_upstream_avg)
summary(mod)
r.squared(mod)
summary(lm(Cl_mgClL ~ allhumn_upstream_avg, data = env_data))$r.squared

# Wetland----
## import data----
env_data <- read.csv("wetland_env_data_no-prelim_no-outlier.csv", header = T)

# Exploratory plots----
## all variables----
xvar <- names(env_data)[1]

env_long <- env_data %>%
  pivot_longer(
    cols = -all_of(xvar),   # everything except the first column
    names_to = "variable",
    values_to = "value"
  )

# ggplot(env_long, aes_string(x = xvar, y = "value")) +
#   geom_point(alpha = 0.6) +
#   geom_smooth(method = "lm", se = FALSE, color = "blue") +
#   facet_wrap(~ variable, scales = "free_y") +
#   theme_bw() +
#   labs(x = xvar, y = "Value")

# Redoing to only add line when significant relationship 
# Fit linear models and extract slope p-values for each variable
lm_results <- env_data %>%
  pivot_longer(cols = -all_of(xvar), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarize(
    p_value = summary(lm(value ~ .data[[xvar]]))$coefficients[2, 4],
    r_squared = summary(lm(value ~ .data[[xvar]]))$r.squared
  )

lm_results %>%
  mutate(p_value = format(p_value, scientific = FALSE)) %>%
  print(n = 23)

env_long <- env_data %>%
  pivot_longer(cols = -all_of(xvar), names_to = "variable", values_to = "value") %>%
  left_join(lm_results, by = "variable")

# only plot line when sig
ggplot() +
  # points for all variables
  geom_point(data = env_long,
             aes_string(x = xvar, y = "value"),
             alpha = 0.6) +
  
  # regression line only for significant relationships
  geom_smooth(
    data = env_long %>% filter(p_value < 0.05),
    aes_string(x = xvar, y = "value"),
    method = "lm",
    se = FALSE,
    color = "blue"
  ) +
  
  facet_wrap(~ variable, scales = "free_y") +
  theme_bw() +
  labs(x = xvar, y = "Value")

# To test inidvidually to see what value we get
mod <- aov(data = env_data, Cl_mgClL ~ allhumn_upstream_avg)
summary(mod)
r.squared(mod)
summary(lm(Cl_mgClL ~ allhumn_upstream_avg, data = env_data))$r.squared



