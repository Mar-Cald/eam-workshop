# Simulated example data
set.seed(123)
n <- 1000
data <- data.frame(
  RT = sort(runif(n, 200, 2000)),  # RTs from 200 ms to 2000 ms
  correct = rbinom(n, 1, prob = pmin(0.5 + (1:n)/n * 0.5, 0.95)),  # Accuracy improves with RT
  ntrial = 1:n
  )


# Bin RTs (e.g., 50 ms bins)
library(dplyr)
data_binned <- data %>%
  mutate(bin = cut(RT, breaks = seq(200, 2000, by = 30))) %>%
  group_by(bin) %>%
  summarise(
    mean_RT = mean(RT),
    accuracy = mean(correct),
    n = n()
  ) %>%
  ungroup()

# Plot accuracy as a function of RT
library(ggplot2)
ggplot(data_binned, aes(x = mean_RT, y = accuracy)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
  labs(title = "Accuracy as a Function of RT",
       x = "Mean RT (ms)",
       y = "Proportion Correct")

# Identify the RT where accuracy rises above chance (50%)
# Find first RT bin where accuracy exceeds chance + a small margin (e.g., 0.55)
threshold_bin <- data_binned %>%
  filter(accuracy > 0.55) %>%
  slice(1)

chance_boundary <- threshold_bin$mean_RT
cat("Chance-performance RT boundary:", round(chance_boundary), "ms\n")

# Filter original data to exclude RTs below this point
data_filtered <- data %>%
  filter(RT >= chance_boundary)

