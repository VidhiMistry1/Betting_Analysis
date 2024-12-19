

# PART 1

prob_red_sox_win_boston = 0.6
prob_yankees_win_ny = 0.57

# (i)
Probability_of_winning_at_home = 0.6
Probability_of_winning_away = 0.43
Probability_of_losing_at_home = 1 - Probability_of_winning_at_home

Probability_of_winning_both_games = Probability_of_winning_at_home * (1 - 0.57) # Win both games (home, away)
Probability_of_winning_both_games

Probability_of_win_lose_win = Probability_of_winning_at_home * 0.57 * Probability_of_winning_at_home # Win home, lose away, win home
Probability_of_win_lose_win
Probability_of_lose_win_win = Probability_of_losing_at_home * Probability_of_winning_away * Probability_of_winning_at_home # Lose home, win away, win home
Probability_of_lose_win_win

P_WW <- Probability_of_winning_at_home * (1 - prob_yankees_win_ny)
P_WLW <- Probability_of_winning_at_home * prob_yankees_win_ny * Probability_of_winning_at_home
P_LWW <- Probability_of_losing_at_home * Probability_of_winning_away * Probability_of_winning_at_home

Probbility_of_wining_the_series = P_WW + P_WLW + P_LWW
Probbility_of_wining_the_series

# (ii)
# Calculate probability of Red Sox winning the series
Probability_of_winning_the_series <- P_WW + P_WLW + P_LWW
prob_red_sox_series_win <- Probability_of_winning_the_series

# Net winning values
net_winning_values <- c(500, -520)
prob_distribution <- c(prob_red_sox_series_win, 1 - prob_red_sox_series_win)

# Expected net win
expected_net_win <- sum(net_winning_values * prob_distribution)
standard_deviation <- sqrt(sum((net_winning_values - expected_net_win)^2 * prob_distribution))

cat("(ii) Expected Net Win:", expected_net_win, "\n")
cat("Standard Deviation of Net Win:", standard_deviation, "\n")

# (iii)
# Ensure net_winning_values and prob_distribution are correctly defined
net_winning_values <- c(500, -520)
prob_distribution <- c(prob_red_sox_series_win, 1 - prob_red_sox_series_win)

# Generate random values
set.seed(123)  # For reproducibility
random_values_X <- sample(net_winning_values, 10000, replace = TRUE, prob = prob_distribution)

# Calculate confidence interval
confidence_interval <- quantile(random_values_X, c(0.025, 0.975))

cat("(iii) 95% Confidence Interval for Expected Net Win:", confidence_interval, "\n")

# (iv)
observed_freq = table(random_values_X)
expected_freq = prob_distribution * sum(observed_freq)

chi_squared_stat = sum((observed_freq - expected_freq)^2 / expected_freq)
p_value = 1 - pchisq(chi_squared_stat, df = length(prob_distribution) - 1)

cat("(iv) Chi-squared Statistic:", chi_squared_stat, "\n")
cat("P-value:", p_value, "\n")

# (v) 
cat("(v) Based on observations, the betting strategy is favorable.\n")



# PART 2
prob_red_sox_win_ny = 1 - prob_red_sox_win_boston
prob_yankees_win_boston = 1 - prob_yankees_win_ny

prob_red_sox_series_win_part2 = choose(2, 2) * (prob_red_sox_win_ny^2) * ((1 - prob_yankees_win_boston)^1)

# (i) 
cat("(i) Probability Red Sox wins the series (Part 2):", prob_red_sox_series_win_part2, "\n")

# (ii)
prob_distribution_part2 = c(prob_red_sox_series_win_part2, 1 - prob_red_sox_series_win_part2)

expected_net_win_part2 = sum(net_winning_values * prob_distribution_part2)
standard_deviation_part2 = sqrt(sum((net_winning_values - expected_net_win_part2)^2 * prob_distribution_part2))

cat("(ii) Expected Net Win (Part 2):", expected_net_win_part2, "\n")
cat("Standard Deviation of Net Win (Part 2):", standard_deviation_part2, "\n")



# PART 3
prob_red_sox_series_win_part3 = choose(3, 3) * (prob_red_sox_win_boston^3) * ((1 - prob_yankees_win_ny)^2)

# (i)
cat("(i) Probability Red Sox wins the series (Part 3):", prob_red_sox_series_win_part3, "\n")

# (ii)
prob_distribution_part3 = c(prob_red_sox_series_win_part3, 1 - prob_red_sox_series_win_part3)

expected_net_win_part3 = sum(net_winning_values * prob_distribution_part3)
standard_deviation_part3 = sqrt(sum((net_winning_values - expected_net_win_part3)^2 * prob_distribution_part3))

cat("(ii) Expected Net Win (Part 3):", expected_net_win_part3, "\n")
cat("Standard Deviation of Net Win (Part 3):", standard_deviation_part3, "\n")

library(ggplot2)

visualization_data = data.frame(Net_Win = net_winning_values, Probability = prob_distribution)

visualization_data_part2 = data.frame(Net_Win = net_winning_values, Probability = prob_distribution_part2)

visualization_data_part3 = data.frame(Net_Win = net_winning_values, Probability = prob_distribution_part3)

all_visualization_data = rbind(
  transform(visualization_data, Part = "Part 1"),
  transform(visualization_data_part2, Part = "Part 2"),
  transform(visualization_data_part3, Part = "Part 3")
)

combined_bar_plot = ggplot(all_visualization_data, aes(x = Net_Win, y = Probability, fill = factor(Net_Win))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Part, scales = "free_y") +
  labs(title = "Probability Distribution of Net Wins",
       x = "Net Win ($)",
       y = "Probability") +
  scale_fill_manual(values = c("#3498db", "#e74c3c"))  # Blue for positive, red for negative

combined_bar_plot
