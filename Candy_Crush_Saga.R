# This sets the size of plots to a good default.
options(repr.plot.width = 5, repr.plot.height = 4)

# Loading in packages
library(readr)
library(dplyr)
library(ggplot2)

# Reading in the data
data <- read_csv("datasets/candy_crush.csv")

# Printing out the first six rows
head(data)

# Count and display the number of unique players
print("Number of players:")
data$player_id %>% 
  unique() %>%
  length()
# Display the date range of the data
print("Period for which we have data:")
data$dt %>%
  range()

# Calculating level difficulty
difficulty <- data %>%
  group_by(level)%>%
  summarise(attempts = sum(num_attempts), wins = sum(num_success)) %>%
  mutate(p_win = wins/attempts)
# Printing out the level difficulty
difficulty

# Plotting the level difficulty profile
ggplot(difficulty, aes(level, p_win)) + 
  scale_x_continuous() + 
  scale_y_continuous(labels = scales::percent)

# Adding points and a dashed line
ggplot(difficulty, aes(level, p_win, color = p_win)) + 
  geom_line() +
  scale_x_continuous(breaks = 1:15) +  
  scale_y_continuous(label = scales::percent) +
  geom_point() +
  geom_hline(yintercept = 0.1, linetype = 'dashed')

# Computing the standard error of p_win for each level
difficulty <- difficulty %>%
  mutate(error = sqrt(p_win * (1 - p_win) / attempts))

# Adding standard error bars
ggplot(difficulty, aes(level, p_win, color = p_win)) + 
  geom_line() +
  scale_x_continuous(breaks = 1:15) +  
  scale_y_continuous(label = scales::percent) +
  geom_point() +
  geom_hline(yintercept = 0.1, linetype = 'dashed') +
  geom_errorbar(aes(ymin = p_win - error, ymax = p_win + error))

# The probability of completing the episode without losing a single time
p <- prod(difficulty$p_win)

# Printing it out
p

# Should our level designer worry about that a lot of 
# players will complete the episode in one attempt?
should_the_designer_worry = FALSE # TRUE / FALSE