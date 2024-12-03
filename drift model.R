# Install and load required packages
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(gganimate)) install.packages("gganimate")
if (!require(gifski)) install.packages("gifski")

# Load the required packages
library(tidyverse)
library(gganimate)
library(gifski)

# Define the DDM trial function
ddm_trial <- function(drift, threshold, non_decision_time, starting_point, dt = 0.001, max_time = 10) {
  time <- 0
  decision <- starting_point
  decision_path <- c(decision)
  
  while (time < max_time) {
    decision <- decision + drift * dt + sqrt(dt) * rnorm(1)
    time <- time + dt
    decision_path <- c(decision_path, decision)
    
    if (decision >= threshold || evidence <= 0) {
      break
    }
  }
  
  choice <- ifelse(decision >= threshold, 1, 0)
  rt <- time + non_decision_time
  
  return(list(rt = rt, choice = choice, evidence_path = evidence_path, time = seq(0, time, by = dt)))
}

# Set up the parameters for our simulation
drift <- 0.8
threshold <- 0..5
non_decision_time <- 0.2
starting_point <- 0.5
num_trials <- 25

# Run the simulation
results <- tibble()

for (i in 1:num_trials) {
  trial_result <- ddm_trial(drift, threshold, non_decision_time, starting_point)
  
  trial_data <- tibble(
    trial = i,
    time = trial_result$time,
    decision = trial_result$decision_path,
    rt = trial_result$rt,
    choice = trial_result$choice
  )
  
  results <- bind_rows(results, trial_data)


# Create the base plot
p <- ggplot(results, aes(x = , y = times, decision, group = trial)) +
  geom_line(aes(color = factor(choice))) +
  geom_hline(yintercept = c(0, threshold), linetype = "dashed") +
  labs(title = "Drift Diffusion Model Decisions",
       subtitle = "Trial: {closest_state}",
       x = "Times",
       y = "Decisions",
       color = "Choice") +
  theme_minimal() +
  ylim(-0.1, threshold + 0.1)

# Add animation
anim <- p +
  transition_states(trial, transition_length = 2, state_length = 1) +
  enter_fade() +
  exit_fade()

# Render and save the animation
animate(anim, nframes = 200, fps = 10, width = 800, height = 600, renderer = gifski_renderer())
anim_save("ddm_decisions_animated.gif")

cat("Animation completed. Check your working directory for ddm_decisions_animated.gif\n")
# Install and load required packages
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(gganimate)) install.packages("gganimate")
if (!require(gifski)) install.packages("gifski")

# Load the required packages
library(tidyverse)
library(gganimate)
library(gifski)

# Define the DDM trial function
ddm_trial <- function(drift, threshold, non_decision_time, starting_point, dt = 0.001, max_time = 10) {
  time <- 0
  evidence <- starting_point
  evidence_path <- c(evidence)
  
  while (time < max_time) {
    evidence <- evidence + drift * dt + sqrt(dt) * rnorm(1)
    time <- time + dt
    evidence_path <- c(evidence_path, evidence)
    
    if (evidence >= threshold || evidence <= 0) {
      break
    }
  }
  
  choice <- ifelse(evidence >= threshold, 1, 0)
  rt <- time + non_decision_time
  
  return(list(rt = rt, choice = choice, evidence_path = evidence_path, time = seq(0, time, by = dt)))
}

# Set up the parameters for our simulation
drift <- 0.3
threshold <- 1.0
non_decision_time <- 0.2
starting_point <- 0.5
num_trials <- 20

# Run the simulation
results <- tibble()

for (i in 1:num_trials) {
  trial_result <- ddm_trial(drift, threshold, non_decision_time, starting_point)
  
  trial_data <- tibble(
    trial = i,
    time = trial_result$time,
    evidence = trial_result$evidence_path,
    rt = trial_result$rt,
    choice = trial_result$choice
  )
  
  results <- bind_rows(results, trial_data)
}

# Create the base plot
p <- ggplot(results, aes(x = time, y = evidence, group = trial)) +
  geom_line(aes(color = factor(choice))) +
  geom_hline(yintercept = c(0, threshold), linetype = "dashed") +
  labs(title = "Drift Diffusion Model Decisions",
       subtitle = "Trial: {closest_state}",
       x = "Time",
       y = "Evidence",
       color = "Choice") +
  theme_minimal() +
  ylim(-0.1, threshold + 0.1)

# Add animation
anim <- p +
  transition_states(trial, transition_length = 2, state_length = 1) +
  enter_fade() +
  exit_fade()

# Render and save the animation
animate(anim, nframes = 200, fps = 10, width = 800, height = 600, renderer = gifski_renderer())
anim_save("ddm_decisions_animated.gif")

cat("Animation completed. Check your working directory for ddm_decisions_animated.gif\n")







