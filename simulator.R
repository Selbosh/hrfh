library(R6)
library(tidyverse)

# Define the Patient class using R6
Patient <- R6Class(
  "Patient",
  public = list(
    age = NULL,
    sex = NULL,
    day = NULL,
    ethnicity = NULL,
    phenotype = NULL,
    treatment_status = NULL,
    latent_pain_status = NULL,
    time_since_treatment_started = NULL,
    time_since_last_flare = NULL,
    treatment_effectiveness = NULL,
    days_until_effective = NULL,
    flare_status = NULL,
    days_until_flare_ends = NULL,
    
    initialize = function(age = sample(0:100, 1),
                          sex = sample(c('Male', 'Female'), 1),
                          ethnicity = sample(paste0('Ethnicity', 1:3), 1)) {
      self$day <- 0
      self$age <- age
      self$sex <- sex
      self$ethnicity <- ethnicity
      self$phenotype <- runif(1) # Simulate phenotype as a random value
      self$treatment_status <- "Off" # Initialize treatment status as "Off"
      self$latent_pain_status <- sample(c("High", "Medium", "Low"), 1) # Simulate latent pain status as a random value
      self$time_since_treatment_started <- Inf # Initialize time since treatment started as Inf
      self$time_since_last_flare <- sample(0:365, 1) # Simulate time since last flare as a random value between 0 and 365
      self$treatment_effectiveness <- NA # But no treatment = Ineffective treatment
      self$days_until_effective <- NA
      self$flare_status <- "Off"
      self$days_until_flare_ends <- NA
    },
    
    simulate_treatment_effectiveness = function() {
      sample(c("Effective", "Ineffective"), 1)
    },
    
    simulate_day = function() {
      self$day <- self$day + 1

      # Update latent pain status based on a simple hidden Markov model
      transition_matrix <- matrix(c(0.7, 0.2, 0.1,
                                    0.2, 0.6, 0.2,
                                    0.1, 0.2, 0.7), nrow = 3, byrow = TRUE)
      states <- c("High", "Medium", "Low")
      current_state <- match(self$latent_pain_status, states)
      self$latent_pain_status <- sample(states, 1, prob = transition_matrix[current_state, ])
      
      # Determine if a new treatment starts
      if (self$time_since_treatment_started > 30 && self$latent_pain_status == "High") {
        if (runif(1) < 0.8) { # High probability to start treatment
          self$treatment_status <- "On"
          self$time_since_treatment_started <- 0
          self$treatment_effectiveness <- self$simulate_treatment_effectiveness()
          self$days_until_effective <- sample(1:7, 1) # Random number of days between 1 and 7
        }
      }
      
      # Update time since treatment started
      if (self$treatment_status == "On") {
        self$time_since_treatment_started <- self$time_since_treatment_started + 1
        if (self$treatment_effectiveness == "Effective" && self$time_since_treatment_started >= self$days_until_effective) {
          # Reduce latent pain status by 1 or 2 states
          new_state <- max(1, current_state - sample(1:2, 1))
          self$latent_pain_status <- states[new_state]
          self$treatment_status <- "Off" # End treatment
        }
      }
      
      # Determine if a flare occurs
      if (self$time_since_last_flare > 14 && runif(1) < 0.1) { # Unlikely to occur
        self$flare_status <- "On"
        self$time_since_last_flare <- 0
        self$days_until_flare_ends <- sample(2:14, 1) # Random number of days between 2 and 14
        # Increase latent pain status by 1 or 2 states
        new_state <- min(3, current_state + sample(1:2, 1))
        self$latent_pain_status <- states[new_state]
      }
      
      # Update time since last flare
      if (self$flare_status == "On") {
        self$time_since_last_flare <- self$time_since_last_flare + 1
        if (self$time_since_last_flare >= self$days_until_flare_ends) {
          self$flare_status <- "Off" # End flare
        }
      } else {
        self$time_since_last_flare <- self$time_since_last_flare + 1
      }
      
      # Generate noisy ordinal pain report
      pain_report <- switch(self$latent_pain_status,
                            "High" = sample(7:10, 1),
                            "Medium" = sample(4:6, 1),
                            "Low" = sample(0:3, 1))
      
      # Record the day's data
      list(
        day = self$day,
        latent_status = self$latent_pain_status,
        ordinal_pain_report = pain_report,
        treatment_status = self$treatment_status,
        treatment_effectiveness = ifelse(self$treatment_status == "On", self$treatment_effectiveness, NA),
        flare_status = self$flare_status
      )
    },
    
    simulate_n_days = function(n) {
      results <- vector("list", n)
      for (day in 1:n) {
        results[[day]] <- self$simulate_day() |> tibble::as_tibble()
      }
      dplyr::bind_rows(results)
    },

    simulate_6_months = function() {
        self$simulate_n_days(180)
    }
  )
)

# Simulate 9 patients each for 6 months
patients_data <- purrr::map_dfr(1:9, function(id) {
    patient <- Patient$new()
    traj <- patient$simulate_6_months()
    traj$id <- id
    traj
})

ggplot(patients_data %>% filter(id == 1)) +
  aes(day, ordinal_pain_report) +
  geom_point()
