library(R6)
library(dplyr)
library(purrr)
library(ggplot2)

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
      self$latent_pain_status <- sample(c("High", "Medium", "Low"), 1,
                                        prob = c(.1, .4, .5)) # Simulate latent pain status as a random value
      self$time_since_treatment_started <- Inf # Initialize time since treatment started as Inf
      self$time_since_last_flare <- sample(0:365, 1) # Simulate time since last flare as a random value between 0 and 365
      self$treatment_effectiveness <- NA # But no treatment = Ineffective treatment
      self$flare_status <- "Off"
      self$days_until_flare_ends <- NA
    },
    
    simulate_treatment_effectiveness = function() {
      sample(c("Effective", "Ineffective"), 1, prob = c(.8, .2))
    },
    
    simulate_day = function() {
      self$day <- self$day + 1

      # Update latent pain status based on a simple hidden Markov model
      transition_matrix <- matrix(c(0.9, 0.1, 0.00,
                                    0.1, 0.85, 0.05,
                                    0.05, 0.25, 0.7), nrow = 3, byrow = TRUE)
      states <- c("Low", "Medium", "High")
      current_state <- match(self$latent_pain_status, states)
      self$latent_pain_status <- sample(states, 1, prob = transition_matrix[current_state, ])
      
      # Determine if a new treatment starts
      if (self$time_since_treatment_started > 30 && self$latent_pain_status == "High") {
        if (runif(1) < 0.8) { # High probability to start treatment
          self$treatment_status <- "On"
          self$time_since_treatment_started <- 0
          self$treatment_effectiveness <- self$simulate_treatment_effectiveness()
        }
      }
      
      # Update time since treatment started
      if (self$treatment_status == "On") {
        self$time_since_treatment_started <- self$time_since_treatment_started + 1
        if (self$treatment_effectiveness == "Effective" && self$time_since_treatment_started <= 7) {
          # Reduce latent pain status by 1 or 2 states
          new_state <- max(1, current_state - sample(1:2, 1, prob = c(.2, .8)))
          self$latent_pain_status <- states[new_state]
          #self$treatment_status <- "Off" # End treatment?
        }
      }
      
      # Determine if a flare occurs
      if (self$time_since_last_flare > 21 && self$latent_pain_status != "High" && runif(1) < 0.01) { # Unlikely to occur
        self$flare_status <- "On"
        self$time_since_last_flare <- 0
        self$days_until_flare_ends <- 4 # assume flares are a spike
      }
      
      # Update time since last flare
      if (self$flare_status == "On") {
        # Increase latent pain status by 1 or 2 states
        new_state <- min(3, current_state + sample(1:2, 1))
        self$latent_pain_status <- states[new_state]
        self$days_until_flare_ends <- self$days_until_flare_ends - 1
        if (self$days_until_flare_ends <= 0) {
          self$flare_status <- "Off" # End flare
          # Reduce latent pain status by 1 or 2 states
          # new_state <- max(1, current_state - sample(1:2, 1))
          # self$latent_pain_status <- states[new_state]
        }
      } else {
        self$time_since_last_flare <- self$time_since_last_flare + 1
      }
      
      # Pain levels associated with each state
      pain_levels <- list(
          Low = 0:3,
          Medium = 4:7,
          High = 8:10
      )
      pain_report <- sample(pain_levels[[self$latent_pain_status]], 1)
      
      # Record the day's data
      list(
        day = self$day,
        latent_status = ordered(self$latent_pain_status, levels = rev(states)),
        ordinal_pain_report = pain_report,
        treatment_status = self$treatment_status,
        start_of_treatment = self$time_since_treatment_started == 1,
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

set.seed(123)
# Simulate 10 patients each for 6 months
patients_data <- purrr::map_dfr(1:10, function(id) {
    patient <- Patient$new()
    traj <- patient$simulate_6_months()
    traj$id <- id
    traj
})

ggplot(patients_data) +
  aes(day, ordinal_pain_report) +
  geom_vline(aes(xintercept = day, linetype = treatment_effectiveness), colour = 'seagreen', alpha = .25,
             data = filter(patients_data, start_of_treatment), linewidth = 1) +
  geom_vline(aes(xintercept = day), colour = 'red', alpha = .25,
             data = filter(patients_data, flare_status == 'On'), linewidth = 1) +
  geom_line(alpha = .1) +
  geom_point(size = 1) +
  geom_rug(aes(colour = latent_status)) +
  scale_y_continuous(breaks = seq(0, 10, by = 2), minor_breaks = NULL) +
  facet_wrap(~ id, ncol = 2) +
  guides(fill = guide_legend(title = "Flare"), 
         linetype = guide_legend(title = "Treatment"), 
         colour = guide_legend(title = "Disease")) +
  scale_color_manual(values = rev(c('#B3E2B3', '#DDDD55', '#FFB6C1'))) +
  scale_fill_manual(values = c('tomato', 'grey40')) +
  theme_classic() +
  ylab('Reported pain')

ggsave('trajectories.png', width = 16, height = 8)