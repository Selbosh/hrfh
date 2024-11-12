set.seed(2024)
source('R/trajectory.R')

num_patients <- 1000

patients <- purrr::map_dfr(1:num_patients, ~{
  simulate_pain_trajectory(
    days = 180,
    init_pain = 0,
    sigma = 0.25,
    flare_rate = runif(1, 0.001, 0.02),
    flare_intensity = runif(1, 1, 3),
    flare_temp_prob = 0.5,
    treat_threshold = 7,
    treat_intensity = 1,
    treat_duration = 7,
    treat_interval = 30,
    treat_effect_prob = runif(1, 0.1, 0.9),
    cutpoints = seq(-2, 3, length = 10)
  )}, .id = 'patid'
)

readr::write_csv(patients, 'data/patients.csv')

