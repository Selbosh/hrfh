# 4 categories of latent disease activity: Remission, Low, Medium, High.
# Remission tends to be 0s and 1s; Low is 2s and 3s, Medium is 4s and 6s, High is 7+

# A reasonably high proportion of people/person-time should be spent in the lower disease states.
# Fluctuation within a state should not be too rapid. Thus there should be smoothness within states.
# It would be more realistic to have people more commonly self-report the same pain level multiple days
# before then shifting to an adjacent score. Similarly, movements of one unit should be more common than movements of 2+

# It is currently quite hard to spot what I would pick out as a flare. Someone who is relatively stable,
# perhaps for weeks/months, then they have a significant increase in pain, often rapidly over days.
# Lengthening the time spent in one state might help.

# I think the model does include a hidden treatment and treatment response, yet that is also not easy to spot.
# This might again be helped by lengthening the period of time in the states before things change.

#' Simulate latent trajectory of a patient with flares and treatment
#' 
#' @param num_timepoints Integer length of time series.
#' @param baseline_variation Standard deviation of latent random walk.
#' @param flare_rate Exponential rate of flares. Default every 10 days.
#' @param flare_magnitude Mean size of increase in latent pain due to flare.
#' @param flare_variation Standard deviation of increase in latent pain due to flare.
#' @param treat_magntitude Mean size of decrease in latent pain, per day, when treatment is effective.
#' @param alpha Scaling factor for ordered logit model. Not used.
#' @param cutpoints Latent thresholds for the ordinal model.
#' @param verbose Print diagnostic messages, useful for debugging. Default \code{FALSE}.
latent_process <- function(num_timepoints = 180,
                           baseline_variation = 0.5,
                           flare_rate = 0.01,
                           flare_magnitude = 2,
                           flare_variation = 0.25,
                           treat_magnitude = 1/10,
                           alpha = 1,
                           cutpoints = seq(-2, 3, length = 10),
                           verbose = FALSE) {
    if (any(order(cutpoints) != seq_along(cutpoints)))
        stop('cutpoints should be in nondecreasing order')

    # Baseline variation
    dx <- rnorm(num_timepoints, sd = baseline_variation)

    # Flare variation
    flare_inter <- rexp(100, rate = flare_rate)               # Arbitrarily large # inter-arrival times
    flare_inter <- flare_inter[flare_inter >= 7]              # Don't allow flares more than once a week
    flare_times <- cumsum(flare_inter)                        # Inter-arrival -> absolute times           
    flare_times <- ceiling(flare_times) |> unique()           # Round up to next day, <=1 per day
    flare_times <- flare_times[flare_times <= num_timepoints] # Censor those after the end of the study
    if (length(flare_times) > 0) {
        flare_magnitudes <- rnorm(length(flare_times), mean = flare_magnitude, sd = flare_variation)
        dx[flare_times] <- dx[flare_times] + pmax(flare_magnitudes, 0)
    }

    # Latent state
    #dx[1] <- dx[1] + cutpoints[1]
    dx[1] <- rnorm(1)
    x <- cumsum(dx)

    # browser()
    # Treatment
    if (length(cutpoints) < 10)
      warning('Code optimized for 10 cutpoints')
    # Wait till high state for first treatment
    high_states <- tail(cutpoints, 3)
    # Treatment then lasts for 30 days before resetting due to ineffectiveness/side effects
    first_high <- which(x >= high_states[1])[1]
    if (!is.na(first_high)) {
        if (verbose)
            message('First high: ', first_high, ': ', x[first_high])
        treat_times <- seq(which.max(x >= high_states[1]), num_timepoints, by = 31)
        treat_times <- setdiff(treat_times, flare_times) # Flares nullify treatment
            for (treat_time in treat_times) {
                x <- cumsum(dx)
                if (runif(1) < 0.5 &&                    # Flip a coin for effectiveness
                        x[treat_time] >= cutpoints[1]) { # Don't treat healthy people.
                    if (verbose) message('Treatment at time ', treat_time, ' effective')
                    treat_end <- pmin(treat_time + 30, num_timepoints)
                    treat_ids <- seq(treat_time, treat_end)
                    dx[treat_ids] <- dx[treat_ids] - pmax(rnorm(1, mean = treat_magnitude, sd = 0.01), 0) /
                        abs(ifelse(x[treat_ids] < 0, x[treat_ids], 1)) # decrease effect if already healthy
                } else {
                    if (verbose) message('Treatment at time ', treat_time, ' ineffective')
                    treat_times <- setdiff(treat_times, treat_time)
                }
            }
    } else {
        if (verbose) message('Never treated')
        treat_times <- NULL
    }

    x <- cumsum(dx)

    # Measurement scale (probabilistic)
    # cutpoints <- sort(cutpoints, decreasing = TRUE)
    # cum_probs <- sapply(cutpoints, function(cp) plogis(alpha * (x - cp)))
    # cum_probs <- cbind(cum_probs, 1) # Add 1 for top category
    # probs <- apply(cum_probs, 1, diff) |> t()
    # probs <- cbind(probs, 1 - rowSums(probs))
    # ordinal_levels <- c(0, seq_along(cutpoints))
    # y <- apply(probs, 1, function(p) sample(ordinal_levels, size = 1, prob = p))

    # Measurement scale (deterministic)
    y <- findInterval(x, sort(cutpoints))

    data.frame(time = seq_along(x),
               latent = x, latent_delta = dx,
               pain = y,
               flare = seq_along(x) %in% flare_times,
               treat = seq_along(x) %in% treat_times)
}

# df <- latent_process(100, verbose = TRUE)

library(ggplot2)
# # Plot latent state over time
# ggplot(df, aes(x = time, y = latent)) +
#     geom_line() +
#     geom_point() +
#     geom_hline(yintercept = seq(-2, 3, length = 10), linetype = "dashed") +
#     # Treatment
#     geom_vline(aes(xintercept = time), data = subset(df, treat), colour = 'seagreen') +
#     # Flares
#     geom_vline(aes(xintercept = time), data = subset(df, flare), colour = 'tomato2') +
#     labs(title = "Latent State Over Time", x = "Time", y = "Latent State")

# # Plot pain level over time
# ggplot(df, aes(x = time, y = pain)) +
#     geom_line() +
#     geom_point() +
#     # Treatment
#     geom_vline(aes(xintercept = time), data = subset(df, treat), colour = 'seagreen') +
#     # Flares
#     geom_vline(aes(xintercept = time), data = subset(df, flare), colour = 'tomato2') +
#     labs(title = "Pain Level Over Time", x = "Time", y = "Pain Level") +
#     ylim(0, 10)

set.seed(2)
cutpoints <- seq(-2, 3, length = 10)
patients_data <- purrr::map_dfr(1:10, function(id) {
    patient <- latent_process(verbose = FALSE,
                              flare_rate = 0.01,
                              baseline_variation = 0.1,
                              cutpoints = cutpoints)
    patient$id <- id
    patient
})

# Plot pain level over time
ggplot(patients_data, aes(x = time, y = pain)) +
    # Flares
    geom_vline(aes(xintercept = time), data = subset(patients_data, flare),
               colour = 'tomato2') +
    # Treatment
    geom_vline(aes(xintercept = time), data = subset(patients_data, treat),
               colour = 'seagreen', linetype = 'dashed') +
    geom_line() +
    geom_point() +
    labs(title = "Pain Level Over Time", x = "Time", y = "Pain Level") +
    scale_y_continuous(breaks = seq(0, 10, by = 2)) +
    facet_wrap(~ id, ncol = 2)
ggsave('trajectories.png', width = 16, height = 8)
