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

latent_process <- function(num_timepoints = 100,
                           baseline_variation = 0.5,
                           flare_rate = 0.1,
                           flare_magnitude = 2,
                           flare_variation = 0.5,
                           alpha = 1,
                           cutpoints = seq(-2, 3, length = 10)) {

    # Initialization
    x <- numeric(num_timepoints)
    y <- numeric(num_timepoints)

    # Baseline disease activity
    x <- baseline_walk <- cumsum(rnorm(num_timepoints, sd = baseline_variation))

    # Flares
    flare_times <- cumsum(rexp(100, rate = flare_rate))       # Arbitrarily large # inter-arrival times
    flare_times <- ceiling(flare_times)                       # Round up to next day, <=1 per day
    flare_times <- flare_times[flare_times <= num_timepoints] # Censor those after the end of the study
    num_flares <- length(flare_times)
    flare_magnitudes <- rnorm(num_flares, mean = flare_magnitude, sd = flare_variation)
    x[flare_times] <- x[flare_times] + pmax(flare_magnitudes, 0) # No negative flares

    # Treatment
    # TODO. Similar to flares but only occurs if (reported) pain is high? But not too often. And sometimes doesn't work.

    # Measurement
    cutpoints <- sort(cutpoints, decreasing = TRUE)
    cum_probs <- sapply(cutpoints, function(cp) plogis(alpha * (x - cp)))
    cum_probs <- cbind(cum_probs, 1) # Add 1 for top category
    probs <- apply(cum_probs, 1, diff) |> t()
    ordinal_levels <- c(0, seq_along(cutpoints))
    y <- apply(probs, 1, function(p) sample(ordinal_levels, size = 1, prob = p))

    list(x, y, flare_times, flare_magnitudes)
}

set.seed(1)
latent_process(100)
