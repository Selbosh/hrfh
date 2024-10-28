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
                           baseline_variation = 1,
                           flare_rate = 0.1,
                           flare_magnitude = 2,
                           flare_variation = 0.5,
                           treat_magnitude = 1,
                           alpha = 1,
                           cutpoints = seq(-2, 3, length = 10)) {
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
    dx[1] <- dx[1] + cutpoints[1]
    x <- cumsum(dx)

    # Treatment
    if (length(cutpoints) < 10)
      warning('Code optimized for 10 cutpoints')
    # Wait till high state for first treatment
    high_states <- tail(cutpoints, 3)
    # Treatment then lasts for 30 days before resetting due to ineffectiveness/side effects
    treat_times <- seq(which.max(x >= high_states[1]), num_timepoints, by = 31)
    # Careful as we don't want to treat someone who never hits high pain
    if (min(treat_times) <= num_timepoints & min(treat_times) > 1) {
        for (treat_time in treat_times) {
            if (runif(1) < 0.5) { # Flip a coin for effectiveness
                treat_end <- pmin(treat_time + 30, num_timepoints)
                treat_ids <- seq(treat_time, treat_end)
                dx[treat_ids] <- dx[treat_ids] - pmax(rnorm(1, mean = treat_magnitude), 0)
            }
        }
    }

    x <- cumsum(dx)

    # Measurement scale (probabilistic)
    cutpoints <- sort(cutpoints, decreasing = TRUE)
    cum_probs <- sapply(cutpoints, function(cp) plogis(alpha * (x - cp)))
    cum_probs <- cbind(cum_probs, 1) # Add 1 for top category
    probs <- apply(cum_probs, 1, diff) |> t()
    probs <- cbind(probs, 1 - rowSums(probs))
    ordinal_levels <- c(0, seq_along(cutpoints))
    y <- apply(probs, 1, function(p) sample(ordinal_levels, size = 1, prob = p))

    # Measurement scale (deterministic)
    y <- findInterval(x, sort(cutpoints))

    data.frame(i = seq_along(x), dx = dx, x = x, y = y,
               flare = seq_along(x) %in% flare_times,
               treat = seq_along(x) %in% treat_times)
}

# set.seed(1)
df <- latent_process(flare_rate = 0.01, baseline_variation = 0.5)
plot(x ~ i, data = df, type = 'o')
abline(h = seq(-2, 3, length = 10))
abline(v = df$i[df$flare], lty = 2)
plot(y ~ i, data = df, type = 'o', ylim = c(0, 10))
abline(v = df$i[df$flare])
abline(v = df$i[df$treat], col = 2)
