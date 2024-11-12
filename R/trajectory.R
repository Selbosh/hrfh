simulate_baseline <- function(days, init_pain = 0, sigma = 1) {
    dx <- rnorm(days - 1, sd = sigma)
    x <- init_pain + c(0, cumsum(dx))
    return(x)
}

measure_ordinal <- function(x, cutpoints) {
    if (any(order(cutpoints) != seq_along(cutpoints)))
        stop('cutpoints should be in nondecreasing order')
    findInterval(x, cutpoints)
}

simulate_flares <- function(days, flare_rate = 0.1, flare_intensity = 3, flare_temp_prob = 0.5) {
    stopifnot(flare_intensity >= 0)
    stopifnot(flare_temp_prob >= 0 & flare_temp_prob <= 1)
    stopifnot(flare_rate >= 0 & flare_rate <= 1)
    # Generate random flare days using Poisson process
    flare_days <- which(rbinom(days, 1, flare_rate) == 1)
    flares <- rep(0, days)
    for (day in flare_days) {
        if (runif(1) < flare_temp_prob) {
            # Temporary (transient) flare: lasts 2 days
            flare_period <- day:(day + 1)
            flare_period <- flare_period[flare_period <= days]
            flares[flare_period] <- flare_intensity
        } else {
            # Step change (persistent) flare: increases baseline from this day onward
            flare_period <- day:days
            flares[flare_period] <- flares[flare_period] + flare_intensity
        }
    }
    return(flares)
}

# Unlike baseline variation or flares, treatment depends on current state
apply_treatment <- function(trajectory, day,
                            treat_intensity,
                            treat_duration,
                            treat_effect_prob) {
    treat_intensity <- max(treat_intensity, 0) # Ensure treatment intensity is non-negative
    # Apply treatment effect for specified duration
    end_day <- min(day + treat_duration - 1, length(trajectory))
    if (runif(1) < treat_effect_prob) {
        # Treatment is effective
        trajectory[day:end_day] <- trajectory[day:end_day] - treat_intensity
    }
    return(trajectory)
}

#' @export
simulate_pain_trajectory <- function(days, init_pain = 0, sigma = 1,
                                     flare_rate = 0.1, flare_intensity = 3, flare_temp_prob = 0.5,
                                     treat_threshold = 8,
                                     treat_intensity = 4,
                                     treat_duration = 7,
                                     treat_interval = 30,
                                     treat_effect_prob = 0.5,
                                     cutpoints = seq(-2, 3, length = 10)) {
    stopifnot(treat_duration <= treat_interval)

    # Baseline and flare simulation
    baseline <- simulate_baseline(days, init_pain, sigma)
    flares <- simulate_flares(days, flare_rate, flare_intensity, flare_temp_prob)

    # Combined initial trajectory without treatment
    trajectory <- baseline + flares
    measurement <- measure_ordinal(trajectory, cutpoints)

    # Apply treatment every 30 days if pain level reaches the threshold
    days_on_treatment <- c()
    first_treat_day <- which(measurement >= treat_threshold)[1]
    if (!is.na(first_treat_day)) {
        for (day in seq(first_treat_day, days, by = treat_interval)) {
            if (measurement[day] >= treat_threshold) {
                trajectory <- apply_treatment(trajectory, day, treat_intensity, treat_duration, treat_effect_prob)
                measurement <- measure_ordinal(trajectory, cutpoints)
                days_on_treatment <- c(days_on_treatment, day:min(days, (day + treat_duration - 1)))
            }
        }
    }

    data.frame(
        day = 1:days,
        baseline = baseline,
        flares = flares,
        on_treatment = seq_len(days) %in% days_on_treatment,
        trajectory = trajectory,
        measurement = measurement
    )
}

