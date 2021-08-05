# Functions from covid19 forecasting project
# Simplified for this purpose

sample_crps <- function(forecasts, actuals) {
  # Number of samples for each date/state/origin combination
  total_samples <- length(forecasts$sample[[1]])

  # Unnest and add in actuals
  forecasts <- forecasts %>%
    unnest(sample) %>%
    arrange(date, state, forecast_origin, sample) %>%
    group_by(date, state, forecast_origin) %>%
    mutate(order = row_number()) %>%
    ungroup() %>%
    left_join(actuals, by = c("date", "state"))

  # Compute CRPS averaged over state and forecast horizon
  crps <- forecasts %>%
    mutate(
      h = date - forecast_origin,
      crps = (sample - n) * (total_samples * (n < sample) - order + 0.5)
    ) %>%
    group_by(date, state, forecast_origin, n, h) %>%
    summarise(crps = 2 * sum(crps) / (total_samples^2), .groups="keep") %>%
    group_by(state, h) %>%
    summarise(crps = mean(crps, na.rm = TRUE), .groups="keep") %>%
    ungroup()

  return(crps)
}

# Create equally weighted ensemble by combining equal sized samples
# Keep h days for each state where h = forecast horizon
# By default h is the minimum horizon available from all models
make_ensemble <- function(df, h = NULL, weight = NULL, by_forecast_origin=TRUE) {
  # What horizon to use?
  if (is.null(h)) {
    # Find minimum horizon from all models
    h <- df %>%
      group_by(state, .model, forecast_origin) %>%
      mutate(rn = row_number()) %>%
      filter(rn == max(rn)) %>%
      group_by(state, forecast_origin) %>%
      filter(rn == min(rn)) %>%
      pull(rn) %>%
      min()
  }

  # Number of samples to take from each model.
  # Equal numbers if weight is NULL (default)
  df <- df %>%
    group_by(state, date, .model, forecast_origin) %>%
    mutate(l = length(unlist(sample))) %>%
    ungroup()
  modeln <- tibble(
    .model = unique(df$.model),
    nsamples = max(df$l)
  )
  if(is.null(weight)) {
    weight <- rep(1,NROW(modeln))
    names(weight) <- modeln$.model
  }
  # Ensure they add to 1
  weight <- weight / sum(weight)
  # Add weights to modeln
  if(!identical(sort(modeln$.model), sort(names(weight)))) {
    stop(paste("Model names do not match with weights.\n Use names: ",
               paste(sort(modeln$.model), collapse=", ")))
  }
  modeln <- modeln %>%
    left_join(
      tibble(.model = names(weight), weight = weight),
      by=".model"
    ) %>%
    mutate(nsamples = as.integer(nsamples * weight / min(weight)))
  if(sum(modeln$nsamples) > 10000) {
    modeln$nsamples <- as.integer(modeln$nsamples / sum(modeln$nsamples) * 10000)
  }

  # Resample to ensure same number of samples per date/state
  df <- df %>%
    left_join(modeln, by = ".model") %>%
    group_by(date, state, .model, forecast_origin) %>%
    mutate(
      sample = case_when(
        l == nsamples ~ sample,
        TRUE ~ list(sample(unlist(sample), size = nsamples, replace = TRUE)),
      ),
      #cumcases = case_when(
      #  l == nsamples ~ cumcases,
      #  TRUE ~ list(sample(unlist(cumcases), size = nsamples, replace = TRUE)),
      #)
    ) %>%
    ungroup() %>%
    select(-l, -nsamples)

  # Combine samples into one vector
  ensemble <- df %>%
    group_by(date, state, forecast_origin) %>%
    summarise(
      sample = list(c(unlist(sample))),
      #cumcases = list(c(unlist(cumcases))),
      .groups="keep"
    ) %>%
    group_by(state, forecast_origin) %>%
    mutate(rn = row_number()) %>%
    filter(rn <= h) %>%
    ungroup() %>%
    select(-rn)

  return(ensemble)
}

# Single state ensemble. Return mean CRPS
weight_ensemble_crps <- function(weight, df, actuals) {
  if(any(weight < 0.049999) | sum(weight) > 0.950001)
    return(Inf)
  weight <- c(1-sum(weight),weight)
  models <- sort(unique(df$.model))
  names(weight) <- models

  # Set seed to avoid different samples every time the weights change
  set.seed(2020)
  make_ensemble(df, weight = weight, add_aus=FALSE) %>%
    sample_crps(actuals) %>%
    summarise(crps = mean(crps), .groups="keep") %>%
    pull(crps)
}

find_weight_ensemble_crps <- function(df, actuals, surface) {
  # Optimize weights using best point on surface as starting point
  models <- sort(unique(df$.model))
  weight <- surface %>%
    filter(crps==min(crps)) %>%
    select(-crps) %>%
    as.numeric()
  fred <- optim(weight,
                weight_ensemble_crps,
                df = df,
                actuals=actuals)
  #,control = list(maxit=100))
  weight <- c(1-sum(fred$par), fred$par)
  names(weight) <- models
  return(weight)
}

weight_ensemble_surface <- function(df, actuals) {
  models <- sort(unique(df$.model))
  result <- expand.grid(model2 = seq(0.05,0.95,by=0.05),
                        model3 = seq(0.05,0.95,by=0.05)) %>%
    as_tibble() %>%
    filter(model2 + model3 < 0.95001) %>%
    mutate(crps = NA)
  colnames(result) <- c(models[2:3], "crps")
  for(i in seq(NROW(result))) {
    print(paste("Weight surface:", i, "of", NROW(result)))
    result$crps[i] <- weight_ensemble_crps(unlist(result[i,1:2]), df, actuals)
  }
  return(result)
}

weight_surface_plot <- function(surface, filename=NULL) {
  model_names <- colnames(surface)[1:2]
  colnames(surface)[1:2] <- c("x","y")
  p <- surface %>%
    ggplot() +
    geom_contour_filled(aes(x=x, y=y, z=crps)) +
    xlab(model_names[1]) + ylab(model_names[2])
  if (!is.null(filename)) {
    pdf(paste0(here::here(""), filename), width = 22/2.54, height = 16/2.54)
    print(p)
    dev.off()
  }
  return(p)
}

