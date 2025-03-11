library(data.table)

# Version of \sigma function
sigma_sin <- function(t = seq(0, 1, length.out = 100)) {
  if (!methods::is(t, "numeric") || any(t < 0 | t > 1))
    stop("'t' must be a numeric vector with values between 0 and 1.")
  
  sigma_val <- abs(sin(pi * t))
  return(sigma_val)
}

sigma_arctan_periodic <- function(t = seq(0, 1, length.out = 100), A = 5) {
  if (!methods::is(t, "numeric") || any(t < 0 | t > 1))
    stop("'t' must be a numeric vector with values between 0 and 1.")
  
  sigma_val <- atan(A * sin(2 * pi * t)) / pi + 0.5
  return(sigma_val)
}



sigma_arctan <- function(t = seq(0.2, 0.8, len = 10)){
  if (!methods::is(t, "numeric") && all(t >= 0 & t <= 1))
    stop("'t' must be a numeric vector or scalar value(s) between 0 and 1.")
  
  sigma_val <- atan(t) / pi + 1/2
  return(sigma_val)
}

sigma_linear <- function(t = seq(0.2, 0.8, len = 10), sigma_min = 0.2, sigma_max = 0.8) {
  
  if (! (methods::is(t, "numeric") && all(t >= 0 & t <= 1)))
    stop("'t' must be a numeric vector or scalar value(s) between 0 and 1.")
  
  if (!(methods::is(sigma_min, "numeric") && methods::is(sigma_max, "numeric") &&
        (sigma_min > 0 && sigma_min < 1) && (sigma_max > 0 && sigma_max < 1))) {
    stop("'sigma_min' and 'sigma_max' must be scalar values between 0 and 1")
  }
  
  a <- (sigma_max - sigma_min) / (1 - 0)
  b <- sigma_min
  sigma_val <- pmin(a * t + b, 1)
  return(sigma_val)
}

sigma_logistic <- function(t, sigma_min = 0.2, sigma_max = 0.8, slope = 30,
                           transition_point = 0.5) {
  if (! (methods::is(t, "numeric") && all(t >= 0 & t <= 1)))
    stop("'t' must be a numeric vector or scalar value(s) between 0 and 1.")
  
  if (!(methods::is(sigma_min, "numeric") && methods::is(sigma_max, "numeric") &&
        (sigma_min > 0 && sigma_min < 1) && (sigma_max > 0 && sigma_max < 1) &&
        (length(sigma_min) == 1) && (length(sigma_max) == 1))) {
    stop("'sigma_min' and 'sigma_max' must be scalar values between 0 and 1.")
  }
  
  if (!(methods::is(transition_point, "numeric") &&
        (transition_point > 0 && transition_point < 1) &&
        length(transition_point) == 1)) {
    stop("'transition_point' must be a scalar value between 0 and 1.")
  }
  
  if (! (methods::is(slope, "numeric") && slope > 0 && length(slope) == 1)) {
    stop("'slope' must be a positive scalar value.")
  }
  
  u <- (t - transition_point) / (1 - 0)
  sigma_val <- (sigma_max - sigma_min) / (1 + exp(-slope * u)) + sigma_min
  return(sigma_val)
}