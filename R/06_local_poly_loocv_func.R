library(boot)  # For LOOCV
library(stats) # For loess

local_poly_loocv <- function(x, y, degree = 2, span_grid = (1:40) / 40) {
  # Ensure x and y are numeric vectors
  if (!is.numeric(x) || !is.numeric(y)) stop("x and y must be numeric vectors.")
  if (length(x) != length(y)) stop("x and y must have the same length.")
  
  n <- length(y)
  loocv_errors <- numeric(length(span_grid))
  
  # Function to compute LOOCV error
  loocv_error <- function(sp) {
    loocv_sum <- 0
    for (i in 1:n) {
      dti <- data.table("x" = x[-i], "y" = y[-i])
      dti_test <- data.table("x" = x[i])
      loess_fit <- tryCatch(
        loess(y ~ x, data = dti, span = sp, degree = degree, control = loess.control(surface = "direct")),
        error = function(e) return(NULL)  # Skip failed fits
      )
      if (is.null(loess_fit)) next  # Skip this iteration if the model fails
      
      y_pred <- predict(loess_fit, newdata = dti_test)
      loocv_sum_i <- ifelse(!is.na(y_pred), (y[i] - y_pred)^2, 0)
      loocv_sum <- loocv_sum + loocv_sum_i
    }
    return(loocv_sum / n)  
  }
  
  
  # Compute LOOCV errors for each span in the grid
  loocv_errors <- sapply(span_grid, loocv_error)
  
  # Select best span (minimizing LOOCV error)
  best_span <- span_grid[which.min(loocv_errors)]
  
  # Fit final LOESS model with optimal span
  final_loess <- loess(y ~ x, span = best_span, degree = degree)
  
  # Predict on x
  y_hat <- predict(final_loess, newdata = x)
  
  # Compute residuals
  residuals <- y - y_hat
  
  # Return results as a list
  return(list(
    best_span = best_span,
    yhat = y_hat,
    loocv_errors = loocv_errors,
    residuals = residuals
  ))
}
