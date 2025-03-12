library(data.table)
library(adaptiveFTS)

## {M_n} distribution
bounded_uniform <- function(N, lambda, p = 0.2){
  sample(
    x = seq(floor(lambda * (1 - p)), floor(lambda * (1 + p)), by = 1),
    size = N,
    replace = TRUE
  )
}

# Data generation function
gen_data <- function(N_to_use = 150, lambda_to_use = 40) {
  dt_rd <- adaptiveFTS::simulate_far(
    N = N_to_use, lambda = lambda_to_use,
    tdesign = "random",
    Mdistribution = bounded_uniform,
    tdistribution = runif,
    tcommon = NULL,
    hurst_fun = function(t) hurst_logistic(t, 0.4, 0.8, 50),
    L = 1,
    far_kernel = function(s,t) get_real_data_far_kenel(s,t, operator_norm = 0.5),
    far_mean = get_real_data_mean,
    int_grid = 100L, burnin = 100L,
    remove_burnin = TRUE)
  
  dt_rd[, ttag := NULL]
  dt_rd[, far_mean := NULL]
  
  saveRDS(dt_rd, paste0("./data/data_N=", N_to_use, "_lambda=", lambda_to_use, ".RDS"))
}

# Generate the data
gen_data(N_to_use = 150, lambda_to_use = 40)
gen_data(N_to_use = 150, lambda_to_use = 90)
gen_data(N_to_use = 400, lambda_to_use = 300)
gen_data(N_to_use = 1000, lambda_to_use = 40)
gen_data(N_to_use = 1000, lambda_to_use = 1000)

