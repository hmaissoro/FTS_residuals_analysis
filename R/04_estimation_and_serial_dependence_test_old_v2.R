library(data.table)
library(ggplot2)
library(adaptiveFTS)
library(np)
library(locfit)
library(lmtest)
library(tseries)
library(car)
source("./R/02_sigma_function.R")
source("./R/06_local_poly_loocv_func.R")

# ------------------------------------------------------------------------
# /!\ Change (N, lambda) and run the script
#
# (N, lambda) \in \{(150, 40), (150, 90), (400, 300), (1000, 40), (1000, 1000)}
#
# ------------------------------------------------------------------------

# Import the data
N <- 150
lambda <- 40

dt_raw <- readRDS(paste0("./data/data_N=", N, "_lambda=", lambda, ".RDS"))

ggplot(data = dt_raw, aes(x = tobs, y = X, group = id_curve)) +
  geom_line(colour = "#154360")  +
  xlab(label = "t") +
  ylab(label = latex2exp::TeX("$X_n(t)$  ")) +
  theme_minimal() +
  theme(axis.title = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.x =  element_text(size = 18),
        axis.text.y =  element_text(size = 18))

dt_raw[, epsilon := rnorm(.N, 0, 1), by = "id_curve"]

# Generate epsilon and add sigma function s
dt_raw[, epsilon := rnorm(.N, 0, 1), by = "id_curve"]
dt_raw[, sigma_logistic := sigma_logistic(t = tobs, sigma_min = 0.2, sigma_max = 0.8, slope = 15)]
dt_raw[, sigma_linear := sigma_linear(t = tobs, sigma_min = 0.2, sigma_max = 0.8)]
dt_raw[, sigma_arctan_periodic := sigma_arctan_periodic(t = tobs, A = 1)]
dt_raw[, sigma_sin := sigma_sin(t = tobs)]

# Evaluate sigma function
dt_raw[, eta_logistic := sigma_logistic * epsilon]
dt_raw[, eta_linear := sigma_linear * epsilon]
dt_raw[, eta_arctan_periodic := sigma_arctan_periodic * epsilon]
dt_raw[, eta_sin := sigma_sin * epsilon]

# Compute eta = Sigma * epsilon
dt_raw[, eta_logistic := sigma_logistic * epsilon]
dt_raw[, eta_linear := sigma_linear * epsilon]
dt_raw[, eta_arctan_periodic := sigma_arctan_periodic * epsilon]
dt_raw[, eta_sin := sigma_sin * epsilon]

# Compute observed points
dt_raw[, Yobs_logistic := X + eta_logistic]
dt_raw[, Yobs_linear := X + eta_linear]
dt_raw[, Yobs_arctan_periodic := X + eta_arctan_periodic]
dt_raw[, Yobs_sin := X + eta_sin]

# Function for computing serial dependence tests on a single cuvrve
get_serial_dependence_test_pval <- function(dt_raw, index_curve = 1, yobs_var = "Yobs_logistic") {
  # Extract the index_curve-th curve data
  dt_one_curve <- dt_raw[id_curve == index_curve, .(tobs, "Yobs" = get(yobs_var))]
  
  # Local polynomial smoothing
  # bw_model <- np::npregbw(data = dt_one_curve, formula = Yobs ~ tobs, degree = 0, bwmethod = "cv.ls")
  # res_npreg <- np::npreg(bws = bw_model, degree = 0, residuals = TRUE)
  # m_estim <- res_npreg$mean
  # eta_estim <- res_npreg$resid
  
  res_loess <- local_poly_loocv(
    x = dt_one_curve[, tobs],
    y = dt_one_curve[, Yobs],
    degree = 2, span_grid = (5:15) / 15)
  
  m_estim <- res_loess$yhat
  eta_estim <- res_loess$residuals
  
  # Residuals extracting
  dt_one_curve[, c("mhat", "eta_hat") := .(m_estim, eta_estim)]
  dt_one_curve[, eta_hat_square := eta_hat ** 2]
  
  # Estimation of \sigma^2
  # bw_model_eta <- np::npregbw(data = dt_one_curve, formula = eta_hat_square ~ tobs, degree = 2, bwmethod = "cv.ls")
  # res_npreg_eta <- np::npreg(bws = bw_model_eta, degree = 2, residuals = TRUE)
  # sigma_square_estim <- res_npreg_eta$mean
  
  res_loess_eta <- local_poly_loocv(
    x = dt_one_curve[, tobs],
    y = dt_one_curve[, eta_hat_square],
    degree = 2, span_grid = (5:15) / 15)
  
  sigma_square_estim <- res_loess_eta$yhat
  
  dt_one_curve[, sigma_square := sigma_square_estim]
  
  # Estimate of epsilon residuals
  dt_one_curve[, epsilon_hat := (Yobs - mhat) / sqrt(sigma_square)]
  dt_one_curve[, epsilon_hat_lag := shift(epsilon_hat, n = 1, type = "lag")]
  
  # Durbin-Watson test
  dw_pval <- lmtest::dwtest(lm(dt_one_curve$epsilon_hat ~ 1))$p.value
  
  # Breusch-Godfrey Test
  bg_pval <- lmtest::bgtest(lm(dt_one_curve$epsilon_hat ~ 1))$p.value
  
  # Box-Pierce and Ljung-Box tests on residuals
  box_pierce_pval <- Box.test(dt_one_curve$epsilon_hat, lag = 1, type = "Box-Pierce")$p.value
  ljung_box_pval <- Box.test(dt_one_curve$epsilon_hat, lag = 1, type = "Ljung-Box")$p.value
  
  # Student test
  res_lm <- lm(dt_one_curve$epsilon_hat ~ dt_one_curve$epsilon_hat_lag)
  ttest_pval <- summary(res_lm)$coefficients[2, 4]
  
  dt_res_pval <- data.table::data.table(
    "id_curve" = index_curve,
    "sigma_function" = gsub("Yobs", "sigma", yobs_var),
    "dw_pval" = dw_pval,
    "bg_pval" = bg_pval,
    "box_pierce_pval" = box_pierce_pval,
    "ljung_box_pval" = ljung_box_pval,
    "ttest_pval" = ttest_pval)
  
  return(dt_res_pval)
}

# Serial dependence test on all curve and all sigma version

var_vec <- c("Yobs_logistic", "Yobs_linear", "Yobs_arctan_periodic", "Yobs_sin")

dt_res_serial_dep_test <- data.table::rbindlist(lapply(1:N, function(i){
  dt_res <- data.table::rbindlist(lapply(var_vec, function(var_j){
    dt_res_one_var <- get_serial_dependence_test_pval(dt_raw = dt_raw, index_curve = i, yobs_var = var_j)
    return(dt_res_one_var)
  }))
  return(dt_res)
}))
saveRDS(dt_res_serial_dep_test, file = paste0("./data/data_serial_dependence_test_N=", N, "_lambda=", lambda, ".RDS"))






