library(data.table)
library(ggplot2)

# ------------------------------------------------------------------------
# /!\ Change (N, lambda) and run the script
#
# (N, lambda) \in \{(150, 40), (150, 90), (1000, 40), (1000, 1000)}
#
# ------------------------------------------------------------------------
N <- 1000
lambda <- 40

# Import the estimated pvalues
dt_pval <- readRDS(paste0("./data/data_serial_dependence_test_N=", N, "_lambda=", lambda, ".RDS"))


dt_sigma_name <- data.table(
  "sigma_function" = c("sigma_logistic", "sigma_linear", "sigma_arctan_periodic", "sigma_sin"),
  "Sigma function" = c("Logistic", "Linear", "Arctan periodic", "Sinus")
)

dt_pval <- merge(dt_pval, dt_sigma_name, by = "sigma_function")

# Plot function 
get_pval_graph <- function(data = dt_pval, N_to_use = N, lambda_to_use = lambda,
                           var_pval = "dw_pval", sigma_fun = "sigma_logistic") {
  data_plot <- copy(data)
  data_plot[, val := get(var_pval)]
  if (sigma_fun == "all") {
    gplot <- ggplot(data = data_plot, aes(x = id_curve, y = val, group = `Sigma function`, color = `Sigma function`)) +
      geom_line(linewidth = 0.9)  +
      ylim(0, 1) +
      geom_hline(yintercept = 0.05, colour = "red", linetype = "dashed") + 
      xlab(label = "Curve index in FTS") +
      ylab(label = "P-Value") +
      ggtitle(label = latex2exp::TeX(paste0("$(N, \\lambda)$ = (", N_to_use, ", ", lambda_to_use, ")"))) +
      theme_minimal() +
      theme(axis.title = element_text(size = 18),
            axis.title.x = element_text(size = 18),
            axis.title.y = element_text(size = 18),
            axis.text.x =  element_text(size = 18),
            axis.text.y =  element_text(size = 18),
            plot.title = element_text(size = 18, hjust = 0.5))
    ggsave(
      plot = gplot,
      filename = paste0("./graphs/graph_", var_pval, "_test_pvalue_all_sigma_function_N=", N_to_use, "_lambda=", lambda_to_use, ".png"),
      width = 7, height = 5, units = "in", dpi = 300, bg = "white")
  } else {
    
    gplot <- ggplot(data = data_plot[sigma_function == sigma_fun], mapping = aes(x = id_curve, y = val)) +
      geom_line(linewidth = 0.9, colour = "#154360") +
      ylim(0, 1) +
      geom_hline(yintercept = 0.05, colour = "red", linetype = "dashed") + 
      xlab(label = "Curve index in FTS") +
      ylab(label = "P-Value") +
      ggtitle(label = latex2exp::TeX(paste0("$(N, \\lambda)$ = (", N_to_use, ", ", lambda_to_use, ")"))) +
      theme_minimal() +
      theme(axis.title = element_text(size = 18),
            axis.title.x = element_text(size = 18),
            axis.title.y = element_text(size = 18),
            axis.text.x =  element_text(size = 18),
            axis.text.y =  element_text(size = 18),
            plot.title = element_text(size = 18, hjust = 0.5))
    ggsave(
      plot = gplot,
      filename = paste0("./graphs/graph_", var_pval, "_", sigma_fun, "_N=", N_to_use, "_lambda=", lambda_to_use, ".png"),
      width = 7, height = 5, units = "in", dpi = 300, bg = "white")
    if (var_pval == "dw_pval") {
      ggsave(
        plot = gplot,
        filename = paste0( "../Hassan/report/adaptive_estimation/submission_JTS_2024_09/figures/residuals_dependence/",
                           "graph_", var_pval, "_", sigma_fun, "_N=", N_to_use, "_lambda=", lambda_to_use, ".png"),
        width = 7, height = 5, units = "in", dpi = 300, bg = "white")
    }
  }
  return(gplot)
}

# Durbin Watson test p-values
get_pval_graph(var_pval = "dw_pval", sigma_fun = "all")
get_pval_graph(var_pval = "dw_pval", sigma_fun = "sigma_logistic")
get_pval_graph(var_pval = "dw_pval", sigma_fun = "sigma_linear")
get_pval_graph(var_pval = "dw_pval", sigma_fun = "sigma_arctan_periodic")
get_pval_graph(var_pval = "dw_pval", sigma_fun = "sigma_sin")

# Breusch-Godfrey test p-values
get_pval_graph(var_pval = "bg_pval", sigma_fun = "all")
get_pval_graph(var_pval = "bg_pval", sigma_fun = "sigma_logistic")
get_pval_graph(var_pval = "bg_pval", sigma_fun = "sigma_linear")
get_pval_graph(var_pval = "bg_pval", sigma_fun = "sigma_arctan_periodic")
get_pval_graph(var_pval = "bg_pval", sigma_fun = "sigma_sin")

# Box-Pierce test p-values
get_pval_graph(var_pval = "box_pierce_pval", sigma_fun = "all")
get_pval_graph(var_pval = "box_pierce_pval", sigma_fun = "sigma_logistic")
get_pval_graph(var_pval = "box_pierce_pval", sigma_fun = "sigma_linear")
get_pval_graph(var_pval = "box_pierce_pval", sigma_fun = "sigma_arctan_periodic")
get_pval_graph(var_pval = "box_pierce_pval", sigma_fun = "sigma_sin")

# Ljung-Box test p-values
get_pval_graph(var_pval = "ljung_box_pval", sigma_fun = "all")
get_pval_graph(var_pval = "ljung_box_pval", sigma_fun = "sigma_logistic")
get_pval_graph(var_pval = "ljung_box_pval", sigma_fun = "sigma_linear")
get_pval_graph(var_pval = "ljung_box_pval", sigma_fun = "sigma_arctan_periodic")
get_pval_graph(var_pval = "ljung_box_pval", sigma_fun = "sigma_sin")

# Student test p-values
get_pval_graph(var_pval = "ttest_pval", sigma_fun = "all")
get_pval_graph(var_pval = "ttest_pval", sigma_fun = "sigma_logistic")
get_pval_graph(var_pval = "ttest_pval", sigma_fun = "sigma_linear")
get_pval_graph(var_pval = "ttest_pval", sigma_fun = "sigma_arctan_periodic")
get_pval_graph(var_pval = "ttest_pval", sigma_fun = "sigma_sin")



