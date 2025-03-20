library(data.table)
library(ggplot2)
source("./R/02_sigma_function.R")

# Plot Sinus sigma function
dt_sig_sinus <- data.table(
  "t" = seq(0, 1, len = 100), 
  "sig" = sigma_sin(seq(0, 1, len = 100), sigma_min = 0.25, sigma_max = 0.5)
)
g_sig_sinus <- ggplot(data = dt_sig_sinus, aes(x = t, y = sig)) +
  geom_line(linewidth = 0.9, colour = "#154360")  +
  # ylim(0, 1) +
  xlab(label = "t") +
  ylab(label = latex2exp::TeX("$\\sigma(t)$  ")) +
  scale_color_grey() +
  theme_minimal() +
  theme(axis.title = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.x =  element_text(size = 18),
        axis.text.y =  element_text(size = 18))
g_sig_sinus
ggsave(
  plot = g_sig_sinus,
  filename = "./graphs/sigma_sinus.png",
  width = 7, height = 5, units = "in", dpi = 300, bg = "white")
rm(g_sig_sinus) ; gc()

# Plot Arctan periodic sigma function
dt_sig_arctan_periodic <- data.table("t" = seq(0, 1, len = 100), "sig" = sigma_arctan_periodic(seq(0, 1, len = 100), A = 1))
g_sig_arctan_periodic <- ggplot(data = dt_sig_arctan_periodic, aes(x = t, y = sig)) +
  geom_line(linewidth = 0.9, colour = "#154360")  +
  # ylim(0, 1) +
  xlab(label = "t") +
  ylab(label = latex2exp::TeX("$\\sigma(t)$  ")) +
  scale_color_grey() +
  theme_minimal() +
  theme(axis.title = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.x =  element_text(size = 18),
        axis.text.y =  element_text(size = 18))
g_sig_arctan_periodic
ggsave(
  plot = g_sig_arctan_periodic,
  filename = "./graphs/sigma_arctan_periodic.png",
  width = 7, height = 5, units = "in", dpi = 300, bg = "white")
rm(g_sig_arctan_periodic) ; gc()

# Plot Arctan sigma function
dt_sig_arctan <- data.table("t" = seq(0, 1, len = 100), "sig" = sigma_arctan(seq(0, 1, len = 100)))
g_sig_arctan <- ggplot(data = dt_sig_arctan, aes(x = t, y = sig)) +
  geom_line(linewidth = 0.9, colour = "#154360")  +
  # ylim(0, 1) +
  xlab(label = "t") +
  ylab(label = latex2exp::TeX("$\\sigma(t)$  ")) +
  scale_color_grey() +
  theme_minimal() +
  theme(axis.title = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.x =  element_text(size = 18),
        axis.text.y =  element_text(size = 18))
g_sig_arctan
ggsave(
  plot = g_sig_arctan,
  filename = "./graphs/sigma_arctan.png",
  width = 7, height = 5, units = "in", dpi = 300, bg = "white")
rm(g_sig_arctan) ; gc()

# Plot Linear sigma function
dt_sig_linear <- data.table(
  "t" = seq(0, 1, len = 100),
  "sig" = sigma_linear(seq(0, 1, len = 100), sigma_min = 0.25,sigma_max = 0.5)
)
g_sig_linear <- ggplot(data = dt_sig_linear, aes(x = t, y = sig)) +
  geom_line(linewidth = 0.9, colour = "#154360")  +
  # ylim(0, 1) +
  xlab(label = "t") +
  ylab(label = latex2exp::TeX("$\\sigma(t)$  ")) +
  scale_color_grey() +
  theme_minimal() +
  theme(axis.title = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.x =  element_text(size = 18),
        axis.text.y =  element_text(size = 18))
g_sig_linear
ggsave(
  plot = g_sig_linear,
  filename = "./graphs/sigma_linear.png",
  width = 7, height = 5, units = "in", dpi = 300, bg = "white")
rm(g_sig_linear) ; gc()

# Plot Logistic sigma function
dt_sig_logistic <- data.table(
  "t" = seq(0, 1, len = 100),
  "sig" = sigma_logistic(seq(0, 1, len = 100), slope = 30, sigma_min = 0.25, sigma_max = 0.5)
)
g_sig_logistic <- ggplot(data = dt_sig_logistic, aes(x = t, y = sig)) +
  geom_line(linewidth = 0.9, colour = "#154360")  +
  # ylim(0, 1) +
  xlab(label = "t") +
  ylab(label = latex2exp::TeX("$\\sigma(t)$  ")) +
  scale_color_grey() +
  theme_minimal() +
  theme(axis.title = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.x =  element_text(size = 18),
        axis.text.y =  element_text(size = 18))
g_sig_logistic
ggsave(
  plot = g_sig_logistic,
  filename = "./graphs/sigma_logistic.png",
  width = 7, height = 5, units = "in", dpi = 300, bg = "white")


dt_sig_logistic_bis <- data.table(
  "t" = seq(0, 1, len = 100),
  "sig" = sigma_logistic(seq(0, 1, len = 100), slope = 15, sigma_min = 0.5, sigma_max = 0.25)
)

g_sig_logistic_bis <- ggplot(data = dt_sig_logistic_bis, aes(x = t, y = sig)) +
  geom_line(linewidth = 0.9, colour = "#154360")  +
  # ylim(0, 1) +
  xlab(label = "t") +
  ylab(label = latex2exp::TeX("$\\sigma(t)$  ")) +
  scale_color_grey() +
  theme_minimal() +
  theme(axis.title = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.x =  element_text(size = 18),
        axis.text.y =  element_text(size = 18))
g_sig_logistic_bis
ggsave(
  plot = g_sig_logistic_bis,
  filename = "./graphs/sigma_logistic_bis.png",
  width = 7, height = 5, units = "in", dpi = 300, bg = "white")

