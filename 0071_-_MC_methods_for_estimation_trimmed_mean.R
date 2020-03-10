#auth: christian bitter
#desc: trimmed mean examples

rm(list = ls());

library(tidyverse);
library(latex2exp);plot_title   <- "Statistical Computing in R - MC Methods";
plot_caption <- "(c) 2020 christian bitter";

# simulate the 1st order trimmed mean
N      <- 20;
mu     <- 0;
sigma2 <- 1;
sigma  <- sqrt(sigma2);

X <- rnorm(N, mean = mu, sd = sigma);
k <- 1;
X_df <- 
  data.frame(x = X, px = dnorm(X, mu, sigma), id = 1) %>%
  dplyr::arrange(x) %>%
  dplyr::mutate(n = dplyr::row_number()) %>%
  dplyr::mutate(mark = dplyr::row_number() <= 0 + k | dplyr::row_number() >= (N + 1 - k));

X_df %>%
  dplyr::filter(mark == F) %>%
  dplyr::summarize(trimmed_mean = sum(x) / n(),
                   id = max(id)) %>%
  dplyr::right_join(X_df, by = "id") %>%
  ggplot(aes(x = x, y = px, colour = mark)) + 
  geom_point() + 
  geom_vline(aes(xintercept = unique(trimmed_mean)), colour = "blue") +
  geom_vline(aes(xintercept = mean(x)), colour = "black") +
  labs(title = plot_title, caption = plot_caption,
       x = "x", y = latex2exp::TeX("$p_{x}$"),
       subtitle = "1st order trimmed mean of standard normal r.v.") + 
  theme_light()

# now that we have a procedure for generating a trimmed mean ... simulate some M
# number of times
sim_trimmed_mean <- function() {
  X <- rnorm(N, mean = mu, sd = sigma);
  k <- 1;
  tm <-
    data.frame(x = X, px = dnorm(X, mu, sigma), id = 1) %>%
    dplyr::arrange(x) %>%
    dplyr::mutate(n = dplyr::row_number()) %>%
    dplyr::mutate(mark = dplyr::row_number() <= 0 + k | dplyr::row_number() >= (N + 1 - k)) %>%
    dplyr::filter(mark == F) %>%
    dplyr::summarize(trimmed_mean = sum(x) / n(),
                     id = max(id)) %>% .[["trimmed_mean"]];
  return(tm);
}

M <- 1e3;
tms <- replicate(n = M, expr = sim_trimmed_mean());

# now compute the statitics estimates 
mse    <- mean((tms - mu_hat)^2);
mu_hat <- mean(tms, main = "Trimmed Mean");
se     <- sqrt(sum((tms - mu_hat)^2)) / M;
plot(tms, col = "gray", main = "Statistical Computing - Trimmed Mean Statistics",
     sub = "(c) 2020 Christian bitter");
abline(h = mu_hat, col = "blue");
abline(h = mse, col = "green");
abline(h = c(mu_hat + se, mu_hat - se), col = "red");
