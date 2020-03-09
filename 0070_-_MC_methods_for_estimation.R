#auth: christian bitter
#desc: Monte Carlo based recovery of parameter mu of normal variable

rm(list = ls());

library(tidyverse);
library(latex2exp);

plot_title   <- "Statistical Computing in R - MC Methods";
plot_caption <- "(c) 2020 christian bitter";

# in the first instance we obtain draws from a normal r.v.
# aiming to recover it's mean mu
N  <- 1e3;

n  <- 1e2;
mu <- 2;
sd <- 1;
x  <- rnorm(n = n, mean = mu, sd = sd);

# now compute the sample mean
mu_hat <- mean(x);

sim_df <- data.frame("x" = x);
sim_df %>%
  ggplot(aes(x = x)) + 
  geom_histogram() + 
  geom_vline(aes(xintercept = mu, colour = "mu")) + 
  geom_vline(aes(xintercept = mu_hat, colour = "muhat")) +
  labs(title = plot_title, caption = plot_caption,
       x = "X",
       subtitle = "Our estimate is dependent on sample size, simulating only one time leaves room for improvement") + 
  theme_light();

# let's look at how generating a sampling distribution of our estimate and computing the mean
# estimate from there, as the approximation for muhat.

estimator_g <- function(X)mean(X);

sim_x <- function()estimator_g(rnorm(n = n, mean = mu, sd = sd));

sampling_distribution <- replicate(n = N,
                                   expr = sim_x(),
                                   simplify = T);
muhatr <- mean(sampling_distribution);
cat("Difference between one sample muhat and N sample muhat: ", mu_hat, "<->", muhatr, "=>", 
    100 * abs(muhatr - mu_hat) / mu_hat, "%");

data.frame("x" = sampling_distribution) %>%
  ggplot(aes(x = x)) + 
  geom_histogram() + 
  geom_vline(aes(xintercept = mu, colour = "mu")) + 
  geom_vline(aes(xintercept = mean(x), colour = "muhatr")) +
  geom_vline(aes(xintercept = mu_hat, colour = "muhat")) +
  scale_color_discrete(name = "Estimates") + 
  labs(title = plot_title, caption = plot_caption,
       x = "X",
       subtitle = "Computing the estimate from a number of simulations.") + 
  theme_light();