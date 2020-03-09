# auth: christian bitter
# date: 2020
# desc:
# in this example, we standardize a 2D random normal

rm(list = ls());

library(dplyr);
library(ggplot2);
library(mvtnorm);

set.seed(1234);

# first we are going to simulate some 2D random normal
mu <- c(1, 1);
S  <- c(1, .5, .5, 2);
S  <- matrix(S, ncol = 2, nrow = 2, byrow = T);

N   <- 1e4;
N2D <- 
  mvtnorm::rmvnorm(n = N, mean = mu, sigma = S) %>%
  as_tibble();

plot_title   <- "Statistical Computing Techniques";
plot_caption <- "(c) Christian Bitter 2020";

N2D %>%
  ggplot(aes(V1, V2)) + 
  geom_point() + 
  geom_density_2d() + 
  labs(title = plot_title, caption = plot_caption,
       subtitle = "Simulating 2D random uniform from 2D standard normal",
       x = "x1", y = "x2") +
  theme_light();
  
# so how can we standardize this ... 
# multiply with the inverse of the variance-covariance structure
# and we subtract of the mean vector
# so let's compute the inverse
SInv <- solve(S);
# test that SInv is indeed the inverse to S
cat("SInv is inverse of S?: ", sum(SInv %*% S == diag(nrow = 2, ncol = 2)) == 4, "\r\n");

# now factor the matrix
f <- eigen(SInv, symmetric = T);
sigma <- f$values;
v <- f$vectors;
Q <- v %*% diag(sigma, ncol = 2) %*% t(v);

# now multiply with the inverse factored and subtract of the mean
Z2D <- N2D - matrix(mu, ncol = 2);
Z2D <- Q %*% t(Z2D);

Z2D <- 
  t(Z2D) %>%
  as_tibble();

Z2D %>%
  ggplot(aes(V1, V2)) + 
  geom_point() + 
  geom_density_2d() + 
  labs(title = plot_title, caption = plot_caption,
       subtitle = "Standardizing a simulated 2D multivariate gaussian",
       x = "x1", y = "x2") +
  theme_light();

# let's overlay with a freshly generated 2D Z
Z2G <- mvtnorm::rmvnorm(n = N, mean = c(0, 0)) %>% as_tibble();

Z2D %>%
  ggplot(aes(V1, V2)) + 
  geom_point(data = Z2D, aes(colour = "Standardized")) + 
  geom_point(data = Z2G, aes(x = V1, y = V2, colour = "Generated")) + 
  labs(title = plot_title, caption = plot_caption,
       subtitle = "Standardizing a simulated 2D multivariate gaussian",
       x = "x1", y = "x2") +
  theme_light();
