#auth: christian bitter
#desc: estimate the expectation of the absolute difference of two standard
# normal variables

rm(list = ls());

library(tidyverse);

plot_title   <- "Statistical Computing in R - MC Methods";
plot_caption <- "(c) 2020 christian bitter";

M <- 1e3;
X <- rep(0, M);

for (i in 1:M) {
  u1 <- rnorm(1);
  u2 <- rnorm(1);
  du <- abs(u1 - u2);
  X[i] <- du;
}

data.frame(x = X) %>%
  ggplot(aes(x = x)) + 
  geom_histogram() + 
  geom_vline(aes(xintercept = mean(x), colour = "Sample mean")) + 
  labs(caption = plot_caption,
       x = "x", y = "#") +
  ggtitle(label = plot_title,
          subtitle = latex2exp::TeX("Assuming $X_{1} , X_{2} \\sim N(0, 1)$ , we recover $\\E\\lbrack |X_{1} - X_{2}| \\rbrack$")) + 
  theme_light();

# if we look at the difference of the two standard normal r.v., we see
# that the resulting distribution of Z = X1 - X2 has mean 0 and variance 2
M  <- 1e4;
X1 <- rnorm(M);
X2 <- rnorm(M);
Z  <- X1 - X2;
A  <- abs(Z);

df <- data.frame(x1 = X1, x2 = X2, z = Z, a = A, as = rnorm(2 * M, mean = 0, sd = sqrt(2)));

df %>%
  ggplot(aes(x = z)) + 
  geom_density(aes(x = z, colour = "simulated"), alpha = .3) + 
  geom_density(aes(x = as, colour = "theoretical"), alpha = .3) + 
  labs(title = plot_title, caption = plot_caption,
       subtitle = "Z = X1 - X2; where X1, X2 ~ N(0, 1); the theoretical standard normal is overlaid in blue", x = "z") + 
  theme_light();

# this should be distributed as a N(mu1 + mu2, var1 + var2) = N(0, sigma^2=2);
# looking at the simulated and theoretical, we can see that they are in quite
# close agreement

# now taking the absolute, will remove the negative values from our distribution
# and put more mass into the tails, since we have the previously negative ones
# counting for us as well
df %>%
  ggplot(aes(x = a)) + 
  geom_histogram() +
  labs(title = plot_title, caption = plot_caption,
       subtitle = "Z = |X1 - X2|; where X1, X2 ~ N(0, 1)", x = "z") + 
  theme_light();