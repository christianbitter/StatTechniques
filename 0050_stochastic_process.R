rm(list = ls());

library(dplyr);
library(ggplot2);

# desc: a simple random process
# a random process {X(t): t > 0} is a set of r.v. indexed by t
# typically, t represents (continuous or discrete) time.
# the values of X(t) denotes the range of the random process

t0  <- 10;
t_i <- 1:t0;
X_t <- rep(0, length(t_i));


# the following is a simple random process where we allow X_i to be any
# value coming from a random unform within -1 and 1
for (i in t_i) {
  X_t[i] <- runif(1, min = -1, max = 1);
}

df <- data.frame("T" = t_i, "X_t" = X_t) %>% as_tibble();

# compare
plot_title   <- "Statistical Techniques - Stochastic Process";
plot_caption <- "(c) Christian Bitter 2020";

df %>%
  ggplot(aes(x = T, y = X_t)) + 
  geom_point(aes(colour = "X(t)")) + 
  geom_point(aes(x = T, y = cumsum(X_t), colour = "Sum(X(t))")) + geom_line(linetype = 2, colour = "black") + 
  theme_light() + 
  labs(title = plot_title, caption = plot_caption,
       subtitle = "Simple random process, where X(t) ~ Unif(-1, 1)",
       x = "T", y = "X(t)");