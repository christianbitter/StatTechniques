rm(list = ls());

library(ggplot2);
library(dplyr);

set.seed(1245);

# using the uniform distribution
# we can obtain a random variable distributed as f
# by drawing samples from the uniform distribution and a limiting/ envelope
# distribution

# see the example 3.7 in statistical computing with r
# f is a beta(2,2) density
# g is the uniform(0,1)

# let's briefly look at the beta(2,2) density - using R built-in density
# and our knowledge of the analytical formula. Let's briefly compare
x <- seq(0, 1, length.out = 1000);
y <- dbeta(x, 2, 2);
ty <- 6 * x * (1 - x);
df_xy <- data.frame(x = x, f_x = y, f_tx = ty);
df_xy %>% ggplot(aes(x, y)) + 
  geom_point(size = .5) + 
  # geom_point(aes(x, y = ty), colour = "blue") + 
  labs(title = "Statistical Computation",
       subtitle = "Simulation of random variables via Hit-Reject - Beta(2,2)",
       x = "x", y = "f(X)") + 
  theme_light();

# Now enter the acceptance / accept-reject method
# we need our density f - which is the target density, i.e. beta(2,2)
f_x <- function(x) {
  return(6 * x * (1 - x));
}
# We need a way to generate random numbers and get the density from
# our envelope distribution g
g_x <- function(x = NULL){
  if (is.null(x)) {
    return(runif(1));
  } else { 
    return(dunif(x));
  }
}

# Now we are ready to implement the acceptance method by 
# generating a uniform u
# generating a random y ~ g()
# generating t = f(y) / c * g(y)
# accepting the proposal y if t > u

# if we change the envelope, our accept-reject becomes different
# e.g. c closer to 6 approaches the theoretical beta(2,2) much better
# than a c of 1. On the other picking a too large envelope, wastes samples

c <- 6;
n <- 1e3;
i <- 1;
j <- 0;
iter <- 0;
X <- rep(0, n);

while (i < n) {
  u <- runif(1);
  j <- j + 1;
  y <- g_x();
  t <- f_x(y) / (g_x(y) * c);
  if (u < t) {
    X[i] <- y;
    i = i + 1;
  }
}
cat("It took us ", j, " iterations to generate ", n, " samples.\r\n");
p <- seq(.1, .9, .1);
Qhat <- quantile(X, p);
Q <- qbeta(p, 2, 2);

xy <- seq(0,1,length.out = 1000);
plot(xy, qbeta(xy, 2, 2), col = "gray");
points(xy, quantile(X, xy), col = "red");