rm(list = ls());

library(dplyr);
library(ggplot2);

#
# simulating a d-dimensional multivariate normal with mean m and variance s
# using a d-dimensional multivariate standard normal
#

# simulate some d-dimensional standard normal
n <- 1e3;
d <- 2;

z <- rnorm(n = n * d, mean = 0, sd = 1);
Z <- matrix(data = z, ncol = 2, byrow = T);

df <- data.frame(x = Z[, 1], y = Z[, 2]);

df %>% 
  ggplot(aes(x, y)) + 
  geom_point();

mu <- c(2, 2);

# variance in x and y
sd_x <- 1.;
sd_y <- 2.;
c_xy <- .5;

S  <- c(sd_x, c_xy, c_xy, sd_y);
S  <- matrix(data = S, ncol = 2, byrow = T);

# now factor the matrix
f <- eigen(S, symmetric = T);
sigma <- f$values;
v <- f$vectors;
Q <- v %*% diag(sigma, ncol = d) %*% t(v);
N <- Z %*% Q + matrix(data = mu, nrow = n, ncol = d);

# our target distribution is A * Z + mu * b;
df <- cbind(df, nX = N[, 1], nY = N[, 2]);

df %>% 
  ggplot(aes(x = x, y = y)) + 
  geom_point(aes(x = x, y = y, colour = "Z"), size = 1., alpha = .5) + 
  geom_point(aes(x = nX, y = nY, colour = "N"), size = 1., alpha = .5) + geom_density_2d(aes(x = nX, y = nY)) + 
  labs(title = "Statistical Computing Techniques",
       subtitle = "Simulating a 2-D N(mu, Sigma) from a 2-D standard normal Z",
       x = "x1", y = "x2") +
  theme_light()