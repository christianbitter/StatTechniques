rm(list = ls());

library(ggplot2);
library(dplyr);


set.seed(1234);

#
# generating a 2d uniform from some standard normals
#

n <- 1e3;
d <- 2;

z <- rnorm(n =  n * d, mean = 0, sd = 1);
Z <- matrix(data = z, ncol = 2, byrow = T);

df <- data.frame(x = Z[, 1], y = Z[, 2]);

df %>% 
  ggplot(aes(x, y)) + 
  geom_point();

# we have to normalize

mag <- function(x) {
  sqrt(x[1]*x[1] + x[2]*x[2]);
}
normalize <- function(x) {
  l <- mag(x);
  linv = 1. / l;
  return(linv * x);
}

U <- lapply(X = 1:n, FUN = function(i)normalize(Z[i, ]));
U <- matrix(data = unlist(U), ncol = 2, byrow = T)

df <- data.frame(zX = Z[, 1], zY = Z[, 2],
                 uX = U[, 1], uY = U[, 2]);

df %>% 
  ggplot(aes(x, y)) + 
  geom_point(aes(zX, zY, colour = "Z")) + 
  geom_point(aes(uX, uY, colour = "U")) + 
  labs(title = "Statistical Computing Techniques",
       subtitle = "Simulating 2D random uniform from 2D standard normal",
       x = "x1", y = "x2") +
  theme_light();