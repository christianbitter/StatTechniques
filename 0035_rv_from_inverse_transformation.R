rm(list = ls());

library(ggplot2);
library(dplyr);

# See Statistical Computing with R ... 

# Exercise 3.5
x  <- c(0, 1, 2, 3, 4);
fx <- c(.1, .2, .2, .2, .3);
Fx <- cumsum(fx);

df <- data.frame(
  x = x, fx = fx, Fx = Fx
);

plot_title   <- "Statistical Computing in R";
plot_caption <- sprintf("(c) Christian Bitter / %s", date());

df %>%
  ggplot(aes(x = x)) + 
  geom_point(aes(x = x, y = fx, colour = "fx")) + geom_bar(stat = "identity", aes(x, y = fx), width = .1, alpha = .5) + 
  geom_point(aes(x = x, y = Fx, colour = "Fx")) + geom_step(aes(x = x, y = Fx, linetype = "1")) + 
  labs(title = plot_title, caption = plot_caption,
       x = "x", y = "pmf/ cdf",
       subtitle = "Exercise 3.5 - Simulating the discrete random variable x.") + 
  theme_light()

n  <- 1e3;
vx <- integer(n);

for (i in 1:n) {
  u <- runif(1);
  vx[i] <- which(Fx > u, arr.ind = T)[1] - 1;
}

# let's compare the actual vs. the emperical distribution
emperical <- table(vx) / n;
df <- cbind(df, data.frame(efx = as.numeric(emperical)));

df %>%
  ggplot(aes(x = x)) + 
  geom_point(aes(x = x, y = fx, colour = "fx")) + geom_bar(stat = "identity", aes(x, y = fx), width = .1, alpha = .5) + 
  geom_point(aes(x = x, y = efx, colour = "efx")) + geom_bar(stat = "identity", aes(x, y = efx), width = .1, alpha = .5) + 
  labs(title = plot_title, caption = plot_caption,
       x = "x", y = "f(x)/ F(x)",
       subtitle = "Exercise 3.5 - Comparison of theoretical and emperical pmf.\nVarying the number of samples, changes the similarity between the two.") + 
  theme_light()
