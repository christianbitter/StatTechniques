#auth: christian bitter
#desc: experiments around the wiener process or brownian motion

rm(list = ls());

library(dplyr);
library(ggplot2);
library(latex2exp);

set.seed(1234);

source("r/common.R");

# In the following example we assume that T equals N and we sample each t_i
# However, since the wiener process is the continuous equivalent to the 
# symetric random walk, we actually define the time interval and sample 
# at regular points in that interval - example 2
N  <- 1e3;
W_j <- 0;
W  <- rep(0, N);
# Wt - Ws ~ N(0, t - s)
# so W_t = W_s + N(0, t-s) = W_s + d_W
# so the Wiener is essentially a regressive component (W_i) + random noise

for (t in 1:N) {
  # generate the variance component, i.e. Wt-Ws ~ N(0, t - s);
  s      <- t - 1;
  stddev <- sqrt(t - s);
  Z_i    <- rnorm(n = 1, mean = 0, sd = 1);
  # N(0, t - s) = sqrt(t-s) * Z
  d_W    <- stddev * Z_i;
  W[t]    <- W_j + d_W;
  
  W_j    <- W[t];
}

W <- c(0, W);
df <- data.frame("t" = seq(0, length(W) - 1), "W" = W);

plot_title   <- "Statistical Computing - Wiener Process";
plot_caption <- "(c) Christian Bitter";

df %>%
  ggplot(aes(x = t, y = W)) + 
  geom_point(size = .5) + 
  geom_line(linetype = 4, colour = "gray") + 
  labs(title = plot_title, caption = plot_caption,
       x = "t", y = latex2exp::TeX("$S_n$"),
       subtitle = sprintf("Simulating Brownian Motion sampled at fixed integer increments (e.g, dt = 1) with T = N = %s", N)) + 
  theme_light();


# Now in our first example we sampled at integer positions j until T = N
# in the following example we take T to be a fixed integer and sample
# at any point in 0..T
T     <- 1;
N     <- 1e3;
dt    <- T / N;
W     <- rep(0, N + 1);
t_j   <- 0;

for (ix in 1:N) {
  t_i <- t_j + dt;
  z_i <- rnorm(n = 1, mean = 0, sd = 1);
  W[ix + 1] <- W[ix] + (sqrt(t_i - t_j) * z_i);
}

df <- data.frame("t" = seq(0, T, dt), "W" = W);
df %>%
  ggplot(aes(x = t, y = W)) + 
  geom_point(size = .5) + 
  geom_line(linetype = 4, colour = "gray") + 
  labs(title = plot_title, caption = plot_caption,
       x = "t", y = latex2exp::TeX("$S_n$"),
       subtitle = sprintf("Simulating Brownian Motion sampled at arbitrary real increments (e.g, dt = %s) with T = %s", dt, T)) + 
  theme_light();

# Now, let's optimize by vectorizing, some elements
T <- 1;
N <- 1e3;
z <- rnorm(n = N, mean = 0, sd = 1);
# how can we vectorize the z-scaling ...
times <- seq(0, T, T/N);
s     <- sqrt(diff(times));
W     <- rep(0, N);
for (i in 2:N) {
  W[i] <- W[i - 1] + s[i] * z[i];
}

df <- data.frame("t" = times, "W" = c(0, W));
df %>%
  ggplot(aes(x = t, y = W)) + 
  geom_point(size = .5) + 
  geom_line(linetype = 4, colour = "gray") + 
  labs(title = plot_title, caption = plot_caption,
       x = "t", y = latex2exp::TeX("$S_n$"),
       subtitle = sprintf("Simulating Brownian Motion sampled at arbitrary real increments (e.g, dt = %s) with T = %s", dt, T)) + 
  theme_light();

# now simple function
brownian <- function(T, N) {
  if (is.null(T)) stop("brownian - T cannot be null");
  if (is.null(N)) stop("brownian - N cannot be null");
  if (N < T) stop("brownian - N < T");
  
  z <- rnorm(n = N, mean = 0, sd = 1);
  # how can we vectorize the z-scaling ...
  times <- seq(0, T, T/N);
  s     <- sqrt(diff(times));
  W     <- rep(0, N);
  for (i in 2:N) {
    W[i] <- W[i - 1] + s[i] * z[i];
  }
  df <- data.frame("t" = times, "W" = c(0, W));
  return(df);
}

# and with it two brownians
T  <- 2;
b0 <- brownian(T = T, N = 1e1);
b0$name <- "b0";

b1 <- brownian(T = T, N = 1e3);
b1$name <- "b1";

df <- rbind(b0, b1);
df %>%
  ggplot(aes(x = t, y = W, colour = name)) + 
  geom_point(size = .5) + 
  geom_line(linetype = 4) + 
  labs(title = plot_title, caption = plot_caption,
       x = "t", y = latex2exp::TeX("$S_n$"),
       subtitle = sprintf("Simulating two brownian motions over the same time horizon (T = %s) with different resolution", 2)) + 
  theme_light() + 
  theme(legend.title = element_blank());

# Now allow interpolation
# linear interpolation between brownian at t
interpolate.brownian <- function(df_brownian, t) {
  times <- df_brownian$t;
  W     <- df_brownian$W;
  
  k  <- sum(times) < t;
  ki <- k + 1;
  b  <- (t - times[k]) / (times[ki] - times[k]);
  return(W[k] + b * (W[ki] - W[k]));
}