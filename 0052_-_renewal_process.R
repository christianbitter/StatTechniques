rm(list = ls());
library(ggplot2);
library(dplyr);
library(latex2exp);

set.seed(1234);

source("r/common.R");

# desc: a renewal process is the generalization of a poisson process, i.e.
#       a stochastic process of events/ arrivals where the arrival times 
#       are distributed according to some general distribution not
#       necessarily a poisson
# the example is taken from statistical computing with R / Rizzo
# the interarrival times of some renewal process are distributed according
# to the geometric distribution

sim_fn <- function(t0 = 5, M = 1e2, p = .2) {
  # cat("Simulating with t0=", t0, "\r\n");
  # t0 <- 5; # time frame
  Tn <- rgeom(M, prob = p); # interarrival times
  Sn <- cumsum(Tn); #arrivals
  n  <- min(which(Sn > t0)); # which are in t0
  
  return(n - 1);
}

N <- 1e1;

out <- replicate(n = N, expr = sim_fn());
cat("Simulating the geometric renewal process\r\n");
# in order to estimate the expected number of arrivals we vary the time frame
N  <- 1e3;
t_0 <- seq(0, 30, .1);
vi <- matrix(0, nrow = length(t_0), ncol = 2);
p  <- .2;
for (ix in 1:length(t_0)) {
  t_i      <- t_0[ix];
  out_i    <- replicate(n = N, expr = sim_fn(t0 = t_i, M = 1e3, p = p));
  vi[ix, ] <- c(t_i, mean(out_i));
}

df <- data.frame(vi);
names(df) <- c("ti", "E");

plot_title   <- "Statistical Computing - Renewal Process";
plot_caption <- "(c) Christian Bitter"

df %>%
  ggplot(aes(x = ti, y = E)) + 
  geom_point(size = .5) + 
  labs(title = plot_title, caption = plot_caption,
       x = "t", y = "E",
       subtitle = "Simulation of Geometrically Distributed Renewal Process") + 
  theme_light();

# the geometric-renewal process is the discrete analog to the continuous poisson process
# that is a geometric with p = .2 means success rate is .2
# the geometric distribution has expectation of (1 - p)/p

EX <- (1 - p) / p; 
paste("Theoretical expectation of renewal process with geometrically", 
      sprintf("distributed interarrival times (p = %s) is %s", p, EX), collapse = "\n");

# so we have 4 events in one unit of time, meaning that ti = 1, lambda as 
# the rate of arrivals should be ti/EX = 1/4.
# meaning we need to simulate a poisson process using lambda * ti = ti/EX
cat("Adding the theoretic poisson expectation\r\n");
df <- 
  df %>%
    dplyr::mutate(TEP = ti / EX);

df %>%
  ggplot(aes(x = ti, y = E)) + 
  geom_point(aes(colour = "Geometric"), size = .5 ) + 
  geom_line(aes(x = ti, y = TEP, colour = "Theoretic Poisson")) + 
  labs(x = latex2exp::TeX("$t_i$"), y = latex2exp::TeX("$E\\left[X\\right]$"),
       title = plot_title, caption = plot_caption,
       subtitle = "Overlay of simulated geometric renewal process and theoretic poisson") + 
  theme_light() + 
  theme(legend.title = element_blank());

# to complete the circle we now also simulate the poisson
cat("Simulating the Poisson process\r\n");
N   <- 1e3;
t_0 <- seq(0, 30, .1);
vj  <- matrix(0, nrow = length(t_0), ncol = 2);
for (ix in 1:length(t_0)) {
  t_i      <- t_0[ix];
  out_i    <- replicate(n = N, expr = poisson_process(t0 = t_i, lambda = 1 / EX, upper = 100));
  vj[ix, ] <- c(t_i, mean(out_i));
}

dfj <- data.frame("ti" = vj[, 1], "EP" = vj[, 2]);
df <- df %>%  dplyr::inner_join(dfj);

df %>%
  ggplot(aes(x = ti, y = E)) + 
  geom_point(aes(colour = "Geometric"), alpha = .5, size = .75) + 
  geom_point(aes(x = ti, y = EP, colour = "Simulated Poisson"), alpha = .5, size =.75 ) + 
  geom_line(aes(x = ti, y = TEP, colour = "Theoretic Poisson")) + 
  labs(x = latex2exp::TeX("$t_i$"), y = latex2exp::TeX("$E\\left[X\\right]$"),
       title = plot_title, caption = plot_caption,
       subtitle = "Overlay of expectations of simulated geometric renewal process, theoretic poisson process and simulated poisson process.") + 
  theme_light() + 
  theme(legend.title = element_blank());
