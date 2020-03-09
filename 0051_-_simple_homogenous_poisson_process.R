rm(list = ls());

library(ggplot2);
library(dplyr);

set.seed(1234);

# A poisson process is characterized by having events occur according to
# arrival time or occurrence rae lambda

S0     <- 0;
lambda <- 1;
t0 <- 100;
N_i <- rep(0, t0);
T_i <- 1:t0;
i <- 1;

while (S0 < t0) {
  T_i <- rexp(1, lambda);  
  S0 <- S0 + T_i;
  N_i[S0] <- 1;
}
N_i <- N_i[1:t0];

plot_title   <- "Statistical Techniques - Poisson Process";
plot_caption <- "(c) Christian Bitter 2020";

df <- data.frame("T" = 1:t0, "X" = N_i, N = cumsum(N_i));

df %>%
  as_tibble() %>%
  ggplot(aes(x = T, y = X)) + 
  geom_bar(stat = "identity", fill = "red") + 
  geom_line(aes(y = N), linetype = 2) + 
  theme_light() + 
  labs(x = "T", y = "N",
       caption = plot_caption, title = plot_title,
       subtitle = "Generating arrival events at T. Dashed line shows cumulative number of arrivals.");

# a 2nd approach to simulating a poisson process is 
# to fix the number of events in some interval t - getting a random poisson
# and then simulating the interarrival times leading up to the number of events
# using that conditional on the number arrivals, the interarrival times 
# are uniformly distributed
M      <- 1e3;

lambda <- 2;
upper  <- 100;
t0     <- 3;
sims   <- rep(0, M);

for (i in 1:M) {
  NoEvents <- rpois(n = 1, lambda * upper);
  UnorderedArrivalTimes <- runif(n = NoEvents, min = 0, max = upper);
  Sn <- sort(UnorderedArrivalTimes);
  
  # the smallest when we did not exceed - arrivals in 0 - t1
  N_t1    <- min(which(Sn > t0)) - 1;
  sims[i] <- N_t1;
}

hist(sims)
# expectation should be around lambda * t
mean(sims);
# variance should be around lambda * t
var(sims);