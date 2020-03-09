# auth: christian bitter
# date: 2020
# desc:
# simulate various distributions, building on the standard normal distribution

rm(list = ls());

library(ggplot2);
library(dplyr);

set.seed(1234);

# generate a sample of n chi2(1) - which is 
# which is based on chisq2(1) = Z^2, where Z^2 is one standard normal

n   <- 1e4;
Z   <- matrix(rnorm(n = n), ncol = 1);
C_1 <- Z * Z;

df <- data.frame("Chi2" = C_1, Type = "Simulated-From-Z");
df <- rbind(df, 
            data.frame("Chi2" = rchisq(n = n, df = 1), Type = "R-Built-in"));

# compare
plot_title   <- "Statistical Techniques - R.V. from Standard Normal";
plot_caption <- "(c) Christian Bitter 2020";

df %>%
  ggplot(aes(x = Chi2, fill = Type)) + 
  geom_histogram() + 
  facet_grid(Type ~ .) + 
  labs(title = plot_title, caption = plot_caption, x = "x", y = "#",
       subtitle = "Comparison of Chi^2 from Z and R-builtin") + 
  theme_light();

df %>%
  ggplot(aes(y = Chi2, fill = Type)) + 
  geom_boxplot() + 
  facet_grid(. ~ Type) + 
  labs(title = plot_title, caption = plot_caption, x = "x", y = "#",
       subtitle = "Comparison of Chi^2 from Z and R-builtin") + 
  theme_light();

# Now do the same but for a Chi2 with m degree of freedom
N <- 1e4;
M <- 10;
Z <- matrix(rnorm(n = N * M), ncol = M);
Chi2 <- Z * Z;
Chi2 <- apply(X = Chi2, MARGIN = 1, FUN = sum);

df <- data.frame("Chi2" = Chi2, Type = "Simulated-From-Z");
df <- rbind(df, 
            data.frame("Chi2" = rchisq(n = N, df = M), Type = "R-Built-in"));
df %>%
  ggplot(aes(y = Chi2, fill = Type)) + 
  geom_boxplot() + 
  facet_grid(. ~ Type) + 
  labs(title = plot_title, caption = plot_caption, x = "x", y = "#",
       subtitle = "Comparison of Chi^2 from Z and R-builtin") + 
  theme_light();


# next up - simulate the ratio of 2 Chi-squared A and B with dof n, m
# then the ratio of A/n / B/ m ~ F_n_m
r_chi2 <- function(N, M) {
  if (N < 1) stop("r_chi2 - N < 1");
  if (M < 1) stop("r_chi2 - M < 1");
  
  Z <- matrix(rnorm(n = N * M), ncol = M);
  Chi2 <- Z * Z;
  Chi2 <- apply(X = Chi2, MARGIN = 1, FUN = sum);
  return(Chi2);
}

N      <- 1e5;
M_1    <- 40;
M_2    <- 20;
A <- r_chi2(N = N, M = M_1) / M_1;
B <- r_chi2(N = N, M = M_2) / M_2;
F_a_b  <- A / B;
r_f    <- rf(n = N, df1 = M_1, df2 = M_2);

df <- 
  data.frame(X = F_a_b, Type = "F-from-Chi2") %>%
  bind_rows(data.frame(X = r_f, Type = "F-from-R"));

df %>%
  ggplot(aes(x = X, fill = Type)) + 
  geom_histogram() + 
  facet_grid(. ~ Type) + 
  labs(title = plot_title, caption = plot_caption, x = "x", y = "#",
       subtitle = sprintf("Comparison of F(%s, %s) from Chi^2 and R-builtin", M_1, M_2)) + 
  theme_light();