#auth: christian bitter
#desc: experiments around the symetric random walk

rm(list = ls());

library(dplyr);
library(ggplot2);
library(latex2exp);

set.seed(1234);

source("r/common.R");

N   <- 100;
T_i <- sample(x = c(-1, 1), size = N, replace = T);
S_n <- c(0, cumsum(T_i));

df  <- 
  data.frame("i" = 0:N, "S_n" = S_n) %>%
  dplyr::mutate(marks_zero = S_n == 0);

plot_title   <- "Statistical Computing";
plot_caption <- "(c) Christian Bitter 2020"

df %>%
  ggplot(aes(x = i, y = S_n)) + 
  geom_point(aes(colour = marks_zero), show.legend = FALSE) + 
  geom_line(aes(x = i, y = S_n), colour = "gray", linetype = 4)  + 
  labs(x = "t", y = latex2exp::TeX("$S_n$"),
       title = plot_title, caption = plot_caption,
       subtitle = "Simulating a symetric random walk. Points in time where the walk returned to 0 are marked with blue") + 
  scale_colour_discrete(breaks = c(1, -1)) + 
  guides(color = element_blank()) +
  theme_light();