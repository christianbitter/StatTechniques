#
# description:
#
# sampling from a grid-approximate posterior
# follows the same example like 0001 
#
# we use grid approximation, i.e. a walk through the parameter grid
# to construct samples which realize our data
#
# by summarizing the distribution of the samples, we obtain an answer as to what
# is the parameter configuration best describing our samples
#

rm(list = ls());

smode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

library(ggplot2);

#construct the parameter grid
sample.size <- 1e5;
grid.res    <- 1e4;
grid <- seq(from = 0, to = 1, length.out = grid.res);
#plot(grid);

#from the unknown model we drew the following sample
observed_sample <- c("W", "L", "W", "W", "W", "L", "W", "L", "W");

n <- length(observed_sample);
m <- sum( observed_sample == "W" );

#the prior of the data is the uninformative, non-constraining prior, i.e. each possible
#parameter configuration has the same prior probability
prior <- rep(1.0, grid.res);
#the likelihood of the data is ... p(data|p)
likelihood <- dbinom(x = m, size = n, prob = grid);

#now compute the posterior
posterior <- likelihood * prior;
#scale
posterior <- posterior / sum(posterior);
#let us sample with posterior probablity from the parameter space
#so the posterior dictates what parameter may be rather likely given the data
#so that we can then take sample.size samples from the parameter space
posterior.sample <- sample(x = grid, size = sample.size, replace = TRUE, prob = posterior);

data.df <- data.frame(posterior = posterior.sample, i = seq(sample.size));

ggplot(data.df, aes(x = i, y = posterior)) + 
  geom_point(alpha = 1/50) + 
  labs(title = "Random sample of parameters using probabilty of posterior distribution", 
       x = "sample #", y = "parameter probability");
#we can observe that the plot gets a lot darker in the upper horizontal region - around 0.65

ggplot(data.df, aes(x=posterior)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=0.01,
                 colour="black", fill = "gray") +
  geom_density(alpha=.4, colour="blue")  # Overlay with transparent density plot


#Plot CDI - credible density intervals
ggplot(data.df, aes(x=posterior.sample)) + geom_histogram(binwidth = 1/500);
ggplot(data.df, aes(x=posterior.sample, fill=(posterior.sample <= 0.8))) + 
         geom_histogram(binwidth = 1/500);


#the 90% credible interval
q90 <- as.numeric(quantile(posterior.sample, c(0.1, 0.9)));
ggplot(data.df, aes(x=posterior.sample, fill=(posterior.sample >= q90[1] & posterior.sample <= q90[2]))) + 
  geom_histogram(binwidth = 1/500) + 
  guides(fill = FALSE);


#now a point estimate
med <- median(posterior.sample);
avg <- mean(posterior.sample);
mod <- smode(posterior.sample);

#here we add a point estimate and the cdi to the plot
ggplot(data.df, 
       aes(x=posterior.sample, alpha = 0.2, fill=(posterior.sample >= q90[1] & posterior.sample <= q90[2]))) + 
  geom_histogram(binwidth = 1/500) +
  geom_vline(xintercept = med, colour = "red") + 
  geom_vline(xintercept = avg, colour=  "blue") + 
  geom_vline(xintercept = mod, colour= "black") + 
  scale_colour_discrete(breaks = c("median", "mean", "mode")) + 
  guides(fill = FALSE, alpha = FALSE, colour=guide_legend());

#picking a particular point estimate is associated with choosing a certain
#loss criterium - or the loss function which minimizes the respective loss criterium
#, such as mean absolute loss when picking the median
#or square average loss when picking 

#now when choosing the point estimate we can evaluate it against the prior under the loss function
#so what is the loss - it is the difference in our prediction against the true parameter w.r.t. 
#chosen loss function
plot(posterior)
sum(posterior * abs(0.5 - grid))