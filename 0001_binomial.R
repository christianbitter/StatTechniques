#
#
# description:
#
# the bernoulli trial allows us to ask about the probability of some event with p(success) = l
# happening. Trials are independent. 
#
# the binomial distribution is a generalization of the bernoulli trial in that it asks
# what is the probability of m successes out of n = (n - m) + m trials
# where success rate is p(success) = l and p(not success) = 1 - l
#
# example is from statistical rethinking
#
# estimatating the water surface by throwing an earth-like sphere into the air
#
rm(list = ls());
set.seed(12345);

library(ggplot2);

#probability of water vs. earth - no preference since we do not know
assumed_prob     <- 0.5; 
#we threw the sphere into the air and obtained the following sequence
#each throw is an individual bernoulli trial where earth or water may come up with p and 1-p
watched_sequence <- c("W", "L", "W", "W", "W", "L", "W", "L", "W");
n       <- length(watched_sequence); 
w_count <- sum(watched_sequence == "W");

#now what is the probability of observing this sequence of w_count water visits
#if we assume water and earth to be distributed equally
p_event <- dbinom(w_count, n, prob = assumed_prob);

#the probability of the event happening under our model is 0.164

#let us investigate which prior-probability may make our observed data more credible
grid <- seq(from = 0, to = 1, by = 0.01);
vp_event <- dbinom(w_count, n, prob = grid);

data.df <- data.frame( prior = grid, credibility = vp_event);

ggplot(data.df, aes(x = prior, y = credibility)) + 
  geom_point(colour="blue") + 
  geom_line(colour = "gray") + 
  labs(title = "Which prior probability of water makes the observed sequence most likely") + 
  geom_vline(xintercept = data.df$prior[which.max(data.df$credibilit)], colour="red");

