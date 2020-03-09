rm(list = ls());

library(ggplot2);


lambda <- 2;
n      <- 20;
p_i    <- rep(0, n);
p_1    <- rep(0, n);
p_2    <- rep(0, n);

for(i in 1:n){
  p_1[i] <- dpois(x = i - 1, lambda = lambda);
  p_2[i] <- dpois(x = i, lambda = lambda);
}

data_df <- data.frame( i = 0:(n - 1), j = 1:n, p_1 = p_1, p_2 = p_2, 
                       p_ratio = p_1/p_2, p_diff = p_2 - p_1);

#the difference is 1/lambda
#which makes sense since it is the inverse unit production rate of the process
#through the exponentiation we get the linear behaviour

ggplot(data_df, aes(x = i)) + 
  geom_point(aes(y = p_1), colour="gray") +
  geom_point(aes(y = p_2), colour="blue") +
  geom_point(aes(y = p_ratio), colour = "black");

ggplot(data_df, aes(x = i)) + 
  geom_point(aes(y = p_1), colour="black") +
  geom_point(aes(y = p_2), colour="blue") + 
  theme_light();
  
ggplot(data_df, aes(x = i)) + 
  geom_point(aes(y = p_ratio), colour="gray") +
  geom_point(aes(y = p_diff), colour="blue");
  
