# Train a decision tree classifier on the iris data set 
# and explore it's decision boundary graphically
# what we can nicely see is the orthogonality of the decision tree splits
#


rm(list = ls());

library(ggplot2);
library(dplyr);
library(rpart);

species_to_colour <- function(species, cols = c("salmon", "lightgreen", "lightblue")){
  return(
    ifelse(species == "setosa", cols[1],
           ifelse(species == "virginica", cols[2], cols[3]))
  );
}


data_df <- iris;

#pairs(data_df)


#petal length and petal width provide nice separation
N         <- length(data_df$Species);
train_pct <- .7;
train_idx <- sample(1:N, size = train_pct * N);

train_df <- data_df[train_idx, ];
test_df  <- data_df[-train_idx, ];
tree     <- rpart(formula = Species ~ Petal.Length + Petal.Width, data = train_df);
species  <- levels(data_df$Species);

pl <- seq(0, 10, .1);
pw <- seq(0, 3, .01);
p_grid <- expand.grid(pl, pw)
p_grid_df <- data.frame(p_grid);
names(p_grid_df) <- c("Petal.Length", "Petal.Width");

yhat_train <- predict(tree, train_df);
yhat_test  <- predict(tree, test_df);
yhat_grid  <- predict(tree, p_grid_df)

yhat_train <- species[apply(yhat_train, MARGIN = 1, which.max)]
yhat_test  <- species[apply(yhat_test, MARGIN = 1, which.max)]
yhat_grid  <- species[apply(yhat_grid, MARGIN = 1, which.max)]


p_grid_df$Species <- yhat_grid;

p_grid_df$Color <- species_to_colour(p_grid_df$Species);
data_df$Color   <- species_to_colour(data_df$Species, cols = c("darkred", "darkgreen", "darkblue"));

plot(p_grid_df$Petal.Length, p_grid_df$Petal.Width, col = alpha(p_grid_df$Color, .2),
     main = "Decision Boundary of Iris Classifier", 
     xlab = "Petal Length (inch)", 
     ylab = "Petal Width (inch)");
points(data_df$Petal.Length, data_df$Petal.Width, col = data_df$Color, pch = 1);