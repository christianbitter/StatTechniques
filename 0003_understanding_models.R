#
# predictive difference among variables
#
#

rm(list = ls());

library(ggplot2);
library(dplyr);
library(lattice);
library(plot3D);

data.df <- mtcars;
#head(data.df)

#predict miles per gallon using two factors
ggplot(data.df, aes(y = mpg, x = hp)) + geom_point() + stat_smooth(method = "lm");
ggplot(data.df, aes(y = mpg, x = wt)) + geom_point() + stat_smooth(method = "lm");


lm.1 <- lm(mpg ~ hp + wt, data.df);
print(summary(lm.1))

# Residuals:
#   Min     1Q Median     3Q    Max 
# -3.941 -1.600 -0.182  1.050  5.854 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 37.22727    1.59879  23.285  < 2e-16 ***
#   hp          -0.03177    0.00903  -3.519  0.00145 ** 
#   wt          -3.87783    0.63273  -6.129 1.12e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.593 on 29 degrees of freedom
# Multiple R-squared:  0.8268,	Adjusted R-squared:  0.8148 
# F-statistic: 69.21 on 2 and 29 DF,  p-value: 9.109e-12


#now we try to make sense of the model by using predictive differences 
#we are going to vary the hp between high and low and hold wt constant - at the mean
r.hp <- range(data.df$hp);
s.hp <- seq(r.hp[1], r.hp[2], (r.hp[2] - r.hp[1]) / 100);
r.wt <- range(data.df$wt);
s.wt <- seq(r.wt[1], r.wt[2], (r.wt[2] - r.wt[1]) / 100);

g <- expand.grid(s.hp, s.wt)

p.df <- data.frame(hp = g[,1], wt = g[,2]);
p.df$mpg <- predict(lm.1, p.df);

#the surface and wireframe clearly show that increasing horsepower is a dominant factor in consumption of gasoline
#weight also decreases mileage
ggplot(p.df, aes(x = hp, y=wt, fill = mpg)) + geom_tile()

wireframe(mpg ~ hp * wt, p.df, 
       main = "Gasoline Consumption [MpG] as a function of car weight and horsepower",
       drape = TRUE,
       colorkey = TRUE,
       screen = list(z = -60, x = -60));