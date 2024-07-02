#### Q1 ####

# PART A
set.seed(102)
X = rnorm(100)
noise = rnorm(100)

# PART B
# Mention how you can break lines to make it cleaner
y = 1 + X * 3  + X^2 * (-2) + 
  X^3 * -1 + noise

# PART C
library(leaps)

help(regsubsets)

data = data.frame(y)
# Show as shortcut
data = cbind(data, outer(X, 1:10, FUN = "^"))
best_subset_res = regsubsets(y ~ ., data = data, nvmax = 10)
# Explain why we need the summary for it
best_subset_res = summary(best_subset_res)
par(mfrow = c(2,2))

plot(1:10, best_subset_res$rss, main = "RSS", type = "l", lwd = 2)
plot(1:10, best_subset_res$cp, main = "Cp", type = "l", lwd = 2)
plot(1:10, best_subset_res$bic, main = "BIC", type = "l", lwd = 2)
plot(1:10, best_subset_res$adjr2, main = "Adj R2", type = "l", lwd = 2)

# PART D
forward_subset_res = regsubsets(y ~ ., data = data, nvmax = 10, method = "forward")
# Explain why we need the summary for it
forward_subset_res = summary(forward_subset_res)
par(mfrow = c(2,2))

plot(1:10, forward_subset_res$rss, main = "RSS", type = "l", lwd = 2)
plot(1:10, forward_subset_res$cp, main = "Cp", type = "l", lwd = 2)
plot(1:10, forward_subset_res$bic, main = "BIC", type = "l", lwd = 2)
plot(1:10, forward_subset_res$adjr2, main = "Adj R2", type = "l", lwd = 2)


backward_subset_res = regsubsets(y ~ ., data = data, nvmax = 10, method = "backward")
# Explain why we need the summary for it
backward_subset_res = summary(backward_subset_res)
par(mfrow = c(2,2))

plot(1:10, backward_subset_res$rss, main = "RSS", type = "l", lwd = 2)
plot(1:10, backward_subset_res$cp, main = "Cp", type = "l", lwd = 2)
plot(1:10, backward_subset_res$bic, main = "BIC", type = "l", lwd = 2)
plot(1:10, backward_subset_res$adjr2, main = "Adj R2", type = "l", lwd = 2)

# PART E
data["y"] = 1 + 2 * x^7 + noise
new_model = summary(regsubsets(y ~ ., data = data, nvmax = 10))
par(mfrow = c(2,2))
plot(1:10, new_model$rss, main = "RSS", type = "l", lwd = 2)
plot(1:10, new_model$cp, main = "Cp", type = "l", lwd = 2)
plot(1:10, new_model$bic, main = "BIC", type = "l", lwd = 2)
plot(1:10, new_model$adjr2, main = "Adj R2", type = "l", lwd = 2)


#### Q2 #########################################################

# PART A
library(ISLR2)
data = Carseats
fit1 = lm(Sales ~ Price + Urban + US, data = data)
summary(fit1)

# If I want to change the base case
summary(slm(Sales ~ Price + Urban + relevel(data$US, ref = "Yes"), data = data))

# PART B
# Just interpretation xd

# Part C
# on board

# Part D
# Read summary()

# Part E
fit2 = lm(Sales ~ Price + US, data = data)
summary(fit2)


# Part F
BIC(fit1)
BIC(fit2)
# Or use AIC or adj R^2 from summary()

# Part G
confint(fit1)
confint(fit2)
# Clarify what it means by "something that inherits an LM object"

# Part H
par(mfrow = c(2,2))
plot(fit2)
# Note that high leverage would be 2(p + 1)/n
# Therefore anything above 0.015


######## Q3 ##################################################

# PART A
x1 <- runif(100)
x2 <- 0.5 * x1 + rnorm(100) / 10
y <- 2 + 2 * x1 + 0.3 * x2 + rnorm(100)

# y = 2 + 2.15 * x1 + noise/10 + noise2

# PART B
plot(x1, x2)
cor(x1, x2)

# PART C
summary(lm(y ~ x1 + x2))

# PART D
summary(lm(y ~ x1))

# PART E
summary(lm(y ~ x2))

# PART F
# Not really, the two variables confuse each other and made it hard
# to estimate the parameter values

# PART G
x1 <- c(x1, 0.1)
x2 <- c(x2, 0.8)
y <- c(y, 6)

# TO SEE IT HAS RESIDUALS
plot(x1,x2)
par(mfrow = c(2,2))
summary(lm(y ~ x1 + x2))
plot(lm(y ~ x1 + x2))

plot(x1,y)
summary(lm(y ~ x1))
plot(lm(y ~ x1))

plot(x2,y)
summary(lm(y ~ x2))
plot(lm(y ~ x2))
