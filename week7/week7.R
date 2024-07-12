########################## Q1 ##################################
set.seed(1)
data = ISLR2::Default

### PART A

fit = glm(default ~ income + balance, family = binomial, data = data)

### PART B
train = sample(1:nrow(data), nrow(data) * 0.8)
train = 1:nrow(data) %in% train

val = data[!train, ]
train = data[train, ]

fit.train = glm(default ~ balance + income, data = train, family = binomial)

help("predict.glm")

pred = ifelse(predict(fit.train, newdata = val, type = "response") > 0.4,
              "Yes",
              "No")

table(true = val$default, pred = pred)
mean(pred == val$default)
mean("No" == val$default)


### PART C

for (i in 1:4) {
  set.seed(i)
  train = sample(1:nrow(data), nrow(data) * 0.8)
  train = 1:nrow(data) %in% train
  
  val = data[!train, ]
  train = data[train, ]
  
  
  fit.train = glm(default ~ balance + income, data = train, family = binomial)
  
  help("predict.glm")
  
  pred = ifelse(predict(fit.train, newdata = val, type = "response") > 0.4,
                "Yes",
                "No")
  
  table(true = val$default, pred = pred)
  print(mean(pred == val$default))
}



##################### Q3 #####################################
library(ggplot2)
library(boot)

### PART A
set.seed(1)
x <- rnorm(100)
y <- x - 2 * x^2 + rnorm(100)

### PART B

data = data.frame(y = y)
data = cbind(data, outer(x, 1:4, FUN = "^"))
colnames(data) = c("y", "x1", "x2", "x3", "x4")

ggplot(data) + geom_point(aes(x = x1, y = y), color  ="blue") +
  theme_classic() +
  labs(title = "Plot of X vs. Y", x = "X values") +
  theme(plot.title= element_text(colour = "red", hjust = 0.5, face = "bold", size = 30))

### PART C
glm1 = glm(y ~ x1, data = data)
cv1 = cv.glm(data, glm1)

glm2 = glm(y ~ x1 + x2, data = data)
cv2 = cv.glm(data, glm2)

glm3 = glm(y ~ x1 + x2 + x3, data = data)
cv3 = cv.glm(data, glm3)

print(cv1$delta)
print(cv2$delta)
print(cv3$delta)

############## Q4 ########################################
data = ISLR2::College

### PART A
train = sample(1:nrow(data), nrow(data) / 2)
train = 1:nrow(data) %in% train

### PART B
fit = lm(Apps ~ ., data = data, subset = train)
pred = predict(fit, newdata = data[!train, ], type = "response")
mean((pred - data$Apps[!train])^2)
mean(fit$residuals^2)


### PART C
library(glmnet)

lambdas = 10^seq(10, -2, length.out = 100)
modelmatrix = model.matrix(Apps ~ ., data = data)[, -1]
fit = glmnet(modelmatrix[train, ], data$Apps[train], alpha = 0, lambda = lambdas)
cv.fit = cv.glmnet(modelmatrix[train, ], data$Apps[train], alpha = 0, lambda = lambdas)
plot(cv.fit)

### PART D
fit2 = glmnet(modelmatrix[train, ], data$Apps[train], alpha = 1, lambda = lambdas)
cv.fit2 = cv.glmnet(modelmatrix[train, ], data$Apps[train], alpha = 1, lambda = lambdas)
cv.fit2$lambda.min

coef(fit2, s= cv.fit2$lambda.min)
coef(fit2, s= 100)
