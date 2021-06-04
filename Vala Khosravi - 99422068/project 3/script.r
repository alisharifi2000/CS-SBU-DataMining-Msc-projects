library(e1071)
library(kernlab)
library(ggplot2)
library(dplyr)
library(rbin)

setwd("~/Documents/Education/Master/data\ mining/exercises/3/")
train_df <- as.data.frame(read.csv("train.csv"))
test_df <- sample_n(train_df, 200) 
kernels <- c('linear', 'polynomial', 'radial', 'sigmoid')
train_df$price_range <- as.factor(train_df$price_range)
test_df$price_range <- as.factor(test_df$price_range)
plot(train_df$ram, train_df$battery_power, col = train_df$price_range)
# ggplot(train_df, aes(x=battery_power, y=ram, color=as.factor(price_range))) + geom_point()
for (kernel in kernels) {
  smv_model <- svm(price_range ~ . , data = train_df, kernel = kernel)
  pred <- predict(smv_model, test_df)
  plot(smv_model, test_df, battery_power~ram, main=kernel)
  title(main= paste("                                      ", kernel, sep=" "))
  print(c(kernel,table(pred)))
  error = 0
  for (i in (1: nrow(test_df))) {
    if (test_df[i,]$price_range != pred[i]) {
      error = error + 1
    }
  }
  print(c('error rate', error / nrow(test_df)))
}
smv_model <- svm(price_range ~ . , data = train_df, kernel = 'polynomial', degree=4)
pred <- predict(smv_model, test_df)
plot(smv_model, test_df, battery_power~ram, main=kernel)
title(main= paste("                                      ", kernel, sep=" "))
print(c(kernel,table(pred)))
error = 0
for (i in (1: nrow(test_df))) {
  if (test_df[i,]$price_range != pred[i]) {
    error = error + 1
  }
}
print(c('error rate', error / nrow(test_df)))

summary(train_df$battery_power)
range1 = seq(500, 1500, by = 100)
hist(train_df$battery_power, breaks = seq(min(train_df$battery_power), max(train_df$battery_power), length.out = 11))
hist(train_df$battery_power, breaks = seq(min(train_df$battery_power), max(train_df$battery_power), length.out = 111))
hist(train_df$battery_power, breaks = c(min(train_df$battery_power),600,640,670,1100,1250,1300, 1410, 1900,max(train_df$battery_power)))


library(caret)
train_df$price_range <- as.factor(train_df$price_range)
train_df$blue <- as.factor(train_df$blue)
train_df$touch_screen <- as.factor(train_df$touch_screen)
train_df$three_g <- as.factor(train_df$three_g)
dummy <- dummyVars(" ~ .", data=train_df)
newdata <- data.frame(predict(dummy, newdata = train_df)) 

