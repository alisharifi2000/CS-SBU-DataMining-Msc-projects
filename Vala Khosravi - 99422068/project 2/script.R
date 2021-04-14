library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(GGally)
library(e1071)
library(tidyverse)
library(caret)

setwd("~/Documents/Education/Master/Data\ mining/exercises/norooz/")
df <- as.data.frame(read.csv("heart.csv"))
boxplot(df)
findOutliers <- function(dataframe){
  dataframe %>%
    select_if(is.numeric) %>% 
    map(~ boxplot.stats(.x)$out)
}
outliers <- findOutliers(df)
temp <- list()
for (col in names(outliers)) {
  outlier <- outliers[[col]]
  if (length(outlier) > 0) {
    temp[col] <- df[-which(df[[col]] %in% outlier),][col]
  } else {
    temp[col] <- df[col]
  }
}
boxplot(temp)
df$target = as.factor(df$target)
ggpairs(df, aes(colour = target, alpha = 0.4))

train_ind <- sample(nrow(df), 242, replace = FALSE, prob = NULL)
train <- df[train_ind, ]
test <- df[-train_ind, ]

gaussian_naive_bayes <- function (x, y, prior = NULL, ...) {
  if (!is.factor(y) & !is.character(y) & !is.logical(y)) 
    stop("gaussian_naive_bayes(): y must be either a factor or character or logical vector", 
         call. = FALSE)
  if (!is.factor(y)) 
    y <- factor(y)
  levels <- levels(y)
  nlev <- nlevels(y)
  vars <- colnames(x)
  class_x <- class(x)[1]
  use_Matrix <- class_x %in% .matrix_classes
  if (!is.matrix(x) & !use_Matrix) {
    warning("gaussian_naive_bayes(): x was coerced to matrix.", 
            call. = FALSE)
    x <- as.matrix(x)
    if (mode(x) != "numeric") 
      stop("gaussian_naive_bayes(): x must be a matrix/dgCMatrix with with numeric columns.", 
           call. = FALSE)
  }
  if (use_Matrix) {
    if (!"Matrix" %in% rownames(utils::installed.packages())) 
      stop("gaussian_naive_bayes(): please install \"Matrix\" package.")
    if (class_x != "dgCMatrix") 
      stop("gaussian_naive_bayes(): dgCMatrix class from the Matrix package is only supported.", 
           call. = FALSE)
  }
  if (nlev < 2) 
    stop("gaussian_naive_bayes(): y must contain at least two classes. ", 
         call. = FALSE)
  if (is.null(vars)) 
    stop("gaussian_naive_bayes(): x must have unique column names.\n", 
         call. = FALSE)
  NAy <- anyNA(y)
  NAx <- anyNA(x)
  if (NAy) {
    na_y_bool <- is.na(y)
    len_na <- sum(na_y_bool)
    warning(paste0("gaussian_naive_bayes(): y contains ", 
                   len_na, " missing", ifelse(len_na == 1, " value", 
                                              " values"), ". ", ifelse(len_na == 1, "It is", 
                                                                       "They are"), " not included (together with the corresponding instances in x) ", 
                   " into the estimation process."), call. = FALSE)
    y <- y[!na_y_bool]
    x <- x[!na_y_bool, ]
  }
  if (NAx) {
    na_x <- is.na(x) * 1
    len_nax <- sum(na_x)
    warning(paste0("gaussian_naive_bayes(): x contains ", 
                   len_nax, " missing", ifelse(len_nax == 1, " value", 
                                               " values"), ". ", ifelse(len_nax == 1, "It is", 
                                                                        "They are"), " not included (also the corresponding rows in x) ", 
                   "into the estimation process."), call. = FALSE)
  }
  y_counts <- stats::setNames(tabulate(y), levels)
  y_min <- y_counts < 2
  if (any(y_min)) 
    stop(paste0("gaussian_naive_bayes(): y variable has to contain at least ", 
                "two observations per class for estimation process.", 
                " Class ", paste0(levels[y_min], collapse = ", "), 
                " has less than 2 observations."), call. = FALSE)
  if (is.null(prior)) {
    prior <- prop.table(y_counts)
  }
  else {
    if (length(prior) != nlev) 
      stop(paste0("gaussian_naive_bayes(): Vector with prior probabilities should have ", 
                  nlev, " entries"))
    prior <- stats::setNames(prior/sum(prior), levels)
  }
  if (!NAx) {
    if (use_Matrix) {
      params <- do.call("rbind", lapply(levels, function(lev) {
        lev_subset <- x[y == lev, , drop = FALSE]
        mu <- Matrix::colMeans(lev_subset, na.rm = TRUE)
        sd <- sqrt((Matrix::colSums(lev_subset^2, na.rm = TRUE) - 
                      mu^2 * y_counts[lev])/(y_counts[lev] - 1))
        rbind(mu, sd)
      }))
      mu <- params[rownames(params) == "mu", ]
      rownames(mu) <- levels
      sd <- params[rownames(params) == "sd", ]
      rownames(sd) <- levels
    }
    else {
      mu <- rowsum(x, y, na.rm = TRUE)/y_counts
      sd <- sqrt((rowsum(x^2, y, na.rm = TRUE) - mu^2 * 
                    y_counts)/(y_counts - 1))
    }
  }
  else {
    n <- if (use_Matrix) {
      na_per_feature <- lapply(levels, function(lev) {
        Matrix::colSums(na_x[y == lev, , drop = FALSE], 
                        na.rm = TRUE)
      })
      n_feature_obs <- y_counts - do.call("rbind", na_per_feature)
      rownames(n_feature_obs) <- levels
      n_feature_obs
    }
    else {
      y_counts - rowsum.default(na_x, y)
    }
    if (any(n < 2)) 
      warning("gaussian_naive_bayes(): infinite variances (NaN) are present, ", 
              "in each case due to less than two observations after removing missing values.", 
              call. = FALSE)
    if (use_Matrix) {
      params <- do.call("rbind", lapply(levels, function(lev) {
        lev_subset <- x[y == lev, , drop = FALSE]
        mu <- Matrix::colMeans(lev_subset, na.rm = TRUE)
        nlev <- n[rownames(n) == lev]
        sd <- sqrt((Matrix::colSums(lev_subset^2, na.rm = TRUE) - 
                      mu^2 * nlev)/(nlev - 1))
        rbind(mu, sd)
      }))
      mu <- params[rownames(params) == "mu", ]
      rownames(mu) <- levels
      sd <- params[rownames(params) == "sd", ]
      rownames(sd) <- levels
    }
    else {
      mu <- rowsum(x, y, na.rm = TRUE)/n
      sd <- sqrt((rowsum(x^2, y, na.rm = TRUE) - mu^2 * 
                    n)/(n - 1))
    }
  }
  structure(list(data = list(x = x, y = y), levels = levels, 
                 params = list(mu = mu, sd = sd), prior = prior, call = match.call()), 
            class = "gaussian_naive_bayes")
}

df$thalach <- as.factor(df$thalach)
df$chol <- as.factor(df$chol)

ggpairs(df, columns = c("thalach", "trestbps", "chol"), aes(colour = target, alpha = 0.4))
model <- naive_bayes(target ~ thalach + trestbps + chol, data = train)
model
par(mfrow=c(3,1))
plot(model)
par(mfrow=c(1,1))
pred <- predict(model, test)
ctable <- as.data.frame(table(pred, test$target))
tn <- ctable$Freq[1]
fn <- ctable$Freq[2]
fp <- ctable$Freq[3]
tp <- ctable$Freq[4]
precision <- tp / (tp+fp)
print(c('precision', precision))
recall <- tp / (tp+fn)
print(c('recall', recall))
f1_score <- 2 * (precision * recall) / (precision + recall)
print(c('f1_score', f1_score))

temp <- train[,c("target","thalach","trestbps", "chol")]
temp$target <- as.factor(temp$target)
smv_model <- svm(target ~ thalach + trestbps + chol, data = temp, kernel = "linear")
pred <- predict(smv_model, test)
ctable <- as.data.frame(table(pred, test$target))
tn <- ctable$Freq[1]
fn <- ctable$Freq[2]
fp <- ctable$Freq[3]
tp <- ctable$Freq[4]
precision <- tp / (tp+fp)
print(c('precision', precision))
recall <- tp / (tp+fn)
print(c('recall', recall))
f1_score <- 2 * (precision * recall) / (precision + recall)
print(c('f1_score', f1_score))

smv_model <- svm(target ~ thalach + trestbps + chol, data = temp, kernel = "polynomial")
pred <- predict(smv_model, test)
ctable <- as.data.frame(table(pred, test$target))
tn <- ctable$Freq[1]
fn <- ctable$Freq[2]
fp <- ctable$Freq[3]
tp <- ctable$Freq[4]
precision <- tp / (tp+fp)
print(c('precision', precision))
recall <- tp / (tp+fn)
print(c('recall', recall))
f1_score <- 2 * (precision * recall) / (precision + recall)
print(c('f1_score', f1_score))

smv_model <- svm(target ~ ., data = train, kernel = "linear")
pred <- predict(smv_model, test)
ctable <- as.data.frame(table(pred, test$target))
tn <- ctable$Freq[1]
fn <- ctable$Freq[2]
fp <- ctable$Freq[3]
tp <- ctable$Freq[4]
precision <- tp / (tp+fp)
print(c('precision', precision))
recall <- tp / (tp+fn)
print(c('recall', recall))
f1_score <- 2 * (precision * recall) / (precision + recall)
print(c('f1_score', f1_score))

set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model <- train(Fertility ~., data = swiss, method = "lm",
               trControl = train.control)

