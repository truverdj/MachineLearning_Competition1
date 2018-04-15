library(MASS)
library(dplyr)
library(stringr)
mae = function(y, y_pred){
  mean(abs(y-y_pred))
}
pml.train = read.csv("pml_train.csv")
catsAll = unlist(lapply(pml.train[,2:117], function(x){length(levels(x))})) %>% unname()
y = pml.train$loss
set.seed(561)
n = nrow(pml.train)
n_folds = 10
indices = 1:nrow(pml.train)
fold = split(indices, sample(1:n_folds, n, replace = TRUE))
C = seq(0.05,1,by = 0.05)
MAEmatrix = NULL
for (i in 1:n_folds){
  training = pml.train[-fold[[i]],]
  testing = pml.train[fold[[i]],]
  catsToGo = unlist(lapply(training[,2:117], function(x){length(unique(x))})) %>% unname()
  catsRemove = 1 + which((catsToGo != catsAll)==TRUE)
  if (length(catsRemove) == 0){
    training = training
    testing = testing
  } else {
    training = training[,-catsRemove]
    testing = testing[,-catsRemove]
  }
  ridge = lm.ridge(log(loss)~.-id, data = training, lambda = C)
  testing.matrix = model.matrix(log(loss)~.-id, testing)
  pred = testing.matrix %*% t(coef(ridge))
  maes = apply(pred, 2, function(x){mae(testing$loss, exp(x))})
  MAEmatrix = rbind(MAEmatrix, maes)
}
# training = pml.train[-fold[[1]],]
# testing = pml.train[fold[[1]],]
# catsToGo = unlist(lapply(training[,2:117], function(x){length(unique(x))})) %>% unname()
# catsRemove = 1 + which((catsToGo != catsAll)==TRUE)
# training = training[,-catsRemove]
# testing = testing[,-catsRemove]
# ridge = lm.ridge(log(loss)~.-id, data = training, lambda = seq(0.05,1,by = 0.05))
# testing.matrix = model.matrix(log(loss)~.-id, testing)
# pred = testing.matrix %*% t(coef(ridge))
# maes = apply(pred, 2, function(x){mae(testing$loss, exp(x))})