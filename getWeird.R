library(dplyr)
library(stringr)
mae = function(y, y_pred){
  mean(abs(y-y_pred))
}
pml.train = read.csv("pml_train.csv")
y = pml.train$loss
num_quants = 2.1
quants = data.frame(lwr = rep(NA, num_quants), upr = rep(NA, num_quants))
pred_mean = rep(NA, num_quants)
pred_med = rep(NA, num_quants)
if(num_quants == 10){
  up = seq(0.1,1, length.out = num_quants)
  lo = up - 0.1
} else if(num_quants == 11) {
  up = c(seq(0.1,0.9, length.out = 9),0.99,1)
  lo = c(seq(0,0.9, length.out = 10),0.99)
} else if(num_quants == 4) {
  up = seq(.25,1,length.out = 4)
  lo = seq(0,.75,length.out = 4)
} else if(num_quants == 5){
  up = c(0.25,0.50,0.75,0.95,1)
  lo = c(0,0.25,0.50,0.75,0.95)
} else if(num_quants == 6){
  up = c(0.25,0.50,0.75,0.95,0.99,1)
  lo = c(0,0.25,0.50,0.75,0.95,0.99)
} else if(num_quants == 8){
  up = c(0.25, 0.55, 0.73, 0.82, 0.885, 0.925, 0.95, 0.97, 1)
  lo = c(0, 0.25, 0.55, 0.73, 0.82, 0.885, 0.925, 0.95, 0.97)
} else if(num_quants == 7){
  up = c(0.25,0.50,0.75,0.90,0.95,0.99,1)
  lo = c(0,0.25,0.50,0.75,0.90,0.95,0.99)
} else if (num_quants == 3){
  up = c(0.25,0.75,0.95,1)
  lo = c(0,0.25,0.75,0.95)
} else if (num_quants == 2){
  up = c(0.50,1)
  lo = c(0,0.50)
} else if (num_quants == 2.1){
  up = c(0.33,0.66,1)
  lo = c(0,0.33,0.66)
}
quants = data.frame(lwr = rep(NA, length(up)), upr = rep(NA, length(up)))
pred_mean = rep(NA, length(up))
pred_med = rep(NA, length(up))
for (i in seq_along(up)){
  quants[i,] = quantile(y, c(lo[i], up[i]))
  pml.train[paste0("quant", i)] = 1*(quants$lwr[i] <= y & y < quants$upr[i])
  pred_mean[i] = mean(pml.train$loss[pml.train[paste0("quant", i)] == 1])
  pred_med[i] = median(pml.train$loss[pml.train[paste0("quant", i)] == 1])
}
set.seed(2018)
rootp = floor(sqrt(131))
vars1 = c( sample(names(pml.train)[c(2:131)], rootp), "quant1")
formulas = lapply(paste0("quant",1:length(up),"~."), formula)
n = nrow(pml.train)
n_folds = 10
indices = 1:nrow(pml.train)
fold = split(indices, sample(1:n_folds, n, replace = TRUE))
bags = 100
preds = matrix(NA, nrow = length(fold[[1]]), ncol = bags)
for (j in 1:bags){
  training = pml.train[-fold[[1]],] 
  train_index = sample(1:nrow(training), nrow(training), replace = TRUE)
  training = training[train_index,]
  testing = pml.train[fold[[1]],]
  voting = matrix(NA, nrow = nrow(testing), ncol = nrow(quants))
  vars1 = c( sample(names(pml.train)[c(1:131)], rootp), "quant1")
  for (q in 1:nrow(quants)){
    glm1 = glm(formulas[[q]], data = training[, vars1], family = binomial)
    for (var in vars1[str_detect(vars1, "cat")]){
      glm1$xlevels[[var]] = union(levels(testing[, vars1][[var]]), 
                                  glm1$xlevels[[var]])
    }
    voting[,q] = predict.glm(glm1, newdata = testing[, vars1], type = "response")
    vars1[rootp + 1] = paste0("quant", q+1)
  }
  preds[,j] = apply(voting, 1, which.max)
}
after_vote_preds = apply(preds, 1, function(x){as.integer(names(which.max(table(x))))})
value_preds_mean = pred_mean[after_vote_preds]
value_preds_med = pred_med[after_vote_preds]
y_true = pml.train[fold[[1]], "loss"]
mean_mae = mae(y = y_true, y_pred = value_preds_mean)
med_mae = mae(y = y_true, y_pred = value_preds_med)
# 5 quants: mean_mae = 1782.928; med_mae = 1778.672
# 6 quants: mean_mae = 1774.36; med_mae = 1769.887
# 8 quants: mean_mae = 1864.11; med_mae = 1869.394
# 7 quants: mean_mae = 1874.184; med_mae = 1870.166
# 3 quants: mean_mae = 1862.953; med_mae = 1858.794
# 4 quants: mean_mae = 1936.599; med_mae = 1783.398
#10 quants: mean_mae = 3125.001; med_mae = 2741.118
# 2 quants: mean_mae = 1617.419; med_mae = 1616.23
#11 quants: mean_mae = 2206.6; med_mae = 2114.743
#2.1 quants: mean_mae = 1808.455; med_mae = 1689.38
# glm1 = glm(quant1~., data = pml.train1, family = binomial)
# unionLevels(glmi = glm1)
# pred1 = predict.glm(glm1, newdata = pml.train, type = "response")
# unionLevels = function(glmi, test){
#   for (var in vars1[str_detect(vars1, "cat")]){
#     glmi$xlevels[[var]] = union(levels(test[[var]]), glmi$xlevels[[var]])
#   }
# }
voting = matrix(NA, nrow = length(fold[[1]]), ncol = nrow(quants))
vars1 = c( names(pml.train)[c(2:131)], "quant1")
for (q in 1:nrow(quants)){
  tree1 = tree(formulas[[q]], data = pml.train[-fold[[1]], vars1], family = binomial)
  for (var in vars1[str_detect(vars1, "cat")]){
    glm1$xlevels[[var]] = union(levels(pml.train[fold[[1]], vars1][[var]]), 
                                glm1$xlevels[[var]])
  }
  voting[,q] = predict.glm(glm1, newdata = pml.train[fold[[1]], vars1], type = "response")
  vars1[132] = paste0("quant", q+1)
}
preds[,j] = apply(voting, 1, which.max)
after_vote_preds = apply(preds, 1, function(x){as.integer(names(which.max(table(x))))})
value_preds_mean = pred_mean[after_vote_preds]
value_preds_med = pred_med[after_vote_preds]
y_true = pml.train[fold[[1]], "loss"]
mean_mae = mae(y = y_true, y_pred = value_preds_mean)
med_mae = mae(y = y_true, y_pred = value_preds_med)