
source("/Users/Ana/Documents/Sola/Faks/2s/2l/DM/Assignment1/Assignment1.r")
# source("C:/Users/Iro Sfoungari/Documents/Data-Mining/Assignment1.r")

library("mlbench")

tp <- function(y_p, y_r) { 
  TP <- 0
  for ( i in 1:length(y_p) ) {
    if ( (y_p[i] == 1) && (y_r[i] == 1) ) {
      TP <- TP + 1
    }
  }
  return(TP)
}

tn <- function(y_p, y_r) { 
  TN <- 0
  for ( i in 1:length(y_p) ) {
    if ( (y_p[i] == 0) && (y_r[i] == 0) ) {
      TN <- TN + 1
    }
  }
  return(TN)
}

fp <- function(y_p, y_r) { 
  FP <- 0
  for ( i in 1:length(y_p) ) {
    if ( (y_p[i] == 1) && (y_r[i] == 0) ) {
      FP <- FP + 1
    }
  }
  return(FP)
}

fn <- function(y_p, y_r) { 
  FN <- 0
  for ( i in 1:length(y_p) ) {
    if ( (y_p[i] == 0) && (y_r[i] == 1) ) {
      FN <- FN + 1
    }
  }
  return(FN)
}

precision <- function(y_p, y_r) { # TP/(TP + FP)
  TP <- tp(y_p, y_r)
  FP <- fp(y_p, y_r)
  return(TP/(TP + FP))
}

recall <- function(y_p, y_r) { # TP/(TP + FN)
  TP <- tp(y_p, y_r)
  FN <- fn(y_p, y_r)
  return(TP/(TP + FN))
}

accurancy <- function(y_p, y_r) { # (TP + TN)/(TP + TP + FP + FN)
  TP <- tp(y_p, y_r)
  FN <- fn(y_p, y_r)
  TN <- tn(y_p, y_r)
  FP <- fp(y_p, y_r)
  return((TP + TN)/(TP + TP + FP + FN))
}

toZeroOne <- function(vec) { # rewrite with apply
  v <- c()
  for ( i in 1:length(vec) ) {
    if ( vec[i] > 0 ) {
      v <- c(v, 1)
    } else {
      v <- c(v, 0)
    }
  }
  return(v)
}

credit.dat <- read.csv("/Users/Ana/Documents/Sola/Faks/2s/2l/DM/Assignment1/Data/credit.txt")
# credit.dat <- read.csv("C:/Users/Iro Sfoungari/credit.txt")

credit.dat.x <- credit.dat[, 1:5]
credit.dat.y <- credit.dat[, 6]

credit.dat.single.tree <- tree.grow(credit.dat.x, credit.dat.y, 20, 1, 41)
credit.dat.single.prediciton.train <- tree.classify(credit.dat.x, credit.dat.single.tree)
credit.dat.single.accurancy.train <- accurancy(credit.dat.single.prediciton.train, credit.dat.y)

data(PimaIndiansDiabetes)
pima.x <- PimaIndiansDiabetes[, 1:8]
pima.y <- as.numeric(PimaIndiansDiabetes[, 9])
pima.y <- pima.y - rep(1, times = length(pima.y))

pima.single.tree <- tree.grow(pima.x, pima.y, 20, 5, 41)
pima.single.prediciton.train <- tree.classify(pima.x, pima.single.tree)
pima.single.accurancy.train <- accurancy(pima.single.prediciton.train, pima.y)

eclipse.2 <- read.csv(file=file.path("/Users/Ana/Documents/Sola/Faks/2s/2l/DM/Assignment1/Data", "eclipse-metrics-packages-2.0.csv"), sep = ";")
# eclipse.2 <- read.csv(file=file.path("C:/Users/Iro Sfoungari/DataForTheFirstAs", "eclipse-metrics-packages-2.0.csv"), sep = ";")

eclipse.2.x <- eclipse.2[, c(3, 5:44)]
eclipse.2.y <- toZeroOne(eclipse.2[, 4])

eclipse.3 <- read.csv(file=file.path("/Users/Ana/Documents/Sola/Faks/2s/2l/DM/Assignment1/Data", "eclipse-metrics-packages-3.0.csv"), sep = ";")
# eclipse.3 <- read.csv(file=file.path("C:/Users/Iro Sfoungari/DataForTheFirstAs", "eclipse-metrics-packages-3.0.csv"), sep = ";")

eclipse.3.x <- eclipse.3[, c(3, 5:44)]
eclipse.3.y <- toZeroOne(eclipse.3[, 4])


### SINGLE ###
print("single")
ptm <- proc.time()
eclipse.single.tree <- tree.grow(eclipse.2.x, eclipse.2.y, 15, 5, 41)
print(proc.time() - ptm)

eclipse.single.prediciton.train <- tree.classify(eclipse.2.x, eclipse.single.tree)
eclipse.single.precision.train <- precision(eclipse.single.prediciton.train, eclipse.2.y)
eclipse.single.recall.train <- recall(eclipse.single.prediciton.train, eclipse.2.y)
eclipse.single.accurancy.train <- accurancy(eclipse.single.prediciton.train, eclipse.2.y)

eclipse.single.prediciton.test <- tree.classify(eclipse.3.x, eclipse.single.tree)
eclipse.single.precision.test <- precision(eclipse.single.prediciton.test, eclipse.3.y)
eclipse.single.recall.test <- recall(eclipse.single.prediciton.test, eclipse.3.y)
eclipse.single.accurancy.test <- accurancy(eclipse.single.prediciton.test, eclipse.3.y)


### BAGGING ###
print("bagging")
eclipse.bagging.tree <- tree.grow.bag(eclipse.2.x, eclipse.2.y, 15, 5, 41, 100)

eclipse.bagging.prediciton.train <- tree.classify.bag(eclipse.bagging.tree, eclipse.2.x)
eclipse.bagging.precision.train <- precision(eclipse.bagging.prediciton.train, eclipse.2.y)
eclipse.bagging.recall.train <- recall(eclipse.bagging.prediciton.train, eclipse.2.y)
eclipse.bagging.accurancy.train <- accurancy(eclipse.bagging.prediciton.train, eclipse.2.y)

eclipse.bagging.prediciton.test <- tree.classify.bag(eclipse.bagging.tree, eclipse.3.x)
eclipse.bagging.precision.test <- precision(eclipse.bagging.prediciton.test, eclipse.3.y)
eclipse.bagging.recall.test <- recall(eclipse.bagging.prediciton.test, eclipse.3.y)
eclipse.bagging.accurancy.test <- accurancy(eclipse.bagging.prediciton.test, eclipse.3.y)


### RANDOM FOREST ###
print("random forest")
eclipse.random.tree <- tree.grow(eclipse.2.x, eclipse.2.y, 15, 5, 6)

eclipse.random.prediciton.train <- tree.classify(eclipse.2.x, eclipse.random.tree)
eclipse.random.precision.train <- precision(eclipse.random.prediciton.train, eclipse.2.y)
eclipse.random.recall.train <- recall(eclipse.random.prediciton.train, eclipse.2.y)
eclipse.random.accurancy.train <- accurancy(eclipse.random.prediciton.train, eclipse.2.y)

eclipse.random.prediciton.test <- tree.classify(eclipse.3.x, eclipse.random.tree)
eclipse.random.precision.test <- precision(eclipse.random.prediciton.test, eclipse.3.y)
eclipse.random.recall.test <- recall(eclipse.random.prediciton.test, eclipse.3.y)
eclipse.random.accurancy.test <- accurancy(eclipse.random.prediciton.test, eclipse.3.y)


### ANALYSE ###

TP_single <- tp(eclipse.single.prediciton.test, eclipse.3.y)
TN_single <- tn(eclipse.single.prediciton.test, eclipse.3.y)
FP_single <- fp(eclipse.single.prediciton.test, eclipse.3.y)
FN_single <- fn(eclipse.single.prediciton.test, eclipse.3.y)

TP_bagging <- tp(eclipse.bagging.prediciton.test, eclipse.3.y)
TN_bagging <- tn(eclipse.bagging.prediciton.test, eclipse.3.y)
FP_bagging <- fp(eclipse.bagging.prediciton.test, eclipse.3.y)
FN_bagging <- fn(eclipse.bagging.prediciton.test, eclipse.3.y)

TP_random <- tp(eclipse.random.prediciton.test, eclipse.3.y)
TN_random <- tn(eclipse.random.prediciton.test, eclipse.3.y)
FP_random <- fp(eclipse.random.prediciton.test, eclipse.3.y)
FN_random <- fn(eclipse.random.prediciton.test, eclipse.3.y)

m1 <- matrix(c(TP_single + TN_single, TP_random + TN_random, FP_single + FN_single, FP_random + FN_random), nrow = 2)
m2 <- matrix(c(TP_single + TN_single, TP_bagging + TN_bagging, FP_single + FN_single, FP_bagging + FN_bagging), nrow = 2)
m3 <- matrix(c(TP_bagging + TN_bagging, TP_random + TN_random, FP_bagging + FN_bagging, FP_random + FN_random), nrow = 2)

f1 <- fisher.test(m1)
f2 <- fisher.test(m2)
f3 <- fisher.test(m3)

d_subsets <- list()
d_subsets[[1]] <- c(1:110)
d_subsets[[2]] <- c(111:220)
d_subsets[[3]] <- c(221:330)
d_subsets[[4]] <- c(331:440)
d_subsets[[5]] <- c(441:550)
d_subsets[[6]] <- c(551:661)

delta <- c()
for (i in 1:length(d_subsets)) {
  d_s <- d_subsets[[i]]
  training_set <- eclipse.3.x[d_s, ]
  real_values <- eclipse.3.y[d_s]
  
  prediction1 <- tree.classify(training_set, eclipse.single.tree)
  prediction2 <- tree.classify(training_set, eclipse.random.tree)
  
  e1 <- fn(prediction1, real_values) + fp(prediction1, real_values)
  e2 <- fn(prediction2, real_values) + fp(prediction2, real_values)
  
  delta <- c(delta, (e1 - e2))
}

delta_l <- length(delta)
delta_s <- sum(delta)/delta_l

delta_s_s <- 0
for (i in 1:delta_l) {
  delta_s_s <- delta_s_s + (delta[i] - delta_s)^2
}

s_delta <- sqrt((1 / (delta_l * (delta_l - 1))) * delta_s_s)

t_n_k <- delta_s / s_delta

z.test <- function(p_1, p_2, r) {
  f_12 <- 0
  f_21 <- 0
  
  for ( i in 1:length(r) ) {
    if ( p_1[i] == 0 && p_2[i] == 1 && r[i] == 0 ) {
      f_12 <- f_12 + 1
    }
    if ( p_1[i] == 1 && p_2[i] == 0 && r[i] == 1 ) {
      f_12 <- f_12 + 1
    }
    
    if ( p_1[i] == 0 && p_2[i] == 1 && r[i] == 1 ) {
      f_21 <- f_21 + 1
    }
    if ( p_1[i] == 1 && p_2[i] == 0 && r[i] == 0 ) {
      f_21 <- f_21 + 1
    }
  }
  
  n <- abs(f_12 - f_21)
  d <- sqrt(f_12 + f_21)
  return(n / d)
}

print(z.test(eclipse.random.prediciton.test, eclipse.single.prediciton.test, eclipse.3.y))
print(z.test(eclipse.single.prediciton.test, eclipse.bagging.prediciton.test, eclipse.3.y))
print(z.test(eclipse.random.prediciton.test, eclipse.bagging.prediciton.test, eclipse.3.y))