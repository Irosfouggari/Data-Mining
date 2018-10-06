
source("/Users/Ana/Documents/Sola/Faks/2s/2l/DM/Assignment1/Assignment1.r")
library("mlbench")

precision <- function(y_p, y_r) { # TP/(TP + FP)
  TP <- 0
  FP <- 0
  for ( i in 1:length(y_p) ) {
    if ( (y_p[i] == 1) && (y_r[i] == 1) ) {
      TP <- TP + 1
    }
    if ( (y_p[i] == 1) && (y_r[i] == 0) ) {
      FP <- FP + 1
    }
  }
  return(TP/(TP + FP))
}

recall <- function(y_p, y_r) { # TP/(TP + FN)
  TP <- 0
  FN <- 0
  for ( i in 1:length(y_p) ) {
    if ( (y_p[i] == 1) && (y_r[i] == 1) ) {
      TP <- TP + 1
    }
    if ( (y_p[i] == 0) && (y_r[i] == 1) ) {
      FN <- FN + 1
    }
  }
  return(TP/(TP + FN))
}

accurancy <- function(y_p, y_r) { # (TP + TN)/(TP + TP + FP + FN)
  TP <- 0
  FN <- 0
  TN <- 0
  FP <- 0
  for ( i in 1:length(y_p) ) {
    if ( (y_p[i] == 1) && (y_r[i] == 1) ) {
      TP <- TP + 1
    }
    if ( (y_p[i] == 0) && (y_r[i] == 1) ) {
      FN <- FN + 1
    }
    if ( (y_p[i] == 0) && (y_r[i] == 0) ) {
      TN <- TN + 1
    }
    if ( (y_p[i] == 1) && (y_r[i] == 0) ) {
      FP <- FP + 1
    }
  }
  
  print(paste("TP: ", TP))
  print(paste("TN: ", TN))
  print(paste("FP: ", FP))
  print(paste("FN: ", FN))
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
eclipse.2.x <- eclipse.2[, c(3, 5:44)]
eclipse.2.y <- toZeroOne(eclipse.2[, 4])

eclipse.3 <- read.csv(file=file.path("/Users/Ana/Documents/Sola/Faks/2s/2l/DM/Assignment1/Data", "eclipse-metrics-packages-3.0.csv"), sep = ";")
eclipse.3.x <- eclipse.3[, c(3, 5:44)]
eclipse.3.y <- toZeroOne(eclipse.3[, 4])


### SINGLE ###
print("single")
eclipse.single.tree <- tree.grow(eclipse.2.x, eclipse.2.y, 15, 5, 41)

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
# eclipse.bagging.tree <- tree.grow.bag(eclipse.2.x, eclipse.2.y, 15, 5, 41, 100)
# 
# eclipse.bagging.prediciton.train <- tree.classify.bag(eclipse.bagging.tree, eclipse.2.x)
# eclipse.bagging.precision.train <- precision(eclipse.bagging.prediciton.train, eclipse.2.y)
# eclipse.bagging.recall.train <- recall(eclipse.bagging.prediciton.train, eclipse.2.y)
# eclipse.bagging.accurancy.train <- accurancy(eclipse.bagging.prediciton.train, eclipse.2.y)
# 
# eclipse.bagging.prediciton.test <- tree.classify.bag(eclipse.bagging.tree, eclipse.3.x)
# eclipse.bagging.precision.test <- precision(eclipse.bagging.prediciton.test, eclipse.3.y)
# eclipse.bagging.recall.test <- recall(eclipse.bagging.prediciton.test, eclipse.3.y)
# eclipse.bagging.accurancy.test <- accurancy(eclipse.bagging.prediciton.test, eclipse.3.y)


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