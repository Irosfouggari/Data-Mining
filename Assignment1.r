credit.dat <- read.csv("/Users/Ana/Documents/Sola/Faks/2s/2l/DM/Assignment1/credit.txt")

impurity <- function(vector) {
  n <- length(vector)
  n_0 <- length(which(vector == 0))
  p <- n_0/n
  return(p*(1-p))
}

bestsplit <- function(x, y) {
  df <- data.frame(x, y)
  
  sorted_x <- sort(unique(x))
  quality <- c()
  
  n <- length(x)
  
  i <- 1
  while ( i < length(sorted_x) ) {
    c <- (sorted_x[i] + sorted_x[i+1]) / 2
    
    subvector_y1 <- df[which(df[,1] <= c), 2]
    subvector_y2 <- df[which(df[,1] > c), 2]
    
    n1 <- length(subvector_y1)
    n2 <- length(subvector_y2)
    
    q <- (n1/n)*impurity(subvector_y1) + (n2/n)*impurity(subvector_y2)
    quality <- c(quality, q)
    
    i <- i+1
  }
  
  j <- which(quality == min(quality))
  return((sorted_x[j] + sorted_x[j + 1]) / 2)
}

is.leaf <- function(data, nmin) {
  if ( ncol(data) <= 1 ) { # can not split it anymore
    return(TRUE)
  }
  
  if ( nrow(data) <= nmin ) {
    return(TRUE)
  }
   
  n_0 <- length(which(data[, "y"] == 0))
  n_1 <- length(which(data[, "y"] == 1))
  if ( n_0 == 0 || n_1 == 0 ) {
    return(TRUE)
  }
  
  return(FALSE)
}

split.node <- function(data, nmin) {
  if ( is.leaf(data, nmin) ) {
    return(NULL)
  }
  
  return(bestsplit(data[,1], data[, "y"]))
}

tree.grow.help <- function(data, nmin) {
  bsplit <- split.node(data, nmin) 
  
  if ( is.null(bsplit) ) { # split does not exist -> node becomes a leaf
    n_0 <- length(which(data[, "y"] == 0))
    n_1 <- length(which(data[, "y"] == 1))
    if ( n_0 > n_1 ) {
      return("0")
    }
    return("1")
  }
  
  left_data <- data[which(data[,1] <= bsplit), , drop = FALSE]
  left_data <- left_data[, -1, drop = FALSE]
  left_tree <- tree.grow.help(left_data, nmin)
  
  right_data <- data[which(data[,1] > bsplit), , drop = FALSE]
  right_data <- right_data[, -1, drop = FALSE]
  right_tree <- tree.grow.help(right_data, nmin)

  tree <- list()
  tree[[1]] <- bsplit 
  tree[[2]] <- left_tree
  tree[[3]] <- right_tree
  
  return(tree)
}

tree.grow <- function(x, y, nmin) {
  data <- cbind(x, "y" = y)
  return(tree.grow.help(data, nmin))
}

tree.classify.help <- function(sample, tr) {
  value <- sample[1]
  c <- tr[[1]]
  
  if ( value <= c ) { # left tree
    tree <- tr[[2]]
  } else { # right tree
    tree <- tr[[3]]
  }
  
  if ( length(tree) == 1 ) {
    if ( tree == "0" ) {
      return(0)
    }
    
    if ( tree == "1" ) {
      return(1)
    }
  }
  
  tree.classify.help(sample[-1], tree)
}

tree.classify <- function(x, tr) {
  for ( row in 1:nrow(x) ) {
    print(tree.classify.help(x[row,], tr))
  }
  return("end")
}