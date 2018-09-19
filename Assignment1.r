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
  
  best_impurity <- min(quality)
  j <- which(quality == best_impurity)
  bs <- list()
  bs[[1]] <- best_impurity
  bs[[2]] <- (sorted_x[j] + sorted_x[j + 1]) / 2
  return(bs)
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
  
  column_names <- names(data)
  classification_column <- data[, "y"]
  
  bs <- 9999999
  
  for ( c in 1:(ncol(data)-1) ) {
    column <- data[, c]
    tmp_bs <- bestsplit(column, classification_column)
    if ( is.null(tmp_bs[[1]]) ) {
      # skip
    } else {
      if ( tmp_bs[[1]] < bs ) {
        bs <- tmp_bs[[1]]
        column_label <- column_names[c]
        column_condition <- tmp_bs[[2]]
      }
    }
  }
  
  col_con <- list()
  col_con[[1]] <- column_label
  col_con[[2]] <- column_condition
  return(col_con)
}

tree.grow.help <- function(data, nmin) {
  bsplit <- split.node(data, nmin) 
  bsplit_label <- bsplit[[1]]
  bsplit_condition <- bsplit[[2]]
  
  if ( is.null(bsplit_condition) ) { # split does not exist -> node becomes a leaf
    n_0 <- length(which(data[, "y"] == 0))
    n_1 <- length(which(data[, "y"] == 1))
    if ( n_0 > n_1 ) {
      return("0")
    }
    return("1")
  }
  
  left_data <- data[which(data[,bsplit_label] <= bsplit_condition), , drop = FALSE]
  left_data <- left_data[, -which(names(left_data) == bsplit_label), drop = FALSE]
  left_tree <- tree.grow.help(left_data, nmin)
  
  right_data <- data[which(data[,bsplit_label] > bsplit_condition), , drop = FALSE]
  right_data <- right_data[, -which(names(right_data) == bsplit_label), drop = FALSE]
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
  c <- tr[[1]]
  col_label <- c[[1]]
  con <- c[[2]]
  value <- sample[1, col_label]
  
  if ( value <= con ) { # left tree
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
  
  tree.classify.help(sample[, -which(names(sample) == col_label)], tree)
}

tree.classify <- function(x, tr) {
  for ( row in 1:nrow(x) ) {
    print(tree.classify.help(x[row, , drop = FALSE], tr))
  }
  return("end")
}

d <- credit.dat
t <- tree.grow(d[, -6], d[, 6], 0)
r <- tree.classify(d[, -6], t)