
impurity <- function(vector) {
  n <- length(vector)
  n_0 <- length(which(vector == 0))
  p <- n_0/n
  return(p*(1-p))
}

bestsplit <- function(x, y, minleaf) {
  df <- data.frame(x, y)
  
  sorted_x <- sort(unique(x))
  quality <- c()
  bquality <- NULL
  bcondition <- NULL
  
  n <- length(x)
  
  i <- 1
  for ( i in 1:(length(sorted_x) - 1) ) {
    c <- (sorted_x[i] + sorted_x[i+1]) / 2
    
    subvector_y1 <- df[which(df[,1] <= c), 2]
    subvector_y2 <- df[which(df[,1] > c), 2]
    
    n1 <- length(subvector_y1)
    n2 <- length(subvector_y2)
    
    if ( (n1 <= minleaf) || (n2 <= minleaf) ) { # we do not allow these splits
      break
    } 
    
    q <- (n1/n)*impurity(subvector_y1) + (n2/n)*impurity(subvector_y2)
    if ( is.null(bquality) || (q < bquality) ) {
      bquality <- q
      bcondition <- c
    }
  }
  
  if ( is.null(bcondition) ) {
    return(NULL)
  }
  
  bs <- list()
  bs[[1]] <- bquality
  bs[[2]] <- bcondition
  return(bs)
}

is.leaf <- function(data, nmin) {
  if ( (ncol(data) <= 1) || (nrow(data) <= nmin) ) { # can not split it anymore
    return(TRUE)
  }
  
  n_0 <- length(which(data[, "y"] == 0))
  n_1 <- length(which(data[, "y"] == 1))
  if ( n_0 == 0 || n_1 == 0 ) { # pure node
    return(TRUE)
  }
  
  return(FALSE)
}

split.node <- function(data, nmin, minleaf) {
  if ( is.leaf(data, nmin) ) {
    return(NULL)
  }
  
  column_names <- names(data)
  classification_column <- data[, "y"]
  
  bs <- NULL
  
  column_label <- NULL
  column_condition <- NULL
  
  for ( c in 1:(ncol(data)-1) ) {
    column <- data[, c]
    tmp_bs <- bestsplit(column, classification_column, minleaf)
    
    if ( is.null(tmp_bs) ) { # skip, there does not exist a best split
      break
    } 
    
    if ( is.null(bs) || (tmp_bs[[1]] < bs) ) {
      bs <- tmp_bs[[1]]
      column_label <- column_names[c]
      column_condition <- tmp_bs[[2]]
    }
    
  }
  
  if ( is.null(column_label) && is.null(column_condition) ) {
    return(NULL)
  }
  
  col_con <- list()
  col_con[[1]] <- column_label
  col_con[[2]] <- column_condition
  return(col_con)
}

tree.grow.help <- function(data, nmin, minleaf, nfeat) {
  nc <- ncol(data)
  if ( nc <= nfeat + 1 ) { # +1 due to y column
    tmp_data <- data
  } else {
    rnd_columns <- sample(nc - 1, size = nfeat, replace = FALSE)
    tmp_data <- data[, c(rnd_columns, nc)] # we add nc (y column)
  }
  
  bsplit <- split.node(tmp_data, nmin, minleaf) 
  
  if ( is.null(bsplit) ) { # split does not exist -> node becomes a leaf
    n_0 <- length(which(data[, "y"] == 0))
    n_1 <- length(which(data[, "y"] == 1))
    if ( n_0 > n_1 ) {
      return(0)
    }
    return(1)
  }
  
  bsplit_label <- bsplit[[1]]
  bsplit_condition <- bsplit[[2]]
  
  left_data <- data[which(data[,bsplit_label] <= bsplit_condition), , drop = FALSE]
  left_tree <- tree.grow.help(left_data, nmin, minleaf, nfeat)
  
  right_data <- data[which(data[,bsplit_label] > bsplit_condition), , drop = FALSE]
  right_tree <- tree.grow.help(right_data, nmin, minleaf, nfeat)
  
  tree <- list()
  tree[[1]] <- bsplit 
  tree[[2]] <- left_tree
  tree[[3]] <- right_tree
  
  return(tree)
}

tree.grow <- function(x, y, nmin, minleaf, nfeat) {
  data <- cbind(x, "y" = y)
  return(tree.grow.help(data, nmin, minleaf, nfeat))
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
  
  if ( class(tree) == "numeric" ) {
    return(tree)
  }
  
  return(tree.classify.help(sample, tree))
}

tree.classify <- function(x, tr) {
  predictions <- c()
  if ( class(tr) != "list" ) {
    print("Just root node.")
  }
  
  else {
    for ( row in 1:nrow(x) ) {
      p <- tree.classify.help(x[row, , drop = FALSE], tr)
      predictions <- c(predictions, p)
    }
  }
  return(predictions)
}

tree.grow.bag <- function(x, y, nmin, minleaf, nfeat, m) {
  trees <- list()
  for ( i in 1:m ) {
    random_rows <- sample(nrow(x), nrow(x), replace = TRUE)
    tmp_x <- x[random_rows,]
    tmp_y <- y[random_rows]
    trees[[i]] <- tree.grow(tmp_x, tmp_y, nmin, minleaf, nfeat)
  }
  return(trees)
}

tree.classify.bag <- function(trees, x) {
  predictions <- c()
  num_trees <- length(trees)
  
  for (r in 1:nrow(x)) {
    row <- x[r, , drop = FALSE]
    tmp_predictions <- c()
    for (t in 1:length(trees)) {
      current_tree <- trees[[t]]
      print(length(current_tree))
      p <- tree.classify(row, current_tree)
      tmp_predictions <- c(tmp_predictions, p)
    }
    
    n <- length(which(tmp_predictions == 0))
    if ( n > num_trees/2 ) {
      predictions <- c(predictions, 0)
    } else {
      predictions <- c(predictions, 1)
    }
  }
  
  return(predictions)
}

