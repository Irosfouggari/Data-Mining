
impurity <- function(vector) {
  n <- length(vector)
  n_0 <- length(which(vector == 0))
  p <- n_0/n
  return(p*(1-p))
}

best.split <- function(data, column_name, labeling_clumn_name, minleaf) {
  
  sorted_x <- sort(unique(data[, column_name]))
  bquality <- NULL
  bcondition <- NULL
  bimpurity <- NULL
  
  i <- 1
  for ( i in 1:(length(sorted_x) - 1) ) {
    c <- (sorted_x[i] + sorted_x[i+1]) / 2
    
    subdata_1 <- data[which(data[,column_name] <= c), , drop = FALSE]
    subdata_2 <- data[which(data[,column_name] > c), , drop = FALSE]
    
    n1 <- nrow(subdata_1)
    n2 <- nrow(subdata_2)
    
    if ( (n1 < minleaf) || (n2 < minleaf) ) { # we do not allow these splits
      next
    }
    
    i1 <- impurity(subdata_1[, labeling_clumn_name])
    i2 <- impurity(subdata_2[, labeling_clumn_name])
    q <- n1*i1 + n2*i2
    
    if ( is.null(bquality) || (q < bquality) ) {
      bquality <- q
      bcondition <- c
      bimpurity <- min(i1, i2)
    } 
  }
  
  if ( is.null(bcondition) ) {
    return(NULL)
  }
  
  bs <- list()
  bs[["quality"]] <- bquality
  bs[["condition"]] <- bcondition
  bs[["impurity"]] <- bimpurity
  return(bs)
}

is.leaf <- function(data, nmin) {
  if ( (nrow(data) < nmin) || (impurity(data[, "y"]) == 0) ) { # we do not allow split anymore || pure node
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
  bipurity <- NULL
  left_data <- NULL
  right_data <- NULL
  
  for ( c in 1:(length(column_names)-1) ) {
    column_name <- column_names[c]
    column <- data[, column_name]
    tmp_bs <- best.split(data, column_name, "y", minleaf)
    
    if ( is.null(tmp_bs) ) { # skip, there does not exist a best split
      next
    } 
    
    if ( is.null(bs) || (tmp_bs[[1]] < bs) ) {
      bs <- tmp_bs[["quality"]]
      
      column_label <- column_name
      column_condition <- tmp_bs[["condition"]]
      
      bipurity <- tmp_bs[["impurity"]]
      
    } else {
      if ( tmp_bs[["quality"]] == bs ) {
        bs_tmp <- tmp_bs[["quality"]]
        column_label_tmp <- column_name
        column_condition_tmp <- tmp_bs[["condition"]]
        
        bipurity_tmp <- tmp_bs[["impurity"]]
        
        if ( bipurity_tmp < bipurity ) {
          bs <- bs_tmp
          
          column_label <- column_label_tmp
          column_condition <- column_condition_tmp
          
          bipurity <- bipurity_tmp
        }
        
      }
    }
  }
  
  if ( is.null(column_label) ) {
    return(NULL)
  }
  
  col_con <- list()
  col_con[["column_label"]] <- column_label
  col_con[["column_condition"]] <- column_condition
  return(col_con)
}

tree.grow.help <- function(data, labeling_column_name, nmin, minleaf, nfeat) {
  nc <- ncol(data)
  
  if ( nc <= nfeat + 1 ) { # +1 due to y column
    tmp_data <- data
  } else {
    labeling_column <- which( colnames(data) == labeling_column_name )
    rnd_columns <- sample(c(1:nc)[-labeling_column], size = nfeat, replace = FALSE)
    tmp_data <- data[, c(rnd_columns, labeling_column)] # we add nc (y column)
  }
  
  bsplit <- split.node(tmp_data, nmin, minleaf) 
  
  if ( is.null(bsplit) ) { # split does not exist -> node becomes a leaf
    n_0 <- length(which(data[, "y"] == 0))
    n_1 <- length(which(data[, "y"] == 1))
    if ( n_0 >= n_1 ) {
      return(0)
    }
    return(1)
  }
  
  bsplit_label <- bsplit[["column_label"]]
  bsplit_condition <- bsplit[["column_condition"]]
  
  left_data <- data[which(data[,bsplit_label] <= bsplit_condition), , drop = FALSE]
  left_tree <- tree.grow.help(left_data, labeling_column_name, nmin, minleaf, nfeat)
  
  right_data <- data[which(data[,bsplit_label] > bsplit_condition), , drop = FALSE]
  right_tree <- tree.grow.help(right_data, labeling_column_name, nmin, minleaf, nfeat)
  
  tree <- list()
  tree[["label_condition"]] <- bsplit 
  tree[["left_tree"]] <- left_tree
  tree[["right_tree"]] <- right_tree
  
  return(tree)
}

tree.grow <- function(x, y, nmin, minleaf, nfeat) {
  data <- cbind(x, "y" = y)
  return(tree.grow.help(data, "y", nmin, minleaf, nfeat))
}

tree.classify.help <- function(sample, tr) {
  if ( class(tr) == "numeric" ) { # is a leaf
    return(tr)
  }
  
  c <- tr[["label_condition"]]
  col_label <- c[["column_label"]]
  con <- c[["column_condition"]]
  value <- sample[1, col_label]
  
  if ( value <= con ) { 
    tree <- tr[["left_tree"]]
  } else { 
    tree <- tr[["right_tree"]]
  }
  
  return(tree.classify.help(sample, tree))
}

tree.classify <- function(x, tr) {
  predictions <- c()

  for ( row in 1:nrow(x) ) {
    p <- tree.classify.help(x[row, , drop = FALSE], tr)
    predictions <- c(predictions, p)
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

