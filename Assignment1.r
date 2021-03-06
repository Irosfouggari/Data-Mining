# impurity
# INPUT: vector (vector)
# OUTPUT: Gini index (numeric)
# The function calculates the Gini index of an input vector
impurity <- function(vector) {
  n <- length(vector)
  n_0 <- length(which(vector == 0))
  p <- n_0 / n
  return(p * (1 - p))
}

# best.split
# INPUT: x (vector), y (vector), minleaf (numeric)
# OUTPUT: [quality of the split, condition, minimum of impurities of child nodes]
# The function finds the best split in a given column, it skips the splits which are not allowed according to the minleaf value.
best.split <- function(x, y, minleaf) {
  sorted_x <- sort(unique(x))
  bquality <- NULL
  bcondition <- NULL
  bimpurity <- NULL
  
  i <- 1
  for (i in 1:(length(sorted_x) - 1)) {
    # average of two consecutive values of x in the sorted order
    c <- (sorted_x[i] + sorted_x[i + 1]) / 2
    
    # one child x<=c
    suby_1 <- y[which(x <= c)]
    # other child x>c
    suby_2 <- y[which(x > c)]
    
    n1 <- length(suby_1)
    n2 <- length(suby_2)
    
    if ((n1 < minleaf) ||
        (n2 < minleaf)) {
      # these splits are not allowed
      next
    }
    
    i1 <- impurity(suby_1)
    i2 <- impurity(suby_2)
    q <- n1 * i1 + n2 * i2
    
    if (is.null(bquality) || (q < bquality)) {
      bquality <- q
      bcondition <- c
      bimpurity <- min(i1, i2)
    }
  }
  
  if (is.null(bquality)) {
    return(NULL)
  }
  
  bs <- list()
  bs[["quality"]] <- bquality
  bs[["condition"]] <- bcondition
  bs[["impurity"]] <- bimpurity
  return(bs)
}

# is.leaf
# INPUT: data (matrix), labeling_column (numeric), nmin (numeric)
# OUTPUT: TRUE or FALSE
# The function accepts as input the data and checks if it is a leaf (if number of records is lower than nmin or the node is pure).
is.leaf <- function(data, labeling_column, nmin) {
  if ((nrow(data) < nmin) ||
      (impurity(data[, labeling_column]) == 0)) {
    return(TRUE)
  }
  return(FALSE)
}

# split.node
# INPUT: data (matrix), lebeling_column (numeric), nmin (numeric), minleaf (numeric)
# OUTPUT: [name of the column after which we split the node, number which separetes child nodes from eachother] / NULL (if we can not split)
# The function finds the optimal split and returns column name after which the best split is done after, and the value which decides in which child node we continue.
# If there is no split found, the function returns NULL.
split.node <- function(data, lebeling_column, nmin, minleaf) {
  if (is.leaf(data, lebeling_column, nmin)) {
    return(NULL)
  }
  
  column_names <- names(data[,-lebeling_column])
  classification_column <- data[, lebeling_column]
  
  bs <- NULL
  
  column_label <- NULL
  column_condition <- NULL
  bimpurity <- NULL
  
  for (c in 1:length(column_names)) {
    column_name <- column_names[c]
    column <- data[, column_name]
    tmp_bs <- best.split(column, classification_column, minleaf)
    
    if (is.null(tmp_bs)) {
      # skip, there does not exist a best split
      next
    }
    
    quality <- tmp_bs[["quality"]]
    if (is.null(bs) || (quality < bs)) {
      bs <- quality
      
      column_label <- column_name
      column_condition <- tmp_bs[["condition"]]
      
      bimpurity <- tmp_bs[["impurity"]]
      
    } else if (quality == bs) {
      bimpurity_tmp <- tmp_bs[["impurity"]]
      
      if (bimpurity_tmp < bimpurity) {
        bs <- quality
        
        column_label <- column_name
        column_condition <- tmp_bs[["condition"]]
        
        bimpurity <- bimpurity_tmp
      }
    }
  }
  
  if (is.null(column_label)) {
    return(NULL)
  }
  
  col_con <- list()
  col_con[["column_label"]] <- column_label
  col_con[["column_condition"]] <- column_condition
  return(col_con)
}

# tree.grow.help
# INPUT: data (matrix), labeling_column (numeric), nmin (numeric), minleaf (numeric), nfeat (numeric)
# OUTPUT: tree which is defined recursively: [[attribute name, value which separates left and right tree], left tree, right tree], if tree is a leaf it equals to either 0 or 1
# The function grows a tree recursively: finds a best split; if it exists it continues the same way on child nodes, otherwise it returns 0 or 1 (majority rule).
tree.grow.help <-
  function(data,
           labeling_column,
           nmin,
           minleaf,
           nfeat) {
    nc <- ncol(data)
    
    if (nc <= nfeat + 1) {
      # +1 due to a labeling column
      tmp_data <- data
      tmp_labeling_column <- labeling_column
    } else {
      rnd_columns <-
        sample(c(1:nc)[-labeling_column], size = nfeat, replace = FALSE)
      tmp_data <-
        data[, c(rnd_columns, labeling_column), drop = FALSE] # we add lebeling column
      tmp_labeling_column <- nfeat + 1
    }
    
    bsplit <- split.node(tmp_data, tmp_labeling_column, nmin, minleaf)
    
    if (is.null(bsplit)) {
      # split does not exist -> node becomes a leaf
      n_0 <- length(which(data[, labeling_column] == 0))
      n_1 <- length(which(data[, labeling_column] == 1))
      if (n_0 >= n_1) {
        return(0)
      }
      return(1)
    }
    
    bsplit_label <- bsplit[["column_label"]]
    bsplit_condition <- bsplit[["column_condition"]]
    
    left_rows <- which(data[, bsplit_label] <= bsplit_condition)
    left_data <- data[left_rows, , drop = FALSE]
    left_tree <-
      tree.grow.help(left_data, labeling_column, nmin, minleaf, nfeat)
    
    right_data <- data[-left_rows, , drop = FALSE]
    right_tree <-
      tree.grow.help(right_data, labeling_column, nmin, minleaf, nfeat)
    
    tree <- list()
    tree[["label_condition"]] <- bsplit
    tree[["left_tree"]] <- left_tree
    tree[["right_tree"]] <- right_tree
    
    return(tree)
  }

# tree.grow
# INPUT: x (matrix), y (vector), nmin (numeric), minleaf (numeric), nfeat (numeric)
# OUTPUT: a tree grown with the function tree.grow.help
# Function binds “x” matrix with labeling vector “y” and gives the new matrix to the function tree.grow.help (with other parameters).
tree.grow <- function(x, y, nmin, minleaf, nfeat) {
  data <- cbind(x, y)
  return(tree.grow.help(data, ncol(data), nmin, minleaf, nfeat))
}

# tree.classify.help
# INPUT: sample (matrix), tr (list)
# OUTPUT: 0 or 1
# The function goes trough the tree and returns the leaf value at the end.
tree.classify.help <- function(sample, tr) {
  if (class(tr) == "numeric") {
    # tree is a leaf
    return(tr)
  }
  
  c <- tr[["label_condition"]]
  col_label <- c[["column_label"]]
  con <- c[["column_condition"]]
  value <- sample[1, col_label]
  
  if (value <= con) {
    tree <- tr[["left_tree"]]
  } else {
    tree <- tr[["right_tree"]]
  }
  
  return(tree.classify.help(sample, tree))
}

# tree.classify
# INPUT: x (matrix), tr (list)
# OUTPUT: vector of predictions
# The function accepts as input the data matrix containing the attribute values of the cases require prediction and a tree object created by the tree.grow.
tree.classify <- function(x, tr) {
  predictions <- c()
  
  for (row in 1:nrow(x)) {
    p <- tree.classify.help(x[row, , drop = FALSE], tr)
    predictions <- c(predictions, p)
  }
  
  return(predictions)
}

# tree.grow.bag
# INPUT: x (matrix), y (vector), nmin (numeric), minleaf (numeric), nfeat (numeric), m (numeric)
# OUTPUT: list of m trees
# The function grows m trees and store them in a list.
tree.grow.bag <- function(x, y, nmin, minleaf, nfeat, m) {
  trees <- list()
  
  for (i in 1:m) {
    random_rows <- sample(nrow(x), nrow(x), replace = TRUE)
    tmp_x <- x[random_rows, , drop = FALSE]
    tmp_y <- y[random_rows]
    trees[[i]] <- tree.grow(tmp_x, tmp_y, nmin, minleaf, nfeat)
  }
  
  return(trees)
}

# tree.classify.bag
# INPUT: trees (list), x (matrix)
# OUPUT: vector of predistions
# The function classifies the input records by majority rule.
tree.classify.bag <- function(trees, x) {
  predictions <- c()
  num_trees <- length(trees)
  
  for (r in 1:nrow(x)) {
    row <- x[r, , drop = FALSE]
    tmp_predictions <- c()
    for (t in 1:num_trees) {
      current_tree <- trees[[t]]
      p <- tree.classify(row, current_tree)
      tmp_predictions <- c(tmp_predictions, p)
    }
    
    n <- length(which(tmp_predictions == 0))
    if (n >= (num_trees / 2)) {
      predictions <- c(predictions, 0)
    } else {
      predictions <- c(predictions, 1)
    }
  }
  
  return(predictions)
}
