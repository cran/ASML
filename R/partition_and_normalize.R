# -------------------------------------------------------------------------
# File: partition_and_normalize.R
#
# Purpose:
#   Provides utilities to partition input data into training and test sets
#   and to normalize outputs according to performance criteria (whether
#   smaller or larger values are better). Also supports family-based
#   splitting of data to preserve proportions across sets.
#
# Major functions:
#   - partition_and_normalize():
#       Main function. Handles partitioning of data (or uses user-supplied
#       train/test sets), normalizes outputs, and manages optional family
#       metadata.
#
#   - data_split():
#       Internal helper function. Splits the data into training and test
#       subsets using either random sampling or stratified sampling based
#       on family information.
#
# Notes:
#   - partition_and_normalize() is exported for user use.
#   - data_split() is internal (not exported, marked with @noRd).
#   - Normalization ensures comparability of KPIs across instances by
#     rescaling according to the best-performing algorithm.
# -------------------------------------------------------------------------

#' @title Partition and Normalize
#' @description Function that processes the input data splitting it into training and test sets and normalizes the outputs depending on the best instance performance. The user can bypass the partition into training and test set by passing the parameters `x.test` and `y.test`.
#' @param x dataframe with the instances (rows) and its features (columns). It may also include a column with the family data.
#' @param y dataframe with the instances (rows) and the corresponding output (KPI) for each algorithm (columns).
#' @param x.test dataframe with the test features. It may also include a column with the family data. If NULL the algorithm will split x into training and test sets.
#' @param y.test dataframe with the test outputs. If NULL the algorithm will y into training and test sets.
#' @param family_column column number of x where each instance family is indicated. If given, adittional options for the training and set test splitting and the graphics are enabled.
#' @param split_by_family boolean indicating if we want to split sets keeping family proportions in case x.test and y.test are NULL. This option requires that option `family_column` is different from NULL.
#' @param test_size float with the segmentation proportion for the test dataframe. It must be a value between 0 and 1. Only needed when `x.test` and `y.test` are NULL.
#' @param better_smaller boolean that indicates whether the output (KPI) is better if smaller (TRUE) or larger (FALSE).
#' @return
#' A list is returned of class `as_data` containing:
#' * `x.train` A data frame with the training features.
#' * `y.train` A data frame with the training output.
#' * `x.test` A data frame with the test features.
#' * `y.test` A data frame with the test output.
#' * `y.train.original` A vector with the original training output (without normalizing).
#' * `y.test.original` A vector with the original test output (without normalizing).
#' * `families.train` A data frame with the families of the training data.
#' * `families.test` A data frame with the families of the test data.
#' @importFrom dplyr %>%
#' @examples
#' data(branching)
#' data_obj <- partition_and_normalize(branching$x, branching$y, test_size = 0.3,
#' family_column = 1, split_by_family = TRUE)
#'
#' @export
partition_and_normalize <- function(x, y, x.test = NULL, y.test = NULL, family_column = NULL, split_by_family = FALSE, test_size = 0.3, better_smaller = TRUE){

  if(nrow(x) != nrow(y)){
    stop("the number of instances in the dataframes x and y does not match.")
  }

  
  
  if(!is.null(x.test) && !is.null(y.test)){
    if(nrow(x.test) != nrow(y.test)){
      stop("the number of instances in the dataframes x.test and y.test does not match.")
    }
    build_test <- FALSE

  } else {
    if(is.null(x.test) && !is.null(y.test)){
      warnings("The y.test element has values but the x.test dataframe is NULL. The algorithm will perform a segmentation over x and y and ignore y.test.")
    } else if(!is.null(x.test) && is.null(y.test)){
      warnings("The x.test element has values but the y.test dataframe is NULL. The algorithm will perform a segmentation over x and y and ignore x.test.")
    }
    build_test <- TRUE
  }

  
  family_data.train <- NULL
  family_data.test <- NULL
  family <- FALSE
  if(!is.null(family_column)){
    if(family_column > ncol(x)){
      stop("family_column is greater than the number of columns in x")
    }
    family <- TRUE
  }

  if (build_test) {
    
    data <- data_split(x, y, test_size, family_column = family_column, split_by_family = split_by_family)
    
    features.train <- data$x.train
    y.train <- data$y.train
    
    features.test  <- data$x.test
    y.test <- data$y.test
  } else {
    
    features.train <- x
    y.train <- y
    
    features.test  <- x.test
  }

  if (family) {
    
    family_data.train <- data.frame(x = features.train[, family_column], row.names = rownames(features.train))
    
    features.train <- features.train[, -family_column]
    
    family_data.test <- data.frame(x = features.test[, family_column], row.names = rownames(features.test))
    
    features.test <- features.test[, -family_column]
  }

  
  features.train <- features.train %>% dplyr::select_if(is.numeric)
  features.test <- features.test %>% dplyr::select_if(is.numeric)
  y.train <- y.train %>% dplyr::select_if(is.numeric)
  y.test <- y.test %>% dplyr::select_if(is.numeric)

  
  
  
  y.train.original = y.train
  y.test.original = y.test
  if(better_smaller){
    eps = 0.001 
    y.train <- sweep(1 / (y.train + eps), 1, FUN = "*", apply((y.train + eps), 1, min))
    y.test <- sweep(1 / (y.test + eps), 1, FUN = "*", apply((y.test + eps), 1, min))
  } else {
    eps = 0.001 
    y.train <- sweep((y.train + eps), 1, FUN = "/", apply((y.train + eps), 1, max))
    y.test <- sweep((y.test + eps), 1, FUN = "/", apply((y.test + eps), 1, max))
  }

  
  if(any(is.na(features.train)) || any(is.na(y.train))){
    warning("There are NA values in the train datasets. Use the na.action parameter in training and prediction to manage this issue.")
  }
  if(any(is.na(features.test)) || any(is.na(y.test))){
    warning("There are NA values in the test datasets. Use the na.action parameter in prediction to manage this issue.")
  }

  proc_data <- list(x.train = features.train, y.train = y.train, y.train.original = y.train.original, x.test = features.test, y.test = y.test, y.test.original = y.test.original, families.train = family_data.train, families.test = family_data.test, better_smaller = better_smaller)
  class(proc_data) <- "as_data"

  return(proc_data)
}


#' @title Data Split into train and test.
#' @description Function that splits x and y to generate train and test dataframes.
#' @param x dataframe with the instances (rows) and its features (columns). It may also include a column with the family data.
#' @param y dataframe with the instances (rows) and the corresponding output (KPI) for each algorithm (columns)
#' @param test_size float with the segmentation proportion for the test dataframe. It must be a value between 0 and 1.
#' @param family_column column number of x where each instance family is indicated. If given, adittional options for the training and test set splitting and the graphics are enabled.
#' @param split_by_family boolean indicating if we want to split sets keeping family proportions. This option requires that option `family_column` is different from NULL.
#' @return
#' A list is returned containing:
#'  \item{x.train}{A data frame with the training features}
#'  \item{y.train}{A data frame with the training output}
#'  \item{x.test}{A data frame with the test features}
#'  \item{y.test}{A data frame with the test output}
#' @noRd
#'

data_split <- function(x, y, test_size = 0.3, family_column = NULL, split_by_family = FALSE){
  if(test_size < 0 || test_size > 1){
    stop("test_size parameter should be a real value between 0 and 1")
  }
  if(!split_by_family){
    
    sample <- sample.int(n = nrow(x), size = floor((1 - test_size) * nrow(x)), replace = F)
  }else{
    if(is.null(family_column)){
      stop("split_by_family is TRUE but family_column is NULL. Please indicate the index of the column where family data is located")
    }else{
      aux = dplyr::bind_cols("id" = 1:nrow(x),"family" = x[ , family_column])
      sample = (aux %>% dplyr::group_by("family") %>% dplyr::slice_sample(prop = (1 - test_size)))$id
    }

  }
  
  x.train <- x[sample, ]
  y.train <- y[sample, ]
  
  x.test  <- x[-sample, ]
  y.test <- y[-sample, ]

  return(list("x.train" = x.train, "y.train" = y.train, "x.test" = x.test, "y.test" = y.test))
}

