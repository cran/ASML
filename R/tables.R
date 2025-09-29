# -------------------------------------------------------------------------
# File: KPI_tables.R
#
# Purpose:
#   Provides functions to generate KPI (Key Performance Indicator) tables
#   and summary tables from objects of class `as_data`. These utilities are
#   used to evaluate algorithm performance on training or test datasets and
#   to compare predictions against optimal and best baselines.
#
# Major functions:
#   - KPI_table():
#       S3 generic. Dispatches to class-specific methods to generate a KPI table.
#
#   - KPI_table.as_data():
#       Implementation for `as_data` objects. Produces a table of means and
#       geometric means for normalized (and optionally original) KPI values.
#       Can include ML predictions if provided.
#
#   - KPI_summary_table():
#       S3 generic. Dispatches to class-specific methods to generate a KPI summary table.
#
#   - KPI_summary_table.as_data():
#       Implementation for `as_data` objects. Produces a table summarizing the
#       KPI for the optimal selection per instance, the best global option,
#       and (if available) the machine learning predictions.
#
# Notes:
#   - KPI_table() and KPI_summary_table() are exported generics.
#   - Their `.as_data` implementations are user-facing and return formatted tables.
#   - Generics can be extended by developers to support additional data object classes.
#   - Requires `dplyr` for data manipulation and geometric mean calculation.
# -------------------------------------------------------------------------

utils::globalVariables(c("which_max_value"))


#' Internal generic for KPI_table
#' 
#' This function serves as the internal S3 generic for `KPI_table` methods.
#' It dispatches the call to the appropriate method based on the class of `data_object`.
#' Currently, only `as_data` is implemented. Users or developers can
#' extend this generic by writing new methods for other classes.
#' 
#' @param data_object object.
#' @param ... other parameters.
#' 
#' @details
#' This generic is not intended to be used directly by package users. It exists
#' to enable method dispatch for different classes. Marked as `internal` to keep
#' it out of the user-facing function index.
#' 
#' @keywords internal
#' @export
KPI_table <- function(data_object, ...) {
  UseMethod("KPI_table")
}


#' @title KPI table
#' @description Function that generates a table with the values of the KPI.
#' @param data_object an object of class `as_data`.
#' @param predictions a data frame with the predicted KPI for each algorithm (columns) and for each instance (rows). If NULL, the table won't include a ML column.
#' @param test flag that indicates whether the function should use test data or training data.
#' @param ... other parameters.
#' @return A table with the statistics of the pace.
#' @importFrom dplyr %>%
#' @examples
#' data(branchingsmall)
#' data_object <- partition_and_normalize(branchingsmall$x, branchingsmall$y, test_size = 0.3,
#' family_column = 1, split_by_family = TRUE)
#' training <- AStrain(data_object, method = "glm")
#' predictions <- ASpredict(training, newdata = data_object$x.test)
#' KPI_table(data_object, predictions = predictions)
#' @export
KPI_table.as_data <- function(data_object, predictions = NULL, test = TRUE, ...) {

  y.original <- NULL
  if (test) {
    message("Test flag is enabled. Output values will be taken from the test set")
    y.norm <- data_object$y.test
    if(!is.null(data_object$y.test.original)){
      y.original <- data_object$y.test.original
    }

  } else {
    message("Test flag is disabled. Output values will be taken from the train set")
    y.norm <- data_object$y.train
    if(!is.null(data_object$y.train.original)){
      y.original <- data_object$y.train.original
    }
  }

  if(!is.null(predictions) && nrow(y.norm) != nrow(predictions)){
    stop(paste("The number of output instances [", nrow(y.norm), "] doesn't match the number of instances of the predicted data [", nrow(predictions), "]", sep=""))
  }

  
  if (!is.null(predictions)) {
    
    wm <- apply(predictions, 1, which.max)
    
    sel_norm <- numeric()
    for (i in 1:nrow(y.norm)){
      sel_norm[i] <- y.norm[i, wm[i]] 
    }
    
    y.norm <- cbind("ML" = sel_norm, y.norm)

    if(!is.null(y.original)){
      sel_original <- numeric()
      for (i in 1:nrow(y.original)){
        sel_original[i] <- y.original[i, wm[i]]
      }
       y.original <- cbind("ML" = sel_original, y.original)
    }
  }

  
  geom <- function(x){
    exp(mean(log(x)))
  }

  if(!is.null(y.original)){
    table <- as.table(cbind(apply(y.norm, 2, mean), apply(y.norm, 2, geom), apply(y.original, 2, mean), apply(y.original, 2, geom)))
    colnames(table) <- c("mean norm", "geom mean norm", "mean not norm", "geom mean not norm")
  }else{
    table <- as.table(cbind(apply(y.norm, 2, mean), apply(y.norm, 2, geom)))
    colnames(table) <- c("mean norm", "geom mean norm")
  }
  return(table)
}


#' Internal generic for KPI_summary_table
#' 
#' This function serves as the internal S3 generic for `KPI_summary_table` methods.
#' It dispatches the call to the appropriate method based on the class of `data_object`.
#' Currently, only `as_data` is implemented. Users or developers can
#' extend this generic by writing new methods for other classes.
#' 
#' @param data_object object.
#' @param ... other parameters.
#' 
#' @details
#' This generic is not intended to be used directly by package users. It exists
#' to enable method dispatch for different classes. Marked as `internal` to keep
#' it out of the user-facing function index.
#' 
#' @keywords internal
#' @export
KPI_summary_table <- function(data_object, ...) {
  UseMethod("KPI_summary_table")
}


#' @title KPI summary table
#' @description Function that generates a summary table of the KPI values. Optimal is the value of the KPI when choosing the best option for each instance. It's the best that we could do with respect to that KPI. Best is the value of the KPI for the best option overall according to the KPI. ML is the value of the KPI choosing for each instance the option selected by the learning.
#' @param data_object an object of class `as_data`.
#' @param predictions a data frame with the predicted KPI for each algorithm (columns) and for each instance (rows). If NULL, the table won't include a ML column.
#' @param test flag that indicates whether the function should use test data or training data.
#' @param normalized whether to use the original values of the KPI or the normalized ones used for the learning.
#' @param ... other parameters.
#' @return A table with the statistics of the pace.
#' @importFrom dplyr %>%
#' @examples
#' data(branchingsmall)
#' data_object <- partition_and_normalize(branchingsmall$x, branchingsmall$y, test_size = 0.3,
#' family_column = 1, split_by_family = TRUE)
#' training <- AStrain(data_object, method = "glm")
#' predictions <- ASpredict(training, newdata = data_object$x.test)
#' KPI_summary_table(data_object, predictions = predictions)
#' @export
KPI_summary_table.as_data <- function(data_object, predictions = NULL, test = TRUE, normalized = FALSE, ...) {
  if (test) {
    message("Test flag is enabled. Output values will be taken from the test set")
    y <- data_object$y.test
    if (!normalized) {
      y_data <- data_object$y.test.original
    } else {
      y_data <- y
    }
  } else {
    message("Test flag is disabled. Output values will be taken from the train set")
    y <- data_object$y.train
    if (!normalized) {
      y_data <- data_object$y.train.original
    } else {
      y_data <- y
    }
  }

  if(!is.null(predictions) && nrow(y) != nrow(predictions)){
    stop(paste("The number of output instances [", nrow(y), "] doesn't match the number of instances of the predicted data [", nrow(predictions), "]", sep=""))
  }

  
  geom <- function(x){
    exp(mean(log(x)))
  }

  data <- list()
  if (normalized | !data_object$better_smaller) {
    best <- which.max(apply(y_data, 2, geom))
  } else {
    best <- which.min(apply(y_data, 2, geom))
  }
  data$best <- y_data[,best]
  
  if (!is.null(predictions)) {
    
    wm <- apply(predictions, 1, which.max)
    
    sel <- numeric()
    for (i in 1:nrow(y)){
      sel[i] <- y_data[i, wm[i]] 
    }
    
    data$ML <- sel
  }

  optimal <- y %>% dplyr::rowwise() %>% dplyr::mutate(which_max_value = which.max(dplyr::c_across(dplyr::everything()))) %>% dplyr::select(which_max_value)
  data$optimal <- y_data[cbind(seq_along(optimal$which_max_value), optimal$which_max_value)]


  data <- as.data.frame(data)
  if (!is.null(predictions)) {
    names(data) <- c("best", "ML", "optimal")
  } else {
    names(data) <- c("best", "optimal")
  }

  table <- as.table(cbind(apply(data, 2, mean), apply(data, 2, geom)))
  colnames(table) <- c("mean", "geom mean")

  return(table)
}

