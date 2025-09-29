# ========================================================================
# File: graphics.R
# 
# Purpose:
# This file contains all plotting and visualization utilities for the
# package. It provides user-facing plotting methods for objects of class
# "as_data" and several helper functions to generate common figures
# such as boxplots, rankings, and model comparison charts.
#
# Major functions:
# - plot.as_data(): Generic plotting method for 'as_data' objects.
#   Displays multiple plots (boxplots, rankings, comparisons) either
#   sequentially or selectively depending on user input.
#
# - boxplots.as_data(): Creates boxplots of objective function values
#   for each method or model in the dataset, including ML predictions
#   when available.
#
# - ranking.as_data(): Produces ranking plots of methods/models across
#   all problem instances, supporting custom ordering and color palettes.
#
# - figure_comparison.as_data(): Generates model comparison figures
#   based on pairwise differences, with configurable handling of ties.
#
# Notes:
# - All functions rely on ggplot2 for plotting.
# - Functions gracefully handle cases where 'predictions' is missing.
# - Color palettes are generated dynamically using Polychrome.
# ========================================================================

utils::globalVariables(c("rowname"))

#' @title Plot
#' @description For an object of class `as_data`, function that makes several plots, including the following: a boxplot, a ranking plot and comparisons between the different options.
#' @param x object of class `as_data`.
#' @param labels character vector with the labels for each of the algorithms. If NULL, the y names of the `data_object` names will be used.
#' @param test flag that indicates whether the function should use test data or training data.
#' @param predictions a data frame with the predicted KPI for each algorithm (columns) and for each instance (rows). If NULL, the plot won't include a ML column.
#' @param by_families boolean indicating whether the function should represent data by families or not. The family information must be included in the `data_object` parameter.
#' @param stacked boolean to choose between bar plot and stacked bar plot.
#' @param legend boolean to activate or deactivate the legend in the plot.
#' @param color_list list with the colors for the plots. If NULL, or insufficient number of colors, the colors will be generated automatically.
#' @param ml_color color for the ML boxplot. If NULL, it will be generated automatically.
#' @param path path where plots will be saved. If NULL they won't be saved.
#' @param ... other parameters.
#' @return A list with `boxplot`, `ranking`, `fig_comp`, `optml_fig_comp` and `optmlall_fig_comp` plots.
#' @importFrom rlang .data
#' @examples
#' \donttest{
#' data(branchingsmall)
#' data <- partition_and_normalize(branchingsmall$x, branchingsmall$y)
#' training <- AStrain(data, method = "glm")
#' predict_test <- ASpredict(training, newdata = data$x.test)
#' plot(data, predictions = predict_test)
#' }
#' @export
plot.as_data <- function(x, labels = NULL, test = TRUE, predictions = NULL, by_families = FALSE, stacked = TRUE, legend = TRUE, color_list = NULL, ml_color = NULL, path = NULL, ...){

  if(!is.null(path)){
    grDevices::pdf(file = path)
    message(paste("Plots will be saved in ", path))
    
    p1 <- suppressMessages(boxplots(x, predictions = predictions, test = test, by_families = by_families, color_list = color_list, ml_color = ml_color, labels = labels))
    p1
    
    p2 <- suppressMessages(ranking(x, predictions = predictions, test = test, by_families = by_families, labels = labels))
    p2
    if(is.null(predictions)){
      message("No comparison plots are displayed because the predictions parameter is NULL")
      p3 <- NULL
      p4 <- NULL
      p5 <- NULL
    }else{
      
      p3 <- suppressMessages(figure_comparison(x, ties ="different_data_points", main = "Option Comparison", predictions = predictions,  test = test, by_families = by_families, color_list = color_list, xlab = labels, legend = legend))
      p3
      p4 <- suppressMessages(figure_comparison(x, ties = "ml_if_optimal", main = "Modified Option Comparison (no ties if ML chooses optimal)", predictions = predictions,  test = test, by_families = by_families, color_list = color_list, xlab = labels, legend = legend))
      p4
      p5 <- suppressMessages(figure_comparison(x, ties = "ml_selection", main = "Modified Option Comparison (for ties, the ML-selected option is used)", predictions = predictions,  test = test, by_families = by_families, color_list = color_list, xlab = labels, legend = legend))
      p5
    }
    grDevices::dev.off()
  }else{
    if(is.null(predictions)){
      message("No comparison plots are displaying due to NULL predictions param")
      message("Generating plots...")
      
      p1 <- suppressMessages(boxplots(x, predictions = predictions, test = test, by_families = by_families, color_list = color_list, ml_color = ml_color, labels = labels))
      p1
      readline("Press enter to see next plot. Remaining: 1")
      
      p2 <- suppressMessages(ranking(x, predictions = predictions, test = test, by_families = by_families, labels = labels))
      p2
      p3 <- NULL
      p4 <- NULL
      p5 <- NULL
    }else{
      message("Generating plots...")
      
      p1 <- suppressMessages(boxplots(x, predictions = predictions, test = test, by_families = by_families, color_list = color_list, ml_color = ml_color, labels = labels))
      p1
      readline("Press enter to see next plot. Remaining: 4")
      
      p2 <- suppressMessages(ranking(x, predictions = predictions, test = test, by_families = by_families, labels = labels))
      p2
      readline("Press enter to see next plot. Remaining: 3")
      
      p3 <- suppressMessages(figure_comparison(x, ties ="different_data_points", main = "Option Comparison", predictions = predictions,  test = test, by_families = by_families, color_list = color_list, xlab = labels, legend = legend))
      p3
      readline("Press enter to see next plot. Remaining: 2")
      p4 <- suppressMessages(figure_comparison(x, ties = "ml_if_optimal", main = "Modified Option Comparison (no ties if ML chooses optimal)", predictions = predictions,  test = test, by_families = by_families, color_list = color_list, xlab = labels, legend = legend))
      p4
      readline("Press enter to see next plot. Remaining: 1")
      p5 <- suppressMessages(figure_comparison(x, ties = "ml_selection", main = "Modified Option Comparison (for ties, ML selection is chosen)", predictions = predictions,  test = test, by_families = by_families, color_list = color_list, xlab = labels, legend = legend))
      p5
    }
  }
  
  return(list("boxplot" = p1, "ranking" = p2, "fig_comp" = p3, "optml_fig_comp" = p4, "optmlall_fig_comp" = p5))
}

#' Internal generic for boxplots
#' 
#' This function serves as the internal S3 generic for `boxplots` methods.
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
boxplots <- function(data_object, ...) {
  UseMethod("boxplots")
}




#' @title Boxplots
#' @description Represents a boxplot for each of the algorithms to compare their performance according to the response variable (KPI). When available, it also includes a box plot for the "ML" algorithm generated from the predictions.
#' @param data_object object of class `as_data`.
#' @param main an overall title for the plot.
#' @param labels character vector with the labels for each of the algorithms. If NULL, the y names of the `data_object` names will be used.
#' @param test flag that indicates whether the function should use test data or training data.
#' @param predictions a data frame with the predicted KPI for each algorithm (columns) and for each instance (rows). If NULL, the plot won't include a ML column.
#' @param by_families boolean indicating whether the function should represent data by families or not. The family information must be included in the `data_object` parameter.
#' @param color_list list with the colors for the plots. If NULL, or insufficient number of colors, the colors will be generated automatically.
#' @param ml_color color por the ML boxplot. If NULL, it will be generated automatically.
#' @param ordered_option_names vector with the name of the columns of `data_object` y variable in the correct order.
#' @param xlab a label for the x axis.
#' @param ylab a label for the y axis.
#' @param ... other parameters.
#' @return A `ggplot` object representing the boxplots of instance-normalized KPI for each algorithm across instances.
#' @examples
#' data(branchingsmall)
#' data <- partition_and_normalize(branchingsmall$x, branchingsmall$y)
#' training <- AStrain(data, method = "glm")
#' predict_test <- ASpredict(training, newdata = data$x.test)
#' boxplots(data, predictions = predict_test)
#' @export
boxplots.as_data <- function(data_object, main = "Boxplot Comparison", labels = NULL, test = TRUE, predictions = NULL, by_families = FALSE, color_list = NULL, ml_color = NULL, ordered_option_names= NULL, xlab = "Strategy", ylab = "KPI", ...) {

  
  if (test) {
    message("Test flag is enabled. The comparison will be performed between the test set and the predictions")
    y <- data_object$y.test
  } else {
    message("Test flag is disabled. The comparison will be performed between the train set and the predictions")
    y <- data_object$y.train
  }

  if(!is.null(predictions) && nrow(y) != nrow(predictions)){
    stop(paste("the number of output instances [", nrow(y), "] doesn't match the number of instances of the predicted data [", nrow(predictions), "]", sep=""))
  }

  
  if(by_families){
    if(test && is.null(data_object$families.test)){
      stop("by_families param is TRUE and data_object has no family classification for test set")
    }else if(!test && is.null(data_object$families.train)){
      stop("by_families param is TRUE and data_object has no family classification for train set")
    }
  }

  
  options <- as.vector(colnames(y))

  
  if (!is.null(predictions)) {
    
    wm <- apply(predictions, 1, which.max)
    
    sel <- numeric()
    for (i in 1:nrow(y)){
      sel[i] <- y[i, wm[i]] 
    }
    
    y <- cbind("ML" = sel, y)
    options <- append(options, "ML")
  }

  
  y$id <- seq(nrow(y))
  y <- reshape2::melt(y, id.vars = c("id"))

  
  if(by_families){
    
    if(test){
      y2 <- data.frame(data_object$families.test)
      y2$id <- seq(nrow(y2))
      y2 <- reshape2::melt(as.vector(data_object$families.test), id = c("id"))
    }else{
      y2 <- data.frame(data_object$families.train)
      y2$id <- seq(nrow(y2))
      y2 <- reshape2::melt(as.vector(data_object$families.train), id = c("id"))
    }
    
    y <- cbind(y, y2)
    colnames(y) <- c("id", "variable", "value", "family")
  }

  
  if (is.null(color_list) || length(color_list) < length(options)) {
    if (!is.null(color_list) && length(color_list) < length(options)) {
      warning("New colors were generated because there weren't enough colors for all the options.")
    }
    color_list <- as.vector(Polychrome::createPalette(length(options) + 1, c("#fadd42", "#25bee8", "#FF34dF", "#64f24b")))
  }

  if (is.null(ordered_option_names)) {
    ordered_option_names = options
  }
  y$variable <- factor(y$variable, levels = ordered_option_names, ordered = T)

  p <- ggplot2::ggplot(y, ggplot2::aes(x = .data[["variable"]], y = .data[["value"]], fill = .data[["variable"]])) +
    ggplot2::geom_boxplot(lwd = 0.15, outlier.size = 0.5, alpha = 0.4, show.legend = FALSE) +
    ggplot2::xlab(xlab) +  ggplot2::ylab(ylab) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(size = 10, face = "bold"),
      axis.title.y = ggplot2::element_text(size = 10, face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      plot.margin = ggplot2::margin(t = 30, r = 30, b = 30, l = 30)
    ) + ggplot2::scale_y_continuous(breaks = seq(0, 1, by = 0.25))


  if(by_families){
    p <- p + ggplot2::facet_wrap(~family)
  }
  
  if(!is.null(labels)){
    if(length(labels) == length(unique(y$variable))){
      p <- p + ggplot2::scale_x_discrete(labels = labels)
    }else{
      warning("the length of the label array doesnt match the number of boxplots. Default labels will be used")
    }
  }

  
  if(is.null(predictions)){
    if(!is.null(ml_color)){
      warning("predictions are NULL so there is no ML boxplot. ml_color param is being ignored")
    }
    p <- p + ggplot2::scale_fill_manual(values = color_list)
  }else{
    if(!is.null(ml_color)){
      p <- p + ggplot2::scale_fill_manual(values = c(ml_color, color_list))
    }else{
      p <- p + ggplot2::scale_fill_manual(values = color_list)
    }
  }

  p <- p + ggplot2::ggtitle(main) + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14))
  return(p)
}




#' Internal generic for ranking
#' 
#' This function serves as the internal S3 generic for `ranking` methods.
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
ranking <- function(data_object, ...) {
  UseMethod("ranking")
}


#' @title Ranking Plot
#' @description After ranking the algorithms for each instance, represents for each of the algorithms, a bar with the percentage of times it was in each of the ranking positions. The number inside is the mean value of the normalized response variable (KPI) for the problems for which the algorithm was in that ranking position. The option `predictions` allows to control if the "ML" algorithm is added to the plot.
#' @param data_object object of class `as_data`.
#' @param main an overall title for the plot.
#' @param labels character vector with the labels for each of the algorithms. If NULL, the y names of the `data_object` names will be used.
#' @param test flag that indicates whether the function should use test data or training data.
#' @param predictions a data frame with the predicted KPI for each algorithm (columns) and for each instance (rows). If NULL, the plot won't include a ML column.
#' @param by_families boolean indicating whether the function should represent data by families or not. The family information must be included in the `data_object` parameter.
#' @param ordered_option_names vector with the name of the columns of data_object y variable in the correct order.
#' @param xlab a label for the x axis.
#' @param ylab a label for the y axis.
#' @param ... other parameters.
#' @return A `ggplot` object representing the ranking of algorithms based on the instance-normalized KPI.
#' @examples
#' data(branchingsmall)
#' data <- partition_and_normalize(branchingsmall$x, branchingsmall$y)
#' training <- AStrain(data, method = "glm")
#' predict_test <- ASpredict(training, newdata = data$x.test)
#' ranking(data, predictions = predict_test)
#' @export
ranking.as_data <- function(data_object, main = "Ranking", labels = NULL, test = TRUE, predictions = NULL, by_families = FALSE, ordered_option_names= NULL, xlab = "", ylab = "", ...){

  
  if (test) {
    message("Test flag is enabled. The comparison will be performed between the test set and the predictions")
    y <- data_object$y.test
  } else {
    message("Test flag is disabled. The comparison will be performed between the train set and the predictions")
    y <- data_object$y.train
  }

  if(!is.null(predictions) && nrow(y) != nrow(predictions)){
    stop(paste("the number of output instances [", nrow(y), "] does not match the number of instances of the predicted data [", nrow(predictions), "]", sep=""))
  }

  
  if(by_families){
    if(test && is.null(data_object$families.test)){
      stop("by_families param is TRUE and data_object has no family classification for test set")
    }else if(!test && is.null(data_object$families.train)){
      stop("by_families param is TRUE and data_object has no family classification for train set")
    }
  }

  size <- ncol(y)
  options <- as.vector(colnames(y))

  if (!is.null(predictions)) {
    wm <- apply(predictions, 1, which.max) 
    sel <- numeric()
    for (i in 1:nrow(y)){
      sel[i] <- y[i, wm[i]] 
    }
    y <- cbind(y, "ML" = sel)
  }

  if (!is.null(predictions)) {
    prank <- t(apply(-y[,-1], 1, rank, ties.method = "min"))
    mls <- sweep(y[,-1], 1, FUN = "==", y[,1])
    prankml <- prank[cbind(1:nrow(prank), apply(mls, 1, which.max))]
    prank <- cbind(prankml, prank)
  } else {
    prank <- t(apply(-y, 1, rank, ties.method = "min"))
  }

  colnames(prank) <- colnames(y)
  pp <- prank %>% as.data.frame() %>% tidyr::gather()
  pp2 <- y %>% as.data.frame() %>% tidyr::gather()

  if(by_families){
    if(test){
      pp3 <- cbind(pp, "kpi" = pp2$value, "fam" = as.vector(data_object$families.test))
    }else{
      pp3 <- cbind(pp, "kpi" = pp2$value, "fam" = as.vector(data_object$families.train))
    }
    names(pp3) <- c("key", "value", "kpi", "fam")
    pp3 <- pp3 %>% dplyr::group_by(.data$key, .data$fam, .data$value) %>% dplyr::summarize(count = dplyr::n(), mean = round(mean(.data$kpi), digits = 2), .groups = "drop_last") %>% dplyr::mutate(freq = .data$count / sum(.data$count))
  } else {
    pp3 <- cbind(pp, "kpi" = pp2$value)
    pp3 <- pp3 %>% dplyr::group_by(.data$key, .data$value) %>% dplyr::summarize(count = dplyr::n(), mean = round(mean(.data$kpi), digits = 2), .groups = "drop_last") %>% dplyr::mutate(freq = .data$count / sum(.data$count))
  }

  if (is.null(ordered_option_names)) {
    ordered_option_names = colnames(y)
  }

  pp3$key <- factor(pp3$key, levels = ordered_option_names, ordered = T)
  pp3$value <- factor(pp3$value, levels = size:1)

  
  fun_color_range <- grDevices::colorRampPalette(c("#9e0142", "#d53e4f", "#f46d43", "#fdae61", "#fee08b", "#e6f598", "#abdda4", "#66c2a5"))
  color_list <- as.vector(fun_color_range(size))


  p <- ggplot2::ggplot(pp3, ggplot2::aes_string("key", "freq", fill = "value", label = "mean"))
  
  if(length(color_list) > 15){
    p <- p + ggplot2::geom_bar(stat = 'identity', position = 'stack', alpha = 0.35, linewidth = 0.25, colour = "white")
  }else{
    p <- p + ggplot2::geom_bar(stat = 'identity', position = 'stack', alpha = 0.35, linewidth = 0.25)
  }
  p <- p + ggplot2::geom_text(size = 2.5, position = ggplot2::position_stack(vjust = 0.5)) +
    ggplot2::scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.25)) +
    ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
    ggplot2::scale_fill_manual(values = color_list) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(size = 10, face = "bold"),
      axis.title.y = ggplot2::element_text(size = 10, face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.title = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 10),
      legend.key.size = ggplot2::unit(0.5, 'cm'),
      plot.margin = ggplot2::margin(t = 30, r = 30, b = 0, l = 30)
    )
  
  if(!is.null(labels)){
    if(length(labels) == length(unique(pp3$key))){
      p <- p + ggplot2::scale_x_discrete(labels = labels)
    }else{
      warning("the length of the label array does not match the number categories. Default labels will be used")
    }
  }

  if (!is.null(main)) {
    p <- p + ggplot2::ggtitle(main)
  }
  p <- p + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14), legend.position = "right", legend.direction = "vertical") + ggplot2::guides(fill = ggplot2::guide_legend(title = "Ranking", reverse = TRUE, nrow = size))
  if(by_families){
    p <- p + ggplot2::facet_wrap(~fam)
  }
  return(p)
}


#' Internal generic for figure_comparison
#' 
#' This function serves as the internal S3 generic for `figure_comparison` methods.
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
figure_comparison <- function(data_object, ...) {
  UseMethod("figure_comparison")
}


#' @title Figure Comparison
#' @rdname topic-name
#' @description Represents a bar plot with the percentage of times each algorithm is selected by ML compared with the optimal selection (according to the response variable or KPI).
#' @param data_object object of class `as_data`.
#' @param ties How to deal with ties. Must be one of:
#' * "different_data_points": Tied algorithms in the optimal selection are all counted as different data points (increasing the total number of x values and therefore giving all of the tied algorithms the same weight).
#' * "ml_if_optimal": For tied algorithms, the one selected by ML is chosen if it corresponds to the optimal one. Otherwise, the same as in option `different_data_points` is done.
#' * "ml_selection": For tied algorithms, the one preferred by the ML is chosen.
#' @param main an overall title for the plot.
#' @param labels character vector with the labels for each of the algorithms. If NULL, the y names of the `data_object` names will be used.
#' @param mllabel character vector with the labels for the Optimal and ML bars. If NULL, default names will be used.
#' @param test flag that indicates whether the function should use test data or training data.
#' @param predictions a data frame with the predicted KPI for each algorithm (columns) and for each instance (rows).
#' @param by_families boolean indicating whether the function should represent data by families or not. The family information must be included in the `data_object` parameter.
#' @param stacked boolean to choose between bar plot and stacked bar plot.
#' @param legend boolean to activate or deactivate the legend in the plot.
#' @param color_list list with the colors for the plots. If NULL, or insufficient number of colors, the colors will be generated automatically.
#' @param ordered_option_names vector with the name of the columns of data_object y variable in the correct order.
#' @param xlab a label for the x axis.
#' @param ylab a label for the y axis.
#' @param ... other parameters.
#' @return A `ggplot` object representing the bar plot with the percentage of times each algorithm is selected by ML compared with the optimal selection (according to the response variable or KPI).
#' @examples
#' data(branchingsmall)
#' data <- partition_and_normalize(branchingsmall$x, branchingsmall$y)
#' training <- AStrain(data, method = "glm")
#' predict_test <- ASpredict(training, newdata = data$x.test)
#' figure_comparison(data, predictions = predict_test)
#' @export
figure_comparison.as_data <- function(data_object, ties = "different_data_points", main = "Option Comparison", labels = NULL, mllabel = NULL, test = TRUE, predictions, by_families = FALSE, stacked = TRUE, color_list = NULL, legend = TRUE, ordered_option_names = NULL, xlab = "Criteria", ylab = "Instances (%)", ...){

  
  if (test) {
    message("Test flag is enabled. The comparison will be performed between the test set and the predictions")
    y <- data_object$y.test
  } else {
    message("Test flag is disabled. The comparison will be performed between the train set and the predictions")
    y <- data_object$y.train
  }

  if(is.null(predictions)){
    stop("predictions parameter can't be NULL")
  }else{
    if(nrow(y) != nrow(predictions)){
      stop(paste("the number of output instances [", nrow(y), "] doesn't match the number of instances of the predicted data [", nrow(predictions), "]", sep=""))
    }
  }


  
  if(by_families){
    if(test && is.null(data_object$families.test)){
      stop("by_families param is TRUE and data_object has no family classification for test set")
    }else if(!test && is.null(data_object$families.train)){
      stop("by_families param is TRUE and data_object has no family classification for train set")
    }
  }

  
  option_names = colnames(y)

  size <- ncol(y)
  rows <- nrow(y)

  
  predictions <- dplyr::as_tibble(predictions) %>% dplyr::mutate(optimal = purrr::map_int(asplit(., 1), .f = which.max))

  
  if (by_families) {
    if(test){
      fam <- as.vector(data_object$families.test[,1])
    }else{
      fam <- as.vector(data_object$families.train[,1])
    }
    predictions <- dplyr::bind_cols(predictions, family=fam)
    
    predictions <- predictions %>% dplyr::group_by(.data$family) %>% dplyr::mutate(nrows = 1/dplyr::n())
  }else{
    
    predictions <- predictions %>% dplyr::mutate(nrows = 1/nrow(predictions))
  }

  
  ml_selections <- predictions %>% dplyr::count(.data$optimal, wt = .data$nrows)

  
  compute_max <- function(x) {
    max(x[1:size])
  }
  y <- dplyr::as_tibble(y) %>% dplyr::mutate(optimal_value = purrr::map_dbl(asplit(., 1), .f = compute_max))

  if (ties == "different_data_points") {
    
    which_optimal_value = function(x) {
      as.vector(which(x[1:size] == x["optimal_value"]))
    }

    y <- y %>% dplyr::mutate(optimal = purrr::map(asplit(., 1), .f = which_optimal_value))
  } else if (ties == "ml_if_optimal") {
    
    get_optimal_current <- function(x) {
      KPIs <- as.numeric(x[1:size + 1]) 
      optimal_value <- as.numeric(x["optimal_value"])
      row <- as.integer(x[1])
      
      KPIs_optimal <- as.numeric(which(KPIs == optimal_value))
      intersect = intersect(KPIs_optimal, predictions[row,]$optimal)

      if (length(intersect) > 0) {
        as.numeric(intersect)
      } else {
        as.numeric(KPIs_optimal)
      }
    }

    y <- y %>% tibble::rownames_to_column() %>% dplyr::mutate(optimal = purrr::map(asplit(., 1), .f = get_optimal_current)) %>% dplyr::select(-rowname)
  } else if (ties == "ml_selection") {
    
    get_optimal_current <- function(x) {
      KPIs <- as.numeric(x[1:size + 1]) 
      optimal_value <- as.numeric(x["optimal_value"])
      row <- as.integer(x[1])
      
      prefered_ML <- sort(as.numeric(predictions[row, 1:size]), index.return = T, decreasing = T)$ix
      
      optimal <- as.numeric(which(KPIs == optimal_value))
      
      optimal <- optimal[order(match(optimal, prefered_ML))]
      return(optimal[1])
    }
    y <- y %>% tibble::rownames_to_column() %>% dplyr::mutate(optimal = purrr::map_dbl(asplit(., 1), .f = get_optimal_current)) %>% dplyr::select(-rowname)
  } else {
    stop("ties parameter must be one of 'different_data_points', 'ml_if_optimal' or 'ml_selection'.")
  }

  if(by_families) {
    y <- dplyr::bind_cols(y, family=fam)
    y <- y %>% dplyr::group_by(.data$family) %>% dplyr::mutate(nrows = dplyr::n())
  } else {
    
    y <- y %>% dplyr::mutate(nrows = rows)
  }

  count_optimal <- function(x) {
    1/length(x[size + 2][[1]])
  }

  
  
  
  y1 <- y[, 1:(size+2)] %>% dplyr::mutate(weight = purrr::map_dbl(asplit(., 1), .f = count_optimal))
  
  y <- cbind(y1, y[, (size+3):ncol(y)]) %>% tidyr::unnest(cols = .data$optimal)


  if(by_families) {
    
    optimal_selections <- y %>% dplyr::group_by(.data$family) %>% dplyr::count(.data$optimal, wt = .data$weight / .data$nrows)
    
    pp3 <- optimal_selections %>% dplyr::full_join(ml_selections, by= c("optimal", "family"))
    
    pp3 <- pp3[with(pp3, order(pp3$family, pp3$optimal)), ]
    
    if (is.null(mllabel)) {
      names(pp3) <- c("family", "option", "optimal", "ML")
    } else {
      names(pp3) <- c(c("family", "option", mllabel))
    }

  } else {
    optimal_selections <- y %>% dplyr::count(.data$optimal, wt = .data$weight / .data$nrows)
    pp3 <- optimal_selections %>% dplyr::full_join(ml_selections, by= "optimal")
    
    pp3 <- pp3[with(pp3, order(pp3$optimal)), ]
    
    if (is.null(mllabel)) {
      names(pp3) <- c("option", "optimal", "ML")
    } else {
      names(pp3) <- c(c("option", mllabel))
    }
  }
  
  pp3[is.na(pp3)] <- 0
  
  pp3 <- pp3 %>% dplyr::mutate(option_name = names(y)[.data$option]) %>% dplyr::select(!.data$option)

  
  if(by_families) {
    pp3 <- pp3 %>% tidyr::pivot_longer(!c(.data$option_name, .data$family))
    names(pp3) <- c("family", "value", "key", "freq")
  } else{
    pp3 <- pp3 %>% tidyr::pivot_longer(!c(.data$option_name))
    names(pp3) <- c("value", "key", "freq")
  }

  
  if (is.null(ordered_option_names)) {
    ordered_option_names = option_names
  }
  pp3$value <- factor(pp3$value, levels = ordered_option_names, ordered = T)

  
  if (is.null(color_list) || length(color_list) < length(option_names)) {
    if (length(color_list) < length(option_names)) {
      warning("New colors were generated because there were not enough colors for all the options.")
    }
    color_list <- as.vector(Polychrome::createPalette(length(levels(pp3$value)), c("#fadd42", "#25bee8", "#FF34dF", "#64f24b")))
  }

  p <- ggplot2::ggplot(pp3, ggplot2::aes(.data$key, .data$freq, fill = .data$value)) +
    ggplot2::scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.1)) +
    ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(size = 10, face = "bold"),
      axis.title.y = ggplot2::element_text(size = 10, face = "bold"),
      plot.margin = ggplot2::margin(t = 30, r = 30, b = 30, l = 30)
    )

  if (!is.null(main)) {
    p <- p + ggplot2::ggtitle(main)
  }

  if (legend) {
    p <- p + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 13, margin = ggplot2::margin(t = 0, r = 0, b = 3, l = 0, unit = "mm")), legend.position = "bottom", legend.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0, unit = "mm")) + ggplot2::guides(fill = ggplot2::guide_legend(title = "Option", reverse = T, nrow = ceiling(size/2)))
  } else {
    p <- p + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 13, margin = ggplot2::margin(t = 0, r = 0, b = 3, l = 0, unit = "mm")), legend.position = "none")
  }

  if(stacked){
    p <- p + ggplot2::geom_col(position = 'stack', alpha = 0.4, linewidth = 0.25, width = 0.5)
  } else{
    p <- p + ggplot2::geom_col(position = ggplot2::position_dodge(), alpha = 0.4, linewidth = 0.25, width = 0.5)
  }

  
  if(!is.null(labels)){
    if(length(labels) == length(unique(pp3$value))){
      p <- p + ggplot2::scale_fill_manual(values = color_list, labels = labels)
    }else{
      warning("the length of the label array doesnt match the number of boxplots. Default labels will be used")
    }
  }else{
    p <- p + ggplot2::scale_fill_manual(values = color_list)
  }
  if(by_families){
    p <- p + ggplot2::facet_wrap(~family)
  }

  return(p)
}
