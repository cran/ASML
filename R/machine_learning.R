#' @title Machine learning process
#' @description Function that proceses input data, trains the machine learning models, makes a prediction and plots the results.
#' @param x dataframe with the instances (rows) and its features (columns). It may also include a column with the family data.
#' @param y dataframe with the instances (rows) and the corresponding output (KPI) for each algorithm (columns).
#' @param x.test dataframe with the test features. It may also include a column with the family data. If NULL, the algorithm will split x into training and test sets.
#' @param y.test dataframe with the test outputs. If NULL, the algorithm will split y into training and test sets.
#' @param family_column column number of x where each instance family is indicated. If given, aditional options for the training and set test splitting and the graphics are enabled.
#' @param split_by_family boolean indicating if we want to split sets keeping family proportions in case x.test and y.test are NULL. This option requires that option `family_column` is different from NULL
#' @param predict boolean indicating if predictions will be made or not. If FALSE plots will use training data only and no ML column will be displayed.
#' @param test_size float with the segmentation proportion for the test dataframe. It must be a value between 0 and 1.
#' @param better_smaller boolean that indicates wether the output (KPI) is better if smaller (TRUE) or larger (FALSE).
#' @param method name of the model to be used. The user can choose from any of the models provided by `caret`. See <http://topepo.github.io/caret/train-models-by-tag.html> for more information about the models supported.
#' @param test boolean indicating whether the predictions will be made with the test set or the training set.
#' @param color_list list with the colors for the plots. If NULL or insufficient number of colors, the colors will be generated automatically.
#' @return A list with the data and plots generated, including:
#' * `data_obj` An `as_data` object with the processed data from `partition_and_normalize()` function.
#' * `training` An `as_train` object with the trainings from the `AStrain()` function.
#' * `predictions` A data frame with the predictions from the `ASpredict()` function, if the predict param is TRUE.
#' * `table` A table with the summary of the output data.
#' * `boxplot`, `ranking_plot`, `figure_comparison`, `optml_figure_comparison` and `optmlall_figure_comparison` with the corresponding plots.
#' @examples
#' \donttest{
#' data(branchingsmall)
#' machine_learning <- ml(branchingsmall$x, branchingsmall$y, test_size = 0.3,
#' family_column = 1, split_by_family = TRUE, method = "glm")
#' }
#' @export

ml <- function(x, y, x.test = NULL, y.test = NULL, family_column = NULL, split_by_family = FALSE, predict = TRUE, test_size = 0.25, better_smaller = TRUE, method ="ranger", test = TRUE, color_list = NULL){
  #para que los posibles warnings se muestren en el momento que ocurren
  options(warn = 1)
  #procesamos los datos
  message("Processing data...")
  data_obj <- partition_and_normalize(x, y, x.test, y.test, test_size = test_size, family_column = family_column, split_by_family = split_by_family, better_smaller = better_smaller)

  # Aprendizaje
  message(paste("\nLearning with",  method, "method"))
  training <- AStrain(data_obj, method = method)

  # Predicción
  predictions <- NULL
  if(predict){
    if(test){
      message("\nPredicting data for test set...")
      predictions <- ASpredict(training, newdata = data_obj$x.test)
    }else{
      message("\nPredicting data for train set...")
      predictions <- ASpredict(training)
    }
  }

  #Resumen de las predicciones
  message("\nGenerating data summary")
  table <- suppressMessages(KPI_table(data_obj, predictions, test = test))

  #Graficos
  message("\nGenerating  plots...")
  boxplot <- suppressMessages(boxplots(data_obj, predictions = predictions, test = test, by_families = !is.null(family_column), color_list = color_list))

  ranking_plot <- suppressMessages(ranking(data_obj, predictions = predictions, test = test, by_families = !is.null(family_column)))

  #los siguientes graficos solo se pueden hacer si hay predicciones
  if(predict){
    fig_comp <- suppressMessages(figure_comparison(data_obj, ties ="different_data_points", main = "Option Comparison", predictions = predictions, test = test, by_families = !is.null(family_column), color_list = color_list))

    optml_fig_comp <- suppressMessages(figure_comparison(data_obj, ties = "ml_if_optimal", main = "Modified Option Comparison (no ties if ML chooses optimal)", predictions = predictions, test = test, by_families = !is.null(family_column), color_list = color_list))

    optmlall_fig_comp <- suppressMessages(figure_comparison(data_obj, ties = "ml_selection", main = "Modified Option Comparison (for ties, ML selection is chosen)", predictions = predictions, test = test, by_families = !is.null(family_column), color_list = color_list))
  }else{
    message("predict param is FALSE. No figure comparison will be done")
    fig_comp <- NULL
    optml_fig_comp <- NULL
    optmlall_fig_comp <- NULL
  }
  return(list("data_obj" = data_obj, "training" = training, "predictions" = predictions, "table" = table, "boxplot" = boxplot, "ranking_plot" = ranking_plot,
              "fig_comp" = fig_comp, "optml_fig_comp" = optml_fig_comp, "optmlall_fig_comp" = optmlall_fig_comp))
}

#' @title Training models for posterior selection of algorithms
#' @description For each algorithm (column) in the data, a model is trained to later predict the output (KPI) for that algorithm (using function `ASpredict()`).
#' @param data_object an object.
#' @param ... other parameters.
#' @return A list, result of the respective AStrain method.
#' @export
"AStrain" <- function(data_object, ...) {
  UseMethod("AStrain")
}

#' @title Training models for posterior selection of algorithms
#' @description For each algorithm (column) in the data, a model is trained to later predict the output (KPI) for that algorithm (using function `ASpredict()`).
#' @param data_object object of class `as_data`.
#' @param method name of the model to be used. The user can choose from any of the models provided by `caret`. See <http://topepo.github.io/caret/train-models-by-tag.html> for more information about the models supported.
#' @param parallel boolean to control whether to parallelise the training or not (paralellization is handled by library snow).
#' @param f function we want to use to train the models. If NULL, `caret`'s function will be used.
#' @param ... arguments passed to the caret train function.
#' @return
#' A list is returned of class `as_train` containing the trained models, one for each of the algorithms.
#' @examples
#' data(branchingsmall)
#' data_object <- partition_and_normalize(branchingsmall$x, branchingsmall$y, test_size = 0.3,
#' family_column = 1, split_by_family = TRUE)
#' training <- AStrain(data_object, method = "glm")
#' custom_function <- function(x, y) {
#'   glm.fit(x, y)
#' }
#' custom_training <- AStrain(data_object, f = "custom_function")
#' @export
AStrain.as_data <- function(data_object, method = NULL, parallel = FALSE, f = NULL, ...) {

  if (is.null(f) && is.null(method)) {
    stop("method parameter missing. Required to use caret. If wanted, set custom training function using the f parameter.")
  }

  data_obj <- data_object
  x <- data_obj$x.train
  y <- data_obj$y.train
  trained_models <- vector(mode = "list", length = ncol(y))

  if (!is.null(f)) {
    message("Using your training function")
  }

  if (!parallel) {
    ####### opcion sin paralelizar
    for (j in 1:ncol(y)){ # tantos modelos como opciones tengamos
      message(paste("Training model ", j, " [", names(y)[j], "]", sep = ""))
      if (is.null(f)) {
        trained_models[[j]] <- caret::train(x, y[,j], method = method, ...)
      } else {
	    func <- get(f, envir = globalenv())  # Obtener la función por su nombre
        trained_models[[j]] <- func(x, y[,j], ...)
      }
    }
    #######
  } else {
    # Comprobación de que el usuario tiene el paquete snow instalado
    if (!requireNamespace("snow", quietly = TRUE)) {
      stop("Yo need to install the snow package to use the parallel option")
    }
    ############# opcion con paquete snow (no mejora el tiempo de ejecucion)
    cluster <- snow::makeCluster(ncol(y))

    if (is.null(f)) {
      train_model1 <- function(j, x, y, method, ...) {
        message(paste("Training model ", j, " [", names(y)[j], "]", sep = ""))
        caret::train(x, y[,j], method = method, ...)
      }
      trained_models <- snow::clusterApply(cluster, 1:ncol(y), train_model1, x, y, method, ...)
    } else {
      train_model2 <- function(j, x, y, ...) {
        message(paste("Training model ", j, " [", names(y)[j], "]", sep = ""))
		func <- get(f, envir = globalenv()) 
        func(x, y[,j], ...)
      }
      trained_models <- snow::clusterApply(cluster, 1:ncol(y), train_model2, x, y, method, ...)
    }

    snow::stopCluster(cluster)
    ############

    ##opcion con foreach (no mejora el tiempo)
    # my.cluster <- parallel::makeCluster(parallel::detectCores() - 1, type = "PSOCK")
    # doParallel::registerDoParallel(cl = my.cluster)
    #
    # trained_models <- foreach(j = 1:ncol(y)) %dopar% {
    #   #message(paste("Training model ", j, " [", names(y)[j], "]", sep = ""))
    #   caret::train(x, y[,j], method = method, ...)
    # }
    #
    # parallel::stopCluster(cl = my.cluster)
  }

  names(trained_models) <- paste("model_", names(y), sep = "")

  class(trained_models) <- c(c("as_train"), class(trained_models))
  return(trained_models)
}

#' @title Predicting the KPI value for the algorithms
#' @description For each algorithm, the output (KPI) is predicted using the models trained with `AStrain()`.
#' @param training_object list of class `as_train`.
#' @param ... other parameters.
#' @return A data frame, result of the respective ASpredict method. 
#' @export
"ASpredict" <- function(training_object, ...) {
  UseMethod("ASpredict")
}


#' @title Predicting the KPI value for the algorithms
#' @description For each algorithm, the output (KPI) is predicted using the models traing with `AStrain()`.
#' @details
#' The `ASpredict()` uses the prediction function from `caret` to compute (for each of the models trained) the predictions for the new data provided by the user.
#' If the user used a custom function in `AStrain()` (given by parameter `f`), `caret`'s default prediction function might not work, and the user might have to provide a custom function for `ASpredict()` as well.
#' Additionally, this custom prediction function allows to pass additional arguments, something that `caret`'s default prediction function does not.
#' The object return by the train function used in `AStrain()` (`caret`'s or a custom one) is the one passed to the custom `f` function defined by the user. This `f` function must return a vector with the predictions.
#'
#' @param training_object list of class `as_train`.
#' @param newdata dataframe with the new data to predict. If not present, predictions are computed using the training data.
#' @param f function to use for the predictions. If NULL, `caret`'s function will be used.
#' @param ... arguments passed to the predict function f when f is not NULL.
#' @return A data frame with the predictions for each instance (rows), corresponding to each algorithm (columns). In case f is specified, some actions might be needed to get the predictions from the returned value.
#' @examples
#' data(branchingsmall)
#' data_object <- partition_and_normalize(branchingsmall$x, branchingsmall$y, test_size = 0.3,
#' family_column = 1, split_by_family = TRUE)
#' training <- AStrain(data_object, method = "glm")
#' predictions <- ASpredict(training, newdata = data_object$x.test)
#' qrf_q_predict <- function(modelFit, newdata, what = 0.5, submodels = NULL) {
#'   out <- predict(modelFit, newdata, what = what)
#'   if (is.matrix(out))
#'     out <- out[, 1]
#'   out
#' }
#' custom_predictions <- ASpredict(training, newdata = data_object$x.test, f = "qrf_q_predict",
#' what = 0.25)
#' @export
ASpredict.as_train <- function(training_object, newdata = NULL, f = NULL, ...){

  if(is.null(f) && !is.null(newdata) && ncol(training_object[[1]]$trainingData) != (ncol(newdata) + 1)){
    stop("the number of features in newdata must be the same as in the training dataset.")
  }

  if(is.null(newdata)){
    predictions = data.frame(matrix(NA, ncol = length(training_object), nrow = nrow(training_object[[1]]$trainingData)))
    #newdata = training_object[[1]]$trainingData[, -ncol(training_object[[1]]$trainingData)]
  }else{
    predictions = data.frame(matrix(NA, ncol = length(training_object), nrow = nrow(newdata)))
  }

  colnames(predictions) <- paste("predictions_",names(training_object), sep = "")

  if(is.null(f)){
    message("Using caret's package default function to predict")
    metodo = training_object[[1]]$method
    for (j in 1:ncol(predictions)){
      predictions[, j] <- eval(parse(text = paste("getModelInfo('", metodo, "')$", metodo, "$predict", sep="")))(training_object[[j]]$finalModel, newdata)
    }
  }else{
    message("Using your prediction function")
    for (j in 1:ncol(predictions)){
	  func <- get(f, envir = globalenv()) 
      predictions[, j] <- func(training_object[[j]], newdata = newdata, ...)
    }
  }

  class(predictions) <- c("as_predict", class(predictions))
  return(predictions)
}
