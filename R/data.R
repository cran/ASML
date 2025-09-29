#' Branching point selection in Polynomial Optimization
#'
#' Data from Ghaddar et al. (2023) used to select among several branching criteria for an RLT-based algorithm. Includes features for the instances and KPI values for the different branching criteria for executions lasting 1 hour.
#'
#' @format A list with x (features) and y (KPIs) data.frames.
#' @source Ghaddar, B., Gómez-Casares, I., González-Díaz, J., González-Rodríguez, B., Pateiro-López, B., & Rodríguez-Ballesteros, S. (2023). Learning for Spatial Branching: An Algorithm Selection Approach. INFORMS Journal on Computing.
"branching"

#' Branching point selection in Polynomial Optimization
#'
#' Data from Ghaddar et al. (2023) used to select among several branching criteria for an RLT-based algorithm. Includes features for the instances and KPI values for the different branching criteria for executions lasting 10 minutes.
#'
#' @format A list with x (features) and y (KPIs) data.frames.
#' @source Ghaddar, B., Gómez-Casares, I., González-Díaz, J., González-Rodríguez, B., Pateiro-López, B., & Rodríguez-Ballesteros, S. (2023). Learning for Spatial Branching: An Algorithm Selection Approach. INFORMS Journal on Computing.
"branchingsmall"

#' Automatic selection of the most suitable storage format for sparse matrices on GPUs
#'
#' Data from Pichel and Pateiro-López (2018), which contains information on 8111 sparse matrices. Each matrix is described by a set of nine structural features, and the performance of the single-precision SpMV kernel was measured under three storage formats: compressed row storage (CSR), ELLPACK (ELL), and hybrid (HYB). For each matrix and format, performance is expressed as the average GFLOPS (billions of floating-point operations per second), over 1000 SpMV operations.
#'
#' @format A list with x (features) and y (KPIs) data.frames.
#' @source Pichel, J. C., & Pateiro-López, B. (2018). A new approach for sparse matrix classification based on deep learning techniques. In 2018 IEEE International Conference on Cluster Computing (CLUSTER) (pp. 46–54). 
"SpMVformat"

