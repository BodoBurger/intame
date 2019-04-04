#' @param model [any model object]\cr
#'   Can be any model that can generate numeric predictions.
#'   For some models you have to provide the correct prediction function.
#'
#'   At the moment the following models are supported out of the box
#'   (meaning you do not have to provide a prediction function):
#'   \itemize{
#'     \item \code{\link[mlr]{WrappedModel}} (\code{mlr} package)
#'     \item \code{\link[caret]{train}} (\code{caret} package)
#'     \item \code{\link[stats]{lm} / \link[stats]{glm}} (\code{stats})
#'     \item \code{\link[randomForest]{randomForest}} (\code{randomForest} package)
#'     \item \code{\link[gbm]{gbm}} (\code{gbm} package)
#'     \item \code{\link[e1071]{svm}} (\code{e1071} package)
#'     \item \code{\link[nnet]{nnet}} (\code{nnet} package)
#'   }
#'
