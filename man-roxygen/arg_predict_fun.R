#' @param predict_fun [\code{function(1)}]\cr
#'   A function returning a numeric vector of predictions for \code{model}.
#'   The function must have two arguments: \code{object} and \code{newdata}.
#'   Default function is: \cr
#'   \preformatted{function(object, newdata)
#'     predict(object, newdata = newdata)}
#'   In case of a classification task it must return probabilities
#'   (or other numeric values) instead of factor levels.\cr
#'   See argument \code{model} for a list of models for which
#'   the correct prediction function is already implemented.
