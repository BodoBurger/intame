#' Wrapper for \code{computeALE} and \code{computePD}
#'
#' Calls either \link[intame]{computeALE} or \link[intame]{computePD} depending on
#' \code{fe_method}.
#'
#' @param fe_method Either "ALE" or "PD".
#' @template arg_model
#' @template arg_data
#' @param feature [\code{character}] Name of the feature.
#' @template arg_predict_fun
#' @param grid_size Create template.
#' @param ... Passed on to the FE method. See \link[intame]{computeALE}
#'            or \link[intame]{computePD} for further arguments.
#'
#' @return Feature effect object, either of class "ALE" or "PD".
#' @export
computeFE = function(model, data, feature, fe_method = "ALE",
                     predict_fun = NULL,
                     grid_size = "default", ...) {
  if (is.null(predict_fun)) predict_fun = get_prediction_function(model)
  else assert_function(predict_fun, args = c("object", "newdata"))
  if (fe_method == "ALE") {
    computeALE(model, data, feature,
      predict_fun = predict_fun, grid_size = grid_size, ...)
  } else if (fe_method == "PD") {
    computePD(model, data, feature,
      predict_fun = predict_fun, grid_size = grid_size, ...)
  } else stop("fe_method=", fe_method, " not supported.")
}
