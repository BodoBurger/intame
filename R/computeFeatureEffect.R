#' Wrapper for computeALE and computePD
#'
#' Calls either \link{computeALE} or \link{computePD} depending on
#' \code{fe_method}.
#'
#' @param fe_method Either "ALE" or "PD".
#' @param model Fitted model object.
#' @param data Date that was used to fit the model.
#' @param feature [\code{character}] Name of the feature.
#' @param ... Passed on to the FE method. See \link{computeALE} or \link{computePD}
#'            for possible arguments.
#'
#' @return Feature effect object, either of class "ALE" or "PD".
#' @export
computeFeatureEffect = function(fe_method, model, data, feature, ...) {
  if (fe_method == "ALE") {
    computeALE(model, data, feature, ...)
  } else if (fe_method == "PD") {
    computePD(model, data, feature, ...)
  } else stop("fe_method=", fe_method, " not supported.")
}
