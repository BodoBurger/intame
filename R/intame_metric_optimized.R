############################
# IntameMetric S3 generics #

#' Get metric part from a linear model
#'
#' @param metric object of a subclass of \code{IntameMetric}
#' @param model [\code{\link{stats}[base]}]
#'
#' @return [\code{numeric}]
#' @export
#'
#' @examples
#' model = lm(mpg ~ disp, mtcars)
#' extract_metric_part_from_lm(new_metric("WMSR2"), model)
extract_metric_part_from_lm_optimized = function(metric, model, f) {
  #assert_class(metric, "IntameMetric")
  #assert_class(model, "lm")
  UseMethod("extract_metric_part_from_lm_optimized")
}

#' Aggregate metric parts
#'
#' @param metric object of a subclass of \code{IntameMetric}
#' @param models [\code{list}] list of \code{lm} objects
#' @param weights [\code{numeric}]
#'
#' @return same class as \code{metric}
#' @export
aggregate_metric_parts_optimized = function(metric, parts, weights) {
  #assert_class(metric, "IntameMetric")
  UseMethod("aggregate_metric_parts_optimized")
}

##############################################-#
# R2 (R squared), subclasses WMSR2 and WMR2 ####

#' @export
extract_metric_part_from_lm_optimized.R2 = function(metric, model, f) {
  1 - sum(model$residuals^2) / sum((f-mean.default(f))^2)
}

# WMSR2 (weighted mean of squared R squared)
#' @export
aggregate_metric_parts_optimized.WMSR2 = function(metric, parts, weights) {
  structure(weighted.mean(parts^2, weights), class = class(metric))
}

# WMR2 (weighted mean of R squared)
#' @export
aggregate_metric_parts_optimized.WMR2 = function(metric, parts, weights) {
  structure(weighted.mean(parts, weights), class = class(metric))
}

#####################################################-#
# WMRSS (weighted mean of residual sum of squares) ####

#' @export
extract_metric_part_from_lm_optimized.WMRSS = function(metric, model, f) {
  mean.default(model$residuals^2)
}

#' @export
aggregate_metric_parts_optimized.WMRSS = function(metric, parts, weights) {
  structure(weighted.mean(parts, weights), class = class(metric))
}
