#' constructor for S3 class "IntameMetric"
#'
#' @param name [\code{character(1)}] Name of (sub-)class.
#'   Currently implemented: WMSR2, WMR2.
#' @param x [\code{numeric}] Metric value(s).
#'
#' @return object of class "IntameMetric"
#' @export
#'
#' @examples
#' new_metric("WMSR2", .5)
new_metric = function(name, x = numeric()) {
  if (name == "WMSR2" || name == "WMR2") name = c(name, "R2")
  structure(x, class = c(name, "IntameMetric", "numeric"))
}

#' @export
print.IntameMetric = function(x, ...) {
  print(unclass(x))
}

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
extract_metric_part_from_lm = function(metric, model) {
  UseMethod("extract_metric_part_from_lm")
}

#' Check whether the value of \code{metric} is "better" than \code{other_metric}.
#'
#' @param metric object of a subclass of \code{IntameMetric}
#' @param other_metric same class as \code{metric}
#'
#' @return [\code{logical(1)}]
#' @export
compare_metric_values = function(metric, other_metric) {
  UseMethod("compare_metric_values")
}

#' Aggregate metric parts
#'
#' @param metric object of a subclass of \code{IntameMetric}
#' @param models [\code{list}] list of \code{lm} objects
#' @param weights [\code{numeric}]
#'
#' @return same class as \code{metric}
#' @export
aggregate_metric_parts = function(metric, models, weights) {
  UseMethod("aggregate_metric_parts")
}

##############################################-#
# R2 (R squared), subclasses WMSR2 and WMR2 ####

#' @export
extract_metric_part_from_lm.R2 = function(metric, model) {
  summary(model)$r.squared
}

#' @export
compare_metric_values.R2 = function(metric, other_metric) {
  metric > other_metric
}

# WMSR2 (weighted mean of squared R squared)
#' @export
aggregate_metric_parts.WMSR2 = function(metric, models, weights) {
  structure(weighted.mean(vapply(models, function(model, metric)
    extract_metric_part_from_lm(metric, model), numeric(1), metric = metric)^2,
    weights), class = class(metric))
}

# WMR2 (weighted mean of R squared)
#' @export
aggregate_metric_parts.WMR2 = function(metric, models, weights) {
  structure(weighted.mean(vapply(models, function(model, metric)
    extract_metric_part_from_lm(metric, model), numeric(1), metric = metric),
    weights), class = class(metric))
}

#####################################################-#
# WMRSS (weighted mean of residual sum of squares) ####

#' @export
extract_metric_part_from_lm.WMRSS = function(metric, model) {
  mean(model$residuals^2)
}

#' @export
compare_metric_values.WMRSS = function(metric, other_metric) {
  metric < other_metric # smaller is better
}

#' @export
aggregate_metric_parts.WMRSS = function(metric, models, weights) {
  structure(weighted.mean(vapply(models, function(model, metric)
    extract_metric_part_from_lm(metric, model), numeric(1), metric = metric),
    weights), class = class(metric))
}
