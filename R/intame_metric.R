#' constructor for S3 class "IntameMetric"
#'
#' @param name [\code{character(1)}] Name of (sub-)class.
#'   Currently implemented: WMSRS, WMRS.
#' @param x [\code{numeric}] Metric value(s).
#'
#' @return object of class "IntameMetric"
#' @export
#'
#' @examples
#' new_metric("WMSRS", .5)
new_metric = function(name, x = numeric()) {
  if (name == "WMSRS" || name == "WMRS") name = c(name, "RS")
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
#' extract_metric_part_from_lm(new_metric("WMSRS"), model)
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

#############################################
# RS (R squared), subclasses WMSRS and WMRS #

#' @export
extract_metric_part_from_lm.RS = function(metric, model) {
  summary(model)$r.squared
}

#' @export
compare_metric_values.RS = function(metric, other_metric) {
  metric > other_metric
}

# WMSRS (weighted mean of squared R squared)
#' @export
aggregate_metric_parts.WMSRS = function(metric, models, weights) {
  structure(weighted.mean(vapply(models, function(model, metric)
    extract_metric_part_from_lm(metric, model), numeric(1), metric = metric)^2,
    weights), class = class(metric))
}

# WMRS (weighted mean of R squared)
#' @export
aggregate_metric_parts.WMRS = function(metric, models, weights) {
  structure(weighted.mean(vapply(models, function(model, metric)
    extract_metric_part_from_lm(metric, model), numeric(1), metric = metric),
    weights), class = class(metric))
}
