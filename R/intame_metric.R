#' constant
ImplementedMetrics = c("WMSR2", "WMR2", "L2", "L1", "Frechet")
# TODOS New Metrics:
# - RSS(i) / RSS(i-1)


#' Find sensible threshold depending on the metric and data
#'
#' @param x [\code{numeric}]
#' @param f [\code{numeric}]
#' @param metric_name [\code{character(1)}] Metric name
#'
#' @return [\code{numeric(1)}]
#' @export
calculate_threshold = function(x, f, metric_name) {

}

#' Get metric part from a linear model
#'
#' @param residuals [\code{numeric}]
#' @param f [\code{numeric}]
#' @param x [\code{numeric}]
#' @param metric_name [\code{character(1)}] Metric name
#'
#' @return [\code{numeric(1)}]
#' @export
#'
#' @examples
#' residuals = lm.fit(x = cbind(1, mtcars$disp), y = mtcars$mpg)$residuals
#' extract_metric_part_from_lm(residuals, mtcars$disp, mtcars$mpg, "WMSR2")
#' extract_metric_part_from_lm(residuals, mtcars$disp, mtcars$mpg, "L2")
extract_metric_part_from_lm = function(residuals, f, x, metric_name) {
  if (metric_name == "L2") {
    mean.default(residuals^2)
  } else if (metric_name == "L1") {
    mean.default(abs(residuals))
  } else if (metric_name == "WMR2") {
    1 - sum(residuals^2) / sum((f-mean.default(f))^2)
  } else if (metric_name == "WMSR2") {
    (1 - sum(residuals^2) / sum((f-mean.default(f))^2))^2
  } else if (metric_name == "Frechet") {
    f_hat = f - residuals
    matrix(c(x, f, f_hat), ncol = 3)
  } else {
    stop("Unknown metric.")
  }
}

#' Aggregate parts of all intervals
#'
#' @param parts Output of extract_metric_part_from_lm()
#' @param weights Number of points in interval
#' @param metric_name [\code{character(1)}] Metric name
#'
#' @return [\code{numeric(1)}]
#' @export
aggregate_metric_parts = function(parts, weights, metric_name) {
  if (metric_name == "Frechet") {
    paths = matrix(ncol = 3, nrow = 0)
    for (part in parts) paths = rbind(paths, part)
    Frechet(paths[,-3], paths[,-2])
  } else {
    weighted.mean(unlist(parts), weights)
  }
}

#' Check whether the value of \code{metric} is "better" than \code{other_metric}.
#'
#' @param metric [\code{numeric(1)}]
#' @param other_metric [\code{numeric(1)}]
#' @param metric_name [\code{character(1)}] Metric name
#'
#' @return [\code{logical(1)}]
#' @export
compare_metric_values = function(metric, other_metric, metric_name) {
  #assert_choice(method)
  switch(metric_name,
    L2 = metric < other_metric,
    L1 = metric < other_metric,
    Frechet = metric < other_metric,
    metric > other_metric)
}

