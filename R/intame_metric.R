#' constant
ImplementedMetrics = c("WMSR2", "WMR2", "WMRSS")

#' Get metric part from a linear model
#'
#' @param model [\code{\link{stats}[base]}]
#' @param f [\code{numeric}]
#' @param metric_name [\code{character(1)}] Metric name
#'
#' @return [\code{numeric(1)}]
#' @export
#'
#' @examples
#' model = lm.fit(x = cbind(1, mtcars$disp), y = mtcars$mpg)
#' extract_metric_part_from_lm(model, mtcars$disp, "WMSR2")
#' extract_metric_part_from_lm(model, mtcars$disp, "WMRSS")
extract_metric_part_from_lm = function(model, f, metric_name) {
  if (metric_name == "WMRSS") mean.default(model$residuals^2)
  else if (metric_name == "WMR2") {
    1 - sum(model$residuals^2) / sum((f-mean.default(f))^2)
  } else {
    (1 - sum(model$residuals^2) / sum((f-mean.default(f))^2))^2
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
  if (metric_name == "WMRSS") metric < other_metric # smaller is better
  else metric > other_metric
}

