#' constant
#' First element is taken as default metric
ImplementedMetrics = c("L2", "L1", "R2", "R2int", "Frechet")

compute_sst = function(x) sum((x-mean.default(x))^2)
compute_sat = function(x) sum(abs(x-mean.default(x)))

#' Get metric part from a linear model
#'
#' @param residuals [\code{numeric}]
#' @param f [\code{numeric}]
#' @param x [\code{numeric}]
#' @template arg_metric_name
#'
#' @return [\code{numeric(1)}]
#' @export
#'
#' @examples
#' residuals = lm.fit(x = cbind(1, mtcars$disp), y = mtcars$mpg)$residuals
#' extract_metric_part_from_lm(residuals, mtcars$disp, mtcars$mpg, "R2int")
#' extract_metric_part_from_lm(residuals, mtcars$disp, mtcars$mpg, "L2")
extract_metric_part_from_lm = function(residuals, f, x, metric_name) {
  if (metric_name == "L2") {
    sum(residuals^2)
  } else if (metric_name == "L1") {
    sum(abs(residuals))
  } else if (metric_name == "R2int") {
    denom_tmp = sum((f-mean.default(f))^2)
    if (denom_tmp == 0) {
      1
    } else {
      1 - sum(residuals^2) / denom_tmp
    }
  } else if (metric_name == "R2") {
    sum(residuals^2)
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
#' @param SST Sum of squared total
#' @template arg_metric_name
#'
#' @return [\code{numeric(1)}]
#' @export
aggregate_metric_parts = function(parts, weights, SST, metric_name) {
  if (metric_name == "L2" | metric_name == "L1") {
    sum(unlist(parts))
  } else if (metric_name == "R2int") {
    weighted.mean(unlist(parts), weights)
  } else if (metric_name == "R2") {
    1 - sum(unlist(parts)) / SST
  } else if (metric_name == "Frechet") {
    paths = matrix(ncol = 3, nrow = 0)
    for (part in parts) paths = rbind(paths, part)
    Frechet(paths[,-3], paths[,-2])
  } else {
    stop("Unknown metric.")
  }
}

#' Check whether the value of \code{metric} is "better" than \code{other_metric}.
#'
#' @param metric [\code{numeric(1)}]
#' @param other_metric [\code{numeric(1)}]
#' @template arg_metric_name
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
