#' constant
ImplementedMetrics = c("R2", "R2int", "L2", "L1", "Frechet")
# TODOS New Metrics:
# - RSS(i) / RSS(i-1)

compute_sst = function(x) sum((x-mean.default(x))^2)

#' Find sensible threshold depending on the metric and data
#'
#' @template arg_model
#' @template arg_data
#' @param features [\code{character}]
#'   The names of all features that should be considered when looking for a threshold.
#' @template arg_metric_name
#' @param fe_method [\code{character(1)}]
#' @param fe [\code{list}] List containing a feature effect object for each feature.
#'   Can be provided if it was already computed.
#' @param explained_fraction [\code{numeric(1)}]
#' @param ... Passed to feature effect method (ALE or PD).
#'
#' @return [\code{numeric(1)}]
#' @export
suggest_threshold = function(model, data, features,
                             metric_name,
                             fe_method = "ALE", fe = NULL,
                             explained_fraction = .95,
                             ...) {
  assert(all(features %in% colnames(data)))
  assert_choice(metric_name, choices = ImplementedMetrics)
  assert_choice(fe_method, choices = c("ALE", "PD"))
  assert_numeric(explained_fraction, lower = 1e-05, upper = 1 - 1e-05)
  if(!is.null(fe)) assert_class(fe, "IntameFeatureEffect")

  if (metric_name %in% c("R2", "R2int"))
    threshold = explained_fraction
  else if (metric_name %in% c("L2", "L1")) {
    if (is.null(fe)) {
      fe = vector(mode = "list", length(features))
      SSTs = numeric(length(features))
      for (i in seq_along(features)) {
        fe[[i]] = computeFeatureEffect(fe_method, model, data, features[i], ...)
        SSTs[i] = compute_sst(fe[[i]]$fp_f)
      }
    } else {
      SSTs = compute_sst(fe$fp_f)
    }
    var_fraction = 1 - explained_fraction # e.g. 0.05 corresponds to R squared of 95%
    threshold = var_fraction * max(SSTs)
  }
  list(threshold = threshold,
       fe = fe)
}

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
