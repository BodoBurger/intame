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
  if (is.null(features)) features = "x"
  else assert(all(features %in% colnames(data)))
  assert_choice(metric_name, choices = ImplementedMetrics)
  assert_choice(fe_method, choices = c("ALE", "PD"))
  assert_numeric(explained_fraction, lower = 1e-05, upper = 1 - 1e-05)
  if(is.null(model)) assert_numeric(fe)
  else assert_class(fe, "IntameFeatureEffect", null.ok = TRUE)
  assert_numeric(explained_fraction, lower = 0, upper = 1, len = 1)

  if (metric_name %in% c("R2", "R2int")) {
    all_thresholds = setNames(rep(explained_fraction, length(features)), features)
    threshold = explained_fraction
  } else if (metric_name %in% c("L2", "L1")) {
    if (metric_name == "L2") compute_st = compute_sst
    else compute_st = compute_sat
    if (is.null(fe)) {
      fe = vector(mode = "list", length(features))
      STs = numeric(length(features))
      for (i in seq_along(features)) {
        fe[[i]] = computeFE(model, data, features[i], fe_method, ...)
        STs[i] = compute_st(fe[[i]]$fp_f)
      }
    } else {
      if (test_class(fe, "IntameFeatureEffect")) STs = compute_st(fe$fp_f)
      else STs = compute_st(fe)
    }
    var_fraction = 1 - explained_fraction # e.g. 0.05 corresponds to R squared of 95%
    all_thresholds = setNames(var_fraction * STs, features)
    threshold = max(all_thresholds)
  }
  structure(list(threshold = threshold,
    all_thresholds = all_thresholds,
    fe = fe, fe_method = fe_method,
    metric_name = metric_name,
    explained_fraction = explained_fraction),
    class = c("IntameThreshold", "list"))
}

#' @export
print.IntameThreshold = function(x, ...) {
  cat("Threshold: ", x$threshold, " (metric: ", x$metric_name, ")\n", sep = "")
  if (length(x$all_thresholds) > 1) {
    cat("---\n")
    print.default(x$all_thresholds)
  }
}
