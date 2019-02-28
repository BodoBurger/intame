# further optimizations:
#   - remove S3 system
#   - fit lm with qr function

#' Fit linear models on segments of AL predictions
#'
#' Iteratively add an additional split on the feature space and fit linear models
#' in the resulting segments until stopping criterium is fullfilled.
#'
#' @param x [\code{numeric}]
#' @param f [\code{numeric}]
#' @param method [\code{character(1)}]
#'   Implemented methods: "WMSR2" (weighted mean squared R squared),
#'                        "WMR2" (weighted mean R squared),
#'                        "WMRSS" (weighted mean of residual sum of squares)
#' @param threshold [\code{numeric}] Stopping criterium.
#' @param max_splits [\code{integer}] Stopping criterium.
#'
#' @return object of class \code{IntamePartition}
#' @export
split_and_fit_optimized = function(x, f, method = "WMSR2",
                         threshold = .9, max_splits = 20,
                         greedy = TRUE) {
  assert_numeric(x)
  assert_numeric(f)
  assert(length(x) == length(f))
  assert_choice(method, choices = ImplementedMetrics)
  assert_numeric(threshold, len = 1)
  assert_integerish(max_splits, len = 1)
  l = length(x)
  x_order = order(x)
  x = x[x_order]
  f = f[x_order]
  threshold = new_metric(method, threshold)
  mod_0 = .lm.fit(cbind(1, x), f)
  opt_metric = new_metric(method,
    extract_metric_part_from_lm_optimized(new_metric(method), mod_0, f))
  metrics = opt_metric
  opt_models = list(mod_0)
  opt_x = x
  opt_split = integer(0)
  n_splits = 0
  if (!compare_metric_values(opt_metric, threshold)) {
    cat(".")
    while(TRUE) {
      n_splits = n_splits + 1
      n_segments = n_splits + 1
      if (greedy) {
        split_combinations = rbind(
          matrix(opt_split, nrow = n_splits - 1, ncol = l-1-n_splits),
          (2:(l-1))[!(2:(l-1) %in% opt_split)])
        if (n_splits > 1) split_combinations = apply(split_combinations, 2, sort.int)
      } else {
        split_combinations = combn(2:(l-1), n_splits)
      }
      n_combinations = ncol(split_combinations)
      #opt_split = sort.int(split_combinations[, 1])

      x_tmp = vector("list", n_segments)
      f_tmp = vector("list", n_segments)
      mod_tmp = vector("list", n_segments)
      weights_tmp = numeric(n_segments)
      metric_parts = numeric(n_segments)

      for (k in 1:n_combinations) {
        split = split_combinations[, k]
        bounds = c(1, split, l)
        for (i in seq_len(n_segments)) {
          indices = bounds[i]:bounds[i+1]
          x_tmp[[i]] = x[indices]
          f_tmp[[i]] = f[indices]
          mod_tmp[[i]] = .lm.fit(cbind(1, x_tmp[[i]]), f_tmp[[i]])
          weights_tmp[i] = length(x_tmp[[i]])
          metric_parts[i] = extract_metric_part_from_lm_optimized(
            opt_metric, mod_tmp[[i]], f_tmp[[i]])
        }
        metric = aggregate_metric_parts_optimized(opt_metric, metric_parts, weights_tmp)
        if (compare_metric_values(metric, opt_metric)) {
          opt_metric = metric
          opt_split = split
          opt_models = mod_tmp
          opt_x = x_tmp
        }
      }
      metrics = c(metrics, opt_metric)
      if (compare_metric_values(opt_metric, threshold) ||
          n_splits == max_splits) {
        cat(" Done.\n")
        cat(format(paste0("Metric (", method, "):"), width = 18, justify = "right"), opt_metric, "\n")
        cat(" Number of splits:", n_splits, "\n")
        break
      }
      cat("|.")
    }
    while (FALSE) { # non-greedy
      weights_tmp = numeric(n_segments)
      split_combinations = combn(2:(l-1), n_splits)
      n_combinations = ncol(split_combinations)
      opt_split = split_combinations[, 1]
      for (k in 1:n_combinations) {
        bounds = sort.int(c(1, l, split_combinations[, k]))
        for (i in seq_len(n_segments)) {
          x_tmp[[i]] = x[bounds[i]:bounds[i+1]]
          f_tmp[[i]] = f[bounds[i]:bounds[i+1]]
          mod_tmp[[i]] = lm(f_tmp[[i]] ~ x_tmp[[i]], x = TRUE)
          weights_tmp[i] = length(x_tmp[[i]])
        }
        metric = aggregate_metric_parts(opt_metric, mod_tmp, weights_tmp)
        if (compare_metric_values(metric, opt_metric)) {
          opt_metric = metric
          opt_split = split_combinations[, k]
          opt_models = mod_tmp
        }
      }
      metrics = c(metrics, opt_metric)
      if (compare_metric_values(opt_metric, threshold) ||
          n_splits == max_splits) {
        cat(" Done.\n")
        cat(format(paste0("Metric (", method, "):"), width = 18, justify = "right"), opt_metric, "\n")
        cat(" Number of splits:", n_splits, "\n")
        break
      }
      cat("|.")
    }
  }
  structure(list(models = opt_models,
                 splits = opt_split,
                 n_splits = n_splits,
                 metrics = metrics,
                 x_org = x, f_org = f,
                 method = method,
                 threshold = threshold,
                 max_splits = max_splits),
    class = c("IntamePartition", "list"))
}
