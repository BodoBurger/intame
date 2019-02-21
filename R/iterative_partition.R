#' Fit linear models on segments of AL predictions
#'
#' Iteratively add an additional split on the feature space and fit linear models
#' in the resulting segments until stopping criterium is fullfilled.
#'
#' @param x [\code{numeric}]
#' @param f [\code{numeric}]
#' @param method [\code{character(1)}]
#'   Implemented methods: "WMSRS" (weighted mean squared R squared),
#'                        "WMRS" (weighted mean R squared).
#' @param threshold [\code{numeric}] Stopping criterium.
#' @param max_splits [\code{integer}] Stopping criterium.
#'
#' @return object of class \code{IntamePartition}
#' @export
split_and_fit = function(x, f, method = "WMSRS", threshold = .95, max_splits = 10) {
  threshold = new_metric(method, threshold)
  l = length(x)
  splits = integer(0)
  mod_0 = lm(f ~ x, x = TRUE)
  opt_metric = new_metric(method,
    extract_metric_part_from_lm(new_metric(method), mod_0))
  metrics = opt_metric
  opt_models = list(mod_0)
  if (compare_metric_values(opt_metric, threshold)) {
  } else {
    while(TRUE) {
      n_segments = length(splits) + 2
      splits_remaining = (2:(l-1))[!(2:(l-1) %in% splits)] # all remaining split.points
      metrics_new_split = new_metric(method, numeric(length(splits_remaining)))
      names(metrics_new_split) = splits_remaining
      opt_split = splits_remaining[1]
      for (split in splits_remaining) {
        x_tmp = vector("list", n_segments)
        f_tmp = vector("list", n_segments)
        mod_tmp = vector("list", n_segments)
        weights = numeric(n_segments)
        bounds = sort(c(1, l, splits, split))
        for (i in seq_len(n_segments)) {
          x_tmp[[i]] = x[bounds[i]:bounds[i+1]]
          f_tmp[[i]] = f[bounds[i]:bounds[i+1]]
          mod_tmp[[i]] = lm(f_tmp[[i]] ~ x_tmp[[i]], x = TRUE)
          weights[i] = length(x_tmp[[i]])
        }
        metric = aggregate_metric_parts(opt_metric, mod_tmp, weights)
        metrics_new_split[as.character(split)] = metric
        if (compare_metric_values(metric, opt_metric)) {
          opt_metric = metric
          opt_split = split
          opt_models = mod_tmp
        }
      }
      splits = c(splits, opt_split)
      metrics = c(metrics, opt_metric)
      if (compare_metric_values(opt_metric, threshold) ||
          length(splits) == max_splits) {
        print("Enough splits.")
        break
      }
    }
  }
  structure(list(models = opt_models,
                 splits = splits,
                 metrics = metrics,
                 x_org = x, f_org = f,
                 method = method,
                 threshold = threshold,
                 max_splits = max_splits),
    class = "IntamePartition")
}

#' @export
plot.IntamePartition = function(x, title = "IntamePartition",
                                plot_org_points = TRUE,
                                return_data = FALSE,
                                ...) {
  gg_data = splits_plot_points(x)
  if (return_data) {
    return(gg_data)
  }
  p = ggplot() +
    geom_line(data = gg_data, aes(x = x, y = y, group = id), color = "blue")
  if (length(x$splits) > 0) {
    p = p + geom_vline(xintercept = x$x_org[x$splits], linetype = 3, size = .4)
  }
  if (plot_org_points) {
    org_data = data.frame(x = x$x_org, y = x$f_org)
    p = p + geom_line(data = org_data, aes(x = x, y = y)) +
      geom_point(data = org_data, aes(x = x, y = y))
  }
  p + ggtitle(title)
}

splits_plot_points = function(partition) {
  x = numeric(0)
  y = numeric(0)
  id = numeric(0)
  models = partition$models
  if (length(models) == 1) {
    x = models[[1]]$x[,2]
    y = models[[1]]$fitted.values
    id = rep(1, length(x))
  } else {
    for (i in seq_along(models)) {
      x_i = models[[i]]$x[,2]
      x = c(x, x_i)
      y = c(y, models[[i]]$fitted.values)
      id = c(id, rep(i, length(x_i)))
    }
  }
  return(data.frame(x, y, id))
}
