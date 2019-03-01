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
#' @param threshold [\code{numeric(1)}] Stopping criterium.
#' @param max_splits [\code{integer(1)}] Stopping criterium.
#' @param greedy [\code{logical(1)}] If FALSE, consider each possible split
#'   combination in the next step.
#'
#' @return object of class \code{IntamePartition}
#' @export
split_and_fit = function(x, f, method = "WMSR2",
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
  mod_0 = .lm.fit(cbind(1, x), f)
  opt_metric = extract_metric_part_from_lm(mod_0, f, metric_name = method)
  metrics = opt_metric
  opt_models = list(mod_0)
  opt_x = x
  opt_split = integer(0)
  n_splits = 0
  if (!compare_metric_values(opt_metric, threshold, metric_name = method)) {
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
          metric_parts[i] = extract_metric_part_from_lm(mod_tmp[[i]], f_tmp[[i]],
            metric_name = method)
        }
        metric = aggregate_metric_parts(metric_parts, weights_tmp,
          metric_name = method)
        if (compare_metric_values(metric, opt_metric, metric_name = method)) {
          opt_metric = metric
          opt_split = split
          opt_models = mod_tmp
          opt_x = x_tmp
        }
      }
      metrics = c(metrics, opt_metric)
      if (compare_metric_values(opt_metric, threshold, metric_name = method) ||
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
                 max_splits = max_splits,
                 greedy = greedy),
    class = c("IntamePartition", "list"))
}

# TODO Split Reihenfolge Unterschied zwischen greedy and non-greedy
#' @export
print.IntamePartition = function(x, ...) {
  metrics_rounded = round(x$metrics, digits = 4)
  cat("### Intame Partition ###\n")
  if (x$n_splits == 0) {
    width = 6
    cat("     #  ", format(0, width = width), "\n")
    cat(" Split: ", format(NA, width = width), "\n")
  } else {
    x_splits_rounded = round(x$x_org[x$splits], digits = 4)
    width = max(6, max(sapply(x_splits_rounded, nchar))+1,
                   max(sapply(metrics_rounded, nchar))+1)
    cat("     #  ", format(0:x$n_splits, width = width), "\n")
    cat(" Split: ", format(c(NA, x_splits_rounded), width = width), "\n")
  }
  cat("Metric: ", format(metrics_rounded, width = width), "\n")
}

#' Visualize IntamePartition NEEDS TO BE FIXED
#'
#' @param x object of class "IntamePartition
#' @param title [\code{character(1)}] Plot title.
#' @param plot_org_points Show points that were devided into partitions.
#' @param show_split_numbers Show label for each split.
#' @param return_data Return data.frame to create individual plots
#' @param ... ignored
#'
#' @return \code{\link[ggplot2]{ggplot}} object (if \code{return_data=FALSE}).
#'
#' @export
plot.IntamePartition = function(x, title = "default",
                                plot_org_points = TRUE,
                                show_split_numbers = TRUE,
                                return_data = FALSE,
                                ...) {
  assert_class(x, classes = "IntamePartition")
  assert_character(title, len = 1)
  if (title == "default") title = paste("Partition using", x$method)
  assert_logical(plot_org_points, len = 1)
  assert_logical(show_split_numbers, len = 1)
  assert_logical(return_data, len = 1)
  gg_data = splits_plot_points(x)
  if (return_data) {
    return(gg_data)
  }
  p = ggplot() +
    geom_line(data = gg_data, aes(x = x, y = y, group = id), color = "blue")
  if (x$n_splits > 0) {
    p = p + geom_vline(xintercept = x$x_org[x$splits], linetype = 3, size = .6,
      col = "darkgray", alpha = 1)
    if (show_split_numbers) {
      p = p + geom_label(mapping=aes(x=x$x_org[x$splits], y=max(gg_data$y),
        label=(1:x$n_splits)), size=2, alpha = .8)
    }
  }
  if (plot_org_points) {
    org_data = data.frame(x = x$x_org, y = x$f_org)
    p = p + geom_line(data = org_data, aes(x = x, y = y), alpha = .3) #+
      #geom_point(data = org_data, aes(x = x, y = y), alpha = .4)
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
