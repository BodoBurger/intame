#' Fit linear models on segments of AL predictions
#'
#' Iteratively add an additional split on the feature space and fit linear models
#' in the resulting segments until stopping criterium is fullfilled.
#'
#' @param x [\code{numeric}]
#' @param f [\code{numeric}]
#' @param metric_name [\code{character(1)}]
#'   Implemented metrics: "WMSR2" (weighted mean squared R squared),
#'                        "WMR2" (weighted mean R squared),
#'                        "WMRSS" (weighted mean of residual sum of squares)
#' @param threshold [\code{numeric(1)}] Stopping criterium.
#' @param max_splits [\code{integer(1)}] Stopping criterium.
#' @param greedy [\code{logical(1)}] If FALSE, consider each possible split
#'   combination in the next step.
#' @param verbose [\code{logical(1)}] Show console output.
#'
#' @return object of class \code{IntamePartition}
#' @export
split_and_fit = function(x, f, metric_name = "WMSR2",
                         threshold = .9, max_splits = 20,
                         greedy = TRUE, verbose = TRUE) {
  assert_numeric(x)
  assert_numeric(f)
  assert(length(x) == length(f))
  assert_choice(metric_name, choices = ImplementedMetrics)
  assert_numeric(threshold, len = 1)
  assert_integerish(max_splits, len = 1)
  l = length(x)
  x_order = order(x)
  x = x[x_order]
  f = f[x_order]
  mod_0 = .lm.fit(cbind(1, x), f)
  opt_metric = extract_metric_part_from_lm(mod_0, f, metric_name)
  opt_models = list(mod_0)
  opt_split = integer(0)
  metrics_history = opt_metric
  splits_history = list()
  n_splits = 0
  if (!compare_metric_values(opt_metric, threshold, metric_name)) {
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

      #x_tmp = vector("list", n_segments)
      #f_tmp = vector("list", n_segments)
      mod_tmp = vector("list", n_segments)
      weights_tmp = numeric(n_segments)
      metric_parts_tmp = numeric(n_segments)

      for (k in 1:n_combinations) {
        split_tmp = split_combinations[, k]
        bounds_tmp = c(1, split_tmp, l)
        for (i in seq_len(n_segments)) {
          indices = bounds_tmp[i]:bounds_tmp[i+1]
          x_tmp = x[indices]
          f_tmp = f[indices]
          mod_tmp[[i]] = .lm.fit(cbind(1, x_tmp), f_tmp)
          weights_tmp[i] = length(x_tmp)
          metric_parts_tmp[i] = extract_metric_part_from_lm(mod_tmp[[i]], f_tmp,
            metric_name)
        }
        metric_tmp = weighted.mean(metric_parts_tmp, weights_tmp)
        if (compare_metric_values(metric_tmp, opt_metric, metric_name)) {
          opt_metric = metric_tmp
          opt_split = split_tmp
          opt_models = mod_tmp
        }
      }
      metrics_history = c(metrics_history, opt_metric)
      splits_history[[n_splits]] = opt_split
      if (compare_metric_values(opt_metric, threshold, metric_name) ||
          n_splits == max_splits) {
        break
      }
      cat("|.")
    }
  }
  if (verbose) {
    cat(" Done.\n")
    cat(format(paste0("Metric (", metric_name, "):"), width = 18, justify = "right"), opt_metric, "\n")
    cat(" Number of splits:", n_splits, "\n")
  }
  structure(list(models = opt_models,
                 metric = opt_metric,
                 splits = opt_split,
                 n_splits = n_splits,
                 metrics_history = metrics_history,
                 splits_history = splits_history,
                 x_org = x, f_org = f,
                 metric_name = metric_name,
                 threshold = threshold,
                 max_splits = max_splits,
                 greedy = greedy),
    class = c("IntamePartition", "list"))
}

#' @export
print.IntamePartition = function(x, ...) {
  if (x$n_splits == 0) {
    cat("--- No splits necessary ---\n")
  } else {
    x_splits_rounded = round(x$x_org[x$splits], digits = 4)
    width = max(sapply(x_splits_rounded, nchar)) + 1
    cat("--- Optimal splits ---\n")
    cat("     #  ", format(1:x$n_splits, width = width), "\n")
    cat("Values: ", format(x_splits_rounded, width = width), "\n")
  }
  cat("\n")
  cat("---", x$metric_name, "final ---\n")
  cat(x$metric, "\n")
}

# TODO show split history
#      Split Reihenfolge Unterschied zwischen greedy and non-greedy
#      show metric history
# summary.IntamePartition = function(object, ...) {
#   metrics_rounded = round(x$metrics, digits = 4)
#   cat("### Intame Partition ###\n")
#   if (x$n_splits == 0) {
#     width = 6
#     cat("     #  ", format(0, width = width), "\n")
#     cat(" Split: ", format(NA, width = width), "\n")
#   } else {
#     x_splits_rounded = round(x$x_org[x$splits], digits = 4)
#     width = max(6, max(sapply(x_splits_rounded, nchar))+1,
#                    max(sapply(metrics_rounded, nchar))+1)
#     cat("     #  ", format(0:x$n_splits, width = width), "\n")
#     cat(" Split: ", format(c(NA, x_splits_rounded), width = width), "\n")
#   }
#   cat("Metric: ", format(metrics_rounded, width = width), "\n")
# }

#' Visualize IntamePartition
#'
#' @param x object of class "IntamePartition
#' @param title [\code{character(1)}] Plot title.
#' @param plot_org_points Show points that were devided into partitions.
#' @param return_data Return data.frame to create individual plots
#' @param ... ignored
#'
#' @return \code{\link[ggplot2]{ggplot}} object (if \code{return_data=FALSE}).
#'
#' @export
plot.IntamePartition = function(x, title = "default",
                                plot_org_points = TRUE,
                                #show_split_numbers = TRUE,
                                return_data = FALSE,
                                ...) {
  assert_class(x, classes = "IntamePartition")
  assert_character(title, len = 1)
  if (title == "default") {
    title = paste0("Partition using ", x$metric_name, ", greedy=", x$greedy)
  }
  assert_logical(plot_org_points, len = 1)
  #assert_logical(show_split_numbers, len = 1)
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
    # if (show_split_numbers) {
    #   p = p + geom_label(mapping=aes(x=x$x_org[x$splits], y=max(gg_data$y),
    #     label=(1:x$n_splits)), size=2, alpha = .8)
    # }
  }
  if (plot_org_points) {
    org_data = data.frame(x = x$x_org, y = x$f_org)
    p = p + geom_line(data = org_data, aes(x = x, y = y), alpha = .3) #+
      #geom_point(data = org_data, aes(x = x, y = y), alpha = .4)
  }
  p + ggtitle(title)
}

splits_plot_points = function(obj) {
  n_splits = obj$n_splits
  x_org = obj$x_org
  f_org = obj$f_org
  l = length(x_org)
  x = numeric(0)
  y = numeric(0)
  id = numeric(0)
  models = obj$models
  if (n_splits == 0) {
    x = x_org
    y = f_org - models[[1]]$residuals
    id = rep(1, length(x))
  } else {
    bounds = c(1, obj$splits, l)
    for (i in seq_len(n_splits+1)) {
      indices = bounds[i]:bounds[i+1]
      x_i = x_org[indices]
      x = c(x, x_i)
      y = c(y, f_org[indices] - models[[i]]$residuals)
      id = c(id, rep(i, length(x_i)))
    }
  }
  return(data.frame(x, y, id))
}
