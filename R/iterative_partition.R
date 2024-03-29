#' Fit linear models on segments of AL predictions
#'
#' Iteratively add an additional split on the feature space and fit linear models
#' in the resulting segments until stopping criterium is fullfilled.
#'
#' @param x [\code{numeric}]
#' @param f [\code{numeric}]
#' @template arg_metric_name_th
#' @param threshold [\code{numeric(1)}] Stopping criterium.
#' @param max_splits [\code{integer(1)}] Stopping criterium.
#' @param greedy [\code{logical(1)}] If FALSE, consider each possible split
#'   combination in the next step.
#' @param verbose [\code{logical(1)}]\cr
#'   Show progress while looking for optimal split.
#'   If \code{NULL} verbose is set \code{getOption("verbose")} (global verbose level).
#' @param ... ignored
#'
#' @return object of class \code{IntamePartition}
#' @export
iterative_partition = function(x, f, metric_name = NULL,
                               threshold = NULL, max_splits = 8L,
                               greedy = FALSE, verbose = NULL, ...) {
  assert_numeric(x)
  assert_numeric(f)
  assert(length(x) == length(f))
  assert_choice(metric_name, choices = ImplementedMetrics, null.ok = TRUE)
  if (test_class(threshold, "IntameThreshold")) {
    if (is.null(metric_name)) metric_name = threshold$metric_name
    else if (metric_name != threshold$metric_name) {
      warning("metric_name is not the same as used for the threshold.
        Set to the threshold metric.")
      metric_name = threshold$metric_name
    }
    threshold = threshold$threshold
  } else assert_numeric(threshold, len = 1, null.ok = TRUE)
  if (is.null(metric_name)) metric_name = ImplementedMetrics[1]
  if (is.null(threshold)) threshold = suggest_threshold(model = NULL, data = NULL,
    features = NULL, metric_name = metric_name, fe = f, ...)$threshold
  assert_integerish(max_splits, len = 1)
  assert_logical(greedy)
  assert_logical(verbose, null.ok = TRUE)
  if (is.null(verbose)) verbose = getOption("verbose")
  l = length(x)
  x_order = order(x)
  x = x[x_order]
  f = f[x_order]
  SST =  sum((f-mean.default(f))^2)
  mod_0 = .lm.fit(cbind(1, x), f)
  part_0 = list(extract_metric_part_from_lm(mod_0$residuals, f, x, metric_name))
  opt_metric = aggregate_metric_parts(part_0, 1, SST, metric_name)
  opt_models = list(mod_0)
  opt_split = integer(0)
  metrics_history = opt_metric
  metrics_history_change = numeric(0)
  splits_history = list()
  n_splits = 0
  if (!compare_metric_values(opt_metric, threshold, metric_name)) {
    if (verbose) cat(".")
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
      metric_parts_tmp = vector("list", n_segments)

      for (k in 1:n_combinations) {
        split_tmp = split_combinations[, k]
        bounds_tmp = c(1, split_tmp, l)
        for (i in seq_len(n_segments)) {
          indices = bounds_tmp[i]:bounds_tmp[i+1]
          x_tmp = x[indices]
          f_tmp = f[indices]
          mod_tmp[[i]] = .lm.fit(cbind(1, x_tmp), f_tmp)
          residuals_tmp = mod_tmp[[i]]$residuals
          weights_tmp[i] = length(x_tmp)
          metric_parts_tmp[[i]] = extract_metric_part_from_lm(residuals_tmp, f_tmp,
            x_tmp, metric_name)
        }
        metric_tmp = aggregate_metric_parts(metric_parts_tmp, weights_tmp, SST,
          metric_name)
        if (compare_metric_values(metric_tmp, opt_metric, metric_name)) {
          opt_metric = metric_tmp
          opt_split = split_tmp
          opt_models = mod_tmp
        }
      }
      metrics_history = c(metrics_history, opt_metric)
      metrics_history_change = c(metrics_history_change, (metrics_history[n_splits+1] -
        metrics_history[n_splits])/metrics_history[n_splits])
      splits_history[[n_splits]] = opt_split
      if (verbose) cat("|.")
      if (compare_metric_values(opt_metric, threshold, metric_name)) break
      if (n_splits == max_splits) {
        warning("Stopped because number of splits == max_splits.")
        break
      }
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
                 metrics_history_change = metrics_history_change,
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
#' @param rugs [\code{logical(1)}]
#' @param return_data Return data.frame to create individual plots
#' @param ... ignored
#'
#' @return \code{\link[ggplot2]{ggplot}} object (if \code{return_data=FALSE}).
#'
#' @export
plot.IntamePartition = function(x, title = "default",
                                #show_split_numbers = TRUE,
                                rugs = TRUE,
                                return_data = FALSE,
                                ...) {
  assert_class(x, classes = "IntamePartition")
  assert_character(title, len = 1)
  if (title == "default") {
    title = paste0("Partition using ", x$metric_name, ", greedy=", x$greedy)
  }
  #assert_logical(show_split_numbers, len = 1)
  assert_logical(return_data, len = 1)
  splits_values = x$x_org[x$splits]
  gg_data = splits_plot_points(x)
  if (return_data) {
    return(gg_data)
  }
  p = ggplot() +
    geom_line(data = gg_data, aes(x = x, y = y, group = id), color = "blue")
  if (x$n_splits > 0) {
    p = p + geom_vline(xintercept = splits_values, linetype = 3, size = .6,
      col = "darkgray", alpha = 1)
    # if (show_split_numbers) {
    #   p = p + geom_label(mapping=aes(x=splits_values, y=max(gg_data$y),
    #     label=(1:x$n_splits)), size=2, alpha = .8)
    # }
  }
  if (rugs) p = p + geom_rug(aes(x = x$x_org), alpha = .3, sides = "b")
  p + geom_line(aes(x = x$x_org, y = x$f_org), alpha = .3) +
    ggtitle(title) +
    scale_x_continuous(breaks = splits_values, minor_breaks = NULL,
      labels = format(splits_values, digits = 3))
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
