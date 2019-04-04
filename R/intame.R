#' Compute interval-based marginal effects
#'
#' Compute AME given break points on the sample space of a feature.
#'
#' @template arg_model
#' @template arg_data
#' @param feature [\code{character(1)}]\cr
#'   Feature name, subset of \code{colnames(data)}.
#' @template arg_predict_fun
#' @template arg_metric_name_th
#' @param threshold [\code{numeric(1)}]\cr
#'   Stopping criterium. See \link[intame]{suggest_threshold} for more details
#'   and explanation of default values.
#' @param max_splits [\code{integer(1)}]\cr
#'   Number of intervals.
#' @param fe_method [\code{character(1)}]\cr
#'   Method for calculating fine-granulated marginal effects.
#'   Based on these values, the algorithm determines intervals of similar
#'   marginal effects.\cr
#'   "ALE": Accumulated Local Effect\cr
#'   "PD": Partial Derivative
#' @param fe_grid_size [\code{integer(1)}]\cr
#'   Number of intervals/segments. Same as parameter K in ALEPlot package.
#' @param x_splits [\code{numeric}]\cr
#'   Define interval limits manually. If given, search for optimal split points
#'   skipped.
#' @param output_method [\code{character(1)}]\cr
#'   "lm": Report slope of linear model in each interval.
#'   "ALE": Recalculate ALE with "optimal" intervals.
#'   "AME": Calculate "classic" Average Marginal Effects for each interval.
#' @param use_iter_algo [\code{logical(1)}] (recommended)
#' @param ... Arguments passed on to other functions: computeALE, computePD
#'
#' @return
#'   Object of \code{\link[base]{class}} "intame" which is a list containning
#'   the following components:
#'   \describe{
#'     \item{AME}{named vector}
#'     \item{fp_x}{text-2}
#'     \item{fp_f}{text-2}
#'   }
#'
#' @export
#'
#' @examples
#' library(nnet)
#' set.seed(4219)
#' n = 200
#' x = runif(n, min = 0, max = 1)
#' x1 = runif(n, min=0, max=1) + .5*x
#' x2 = runif(n, min=0, max=1) + .5*x
#' y2 = function(x) -4 * cos(4*pi*x) * x + 4
#' y = 4*x1 + y2(x2) + rnorm(n, 0, .5)
#' df = data.frame(y, x1, x2)
#' nnet.mod = nnet(y ~ ., df, size = 20, linout = TRUE,
#'   decay = 0.0001, maxit = 1000, trace = FALSE)
#' AME.x1 = intame(nnet.mod, df, "x1")
#' AME.x1
#' plot(AME.x1)
#' AME.x2 = intame(nnet.mod, df, "x2", metric_name = "R2", threshold = .9,
#'   fe_grid_size = 25, greedy = TRUE)
#' AME.x2
#' plot(AME.x2)
intame = function(model, data, feature,
                  predict_fun = NULL,
                  metric_name = NULL, threshold = NULL, max_splits = 10L,
                  fe_method = "ALE", fe_grid_size = "default",
                  x_splits = NULL,
                  output_method = "lm", use_iter_algo = TRUE,
                  ...) {
  assert_choice(feature, colnames(data))
  if (is.null(predict_fun)) predict_fun = get_prediction_function(model)
  else assert_function(predict_fun, args = c("object", "newdata"))
  assert_integerish(max_splits, lower = 2, any.missing = FALSE, max.len = 1)
  assert_choice(fe_method, c("ALE", "PD"))
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

  FE = computeFE(model, data, feature, fe_method = fe_method,
    predict_fun = predict_fun, grid_size = fe_grid_size, ...)
  fp_x = FE$fp_x
  fp_f = FE$fp_f
  fe_x = FE$fe_x
  fe_f = FE$fe_f
  if (is.null(x_splits)) {
    if (use_iter_algo) {
      if (is.null(threshold)) {
        threshold = suggest_threshold(model, data, feature, metric_name,
          fe_method, fe = FE, ...)$threshold
      }
      intame_partition = iterative_partition(fp_x, fp_f, metric_name, threshold, max_splits, ...)
      x_splits = fp_x[intame_partition$splits]
    } else {
      intame_partition = NULL
      x_splits = partition(fe_x, fe_f, max_splits)
    }
  } else {
    intame_partition = NULL
  }

  x = data[, feature]
  bounds = c(min(x), sort(x_splits), max(x))
  #if (length(bounds)-1 < n_intervals) message("Found less than ", n_intervals, " n_intervals.")
  n_intervals = length(bounds) - 1
  AME = numeric(n_intervals) # slope
  y.hat.mean = numeric(n_intervals) # ordinate
  x.interval.average = numeric(n_intervals) # abscissa
  #if (is.null(intame_partition)) use_AME = TRUE
  ALEint = NULL
  if (output_method == "lm") {
    for (i in 1:n_intervals) {
      coefficients_interval = intame_partition$models[[i]]$coefficients
      AME[i] = coefficients_interval[2]
      x.interval.average[i] = (bounds[i] + bounds[i+1])/2
      y.hat.mean[i] = x.interval.average[i] * AME[i] + coefficients_interval[1]
    }
  } else if (output_method == "ALE") {
    ALEint = computeFE(model = model, data = data, feature = feature,
      fe_method = "ALE",
      predict_fun = predict_fun, grid_size = fe_grid_size,
      grid_breaks = x_splits, ...)
    AME = ALEint$fe_f
    x.interval.average = ALEint$fe_x
    y.hat.mean = (ALEint$fp_f[1:n_intervals] + ALEint$fp_f[2:(n_intervals+1)]) / 2
  } else if (output_method == "AME") {
    y.hat = predict_fun(model, data)
    bounds[n_intervals+1] = bounds[n_intervals+1] + 0.000001
    for (i in 1:n_intervals) {
      selection = x >= bounds[i] & x < bounds[i+1]
      data.interval = data[selection,]
      AME[i] = ame::computeAME(model, data.interval, feature,
        predict.fun = predict_fun)[, feature]
      y.hat.mean[i] = mean(y.hat[selection])
      x.interval.average[i] = mean(x[selection])
    }
  } else stop("output_method=", output_method, " not supported.")

  bounds.rounded = round(bounds, digits = 3)
  interval.desc = character(n_intervals)
  interval.desc[n_intervals] = paste0("[", bounds.rounded[n_intervals], ", ",
    bounds.rounded[n_intervals+1], "]")
  for (i in 1:(n_intervals-1)) {
    interval.desc[i] = paste0("[", bounds.rounded[i], ", ", bounds.rounded[i+1], ")")
  }
  return(structure(list(AME = setNames(AME, interval.desc),
                        intame_partition = intame_partition,
                        bounds = bounds,
                        x_splits = x_splits,
                        feature = feature,
                        y.hat.mean = y.hat.mean,
                        x.interval.average = x.interval.average,
                        x = x,
                        fp_x = fp_x, fp_f = fp_f,
                        fe_x = fe_x, fe_f = fe_f,
                        metric_name = metric_name,
                        FE = FE, fe_method = fe_method,
                        output_method = output_method, ALEint = ALEint),
                   class = "Intame",
                   comment = "Main class of intame package."))
}

#' @export
print.Intame = function(x, ...) {
  print(x$AME)
}

#' @export
summary.Intame = function(object, ...) {
  cat("# Interval-based Marginal Effects for", object$feature, "#\n")
  cat("Output method: ", object$output_method, "\n")
  cat("---\n")
  cat(format("Interval", width = 14, justify = "right"),
    " | Average Marginal Effect\n", sep = "")
  for (i in seq_along(object$AME)) {
    cat(format(names(object$AME)[i], width = 14, justify = "right"),
      " | ", object$AME[i], "\n", sep = "")
  }
  cat("---\n")
  cat("Number of intervals: ", length(object$AME), "\n")
  if(!is.null(object$intame_partition)) {
    ip = object$intame_partition
    cat("---\n")
    cat("Metric used to evaluate splits: ", ip$metric_name, "\n")
    cat("Metric history: ", ip$metrics_history, "\n")
    cat("Metric change when split added: ", ip$metrics_history_change, "\n")
  }
}

#' Visualize the result of intame
#'
#' @param x [\code{intame}]
#' @param title [\code{character(1)}] Plot title
#' @param show_slopes [\code{logical(1)}]
#' @param rugs [\code{logical(1)}]
#' @param ... ignored
#'
#' @export
plot.Intame = function(x, title = "default",
                       show_slopes = TRUE, rugs = TRUE, ...) {
  assert_character(title, len = 1)
  assert_logical(show_slopes, len = 1)
  assert_logical(rugs, len = 1)
  if (title == "default") {
    title = paste0("Intame using ", x$fe_method, ", metric: ", x$metric_name,
      ", output: ", x$output_method)
  }
  AME = x$AME
  x.0 = x$x.interval.average
  y.0 = x$y.hat.mean
  bounds = x$bounds
  p = ggplot() +
    geom_line(mapping = aes(x$fp_x, x$fp_f), alpha = .3)
  if (x$output_method == "ALE") {
    p = p + geom_line(data = x$ALEint$plot_data, aes(x = fp_x, y = fp_f),
      col = "blue", inherit.aes = FALSE)
  } else {
    for(i in 1:(length(bounds)-1)) {
      p = p + geom_line(mapping = aes_string(bounds[i:(i+1)], c(y.0[i] -
        (x.0[i] - bounds[i]) * AME[i], y.0[i] + (bounds[i+1] - x.0[i]) * AME[i])),
        col = "blue", inherit.aes = FALSE)
    }
  }
  if (length(x$x_splits) > 0) {
    p = p + geom_vline(xintercept = bounds, linetype = 3, size = .6,
      col = "darkgray", alpha = 1)
  }
  if (show_slopes) p = p + geom_label(mapping = aes(x = x.0, y = y.0),
    label = format(AME, digits = 4), size = 2.5, alpha = .5)
  if (rugs) p = p + geom_rug(aes(x = x$x), alpha = .1, sides = "b")
  p + xlab(x$feature) + ylab(x$fe_method) + ggtitle(title) +
    scale_x_continuous(breaks = bounds, minor_breaks = NULL,
      labels = format(bounds, digits = 3, width = 3))
}
