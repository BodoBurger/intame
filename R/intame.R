#' Compute interval-based marginal effects
#'
#' Compute AME given break points on the sample space of a feature.
#'
#' @template arg_model
#' @template arg_data
#' @param feature [\code{character(1)}]\cr
#'   Feature name, subset of \code{colnames(data)}.
#' @param intervals [\code{integer(1)}]\cr
#'   Number of intervals.
#' @template arg_predict.fun
#' @param fe_method [\code{character(1)}]\cr
#'   Method for calculating fine-granulated marginal effects.
#'   Based on these values, the algorithm determines intervals of similar
#'   marginal effects.\cr
#'   "ALE": Accumulated Local Effect\cr
#'   "PDeriv": Partial Derivative
#' @param fe_grid_size [\code{integer(1)}]\cr
#'   Number of intervals/segments. Same as parameter K in ALEPlot package.
#' @param x_splits [\code{numeric}]\cr
#'   Define interval limits manually. Overwrites \code{intervals} if provided.
#' @param use_AME [\code{logical(1)}]
#'   Calculate "classic" Average Marginal Effects for each interval (not recommended).
#' @param use_iterative_algorithm (recommended)
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
#' AME.x2 = intame(nnet.mod, df, "x2", fe_grid_size = 20, greedy = TRUE)
#' AME.x2
#' plot(AME.x2)
intame = function(model, data, feature,
                  intervals = 5,
                  predict.fun = function(object, newdata) predict(object, newdata),
                  fe_method = "ALE", fe_grid_size = "default",
                  x_splits = NULL,
                  use_AME = FALSE, use_iterative_algorithm = TRUE,
                  ...) {
  assert_choice(feature, colnames(data))
  assert_integerish(intervals, lower = 2, any.missing = FALSE, max.len = 1)
  assert_choice(fe_method, c("ALE", "PDeriv"))

  if (fe_method == "ALE") {
    FE = computeALE(model = model, data = data, feature = feature,
      predict.fun = predict.fun, grid.size = fe_grid_size, ...)
  } else if (fe_method == "PDeriv") {
    FE = computePD(model = model, data = data, feature = feature,
      predict.fun = predict.fun, grid.size = fe_grid_size, derivative = FALSE, ...)
  }
  fp_x = FE$fp_x
  fp_f = FE$fp_f
  fe_x = FE$fe_x
  fe_f = FE$fe_f
  if (is.null(x_splits)) {
    if (use_iterative_algorithm) {
      intame_partition = iterative_partition(fp_x, fp_f, ...)
      x_splits = fp_x[intame_partition$splits]
    } else {
      intame_partition = NULL
      x_splits = partition(fe_x, fe_f, intervals)
    }
  }

  x = data[, feature]
  bounds = c(min(x), sort(x_splits), max(x))
  #if (length(bounds)-1 < intervals) message("Found less than ", intervals, " intervals.")
  intervals = length(bounds) - 1
  AME = numeric(intervals) # slope
  y.hat.mean = numeric(intervals) # ordinate
  x.interval.average = numeric(intervals) # abscissa
  if (!use_AME) {
    for (i in 1:intervals) {
      coefficients_interval = intame_partition$models[[i]]$coefficients
      AME[i] = coefficients_interval[2]
      x.interval.average[i] = (bounds[i] + bounds[i+1])/2
      y.hat.mean[i] = x.interval.average[i] * AME[i] + coefficients_interval[1]
    }
  } else {
    bounds[intervals+1] = bounds[intervals+1] + 0.000001
    for (i in 1:intervals) {
      selection = x >= bounds[i] & x < bounds[i+1]
      data.interval = data[selection,]
      AME[i] = ame::computeAME(model, data.interval, feature,
        predict.fun = predict.fun)[, feature]
      y.hat.mean[i] = mean(y.hat[selection])
      x.interval.average[i] = mean(x[selection])
    }
  }

  bounds.rounded = round(bounds, digits = 3)
  interval.desc = character(intervals)
  interval.desc[intervals] = paste0("[", bounds.rounded[intervals], ", ",
    bounds.rounded[intervals+1], "]")
  for (i in 1:(intervals-1)) {
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
                        fe_method = fe_method),
                   class = "Intame",
                   comment = "Main class of intame package."))
}

#' @export
print.Intame = function(x, ...) {
  print(x$AME)
}

# TODO: check char length of interval
#' @export
summary.Intame = function(object, ...) {
  cat("# Interval-based Marginal Effects for", object$feature, "#\n\n")
  cat(format("Interval", width = 14, justify = "right"), " | Average Marginal Effect\n", sep = "")
  for (i in seq_along(object$AME)) {
    cat(format(names(object$AME)[i], width = 14, justify = "right"), " | ", object$AME[i], "\n", sep = "")
  }
  cat("\n")
}

#' Visualize the result of intame
#'
#' TODO: add slope as label for each interval
#'
#' @param x [\code{intame}]
#' @param title [\code{character(1)}] Plot title
#' @param ... ignored
#'
#' @export
plot.Intame = function(x, title = "default", ...) {
  if (title == "default") {
    title = paste0("Intame using ", x$fe_method)
  }
  AME = x$AME
  x.0 = x$x.interval.average
  y.0 = x$y.hat.mean
  bounds = x$bounds
  p = ggplot() +
    geom_line(mapping = aes(x$fp_x, x$fp_f), alpha = .3)
  for(i in 1:(length(bounds)-1)) {
    p = p + geom_line(mapping = aes_string(bounds[i:(i+1)], c(y.0[i] -
      (x.0[i] - bounds[i]) * AME[i], y.0[i] + (bounds[i+1] - x.0[i]) * AME[i])),
      col = "blue", inherit.aes = FALSE)
  }
  if (x$intame_partition$n_splits > 0) {
    p = p + geom_vline(xintercept = bounds, linetype = 3, size = .6,
      col = "darkgray", alpha = 1)
  }
  p + xlab(x$feature) + ylab(x$fe_method) + ggtitle(title)
}
