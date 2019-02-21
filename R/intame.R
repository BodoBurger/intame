#' Compute interval-based marginal effects
#'
#' Compute AME given break points on the sample space of a feature.
#'
#' @template arg_model
#' @template arg_data
#' @param feature [\code{character(1)}]\cr
#'   Feature name, subset of \code{colnames(data)}.
#' @param intervals [\code{integer}]\cr
#'   Number of intervals.
#' @template arg_predict.fun
#' @param method [\code{character}]\cr
#'   Method for calculating fine-granulated marginal effects.
#'   Based on these values, the algorithm determines intervals of similar
#'   marginal effects.\cr
#'   "ALE": Accumulated Local Effect\cr
#'   "PDeriv": Partial Derivative
#' @param breaks [\code{numeric}]\cr
#'   Define interval limits manually. Overwrites \code{intervals} if provided.
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
#' n = 500
#' x = runif(n, min = 0, max = 1)
#' x1 = runif(n, min=0, max=1) + .5*x
#' x2 = runif(n, min=0, max=1) + .5*x
#' y2 = function(x) -4 * cos(4*pi*x) * x + 4
#' y = 4*x1 + y2(x2) + rnorm(n, 0, .5)
#' df = data.frame(y, x1, x2)
#' nnet.mod = nnet(y ~ ., df, size = 20, linout = TRUE,
#'   decay = 0.0001, maxit = 1000, trace = FALSE)
#' AME.x1 = intame(nnet.mod, df, "x1", intervals = 2)
#' AME.x1
#' plot(AME.x1)
#' AME.x2 = intame(nnet.mod, df, "x2")
#' AME.x2
#' plot(AME.x2)
intame = function(model, data, feature,
                  intervals = 5,
                  predict.fun = function(object, newdata) predict(object, newdata),
                  method = "ALE", breaks = NULL, ...) {
  checkmate::assert_choice(feature, colnames(data))
  checkmate::assert_integerish(intervals, lower = 2, any.missing = FALSE, max.len = 1)

  if (method == "ALE") {
    FE = computeALE(model = model, data = data, feature = feature,
      predict.fun = predict.fun, ...)
  } else if (method == "PDeriv") {
    FE = computePD(model = model, data = data, feature = feature,
      predict.fun = predict.fun, derivative = FALSE, ...)
  }
  fp_x = FE$fp_x
  fp_f = FE$fp_f
  fe_x = FE$fe_x
  fe_f = FE$fe_f
  if (is.null(breaks)) breaks = partition(fe_x, fe_f, intervals)

  x = data[, feature]
  y.hat = predict.fun(model, newdata = data)
  bounds = unique(c(min(x), sort(breaks), max(x) + 0.00001))
  if (length(bounds)-1 < intervals) message("Found less than ", intervals, " intervals.")
  intervals = length(bounds) - 1
  AME = numeric(intervals)
  y.hat.mean = numeric(intervals)
  x.interval.average = numeric(intervals)
  for (i in 1:intervals) {
    selection = x >= bounds[i] & x < bounds[i+1]
    data.interval = data[selection,]
    AME[i] = ame::computeAME(model, data.interval, feature,
      predict.fun = predict.fun)[, feature]
    y.hat.mean[i] = mean(y.hat[selection])
    x.interval.average[i] = mean(x[selection])
  }
  bounds.rounded = round(bounds, digits = 3)
  interval.desc = character(intervals)
  interval.desc[intervals] = paste0("[", bounds.rounded[intervals], ", ",
    bounds.rounded[intervals+1], "]")
  for (i in 1:(intervals-1)) {
    interval.desc[i] = paste0("[", bounds.rounded[i], ", ", bounds.rounded[i+1], ")")
  }
  return(structure(list(AME = setNames(AME, interval.desc),
                        bounds = bounds,
                        breaks = breaks,
                        feature = feature,
                        y.hat.mean = y.hat.mean,
                        x.interval.average = x.interval.average,
                        y.hat = y.hat,
                        x = x,
                        fp_x = fp_x, fp_f = fp_f,
                        fe_x = fe_x, fe_f = fe_f),
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
#' TODO: plot ALE instead of predictions
#'
#' @param x [\code{intame}]
#' @param ... ignored
#'
#' @export
plot.Intame = function(x, ...) {
  AME = x$AME
  x.0 = x$x.interval.average
  y.0 = x$y.hat.mean
  bounds = x$bounds
  p = ggplot() +
    geom_point(mapping = aes(x$fp_x, x$fp_f), pch = 16, alpha = .3)
  for(i in 1:(length(bounds)-1)) {
    p = p + geom_line(mapping = aes_string(bounds[i:(i+1)], c(y.0[i] -
      (x.0[i] - bounds[i]) * AME[i], y.0[i] + (bounds[i+1] - x.0[i]) * AME[i])),
      col = "green", inherit.aes = FALSE)
  }
  return(p)
}
