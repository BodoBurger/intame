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
#' @return [\code{intame}]
#' @export
#'
#' @examples
intame = function(model, data, feature,
                  intervals = 5,
                  predict.fun = predict,
                  method = "ALE", breaks = NULL, ...) {
  checkmate::assert_choice(feature, colnames(data))
  checkmate::assert_integerish(intervals, lower = 2, any.missing = FALSE, max.len = 1)
  if (is.null(breaks)) {
    if (method == "ALE") {
      ALE = computeALE(model, data, feature, ...)
      breaks = partition(ALE$ale.x, ALE$ale, intervals)
    } else if (method == "PDeriv") {
      PD = computePD(model = model, data = data, feature = feature, derivative = TRUE, ...)
      breaks = partition(PD$x.grid, PD$y.hat, intervals)
    }
  }

  x = data[, feature]
  y.hat = predict.fun(model, newdata = data)
  bounds = unique(c(min(x), sort(breaks), max(x) + 0.00001))
  l = length(bounds) - 1
  AME = numeric(l)
  y.hat.mean = numeric(l)
  x.interval.average = numeric(l)
  for (i in 1:l) {
    selection = x >= bounds[i] & x < bounds[i+1]
    data.interval = data[selection,]
    AME[i] = ame::computeAME(model, data.interval, feature,
      predict.fun = predict.fun)[, feature]
    y.hat.mean[i] = mean(y.hat[selection])
    x.interval.average[i] = mean(x[selection])
  }
  bounds.rounded = round(bounds, digits = 3)
  interval.desc = character(l)
  interval.desc[l] = paste0("[", bounds.rounded[l-1], ", ", bounds.rounded[l], "]")
  for (i in 1:(l-1)) {
    interval.desc[i] = paste0("[", bounds.rounded[i], ", ", bounds.rounded[i+1], ")")
  }
  return(structure(list(AME = setNames(AME, interval.desc),
                        bounds = bounds,
                        breaks = breaks,
                        feature = feature,
                        y.hat.mean = y.hat.mean,
                        x.interval.average = x.interval.average,
                        y.hat = y.hat,
                        x = x),
                   class = "intame",
                   comment = "Main class of intame package."))
}

#' @export
print.intame = function(x, ...) {
  print(x$AME)
}

# TODO: check char length of interval
#' @export
summary.intame = function(object, ...) {
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
#' @param ...
#'
#' @export
plot.intame = function(x, ...) {
  AME = x$AME
  x.0 = x$x.interval.average
  y.0 = x$y.hat.mean
  bounds = x$bounds
  p = ggplot() +
    geom_point(mapping = aes(x$x, x$y.hat), pch = 16, alpha = .2)
  for(i in 1:(length(bounds)-1)) {
    p = p + geom_line(mapping = aes_string(bounds[i:(i+1)], c(y.0[i] - (x.0[i] - bounds[i]) * AME[i],
      y.0[i] + (bounds[i+1] - x.0[i]) * AME[i])), col = "green", inherit.aes = FALSE)
  }
  return(p)
}
