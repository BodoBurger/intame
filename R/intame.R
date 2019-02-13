#' Compute interval-based marginal effects
#'
#' Compute AME given break points on the sample space of a feature.
#'
#' @template arg_model
#' @template arg_data
#' @param feature [\code{character(1)}]\cr
#'   Feature name, subset of \code{colnames(data)}.
#' @param breaks [\code{numeric}]
#'
#' @return [\code{intame}]
#' @export
#'
#' @examples
intame = function(model, data, feature, n.parts = 5, method = "ALE", breaks = NULL, ...) {

  if (is.null(breaks)) {
    if (method == "ALE") {
      ALE = computeALE(model, data, feature, ...)
      breaks = partition(ALE$ale.x, ALE$ale, n.parts)
    } else if (method == "PDeriv") {
      PD = computePD(model = model, data = data, feature = feature, derivative = TRUE, ...)
      breaks = partition(PD$x.grid, PD$y.hat, n.parts)
    }
  }

  x = data[, feature]
  y.hat = predict(model, newdata = data)
  bounds = unique(c(min(x), sort(breaks), max(x) + 0.00001))
  l = length(bounds) - 1
  AME = numeric(l)
  y.hat.mean = numeric(l)
  x.interval.average = numeric(l)
  for (i in 1:l) {
    selection = x >= bounds[i] & x < bounds[i+1]
    data.interval = data[selection,]
    AME[i] = ame::computeAME(model, data.interval, feature)[, feature]
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
print.intame = function(x) {
  print(x$AME)
}

#' @export
summary.intame = function(x) {
  cat("# Interval-based Marginal Effects for", x$feature, "#\n\n")
  cat(format("Interval", width = 14, justify = "right"), " | Average Marginal Effect\n", sep = "")
  for (i in seq_along(x$AME)) {
    cat(format(names(x$AME)[i], width = 14, justify = "right"), " | ", x$AME[i], "\n", sep = "")
  }
  cat("\n")
}

#' Visualize the result of intame
#'
#' TODO: plot ALE instead of predictions
#'
#' @param x [\code{intame}]
#'
#' @export
plot.intame = function(x) {
  AME = x$AME
  x.0 = x$x.interval.average
  y.0 = x$y.hat.mean
  bounds = x$bounds
  p = ggplot2::ggplot() +
    ggplot2::geom_point(mapping = aes(x$x, x$y.hat), pch = 16, alpha = .2)
  for(i in 1:(length(bounds)-1)) {
    p = p + ggplot2::geom_line(mapping = aes_string(bounds[i:(i+1)], c(y.0[i] - (x.0[i] - bounds[i]) * AME[i],
      y.0[i] + (bounds[i+1] - x.0[i]) * AME[i])), col = "green", inherit.aes = FALSE)
  }
  return(p)
}
