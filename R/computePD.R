#' Compute Partial Dependence
#'
#' Compute Partial Dependence with weighing or selecting observations based on distance.
#'
#' @section TODO:
#' \itemize{
#'   \item implement mlr models
#'   \item factor features?
#' }
#'
#' @template arg_model
#' @template arg_data
#' @param feature [\code{character(1)}]\cr
#'   Feature name, subset of \code{colnames(data)}.
#' @param grid.size [\code{integer(1)}]\cr Grid size.
#' @param grid.method [\code{character(1)}]\cr
#'   "uniform": set grid points equidistant between min and max feature value.\cr
#'   "quantile": set grid points equal to 1/grid.size quantiles (same as ALE).
#'     Here, grid.size is equal to the number of segments,
#'     e.g. for grid.size = 2: x.grid = c(min(x), median(x), max(x)).\cr
#'   "sample": sample grid points from actual data points.
#' @template arg_predict.fun
#' @param l [\code{integer(1)}]\cr
#'   Number of points that are defined as local to a grid point
#'   (Local Partial Dependence).
#' @param wp [\code{numeric}]\cr
#'   Defines the weights that are based on the distance from the grid point.
#'   Higher wp means more weight on nearby observations.\cr
#'   wp = 0: all weights equal 1 (vanilla Partial Dependence).
#' @param derivative [\code{logical(1)}]\cr
#'   FALSE: Partial Dependence. TRUE: Partial Derivative.
#' @param multiclass [\code{logical(1)}]\cr
#'   If multiclassification task (TODO: try to infer this automatically).
#'
#' @return [\code{PD}]
#' @export
#'
#' @examples
computePD = function(model, data, feature,
                     grid.size = "default", grid.method = "uniform",
                     predict.fun = function(object, newdata) predict(object, newdata = newdata),
                     l = "default", wp = 0,
                     derivative = FALSE, multiclass = FALSE) {
  checkmate::assert_choice(feature, colnames(data))
  checkmate::assertFunction(predict.fun, args = c("object"))

  if (grid.size == "default") grid.size = nrow(data)/5
  else checkmate::assert_integerish(grid.size, lower = 2, max.len = 1, any.missing = FALSE)

  lokal = FALSE
  if (l == "default") {
    l = nrow(data)
  } else {
    checkmate::assert_integerish(l, max.len = 1)
    if (l > nrow(data)) {
      warning("computePD: l greater than nrow(data). Set to nrow(data).")
      l = nrow(data)
    } else lokal = TRUE
  }
  checkmate::assertNumeric(wp, max.len = 1)
  checkmate::assertLogical(derivative)
  checkmate::assertLogical(multiclass)

  ##############################################################################
  x = data[, feature]
  if (grid.method == "uniform") {
    x.grid = seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = grid.size)
  } else if (grid.method == "quantile") {
    x.grid = c(min(x), as.numeric(quantile(x, seq(1/grid.size, 1,
      length.out = grid.size), type = 1)))
    grid.size = length(x.grid)
  } else if (grid.method == "sample") {
    x.grid = sort(sample(x, grid.size))
  } else stop("computePD: grid.method ", grid.method, " not supported.")

  if (multiclass) {
    y.hat.1 = predict.fun(model, newdata = data[1,])
    if (is.matrix(y.hat.1) | is.data.frame(y.hat.1)) {
      y.hat = matrix(nrow = grid.size, ncol = ncol(y.hat.1))
      classes = colnames(y.hat.1)
      colnames(y.hat) = classes
    } else {
      y.hat = matrix(nrow = grid.size, ncol = length(y.hat.1))
      classes = names(y.hat.1)
      colnames(y.hat) = classes
    }
  } else y.hat = numeric(grid.size)

  for (i in 1:(grid.size)) {
    distances = abs(x - x.grid[i])
    weights = (1 - normalize(distances))^wp
    if (lokal) {
      max.local.distance = sort(distances)[l]
      local.indices = which(distances <= max.local.distance)
      tmp.data = data[local.indices, ]
      tmp.data[, feature] = x.grid[i]
      weights = weights[local.indices]
    } else {
      tmp.data = data
      tmp.data[, feature] = x.grid[i]
    }
    if (derivative) {
      if (multiclass) {
        for (class in classes) {
          y.hat[i, class] = weighted.mean(derivative(tmp.data[, feature],
            feature, tmp.data, model, predict.fun = function(object, newdata)
              as.numeric(predict(object, newdata)[, class])), weights)
        }
      } else {
        y.hat[i] = weighted.mean(derivative(tmp.data[, feature],
          feature, tmp.data, model, predict.fun = predict.fun), weights)
      }
    } else {
      if (multiclass) y.hat[i,] = apply(predict.fun(model, newdata = tmp.data), 2, weighted.mean, weights)
      else y.hat[i] = weighted.mean(predict.fun(model, newdata = tmp.data), weights)
    }
  }

  if (multiclass) {
    plot.data = reshape2::melt(data = data.frame(x = x.grid, y.hat),
      id.vars = "x", variable.name = "class", value.name = "probability")
  } else {
    plot.data = data.frame(x = x.grid, y.hat)
  }
  return(structure(list(y.hat = y.hat, x.grid = x.grid,
                        plot.data = plot.data,
                        grid.size = grid.size, l = l,
                        multiclass = multiclass, feature = feature),
                   class = c("PD", "intame"),
                   comment = "Partial Dependence"))
}

#' Plot partial dependence
#'
#' @param x PD object created by \code{\link{computePD}}
#' @param title [\code{character}] Plot title.
#'
#' @return \code{ggplot2} plot object
#' @export
plot.PD = function(x, title = "PD Plot", ...) {
  PD = x
  if (PD$multiclass) {
    ggplot(data = PD$plot.data,
      aes(x = x, y = probability, group = class, col = class)) +
      geom_line() + geom_point() +
      xlab(PD$feature) + ggtitle(title)
  } else {
    ggplot(data = PD$plot.data, mapping = aes(x, y.hat)) +
      geom_line() + geom_point() +
      xlab(PD$feature) + ggtitle(title)
  }
}

normalize = function(x, lower.bound = 0, upper.bound = 1) {
  x.min = min(x)
  c1 = (upper.bound - lower.bound)/(max(x) - x.min)
  c2 = lower.bound - c1 * x.min
  return(c1 * x + c2)
}
