#' Compute Partial Dependence
#'
#' Compute Partial Dependence with weighing or selecting observations based on distance.
#'
#' @template arg_model
#' @template arg_data
#' @param feature [\code{character(1)}]\cr
#'   Feature name, subset of \code{colnames(data)}.
#' @param grid_size [\code{integer(1)}]\cr Grid size.
#' @param grid_method [\code{character(1)}] c("uniform", "quantile", "sample")\cr
#'   \itemize{
#'     \item{"uniform": set grid points equidistant between min and max feature value.}
#'     \item{"quantile": set grid points equal to 1/grid_size quantiles (same as ALE).
#'     Here, grid_size is equal to the number of segments,
#'     e.g. for grid_size = 2: x.grid = c(min(x), median(x), max(x)).}
#'     \item{"sample": sample grid points from actual data points.}
#'   }
#' @template arg_predict_fun
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
#'   If multiclassification task
#' @param ... ignored
#'
#' @return [\code{PD}]
#' @export
#'
#' @examples
#' ## Replication of example 2 of ?ALEPlot::ALEPlot,
#' ## but using Partial Dependence
#' if (require(nnet) && require(ggplot2) && require(gridExtra)) {
#'   n = 1000
#'   x1 = runif(n, 0, 1)
#'   x2 = runif(n, 0, 1)
#'   x3 = runif(n, 0, 1)
#'   y = x1 + 2 * x2^2 + (x1-0.5) * (x3-0.5) + rnorm(n, 0, 0.1)
#'   df = data.frame(y, x1, x2, x3)
#'   nnet.fit = nnet(y ~ ., data = df, size = 10, linout = TRUE,
#'     decay=0.01, maxit = 1000, trace = FALSE)
#'   predict_fun = function(X.model, newdata)
#'     as.numeric(predict(object, newdata))
#'   p1 = plot(computePD(nnet.fit, df, feature="x1", grid_size=50))
#'   p2 = plot(computePD(nnet.fit, df, feature="x2", grid_size=50))
#'   p3 = plot(computePD(nnet.fit, df, feature="x3", grid_size=50))
#'   grid.arrange(p1, p2, p3, ncol=2)
#' }
computePD = function(model, data, feature,
                     predict_fun = predict,
                     grid_size = "default", grid_method = "uniform",
                     l = "default", wp = 0,
                     derivative = FALSE, multiclass = FALSE, ...) {
  assert_choice(feature, colnames(data))
  assert_function(predict_fun, args = c("object"))

  if (grid_size == "default") {
    grid_size = round(nrow(data)/5)
    if (grid_size > 40) grid_size = 40
  } else assert_integerish(grid_size, lower = 2, max.len = 1, any.missing = FALSE)

  lokal = FALSE
  if (l == "default") {
    l = nrow(data)
  } else {
    assert_integerish(l, max.len = 1)
    if (l > nrow(data)) {
      warning("computePD: l greater than nrow(data). Set to nrow(data).")
      l = nrow(data)
    } else lokal = TRUE
  }
  assert_numeric(wp, max.len = 1)
  assert_logical(derivative)
  assert_logical(multiclass)

  ##############################################################################
  x = data[, feature]
  if (grid_method == "uniform") {
    x.grid = seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = grid_size)
  } else if (grid_method == "quantile") {
    x.grid = c(min(x), as.numeric(quantile(x, seq(1/grid_size, 1,
      length.out = grid_size), type = 1)))
    grid_size = length(x.grid)
  } else if (grid_method == "sample") {
    x.grid = sort(sample(x, grid_size))
  } else stop("computePD: grid_method ", grid_method, " not supported.")

  if (multiclass) {
    y.hat.1 = predict_fun(model, newdata = data[1,])
    if (is.matrix(y.hat.1) | is.data.frame(y.hat.1)) {
      y.hat = matrix(nrow = grid_size, ncol = ncol(y.hat.1))
      classes = colnames(y.hat.1)
      colnames(y.hat) = classes
    } else {
      y.hat = matrix(nrow = grid_size, ncol = length(y.hat.1))
      classes = names(y.hat.1)
      colnames(y.hat) = classes
    }
  } else y.hat = numeric(grid_size)

  for (i in 1:(grid_size)) {
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
      stop("Has to be fixed.")
      if (multiclass) {
        for (class in classes) {
          y.hat[i, class] = weighted.mean(derivative(tmp.data[, feature],
            feature, tmp.data, model, predict_fun = function(object, newdata)
              as.numeric(predict(object, newdata)[, class])), weights)
        }
      } else {
        y.hat[i] = weighted.mean(derivative(tmp.data[, feature],
          feature, tmp.data, model, predict_fun = predict_fun), weights)
      }
    } else {
      if (multiclass) y.hat[i,] = apply(predict_fun(model, newdata = tmp.data), 2,
        weighted.mean, weights)
      else y.hat[i] = weighted.mean(predict_fun(model, newdata = tmp.data), weights)
    }
  }

  if (multiclass) {
    plot_data = reshape2::melt(data = data.frame(x = x.grid, y.hat),
      id.vars = "x", variable.name = "class", value.name = "probability")
  } else {
    plot_data = data.frame(x = x.grid, y.hat)
  }
  fe_x = (x.grid[-length(x.grid)] + x.grid[-1])/2
  fe_f = diff(y.hat)/diff(x.grid)
  return(structure(list(fp_x = x.grid, fp_f = y.hat,
                        fe_x = fe_x, fe_f = fe_f,
                        plot_data = plot_data,
                        x_org = x,
                        grid_size = grid_size, l = l,
                        multiclass = multiclass, feature = feature),
                   class = c("PD", "IntameFeatureEffect"),
                   comment = "Partial Dependence"))
}

#' @export
print.PD = function(x, ...) {
  values = x$fp_f
  names(values) = round(x$fp_x, digits = 5)
  print(values)
}

#' Plot partial dependence
#'
#' @param x PD object created by \code{\link{computePD}}
#' @param title [\code{character}] Plot title.
#' @param rugs [\code{logical(1)}]
#' @param ... ignored
#'
#' @return \code{ggplot2} plot object
#' @export
plot.PD = function(x, title = "PD Plot", rugs = TRUE, ...) {
  PD = x
  if (PD$multiclass) {
    ggplot(data = PD$plot_data,
      aes(x = x, y = probability, group = class, col = class)) +
      geom_line() + geom_point() +
      xlab(PD$feature) + ggtitle(title)
  } else {
     p = ggplot(data = PD$plot_data, mapping = aes(x, y.hat)) +
      geom_line() + geom_point()
     if (rugs) p = p + geom_rug(data = data.frame(x = PD$x_org), aes(x = x),
       alpha = .2, sides = "b", inherit.aes = FALSE)
     p + xlab(PD$feature) + ggtitle(title)
  }
}

normalize = function(x, lower.bound = 0, upper.bound = 1) {
  x.min = min(x)
  c1 = (upper.bound - lower.bound)/(max(x) - x.min)
  c2 = lower.bound - c1 * x.min
  return(c1 * x + c2)
}
