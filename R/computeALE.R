#' Compute Local Accumulated Effects (ALE)
#'
#' Implementation of \href{https://arxiv.org/abs/1612.08468}{Apley (2016) Visualizing the Effects of Predictor Variables in Black Box Supervised Learning Models}
#'
#' @template arg_model
#' @template arg_data
#' @param feature [\code{character(1)}]\cr
#'   Feature name, subset of \code{colnames(data)}.
#' @param grid_size [\code{integer(1)}]\cr
#'   Number of intervals/segments. Same as parameter K in ALEPlot package.
#' @param grid_breaks [\code{numeric}]
#' @template arg_predict_fun
#' @param center_at_zero [\code{logical(1)}]\cr
#'   Apley suggests centering AL predictions around zero.\cr
#'   Set to FALSE to compare ALE and PD.
#' @param multiclass [\code{logical(1)}]\cr
#'   If multiclassification task
#' @param ... ignored
#'
#' @return [\code{ALE}]
#' @export
#'
#' @examples
#' ## Replication of example 2 of ?ALEPlot::ALEPlot
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
#'   p1 = plot(computeALE(nnet.fit, df, feature="x1", grid_size=50))
#'   p2 = plot(computeALE(nnet.fit, df, feature="x2", grid_size=50))
#'   p3 = plot(computeALE(nnet.fit, df, feature="x3", grid_size=50))
#'   grid.arrange(p1, p2, p3, ncol=2)
#' }
computeALE = function(model, data, feature,
                      predict_fun = predict,
                      grid_size = "default", grid_breaks = NULL,
                      center_at_zero = FALSE, multiclass = FALSE, ...) {
  assert_choice(feature, colnames(data))
  assert_function(predict_fun, args = c("object"))

  if (grid_size == "default") {
    grid_size = round(nrow(data)/5)
    if (grid_size > 40) grid_size = 40
  } else assert_integerish(grid_size, lower = 2, max.len = 1, any.missing = FALSE)

  assert_logical(multiclass)

  ##############################################################################
  x = data[, feature]
  if (is.null(grid_breaks)) {
    if (grid_size >= length(x)-1) {
      warning("grid_size was set to equal (or greater) than number of obs.")
      z = sort(x)
    } else {
      z = c(min(x), as.numeric(quantile(x, seq(1/grid_size, 1,
        length.out = grid_size), type = 1)))
        # c(min(), quantile()) necessary for grid_size = n
    }
  } else {
    if (!all(grid_breaks > min(x)) | !all(grid_breaks < max(x)))
      stop("Provided grid_breaks not in range of feature values.")
    z = c(min(x), sort(grid_breaks), max(x))
  }
  z = unique(z) # if grid_size > nrow(data) or x has lots of non-unique values
  grid_size = length(z) - 1
  # if grid_size >= (n-1) the first two obs are assigned to the first interval
  interval_indices = as.numeric(cut(x, breaks = z, include.lowest = TRUE))
  w = as.numeric(table(interval_indices))

  data_l = data
  data_u = data
  data_l[, feature] = z[interval_indices]
  data_u[, feature] = z[interval_indices + 1]
  y_hat_l = predict_fun(model, newdata = data_l)
  y_hat_u = predict_fun(model, newdata = data_u)
  #delta = y_hat_u - y_hat_l

  if (multiclass) { # multi-class
    nclass = ncol(y_hat_l)
    for (i in 1:nclass) {
      #delta[1:grid_size,i] = tapply(delta[,i], interval_indices, mean)
      y_hat_l[1:grid_size, i] = tapply(y_hat_l[,i], interval_indices, mean)
      y_hat_u[1:grid_size, i] = tapply(y_hat_u[,i], interval_indices, mean)
    }
    y_hat_l = y_hat_l[1:grid_size,]
    y_hat_u = y_hat_u[1:grid_size,]
    delta = y_hat_u - y_hat_l
    #delta = delta[1:grid_size, ]
    #f = apply(rbind(y_hat_l[1, ], delta), 2, function(x) cumsum(x))
    f = apply(delta, 2, function(x) c(0, cumsum(x)))
    f = f + matrix(y_hat_l[1,], grid_size+1, nclass, byrow = TRUE)
    #f = apply(f, 2, function(f) f - sum((f[1:grid_size] + f[2:(grid_size + 1)])/2 * w) / sum(w))
    ale = apply(delta, 2, function(x) x/diff(z))
    plot_data = reshape2::melt(data = data.frame(x = z, f), id.vars = "x",
      variable.name = "class", value.name = "f")
    #plot_data = reshape2::melt(data = data.frame(x = z[-length(z)], y_hat_l),
    #  id.vars = "x", variable.name = "class", value.name = "probability")
  } else {
    delta = y_hat_u - y_hat_l
    # probably better (numerically) to do tapply on y.hat, see multiclass
    delta = as.numeric(tapply(delta, interval_indices, mean))
    f = c(0, cumsum(delta))
    f = f - sum((f[1:grid_size] + f[2:(grid_size + 1)])/2 * w) / sum(w)
    if (!center_at_zero) {
      f = f + mean(c(y_hat_l, y_hat_u))
    }
    ale = delta/diff(z)
    plot_data = data.frame(fp_x = z, fp_f = f)
  }
  ale.x = (z[-length(z)] + z[-1]) / 2
  return(structure(list(fp_x = z, fp_f = f, # predicted values
                        fe_x = ale.x, fe_f = ale, # predicted derivatives
                        plot_data = plot_data,
                        x_org = x,
                        grid = z, grid_size = grid_size,
                        interval_indices = interval_indices,
                        multiclass = multiclass, feature = feature),
                   class = c("ALE", "IntameFeatureEffect"),
                   comment = "Accumulated Local Effect"))
}

#' @export
print.ALE = function(x, ...) {
  ale.values = x$fe_f
  names(ale.values) = round(x$fe_x, digits = 5)
  print(ale.values)
}

#' Create ALE Plot
#'
#' @param x object created by \code{\link{computeALE}}
#' @param title Plot title.
#' @param rugs [\code{logical(1)}]
#' @param derivative If TRUE, plot ALEs, otherwise plot predictions at
#'        interval limits.
#' @param ... ignored
#'
#' @return \code{ggplot2} plot object
#' @export
plot.ALE = function(x, title = "ALE Plot", rugs = TRUE, derivative = FALSE, ...) {
  ALE = x
  if (ALE$multiclass) {
    ggplot(data = ALE$plot_data,
      aes(x = x, y = f, group = class, col = class)) +
      geom_line() + geom_point() +
      xlab(ALE$feature) + ggtitle(title)
  } else {
    if (derivative) {
      ggplot(data.frame(x = ALE$fe_x, ALE = ALE$fe_f), aes(x = x, y = ALE)) +
        geom_line() + geom_point() +
        xlab(ALE$feature) + ggtitle("ALE Plot (derivative)")
    } else {
      p = ggplot(data = ALE$plot_data, aes(x = fp_x, y = fp_f)) +
        geom_line() + geom_point()
      if (rugs) p = p + geom_rug(data = data.frame(x = ALE$x_org), aes(x = x),
        alpha = .2, sides = "b", inherit.aes = FALSE)
      p + xlab(ALE$feature) + ggtitle(title)
    }
  }
}
