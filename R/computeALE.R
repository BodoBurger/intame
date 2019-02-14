#' Compute Local Accumulated Effects (ALE)
#'
#' Implementation of \href{https://arxiv.org/abs/1612.08468}{Apley (2016) Visualizing the Effects of Predictor Variables in Black Box Supervised Learning Models}
#'
#' @section TODO:
#' \itemize{
#'   \item implement option for Apley's normalization
#'   \item implement mlr models
#'   \item factor features
#'   \item add uniform grid option is useful (what happens to empty intervals?)
#'   \item minbucket
#'   \item pass specific interval boundaries (z)
#'   \item second-order effects
#' }
#'
#' @template arg_model
#' @template arg_data
#' @param feature [\code{character(1)}]\cr
#'   Feature name, subset of \code{colnames(data)}.
#' @param grid.size [\code{integer(1)}]\cr
#'   Number of intervals/segments. Same as parameter K in ALEPlot package.
#' @template arg_predict.fun
#' @param multiclass [\code{logical(1)}]\cr
#'   If multiclassification task (TODO: try to infer this automatically).
#' @param minbucket Not yet implemented.
#'
#' @return [\code{ALE}]
#' @export
#'
#' @examples
computeALE = function(model, data, feature,
                      grid.size = "default",
                      predict.fun = predict,
                      multiclass = FALSE, minbucket = 1) {
  checkmate::assert_choice(feature, colnames(data))
  checkmate::assertFunction(predict.fun, args = c("object"))

  if (grid.size == "default") grid.size = nrow(data)/5
  else checkmate::assert_integerish(grid.size, lower = 2, max.len = 1, any.missing = FALSE)

  checkmate::assertLogical(multiclass)

  ##############################################################################
  x = data[, feature]
  if (grid.size >= length(x)-1) {
    warning("grid.size was set to length of feature.")
    z = sort(x)
  } else {
    z = c(min(x), as.numeric(quantile(x, seq(1/grid.size, 1,
      length.out = grid.size), type = 1)))
      # c(min(), quantile()) necessary for grid.size = n
  }
  z = unique(z) # if grid.size > nrow(data) or x has lots of non-unique values
  grid.size = length(z) - 1
  # if grid.size >= (n-1) the first two obs are assigned to the first interval
  interval.indices = as.numeric(cut(x, breaks = z, include.lowest = TRUE))
  w = as.numeric(table(interval.indices))

  data.l = data
  data.u = data
  data.l[, feature] = z[interval.indices]
  data.u[, feature] = z[interval.indices + 1]
  y.hat.l = predict.fun(model, newdata = data.l)
  y.hat.u = predict.fun(model, newdata = data.u)
  #delta = y.hat.u - y.hat.l

  if (multiclass) { # multi-class
    nclass = ncol(y.hat.l)
    for (i in 1:nclass) {
      #delta[1:grid.size,i] = tapply(delta[,i], interval.indices, mean)
      y.hat.l[1:grid.size, i] = tapply(y.hat.l[,i], interval.indices, mean)
      y.hat.u[1:grid.size, i] = tapply(y.hat.u[,i], interval.indices, mean)
    }
    y.hat.l = y.hat.l[1:grid.size,]
    y.hat.u = y.hat.u[1:grid.size,]
    delta = y.hat.u - y.hat.l
    #delta = delta[1:grid.size, ]
    #f = apply(rbind(y.hat.l[1, ], delta), 2, function(x) cumsum(x))
    f = apply(delta, 2, function(x) c(0, cumsum(x)))
    f = f + matrix(y.hat.l[1,], grid.size+1, nclass, byrow = TRUE)
    #f = apply(f, 2, function(f) f - sum((f[1:grid.size] + f[2:(grid.size + 1)])/2 * w) / sum(w))
    ale = apply(delta, 2, function(x) x/diff(z))
    ale.plot.data = reshape2::melt(data = data.frame(x = z, f), id.vars = "x",
      variable.name = "class", value.name = "f")
    #ale.plot.data = reshape2::melt(data = data.frame(x = z[-length(z)], y.hat.l),
    #  id.vars = "x", variable.name = "class", value.name = "probability")
  } else {
    delta = y.hat.u - y.hat.l
    # probably better (numerically) to do tapply on y.hat, see multiclass
    delta = as.numeric(tapply(delta, interval.indices, mean))
    f = c(0, cumsum(delta))
    f = f - sum((f[1:grid.size] + f[2:(grid.size + 1)])/2 * w) / sum(w)
    ale = delta/diff(z)
    ale.plot.data = data.frame(x = z, f)
  }
  ale.x = z[-length(z)]
  return(structure(list(x = z, f = f, grid.size = grid.size, i = interval.indices,
                        ale = ale, ale.x = ale.x,
                        ale.plot.data = ale.plot.data,
                        multiclass = multiclass, feature = feature),
                   class = c("ALE", "intame"),
                   comment = "Accumulated Local Effect"))
}

#' Create ALE Plot
#'
#' @param ALE object created by \code{\link{computeALE}}
#' @param title Plot title.
#' @param derivative If TRUE, plot ALEs, otherwise plot predictions at
#'        interval limits.
#'
#' @return \code{ggplot2} plot object
#' @export
plot.ALE = function(x, title = "ALE Plot", derivative = FALSE, ...) {
  ALE = x
  if (ALE$multiclass) {
    ggplot(data = ALE$ale.plot.data,
      aes(x = x, y = f, group = class, col = class)) +
      geom_line() + geom_point() +
      xlab(ALE$feature) + ggtitle(title)
  } else {
    if (derivative) {
      ggplot(data.frame(x = ALE$ale.x, ALE = ALE$ale), aes(x = x, y = ALE)) +
        geom_line() + geom_point() +
        xlab(ALE$feature) + ggtitle("ALE Plot (derivative)")
    } else {
      ggplot(data = ALE$ale.plot.data, aes(x = x, y = f)) +
        geom_line() + geom_point() +
        xlab(ALE$feature) + ggtitle(title)
    }
  }
}
