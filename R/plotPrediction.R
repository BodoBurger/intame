#' Plot feature values against predictions of a model
#'
#' Creates univariate plots of a feature's values against model predictions and a smooth curve
#' using loess regression.
#'
#' @param model Model object. Object created by some fitting algorithm.
#' @param ... Passed on to S3 methods
#'
#' @return \code{ggplot}
#' @export
plotPrediction = function(model, ...) {
  UseMethod("plotPrediction")
}

#' @export
plotPrediction.default = function(model, data, feature, predict_fun = predict, loess = TRUE,
    span = .2, plot.points = TRUE, ...) {
  x = data[, feature]
  y.hat = predict_fun(model, newdata = data)
  plot_data = data.frame(x, y.hat)
  colnames(plot_data) = c(feature, "y.hat")
  if (loess) {
    loess.mod = loess(y.hat ~ x, data = plot_data, span = span, ...)
    y.hat.loess = predict(loess.mod)
    plot_data = data.frame(plot_data, y.hat.loess)
  }
  p = ggplot(data = plot_data, aes(x = x, y = y.hat)) + geom_line(alpha = .7) + xlab(feature)
  if (plot.points) p = p + geom_point(alpha = .2)
  if (loess) {
    p = p + geom_line(aes(y = y.hat.loess), col = "blue")
  }
  return(list(plot = p, plot_data = plot_data))
}

#' @export
plotPrediction.WrappedModel = function(model, task, feature, ...) {
  data = mlr::getTaskData(task)
  predict_fun = function(model, newdata = data) mlr::getPredictionResponse(predict(model, newdata = data))
  plotPrediction.default(model, data, feature, predict_fun = predict_fun, ...)
}
