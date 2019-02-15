#' Visualize AME feature-wise
#'
#' @param x object created by \code{\link[ame]{computeAME}}
#' @param data data.frame
#' @param target character(1) Name of target variable.
#' @param model model object
#' @param ... ignored
#'
#' @section TODOS:
#' \itemize{
#' \item Write tests (you can use examples from AME vignette)
#' \item Wrapped.Model / task support
#' \item classification support
#' \item Facet of different models (i.e. draw multiple AME lines for one feature)
#' }
#'
#' @export
plot.AME = function(x, data, target, model = NA, ...) {
  AME = x
  features = names(AME)
  ame = as.numeric(AME)
  names(ame) = features
  data = data[, c(features, target)]
  features.mean = colMeans(data[, features, drop = FALSE])
  intercepts = mean(data[, target]) - ame * features.mean

  data.plot = reshape2::melt(data, id.vars = target, variable.name = "feature", value.name = "x")
  data.plot[["y.ame"]] = numeric(nrow(data.plot))
  for (feature in features) {
    data.plot[data.plot$feature==feature, "y.ame"] = intercepts[feature] +
      data.plot[data.plot$feature == feature, "x"] * ame[feature]
  }
  ggplot(data = data.plot, mapping = aes(x = x)) +
    geom_point(mapping = aes(y = y), alpha = .3) +
    geom_line(mapping = aes(y = y.ame), col = "red") +
    facet_wrap(~ feature, scales = "free_x")
}
