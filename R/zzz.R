#' @import checkmate
#' @importFrom ggplot2 ggplot geom_point geom_line geom_vline aes aes_string facet_wrap xlab ylab ggtitle
#' @importFrom stats lm predict quantile setNames weighted.mean
.onLoad = function(libname, pkgname) {
  # CRAN Note avoidance
  if(getRversion() >= "2.15.1")
    utils::globalVariables(c("x", "f", "probability", "y", "y.ame", "y.hat",
                             "fp_x", "fp_f", "id"))

  invisible()
}
