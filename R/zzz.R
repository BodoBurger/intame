#' @importFrom checkmate assert assert_character assert_choice assert_class assert_function assert_integerish assert_list assert_logical assert_numeric check_vector
#' @importFrom ggplot2 ggplot geom_point geom_line geom_vline geom_label aes aes_string facet_wrap xlab ylab ggtitle
#' @importFrom stats .lm.fit predict quantile setNames weighted.mean
#' @importFrom utils combn
.onLoad = function(libname, pkgname) {
  # CRAN Note avoidance
  if(getRversion() >= "2.15.1")
    utils::globalVariables(c("x", "f", "probability", "y", "y.ame", "y.hat",
                             "fp_x", "fp_f", "id"))

  invisible()
}
