#' @importFrom checkmate assert assert_character assert_choice assert_class assert_function assert_integerish assert_list assert_logical assert_numeric check_vector test_class
#' @importFrom ggplot2 ggplot geom_point geom_line geom_vline geom_rug geom_label aes aes_string facet_wrap xlab ylab ggtitle scale_x_continuous
#' @importFrom stats .lm.fit predict quantile sd setNames weighted.mean
#' @importFrom utils combn
#' @importFrom SimilarityMeasures Frechet
.onLoad = function(libname, pkgname) {
  # CRAN Note avoidance
  if(getRversion() >= "2.15.1")
    utils::globalVariables(c("x", "f", "probability", "y", "y.ame", "y.hat",
                             "fp_x", "fp_f", "id"))

  invisible()
}
