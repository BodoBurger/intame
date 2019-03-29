#' @param metric_name [\code{character(1)}] Implemented metrics: \cr
#'   \itemize{
#'     \item{"L2": Sum of squared errors. (Default)}
#'     \item{"L1": Sum of absolute errors.}
#'     \item{"R2": R squared with SST of feature.}
#'     \item{"R2int": Weighted mean of R squared with SST computed within each interval.}
#'     \item{"Frechet": \href{https://en.wikipedia.org/wiki/Frechet_distance}{Frechet-Distance}.}
#'   }
#'   \emph{Notice:} If you provide an \code{IntameThreshold} object
#'   \code{metric_name} will be set to the metric used for the threshold.
#'
