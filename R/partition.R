#' Partition x based on y
#'
#' @param x [\code{numeric}]\cr
#'   Partitioned feature. Feature that is divided into segments on basis of
#'   the values of feature \code{y}.
#' @param y [\code{numeric}]\cr
#'   Partitioning feature. Values on which basis feature \code{x} is partitioned.
#' @param intervals [\code{integer}]\cr Number of intervals.
#' @param part.method c("CART", "cluster")
#'
#' @return split.points numeric(length(n.parts)-1)
#' @export
#'
#' @examples
#' library(ggplot2)
#' set.seed(0815)
#' n = 6
#' x = sample(n) + 1.5
#' y = runif(n, 0, 10)
#' df = data.frame(x, y)
#' breaks.rpart = partition(df$x, df$y, intervals = 4)
#' breaks.cluster = partition(df$x, df$y, intervals = 4,
#'   part.method = "cluster")
#' ggplot() +
#'   geom_point(data=df, aes(x = x, y = y)) +
#'   geom_vline(aes(xintercept = breaks.rpart-.03,
#'     color = "rpart")) +
#'   geom_vline(aes(xintercept = breaks.cluster+.03,
#'     color = "cluster"))
partition = function(x, y, intervals, part.method="CART") {
  assert_numeric(x)
  assert_numeric(y)
  if (length(x) != length(y)) stop("x and y have to have the same length.")
  assert_integerish(intervals)

  if (part.method == "CART") return(partitionCART(x, y, (intervals-1)))
  #else if (part.method == "MOB") return(partitionMOB(x, y, intervals))
  else if (part.method == "cluster") return(partitionCluster(x, y, intervals))
  else stop("Partition method \'", part.method, "\' not implemented.")
}

#' Partition using CART algorithm
#'
#' @param x [\code{numeric}]\cr
#'   Partitioned feature. Feature that is divided into segments on basis of
#'   the values of feature \code{y}.
#' @param y [\code{numeric}]\cr
#'   Partitioning feature. Values on which basis feature \code{x} is partitioned.
#' @param max.splits Upper limit to applied splits on feature space.
partitionCART = function(x, y, max.splits) {
  mod = rpart::rpart(y ~ x, cp = 0, maxcompete = 0,
    minsplit = 1, minbucket = 1, xval = 0, maxsurrogate = 0)
  cp.ind = max(which(mod$cptable[,"nsplit"] <= max.splits))
  mod = rpart::prune(mod, cp = mod$cptable[cp.ind, "CP"])
  unname(mod$splits[,"index"])
}

#' Clustering-like method
#'
#' Iteratively group neighboring points of x based on the evaluation function
#' that is applied values of y.
#'
#' Difference to "standard" clustering is that a feature is clustered
#' on basis of values of another feature.
#'
#' @param x [\code{numeric}]\cr
#'   Partitioned feature. Feature that is divided into segments on basis of
#'   the values of feature \code{y}.
#' @param y [\code{numeric}]\cr
#'   Partitioning feature. Values on which basis feature \code{x} is partitioned.
#' @param n.parts [\code{integer}] Number of intervals.
#' @param eval.fun [\code{function}]\cr
#'   Method
partitionCluster = function(x, y, n.parts, eval.fun = absDiffMean) {
  if (n.parts < 2) stop("\'n.pars\' has to be greater equal 2!")

  s = sort(x, index.return=TRUE)
  yp = as.list(y[s$ix])
  intervals = lapply(s$x, function(x) c(x, x))
  while(length(yp) > n.parts) {
    i = which.min(eval.fun(yp))
    intervals[[i]] = c(intervals[[i]][1], intervals[[i+1]][2])
    intervals[[i+1]] = NULL
    yp[[i]] = unlist(yp[c(i, i+1)])
    yp[i+1] = NULL
  }
  split.points = vapply(seq.int(1, length(intervals)-1),
    function(i) mean(c(intervals[[i]][2], intervals[[i+1]][1])), FUN.VALUE = numeric(1))
  return(split.points)
}

#' Absolute values of differences of means of succeeding elements
#'
#' Calculate mean for every element of the list,
#' calculate the differences between succeeding means,
#' and return absolute values of these differences.
#'
#' @param y list of numerical vectors
#'
#' @return numeric(length(y) - 1)
absDiffMean = function(y) {
  l = length(y)
  abs(vapply(1:(l-1), function(i) mean(y[[i+1]]) - mean(y[[i]]), FUN.VALUE = numeric(1)))
}
