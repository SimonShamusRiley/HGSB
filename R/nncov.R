#' @title Nearest-neighbor covariates
#'
#' @description Calculate spatially-lagged covariates or adjacency matrices for
#'   observations indexed by any arbitrary number of dimensions.
#'
#' @param ... Vector of numeric values indicating the location of observations along each
#'   dimension. Non-numeric values are coerced to numeric values with a warning if
#'   \code{warn = TRUE}.
#' @param covariate An optional vector of covaritate values. If provided, the function
#'   returns the spatially-lagged covariate values (one for each dimension supplied via
#'   \code{...}). If \code{covariate} is not supplied, the adjacency matrices (one for
#'   each dimension supplied via \code{...})).
#' @param thresholds Numeric. A vector of three values defining, respectively: the maximum
#'   distance at which observations are at considered to be at the "equivalent" locations
#'   in a given dimension, and the minimum and maximum distances distances which define
#'   "adjacent" locations in a dimension. See \code{Details} and \code{Examples} for
#'   additional information.
#' @param rownorm Logical. Should the adjacency matrices be row standardized? Defaults to
#'   \code{TRUE}.
#' @param reduce Logical. Should the adjacency matrices (or covariate vectors) in each
#'   dimension be reduced to a single adjacency matrix (or covariate vector)? Default is
#'   \code{TRUE}
#' @param warn Logical. Issue a warning when arguments to \code{...} have been coerced to
#'   numeric values? Defaults to \code{TRUE}.
#'
#' @details Observations are defined here as being adjacent in any given dimension d (in
#'   the set of dimensions D) if the Euclidean distance between those observations is less
#'   than or equal to t_1 in dimensions d' and greater than t_2 but less than or equal to
#'   t_3 in dimension d.
#'
#' @return One or more vector of spatially-lagged covariates if \code{covariate} is
#'   supplied; otherwise one or more adjacency matrix.
#'
#' @references Arbia, G. (2014). "Some Important Spatial Definitions" in A Primer of
#'   Spatial Econometrics with examples in R. New York City, NY: Palgrave Macmillan.
#'
#' @examples
#' # Create and plot example field layout
#' library(desplot)
#' field <- expand.grid(Row = 1:4, Col = 1:4)
#' field$Y <- rnorm(nrow(field))
#' field$ID <- rownames(field)
#' desplot(field, ~ Row + Col, text = ID, cex = 2)
#'
#' # Plot 4 (the fourth row in the resulting matrices)
#' # should be row-adjacent to plot 3 but not 5 or 8,
#' # and column-adjacent to 8 but not 3 or 5:
#' nncov(Row = field$Row, Col = field$Col,
#'       rownorm = F, reduce = F, warn = F)
#'
#' # In the reduced adjacency matrix, plot 4 should
#' # appear adjacent to 3 and 8 only, while plot 10
#' # should appear adjacent to 6, 11, 9 and 14:
#' nncov(Row = field$Row, Col = field$Col,
#'       rownorm = F, reduce = T, warn = F)
#'
#' # Default thresholds = c(0, 0, 1) generates "rook"
#' # adjacency; for "Queen" adjacency use
#' # thresholds = c(1, 0, 1)
#' nncov(Row = field$Row, Col = field$Col,
#'       thresholds = c(1, 0, 1),
#'       rownorm = F, reduce = T, warn = F)
#' @export
nncov <- function(..., covariate, thresholds = c(0, 0, 1), rownorm = T, reduce = T, warn = T){
  # L is a list of vectors supplied to ...
  L <- list(...)
  check <- sapply(L, class)
  if (any(check != 'numeric')){
    nonnum <- unique(check[check != 'numeric'])
    if (warn == T){
      warning(simpleWarning(paste0('vectors of class: ', nonnum, ' have been coerced to numeric values')))
    }
    fix <- which(check != 'numeric')
    L[fix] <- lapply(L[fix], function(x){as.numeric(factor(x))})
  }
  # nms is the names of the dimensions
  if (is.null(names(L)) | any(names(L) == '')){
    nms <- deparse(substitute(list(...)))
    nms <- substr(nms, 6, nchar(nms)-1)
    nms <- unlist(strsplit(nms, split = ','))
  } else {
    nms <- names(L)
  }

  # D is a list of distance matrices, one for each entry in L
  D <- lapply(L, function(x) {as.matrix(dist(as.numeric(x), upper = T, diag = T))})

  # Dbin is the binary (within or beyond thresholds) representation of the distance
  # matrices, A the adjaceny matrices (one for entry in L)
  A <- Dbin <- vector('list', length = length(D))
  for (n in seq(length(D))){
    if (length(D) > 1){
      Dbin[[-n]] <- D[[-n]] <= thresholds[1]
    }
    Dbin[[n]] <- 1*(D[[n]] > thresholds[2] & D[[n]] <= thresholds[3])

    A[[n]] <- Reduce('*', Dbin)
  }

  if (reduce == T){
    A <- 1*(Reduce('+', A) > 0)

    if (rownorm == T){
      A <- A/rowSums(A)
    }
    if (!missing(covariate)){
      return(A %*% covariate)
    } else {
      return(A)
    }
  } else {
    names(A) <- nms
    if (rownorm == T){
      A <- lapply(A, function(x){x/rowSums(x)})
    }
    if (!missing(covariate)){
      lapply(A, `%*%`, covariate)
      return(lapply(A, `%*%`, covariate))
    } else {
      return(A)
    }
  }
}

