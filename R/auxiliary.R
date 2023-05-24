#' @title Confidence intervals for coefficient of variation
#'
#' @description This function calculates the coefficient of variation and
#'   confidence intervals for a fitted model using the modified MacKey method,
#'   as described by Vangal (DOI: 10.1080/00031305.1996.10473537, see References
#'   section for complete citation). The code is adapted from the SAS macro
#'   cvmixed, by Stephen Bowley
#'
#' @param mean Numeric. A mean.
#' @param sd Numeric. A standard deviation.
#' @param n Integer. Number of observations.
#' @param alpha Numeric. Type I error rate for confidence intervals. Defaults
#'  to 0.05.
#' @param pct Logical. If TRUE (the default), results are expressed as percents.
#'
#' @return An array, containing the coefficient of variation, and the upper and
#'   lower confidence intervals for the given alpha.
#'
#' @references Vangel MG (1996). "Confidence Intervals for a Normal Coefficient
#'   of Variation". _The American Statistician_, _50_(1), 20-26. doi:
#'   \href{https://doi.org/10.1080/00031305.1996.10473537}{10.1080/00031305.1996.10473537}
#'
#' @export
cv_confint <- function(mean, sd, n, alpha = 0.05, pct = T){
  k <- sd/mean
  u <- qchisq(c(1 - alpha/2, alpha/2), n - 1)
  ci <- k/sqrt( ( (u + 2)/n - 1)*k^2 + (u/(n-1)))

  name_list = list(paste0('Coefficient of Variation ', ifelse(pct, '(%)', '')),
                   c('Estimate',
                      paste0('Lower ', 100*(1-alpha), '% CI'),
                      paste0('Upper ', 100*(1-alpha), '% CI')))
  array(ifelse(pct, 100, 1)*c(k, ci[1], ci[2]),
        dim = c(1, 3), dimnames = name_list)
}

#' @title Post hoc power analysis of contrasts
#'
#' @description This function calculates power of contrasts specified in an emmGrid
#'   object. Code is adapted from the SAS macro powercalc, by Stephen Bowley.
#'
#' @param object An emmGrid object (from the emmeans package): the result of a call
#'   contrast(emmeans(model))
#' @param alpha Numeric. Coverage for confidence intervals.
#'
#' @return A data frame containing the power, along with values used to calculate it, for
#'   each contrast.
#'
#'
#' @export
post_hoc_power <- function(object, alpha = 0.05, denDF = NULL) {
  is_test <- 'summary_emm' %in%  class(object)
  is_contr <- ('emmGrid' %in% class(object)) & ("contrast" %in% names(levels(object)))
  if (!(is_test || is_contr)){
    stop(simpleError("Object must either be an emmGrid object resulting from call to emmeans::contrasts()\n
                     or the result of a call to emmeans::joint_tests()"))
  }
  contr <- as.data.frame(object)
  if (is_contr){
    if (!is.null(denDF)){
      contr$df <- denDF
    }
    fval <- (contr$t.ratio)^2
    fcrit <- sapply(contr$df, qf, p = 1-alpha, df1 = 1)
    pow <- 1-mapply(pf, q = fcrit, df2 = contr$df, ncp = fval, MoreArgs = list(df1 = 1))
    data.frame(Contrast = levels(object)$contrast,
               NumDF = 1, DenDF = contr$df, alpha = alpha,
               NCP = fval, F_crit = fcrit, Power = pow)
  } else {
    if (!is.null(denDF)){
      contr$df2 <- denDF
    }
    fval <- contr$df1*contr$F.ratio
    fcrit <- mapply(qf, df1 = contr$df1, df2 = contr$df2, MoreArgs = list(p = 1-alpha))
    pow <- 1-mapply(pf, q = fcrit, df1 = contr$df1, df2 = contr$df2, ncp = fval)
    data.frame(Contrast = contr$`model term`,
               NumDF = contr$df1, DenDF = contr$df2, alpha = alpha,
               NCP = fval, F_crit = fcrit, Power = pow)
  }
}

#' @title Compute orthogonal polynomial contrasts using the algorithm of Emerson (1968)
#'
#' @description The returns a matrix of orthogonal polynomial contrasts for any
#'  arbitrarily spaced and weighted set of points up to the order specified in the
#'  \code{degree} argument.
#'
#' @param x A numeric vector of data points.
#' @param w An optional vector of weights. If NULL, weights are set to a vector of ones
#'   the length of x.
#' @param degree The highest order polynomial for which contrast coefficients should be
#'   returned. Values greater than x are ignored.
#' @return An array, with columns corresponding to the values of \code{x} and rows
#'   containing the corresponding polynomial contrast coefficients for orders 0 to
#'   \code{degree}.
#'
#' @references Emerson, P. L. (1968). Numerical Construction of Orthogonal Polynomials
#'   from a General Recurrence Formula. Biometrics, 24(3), 695–701.
#'   https://doi.org/10.2307/2528328
#'
#' @examples
#' # Reproduce results from page 700 of Emerson (1968):
#' values <- c(0, 2, 3, 5)
#' emerson_poly(x = values, w = c(2, 6, 6, 2))
#'
#' @export
emerson_poly <- function(x, w = NULL, degree = length(x)-1){
  n <- length(x)
  if (is.null(w)){
    w <- rep(1, n)
  }
  if (degree > (n-1)){
    degree <- n-1
  }
  A <- B <- vector('numeric', degree+1)
  P <- Q <- array(dim = c(degree+1, n), dimnames = list(0:degree, x))
  names(dimnames(P)) <- c('order', deparse(substitute(x)))
  for (j in 1:(degree+1)){
    if (j == 1){
      q <- rep(1, n)
      a <- sqrt(sum(w*q^2))
      p_old <- 0
      p <- q/a
      Q[j, ] <- q
      P[j, ] <- p
      A[j] <- a
      B[j] <- n
    } else {
      b <- sum(x*w*q^2/sum(w*q^2))
      q <- (x - b)*p - a*p_old
      a <- sqrt(sum(w*q^2))
      p_old <- p
      p <- q/a
      Q[j, ] <- q
      P[j, ] <- p
      A[j] <- a
      B[j] <- b
    }
  }
  attr(P, 'Q') <- Q
  attr(P, 'A') <- A
  attr(P, 'B') <- B
  return(P)
}

#' @title Diagnosing multicollinearity in regression models
#'
#' @description Calculate eigenvalues, conditional index and variance decomposition proportion
#'   for a fitted model.
#'
#' @param object A fitted object.
#' @param digits the number of digits to print
#'
#' @references Hallahan, Charlie. 1995. “Understanding the Collineartiy Diagnostics in
#'   Sas/Insight and Proc Reg.” In NESUG 1995, 801–4. Washington, D.C: Northeast SAS Users
#'   Group. https://lexjansen.com/nesug/nesug95/NESUG95147.pdf.
#'
#' @export
collinearity_diagnostic <- function(object, digits = 4){
  X <- model.matrix(object)
  Xs <- scale(X, center = F, scale = sqrt(colSums(X^2)))

  lambda <- svd(Xs)$d^2
  cond_index <- sqrt(lambda[1]/lambda)
  v2 <- svd(Xs)$v^2
  phi <- (t(v2)/lambda)
  p <- prop.table(phi, 2)
  out <- cbind(lambda, cond_index, p)
  colnames(out) <- c('Eigenvalue', 'Conditional Index', colnames(X))
  round(out, digits)
}

#' @title Calculate adjusted rand index
#'
#' @description This function is taken verbatim from the `mclust` package.
#'
#' @param x,y Vector of classifications
#'
#' @export
ari <- function(x, y){
    x <- as.vector(x)
    y <- as.vector(y)
    if (length(x) != length(y))
      stop("arguments must be vectors of the same length")
    tab <- table(x, y)
    if (all(dim(tab) == c(1, 1)))
      return(1)
    a <- sum(choose(tab, 2))
    b <- sum(choose(rowSums(tab), 2)) - a
    c <- sum(choose(colSums(tab), 2)) - a
    d <- choose(sum(tab), 2) - a - b - c
    ARI <- (a - (a + b) * (a + c)/(a + b + c + d))/((a + b +                                                      a + c)/2 - (a + b) * (a + c)/(a + b + c + d))
    return(ARI)
  }


