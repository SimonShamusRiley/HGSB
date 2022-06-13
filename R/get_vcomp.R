#' @title Extract the \strong{X}, \strong{Z}, \strong{G}, \strong{R} and \strong{V}
#' matrices from a fitted model
#'
#' @description The method defined for \code{\link[nlme]{lme}} objects is nearly identical
#'   to the \code{\link[mgcv]{extract.lme.cov}} function from which this function was
#'   adapted, except that it returns both the marginal variance-covariance matrix and the
#'   intermediate outputs used to calculate it, along with the design matrix, rather than
#'   only the marginal variance-covariance matrix itself. For \code{merMod} objects (fit
#'   with \code{\link[lme4]{lmer}}), the \code{\link[lme4]{getME}} function is used
#'   repeatedly to extract/construct the same matrices.
#'
#' @param x A fitted model of class \code{\link[nlme]{lme}} or \code{\link[lme4]{merMod}}.
#' @param data 	The data frame to be used in constructing the design matrix. Uses the data stored in the
#'   model object if not supplied. See details for more information.
#'
#' @details This function returns the design matrix, \strong{X}, the incidence matrix,
#'   \strong{Z}, the random effects covariance
#'   matrix, \strong{G}, the residual covariance matrix, \strong{R}, and the marginal covariance matrix, \strong{V}. These
#'   matrices are useful for calculating the covariance matrix of the fixed and
#'   random effects "estimates" together, which is needed for the calculation
#'   of the square root of the estimated projection variance for EBLUPs. The
#'   marginal variance covariance matrix \strong{V} is also returned.
#'
#'   When the "na action" of e.g. lme removes some rows from the data set during the model
#'   fitting process, it may be required to supply the abridged data set manually using
#'   the \code{data} argument. If a different data set than the one used to fit the model
#'   using \code{nlme::lme} is supplied to the \code{data} argument, the model is refit to
#'   the new data set while attempting to hold constant the random effects, correlation
#'   and variance estimates of the original model before extracting the \strong{G},
#'   \strong{V}, etc. matrices. This functionality remains in development and should not be
#'   employed.
#'
#' @return A list of matrices.
#'
#' @references Stroup, W., Milliken, G., Claasen, E., Wolfinger, R. (2018).
#'   "SAS for Mixed Models: Introduction and Basic Applications". Cary, NC: SAS
#'   Institute, Inc.
#' @references Wood, S. (2021). mgcv: Mixed GAM Computation Vehicle with
#'   Automatic Smoothness Estimation (1.8-36). \code{\url{https://CRAN.R-project.org/package=mgcv}}
#'
#' @export
get_vcomp = function(x, ...){
  UseMethod('get_vcomp', x)
}

#' @exportS3Method
get_vcomp.lme = function (x, data = NULL) {
  start.level = 1

  if (is.null(data)) {
    na.act <- na.action(x)
    data <- if (is.null(na.act))
      x$data
    else x$data[-na.act, ]
  }

  refit <- update(x, data = data)
  grps <- nlme::getGroups(refit)
  n <- length(grps)
  if (is.null(x$modelStruct$varStruct)) {
    w <- rep(x$sigma, n)
  } else {
    same_coef_order <- all(attr(refit$modelStruct$varStruct, 'groupNames') == attr(x$modelStruct$varStruct, 'groupNames'))
    if (!same_coef_order){
      warning(simpleWarning('Differences in the grouping levels or their ordering in the new data set may be causing variance parameters to be misspecified; The V matrix may be incorrect'))
    }
    coef(refit$modelStruct$varStruct) <- coef(x$modelStruct$varStruct)
    w <- 1/nlme::varWeights(refit$modelStruct$varStruct)
    group.name <- names(x$groups)
    order.txt <- paste("ind<-order(data[[\"", group.name[1],
                       "\"]]", sep = "")
    if (length(x$groups) > 1)
      for (i in 2:length(x$groups)) order.txt <- paste(order.txt,
                                                       ",data[[\"", group.name[i], "\"]]",
                                                       sep = "")
    order.txt <- paste(order.txt, ")")
    eval(parse(text = order.txt))
    w[ind] <- w
    w <- w * x$sigma
  }
  if (is.null(x$modelStruct$corStruct)){
    V <- diag(n)
  } else {
    coef(refit$modelStruct$corStruct) <- coef(x$modelStruct$corStruct)
    c.m <- nlme::corMatrix(refit$modelStruct$corStruct)
    if (!is.list(c.m))
      V <- c.m
    else {
      V <- matrix(0, n, n)
      gr.name <- names(c.m)
      n.g <- length(c.m)
      j0 <- 1
      ind <- ii <- 1:n
      for (i in 1:n.g) {
        j1 <- j0 + nrow(c.m[[i]]) - 1
        V[j0:j1, j0:j1] <- c.m[[i]]
        ind[j0:j1] <- ii[grps == gr.name[i]]
        j0 <- j1 + 1
      }
      V[ind, ] <- V
      V[, ind] <- V
    }
  }
  V <- as.vector(w) * t(as.vector(w) * V)
  R <- V
  X <- list()
  grp.dims <- x$dims$ncol
  coef(refit$modelStruct$reStruct) <- coef(x$modelStruct$reStruct)
  Zt <- model.matrix(refit$modelStruct$reStruct, data)
  cov <- as.matrix(refit$modelStruct$reStruct)
  i.col <- 1
  n.levels <- length(x$groups)
  Z <- matrix(0, n, 0)
  if (start.level <= n.levels) {
    for (i in 1:(n.levels - start.level + 1)) {
      if (length(levels(refit$groups[[n.levels - i + 1]])) ==
          1) {
        X[[1]] <- matrix(rep(1, nrow(refit$groups)))
      }
      else {
        clist <- list(`refit$groups[[n.levels - i + 1]]` = c("contr.treatment",
                                                             "contr.treatment"))
        X[[1]] <- model.matrix(~refit$groups[[n.levels -
                                                i + 1]] - 1, contrasts.arg = clist)
      }
      X[[2]] <- Zt[, i.col:(i.col + grp.dims[i] - 1), drop = FALSE]
      i.col <- i.col + grp.dims[i]
      Z <- cbind(Z, mgcv::tensor.prod.model.matrix(X))
    }
    Vr <- matrix(0, ncol(Z), ncol(Z))
    start <- 1
    for (i in 1:(n.levels - start.level + 1)) {
      k <- n.levels - i + 1
      for (j in 1:x$dims$ngrps[i]) {
        stop <- start + ncol(cov[[k]]) - 1
        Vr[start:stop, start:stop] <- cov[[k]]
        start <- stop + 1
      }
    }
    Vr <- Vr * x$sigma^2
  }

  list(X = model.matrix(formula(x), data), G = Vr, R = R, Z = Z, V = Z %*% Vr %*% t(Z) + R)
}

#' @exportS3Method
get_vcomp.merMod <- function(x){
  X <- lme4::getME(x, 'X')
  G_vec <- as.vector(unlist(mapply(FUN = rep, x = unlist(lme4::VarCorr(x)), times = diff(lme4::getME(x, 'Gp')))))
  G <- diag(x = G_vec)
  R <- diag(sigma(x)^2, nrow = nrow(x@frame))
  Z <- as.matrix(lme4::getME(x, 'Z'))
  list(X = X, G = G, R = R, Z = Z, V = Z %*% G %*% t(Z) + R)
}
#' @exportS3Method
get_vcomp.gls <- function(model, data){
  if (missing(data)){
    data <- nlme::getData(model)
  }

  modcomps <- names(model$modelStruct)

  grps <- nlme::getGroups(model)
  n <- ifelse(is.null(grps), nrow(data), length(grps))

  if ('varStruct' %in% modcomps) {
    w <- 1/nlme::varWeights(model$modelStruct)*sigma(model)
  } else {
    w <- rep(sigma(model), n)
  }

  if ('corStruct' %in% modcomps){
    c.m <- nlme::corMatrix(model$modelStruct$corStruct)
    if (!is.list(c.m))
      V <- c.m
    else {
      V <- matrix(0, n, n)
      gr.name <- names(c.m)
      n.g <- length(c.m)
      j0 <- 1
      ind <- ii <- 1:n
      for (i in 1:n.g) {
        j1 <- j0 + nrow(c.m[[i]]) - 1
        V[j0:j1, j0:j1] <- c.m[[i]]
        ind[j0:j1] <- ii[grps == gr.name[i]]
        j0 <- j1 + 1
      }
      V[ind, ] <- V
      V[, ind] <- V
    }
  } else {
    V <- diag(n)
  }

  V <- as.vector(w) * t(as.vector(w) * V)
  list(X = model.matrix(model, data), V = V)
}

get_vcomp.glmmTMB <- function(x){
  X <- lme4::getME(x, 'X')
  G_vec <- as.vector(unlist(mapply(FUN = rep, x = unlist(lme4::VarCorr(x)), times = diff(lme4::getME(x, 'Gp')))))
  G <- diag(x = G_vec)
  R <- diag(sigma(x)^2, nrow = nrow(x@frame))
  Z <- as.matrix(lme4::getME(x, 'Z'))
  list(X = X, G = G, R = R, Z = Z, V = Z %*% G %*% t(Z) + R)
}

#' @title Calculate EBLUPs, contrasts, and other linear functions for a fitted
#'   \code{nlme::lme()} \code{lme4::lmer()} model
#'
#' @description Uses the methods described in Appendix A of Stroup et al. (2018) to
#'   calculate marginal means, EBLUPs, or single degree of freedom contrasts, along with
#'   their "standard errors" (the square root of the estimated projection variance), t
#'   statistics and p-values or - in the case of multiple degree of freedom contrasts -
#'   the F-statistic and p-value of the test.
#'
#' @param object A fitted \code{\link[nlme]{lme}} or \code{\link[lme4]{lmer}} model
#' @param predictions Either a numeric vector or matrix specifying predictions to make and
#'   test, or a (optionally) named list of such matrices or vectors.
#'
#' @details The predictions to be calculated must be supplied in the form of a named list,
#'   with each list item consisting of a contrast specification. The contrast coefficients
#'   must be ordered following the list of terms generated via \code{fit$coefficients}.
#'   Note that this implies that how a given prediction is specified with differ depending
#'   on the settings of \code{options()$contrasts}. See \code{\link[stats]{contrasts}} for
#'   details. If a matrix is supplied, each line is taken to be a seperate contrast
#'   specification and an F-test is conducted.
#'
#'   Note that Stroup et al. (2018) advise that prediction variance estimates (and thus
#'   the "standard error" in the function's output) tend to be downwardly biased,
#'   sometimes severely so.
#'
#' @return A list of one or two data frames corresponding to the single and multiple
#'   degree of freedom predictions/contrasts, respectively.
#'
#' @references Stroup, W., Milliken, G., Claasen, E., Wolfinger, R. (2018).
#'   "SAS for Mixed Models: Introduction and Basic Applications". Cary, NC: SAS
#'   Institute, Inc.
#'
#' @examples
#' library(HGSB)
#' \dontrun{library(nlme)}
#'
#' #Example drawn from page 313 of Stroup et al. (2018):
#' breeding <- data.frame(sire = rep(1:5, each = 4),
#'                       dam = rep(c(1, 1, 2, 2), 5),
#'                       adg = c(2.24, 1.85, 2.05, 2.41, 1.99, 1.93, 2.72,
#'                               2.32, 2.33, 2.68, 2.69, 2.71, 2.42, 2.01,
#'                               1.86, 1.79, 2.82, 2.64, 2.58, 2.56))
#'
#'pred.list <- list('sire 1 BLUP "broad"' = c(1,
#'                                            1, 0, 0, 0, 0,
#'                                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
#'                  'sire 1 BLUP "narrow"' = c(2,
#'                                             2, 0, 0, 0, 0,
#'                                             1, 1, 0, 0, 0, 0, 0, 0, 0, 0)/2,
#'                  'sire 1 BLUP dam 1' = c(1,
#'                                          1, 0, 0, 0, 0,
#'                                          1, 0, 0, 0, 0, 0, 0, 0, 0, 0))
#'
#'fit <- lme(adg ~ 1, breeding, ~ 1|sire/dam)
#'
#'# Predictions are identical, but standard errors lack Kenward-Roger adjustment
#'# and degrees of freedom differ must be set manually
#'eblup(fit, pred.list, df = 4)
#'
#'
#'@export
eblup <- function(object, predictions, df = NULL){
  # Verify that arguments are valid
  if (!inherits(object, "lme") & !inherits(object, 'lmerMod')){
    stop(simpleError("object must be of class lme"))
  }
  if (!all(sapply(predictions, function(x) {is.vector(x) | is.matrix(x) | is.array(x)}))) {
    stop(simpleError('predictions must be a list of vectors, matrices or arrays'))
  }
  if (!is.list(predictions)){
    predictions = list('Linear Function:' = predictions)
  }
  if (is.null(names(predictions))){
    names(predictions) = paste0('Linear Function ', 1:length(predictions), ':')
  }
  options(contrasts = c('contr.sum', 'contr.poly'))
  object = update(object)
  # Calculation of standard errors and CI for eblups requires X, Z, G and R matrices
  vcomp <- get_vcomp(object)
  X <- vcomp$X
  G <- vcomp$G
  R <- vcomp$R
  Z <- vcomp$Z
  drop_empty <- colSums(Z) > 0
  Z <- Z[ , drop_empty]
  G <- G[drop_empty, drop_empty]

  # From Stroup, W., Milliken, G., Claasen, E., Wolfinger, R. (2018)
  M1 <- t(X) %*% solve(R) %*% X
  M2 <- t(Z) %*% solve(R) %*% X
  M3 <- t(X) %*% solve(R) %*% Z
  M4 <- t(Z) %*% solve(R) %*% Z + solve(G)
  M <- cbind(rbind(M1, M2), rbind(M3, M4))
  C <- solve(M)

  # Enforce consistency in predictions by making vectors 1-column matrices
  L <- lapply(predictions, function(x) {
    if (is.vector(x)){
      matrix(x, nrow = 1)
    } else {
      x
    }
  })

  # Calculate predictions
  fe_id <- seq(length(fixef(object)))
  B <- matrix(c(fixef(object), unlist(ranef(object)))[drop_empty])
  preds <- sapply(L, `%*%`, B)

  # Rank of prediction matrices determines t- vs. F-test (with "v" den degree freedom)
  r <- sapply(L, Matrix::rankMatrix)
  v <- ifelse(is.null(df),
              ifelse(class(object) == 'lme',
                     object$fixDF$terms[1],
                     getME(object, 'devcomp')$dims['nmp']),
              df)

  single_df <- r == 1

  Ls <- L[single_df]
  Lm <- L[!single_df]

  any_single <- length(Ls) > 0
  if (any_single){
    se <- sapply(Ls, function(L, C) {
      sqrt(L %*% C %*% t(L))
    }, C = C)

    tstat  <- unlist(preds[single_df])/se
    tp <- 2*pt(-abs(tstat), v)
    single_colnames <- c("Prediction", "Std. Err", "DF",'t Value', 'Pr > |t|')
    single_out <- data.frame(unlist(preds[single_df]), se, v, tstat, tp)
    colnames(single_out) <- single_colnames
  } else {
    single_out <- NULL
  }
  any_multi <- length(Lm) > 0
  if (any_multi){
    fstat <- mapply(function(L, B, C, r) {
      (t(B) %*% t(L) %*% solve(L %*% C %*% t(L)) %*% L %*% B)/r
    }, L = Lm, r = r[!single_df], MoreArgs = list(B = B, C = C))

    fp <- 1 - pf(fstat, r[!single_df], v)
    test_names <- c("Num DF", 'Den DF', 'F Value', "Pr > F")
    multi_out <- data.frame(r[!single_df], v, fstat, fp)
    colnames(multi_out) <- test_names
  } else {
    multi_out <- NULL
  }

  # format output
  out = list("Single DF Predictions/Estimates" = single_out, "Multiple DF Contrasts" = multi_out)[c(any_single, any_multi)]

  if (sum(c(any_single, any_multi)) == 1){
    out <- out[[1]]
  }
    out
}

# old code
# eblup = function(object, predictions, df = NULL){
#   if (!inherits(object, "lme"))
#     stop("object does not appear to be of class lme")
#
#   X <- model.matrix(formula(object), object$data)
#   V <- HGSB::get_vcomp(object)
#   G <- V$G
#   R <- V$R
#   Z <- V$Z
#
#   M1 <- t(X) %*% solve(R) %*% X
#   M2 <- t(Z) %*% solve(R) %*% X
#   M3 <- t(X) %*% solve(R) %*% Z
#   M4 <- t(Z) %*% solve(R) %*% Z + solve(G)
#   M <- cbind(rbind(M1, M2), rbind(M3, M4))
#   C <- solve(M)
#
#   B <- matrix(c(fixef(object), unlist(ranef(object))))
#   preds <- sapply(predictions, `%*%`, B)
#   se <- sapply(predictions, function(L, C) {
#     L <- matrix(unlist(L), nrow = 1)
#     sqrt(L %*% C %*% t(L))
#     }, C = C)
#   tstat <- preds/se
#   v <- ifelse(is.null(df), object$fixDF$terms[1], df)
#   pval <- 2*pt(-abs(tstat), v)
#   out <- data.frame(preds, se, v, tstat, pval)
#   colnames(out) <- c("Prediction", "Std. Err", "DF", "t Value", "Pr > |t|")
#
#   out
# }
#
#
#
#' @title Calculate the random effects estimates implied in a marginal (compound symmetric)
#' model
#'
#' @description Given the mathematical equivalence between a conditional model with independent
#'  random effects and a compound symmetric correlation structure, its possible to calculate the
#'  random effects estimates implied by the latter:
#'  \deqn{\mathbf{G}\mathbf{Z}'\mathbf{V}^{-1}(Y - \mathbf{X}\beta)}{GZ'V^ (-1) (Y - X \beta)}
#'
#' @param object An \code{\link[nlme]{lme}} or \code{\link[nlme]{gls}} object containing a
#'   corCompSymm correlation structure.
#' @param data 	The data frame used in fitting \code{object}. Required if \code{object} is
#'   of class \code{gls}
#' @details
#'
#' @return A vector of random effects estimates.
#'
#' @references Stroup, W., Milliken, G., Claasen, E., Wolfinger, R. (2018).
#'   "SAS for Mixed Models: Introduction and Basic Applications". Cary, NC: SAS
#'   Institute, Inc.
predict_cs_conditional <- function(object, data){
  check_cs <- 'corCompSymm' %in% class(object$modelStruct$corStruct)
  check_gls <- 'gls' %in% class(object)
  check_lme <- 'lme' %in% class(object)
  if (missing(data)){
    data <- getData(object)
  }
  if (!check_cs){
    stop(simpleError('object does not have compound symmetry correlation structure'))
  }
  if (!check_cs & !check_lme){
    stop(simpleError('object must be of class gls or of class lme'))
  }

  if (check_gls){
    randform <- attr(object$modelStruct$corStruct, 'formula')
    fixform <- formula(object)
    refit <- lme(fixed = fixform, data = data, random = randform)
    vc <- getVarCov(object)

    G <- diag(rep(vc[1, 2], dim(get_vcomp(refit)$G)[1]))
    Z <- get_vcomp(refit)$Z
    R <- diag(rep(vc[1, 1]), dim(get_vcomp(refit)$V)[1])
    V <- Z %*% G %*% t(Z) + R
    respvar <- names(attr(refit$terms, 'dataClasses')[1])
    Y <- data[, respvar]
    X <- model.matrix(fixform, data = data)
    B <- coef(object)
    u <- G %*% t(Z) %*% solve(V) %*% (Y - X %*% B)

  } else {
    vc <- mgcv::extract.lme.cov2(object)$V[[1]]

    G <- diag(rep(vc[1, 2], dim(get_vcomp(object)$G)[1]))
    Z <- get_vcomp(object)$Z
    R <- diag(rep(vc[1, 1]), dim(get_vcomp(object)$V)[1])
    V <- get_vcomp(object)$V #Z %*% G %*% t(Z) + R
    respvar <- names(attr(object$terms, 'dataClasses')[1])
    Y <- data[, respvar]
    X <- model.matrix(formula(object), data = data)
    B <- fixef(object)
    u <- G %*% t(Z) %*% solve(V) %*% (Y - X %*% B)
  }
  return(u)
}

#' @title Extract the fixed and random effects in a fitted model, ordered for use in
#'   specifying estimable linear functions.
#'
#' @description This is a convenience function to aid in the specification of estimable linear
#'   functions where the number of fixed and/or random effects in the model is large.
#'
#' @param object A model fitted with \code{\link[nlme]{lme}} or \code{\link[lme4]{lmer}}.
#'
#' @return A column vector of the names of fixed- and random-effects coefficients in \code{object}.
#'
#' @examples
#' library(HGSB)
#' \dontrun{library(lme4)}
#'data(trefoil)
#'fit <- lmer(seed_g ~ 1 + (1|family) + (1|block), trefoil)
#'
#'eblup_terms(fit)
#'
#'\dontrun{library(nlme)}
#'fit <- lme(seed_g ~ 1, trefoil, random = list(family = ~ 1, family = pdIdent(form = ~ block)))
#'
#'eblup_terms(fit)
#'
#'@export
eblup_terms <- function(object){
  if (!inherits(object, 'lme') & !inherits(object, 'merMod')){
    stop(simpleError('Object must be of class "lme" or "merMod'))
  }
  terms <- c(fixef(object), unlist(ranef(object)))
  terms <- terms[terms != 0]
  terms <- names(terms)
  matrix(terms, ncol = 1, dimnames = list(Index = 1:length(terms), 'Coefficient'))
}
