model.matrix.reStruct <- function (object, data, contrast = NULL, ...){
  if (is.null(form <- formula(object, asList = TRUE))) {
    stop("cannot extract model matrix without formula")
  }
  form1 <- asOneFormula(form)
  if (length(form1) > 0) {
    data <- model.frame(form1, data = data)
  } else {
    data <- data.frame(`(Intercept)` = rep(1, nrow(data)))
  }
  idents <- sapply(object, function(x) {'pdIdent' %in% class(x)})
  for (i in which(idents)){
    form[[i]] = ~1
  }
  any2list <- function(object, data, contrast) {
    form2list <- function(form, data, contrast) {
      if (length(asOneFormula(form)) == 0) {
        return(list(`(Intercept)` = rep(1, dim(data)[1])))
      }
      as.data.frame(unclass(model.matrix(form, model.frame(form,
                                                           data), contrast)))
    }
    if (inherits(object, "formula")) {
      return(form2list(object, data, contrast))
    }
    if (is.list(object)) {
      return(unlist(lapply(object, form2list, data = data,
                           contrast = contrast), recursive = FALSE))
    }
    return(NULL)
  }
  value <- as.list(lapply(form, any2list, data = data, contrast = contrast))
  contr <- as.list(lapply(as.data.frame(data), function(x) if (inherits(x,
                                                                        "factor") && length(levels(x)) > 1)
    contrasts(x)
    else NULL))
  contr[names(contrast)] <- contrast
  ncols <- lengths(value)
  nams <- if (length(value) == 1) {
    names(value[[1]])
  }
  else {
    paste(rep(names(value), ncols), unlist(lapply(value,
                                                  names)), sep = ".")
  }
  structure(matrix(unlist(value), nrow = nrow(data), dimnames = list(row.names(data),
                                                                     nams)), ncols = ncols, nams = lapply(value, names), contr = contr)
}

#' @title Extract the \strong{Z}, \strong{G}, \strong{R} and \strong{V}
#' matrices from a fitted model
#'
#' @description The marginal variance-covariance matrix \strong{V} for fitted model
#'  mixed effects model is equal to:
#'  V = ZGZ' + R
#'  Where G is the random effects variance-covariance matrix, R is residual variance-covariance
#'  matrix and Z is the random effects design matrix. These matrices are useful for calculating,
#'  for example, EBLUPs and their standard errors.
#'
#' @param x A fitted model. Currently, models of the following class are supported: "lme", "lmerMod".
#' @param data 	The data frame to be used in constructing the design matrix. Uses the data stored in the
#'   model object if not supplied. See details for more information.
#'
#' @details This function returns the incidence matrix,
#'   \strong{Z}, the random effects covariance
#'   matrix, \strong{G}, the residual covariance matrix, \strong{R}, and the marginal covariance matrix, \strong{V}. These
#'   matrices are useful for calculating the covariance matrix of the fixed and
#'   random effects "estimates" together, which is needed for the calculation
#'   of the square root of the estimated projection variance for EBLUPs. The
#'   marginal variance covariance matrix \strong{V} is also returned.
#'
#'   For models of class "lme", the code employed is adapted from the mcgv::extract.lme.cov() function
#'   of Wood (2021), with the most significant modification being adjustments which correct
#'   the G and Z matrices when crossed random effects are specified.
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
get_vcomp.lme <- function(x, data = NULL){
  if (!inherits(x, "lme"))
    stop("object does not appear to be of class lme")
  if (is.null(data)) {
    na.act <- na.action(x)
    data <- if (is.null(na.act))
      x$data
    else x$data[-na.act, ]
  }

  # x$groups, x$dims$ngrp and x$dims$ncol are all misspecified when crossed random
  # effects are present; this corrects those
  forms = formula(reStruct(x$modelStruct$reStruct))
  nested <- names(forms)
  nms <- gsub(x = as.character(forms), pattern = "[^[:alpha:]]", replacement = '')
  nms <- mapply(paste0, names(forms), nms, MoreArgs = list(collapse = '_'))
  grps <- lapply(forms, model.frame, data = data)
  names(grps) <- nms
  if (any(pull <- sapply(grps, length)==0)){
    pull <- names(grps)[pull]
    for (c in pull){
      grps[[c]] <- data[, c, drop = F]
    }
  }
  grps <- as.data.frame(grps)
  grps[, 1:ncol(grps)] <- lapply(grps[, 1:ncol(grps), drop = F], as.character)

  for (n in 1:ncol(grps)){
    grps[, n] <- factor(paste0(grps[, n], as.character(data[, nested[n]])),
                        levels = unique(paste0(grps[, n], as.character(data[, nested[n]]))))
  }
  grps <- grps[, 1:ncol(grps), drop = F]

  x$groups <- grps

  idents <- sapply(x$modelStruct$reStruct, function(x) {'pdIdent' %in% class(x)})
  x$dims$ngrps <- sapply(x$groups[, ncol(x$groups):1, drop = F], nlevels)
  x$dims$ncol[which(idents)] <- 1

  for (w in which(idents)){
    attr(x$modelStruct$reStruct[[w]], 'ncol') = 1
    attr(x$modelStruct$reStruct[[w]], 'Dimnames') = list("(Intercept)", "(Intercept)")
  }

  grps <- nlme::getGroups(x)
  n <- length(grps)
  if (is.null(x$modelStruct$varStruct))
    w <- rep(x$sigma, n)
  else {
    w <- 1/nlme::varWeights(x$modelStruct$varStruct)
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
  if (is.null(x$modelStruct$corStruct))
    V <- diag(n)
  else {
    c.m <- nlme::corMatrix(x$modelStruct$corStruct)
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
  M <- list()

  grp.dims <- x$dims$ncol
  Zt <- model.matrix(x$modelStruct$reStruct, data)
  cov <- as.matrix(x$modelStruct$reStruct)
  i.col <- 1
  n.levels <- length(x$groups)
  Z <- matrix(0, n, 0)
  for (i in 1:n.levels) {
    if (length(levels(x$groups[[n.levels - i + 1]])) ==
        1) {
      M[[1]] <- matrix(rep(1, nrow(x$groups)))
    }
    else {
      clist <- list(`x$groups[[n.levels - i + 1]]` = c("contr.treatment",
                                                       "contr.treatment"))

      M[[1]] <- model.matrix(~x$groups[[n.levels -
                                          i + 1]] - 1, contrasts.arg = clist)
    }
    M[[2]] <- Zt[, i.col:(i.col + grp.dims[i] - 1), drop = FALSE]
    i.col <- i.col + grp.dims[i]
    Z <- cbind(Z,  mgcv::tensor.prod.model.matrix(M))
  }
  Vr <- matrix(0, ncol(Z), ncol(Z))
  start <- 1
  for (i in 1:n.levels) {
    k <- i #n.levels - i + 1
    for (j in 1:x$dims$ngrps[i]) {
      stop <- start + ncol(cov[[k]]) - 1
      Vr[start:stop, start:stop] <- cov[[k]]
      start <- stop + 1
    }
  }
  Vr <- Vr * x$sigma^2
  V <- V + Z %*% Vr %*% t(Z)
  list(Z = Z, G = Vr, R = R, V = V)
}

#' @exportS3Method
get_vcomp.lmerMod <- function(x){
 # X <- lme4::getME(x, 'X')
  G_vec <- as.vector(unlist(mapply(FUN = rep, x = unlist(lme4::VarCorr(x)), times = diff(lme4::getME(x, 'Gp')))))
  G <- diag(x = G_vec)
  R <- diag(sigma(x)^2, nrow = nrow(x@frame))
  Z <- as.matrix(lme4::getME(x, 'Z'))
  list(Z = Z, G = G, R = R,  V = Z %*% G %*% t(Z) + R)
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
  list(V = V)
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
#'   must be ordered as per the results generated from a call to \code{eblup_terms()},
#'   which accounts for the fact that the model will be refitted (if necessary) using
#'   sum-to-zero contrasts and that the Z matrix, and thus the projection matrix, are
#'   constructed such that the ordering of terms may not agree with that generated from a
#'   call to fixef and/or ranef. Note that this implies that how a given prediction is
#'   specified with differ depending If a matrix is supplied, each row is taken to be a
#'   seperate contrast specification and an F-test is conducted.
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
#'# and degrees of freedom differ and must be set manually
#'eblup(fit, pred.list, df = 4)
#'
#'
#'@export
eblup <- function(object, predictions, df = NULL){
  # Verify that arguments are valid
  if (!inherits(object, "lme") & !inherits(object, 'lmerMod')){
    stop(simpleError(paste0("objects of class ", class(object), " are not supported")))
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
  if ('lme' %in% class(object)){
    contr_list <- attr(model.matrix(formula(object), object$data), 'contrasts')
    sum_zero <- sapply(object$contrasts, function(x){
      ncol(x) == 1 | all(colSums(x) == 0)
    })
    if (!all(sum_zero)){
      warning(simpleWarning('Refitting model with sum-to-zero contrasts; see help("eblup_term") for details'))
      contr_list <- lapply(contr_list, `[<-`, 'contr.sum')
      object <- update(object, contrasts = contr_list)
    }
    X <- model.matrix(formula(object), object$data, contrasts.arg = contr_list)
  } else if ('lmerMod' %in% class(object)){
    options(contrasts = c('contr.sum', 'contr.poly'))
    object <- update(object)
    X <- getME(object, 'X')
  }

  vc <- get_vcomp(object)
  G <- vc$G
  Z <- vc$Z
  R <- vc$R

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
  #fe_id <- seq(length(fixef(object)))
  keep_real <-  which(unlist(ranef(object)) != 0)

  B <- matrix(c(fixef(object), unlist(ranef(object))[keep_real]))
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
#' @description This function provides the names of the terms, in proper order, for
#'   specifying contrast statements in calls to eblup(). Note that to correctly calculate
#'   the standard errors of eblups, the ordering of the terms must agree with that of the
#'   projection matrix, which may differ from the ordering of terms provided by calls to
#'   fixef() and/or ranef().
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
  if ('lme' %in% class(object)){
    contr_list <- attr(model.matrix(formula(object), object$data), 'contrasts')

    sum_zero <- sapply(object$contrasts, function(x){
      ncol(x) == 1 | all(colSums(x) == 0)
    })
    if (!all(sum_zero)){
      warning(simpleWarning('Refitting model with sum-to-zero contrasts'))
      contr_list <- lapply(contr_list, `[<-`, 'contr.sum')
      object <- update(object, contrasts = contr_list)
    }} else if ('lmerMod' %in% class(object)){
      options(contrasts = c('contr.sum', 'contr.poly'))
      object <- update(object)
    }
  data <- nlme::getData(object)
  forms <- formula(reStruct(object$modelStruct$reStruct))
  nested <- names(forms)
  nms <- gsub(x = as.character(forms), pattern = "[^[:alpha:]]", replacement = '')
  nms <- mapply(paste, names(forms), nms, MoreArgs = list(sep = ""))
  grps <- lapply(forms, model.frame, data = data)
  names(grps) <- nms
  if (any(pull <- sapply(grps, length)==0)){
    pull <- names(grps)[pull]
    for (c in pull){
      grps[[c]] <- data[, c, drop = F]
    }
  }
  grps <- as.data.frame(grps)
  grps[, 1:ncol(grps)] <- lapply(grps[, 1:ncol(grps), drop = F], as.character)

  for (n in 1:ncol(grps)){
    redundant <- mapply(regexpr, pattern = grps[, nested[n]], text = grps[, n])
    if (colnames(grps)[n] == nested[n] | all(redundant > 0)){
      grps[, n] <- factor(grps[, n], levels = unique(grps[, n]))
    } else {
      grps[, n] <- factor(paste0(grps[, n], as.character(data[, nested[n]])),
                          levels = unique(paste0(grps[, n], as.character(data[, nested[n]]))))
    }
  }
  grps <- grps[, ncol(grps):1, drop = F]
  u <- unlist(sapply(grps, levels), use.names = F)
  object$groups <- grps

  nlev <- sapply(object$groups, function(x){length(levels(x))})
  labs <- unlist(mapply(rep, x = names(object$groups), each = nlev))
  u <- paste(labs, unlist(sapply(object$groups, levels)), sep = ':')
  B <- c(names(fixef(object)), u)
  noquote(array(B, dim = c(length(B), 1), dimnames = list(seq(length(B)), 'Term:')))
}







