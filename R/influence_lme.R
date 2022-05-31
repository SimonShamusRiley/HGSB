#' @title Utilities for case deletion diagnostics for mixed models
#'
#' @aliases part_mat inv_subV
#'
#' @param M A (square) matrix.
#' @param n Integer. Denotes the row/column along which to partition the matrix M.
#' @param which Which of the four partitions of M matrix should be returned (see details).
#' @param V A marginal variance-covariance matrix.
#'
#' @details These functions are described in Christensen, Pearson & Johnson (1992). The
#'   four components of the M matrix are as follows: 1 = `M[n, n]`, 2 = `M[n, -n]`, 3 =
#'   `M[-n, -n]`, and 4 = `M[-n, n]`
#' @references Christensen, Ronald, Larry M. Pearson, and Wesley Johnson. 1992.
#'   “Case-Deletion Diagnostics for Mixed Models.” Technometrics 34 (1): 38.
#'   URL:https://doi.org/10.2307/1269550.
#' @export
part_mat <- function(M, n, which = c(1, 2, 3, 4)){
  switch(which,
         M[n, n],
         t(M[n, -n]),
         M[-n, -n],
         M[-n, n])
}
#' @export
inv_subV <- function(V){
  inv_V = solve(V)
  lapply(1:nrow(inv_V), function(n, M){
    L <- part_mat(M, n = n, which = 3)
    lc <- part_mat(M, n = n, which = 4)
    lr <- part_mat(M, n = n, which = 2)
    v_ii <- part_mat(M, n = n, which = 1)
    L - (lc%*%lr)/v_ii
  }, M = inv_V)
}

#' @exportS3Method
hatvalues.lme <- function(model, ...) {

  Y <- unlist(getData(model)[as.character(formula(model)[[2]])], use.names = F)
  X <- model.matrix(formula(model), getData(model))
  V <- get_vcomp(model)$V
  n <- nrow(X)
  p <- ncol(X)

  X_i <- lapply(1:nrow(X), FUN = function(n, M){
    matrix(M[row(M) != n], ncol = ncol(M))
  }, M = X)
  sV_i <- inv_subV(V)

  xbar <- matrix(nrow = n, ncol = p)
  for (i in 1:n){
    vi <- part_mat(V, i, 4)
    xbar[i, ] <- t(X[i, ] - t(X_i[[i]]) %*% (sV_i[[i]]) %*% vi)
  }

  hbar <-vector('numeric', n)
  for (i in 1:n){
    hbar[i] <- t(xbar[i, ]) %*% solve(t(X) %*% solve(V) %*% X) %*% xbar[i, ]
  }

  sbar <- vector('numeric', n)
  for (i in 1:n){
    vii <- part_mat(V, i, 1)
    tvi <- part_mat(V, i, 2)
    vi <- part_mat(V, i, 4)
    sbar[i] <- vii -  tvi %*% sV_i[[i]] %*% vi
  }
  hbar/sbar
}

#' @exportS3Method
rstandard.lme <- function(model, level = 0, ...){
  dat <- getData(model)
  V <- get_vcomp(model)$V
  X <- model.matrix(formula(model), dat)
  Q <- (X %*% solve(t(X) %*% solve(V) %*% X) %*% t(X))

  r <- resid(model, type = 'response', level = level, ...)
  if (level == 0){
    sdres <- r/sqrt(diag(V-Q))
  } else {
    grp <- names(model$groups)[level]
    Z <- get_vcomp(model)$Z
    G <- get_vcomp(model)$G
    K <- diag(max(dim(Z))) - Z %*% G %*% t(Z) %*% solve(V)
    sdres <- r/sqrt(diag(K%*%(V-Q)%*%t(K)))
  }
  return(sdres)
}

#' @exportS3Method
influence.lme <- function(model, groups, data, ncores = 1, ...) {
  if (is.infinite(ncores)) {
    ncores <- parallel::detectCores(logical = FALSE)
  }
  if (missing(data)) {
    data <- nlme::getData(model)
  }
  if (missing(groups)) {
    groups <- ".case"
    data$.case <- rownames(data)
  } else if (length(groups) > 1) {
    del.var <- paste0(groups, collapse = ".")
    data[, del.var] <- apply(data[, groups], 1, function(row) paste0(row,
                                                                     collapse = "."))
    groups <- del.var
  }

  unique.del <- unique(data[, groups])
  data$.groups <- data[, groups]

  fixed <- fixef(model)
  vc <- attr(model$apVar, "Pars")
  vcov.1 <- vector(length(unique.del), mode = "list")

  deleteGroup <- function(del) {
    data$del <- del
    mod.1 <- suppressWarnings(update(model, data = data,
                                     subset = .groups != del, control = nlme::lmeControl(returnObject = TRUE)))
    fixed.1 <- fixef(mod.1)
    vc.0 <- attr(mod.1$apVar, "Pars")
    vc.1 <- if (!is.null(vc.0))
      vc.0
    else rep(as.numeric(NA), length(vc))
    vcov.1 <- vcov(mod.1)
    list(fixed.1 = fixed.1, vc.1 = vc.1, vcov.1 = vcov.1)
  }
  refits <- if (ncores >= 2) {
    message("Note: using a cluster of ", ncores, " cores")
    cl <- parallel::makeCluster(ncores)
    on.exit(parallel::stopCluster(cl))
    parallel::clusterEvalQ(cl, require("nlme"))
    parallel::clusterApply(cl, unique.del, deleteGroup)
  }
  else {
    lapply(unique.del, deleteGroup)
  }
  refits <- unlist(refits, recursive = F)

  fixed.1 <- matrix(unlist(refits[which(names(refits) == 'fixed.1')]),
                    nrow = length(unique.del), ncol = length(fixed), byrow = T)
  rownames(fixed.1) <- unique.del
  colnames(fixed.1) <- names(fixed)

  if (is.null(unlist(refits[which(names(refits) == 'vc.1')]))){
    vc.1 <- NULL
  } else {
    vc.1 <- matrix(unlist(refits[which(names(refits) == 'vc.1')]),
                   nrow = length(unique.del), ncol = length(vc), byrow = T)
    rownames(vc.1) <- unique.del
    colnames(vc.1) <- names(vc)
  }

  vcov.1 <- refits[which(names(refits) == 'vcov.1')]
  names(vcov.1) <- unique.del

  left <- "[-"
  right <- "]"
  if (groups == ".case") {
    groups <- "case"
  }
  nms <- c("fixed.effects", paste0("fixed.effects",
                                   left, groups, right), "var.cov.comps", paste0("var.cov.comps",
                                                                                 left, groups, right), "vcov", paste0("vcov",
                                                                                                                      left, groups, right), "groups", "deleted")
  result <- list(fixed, fixed.1, vc, vc.1,
                 vcov(model), vcov.1, groups, unique.del)
  names(result) <- nms
  class(result) <- "influence.lme"
  result
}

#' @exportS3Method
dffits.lme <- function(model, data = NULL, level = 0){
  if (missing(data)){
    data = getData(model)
  }
  V <- get_vcomp(model)$V

  yi <- sapply(1:nrow(data), function(x, d, n, l) {
    predict(x, newdata = d[n, ], level = l)},
    x = model, d = data, l = level)
  y_i_ <- sapply(1:nrow(data), function(x, d, n, l) {
    predict(update(x, data = d[-n, ]), newdata = d[n, ], level = l)},
    x = model, d = data, l = level)


  predsd <- function(V){
    vii <- lapply(1:nrow(V), part_mat, M = V, which = 1)
    tvi <- lapply(1:nrow(V), part_mat, M = V, which = 2)
    vi <- lapply(1:nrow(V), part_mat, M = V, which = 4)
    V_i_ <- inv_subV(V)

    si <- mapply(function(vii, tvi, V_i_, vi) {vii - tvi %*% V_i_ %*% vi},
                 vii = vii, tvi = tvi, V_i_ = V_i_, vi = vi)
    return(si)
  }
  s <- predsd(V)
  h = hatvalues.lme(model)
  d <- (yi - y_i_)/(sqrt(s*h))
  return(d)
}

#' @exportS3Method
press.lme <- function(model, infl, data){
  if (missing(data)){
    data <-getData(model)
  }
  if (missing(infl)){
    infl <- influence(model)
  }
  N <- nrow(data)
  X <- get_vcomp(model)$X
  Y <- data[, as.character(formula(model)[[2]])]
  out = vector('numeric', N)
  for (n in 1:N){
    b = infl$`fixed.effects[-case]`[n, ]
    out[n] = Y[n] - X[n, ] %*% b
  }
  return(out)
}

#' @exportS3Method
dfbeta.influence.lme <- function(model, which = c("fixed.effects", "var.cov.comps"), ...){
  which <- match.arg(which)
  which.cols <- grep(which, names(model), value = T)
  b  <- model[[grep('[', which.cols, fixed = T, value = T)]]
  b0 <- model[[which]]
  b - matrix(b0, nrow = nrow(b), ncol = ncol(b), byrow = TRUE)
}

#' @exportS3Method
dfbeta.lme <- function(model, which = c('fixed.effects', 'var.cov.comps'), ...){
  dfbeta.influence.lme(influence(model), match.arg(which, choices = c('fixed.effects', 'var.cov.comps')), ...)
}

#' @exportS3Method
dfbetas.influence.lme <- function (model, which = c("fixed.effects", "var.cov.comps"), ...) {
  which <- match.arg(which)
  b <- if (which == "fixed.effects")
    model[[2]]
  else model[[4]]
  b0 <- if (which == "fixed.effects")
    model[[1]]
  else model[[3]]
  b - matrix(b0, nrow = nrow(b), ncol = ncol(b), byrow = TRUE)
}

#' @exportS3Method
dfbetas.lme <- function(model, which = c('fixed.effects', 'var.cov.comps'), ...){
  dfbetas.influence.lme(influence(model), match.arg(which, choices = c('fixed.effects', 'var.cov.comps')), ...)
}

#' @exportS3Method
cooks_distance.lme <- function(model, infl, ...){
  if (missing(infl)){
    infl <- influence.lme(model)
  }
  B <- infl$fixed.effects
  B_i_ <- infl$`fixed.effects[-case]`

  X <- model.matrix(formula(model), data = getData(model))
  V <- get_vcomp.lme(model)$V
  p <- ncol(X)
  apply(B_i_, MARGIN = 1, FUN = function(bi, b, x, v){
    t(B - bi) %*% (t(X) %*% solve(V) %*% X) %*% (B - bi)/p
    }, b = B, x = X, v = V)
}

#' @exportS3Method
covratio.influence.lme <- function(infl, ...){
  which.cols <- grep('vcov', names(infl), value = T)
  n  <- infl[[grep('[', which.cols, fixed = T, value = T)]]
  d <- infl[['vcov']]
  sapply(n, function(n, d){det(n)/det(d)}, d = d)
}

#' @exportS3Method
covratio.lme <- function(model, ...){
  covratio.influence.lme(influence(model, ...))
}

#' @exportS3Method
influence_measures.lme <- function(model, infl, level = 0, measures = c('student', 'dffits', 'cooksd', 'covratio', 'press', 'leverage', 'dfbeta'), ...){
  opts <- c('student', 'dffits', 'cooksd', 'covratio', 'press', 'leverage', 'dfbeta')
  m <- match.arg(measures, choices = opts, several.ok = T)

  if (missing(infl)){
    infl <- influence(model, ...)
  }

  infmat <- sapply(m[!m == 'dfbeta'], switch,
                student = rstandard(model, level = level),
                dffits = dffits(model, level = level),
                cooksd = cooks_distance(model, infl),
                covratio = covratio(model),
                press = press(model, infl),
                leverage = hatvalues(model))
  if ('dfbeta' %in% m){
    d <-  dfbeta.influence.lme(infl)
    colnames(d) <- paste0('dfbeta_', colnames(d))
    infmat <- cbind(infmat, d)
  }
  rownames(infmat) = NULL
  out <- list(infmat = infmat, call = formula(model))
  class(out) <- 'infl'
  out
}

