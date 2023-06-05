# #' @exportS3Method
#' hatvalues.gls <- function(model, ...) {
#'   Y <- unlist(getData(model)[as.character(formula(model)[[2]])], use.names = F)
#'   X <- model.matrix(formula(model), getData(model))
#'   V <- get_vcomp(model)$V
#'   n <- nrow(X)
#'   p <- ncol(X)
#'
#'   X_i <- lapply(1:nrow(X), FUN = function(n, M){
#'     matrix(M[row(M) != n], ncol = ncol(M))
#'   }, M = X)
#'   sV_i <- inv_subV(V)
#'
#'   xbar <- matrix(nrow = n, ncol = p)
#'   for (i in 1:n){
#'     vi <- part_mat(V, i, 4)
#'     xbar[i, ] <- t(X[i, ] - t(X_i[[i]]) %*% (sV_i[[i]]) %*% vi)
#'   }
#'
#'   hbar <-vector('numeric', n)
#'   for (i in 1:n){
#'     hbar[i] <- t(xbar[i, ]) %*% solve(t(X) %*% solve(V) %*% X) %*% xbar[i, ]
#'   }
#'
#'   sbar <- vector('numeric', n)
#'   for (i in 1:n){
#'     vii <- part_mat(V, i, 1)
#'     tvi <- part_mat(V, i, 2)
#'     vi <- part_mat(V, i, 4)
#'     sbar[i] <- vii -  tvi %*% sV_i[[i]] %*% vi
#'   }
#'   hbar/sbar
#' }
#'
#' rstudent.gls <- function(model, ...){
#'   V <- get_vcomp(model)$V
#'   X <- model.matrix(formula(model), getData(model))
#'   Q <- (X %*% solve(t(X) %*% solve(V) %*% X) %*% t(X))
#'   resid(model, type = 'response', ...)/sqrt(diag(V-Q))
#' }
#'
# #' @exportS3Method
#' influence.gls <- function(model, groups, data, ncores = 1, ...) {
#'   if (is.infinite(ncores)) {
#'     ncores <- parallel::detectCores(logical = FALSE)
#'   }
#'   if (missing(data)) {
#'     data <- nlme::getData(model)
#'   }
#'   if (missing(groups)) {
#'     groups <- ".case"
#'     data$.case <- rownames(data)
#'   } else if (length(groups) > 1) {
#'     del.var <- paste0(groups, collapse = ".")
#'     data[, del.var] <- apply(data[, groups], 1, function(row) paste0(row, collapse = "."))
#'     groups <- del.var
#'   }
#'   unique.del <- unique(data[, groups])
#'   data$.groups <- data[, groups]
#'
#'   fixed <- coef(model)
#'   vc <- if (length(names(model$modelStruct)) > 0) {
#'     attr(model$apVar, 'Pars')
#'   } else {
#'     log(sigma(model))
#'   }
#'   vcov <- vcov(model)
#'
#'   deleteGroup <- function(del) {
#'     data$del <- del
#'     mod.1 <- suppressWarnings(update(model, data = data,
#'                                      subset = .groups != del,
#'                                      control = nlme::lmeControl(returnObject = TRUE)))
#'     fixed.1 <- coef(mod.1)
#'     vc.1    <- if (length(names(mod.1$modelStruct)) > 0) {
#'       attr(mod.1$apVar, 'Pars')
#'     } else {
#'       log(sigma(mod.1))
#'     }
#'     vcov.1  <- vcov(mod.1)
#'     list(fixed.1 = fixed.1, vc.1 = vc.1, vcov.1 = vcov.1)
#'   }
#'
#'   refits <- if (ncores >= 2) {
#'     message("Note: using a cluster of ", ncores, " cores")
#'     cl <- parallel::makeCluster(ncores)
#'     on.exit(parallel::stopCluster(cl))
#'     parallel::clusterEvalQ(cl, require("nlme"))
#'     parallel::clusterApply(cl, unique.del, deleteGroup)
#'   } else {
#'     lapply(unique.del, deleteGroup)
#'   }
#'   refits <- unlist(refits, recursive = F)
#'
#'   fixed.1 <- matrix(unlist(refits[which(names(refits) == 'fixed.1')]),
#'                     nrow = length(unique.del), ncol = length(fixed), byrow = T)
#'   rownames(fixed.1) <- unique.del
#'   colnames(fixed.1) <- names(fixed)
#'
#'   if (is.null(unlist(refits[which(names(refits) == 'vc.1')]))){
#'     vc.1 <- NULL
#'   } else {
#'     vc.1 <- matrix(unlist(refits[which(names(refits) == 'vc.1')]),
#'                    nrow = length(unique.del), ncol = length(vc), byrow = T)
#'     rownames(vc.1) <- unique.del
#'   }
#'   colnames(vc.1) <- if (is.null(names(vc))) {
#'     'lSigma'
#'   } else {
#'     names(vc)
#'   }
#'
#'   vcov.1 <- refits[which(names(refits) == 'vcov.1')]
#'   names(vcov.1) <- unique.del
#'
#'   left <- "[-"
#'   right <- "]"
#'   if (groups == ".case") {
#'     groups <- "case"
#'   }
#'   nms <- c("fixed.effects", paste0("fixed.effects",
#'                                    left, groups, right), "var.cov.comps", paste0("var.cov.comps",
#'                                                                                  left, groups, right), "vcov", paste0("vcov",
#'                                                                                                                       left, groups, right), "groups", "deleted")
#'   result <- list(fixed, fixed.1, vc, vc.1,
#'                  vcov, vcov.1, groups, unique.del)
#'   names(result) <- nms
#'   class(result) <- "influence.gls"
#'   result
#' }
#'
# #' @exportS3Method
#' dfbeta.influence.gls <- function(model, which = c("fixed.effects", "var.cov.comps"), ...){
#'   which <- match.arg(which)
#'   which.cols <- grep(which, names(model), value = T)
#'   b  <- model[[grep('[', which.cols, fixed = T, value = T)]]
#'   b0 <- model[[which]]
#'   b - matrix(b0, nrow = nrow(b), ncol = ncol(b), byrow = TRUE)
#' }
#'
# #' @exportS3Method
#' dffits.gls <- function(model, preds, data, ...){
#'   if (missing(preds)){
#'     preds <- loocv(model, ...)
#'   }
#'   X <- get_vcomp(model)$X
#'   V <- get_vcomp(model)$V
#'
#'   ese <- apply(X, MARGIN = 1, FUN = function(x_i, X, V) {
#'     sqrt(t(x_i) %*% solve(t(X) %*% solve(V) %*% X) %*% x_i)
#'   }, X = X, V = V)
#'
#'   as.numeric((fitted(model, level = 0) - preds)/ese)
#' }
#'
# #' @exportS3Method
#' press.gls <- function(model, preds, data, ...){
#'   if (missing(data)){
#'     data <- tryCatch(getData(model),
#'                      error = function(e){
#'                        stop('Data cannot be retrieved and must be supplied')})
#'   }
#'   if (missing(preds)){
#'     preds <- loocv(model, data = data, ...)
#'   }
#'
#'   X <- get_vcomp(model)$X
#'   V <- get_vcomp(model)$V
#'
#'   Y <- fitted(model, level = 0) + as.numeric(resid(model, type = 'response'))
#'   as.numeric(Y - preds)
#' }
#'
# #' @exportS3Method
#' cooks_distance.influence.gls <- function(model, ...){
#'   db <- dfbeta(model)
#'   n <- nrow(db)
#'   p <- ncol(db)
#'   d <- numeric(n)
#'   vcovs <- model[[6]]
#'   sig.sq <- (exp((model[[4]])[, ncol((model[[4]]))]))^2
#'   for (i in 1:n) {
#'     d[i] <- (db[i, ] %*% solve(vcovs[[i]]) %*% db[i, ])/(p *sig.sq[i])
#'   }
#'   d
#' }
#'
# #' @exportS3Method
#' cooks_distance.gls <- function(model, ...) {
#'   cooks_distance.influence.gls(influence(model, ...))
#' }
#'
# #' @exportS3Method
#' hatvalues.gls <- function(model, ...){
#'   X <- model.matrix(formula(model), getData(model))
#'   V <- get_vcomp(model)$V
#'   H <- X %*% solve(t(X) %*% solve(V) %*% X) %*% t(X) %*% solve(V)
#'   diag(H)
#' }
#'
# #' @exportS3Method
#' covratio.influence.gls <- function(model, ...){
#'   which.cols <- grep('vcov', names(model), value = T)
#'   d  <- model[[grep('[', which.cols, fixed = T, value = T)]]
#'   n <- model[['vcov']]
#'   sapply(d, function(n, d){det(n)/det(d)}, n = n)
#' }
#'
# #' @exportS3Method
#' covratio.gls <- function(model, ...){
#'   covratio.influence.gls(influence(model, ...))
#' }
#'
# #' @exportS3Method
#' influence_measures.gls <- function(model, infl, preds, ...){
#'   if (missing(infl)){
#'     infl <- influence(model, ...)
#'   }
#'   if (missing(preds)){
#'     preds <- loocv(model, ...)
#'   }
#'   df <- dfbeta(model = infl)
#'   cov <- covratio(model = infl)
#'   cookd <- cooks_distance(model = infl)
#'
#'   dtheta <- (infl$fixed.effects - infl$`var.cov.comps[-case]`)
#'
#'   dffit <- dffits(model = model, preds = preds)
#'   press <- press(model = model, preds = preds)
#'   hats <- hatvalues(model)
#'   students <- rstudent(model = model, infl = infl)
#'
#'   infmat <- as.matrix(cbind(df, cov, cookd, cookdvc, dffit, press, hats, students))
#'   colnames(infmat) <- c(paste0('dfb_', colnames(df)), "CovRatio", "CooksD", "CooksD_CovParms", "DFFITS", "PRESS", "Leverage", "Studentized_Resid")
#'   out <- list(infmat = infmat, is.inf = matrix('-', nrow = nrow(infmat)), call = getCall(model))
#'   class(out) <- 'infl'
#'   out
#' }
