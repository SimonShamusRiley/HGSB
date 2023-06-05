# #### The lists methods missing for each of the following classes:
# #### lme: RStudent(?)
# #### gls: RStudent(?)
# #### glmmTMB: influence, dfbeta, hatvalues, covratio, cooks distance, PRESS, DFFITS, RStudent(?)
# #### lmer: covratio, PRESS, DFFITS, RStudent(?)
#
# #### model agnostic
# loocv <- function(model, data, groups, ...){
#   if (missing(data)){
#     data <- tryCatch(getData(model),
#                      error = function(e){
#                        stop('Data cannot be retrieved and must be supplied')})
#   }
#   if (missing(groups)) {
#     groups <- ".case"
#     data$.case <- rownames(data)
#   } else if (length(groups) > 1) {
#     del.var <- paste0(groups, collapse = ".")
#     data[, del.var] <- apply(data[, groups], 1, function(row) paste0(row, collapse = "."))
#     groups <- del.var
#   }
#   unique.del <- unique(data[, groups])
#   data$.groups <- data[, groups]
#
#   loo <- function(model, data, grp, ...){
#     predict(object = update(model, data = data[data$.groups != grp, ], ...),
#             newdata = data[data$.groups == grp, ])
#   }
#   sapply(unique(data$.groups), loo,
#          model = model, data = data, ...)
# }
#
# dffits.lm <- dffits.glm <- dffits.mlm <- dffits
# dffits <- function(model, ...){
#   UseMethod('dffits', model)
# }
# dffits.gls <- dffits.lme <- dffits.lmer <- dffits.glmmTMB <- function(model, preds, data, ...){
#   if (missing(preds)){
#     preds <- loocv(model, ...)
#   }
#   X <- get_vcomp(model)$X
#   V <- get_vcomp(model)$V
#
#   ese <- apply(X, MARGIN = 1, FUN = function(x_i, X, V) {
#     sqrt(t(x_i) %*% solve(t(X) %*% solve(V) %*% X) %*% x_i)
#   }, X = X, V = V)
#
#   as.numeric((fitted(model, level = 0) - preds)/ese)
# }
#
# press <- function(model, ...){
#   UseMethod('press', model)
# }
# press.gls <- press.lme <- press.lmer <- press.glmmTMB <- function(model, preds, data, ...){
#   if (missing(data)){
#     data <- tryCatch(getData(model),
#                      error = function(e){
#                        stop('Data cannot be retrieved and must be supplied')})
#   }
#   if (missing(preds)){
#     preds <- loocv(model, data = data, ...)
#   }
#
#   X <- get_vcomp(model)$X
#   V <- get_vcomp(model)$V
#
#   Y <- fitted(model, level = 0) + as.numeric(resid(model, type = 'response'))
#   as.numeric(Y - preds)
# }
#
# #### gls() methods
# influence.gls <- function(model, groups, data, ncores = 1, ...) {
#   if (is.infinite(ncores)) {
#     ncores <- parallel::detectCores(logical = FALSE)
#   }
#   if (missing(data)) {
#     data <- nlme::getData(model)
#   }
#   if (missing(groups)) {
#     groups <- ".case"
#     data$.case <- rownames(data)
#   } else if (length(groups) > 1) {
#     del.var <- paste0(groups, collapse = ".")
#     data[, del.var] <- apply(data[, groups], 1, function(row) paste0(row, collapse = "."))
#     groups <- del.var
#   }
#   unique.del <- unique(data[, groups])
#   data$.groups <- data[, groups]
#
#   fixed <- coef(model)
#   vc <- attr(model$apVar, "Pars")
#   vcov <- vcov(model)
#
#   deleteGroup <- function(del) {
#     data$del <- del
#     mod.1 <- suppressWarnings(update(model, data = data,
#                                      subset = .groups != del,
#                                      control = nlme::lmeControl(returnObject = TRUE)))
#     fixed.1 <- coef(mod.1)
#     vc.1    <- attr(mod.1$apVar, 'Pars')
#     vcov.1  <- vcov(mod.1)
#     list(fixed.1 = fixed.1, vc.1 = vc.1, vcov.1 = vcov.1)
#   }
#
#   refits <- if (ncores >= 2) {
#     message("Note: using a cluster of ", ncores, " cores")
#     cl <- parallel::makeCluster(ncores)
#     on.exit(parallel::stopCluster(cl))
#     parallel::clusterEvalQ(cl, require("nlme"))
#     parallel::clusterApply(cl, unique.del, deleteGroup)
#   } else {
#     lapply(unique.del, deleteGroup)
#   }
#   refits <- unlist(refits, recursive = F)
#
#   fixed.1 <- matrix(unlist(refits[which(names(refits) == 'fixed.1')]),
#                    nrow = length(unique.del), ncol = length(fixed), byrow = T)
#   rownames(fixed.1) <- unique.del
#   colnames(fixed.1) <- names(fixed)
#
#   if (is.null(unlist(refits[which(names(refits) == 'vc.1')]))){
#     vc.1 <- NULL
#   } else {
#     vc.1 <- matrix(unlist(refits[which(names(refits) == 'vc.1')]),
#                    nrow = length(unique.del), ncol = length(vc), byrow = T)
#     rownames(vc.1) <- unique.del
#     colnames(vc.1) <- names(vc)
#   }
#
#   vcov.1 <- refits[which(names(refits) == 'vcov.1')]
#   names(vcov.1) <- unique.del
#
#   left <- "[-"
#   right <- "]"
#   if (groups == ".case") {
#     groups <- "case"
#   }
#   nms <- c("fixed.effects", paste0("fixed.effects",
#                                    left, groups, right), "var.cov.comps", paste0("var.cov.comps",
#                                                                                  left, groups, right), "vcov", paste0("vcov",
#                                                                                                                       left, groups, right), "groups", "deleted")
#   result <- list(fixed, fixed.1, vc, vc.1,
#                  vcov, vcov.1, groups, unique.del)
#   names(result) <- nms
#   class(result) <- "influence.gls"
#   result
# }
#
# dfbeta.influence.gls <- function(model, which = c("fixed.effects", "var.cov"), ...){
#   which <- match.arg(which)
#   which.cols <- grep(which, names(model), value = T)
#   b  <- model[[grep('[', which.cols, fixed = T, value = T)]]
#   b0 <- model[[which]]
#   b - matrix(b0, nrow = nrow(b), ncol = ncol(b), byrow = TRUE)
# }
#
# cooks.distance.influence.gls <- function(model, ...){
#   db <- dfbeta(model)
#   n <- nrow(db)
#   p <- ncol(db)
#   d <- numeric(n)
#   vcovs <- model[[6]]
#   sig.sq <- (exp((model[[4]])[, ncol((model[[4]]))]))^2
#   for (i in 1:n) {
#     d[i] <- (db[i, ] %*% solve(vcovs[[i]]) %*% db[i, ])/(p *sig.sq[i])
#   }
#   d
# }
#
# cooks.distance.gls <- function(model, ...) {
#   cooks.distance.influence.gls(influence(model, ...))
# }
#
# hatvalues.gls <- function(model, ...){
#   X <- model.matrix(formula(model), getData(model))
#   V <- get_vcomp(model)$V
#   H <- X %*% solve(t(X) %*% solve(V) %*% X) %*% t(X) %*% solve(V)
#   diag(H)
# }
#
# covratio.influence.gls <- function(model, ...){
#   which.cols <- grep('vcov', names(model), value = T)
#   d  <- model[[grep('[', which.cols, fixed = T, value = T)]]
#   n <- model[['vcov']]
#   sapply(d, function(n, d){det(n)/det(d)}, n = n)
# }
#
# covratio.gls <- function(model, ...){
#   covratio.influence.gls(influence(model, ...))
# }
#
# # lme() methods
# influence.lme <- function(model, groups, data, ncores = 1, ...) {
#   if (is.infinite(ncores)) {
#     ncores <- parallel::detectCores(logical = FALSE)
#   }
#   if (missing(data)) {
#     data <- nlme::getData(model)
#   }
#   if (missing(groups)) {
#     groups <- ".case"
#     data$.case <- rownames(data)
#   } else if (length(groups) > 1) {
#     del.var <- paste0(groups, collapse = ".")
#     data[, del.var] <- apply(data[, groups], 1, function(row) paste0(row,
#                                                                      collapse = "."))
#     groups <- del.var
#   }
#
#   unique.del <- unique(data[, groups])
#   data$.groups <- data[, groups]
#
#   fixed <- fixef(model)
#   vc <- attr(model$apVar, "Pars")
#   vcov.1 <- vector(length(unique.del), mode = "list")
#
#   deleteGroup <- function(del) {
#     data$del <- del
#     mod.1 <- suppressWarnings(update(model, data = data,
#                                      subset = .groups != del, control = nlme::lmeControl(returnObject = TRUE)))
#     fixed.1 <- fixef(mod.1)
#     vc.0 <- attr(mod.1$apVar, "Pars")
#     vc.1 <- if (!is.null(vc.0))
#       vc.0
#     else rep(as.numeric(NA), length(vc))
#     vcov.1 <- vcov(mod.1)
#     list(fixed.1 = fixed.1, vc.1 = vc.1, vcov.1 = vcov.1)
#   }
#   refits <- if (ncores >= 2) {
#     message("Note: using a cluster of ", ncores, " cores")
#     cl <- parallel::makeCluster(ncores)
#     on.exit(parallel::stopCluster(cl))
#     parallel::clusterEvalQ(cl, require("nlme"))
#     parallel::clusterApply(cl, unique.del, deleteGroup)
#   }
#   else {
#     lapply(unique.del, deleteGroup)
#   }
#   refits <- unlist(refits, recursive = F)
#
#   fixed.1 <- matrix(unlist(refits[which(names(refits) == 'fixed.1')]),
#                     nrow = length(unique.del), ncol = length(fixed), byrow = T)
#   rownames(fixed.1) <- unique.del
#   colnames(fixed.1) <- names(fixed)
#
#   if (is.null(unlist(refits[which(names(refits) == 'vc.1')]))){
#     vc.1 <- NULL
#   } else {
#     vc.1 <- matrix(unlist(refits[which(names(refits) == 'vc.1')]),
#                    nrow = length(unique.del), ncol = length(vc), byrow = T)
#     rownames(vc.1) <- unique.del
#     colnames(vc.1) <- names(vc)
#   }
#
#   vcov.1 <- refits[which(names(refits) == 'vcov.1')]
#   names(vcov.1) <- unique.del
#
#   left <- "[-"
#   right <- "]"
#   if (groups == ".case") {
#     groups <- "case"
#   }
#   nms <- c("fixed.effects", paste0("fixed.effects",
#                                    left, groups, right), "var.cov.comps", paste0("var.cov.comps",
#                                                                                  left, groups, right), "vcov", paste0("vcov",
#                                                                                                                       left, groups, right), "groups", "deleted")
#   result <- list(fixed, fixed.1, vc, vc.1,
#                  vcov(model), vcov.1, groups, unique.del)
#   names(result) <- nms
#   class(result) <- "influence.lme"
#   result
# }
#
# dfbeta.influence.lme <- function(model, which = c("fixed.effects", "var.cov"), ...){
#   which <- match.arg(which)
#   which.cols <- grep(which, names(model), value = T)
#   b  <- model[[grep('[', which.cols, fixed = T, value = T)]]
#   b0 <- model[[which]]
#   b - matrix(b0, nrow = nrow(b), ncol = ncol(b), byrow = TRUE)
# }
#
# cooks.distance.influence.lme <- function(model, ...){
#   db <- dfbeta(model, ...)
#   n <- nrow(db)
#   p <- ncol(db)
#   d <- numeric(n)
#   vcovs <- model[[6]]
#   sig.sq <- (exp(model[[4]][, ncol(model[[4]])]))^2
#   for (i in 1:n) {
#     d[i] <- (db[i, ] %*% solve(vcovs[[i]]) %*% db[i, ])/(p * sig.sq[i])
#   }
#   d
# }
#
# cooks.distance.lme <- function(model, ...) {
#   cooks.distance.influence.lme(influence(model, ...))
# }
#
# hatvalues.lme <- function(model, ...) {
#   X <- model.matrix(formula(model), getData(model))
#   V <- get_vcomp(model)$V
#   H <- X %*% solve(t(X) %*% solve(V) %*% X) %*% t(X) %*% solve(V)
#   diag(H)
# }
#
# covratio.influence.lme <- function(model, ...){
#   which.cols <- grep('vcov', names(model), value = T)
#   d  <- model[[grep('[', which.cols, fixed = T, value = T)]]
#   n <- model[['vcov']]
#   sapply(d, function(n, d){det(n)/det(d)}, n = n)
# }
#
# covratio.lme <- function(model, ...){
#   covratio.influence.lme(influence(model, ...))
# }
#
# ##### glmmTMB functions
#
# getData.glmmTMB <- function(model){
#   model$frame
# }
#
