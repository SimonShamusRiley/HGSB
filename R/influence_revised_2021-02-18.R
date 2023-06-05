# options(contrasts = c('contr.SAS', 'contr.poly'))
# library(nlme)
# # peas <- HGSB::peas
# # peas$cat <- factor(peas$trt != 'Control', labels = c('Control', 'Sugar'))
# # model <- gls(length_mm ~ trt, peas, correlation = corCompSymm(form = ~trt|block), weights = varIdent(form = ~1|trt))
# # model <- lme(length_mm ~ trt, peas, ~ 1|block, weights = varIdent(form = ~1|cat))
# # V <- HGSB::get_vcomp(model)$V
#
# d <- matrix(c(1, 1, 1, 56, 1, 1, 2, 41, 1, 2, 1, 50, 1, 2, 2, 36, 1, 3, 1, 39, 1, 3, 2, 35, 2, 1, 1, 30, 2, 1, 2, 25, 2, 2, 1, 36, 2, 2, 2, 28, 2, 3, 1, 33, 2, 3, 2, 30, 3, 1, 1, 32, 3, 1, 2, 24, 3, 2, 1, 31, 3, 2, 2, 27, 3, 3, 1, 15, 3, 3, 2, 19, 4, 1, 1, 30, 4, 1, 2, 25, 4, 2, 1, 35, 4, 2, 2, 30, 4, 3, 1, 17, 4, 3, 2, 18), ncol = 4, byrow = T)
# d <- data.frame(Block = factor(d[,1]), A = factor(d[, 2]), B = factor(d[, 3]), Y = d[, 4])
# model <- lme(Y ~ A*B, d, ~1|Block/A)
# model
# V <- HGSB::get_vcomp(model)$V
#
# inv_subV <- function(V){
#   inv_V = solve(V)
#   lapply(1:nrow(inv_V), function(n, M){
#     L <- part_mat(M, n = n, which = 3)
#     lc <- part_mat(M, n = n, which = 4)
#     lr <- part_mat(M, n = n, which = 2)
#     v_ii <- part_mat(M, n = n, which = 1)
#     L - (lc%*%lr)/v_ii
#   }, M = inv_V)
# }
#
# part_mat <- function(M, n, which = c(1, 2, 3, 4)){
#   switch(which,
#          M[n, n],
#          t(M[n, -n]),
#          M[-n, -n],
#          M[-n, n])
# }
#
# Y <- unlist(getData(model)[as.character(formula(model)[[2]])], use.names = F)
# X <- model.matrix(formula(model), getData(model))
# n <- nrow(X)
# p <- ncol(X)
# e <- resid(model, type = 'response', level = 0)
#
# X_i <- lapply(1:nrow(X), FUN = function(n, M){
#   matrix(M[row(M) != n], ncol = ncol(M))
#   }, M = X)
#
# sV_i <- inv_subV(V)
#
# xbar <- matrix(nrow = n, ncol = p)
# for (i in 1:n){
#   vi <- part_mat(V, i, 4)
#   xbar[i, ] <- t(X[i, ] - t(X_i[[i]]) %*% (sV_i[[i]]) %*% vi)
# }
#
# hbar <-vector('numeric', n)
# for (i in 1:n){
#   hbar[i] <- t(xbar[i, ]) %*% solve(t(X) %*% solve(V) %*% X) %*% xbar[i, ]
# }
#
# sbar <- vector('numeric', n)
# for (i in 1:n){
#   vii <- part_mat(V, i, 1)
#   tvi <- part_mat(V, i, 2)
#   vi <- part_mat(V, i, 4)
#   sbar[i] <- vii -  (tvi %*% sV_i[[i]] %*% vi)
# }
# hbar
# hbar/sbar
#
# ybar <- vector('numeric', length = n)
# for (i in 1:n){
#   vi <- part_mat(V, i, 4)
#   ybar[i] <- Y[i] - t(Y[-i]) %*% sV_i[[i]] %*% vi
# }
#
# B = fixef(model)
#
# Bbar <- matrix(nrow = n, ncol = p)
# for (i in 1:n){
#   Bbar[i, ] <- B - solve(t(X) %*% solve(V) %*% X) %*% xbar[i, ] %*% (ybar[i] - t(xbar[i, ]) %*% B)/(sbar[i] - hbar[i])
# }
#
#
# pred <- loocv(model)
# xbar[1, ] %*% Bbar[1, ]
#
#
# cdB <- vector('numeric', length = n)
# for (i in 1:n){
# # cd_fe[i, ] <- B - solve(t(X) %*% solve(V) %*% X) %*% xbar[i, ] %*% (ybar[i] - t(xbar[i, ]) %*% B)/(sbar[i] - hbar[i])
#   cdB[i] <- (ybar[i] - (t(xbar[i, ]) %*% B))^2%*%hbar[i]/(p*(sbar[i] - hbar[i])^2)
#
# }
#
# type = 'conditional'
# Z <- HGSB::get_vcomp(model)$Z
# if (is.null(Z)){
#   type = 'marginal'
# } else {
#   u <- unlist(ranef(model))
# }
# if (type == 'conditional'){
#   e <- Y - X %*% B - Z %*% u
# } else {
#   e <- Y - X %*% B
#
# }
#
# Q <- (X %*% solve(t(X) %*% solve(V) %*% X) %*% t(X))
# ve <- diag(V - Q)
# student <- e/sqrt(ve)
#
# VarB <- fastmatrix::sweep.operator(solve(t(X) %*% solve(V) %*% X))
#
# model$apVar
# theta <- attr(model$apVar, 'Pars')
# cd_re <- matrix(nrow = n, ncol = length(theta))
#
# exstudent <- vector('numeric', n)
# pre <- loocv(model, level = 0)
# dffit <- vector('numeric', n)
# for (i in 1:n){
# xivixi <- solve(t(X) %*% solve(V) %*% X) +
#   solve(t(X) %*% solve(V) %*% X) %*%
#   (xbar[i, ]) %*% t(xbar[i, ]) %*%
#   solve(t(X) %*% solve(V) %*% X)/(sbar[i] - hbar[i])
#
# Qi <- t(xbar[i, ]) %*% xivixi %*% xbar[i, ]
# dffit[i] <- (((X%*%B) - (X%*%Bbar[i, ]))[i])/sqrt(Qi)
# }
# dffit
# ebar/sqrt(diag(V-Qi))
#
# ebar <- vector('numeric', length = n)
# for (i in 1:n){
#   ebar[i] <- Y[i] - X[i, ] %*% Bbar[i, ]
# }
