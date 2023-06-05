# # Modified to remove redundant columns for effects with structure pdIdent
# model.matrix.reStruct <- function (object, data, contrast = NULL, ...){
#   if (is.null(form <- formula(object, asList = TRUE))) {
#     stop("cannot extract model matrix without formula")
#   }
#   form1 <- asOneFormula(form)
#   if (length(form1) > 0) {
#     data <- model.frame(form1, data = data)
#   } else {
#     data <- data.frame(`(Intercept)` = rep(1, nrow(data)))
#   }
#   idents <- sapply(object, function(x) {'pdIdent' %in% class(x)})
#   for (i in which(idents)){
#     form[[i]] = ~1
#   }
#   any2list <- function(object, data, contrast) {
#     form2list <- function(form, data, contrast) {
#       if (length(asOneFormula(form)) == 0) {
#         return(list(`(Intercept)` = rep(1, dim(data)[1])))
#       }
#       as.data.frame(unclass(model.matrix(form, model.frame(form,
#                                                            data), contrast)))
#     }
#     if (inherits(object, "formula")) {
#       return(form2list(object, data, contrast))
#     }
#     if (is.list(object)) {
#       return(unlist(lapply(object, form2list, data = data,
#                            contrast = contrast), recursive = FALSE))
#     }
#     return(NULL)
#   }
#   value <- as.list(lapply(form, any2list, data = data, contrast = contrast))
#   contr <- as.list(lapply(as.data.frame(data), function(x) if (inherits(x,
#                                                                         "factor") && length(levels(x)) > 1)
#     contrasts(x)
#     else NULL))
#   contr[names(contrast)] <- contrast
#   ncols <- lengths(value)
#   nams <- if (length(value) == 1) {
#     names(value[[1]])
#   }
#   else {
#     paste(rep(names(value), ncols), unlist(lapply(value,
#                                                   names)), sep = ".")
#   }
#   structure(matrix(unlist(value), nrow = nrow(data), dimnames = list(row.names(data),
#                                                                      nams)), ncols = ncols, nams = lapply(value, names), contr = contr)
# }
#
# # get_vcomp.lme is adapted from the mgcv::extract.lme.cov() function, with the following
# # adjustments in order for it to work properly when crossed random effects are specified:
# # 1.
# get_vcomp.lme <- function(x, data = NULL){
#   if (!inherits(x, "lme"))
#     stop("object does not appear to be of class lme")
#   if (is.null(data)) {
#     na.act <- na.action(x)
#     data <- if (is.null(na.act))
#       x$data
#     else x$data[-na.act, ]
#   }
#
#   # x$groups, x$dims$ngrp and x$dims$ncol are all misspecified when crossed random
#   # effects are present; this corrects those
#   forms = formula(reStruct(x$modelStruct$reStruct))
#   nested <- names(forms)
#   nms <- gsub(x = as.character(forms), pattern = "[^[:alpha:]]", replacement = '')
#   nms <- mapply(paste0, names(forms), nms, MoreArgs = list(collapse = '_'))
#   grps <- lapply(forms, model.frame, data = data)
#   names(grps) <- nms
#   if (any(pull <- sapply(grps, length)==0)){
#     pull <- names(grps)[pull]
#     for (c in pull){
#       grps[[c]] <- data[, c, drop = F]
#     }
#   }
#   grps <- as.data.frame(grps)
#   grps[, 1:ncol(grps)] <- lapply(grps[, 1:ncol(grps)], as.character)
#
#   for (n in 1:ncol(grps)){
#     grps[, n] <- factor(paste0(grps[, n], as.character(data[, nested[n]])))
#   }
#   grps <- grps[, ncol(grps):1]
#
#   x$groups <- grps
#
#   idents <- sapply(x$modelStruct$reStruct, function(x) {'pdIdent' %in% class(x)})
#   x$dims$ngrps <- sapply(x$groups[, ncol(x$groups):1], nlevels)
#   x$dims$ncol[which(idents)] <- 1
#
#   for (w in which(idents)){
#     attr(x$modelStruct$reStruct[[w]], 'ncol') = 1
#     attr(x$modelStruct$reStruct[[w]], 'Dimnames') = list("(Intercept)", "(Intercept)")
#   }
#
#     grps <- nlme::getGroups(x)
#     n <- length(grps)
#     if (is.null(x$modelStruct$varStruct))
#     w <- rep(x$sigma, n)
#     else {
#       w <- 1/nlme::varWeights(x$modelStruct$varStruct)
#       group.name <- names(x$groups)
#       order.txt <- paste("ind<-order(data[[\"", group.name[1],
#                          "\"]]", sep = "")
#       if (length(x$groups) > 1)
#         for (i in 2:length(x$groups)) order.txt <- paste(order.txt,
#                                                          ",data[[\"", group.name[i], "\"]]",
#                                                          sep = "")
#       order.txt <- paste(order.txt, ")")
#       eval(parse(text = order.txt))
#       w[ind] <- w
#       w <- w * x$sigma
#     }
#     if (is.null(x$modelStruct$corStruct))
#     V <- diag(n)
#     else {
#       c.m <- nlme::corMatrix(x$modelStruct$corStruct)
#       if (!is.list(c.m))
#         V <- c.m
#       else {
#         V <- matrix(0, n, n)
#         gr.name <- names(c.m)
#         n.g <- length(c.m)
#         j0 <- 1
#         ind <- ii <- 1:n
#         for (i in 1:n.g) {
#           j1 <- j0 + nrow(c.m[[i]]) - 1
#           V[j0:j1, j0:j1] <- c.m[[i]]
#           ind[j0:j1] <- ii[grps == gr.name[i]]
#           j0 <- j1 + 1
#         }
#         V[ind, ] <- V
#         V[, ind] <- V
#       }
#     }
#     V <- as.vector(w) * t(as.vector(w) * V)
#     R <- V
#     M <- list()
#
#     grp.dims <- x$dims$ncol
#     Zt <- model.matrix(x$modelStruct$reStruct, data)
#     cov <- as.matrix(x$modelStruct$reStruct)
#     i.col <- 1
#     n.levels <- length(x$groups)
#     Z <- matrix(0, n, 0)
#     for (i in n.levels:1) {
#       if (length(levels(x$groups[[n.levels - i + 1]])) ==
#           1) {
#         M[[1]] <- matrix(rep(1, nrow(x$groups)))
#       }
#       else {
#         clist <- list(`x$groups[[n.levels - i + 1]]` = c("contr.treatment",
#                                                          "contr.treatment"))
#
#         M[[1]] <- model.matrix(~x$groups[[n.levels -
#                                             i + 1]] - 1, contrasts.arg = clist)
#       }
#       M[[2]] <- Zt[, i.col:(i.col + grp.dims[i] - 1), drop = FALSE]
#       i.col <- i.col + grp.dims[i]
#       Z <- cbind(Z, mgcv::tensor.prod.model.matrix(M))
#     }
#     Vr <- matrix(0, ncol(Z), ncol(Z))
#     start <- 1
#     for (i in n.levels:1) {
#       k <- n.levels - i + 1
#       for (j in 1:x$dims$ngrps[i]) {
#         stop <- start + ncol(cov[[k]]) - 1
#         Vr[start:stop, start:stop] <- cov[[k]]
#         start <- stop + 1
#       }
#     }
#     Vr <- Vr * x$sigma^2
#     V <- V + Z %*% Vr %*% t(Z)
#     contr_type <-
#     list(Z = Z, G = Vr, R = R, V = V)
# }
#
# eblup_terms.lme <- function(object){
#   contr_list <- attr(model.matrix(formula(object), object$data), 'contrasts')
#
#   sum_zero <- sapply(object$contrasts, function(x){
#     ncol(x) == 1 | all(colSums(x) == 0)
#   })
#   if (!all(sum_zero)){
#     warning(simpleWarning('Refitting model with sum-to-zero contrasts'))
#     contr_list <- lapply(contr_list, `[<-`, 'contr.sum')
#     object <- update(object, contrasts = contr_list)
#   }
#   keep_real <-  which(unlist(ranef(object)) != 0)
#   B <- c(fixef(object), unlist(ranef(object))[keep_real])
#   noquote(array(names(B), dim = c(length(B), 1), dimnames = list(seq(length(B)), 'Term:')))
# }
#
# eblup.lme <- function(object, predictions, df = NULL){
#   contr_list <- attr(model.matrix(formula(object), object$data), 'contrasts')
#
#   sum_zero <- sapply(object$contrasts, function(x){
#     ncol(x) == 1 | all(colSums(x) == 0)
#   })
#   if (!all(sum_zero)){
#     warning(simpleWarning('Refitting model with sum-to-zero contrasts; see help("eblup_term") for details')
#     contr_list <- lapply(contr_list, `[<-`, 'contr.sum')
#     object <- update(object, contrasts = contr_list)
#   }
#
#   vc <- get_vcomp.lme(object)
#   X <- model.matrix(formula(object), object$data, contrasts.arg = contr_list)
#   G <- vc$G
#   Z <- vc$Z
#   R <- vc$R
#
#   M1 <- t(X) %*% solve(R) %*% X
#   M2 <- t(Z) %*% solve(R) %*% X
#   M3 <- t(X) %*% solve(R) %*% Z
#   M4 <- t(Z) %*% solve(R) %*% Z + solve(G)
#   M <- cbind(rbind(M1, M2), rbind(M3, M4))
#   C <- solve(M)
#
#   # Enforce consistency in predictions by making vectors 1-column matrices
#   L <- lapply(predictions, function(x) {
#     if (is.vector(x)){
#       matrix(x, nrow = 1)
#     } else {
#       x
#     }
#   })
#
#   keep_real <-  which(unlist(ranef(object)) != 0)
#   B <- matrix(c(fixef(object), unlist(ranef(object))[keep_real]))
#   preds <- sapply(L, `%*%`, B)
#
#   se <- sapply(L, function(L, C) {
#     sqrt(L %*% C %*% t(L))
#   }, C = C)
#   cbind(preds, se)
# }
#
#
