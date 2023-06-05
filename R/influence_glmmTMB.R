#' influence.glmmTMB <- function(model, data, groups, ncores = 1, ...){
#'   if (is.infinite(ncores)) {
#'     ncores <- parallel::detectCores(logical = FALSE)
#'   }
#'   if (missing(data)) {
#'     data <- getData(model)
#'   }
#'   if (missing(groups)) {
#'     groups <- ".case"
#'     data$.case <- rownames(data)
#'   } else if (length(groups) > 1) {
#'     del.var <- paste0(groups, collapse = ".")
#'     data[, del.var] <- apply(data[, groups], 1, function(row) paste0(row,
#'                                                                      collapse = "."))
#'     groups <- del.var
#'   }
#'
#'   unique.del <- unique(data[, groups])
#'   data$.groups <- data[, groups]
#'
#'   fixed <- fixef(model)$cond
#'   vc <- c(getME(model, 'theta'), log(sigma(model)))
#'   vcov <- vcov(model)$cond
#'
#'   deleteGroup <- function(del, ...) {
#'     data$del <- del
#'     mod.1 <- suppressWarnings(update(model, data = data[data$.groups != del, ], ...))
#'     fixed.1 <- fixef(mod.1)$cond
#'     vc.0 <- c(getME(model, 'theta'), log(sigma(model)))
#'     vcov.1 <- vcov(mod.1)$cond
#'     list(fixed.1 = fixed.1, vc.1 = vc.1, vcov.1 = vcov.1)
#'   }
#'   refits <- if (ncores >= 2) {
#'     message("Note: using a cluster of ", ncores, " cores")
#'     cl <- parallel::makeCluster(ncores)
#'     on.exit(parallel::stopCluster(cl))
#'     parallel::clusterEvalQ(cl, require("glmmTMB"))
#'     parallel::clusterApply(cl, unique.del, deleteGroup, ...)
#'   } else {
#'     lapply(unique.del, deleteGroup, ...)
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
#'     colnames(vc.1) <- names(vc)
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
#'   class(result) <- "influence.glmmTMB"
#'   result
#' }
#'
#' dfbeta.influence.glmmTMB <- function(model, which = c("fixed.effects", "var.cov.comps"), ...){
#'   which <- match.arg(which)
#'   which.cols <- grep(which, names(model), value = T)
#'   b  <- model[[grep('[', which.cols, fixed = T, value = T)]]
#'   b0 <- model[[which]]
#'   b - matrix(b0, nrow = nrow(b), ncol = ncol(b), byrow = TRUE)
#' }
#'
# #' @exportS3Method
#' dffits.glmmTMB <- function(model, preds, data, ...){
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
#'
# #' @exportS3Method
#' press.glmmTMB <- function(model, preds, data, ...){
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
#'
