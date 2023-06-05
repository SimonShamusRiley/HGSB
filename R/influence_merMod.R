# #' @exportS3Method
#' dffits.merMod <- function(model, preds, data, ...){
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
#' press.merMod <- function(model, preds, data, ...){
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
