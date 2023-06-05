# # cramers v, etc
#
# library(HGSB)
# data(pairing)
# pairing
# x <- pairing
# mar1 <- margin.table(x, margin = 1)
# mar2 <- margin.table(x, margin = 2)
# dim(t(mar1))
# dim(t(t(mar2)))
# sum(mar1*mar2)
#
# data(pairing)
# x <- pairing
# x <- read.csv('raw/pairing.csv')
#
# check1 = any(sapply(c('matrix', 'array', 'table'), `%in%`, class(x))) & length(dim(x)) == 2
# check2 = 'data.frame' %in% class(x) & dim(x)[2] == 3 & sum(sapply(x, is.numeric)) == 1
# if (!check1 & !check2){
#   stop(simpleError('x must be either: (1) a 2-dimensional matrix, array or table, or (2) a data.frame with exactly 3 columns, only one of which is numeric'))
# }
# if (check2) {
#   toFac <- which(!sapply(x, is.numeric) & !sapply(x, is.logical))
#   x[, toFac] <- lapply(x[, toFac], factor)
#   x <- x[order(x[, toFac[1]], x[, toFac[2]])]
#   x <- array(x[, !toFac],
#              dim = c(length(unique(x[, toFac[1]])), length(unique(x[, toFac[2]]))),
#              dimnames = list(names(toFac)[1] = unique(x[, toFac[1]]), names(toFac[2]) = unique(x[, toFac[2]]))
# }
# n <- sum(x)
# rowmars <- matrix(rowSums(x), ncol = 1)
# colmars <- matrix(colSums(x), nrow = 1)
# chisq <- sum((x - (rowmars %*% colmars)/n)^2/((rowmars %*% colmars)/n))
# phi <- chisq/n
# d <- min(dim(x)-1)
# v <- sqrt(phi/d)
# out <- list("phi" = phi,
#             "Cramer's V" = v)
# class(out) = 'htest'
# cat(paste("\tCramer's V Test for Independence\n\n",
#           "data: ", deparse(substitute(x)),
#           "corrected: ", correct,
#           "\U03C6 statistic: ", out$phi))

#### fix var coef ####
# library(lme4)
# library(HGSB)
# data("forage1")
# forage1$rate_num <- as.numeric(as.character(forage1$rate))
# m0 <- lmer(yield_tha ~ rate_num + I(rate_num^2) + rate:rate_num + (1|block), forage1)
# VarCorr(m0)
# newsd <- c(0.55435)
# newsd <- 0.3075
# ff <- dd(c(0))
# opt <- list(par=c(0,0),fval=ff,conv=0)
# lmod <- lFormula(yield_tha ~ rate_num + I(rate_num^2) + rate:rate_num + (1|block), forage1)
# m1X <- mkMerMod(environment(dd), opt, lmod$reTrms, fr = lmod$fr,
#                 mc = quote(hacked_lmer()))
#
# m1 <- lmer(yield_tha ~ rate_num + I(rate_num^2) + (1|block), forage1)
# dd <- as.function(m1)
# ff2 <- dd(sqrt(newsd)/sigma(m1))
# opt2 <- list(par=c(0,0),fval=ff,conv=0)
# m2X <- mkMerMod(environment(dd), opt, lmod$reTrms, fr = lmod$fr,
#                 mc = quote(hacked_lmer()))
#
#
# buildMM <- function(theta) {
#   dd <- as.function(m1)
#   ff <- dd(theta)
#   opt <- list(par=c(0,0),fval=ff,conv=0)
#   mm <- mkMerMod(environment(dd), opt, lmod$reTrms, fr = lmod$fr,
#                  mc = quote(hacked_lmer()))
#   return(mm)
# }
# objfun <- function(x, target=c(0.3075)) {
#   mm <- buildMM(sqrt(x))
#   return(sum((unlist(VarCorr(mm))-target)^2))
# }
# s0 <- c(0.3075)/sigma(m1)^2
# opt <- optim(fn=objfun, par=s0, method = 'L-BFGS-B')
# mm_final <- buildMM(sqrt(opt$par))
# summary(mm_final)

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
#   loo <- function(model, data, grp, level, ...){
#     predict(object = update(model, data = data[data$.groups != grp, ], ...),
#             newdata = data[data$.groups == grp, ], level = 0)
#   }
#   sapply(unique(data$.groups), loo,
#          model = model, data = data, ...)
# }
#
