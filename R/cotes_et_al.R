#'@title Calculate the Yield Stability Index of Cotes et al. (2002)
#'
#'@description This function is ported from the SAS macro given in Cotes et al. (2002),
#'  which those authors describe as being derived from the approach described by Kang
#'  (1996).
#'
#'@param data data.frame. The data frame containing all the columns given by the
#'  parameters response, environment, genotype and, optionally, block.
#'@param response character. The name of the column which contains the response variable.
#'  The default is "yield". Also note that in selecting genotypes, it is assumed that
#'  higher values of the response are better.
#'@param genotype character. The name of column containing the genotype labels. The
#'  default is "geno".
#'@param environment character. The name of the column containing the environemnt labels.
#'  The default is "env".
#'@param block character. The name of the column containing the block labels. The default
#'  is NULL, corresponding to no blocks (i.e a completely randomized design at all
#'  locations).
#'@param alpha The type I error rate to use for making the LSD adjustment to the yield
#'  rankings (see Cotes et al. (2002) for details). Defaults to 0.10.
#'@param ... Other arguments passed to `nlme::lme()` during model fitting for the
#'  estimation of Shukla's variances and SED's.
#'
#'@return a data frame containing the yield stability rankings, along with some
#'  intermediate values used in their calculation. \code{object}.
#'
#'@references Cotes, J. M., Ñustez, C. E., Martinez, R., & Estrada, N. (2002). Analyzing
#'  genotype by environment interaction in potato using yield-stability index. American
#'  Journal of Potato Research, 79(3), 211–218. https://doi.org/10.1007/BF02871937
#'@references Kang, M. S., & Magari, R. (1996). New development in selecting for
#'  phenotypic stability in crop breeding. In M. S. Kang & H. G. Gauch (Eds.), Genotype by
#'  Environment Interaction (First Edition). CRC-Press.
#'
#'@export
cotes_stability = function(data, response = 'yield', genotype = 'geno',
                           environment = 'env', block = NULL, alpha = 0.1, verbose = T) {
  require(nlme)
  require(emmeans)

  if (!all(c(response, genotype, environment, block) %in% colnames(data))) {
    stop(simpleError('One or more of the supplied variable names is not present in the data'))
  }

  fe = as.formula(paste0(response, '~', genotype))
  re = list(nlme::pdIdent(form = ~1),
            nlme::pdIdent(form = as.formula(paste0('~', block, '-1'))),
            nlme::pdIdent(form = as.formula(paste0('~', genotype, '-1'))))
  names(re) = rep(environment, 3)

  if (is.null(block)){
    re = re[c(1, 3)]
  }
  if (verbose){
    cat('Calculating marginal mean responses...\n')
  }
  fit1 = nlme::lme(fixed = fe, data = data, random = re)

  emm = suppressWarnings({
   emmeans::emmeans(fit1, as.formula(paste0('~', genotype)), data = getData(fit1))
  })

  if (verbose){
    cat('Calculating Satterthwaite DF and least significant difference\n')
  }

  call = formula(fit1$terms)
  A = fit1$apVar
  theta = attr(A, "Pars")
  V = fit1[['varFix']]
  sig = 0.01 * fit1$sigma
  yname = all.vars(eval(call))[1]
  y = data[[yname]]
  n = length(y)
  fit1$call[[2]] = call
  dat = t(replicate(2 + length(theta), {
    data[[yname]] = y + sig * rnorm(n)
    mod = update(fit1, data = data)
    c(attr(mod$apVar, "Pars") - theta, as.numeric(mod[['varFix']] -
                                                    V))
  }))
  dimnames(dat) = c(NULL, NULL)
  xcols = seq_along(theta)
  B = lm.fit(dat[, xcols], dat[, -xcols])$coefficients
  G = lapply(seq_len(nrow(B)), function(i) matrix(B[i, ],
                                                     nrow = nrow(V)))
  dfargs = list(V = V, A = fit1$apVar, G = G)
  dffun = function(k, dfargs) {
    est = tcrossprod(crossprod(k, dfargs$V), k)
    g = sapply(dfargs$G, function(M) tcrossprod(crossprod(k,
                                                          M), k))
    varest = tcrossprod(crossprod(g, dfargs$A), g)
    2 * est^2/varest
  }

  DF = dffun(k = as.data.frame(emm)[, 2], dfargs)
  SEM = mean(as.data.frame(pairs(emm))[, 3])
  tstat = qt(1-alpha/2, df = DF)
  LSD = tstat*SEM

  out = as.data.frame(emm)[, 1:2]
  colnames(out) = c('Genotype', 'MeanResponse')

  re = list(nlme::pdIdent(form = ~1),
            nlme::pdIdent(form = as.formula(paste0('~', block, '-1'))),
            nlme::pdDiag(form = as.formula(paste0('~', genotype, '-1'))))
  names(re) = rep(environment, 3)

  if (is.null(block)){
    re = re[c(1, 3)]
  }
  if (verbose){
    cat("Calculating Shukla's variance (slow)...\n")
  }
  fit2 = nlme::lme(fixed = fe, data = data, random = re)

  out$ShuklaVariance = as.numeric(VarCorr(fit2)[grep(genotype, rownames(VarCorr(fit2)), value = T), 1])
  out$MeanRank = rank(out$MeanResponse)
  out = out[order(out$MeanRank, decreasing = T), ]

  if (verbose){
    cat('Calculating yield stability rankings...\n')
  }
  out$LSDAdjustment = abs((out$MeanResponse-mean(out$MeanResponse)) %/% LSD) + 1
  out$LSDAdjustment = out$LSDAdjustment*sign(out$MeanResponse-mean(out$MeanResponse))
  out$AdjRankYield = out$MeanRank+out$LSDAdjustment

  thresh = sapply(c(0.01, 0.05, 0.10), qf, df1 = nrow(out)-1, df2 = DF)
  out$StabilityRating = -2*rowSums(t(sapply(out$Shukla, `>`, thresh)))
  out$YieldStabilityRank = out$AdjRankYield+out$StabilityRating
  out$Select = out$YieldStabilityRank > mean(out$YieldStabilityRank)
  if (verbose){
    cat('Finished!\n')
  }
  return(out)
}

