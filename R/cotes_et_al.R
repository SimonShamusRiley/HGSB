#'@title Calculate the Yield Stability Index of Coates et al. (2002)
#'
#'@description This function is ported from the SAS macro given in Coates et al. (2002),
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
#'  rankings (see Coates et al. (2002) for details). Defaults to 0.10.
#'@param ... Other arguments passed to `nlme::lme()` during model fitting for the
#'  estimation of Shukla's variances and SED's.
#'
#'@return a data frame containing the yield stability rankings, along with some
#'  intermediate values used in their calculation. \code{object}.
#'
#'
#'@export
geys = function(data, response = 'yield', genoytpe = 'geno',
                environment = 'env', block = NULL, alpha = 0.1, ...) {

  fe = as.formula(paste0(response, '~', genotype))
  re = list(nlme::pdIdent(form = ~1),
            nlme::pdIdent(form = as.formula(paste0('~', block, '-1'))),
            nlme::pdIdent(form = as.formula(paste0('~', genotype, '-1'))))
  names(re) = rep(environment, 3)

  if (is.null(block)){
    re = re[c(1, 3)]
  }

  fit1 = nlme::lme(fixed = fe, data = data, random = re, ...)

  emm = suppressWarnings({
   emmeans::emmeans(fit1, as.formula(paste0('~', genotype)), mode ='satterthwaite')
  })

  DF = mean(as.data.frame(pairs(emm))[, 4])
  SEM = mean(as.data.frame(pairs(emm))[, 3])
  out = as.data.frame(emm)[, 1:2]
  colnames(out) = c('Genotype', 'MeanResponse')

  re = list(nlme::pdIdent(form = ~1),
            nlme::pdIdent(form = as.formula(paste0('~', block, '-1'))),
            nlme::pdDiag(form = as.formula(paste0('~', genotype, '-1'))))
  names(re) = rep(environment, 3)

  if (is.null(block)){
    re = re[c(1, 3)]
  }

  fit2 = nlme::lme(fixed = fe, data = data, random = re, ...)

  out$ShuklaVariance = as.numeric(VarCorr(fit2)[grep(genotype, rownames(VarCorr(fit2)), value = T), 1])
  out$MeanRank = rank(out$MeanResponse)
  out = out[order(out$MeanRank, decreasing = T), ]

  tstat = qt(1-alpha/2, df = DF)
  LSD = tstat*SEM

  out$LSDAdjustment = abs((out$MeanResponse-mean(out$MeanResponse)) %/% LSD) + 1
  out$LSDAdjustment = out$LSDAdjustment*sign(out$MeanResponse-mean(out$MeanResponse))
  out$AdjRankYield = out$Rank+out$LSDAdjustment

  thresh = sapply(c(0.01, 0.05, 0.10), qf, df1 = nrow(out)-1, df2 = DF)
  out$StabilityRating = -2*rowSums(t(sapply(out$Shukla, `>`, thresh)))
  out$YieldStabilityRank = out$AdjRankYield+out$StabilityScore
  out$Select = out$YieldStabilityRank > mean(out$YieldStabilityRank)
  return(out)
}

