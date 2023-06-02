#' @title Likelihood Ratio Chi-Square Test
#'
#' @description
#' Performs a likelihood ratio chi-square test.
#'
#' @param x Array. A 2 dimensional contingency table in the form of an array or matrix.
#'
#' @return A list with class "htest" containing: the test statistic, the degrees of
#' freedom, and the p-value
#'
#' @export
lr.test = function(x){
  e = tcrossprod(rowSums(x), colSums(x))/sum(x)
  stat = 2*sum(x*log(x/e))
  names(stat) <- 'X-squared'
  df = prod(dim(x)-1)
  names(df) <- 'DF'
  p = 1-pchisq(stat, df)
  structure(list(statistic = stat, parameter = df,
                 p.value = p, method = 'Likelihood Ratio Chi-Square Test',
                 data.name = deparse(substitute(x))), class = "htest")
}
