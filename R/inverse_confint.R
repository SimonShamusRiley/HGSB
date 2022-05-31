#' @title Calculate inverse predictions with confidence intervals for probit
#'  dose response curve
#'
#' @description This function takes fits a generalized linear model via the
#'   \code{\link[stats]{glm}} function with family = binomial(link = probit),
#'   then uses the procedures described in Finney (1971) to generate inverse
#'   predictions and corrected confidence intervals. The code was contributed
#'   by James Collee.
#'
#' @param n.treated A vector of integers describing the number of subjects
#'   exposed at each level of dosage. The vctor must be the same length as that
#'   supplied for \code{dose}
#' @param n.responding A vector of integers describing the number of exposed
#'   subjects who responded at each level of dosage. The vector must be the
#'   same length as that supplied for \code{dose} and no no integer may exceed
#'   the corresponding \code{n.treated} value.
#' @param dose A vector of numeric values.
#' @param conf.level Numeric. A value between 0 and 1 controlling the width of
#'   the confidence intervals.
#' @param p An optional vector of probabilities at which to calculate dosages.
#'   If left as \code{NULL}, dosages are calculated for probabilities ranging
#'   from 0.01 to 0.99 at intervals of 0.01 (p < 0.1 and p > 0.9) or 0.05 (0.1
#'   < p < 0.9)
#' @param ... Other options passed to \code{glm}.
#'
#' @return a \code{data.frame}.
#'
#' @references Finney, D.J. (1971) Probit Analysis. 3rd Edition, Cambridge
#'   University Press: Cambridge.
#'
#' @export
inverse_confint <- function(n.treated, n.responding, dose,
                            conf.level=.95, probs = NULL, ...) {

  check1 <- all(n.responding/n.treated <= 1 & n.responding/n.treated >= 0)
  if (!check1){
    stop(simpleError('Invalid values provided for n.treated and/or n.responding'))
  }
  check2 <- length(n.treated) == length(n.responding) &
    length(n.treated) == length(dose)
  if (!check2){
    stop(simpleError('n.treated, n.responding and dose are not all the same length'))
  }

  if (is.null(probs)){
    p <- c(seq(0.01, 0.1, .01), seq(0.15, 0.9, 0.05), seq(0.91, 0.99, .01))
  } else {
    p <- probs
  }

  ## r=number responding, n=number treated, d=dose (untransformed), confidence
  #interval level,
  mod <- glm(cbind(n.responding, (n.treated-n.responding)) ~ dose,
             family = binomial(link=probit), ...)
  ## Calculate heterogeneity correction to confidence
  #intervals according to Finney, 1971, (p. ## 72, eq. 4.27; also called "h")
  het = deviance(mod)/df.residual(mod)
  if(het < 1){het = 1} ### Heterogeneity cannot be less than 1

  ## Extract slope and intercept
  summary <- summary(mod, dispersion = het, cor = F)
  intercept <- summary$coefficients[1]
  interceptSE <- summary$coefficients[3]
  slope <- summary$coefficients[2]
  slopeSE <- summary$coefficients[4]
  z.value <- summary$coefficients[6]
  N <- sum(n.treated)

  ## Intercept (alpha)
  b0<-intercept
  ## Slope (beta)
  b1<-slope
  ## Slope variance
  vcov = summary(mod)$cov.unscaled
  var.b0<-vcov[1,1]
  ## Intercept variance
  var.b1<-vcov[2,2]
  ## Slope intercept covariance
  cov.b0.b1<-vcov[1,2]

  ## Adjust alpha depending on heterogeneity (Finney, 1971, p. 76)
  alpha<-1-conf.level
  if(het > 1) {
    talpha <- -qt(alpha/2, df=df.residual(mod))
  } else {talpha <- -qnorm(alpha/2)
  }

  ## Calculate g (Finney, 1971, p 78, eq. 4.36) "With almost all good sets of
  ## data, g will be substantially smaller than 1.0 and seldom greater than
  ## 0.4."
  g <- het * ((talpha^2 * var.b1)/b1^2)

  ## Calculate theta.hat for all LD levels based on probits in eta (Robertson
  ## et al., 2007, pg. 27; or "m" in Finney, 1971, p. 78)
  eta = family(mod)$linkfun(p)  #probit distribution curve
  theta.hat <- (eta - b0)/b1

  ## Calculate correction of fiducial limits according to Fieller method
  ## (Finney, 1971, p. 78-79. eq. 4.35)
  const1 <- (g/(1-g))*(theta.hat + cov.b0.b1/var.b1)
  const2a <- var.b0 + 2*cov.b0.b1*theta.hat +
    var.b1*theta.hat^2 - g*(var.b0 - (cov.b0.b1^2/var.b1))
  const2 <- talpha/((1-g)*b1) * sqrt(het * (const2a))

  ## Calculate the confidence intervals LCL=lower, UCL=upper (Finney, 1971, p.
  ## 78-79. eq. 4.35)
  LCL <- (theta.hat + const1 - const2)
  UCL <- (theta.hat + const1 + const2)

  ## Calculate variance for theta.hat (Robertson et al., 2007, pg. 27)
  var.theta.hat <- (1/(theta.hat^2)) * (var.b0 + 2*cov.b0.b1*theta.hat +
                                          var.b1*theta.hat^2 )

  ## Make a data frame from the data at all the different values
  ECtable <- data.frame(
    "Prob"=p,
    "N" = N,
    "Dose"=theta.hat,
    "LCL"=LCL,
    "UCL"=UCL)

  return(ECtable)
}
