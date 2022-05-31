#' @title Summarize the performance of a diagnostic test
#'
#' @description Calculates common measures of performance for a diagnostic or
#'   other binary classification test, along with confidence intervals.
#'
#' @param TP Integer. The count of results which are true positives.
#' @param FN Integer. The count of results which are false negatives.
#' @param FP Integer. The count of results which are false positives.
#' @param TN Integer. The count of results which are true negatives
#' @param conf.level Numeric, The confidence level for the confidence interval.
#'   Defaults to 0.95.
#' @param digits Integer. The number of decimal places at which to round.
#'  Defaults to 4.
#'
#' @details This function is adapted from the \code{diagnostic} function of the
#'   \code{ThresholdROC} package written by Sara Perez-Jaume, Konstantina
#'   Skaltsa, Natalia Pallares and Josep L. Carrasco (DOI:
#'   10.18637/jss.v082.i04, see the References section for full citation).
#'
#' @return A data.frame with ten rows and three columns containing the point
#'   estimate and confidence intervals for the following statistical measures:
#'   sensitivity, specificity, positive predictive value, negative predictive
#'   value, positive likelihood ratio, negative likelihood ratio, odds ratio,
#'   Youden index, accuracy and error rate.
#'
#' @references Perez-Jaume S, Skaltsa K, Pallarès N, Carrasco JL (2017).
#'   "ThresholdROC: Optimum Threshold Estimation Tools for Continuous Diagnostic
#'   Tests in R." _Journal of Statistical Software_, _82_(4), 1-21. doi:
#'   \href{https://www.jstatsoft.org/article/view/v082i04}{10.18637/jss.v082.i04}.
#'
#' @export
diagnostic_summary <- function(TP, FN, FP, TN, conf.level=0.95, digits = 4){

  # Estimate + CI for Odds Ratio
  OR <- function(TP, FN, FP, TN, conf.level=0.95){
    # Sensitivity
    Se <- TP/(TP + FN)
    # Specificity
    Sp <- TN/(FP + TN)

    z <- qnorm(1-(1-conf.level)/2)
    OR <- (Se/(1-Se))/((1-Sp)/Sp)
    var.ln.OR <- 1/TP+1/FP+1/FN+1/TN
    OR.ci <- c(OR, OR*exp(-z*sqrt(var.ln.OR)), OR*exp(z*sqrt(var.ln.OR)))
    return(OR.ci)
  }

  # Estimate + CI for LR+ and LR-
  LR <- function(TP, FN, FP, TN, which, conf.level=0.95){
    # Sensitivity
    Se <- TP/(TP + FN)
    # Specificity
    Sp <- TN/(FP + TN)

    z <- qnorm(1-(1-conf.level)/2)
    if (which == "+"){
      LRP <- Se/(1-Sp)
      var.ln.LRP <- (1-Se)/TP+Sp/FP
      LRP.ci <- c(LRP, LRP*exp(-z*sqrt(var.ln.LRP)), LRP*exp(z*sqrt(var.ln.LRP)))
      return(LRP.ci)
    }else if (which=="-"){
      LRN <- (1-Se)/Sp
      var.ln.LRN <- Se/FN+(1-Sp)/TN
      LRN.ci <- c(LRN, LRN*exp(-z*sqrt(var.ln.LRN)), LRN*exp(z*sqrt(var.ln.LRN)))
      return(LRN.ci)
    }else{
      stop("'which' must be '+' or '-'")
    }
  }

  # Sensitivity (Se)
  Se <- TP/(TP + FN)
  # Specificity (Sp)
  Sp <- TN/(FP + TN)

  # CI for Se and Sp
  Se.ci <- c(Se, binom.test(TP, TP + FN, conf.level=conf.level)$conf.int)
  Sp.ci <- c(Sp, binom.test(TN, FP + TN, conf.level=conf.level)$conf.int)


  # PPV and NPV
  # Positive predictive value
  PPV <- TP/(TP + FP)
  # Negative predictive value
  NPV <- TN/(TN + FN)
  # CI for PPV and NPV
  PPV.ci <- c(PPV, binom.test(TP, TP + FP, conf.level=conf.level)$conf.int)
  NPV.ci <- c(NPV, binom.test(TN, TN + FN, conf.level=conf.level)$conf.int)

  # LR+
  if (Sp==1 | Se==0){
    # applying continuity correction
    LRP.ci <- LR(TP + 0.5, FN + 0.5, FP + 0.5, TN + 0.5, "+", conf.level)
  }else{
    LRP.ci <- LR(TP, FN, FP, TN, "+", conf.level)
  }

  # LR-
  if (Sp==1 | Se==0){
    # applying continuity correction
    LRN.ci <- LR(TP + 0.5, FN + 0.5, FP + 0.5, TN + 0.5, "-", conf.level)
  } else {
    LRN.ci <- LR(TP, FN, FP, TN, "-", conf.level)
  }

  # Odds ratio
  if (any(c(TP, FN, FP, TN) == 0)){
    # applying continuity correction
    OR.ci <- OR(TP + 0.5, FN + 0.5, FP + 0.5, TN + 0.5, conf.level)
  } else {
    OR.ci <- OR(TP, FN, FP, TN, conf.level)
  }

  # Youden index
  YI <- Se + Sp - 1
  var.YI <- Se*(1-Se)/(TP+FN) + Sp*(1-Sp)/(FP+TN)
  z <- qnorm(1-(1-conf.level)/2)
  YI.ci <- c(YI, YI - z*sqrt(var.YI), YI + z*sqrt(var.YI))

  # Accuracy (probability of a correct test result)
  Acc <- (TP+TN)/(TP+FP+FN+TN)
  # CI for accuracy
  Acc.ci <- c(Acc, binom.test(TP+TN, TP+FP+FN+TN, conf.level=conf.level)$conf.int)

  # Error rate
  ER <- (FP + FN)/(TP+TN+FP+FN)
  # CI for error rate
  ER.ci <- c(ER, binom.test(FP+FN, TP+FP+FN+TN, conf.level=conf.level)$conf.int)

  # table of results
  res <- rbind(Se.ci, Sp.ci, PPV.ci, NPV.ci, LRP.ci,
               LRN.ci, OR.ci, YI.ci, Acc.ci, ER.ci)

  colnames(res) <- c("Estimate", paste("Lower CI (", conf.level*100, "%)", sep=""),
                     paste("Upper CI (", conf.level*100, "%)", sep=""))
  rownames(res) <- c("Sensitivity", "Specificity", "Pos.Pred.Val.", "Neg.Pred.Val.",
                     "LR+", "LR-", "Odds ratio", "Youden index", "Accuracy",
                     "Error rate")
  res <- round(as.data.frame(res), 4)
  return(res)
}

