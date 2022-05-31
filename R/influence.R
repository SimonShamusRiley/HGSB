#' @title Additional methods for calculating influence measures
#'
#' @aliases influence_measures influence.lme
#' @description Additional measures of influence and case-deletion diagnostics for regression models.
#'
#' @param model An fitted model
#' @param groups Character. An optional column name in \code{data} defining the groupings to be removed in calculating leave-one-out deletion diagnostics. If not supplied, defaults to each row in \code{data}.
#' @param data A data.frame. The data used in fitting the original model. Only required when no method for \code{getData} exists.
#' @param infl An object created by calling \code{influence()}.
#' @param ncores Integer. The number of cores to use for parallel computing of influence measures.
#' @param level Integer. Level of conditioning in calculating residuals, predictions, etc. Level = 0 returns marginal residuals.
#' @param ... Additional arguments passed to other functions.
#'
#' @details When the HGSB package is loaded, S3 methods for models of class \code{lm},
#'   \code{glm} and \code{mlm} are registered which use the code from the
#'   \code{stats::covratio}, \code{stats::dffits} functions. These base functions are then
#'   masked by generics of the same name, allowing these methods to be extended to
#'   additional classes of models.
#'
#'   The following methods are then registered for models of class \code{lme}:
#'   \code{influence}, \code{dfbeta} and \code{dfbetas} (code adopted/adapted from Fox and
#'   Weisberg, 2019); \code{hatvalues}, \code{cooks_distance} and \code{dffits} (following
#'   the approaches described in Christensen, Pearson & Johnson, 1992), and
#'   \code{covratio}, \code{rstandard}, and \code{press} (following the methods described
#'   in SAS Institute, 2011). Note that, somewhat confusingly, \code{rstandard} calculates
#'   what SAS refers to as internally studentized residuals, not what SAS defines as
#'   standardized residuals.
#'
#'   In the future, these methods may be extended to models fit
#'   using other functions from the \code{nlme} package (e.g. \code{nlme::gls}), or other
#'   packages entirely (e.g. \code{glmmTMB}).
#'
#' @references Christensen, Ronald, Larry M. Pearson, and Wesley Johnson. 1992.
#'   “Case-Deletion Diagnostics for Mixed Models.” Technometrics 34 (1): 38.
#'   URL:https://doi.org/10.2307/1269550.
#' @references John Fox and Sanford Weisberg. 2019. An {R} Companion to Applied
#'   Regression, Third Edition. Thousand Oaks CA: Sage.
#'   URL:https://socialsciences.mcmaster.ca/jfox/Books/Companion/
#' @references SAS Institute Inc. 2011. "The Mixed Procedure: Residuals and Influence
#'   Diagnostics" in the SAS/STAT® 9.3 User’s Guide. Cary, NC:SAS Institute Inc.
#'

#' @export
dffits <- function(model, ...){
  UseMethod('dffits', model)
}
#' @exportS3Method
dffits.lm <- stats::dffits
#' @exportS3Method
dffits.glm <-  stats::dffits
#' @exportS3Method
dffits.mlm <- stats::dffits

#' @export
covratio <- function(model, ...){
  UseMethod('covratio', model)
}
#' @exportS3Method
covratio.lm <- stats::covratio
#' @exportS3Method
covratio.glm <- stats::covratio

#' @export
press <- function(model, ...){
  UseMethod('press', model)
}

#' @export
cooks_distance <- function(model, ...){
  UseMethod('cooks_distance', model)
}
#' @exportS3Method
cooks_distance.lm <- stats:::cooks.distance.lm
#' @exportS3Method
cooks_distance.glm <- stats:::cooks.distance.glm
#' @exportS3Method
cooks_distance.lmer <- lme4:::cooks.distance.merMod
#' @exportS3Method
cooks_distance.influence.lmer <- lme4:::cooks.distance.influence.merMod

#' @export
influence_measures <- function(model, infl, ...){
  UseMethod('influence_measures', model)
}

#' @exportS3Method
influence_measures.lm <- stats::influence.measures

#' @exportS3Method
influence_measures.glm <- stats::influence.measures

#' @exportS3Method
print.infl = function(x, digits = max(3L, getOption("digits") - 4L), ...){
  cat("Influence measures of\n\t", deparse(x$call), ":\n\n")
  print(data.frame(x$infmat), digits = digits, ...)
  invisible(x)
}

