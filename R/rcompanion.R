#' @title Histogram with normal curve
#'
#' @description Plots a histogram and superimposes on it a normal probability
#'   density curve with mean and sigma parameters calculated from the data.
#'
#' @param x A vector of numeric values to plot.
#' @param by A vector indicating grouping variables. A separate plot is generated for
#'   each unique value present in \code{by}.
#' @param bar.opts A named list of optional arguments passed to
#'   \code{\link[graphics]{hist}}
#' @param line.opts A named list of optional arguments passed to
#'   \code{\link[graphics]{curve}}
#' @param kernel Logical. Should a kernal density estimate be added to the plot? Defaults
#'  to FALSE.
#' @details This function is loosely adapted from the \code{plotNormalHistogram}
#'   function of the \code{rcompanion} package written by Salvatore Mangiafico (
#'   \url{https://CRAN.R-project.org/package=rcompanion}, see the References
#'   section for full citation).
#'
#' @return \code{NULL}. Produces a plot.
#'
#' @references Mangiafico, S. (2021). "rcompanion: Functions to Support
#'   Extension Education Program Evaluation". R package version 2.3.27.
#'   \url{https://CRAN.R-project.org/package=rcompanion}
#'
#' @export
plot_normal_histogram = function(x, by = NULL, bar.opts = list(), line.opts = list(), kernel = F){
  normcurve = function(x, mu, sig, adjust, ...) {
    curve(dnorm(x, mu, sig)*adjust, add = T, ...)
  }
  no_label = is.null(bar.opts$xlab)
  no_title = is.null(bar.opts$main)

  if (is.null(by)){
    bar.opts$x = x

    lab = unlist(strsplit(deparse(substitute(x)), "$", fixed = T))
    lab = lab[length(lab)]

    if (no_label){
      bar.opts$xlab = lab
    }
    if (no_title){
      bar.opts$main = lab
    }

    h = do.call('hist', bar.opts)
    mu = mean(x, na.rm = T)
    sig = sd(x, na.rm = T)

    bar.opts$freq = ifelse(is.null(bar.opts$freq), T, bar.opts$freq)
    bar.opts$prob = ifelse(is.null(bar.opts$prob), F, bar.opts$prob)

    #peak = which(h$counts == max(h$counts))
    adjust = ifelse(bar.opts$prob == T | bar.opts$freq == F, 1, mean(h$counts/h$density, na.rm = T))
    if (kernel == T){
      suppressWarnings({
        lines(density(x, weights = rep(adjust/length(x), length(x))), lty = 2)
      })
    }
    line.opts = c(line.opts, list(x = x, mu = mu, sig = sig, adjust = adjust))
    do.call(normcurve, line.opts)
  } else {
    indices = sapply(unique(by), function(x, by) {by == x}, by = by)
    for (gr in 1:ncol(indices)){
      z = x[indices[, gr]]
      bar.opts$x = z

      lab = unlist(strsplit(deparse(substitute(x)), "$", fixed = T))
      lab = lab[length(lab)]

      group = unlist(strsplit(deparse(substitute(by)), "$", fixed = T))
      group = group[length(group)]

      title = paste0(lab, ": ", group, ' ', unique(by)[gr])

      if (no_label){
        bar.opts$xlab = lab
      }
      if (no_title){
        bar.opts$main = title
      }

      h = do.call('hist', bar.opts)
      mu = mean(z, na.rm = T)
      sig = sd(z, na.rm = T)

      bar.opts$freq = ifelse(is.null(bar.opts$freq), T, bar.opts$freq)
      bar.opts$prob = ifelse(is.null(bar.opts$prob), F, bar.opts$prob)

      #peak = which(h$counts == max(h$counts))
      adjust = ifelse(bar.opts$prob == T | bar.opts$freq == F, 1, mean(h$counts/h$density, na.rm = T))
      if (kernel == T){
        suppressWarnings({
          lines(density(x, weights = rep(adjust/length(x), length(x))), lty = 2)
        })
      }
      line.opts$x = z
      line.opts$mu = mu
      line.opts$sig = sig
      line.opts$adjust = adjust
      do.call(normcurve, line.opts)
    }
  }
}

#' @title Cramer's V Test
#'
#' @description
#'
#' @param x
#'
#' @details
#'
#' @return
#'
#' @references Mangiafico, S. (2021). "rcompanion: Functions to Support
#'   Extension Education Program Evaluation". R package version 2.3.27.
#'   \url{https://CRAN.R-project.org/package=rcompanion}
#'
#' @export
Cramer_v <- function (x, y = NULL, ci = FALSE, conf = 0.95, type = "perc",
          R = 1000, histogram = FALSE, digits = 4, bias.correct = FALSE,
          reportIncomplete = FALSE, verbose = FALSE, ...)
{
  CV = NULL
  if (is.factor(x)) {
    x = as.vector(x)
  }
  if (is.factor(y)) {
    y = as.vector(y)
  }
  if (is.vector(x) & is.vector(y)) {
    N = length(x)
    Chi.sq = suppressWarnings(chisq.test(x, y, correct = FALSE,
                                         ...)$statistic)
    Phi = Chi.sq/N
    Row = length(unique(x))
    C = length(unique(y))
    CV = sqrt(Phi/min(Row - 1, C - 1))
  }
  if (is.matrix(x)) {
    x = as.table(x)
  }
  if (is.table(x)) {
    TABLE = x
    N = sum(TABLE)
    Chi.sq = suppressWarnings(chisq.test(TABLE, correct = FALSE,
                                         ...)$statistic)
    Phi = Chi.sq/N
    Row = nrow(x)
    C = ncol(x)
    CV = sqrt(Phi/min(Row - 1, C - 1))
  }
  PhiOrg = Phi
  VOrg = CV
  PhiNew = NA
  VNew = NA
  if (bias.correct) {
    Phi = max(0, Phi - ((Row - 1) * (C - 1)/(N - 1)))
    CC = C - ((C - 1)^2/(N - 1))
    RR = Row - ((Row - 1)^2/(N - 1))
    CV = sqrt(Phi/min(RR - 1, CC - 1))
    PhiNew = Phi
    VNew = CV
  }
  if (verbose) {
    cat("\n")
    cat("Rows          =", signif(Row, digits = digits))
    cat("\n")
    cat("Columns       =", signif(C, digits = digits))
    cat("\n")
    cat("N             =", signif(N, digits = digits))
    cat("\n")
    cat("Chi-squared   =", signif(Chi.sq, digits = digits))
    cat("\n")
    cat("Phi           =", signif(PhiOrg, digits = digits))
    cat("\n")
    cat("Corrected Phi =", signif(PhiNew, digits = digits))
    cat("\n")
    cat("V             =", signif(VOrg, digits = digits))
    cat("\n")
    cat("Corrected V   =", signif(VNew, digits = digits))
    cat("\n")
    cat("\n")
  }
  if (bias.correct) {
    PhiNew = max(0, Phi - ((Row - 1) * (C - 1)/(N - 1)))
    CC = C - ((C - 1)^2/(N - 1))
    RR = Row - ((Row - 1)^2/(N - 1))
    CV = sqrt(Phi/min(RR - 1, CC - 1))
  }
  CV = signif(as.numeric(CV), digits = digits)
  if (is.nan(CV) & ci == TRUE) {
    return(data.frame(Cramer.V = CV, lower.ci = NA, upper.ci = NA))
  }
  if (ci == TRUE) {
    if (is.matrix(x)) {
      x = as.table(x)
    }
    if (is.table(x)) {
      Counts = as.data.frame(x)
      Long = Counts[rep(row.names(Counts), Counts$Freq),
                    c(1, 2)]
      rownames(Long) = seq(1:nrow(Long))
    }
    if (is.vector(x) & is.vector(y)) {
      Long = data.frame(x = x, y = y)
    }
    L1 = length(unique(droplevels(Long[, 1])))
    L2 = length(unique(droplevels(Long[, 2])))
    Function = function(input, index) {
      Input = input[index, ]
      NOTEQUAL = 0
      if (length(unique(droplevels(Input[, 1]))) != L1 |
          length(unique(droplevels(Input[, 2]))) != L2) {
        NOTEQUAL = 1
      }
      if (NOTEQUAL == 1) {
        FLAG = 1
        return(c(NA, FLAG))
      }
      if (NOTEQUAL == 0) {
        N = length(Input[, 1])
        Chi.sq = suppressWarnings(chisq.test(Input[,
                                                   1], Input[, 2], correct = FALSE, ...)$statistic)
        Phi = Chi.sq/N
        Row = length(unique(Input[, 1]))
        C = length(unique(Input[, 2]))
        CV = sqrt(Phi/min(Row - 1, C - 1))
        FLAG = 0
        if (bias.correct == TRUE) {
          Phi = max(0, Phi - ((Row - 1) * (C - 1)/(N -
                                                     1)))
          CC = C - ((C - 1)^2/(N - 1))
          RR = Row - ((Row - 1)^2/(N - 1))
          CV = sqrt(Phi/min(RR - 1, CC - 1))
        }
        return(c(CV, FLAG))
      }
    }
    Boot = boot(Long, Function, R = R)
    BCI = boot.ci(Boot, conf = conf, type = type)
    if (type == "norm") {
      CI1 = BCI$normal[2]
      CI2 = BCI$normal[3]
    }
    if (type == "basic") {
      CI1 = BCI$basic[4]
      CI2 = BCI$basic[5]
    }
    if (type == "perc") {
      CI1 = BCI$percent[4]
      CI2 = BCI$percent[5]
    }
    if (type == "bca") {
      CI1 = BCI$bca[4]
      CI2 = BCI$bca[5]
    }
    if (sum(Boot$t[, 2]) > 0 & reportIncomplete == FALSE) {
      CI1 = NA
      CI2 = NA
    }
    CI1 = signif(CI1, digits = digits)
    CI2 = signif(CI2, digits = digits)
    if (histogram == TRUE) {
      hist(Boot$t[, 1], col = "darkgray", xlab = "V",
           main = "")
    }
  }
  if (ci == FALSE) {
    names(CV) = "Cramer V"
    return(CV)
  }
  if (ci == TRUE) {
    return(data.frame(Cramer.V = CV, lower.ci = CI1, upper.ci = CI2))
  }
}


