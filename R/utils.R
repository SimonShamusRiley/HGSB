#' @title Print compact versions of data.frames, matrices and arrays
#'
#' @description This is a convenience function for concisely and compactly displaying
#'   large data frames, matrices and arrays using ellipses to indicate omitted rows and/or
#'   columns. All columns in the outputted \code{data.frame}, \code{matrix}, or
#'   \code{array} are character values, and thus the output of this function should only be
#'   used for printing or displaying data sets.
#'
#' @param x A data.frame, matrix or array, or any object which can be coerced to a
#'   data.frame.
#' @param rows A vector of two integers giving the number of initial and final rows of
#'   \code{x} to include in the output, respectively. If only a single value is provided,
#'   that number is used for the number of both initial and final rows of \code{x} to
#'   include. Defaults to NULL, in which case all rows are included.
#' @param cols A vector of two integers giving the number of initial and final columns of
#'   \code{x} to include in the output, respectively. If only a single value is provided,
#'   that number is used for both the number of initial and final columns of \code{x} to
#'   include. Defaults to \code{NULL}, in which case all columns are included.
#' @param digits The number of digits at which to round numeric columns in \code{x}
#' @param warn Logical. Should a warning be provided if invalid arguments are provided for
#'   \code{rows} and/or \code{cols}? Defaults to \code{FALSE}.
#' @return Creates and prints a \code{data.frame} of all character vectors, where the
#'   middle rows and/or columns (defined by the arguments \code{rows} and \code{cols})
#'   have been omitted, and replaced with a single row and/or column of ellipses ('...').
#'
#' @details If \code{x} is a data.frame, original column and row names are retained and
#'   printed. If it is an array with non-\code{NULL} dimnames, new column names are
#'   created (and printed) from them. If \code{x} is a matrix, the output is printed
#'   without row or column names. If invalid values are provided for \code{rows} and/or
#'   \code{cols} arguments, the invalid value(s) are ignored (silently if \code{warn = F})
#'   and all rows and/or columns are printed.
#'
#' @export
print_condensed <- function(x, rows = NULL, cols = NULL, digits = 0, warn = F){
  if (!all(class(x) %in% c('data.frame', 'matrix', 'array'))){
  x <- tryCatch(as.data.frame(x),
                error = function(e) stop(simpleError(paste0('x is of class "', paste0(unlist(class(x)), collapse = ' & '), '", and is not (coercible to) a data.frame, array or matrix'))))

  }

  dimlabs <- names(attr(x, 'dimnames'))
  collength <- length(cols[cols > 0])
  keepcols <- switch(collength,
                     c(1:cols, (ncol(x)-cols+1):ncol(x)),
                     c(1:cols[1], (ncol(x)-cols[2]+1):ncol(x)))
  if (length(keepcols) > ncol(x) | length(keepcols) == 0){
    if (warn){
      warning(simpleMessage(paste0('Invalid col argument: printing all ', ncol(x), ' col(s)')))
    }
    keepcols <- 1:ncol(x)
  }

  # numeric (but not integer) columns get rounded while keeping trailing zeroes
  # non-numeric & non-integer columns become characters
  if ('data.frame' %in% class(x)){
    nums <- sapply(x, is.numeric)
    ints <- sapply(x, is.integer)
    chars <- !(nums | ints)
  } else {
    nums <- apply(x, 2, is.numeric)
    ints <- apply(x, 2, is.integer)
    chars <- !(nums | ints)
  }

  # extract top rows, round numeric cols to digits and then convert to text with
  # trailing zeros
  x[, nums]  <- apply(x[, nums, drop = F], 2, round, digits = digits)
  x[, nums]  <- apply(x[, nums, drop = F], 2, sprintf, fmt = paste0('%.', digits, 'f'))
  x[, chars] <- apply(x[, chars, drop = F], 2, as.character)

  col_break <- which(diff(keepcols) > 1)
  if (as.logical(length(col_break))){
    left <- keepcols[1:col_break]
    right <- keepcols[(col_break+1):length(keepcols)]
    x <- cbind(x[, left, drop = F], rep('...', nrow(x)), x[, right, drop = F])
    if (!is.null(colnames(x))){
      colnames(x)[col_break + 1] <- '...'
    }
  }

  rowlength <- length(rows[rows > 0])
  keeprows <- switch(rowlength,
                     c(1:rows, (nrow(x)-rows+1):nrow(x)),
                     c(1:rows[1], (nrow(x)-rows[2]+1):nrow(x)))
  if (length(keeprows) > nrow(x) | length(keeprows) == 0){
    if (warn){
      warning(simpleMessage(paste0('Invalid row argument: printing all ', nrow(x), ' row(s)')))
    }
    keeprows <- 1:nrow(x)
  }

  row_break <- which(diff(keeprows) > 1)
  if (as.logical(length(row_break))){
    top <- keeprows[1:row_break]
    bottom <- keeprows[(row_break+1):length(keeprows)]
    x <- rbind(x[top, , drop = F], rep('...', ncol(x)), x[bottom, , drop = F])
    if (!is.null(rownames(x))){
      rownames(x)[row_break + 1] <- '...'
    }
  }
  names(attr(x, 'dimnames')) <- dimlabs
  if (is.null(rownames(x)) & is.null(colnames(x))){
    prmatrix(x, rowlab = rep('', nrow(x)), collab = rep('', ncol(x)), quote = F )
  } else {
    noquote(x)
  }
}

#' @title Calculate the greatest common factor for a vector of values
#'
#' @description Euclid's algorithm is employed to calculate the greatest common factor
#'   from a set of values.
#'
#' @param x Vector of integers. NOTE that decimals will be silently dropped!
#'
#' @return Integer
#'
#' @export
gcf <- function(x){

  x <- x[order(x, decreasing = T)]
  for (i in 1:(length(x)-1)){
    z <- x[1:2]
    r <- 1
    while(r > 0){
      z <- z[order(z, decreasing = T)]
      #q <- z[1] %/% z[2]
      r <- z[1] %% z[2]
      z[1] <- r
    }
    z <- max(z)
    x <- c(z, x[-c(1:2)])
  }
  x
}

#' @title Coerce objects of class "by" to a data.frame
#'
#' @param x An object of class "by".
#' @param ... Other arguments passed to data.frame
#'
#' @return A data.frame
#'
#' @details Columns are converted to numeric where as.numeric does not produce any
#'   additional NA values.
#' @export
as.data.frame.by <- function(x, row_suffix = F, ...){
  if (row_suffix == FALSE) {
    row_names = as.character(attr(x, "dimnames")[[1]])
  } else if (row_suffix == TRUE) {
    row_suffix <- names(attr(x, "dimnames"))
    row_suffix <- gsub(pattern = '$', replacement = '_', row_suffix, fixed = T)
    row_names <- paste(row_suffix, as.character(attr(x, "dimnames")[[1]]), sep = ':')
  } else {
    row_names <- paste(row_suffix, as.character(attr(x, "dimnames")[[1]]), sep = ':')
  }
  out <- sapply(x, FUN = unlist)
  if (is.null(dim(out))){
    out <- as.data.frame(matrix(out, dimnames = list(row_names, as.character(attr(x, 'call'))[4])))
  } else {
    out <- as.matrix(out)
    out <- data.frame(t(as.data.frame(out)),
                      row.names = row_names)
    try_num <- sapply(suppressWarnings(lapply(out[, 1:ncol(out)], as.numeric)), function(x) {sum(is.na(x))})
    already_na <- sapply(out[, 1:ncol(out)], function(x){sum(is.na(x))})
    num <- which(try_num == already_na)
    out[, num] <- lapply(out[, num], as.numeric)
  }
  return(out)
}

#' @title Coerce objects of class "infl" to a data.frame
#'
#' @param x An object of class "infl" (produced by \code{\link{influence.measures}} or \code{\link{influence_measures}}.
#' @param ... Other arguments passed to data.frame
#'
#' @return A data.frame
#'
#' @export
as.data.frame.infl <- function(x, ...){
  as.data.frame(x$infmat)
}

#' @importFrom nlme getData

#' @export
getData.glmmTMB <- function(object){
  object$frame
}

#' @title Create formulas containing all combinations of 1 to *N* predictor variables
#' @param form The maximal model formula.
#' @param n.max Integer. The largest number of predictor variables to include in the formulae.
#' @param marginality Logical. Should a formula be dropped if it contains an interaction
#'   term for which one or more of the main effects is missing? Defaults to \code{TRUE}.
#' @return A list of formulae
#' @export
formula_comb <- function(form, n.max, marginality = T){
  if (class(form) != 'formula' | length(form) != 3){
    stop(simpleError('form must be a two sided formula'))
  }
  terms <- attr(terms.formula(form), 'term.labels')
  if (length(terms) == 1){
    stop(simpleError('form must contain more than 1 predictor variable'))
  }
  y <- as.character(form[[2]])
  if (missing(n.max)){
    n.max <- length(terms)
  } else {
    if (n.max > length(terms)){
      simpleWarning(paste('n.max cannot exceed terms in form and has been reset to', length(terms)))
      n.max <- length(terms)
    }
  }

  mult <- sapply(2:n.max, combn, x = terms)
  rhs <- unlist(sapply(mult, function(x){
    apply(x, 2, paste, collapse = ' + ')
  }))
  rhs

  marginal <- function(x){
    main <- grep(':', x, value = T, invert = T)
    ints <- strsplit(grep(':', x, value = T), split = ':', fixed = T)
    ints <- unique(unlist(ints))
    all(ints %in% main)
  }

  if (isTRUE(marginality)){
    rhs_termlist <- strsplit(rhs, split = ' + ', fixed = T)
    rhs <- rhs[sapply(rhs_termlist, marginal)]
  }

  rhs <- c(terms, rhs)

  forms <- unlist(sapply(paste(y, '~', rhs), formula, USE.NAMES = T))
  return(forms)
}



