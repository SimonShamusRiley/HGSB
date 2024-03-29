% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nncov.R
\name{nncov}
\alias{nncov}
\title{Nearest-neighbor covariates}
\usage{
nncov(
  ...,
  covariate,
  thresholds = c(0, 0, 1),
  rownorm = T,
  reduce = T,
  warn = T
)
}
\arguments{
\item{...}{Vector of numeric values indicating the location of observations along each
dimension. Non-numeric values are coerced to numeric values with a warning if
\code{warn = TRUE}.}

\item{covariate}{An optional vector of covaritate values. If provided, the function
returns the spatially-lagged covariate values (one for each dimension supplied via
\code{...}). If \code{covariate} is not supplied, the adjacency matrices (one for
each dimension supplied via \code{...})).}

\item{thresholds}{Numeric. A vector of three values defining, respectively: the maximum
distance at which observations are at considered to be at the "equivalent" locations
in a given dimension, and the minimum and maximum distances distances which define
"adjacent" locations in a dimension. See \code{Details} and \code{Examples} for
additional information.}

\item{rownorm}{Logical. Should the adjacency matrices be row standardized? Defaults to
\code{TRUE}.}

\item{reduce}{Logical. Should the adjacency matrices (or covariate vectors) in each
dimension be reduced to a single adjacency matrix (or covariate vector)? Default is
\code{TRUE}}

\item{warn}{Logical. Issue a warning when arguments to \code{...} have been coerced to
numeric values? Defaults to \code{TRUE}.}
}
\value{
One or more vector of spatially-lagged covariates if \code{covariate} is
supplied; otherwise one or more adjacency matrix.
}
\description{
Calculate spatially-lagged covariates or adjacency matrices for
observations indexed by any arbitrary number of dimensions.
}
\details{
Observations are defined here as being adjacent in any given dimension d (in
the set of dimensions D) if the Euclidean distance between those observations is less
than or equal to t_1 in dimensions d' and greater than t_2 but less than or equal to
t_3 in dimension d.
}
\examples{
# Create and plot example field layout
library(desplot)
field <- expand.grid(Row = 1:4, Col = 1:4)
field$Y <- rnorm(nrow(field))
field$ID <- rownames(field)
desplot(field, ~ Row + Col, text = ID, cex = 2)

# Plot 4 (the fourth row in the resulting matrices)
# should be row-adjacent to plot 3 but not 5 or 8,
# and column-adjacent to 8 but not 3 or 5:
nncov(Row = field$Row, Col = field$Col,
      rownorm = F, reduce = F, warn = F)

# In the reduced adjacency matrix, plot 4 should
# appear adjacent to 3 and 8 only, while plot 10
# should appear adjacent to 6, 11, 9 and 14:
nncov(Row = field$Row, Col = field$Col,
      rownorm = F, reduce = T, warn = F)

# Default thresholds = c(0, 0, 1) generates "rook"
# adjacency; for "Queen" adjacency use
# thresholds = c(1, 0, 1)
nncov(Row = field$Row, Col = field$Col,
      thresholds = c(1, 0, 1),
      rownorm = F, reduce = T, warn = F)
}
\references{
Arbia, G. (2014). "Some Important Spatial Definitions" in A Primer of
Spatial Econometrics with examples in R. New York City, NY: Palgrave Macmillan.
}
