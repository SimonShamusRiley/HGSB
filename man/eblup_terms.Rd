% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_vcomp.R
\name{eblup_terms}
\alias{eblup_terms}
\title{Extract the fixed and random effects in a fitted model, ordered for use in
specifying estimable linear functions.}
\usage{
eblup_terms(object)
}
\arguments{
\item{object}{A model fitted with \code{\link[nlme]{lme}} or \code{\link[lme4]{lmer}}.}
}
\value{
A column vector of the names of fixed- and random-effects coefficients in \code{object}.
}
\description{
...Guidelines on specifying predictions and explaining wierd df behavior go here...
}
\examples{
library(HGSB)
\dontrun{library(lme4)}
data(trefoil)
fit <- lmer(seed_g ~ 1 + (1|family) + (1|block), trefoil)

eblup_terms(fit)

\dontrun{library(nlme)}
fit <- lme(seed_g ~ 1, trefoil, random = list(family = ~ 1, family = pdIdent(form = ~ block)))

eblup_terms(fit)

}
