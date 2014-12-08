\name{margins_at}
\alias{margins_at}
\title{Calculate marginal effects for model objects at specified values}
\description{Calculate marginal effects for estimated models}
\usage{
margins_at(x, at = NULL, atmeans = FALSE, ...)
}
\arguments{
    \item{x}{A model object.}
    \item{at}{TBD}
    \item{atmeans}{A logical value indicating whether to calculate marginal effects at means, or average marginal effects (the default).}
    \item{\dots}{Arguments passed to methods.}
}
\details{Calculate marginal effects and associated standard errors for a model object.}
\value{An object of class \dQuote{margins}.}
\author{Thomas J. Leeper}
%\references{}
%\seealso{}
%\examples{}