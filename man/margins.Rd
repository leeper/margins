\name{margins}
\alias{margins}
\alias{margins.lm}
\alias{margins.glm}
\alias{margins.plm}
\alias{margins.polr}
\alias{margins.censReg}
\alias{print.margins}
\alias{summary.margins}
\alias{confint.margins}
\title{Marginal effects for model objects}
\description{Calculate marginal effects for estimated models}
\usage{
margins(x, newdata = NULL, \dots)

\method{margins}{lm}(x, newdata = NULL, \dots)

\method{margins}{glm}(x, newdata = NULL, type = "link", \dots)
}
\arguments{
    \item{x}{A model object.}
    \item{newdata}{A data.frame containing the data at which to evaluate the marginal effects, as in \code{predict}.}
    \item{type}{For \code{margins.glm}, a character string specifying either \dQuote{link} (as in \code{predict.glm}; default; equivalent to Stata's \code{xb()} option) or \dQuote{response} (on the scale of the response variable).}
    \item{\dots}{Arguments passed to methods.}
}
\details{Calculate marginal effects and associated standard errors for a model object.}
\value{An object of class \dQuote{margins}.}
\author{Thomas J. Leeper}
%\references{}
%\seealso{}
%\examples{}