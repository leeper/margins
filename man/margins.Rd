\name{margins}
\alias{margins}
\alias{margins.lm}
\alias{margins.glm}
\alias{margins.plm}
\alias{margins.pglm}
\alias{print.margins}
\alias{summary.margins}
\alias{confint.margins}
\title{Marginal effects for model objects}
\description{Calculate marginal effects for estimated models}
\usage{
margins(x, newdata = NULL, \dots)

\method{margins}{lm}(x, newdata = NULL, at = NULL, atmeans = FALSE, 
                     factors = "continuous", \dots)

\method{margins}{plm}(x, newdata = NULL, at = NULL, atmeans = FALSE, 
                     factors = "continuous", \dots)

\method{margins}{glm}(x, newdata = NULL, at = NULL, atmeans = FALSE, 
                     factors = "continuous", type = "link", \dots)

\method{margins}{pglm}(x, newdata = NULL, at = NULL, atmeans = FALSE, 
                     factors = "continuous", type = "link", \dots)
}
\arguments{
    \item{x}{A model object.}
    \item{newdata}{A data.frame containing the data at which to evaluate the marginal effects, as in \code{predict}.}
    \item{at}{A list of one or more named vectors, specifically values at which to calculate the marginal effects. See examples. NOTE: THIS DOES NOT CURRENTLY WORK WITH VARIABLES THAT ARE TRANSFORMED IN FORMULA EXPRESSIONS (e.g., \code{y~factor(x)} or \code{y~I(x^2)}).}
    \item{atmeans}{A logical indicating whether to calculate marginal effects at the means (i.e., partial effects at the average), as opposed to the default average marginal effects (i.e., average partial effects), which is the default.}
    \item{factors}{A character string indicating whether, if \dQuote{continuous} (the default), to calculate marginal effects treating factor variables as continuous or, if \dQuote{discrete}, to calculate marginal effects for factor variables as discrete first differences (i.e., the difference in fitted values when the factor is 1 versus 0).}
    \item{type}{For \code{margins.glm}, a character string specifying either \dQuote{link} (as in \code{predict.glm}; default; equivalent to Stata's \code{xb()} option) or \dQuote{response} (on the scale of the response variable, e.g., probabilities).}
    \item{\dots}{Arguments passed to methods.}
}
\details{Calculate marginal effects and associated standard errors for a model object.}
\value{An object of class \dQuote{margins}.}
\author{Thomas J. Leeper}
\references{
Greene, W.H. 2012. Econometric Analysis, 7th Ed. Boston: Pearson.

Stata manual: \code{margins}. Retrieved 2014-12-15 from \url{http://www.stata.com/manuals13/rmargins.pdf}.
}
%\seealso{}
\examples{



# Average Marginal Effects (default) versus Marginal Effects at the Mean


# Use of `at`


# Continuous versus discrete handling of factors

}