\name{margins}
\alias{margins}
\title{Marginal effects for model objects}
\description{Calculate marginal effects for estimated models}
\usage{
margins(x, atmeans = FALSE, at = NULL, ...)
}
\arguments{
    \item{x}{}
    \item{atmeans}{}
    \item(at}{}
    \item{...}{}
}
\details{Calculate marginal effects and associated standard errors for a model object.}
\value{An object of class \dQuote{margins}.}
\author{Thomas J. Leeper}
%\references{}
%\seealso{}
\examples{

x <- lm(grade ~ gpa + tuce + psi, data = d)
# margins, dydx(*) atmeans
margins(x, atmeans = TRUE)
# margins, dydx(*)
margins(x)
x <- glm(grade ~ gpa + tuce + psi, data = d, family = gaussian)
# margins, dydx(*) atmeans
margins(x, atmeans = TRUE)
# margins, dydx(*)
margins(x)



x <- glm(grade ~ gpa + tuce + psi, data = d, family = binomial)
# margins, dydx(gpa)
margins(x)["gpa",]
# margins, dydx(gpa) at(gpa = (0 (1) 5))
t(sapply(0:5, function(a) margins(x, at = list(gpa = a))["gpa",]))
# margins, dydx(gpa) at(gpa = (0 (1) 5)) atmeans
t(sapply(0:5, function(a) margins(x, atmeans = TRUE, at = list(gpa = a))["gpa",]))



x <- glm(grade ~ gpa + tuce * psi, data = d, family = gaussian)
# margins, dydx(*) atmeans
margins(x, atmeans = TRUE)
# margins, dydx(*)
margins(x)

x <- glm(grade ~ gpa + factor(tuce) + psi, data = d, family = gaussian)
# margins, dydx(*) atmeans
margins(x, atmeans = TRUE)
# margins, dydx(*)
margins(x)

x <- glm(gpa ~ grade + tuce + psi, x = TRUE, data = d, family = Gamma)
margins(x)

x <- glm(gpa ~ grade + tuce + psi, x = TRUE, data = d, family = inverse.gaussian(link="1/mu^2"))
margins(x)
x <- glm(gpa ~ grade + tuce + psi, x = TRUE, data = d, family = inverse.gaussian(link="inverse"))
margins(x)
x <- glm(gpa ~ grade + tuce + psi, x = TRUE, data = d, family = inverse.gaussian(link="identity"))
margins(x)
x <- glm(gpa ~ grade + tuce + psi, x = TRUE, data = d, family = inverse.gaussian(link="log"))
margins(x)




x <- glm(grade ~ gpa + tuce + psi, x = TRUE, data = d, family = binomial(link="logit"))
erer::maBina(w = x, x.mean = TRUE, rev.dum = FALSE)
margins(x, atmeans = TRUE)
erer::maBina(w = x, x.mean = FALSE, rev.dum = FALSE)
margins(x)

x <- glm(grade ~ gpa + tuce + psi, x = TRUE, data = d, family = binomial(link="probit"))
erer::maBina(w = x, x.mean = TRUE, rev.dum = FALSE)
margins(x, atmeans = TRUE)
erer::maBina(w = x, x.mean = FALSE, rev.dum = FALSE)
margins(x)



x <- glm(tuce ~ gpa + grade + psi, data = d, family = poisson)
margins(x)
}