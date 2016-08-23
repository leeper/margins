# margins #

**margins** is an effort to port Stata's (closed source) [`margins`](http://www.stata.com/help.cgi?margins) command to R as an S3 generic method for calculating the marginal effects (or "partial effects") of covariates included in model objects (like those of classes "lm" and "glm"). A plot method for the new "margins" class additionally ports the `marginsplot` command.

## Motivation ##

With the introduction of Stata's `margins` command, it has become incredibly simple to estimate average marginal effects (i.e., "average partial effects") and marginal effects at representative cases. Indeed, in just a few lines of Stata code, regression results for almost any kind model can be transformed into meaningful quantities of interest and related plots:

```
. import delimited mtcars.csv
. quietly reg mpg c.cyl##c.hp wt
. margins, dydx(*)
------------------------------------------------------------------------------
             |            Delta-method
             |      dy/dx   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
         cyl |   .0381376   .5998897     0.06   0.950    -1.192735     1.26901
          hp |  -.0463187    .014516    -3.19   0.004     -.076103   -.0165343
          wt |  -3.119815    .661322    -4.72   0.000    -4.476736   -1.762894
------------------------------------------------------------------------------
. marginsplot
```

![marginsplot](http://i.imgur.com/VhoaFGp.png)

Stata's `margins` is incredibly robust. It works with nearly any kind of statistical model and estimation procedure, including OLS, generalized linear models, panel regression models, and so forth. It also represents a significant improvement over Stata's previous marginal effects command - `mfx` - which was subject to various well-known bugs. While other Stata modules have provided functionality for deriving quantities of interest from regression estimates (e.g., [Clarify](http://gking.harvard.edu/clarify)), none has done so with the simplicity and genearlity of `margins`.

By comparison, R has no robust functionality in the base tools for drawing out marginal effects from model estimates (though the S3 `predict()` methods implement some of the functionality for computing fitted/predicted values). Nor do any add-on packages implement appropriate marginal effect estimates. Notably, several packages provide estimates of marginal effects for different types of models. Among these are [car](https://cran.r-project.org/package=car), [alr3](https://cran.r-project.org/package=alr3), [mfx](https://cran.r-project.org/package=mfx), [erer](https://cran.r-project.org/package=erer), among others. Unfortunately, none of these packages implement marginal effects correctly (i.e., correctly account for interrelated variables such as interaction terms (e.g., `a:b`) or power terms (e.g., `I(a^2)`) and the packages all implement quite different interfaces for different types of models. [interplot](https://cran.r-project.org/package=interplot) and [plotMElm](https://cran.r-project.org/package=plotMElm) provide functionality simply for plotting quantities of interest from multiplicative interaction terms in models but do not appear to support general marginal effects displays (in either tabular or graphical form), while [visreg](https://cran.r-project.org/package=visreg) provides a more general plotting function but no tabular output. [interactionTest](https://cran.r-project.org/package=interactionTest) provides some additional useful functionality for controlling the false discovery rate when making such plots and interpretations, but is again not a general tool for marginal effect estimation.

Given the challenges of interpreting the contribution of a given regressor in any model that includes quadratic terms, multiplicative interactions, a non-linear transformation, or other complexities, there is a clear need for a simple, consistent way to estimate marginal effects for popular statistical models. This package aims to correctly calculate marginal effects that include complex terms and provide a uniform interface for doing those calculations. Thus, the package implements a single S3 generic method (`margins()`) that can be easily generalized for any type of model implemented in R.

Some technical details of the package are worth briefly noting. The estimation of marginal effects relies on numeric derivatives produced using `predict()` and [`numDeriv::grad()`](https://cran.r-project.org/package=numDeriv). While symbolic differentiation of some models (e.g., linear models) is possible using `D()` and `deriv()`, R's modelling language (the "formula" class) is sufficiently general to enable the construction of model formulae that contain terms that fall outside of R's symbolic differentiation rule table (e.g., `y ~ factor(x)` or `y ~ I(FUN(x))` for any arbitrary `FUN()`). By relying on numeric differentiation, `margins()` supports *any* model that can be expressed in R formula syntax. Even Stata's `margins` command is limited in its ability to handle variable transformations (e.g., including `x` and `log(x)` as predictors) and quadratic terms (e.g., `x^3`); these scenarios are easily expressed in an R formula and easily handled, correctly, by `margins()`.

## Simple code examples ##



Replicating Stata's results is incredibly simple using just the `margins()` method to obtain average marginal effects:


```r
library("margins")
x <- lm(mpg ~ cyl * hp + wt, data = mtcars)
(m <- margins(x))
```

```
##      cyl       hp       wt 
##  0.03814 -0.04632 -3.11981
```

```r
summary(m)
```

```
## Average Marginal Effects
## lm(formula = mpg ~ cyl * hp + wt, data = mtcars) 
## 
##  Factor   dy/dx Std.Err. z value Pr(>|z|)   2.50%  97.50%
##     cyl  0.0381   0.5999  0.0636   0.9493 -1.1376  1.2139
##      hp -0.0463   0.0145 -3.1909   0.0014 -0.0748 -0.0179
##      wt -3.1198   0.6613 -4.7175   0.0000 -4.4160 -1.8236
```

With the exception of differences in rounding, the above results match identically what Stata's `margins` command produces. Using the `plot()` method also yields an aesthetically similar result to Stata's `marginsplot`:


```r
plot(m[[1]])
```

![plot of chunk marginsplot](http://i.imgur.com/MElh69l.png)

If you are only interested in obtaining the marginal effects (without corresponding variances or the overhead of creating a "margins" object), you can call `marginal_effects(x)` directly. This may be useful, for example, for plotting, or getting a quick impression of the results.

While there is still work to be done to improve performance, **margins** is reasonably speedy:


```r
library("microbenchmark")
microbenchmark(marginal_effects(x))
```

```
## Unit: milliseconds
##                 expr      min       lq     mean   median       uq      max neval
##  marginal_effects(x) 7.942455 8.409444 9.199376 9.115538 9.792552 13.37642   100
```

```r
microbenchmark(margins(x))
```

```
## Unit: milliseconds
##        expr      min       lq     mean   median      uq      max neval
##  margins(x) 63.40545 69.59018 74.86074 72.59378 75.9127 169.0655   100
```

In addition to the estimation procedures and `plot()` generic, **margins** offers several plotting methods for model objects. First, there is a new generic `cplot()` that displays predictions or marginal effects (from an "lm" or "glm" model) of a variable conditional across values of third variable (or itself). For example, here is a graph of predicted probabilities from a logit model:


```r
m <- glm(am ~ wt*drat, data = mtcars, family = binomial)
cplot(m, x = "wt", se.type = "shade")
```

![plot of chunk cplot1](http://i.imgur.com/z8ILE3c.png)

and a graph of the effect of `wt` across levels of `drat`:


```r
cplot(m, x = "drat", dx = "wt", what = "effect", type = "response")
```

![plot of chunk cplot2](http://i.imgur.com/6C6MfBT.png)

Second, the package implements methods for "lm" and "glm" class objects for the `persp()` generic plotting function. This enables three-dimensional representations of predicted outcomes:


```r
persp(x, xvar = "cyl", yvar = "hp")
```

![plot of chunk persp1](http://i.imgur.com/I9BU3Kq.png)

and marginal effects:


```r
persp(x, xvar = "cyl", yvar = "hp", what = "effect", nx = 10)
```

![plot of chunk persp2](http://i.imgur.com/G9XOzXp.png)


The numerous package vignettes and help files contain extensive documentation and examples of all package functionality.

## Requirements and Installation ##

[![CRAN](http://www.r-pkg.org/badges/version/slopegraph)](http://cran.r-project.org/web/packages/margins/index.html)
[![Build Status](https://travis-ci.org/leeper/margins.svg?branch=master)](https://travis-ci.org/leeper/margins)
[![Build status](https://ci.appveyor.com/api/projects/status/t6nxndmvvcw3gw7f/branch/master?svg=true)](https://ci.appveyor.com/project/leeper/margins/branch/master)
[![codecov.io](http://codecov.io/github/leeper/margins/coverage.svg?branch=master)](http://codecov.io/github/leeper/margins?branch=master)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)

The development version of this package can be installed directly from GitHub using `ghit`:

```R
if (!require("ghit")) {
    install.packages("ghit")
    library("ghit")
}
install_github("leeper/margins")

# building vignettes takes a moment, so for a quicker install set:
install_github("leeper/margins", build_vignettes = FALSE)
```

