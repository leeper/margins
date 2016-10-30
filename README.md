# margins #

**margins** is an effort to port Stata's (closed source) [`margins`](http://www.stata.com/help.cgi?margins) command to R as an S3 generic method for calculating the marginal effects (or "partial effects") of covariates included in model objects (like those of classes "lm" and "glm"). The package implements several useful features including:

 - A type-safe wrapper around `predict()` called `prediction()` that always returns a tidy data frame.
 - A `plot()` method for the new "margins" class that ports Stata's `marginsplot` command. 
 - A `persp()` method for "lm", "glm", and "loess" objects to provide three-dimensional representations of response surfaces.
 - An `image()` method for the same that produces flat, two-dimensional heatmap-style representations of response surfaces.
 - A plotting function `cplot()` to provide the commonly needed visual summaries of predictions or marginal effects conditional on a second variable.

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

Stata's `margins` command is incredibly robust. It works with nearly any kind of statistical model and estimation procedure, including OLS, generalized linear models, panel regression models, and so forth. It also represents a significant improvement over Stata's previous marginal effects command - `mfx` - which was subject to various well-known bugs. While other Stata modules have provided functionality for deriving quantities of interest from regression estimates (e.g., [Clarify](http://gking.harvard.edu/clarify)), none has done so with the simplicity and genearlity of `margins`.

By comparison, R has no robust functionality in the base tools for drawing out marginal effects from model estimates (though the S3 `predict()` methods implement some of the functionality for computing fitted/predicted values). Nor do any add-on packages implement appropriate marginal effect estimates. Notably, several packages provide estimates of marginal effects for different types of models. Among these are [car](https://cran.r-project.org/package=car), [alr3](https://cran.r-project.org/package=alr3), [mfx](https://cran.r-project.org/package=mfx), [erer](https://cran.r-project.org/package=erer), among others. Unfortunately, none of these packages implement marginal effects correctly (i.e., correctly account for interrelated variables such as interaction terms (e.g., `a:b`) or power terms (e.g., `I(a^2)`) and the packages all implement quite different interfaces for different types of models. [interplot](https://cran.r-project.org/package=interplot) and [plotMElm](https://cran.r-project.org/package=plotMElm) provide functionality simply for plotting quantities of interest from multiplicative interaction terms in models but do not appear to support general marginal effects displays (in either tabular or graphical form), while [visreg](https://cran.r-project.org/package=visreg) provides a more general plotting function but no tabular output. [interactionTest](https://cran.r-project.org/package=interactionTest) provides some additional useful functionality for controlling the false discovery rate when making such plots and interpretations, but is again not a general tool for marginal effect estimation.

Given the challenges of interpreting the contribution of a given regressor in any model that includes quadratic terms, multiplicative interactions, a non-linear transformation, or other complexities, there is a clear need for a simple, consistent way to estimate marginal effects for popular statistical models. This package aims to correctly calculate marginal effects that include complex terms and provide a uniform interface for doing those calculations. Thus, the package implements a single S3 generic method (`margins()`) that can be easily generalized for any type of model implemented in R.

Some technical details of the package are worth briefly noting. The estimation of marginal effects relies on numerical approximations of derivatives produced using `predict()` (actually, a wrapper around `predict()` called `prediction()` that is type-safe). Variance estimation, by default is provided using the delta method a numerical approximation of [the Jacobian matrix](https://en.wikipedia.org/wiki/Jacobian_matrix_and_determinant). While symbolic differentiation of some models (e.g., basic linear models) is possible using `D()` and `deriv()`, R's modelling language (the "formula" class) is sufficiently general to enable the construction of model formulae that contain terms that fall outside of R's symbolic differentiation rule table (e.g., `y ~ factor(x)` or `y ~ I(FUN(x))` for any arbitrary `FUN()`). By relying on numeric differentiation, `margins()` supports *any* model that can be expressed in R formula syntax. Even Stata's `margins` command is limited in its ability to handle variable transformations (e.g., including `x` and `log(x)` as predictors) and quadratic terms (e.g., `x^3`); these scenarios are easily expressed in an R formula and easily handled, correctly, by `margins()`.

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

```
## 
```

```r
summary(m)
```

```
## [[1]]
```

```
## Average Marginal Effects
```

```
## lm(formula = mpg ~ cyl * hp + wt, data = mtcars)
```

```
##     Factor   dy/dx Std.Err. z value Pr(>|z|)   2.50%  97.50%
## cyl    cyl  0.0381   0.5999  0.0636   0.9493 -1.1376  1.2139
## hp      hp -0.0463   0.0145 -3.1909   0.0014 -0.0748 -0.0179
## wt      wt -3.1198   0.6613 -4.7176   0.0000 -4.4160 -1.8236
```

With the exception of differences in rounding, the above results match identically what Stata's `margins` command produces. Using the `plot()` method also yields an aesthetically similar result to Stata's `marginsplot`:


```r
plot(m[[1]])
```

![plot of chunk marginsplot](http://i.imgur.com/FjwAlQ9.png)

If you are only interested in obtaining the marginal effects (without corresponding variances or the overhead of creating a "margins" object), you can call `marginal_effects(x)` directly. Furthermore, the `dydx()` function enables the calculation of the marginal effect of a single named variable:


```r
# all marginal effects, as a data.frame
head(marginal_effects(x))
```

```
##          cyl          hp        wt
## 1 -0.6572244 -0.04987248 -3.119815
## 2 -0.6572244 -0.04987248 -3.119815
## 3 -0.9794364 -0.08777977 -3.119815
## 4 -0.6572244 -0.04987248 -3.119815
## 5  0.5747624 -0.01196519 -3.119815
## 6 -0.7519926 -0.04987248 -3.119815
```

```r
# marginal effect of one variable
head(dydx(mtcars, x, "hp"))
```

```
##            hp
## 1 -0.04987248
## 2 -0.04987248
## 3 -0.08777977
## 4 -0.04987248
## 5 -0.01196519
## 6 -0.04987248
```

These functions may be useful, for example, for plotting, or getting a quick impression of the results.

While there is still work to be done to improve performance, **margins** is reasonably speedy:


```r
library("microbenchmark")
microbenchmark(marginal_effects(x))
```

```
## Unit: milliseconds
##                 expr      min       lq     mean   median       uq      max neval
##  marginal_effects(x) 9.182543 10.22088 11.10373 10.84278 11.55592 19.23141   100
```

```r
microbenchmark(margins(x))
```

```
## Unit: milliseconds
##        expr      min       lq     mean   median       uq      max neval
##  margins(x) 73.43373 76.43205 79.47221 78.16413 79.79947 175.6931   100
```

In addition to the estimation procedures and `plot()` generic, **margins** offers several plotting methods for model objects. First, there is a new generic `cplot()` that displays predictions or marginal effects (from an "lm" or "glm" model) of a variable conditional across values of third variable (or itself). For example, here is a graph of predicted probabilities from a logit model:


```r
m <- glm(am ~ wt*drat, data = mtcars, family = binomial)
cplot(m, x = "wt", se.type = "shade")
```

![plot of chunk cplot1](http://i.imgur.com/oMQ1d3W.png)

And fitted values with a factor independent variable:


```r
cplot(lm(Sepal.Length ~ Species, data = iris))
```

![plot of chunk cplot2](http://i.imgur.com/YrFBtcO.png)

and a graph of the effect of `drat` across levels of `wt`:


```r
cplot(m, x = "wt", dx = "drat", what = "effect", se.type = "shade")
```

![plot of chunk cplot3](http://i.imgur.com/Jc1pRmq.png)

Second, the package implements methods for "lm" and "glm" class objects for the `persp()` generic plotting function. This enables three-dimensional representations of predicted outcomes:


```r
persp(x, xvar = "cyl", yvar = "hp")
```

![plot of chunk persp1](http://i.imgur.com/FCH7YtE.png)

and marginal effects:


```r
persp(x, xvar = "cyl", yvar = "hp", what = "effect", nx = 10)
```

![plot of chunk persp2](http://i.imgur.com/C5ITO3y.png)

And if three-dimensional plots aren't your thing, there are also analogous methods for the `image()` generic, to produce heatmap-style representations:


```r
image(x, xvar = "cyl", yvar = "hp", main = "Predicted Fuel Efficiency,\nby Cylinders and Horsepower")
```

![plot of chunk image11](http://i.imgur.com/ZWbLMh5.png)


The numerous package vignettes and help files contain extensive documentation and examples of all package functionality.

## Requirements and Installation ##

[![CRAN](http://www.r-pkg.org/badges/version/margins)](https://cran.r-project.org/package=margins)
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

