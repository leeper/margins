# CHANGES TO margins 0.3.0 #

## margins 0.2.5

* Fix vignettes so package can be built with them. (#16)

## margins 0.2.4

* Modified `marginal_effects()` to use a vectorized approach to simple numerical differentiation. (#36/#37, h/t Vincent Arel-Bundock)
* Removed `margins.plm()` method, which didn't actually work because "plm" does not provide a `predict()` method.
* Updated Stata/R comparison documents included in `inst/doc`.
* Expanded tests of unit-specific variances. (#21)

## margins 0.2.3

* Added a logical argument to enable/disable calculation of unit-specific marginal effect variances and set it to FALSE by default. (#36, h/t Vincent Arel-Bundock)

## margins 0.2.2

* Removed support for "marginal effects at means" (MEMs) and the `atmeans` argument throughout package. (#35)
* Renamed the `vc` argument to `vcov` for consistency with other packages. (#34)

## margins 0.2.1

* `build_margins()` now returns columns containing unit-specific standard errors of marginal effects.
* Added a `vc` argument to `build_margins()` to allow the passing of arbitrary variance-covariance matrices. (#16, h/t Alex Coppock & Gijs Schumacher)
* `cplot()` now draws confidence intervals for "effect" plots.
* Fixed a bug in `get_marginal_effects()` wherein the `method` argument was ignored. This improves performance significantly when using `method = "simple"` (the default differentiation method).

# CHANGES TO margins 0.2.0 #

* Added `persp()` methods for "lm" and "glm" class objects to display 3-dimensional representations of predicted values and marginal effects.
* Added `plot.margins()` method for mimicking Stata's `marginsplot` behavior.
* Added `cplot()` generic and methods for "lm" and "glm" class objects to display conditional predictions and conditional marginal effects in the style of the **interplot** and **plotMElm** packages.
* Added various variance estimation procedures for marginal effects: delta method (the default), bootstrap, and simulation (ala **Clarify**).
* Fixed estimation of marginal effect variances for generalized linear models, so that they are correct on both "link" and "response" scales.
* Exposed two internal marginal effect estimation functions. First, `build_margins()` is called by `margins()` methods (perhaps repeatedly) and actually assembles a "margins" object from a model and data. It is never necessary to call this directly, but may be useful for very simple marginal effect estimation procedures (i.e., using original data with no `at` specification). Second, `marginal_effects()` is the very low level function that differentiates a model with respect to some input data (or calculate discrete changes in the outcome with respect to factor variables). This is the fastest way to obtain marginal effects without the overhead of creating a "margins" object (for which variance estimation is fairly time-consuming).
* Implemented estimation of "discrete change" representations of marginal effects of factor variables in models, ala Stata's default settings.
* Re-implemented marginal effects estimation using numeric derivatives provided by `numDeriv::grad()` rather than symbolic differentiation. This allows `margins()` to handle almost any model that can be specified in R, including models that cannot be specified in Stata.
* Used **compiler** to byte compile prediction and gradient fucntions, thereby improving estimation speed.
* The internal `build_datalist()` now checks for specification of illegal factor levels in `at` and errors when these are encountered.
* Use the **webuse** package to handle examples.

# CHANGES TO margins 0.1.0 #

* Initial package released.
