## margins 0.3.28

* CRAN issues only (documentation, vignette support)

## margins 0.3.27

* version bump and resubmission to recover from `prediction` archive cascade
* remove unconditional use of `gapminder` in vignette

## margins 0.3.26

* Remove unconditional use of `cairo_pdf` in vignette, per CRAN policy.

## margins 0.3.25

* Setup a `cplot.default()` method and modified documentation of `cplot()`, `image()`, and `persp()` methods slightly. (#84, h/t Luke Sonnet)
* Improve the documentation the behavior of `cplot()` for generalized linear models, which can generate unexpected confidence intervals (albeit ones consistent with base R's behavior). (#92)
* Fix bug that caused spurious `NA`s and errors in `margins()` when `vce` was `"bootstrap"` or `"simulation"` and `variables` had a length of 1. (#112)
* Models fit using the `lme4` package can now have variance estimation via bootstrap and simulation (#105).
* Updated documentation to be more explicit about what the package does for users unfamiliar with Stata. (#119)

## margins 0.3.24

* Added new function `margins_summary()` which provides a single-function expression of `summary(margins(...))`. (#94, h/t Mike DeCrescenzo)
* Added variances of marginal effects to "polr" objects from **MASS**. (#98, @eijoac)
* Fix a bug in `persp()` related to attempting to take the mean of a factor variable. (#93, h/t Jared Knowles)

## margins 0.3.23

* Fix a small issue in `print()` and `summary()` methods related to the release of prediction 0.3.6.

## margins 0.3.22

* Expanded support for objects of class "merMod" from **lme4**, including support for variance estimation and an expanded test suite. (#56)

## margins 0.3.21

* Modified the internals of `gradient_factory()` to be more robust to an expanded set of model classes through the introduction of an internal function `reset_coefs()`. A test suite for this function has been added.

## margins 0.3.20

* Added support for objects of class "ivreg" from **AER**.
* `margins.default()` now attempts to calculate marginal effect variances in order to, by default, support additional model classes.

## margins 0.3.19

* Added support for objects of class "betareg" from **betareg**. (#90)

## margins 0.3.18

* `margins()` now returns attributes "vcov" and "jacobian" (the latter only when `vce = "delta"`), which contain the full variance-covariance matrix for the average marginal effects and jacobian for the same. This is different behavior from the previous draft (v0.3.17) because the attributes now always contain a single matrix; again use the `vcov()` method rather than accessing the attribute directly lest it change in the future. This allows calculation combination of marginal effects, such as the difference between two AMEs. Some internal functions have been renamed and code reorganized to make this possible. (#87, h/t Trenton Mize)
* The "at" attribute returned by `margins()` now contains the input value passed to the `at` argument to the function. New attribute "at_vars" returns a character vector of variables specified therein.
* The data frame returned by `margins()` now contains an added column `"_at_number"`, which specifies which `at` combination a row comes from. This may be changed or removed in the future, but is useful for matching subsets of the data frame to corresponding entries in the "vcov" and "jacobian" matrices.

## margins 0.3.17

* `margins()` now returns an attribute ("vcov") containing the variance-covariance matrix for the average marginal effects and a new `vcov.margins()` method is provided for extracting it. Behavior when using `at` specifications is unspecified and may change in the future. (#87, h/t Trenton Mize)
* Updated examples in `README.Rmd`. (#83)

## margins 0.3.16

* Fixed a bug in `cplot()` when `xvar` was of class "ordered". (#77, h/t Francisco Llaneras)
* Fixed a bug in `plot.margins()` when `at` contained only one variable. (#78, h/t @cyberbryce)

## margins 0.3.15

* Tried to improve the handling of edge case model specifications like `y ~ I(x^2)`, `y ~ x + I(2*x)`, and those involving RHS interactions between factors where some cells are not observed in the data. Added a test suite to cover these cases. (#82)
* Continued to update behavior of internal function `find_terms_in_model()`.

## margins 0.3.14

* Fixed a bug in survey-weighted objects involving weights and expanded the test suite to cover these cases.

## margins 0.3.13

* Fixed a bug in all functions (ultimately in internal utility `clean_terms()`) that occurred when formulae contained variables with backticked names that contained spaces. (#80)

## margins 0.3.12

* `dydx()` now uses the performance-enhancing `prediction::prediction(..., calculate_se = FALSE)` setting, where possible (introduced in **prediction** 0.2.4)
* `data.table::rbindlist()` is used instead of `base::rbind()` inside `dydx()`.

## margins 0.3.11

* Changed some internal representations from data frames to matrices in an effort to improve performance. `marginal_effects()` and `dydx()` gain an `as.data.frame` argument to regulate the class of their responses.
* Internal calls to `prediction::prediction()` were halved by stacking data frames used in calculating numerical derivatives (inside `dydx()` methods) and then splitting the resulting predicted value vectors.

## margins 0.3.10

* Added an (internal use only) argument, `varslist`, to `marginal_effects()` and several internal functions that significantly improves performance. The performance gain is due to computational cost of identifying terms in model formulae each time `marginal_effects()` was called, which occurred repeatedly (e.g., during variance estimation). By performing this once at the `margins()`-level and passing the argument throughout, `margins()` is perhaps twice as fast as in versions <= 0.3.9. But, importantly, note that this argument should not be specified by end users!
* Some internal edits were made to the formula-processing functions `find_terms_in_model()` and `clean_terms()`, removing many regex calls with the goal of improving performance.
* Removed **compiler** dependency, which appeared to not improve performance.

## margins 0.3.9

* Fixed a bug wherein model formulae involving non-standard variables names with spaces in them led to errors. (#80)

## margins 0.3.8

* Added method for "svyglm" from **survey**.
* Improved handling of survey-weighted estimates. Removed weight-related warnings from `margins()` for unweighted models.
* `print()` and `summary()` now handle survey-weighted marginal effects.

## margins 0.3.7

* `margins()` and `marginal_effects()` gain a `variables` argument to request marginal effects for a subset of variables included in a model. (#65, h/t Vincent Arul-Bundock)

## margins 0.3.6

* Export `margins.merMod()`. (#56)

## margins 0.3.5

* Added a `cplot.clm()` method. (#63, h/t David Barron)

## margins 0.3.4

* Fixed a bug in `cplot.polr()`. (#62, h/t David Barron)

## margins 0.3.3

* Fixed "margins" object structure in `margins.merMod()`.
* Switched `print()` and `summary()` methods to using `weighted.mean()` instead of `mean()`. (#45)

## margins 0.3.2

* Added method for class "polr" from **MASS**. (#60)

## margins 0.3.1

* Added method for class "nnet" from **nnet** as an initial implementation of multi-category outcome models. (#60)

## margins 0.3.0

* Significantly modified the data structure returned by `margins()`. It now returns a data frame with an added `at` attribute, specifying the names of the variables that have been fixed by `build_datalist()`. (#58)
* Renamed marginal effects, variance, and standard error columns returned by `margins()`. Marginal effects columns are prefixed by `dydx_`. Variances of the *average* marginal effect are stored (repeatedly, across observations) in new `Var_dydx_` columns. Unit-specific standard errors, if requested, are stored as `SE_dydx_` columns. (#58)
* `summary.margins()` now returns a single data frame of marginal effect estimates. Column names have also changed to avoid use of special characters (thus making it easier to use column names in plotting with, for example, ggplot2). Row-order can be controlled by the `by_factor` attribute, which by default sorts the data frame by the factor/term. If set to `by_factor = FALSE`, the data frame is sorted by the `at` variables. This behavior cascades into the `print.summary.margins()` method. (#58)
* `print.margins()` now presents (but does not return) effect estimates as a condensed data frame with some auxiliary information. Its behavior when using `at` is improved and tidied. (#58)
* `build_margins()` is no longer exported. Arguments used to control its behavior have been exposed in `margins()` methods.
* `plot.margins()` now displays marginal effects across each level of `at`. (#58)
* `build_margins()` and thus `margins()` no longer returns the original data twice (a bug introduced by change in behavior of `prediction()`). (#57)
* All methods for objects of class `"marginslist"` have been removed. (#58)
* The `at` argument in `plot.margins()` has been renamed to `pos`, to avoid ambiguity with `at` as used elsewhere in the package.
* `persp()` and `image()` methods gain a `dx` argument (akin to that in `cplot()`) to allow visualization of marginal effects of a variable across levels of two other variables. The default behavior remains unchanged.
* Cleaned up documentation and add some examples.

## margins 0.2.26

* Added support for `"merMod"` models from **lme4**, though no variance estimation is currently supported.
* Imported `prediction::mean_or_mode()` for use in `cplot()` methods.

## margins 0.2.25

* `cplot.polr()` now allows the display of "stacked" (cumulative) predicted probabilities. (#49)
* Added an example of `cplot(draw = "add")` to display predicted probabilities across a third factor variable. (#46)
* Moved the `build_datalist()` and `seq_range()` functions to the **prediction** package.
* A tentative `cplot.multinom()` method has been added.

## margins 0.2.24

* The internal code of `cplot.lm()` has been refactored so that the actual plotting code now relies in non-exported utility functions, which can be used in other methods. This should make it easier to maintain existing methods and add new ones. (#49)
* A new `cplot()` method for objects of class `"polr"` has been added (#49).

## margins 0.2.23

* The `extract_marginal_effects()` function has been removed and replaced by `marginal_effects()` methods for objects of classes `"margins"` and `"marginslist"`.
* Added a dependency on **prediction** v.0.1.3 and, implicitly, an enhances suggestion of **survey** v3.31-5 to resolve an underlying `prediction()` issue for models of class `"svyglm"`. (#47, h/t Carl Ganz)

## margins 0.2.20

* A warning is now issued when a model uses weights, indicating that they are ignored. (#4)
* Various errors and warnings that occurred when applying `margins()` to a model with weights have been fixed.
* `cplot()` now issues an error when attempting to display the effects of a factor (with > 2 levels).

## margins 0.2.20

* Fixed a bug in `get_effect_variances(vce = "bootstrap")`, wherein the variance of the marginal effects was always zero.

## margins 0.2.20

* Factored the `prediction()` generic and methods into a separate package, **prediction**, to ease maintainence.
* Added a `print.summary.margins()` method to separate construction of the summary data frame the printing thereof.
* The "Technical Details" vignette now describes the package functionality and computational approach in near-complete detail.

## margins 0.2.19

* Plotting functions `cplot()`, `persp()`, and `image()` gain a `vcov` argumetn to pass to `build_margins(). (#43)
* `cplot()` now allows for the display of multiple conditional relationships by setting `draw = "add"`. (#32)
* The package Introduction vignette has improved examples, including ggplot2 examples using `cplot()` data. (#31)

## margins 0.2.18

* Added support in `dydx.default()` to allow the calculation of various discrete changes rather than only numerical derivatives.

## margins 0.2.17

* Fixes to handling of factors and ordered variables converted within formulae. (#38)
* Reconfigured the `data` argument in `margins()` and `prediction()` to be clearer about what is happening when it is set to missing.

## margins 0.2.16

* Switched to using a more reliable "central difference" numerical differentiation and updated the calculation of the step size to follow `marfx` (#31, h/t Jeffrey Arnold)
* Added some functionality `prediction()` methods to, hopefully, reduce memory footprint of model objects. (#26)
* Changed the capitalization of the `variances` field in "margins" objects (to lower case), for consistency.
* Fixed some small errors in documentation and improved width of examples.

## margins 0.2.15

* Expose previously internal `dydx()` generic and methods to provide variable-specific marginal effects calculations. (#31)
* Added example dataset from **marfx** package. (#31)

## margins 0.2.13

* Added support for calculating marginal effects of logical terms, treating them as factors. (#31)

## margins 0.2.12

* Added an `image()` method for "lm", "glm", and "loess" objects, as a flat complement to existing `persp()` methods. (#42)

## margins 0.2.11

* Added a `prediction()` method for "gls" objects (from `MASS::gls()`). (#3)

## margins 0.2.10

* Replaced `numDeriv::jacobian()` with an internal alternative. (#41)

## margins 0.2.8

* Added a `prediction()` method for "ivreg" objects (from `AER::ivreg()`). (#3)
* Added a `prediction()` method for "survreg" objects (from `survival::survreg()`). (#3)

## margins 0.2.7

* Added a `prediction()` method for "polr" objects (from `MASS::polr()`). (#3)
* Added a `prediction()` method for "coxph" objects (from `survival::coxph()`). (#3)

## margins 0.2.7

* `marginal_effects()` and `prediction()` are now S3 generics, with methods for "lm" and "glm" objects, improving extensability. (#39, #40)
* `prediction()` returns a new class ("prediction") and gains a `print()` method.
* Added preliminary support for "loess" objects, including methods for `prediction()`, `marginal_effects()`, `cplot()`, and `persp()`. No effect variances are currently calculated. (#3)
* Added a `prediction()` method for "nls" objects. (#3)
* Internal function `get_effect_variances()` gains a "none" option for the `vce` argument, to skip calculation of ME variances.

## margins 0.2.7

* `marginal_effects()` issues a warning (rather than fails) when trying to extract the marginal effect of a factor variable that was coerced to numeric in a model formula via `I()`. (#38)

## margins 0.2.5

* Added better support for factor `x` variables in `cplot()`.
* Added (rudimentary) tests of variance methods. (#21)
* Removed `.build_predict_fun()` factory function, as it was no longer needed.
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

## margins 0.2.0

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

## margins 0.1.0

* Initial package released.
