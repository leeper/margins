# General Contribution Guidance

Contributions to **margins** are welcome from anyone and are best sent as pull requests on [the GitHub repository](https://github.com/leeper/margins/). This page provides some instructions to potential contributors about how to add to the package.

 1. Contributions can be submitted as [a pull request](https://help.github.com/articles/creating-a-pull-request/) on GitHub by forking or cloning the [repo](https://github.com/leeper/margins/), making changes and submitting the pull request.
 
 2. This package follows [the cloudyr project style guide](http://cloudyr.github.io/styleguide/index.html). Please refer to this when editing package code.
 
 3. Pull requests should involve only one commit per substantive change. This means if you change multiple files (e.g., code and documentation), these changes should be committed together. If you don't know how to do this (e.g., you are making changes in the GitHub web interface) just submit anyway and the maintainer will clean things up.
 
 4. All contributions must be submitted consistent with the package license ([MIT](https://opensource.org/licenses/MIT)).
 
 5. Non-trivial contributions need to be noted in the `Authors@R` field in the [DESCRIPTION](https://github.com/leeper/margins/blob/master/DESCRIPTION). Just follow the format of the existing entries to add your name (and, optionally, email address). Substantial contributions should also be noted in [`inst/CITATION`](https://github.com/leeper/margins/blob/master/inst/CITATION).
 
 6. The package uses royxgen code and documentation markup, so changes should be made to roxygen comments in the source code `.R` files. If changes are made, roxygen needs to be run. The easiest way to do this is a command line call to: `Rscript -e devtools::document()`. Please resolve any roxygen errors before submitting a pull request. The [README.md](https://github.com/leeper/margins/blob/master/README.md) file is built from [README.Rmd](https://github.com/leeper/margins/blob/master/README.Rmd); changes should be made in both places or to [README.Rmd](https://github.com/leeper/margins/blob/master/README.Rmd) and then knitted using using `knitr::knit("README.Rmd")`.
 
 7. Please run `R CMD BUILD margins` and `R CMD CHECK margins_VERSION.tar.gz` before submitting the pull request to check for any errors.

# Contribution Ideas

Some specific types of changes that you might make are:

 1. Bug fixes. Great!
 
 2. Documentation-only changes (e.g., to Rd files, README, vignettes). This is great! All contributions are welcome.
 
 3. New functionality. This is fine, but should be discussed on [the GitHub issues page](https://github.com/leeper/margins/issues) before submitting a pull request.
 
 3. Changes requiring a new package dependency should also be discussed on [the GitHub issues page](https://github.com/leeper/margins/issues) before submitting a pull request.
 
 4. Message translations. These are very appreciated! The format is a pain, but if you're doing this I'm assuming you're already familiar with it.

Any questions you have can be opened as GitHub issues or directed to thosjleeper (at) gmail.com.

# How this Package Works

It may not be entirely obvious how **margins** actually works as a package. The basic idea is as follows:

 1. Users estimate a model, creating a model object.
 2. The `marginal_effects()` function is an S3 generic with methods for various model classes. Each method takes the user-created model and a data frame and returns a data frame of observation-specific marginal effects. The internal function `find_terms_in_model()` identifies the variables used in a model formula and attempts to classify them as numeric (or integer), factor (or character, or ordered), or logical. If users request marginal effects for only a subset of variables, this subsetting is performed within that function once all variables have been identified.
 3. The work of the `marginal_effects()` methods is mostly performed by `dydx()` methods. `dydx()` is also an S3 generic, but rather than dispatching on a model class, it dispatches on a variable class (e.g., "factor", "numeric"). Thus `dydx()` is called once for each variable used in the model, returning the observation-specific marginal effects that are returned by `marginal_effects()` as a single data frame. This is achieved through numerical differentiation via calls to model-specific `prediction()` methods provided by the **prediction** package.
 4. `margins()` is a higher-level S3 generic, which wraps the `marginal_effects()` function via a workhorse function `build_margins()`. The separation of these is meant to allow `margins()` methods to handle class-specific nuance while allowing the construction of its response object to mostly be handled by a shared workhorse function. The response from `margins()` is always a data frame of class `"margins"`, which may carry some additional attributes. Because it is a data frame, these attributes are lost during any subsequent subsetting.
 5. In addition to returning observation-specific marginal effects, `margins()` (or rather `build_margins()`) also does 2-3 additional things:
  - It calculates variances of average marginal effects via one of several optional methods. This is done in the internal `get_effect_variances()` function. If the variance-covariance estimation (vce) method is "delta", a Jacobian matrix is constructed (one row per marginal effect equation) using `gradient_factor()` (a factor function) and `jacobian()` a function to differentiate with respect to the estimated coefficients (as opposed to differentiating with respect to variables for generating marginal effects). The variances of the average marginal effects are then calculated and returned.
  - If users desire observation-specific variances of observation-specific marginal effects (`unit_ses = TRUE`, which is not the default), `build_margins()` performs the `gradient_factor() -> jacobian()` dance separately for each row of the data. It is really rare that users would be interested in this.
  - If users specify `at` to request estimation of marginal effects at counterfactual datasets, the data and `at` specification are passed to `prediction::build_datalist()` (in the **prediction** package) to generate a list of such datasets. `margins()` then loops over each of these, repeating (a-b) as necessary. The results are then stacked to produce a single data set and the `jacobian` and `vcov` matrices are recalculated.
 6. To generate summary quantities of interest, the `summary.margins()` methods averages the observation-specific marginal effects, combines these with variances thereof and produces a summary table (which it return). The basic `print.margins()` method performs the averaging but none of the other behavior.
 7. Additional functions are simply conveniences:
  - `plot.margins()` provide a simple "airplane plot" of average marginal effects and confidence intervals (akin to Stata's `marginsplot` command).
  - `confint.margins()` calculate confidence intervals for a "margins" object.
  - `vcov.margins()` extracts the variance-covariance attribute from a "margins" object.
  - The `cplot()` generic and methods provide two-dimensional plotting of predicted values and marginal effects (at means) for model objects.
  - `persp.lm()` and `image.lm()` provide three-dimensional plotting of the same. The actual underlying calculation of the prediction or effect surface is performed by the internal function `calculate_surface()`.
