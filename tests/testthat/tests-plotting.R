# test that plotting methods execute correctly
context("Plotting")

library(ggplot2)
library(margins)
data(mpg)
data(mtcars)

test_that("persp() method for 'lm' works", {
    x <- lm(mpg ~ wt * hp, data = mtcars)
    expect_true(is.list(persp(x)))
    expect_true(is.list(persp(x, theta = c(30, 60))))
    expect_true(is.list(persp(x, theta = c(30, 60), phi = c(0, 10))))
    expect_true(is.list(persp(x, phi = c(0, 10))))
    expect_true(is.list(persp(x, what = "effect")))
})

test_that("image() method for 'lm' works", {
    x <- lm(mpg ~ wt * hp, data = mtcars)
    expect_true(is.null(image(x)))
    expect_true(is.null(image(x, what = "effect")))
})

test_that("cplot() lm models", {

    mtcars$cyl <- as.factor(mtcars$cyl)
    mod_continuous <- lm(hp ~ wt*drat, data = mtcars)
    mod_factor <- lm(hp ~ cyl*drat, data = mtcars)

    vdiffr::expect_doppelganger('cplot(): continuous prediction', 
        cplot(mod_continuous, x = 'wt', dx = 'drat'))
    
    vdiffr::expect_doppelganger('cplot(): continuous effect', 
        cplot(mod_continuous, x = 'wt', dx = 'drat', what = 'effect'))

    vdiffr::expect_doppelganger('cplot(): factor prediction',
        cplot(mod_factor, x = 'cyl'))

    vdiffr::expect_doppelganger('cplot(): continous styling', 
        cplot(mod_continuous, x = 'wt', dx = 'drat', colour = 'pink', fill='turquoise', alpha=1, linetype = 4, size=4, shape=3))

    vdiffr::expect_doppelganger('cplot(): factor styling', 
        cplot(mod_factor, x = 'cyl', shape = 4, colour = 'turquoise'))

    vdiffr::expect_doppelganger('cplot(): level + prediction (expect tiny confidence intervals)', 
        cplot(mod_continuous, x = 'wt', dx = 'drat', level = .2, what = 'prediction'))

    vdiffr::expect_doppelganger('cplot(): level + effect (expect tiny confidence intervals)', 
        cplot(mod_continuous, x = 'wt', dx = 'drat', level = .1, what = 'effect'))

	vc <- vcov(mod_continuous) / 100
    vdiffr::expect_doppelganger('cplot(): vcov + effect (expect tiny confidence intervals)', 
        cplot(mod_continuous, x = 'wt', dx = 'drat', vcov = vc, what = 'effect'))

	# TODO: this doesn't work yet. vincent thinks this is an upstream prediction-related issue
	#vc <- vcov(mod_continuous) / 100
    #vdiffr::expect_doppelganger('cplot(): vcov + prediction (expect tiny confidence intervals)', 
        #cplot(mod_continuous, x = 'wt', dx = 'drat', vcov = vc, what = 'prediction'))

    #vc <- vcov(mod_continuous) / 100
    #vdiffr::expect_doppelganger('cplot(): vcov + prediction (expect tiny confidence intervals)', 
        #cplot(mod_continuous, x = 'wt', dx='drat', vcov = vc, what = 'prediction'))

})

test_that("cplot() glm models", {

    mod <- glm(am ~ wt*drat, data = mtcars, family = binomial)

    vdiffr::expect_doppelganger('cplot(): logit', 
        cplot(mod, x = 'wt', dx = 'drat'))

})


test_that("plot() lm models", {

    x <- lm(mpg ~ factor(cyl) + hp * drat, data = mtcars)
    x <- margins(x)

    vdiffr::expect_doppelganger('plot(): default', 
        plot(x))

    vdiffr::expect_doppelganger('plot(): level large conf.int', 
        plot(x, level = .999))

    vdiffr::expect_doppelganger('plot(): level tiny conf.int', 
        plot(x, level = .1))

    vdiffr::expect_doppelganger('plot(): styling', 
        plot(x, size=3, colour = 'turquoise', linetype = 'dashed', level = .999))

})
