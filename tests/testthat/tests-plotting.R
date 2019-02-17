# test that plotting methods execute correctly
context("Plotting")

library(ggplot2)
data(mpg)

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

    mod_continuous <- lm(hwy ~ displ + cty * year, data = mpg)
    mod_factor <- lm(hwy ~ displ + cty * trans, data = mpg)

    vdiffr::expect_doppelganger('cplot(): continuous factor', 
        cplot(mod_continuous, x = 'year', dx = 'cty'))
    
    vdiffr::expect_doppelganger('cplot(): continuous effect', 
        cplot(mod_continuous, x = 'year', dx = 'cty', what = 'effect'))

    vdiffr::expect_doppelganger('cplot(): factor prediction',
        cplot(mod_factor, x = 'trans'))

    vdiffr::expect_doppelganger('cplot(): factor effect', 
        cplot(mod_factor, x = 'trans', what = 'effect'))

    vdiffr::expect_doppelganger('cplot(): continous styling', 
        cplot(mod_continuous, x = 'year', dx = 'cty', colour = 'pink', 
              linetype = 4, se_colour = 'green', alpha = .7))

    vdiffr::expect_doppelganger('cplot(): factor styling', 
        cplot(mod_factor, x = 'trans', shape = 4, colour = 'turquoise'))

    vdiffr::expect_doppelganger('cplot(): conf level (expect tiny confidence intervals)', 
        cplot(mod_continuous, x = 'year', dx = 'cty', level = .1, what = 'effect'))

    vc <- vcov(mod_factor) / 10
    vdiffr::expect_doppelganger('cplot(): vcov (expect tiny confidence intervals)', 
        cplot(mod_factor, x = 'trans', vcov = vc, what = 'effect'))

})

test_that("plot() lm models", {

    x <- lm(mpg ~ factor(cyl) + hp * drat, data = mtcars)
    x <- margins(x)

    vdiffr::expect_doppelganger('plot(): default', 
        plot(x))

    vdiffr::expect_doppelganger('plot(): horizontal', 
        plot(x, horizontal = TRUE))

    vdiffr::expect_doppelganger('plot(): xlab + ylab', 
        plot(x, xlab = 'test xlab', ylab = 'test ylab'))

    vdiffr::expect_doppelganger('plot(): level .999', 
        plot(x, level = .999))

    vdiffr::expect_doppelganger('plot(): pch 2', 
        plot(x, pch = 2))

    vdiffr::expect_doppelganger('plot(): points.col = green', 
        plot(x, points.col = 'green'))

    vdiffr::expect_doppelganger('plot(): lwd = 2', 
        plot(x, lwd = 2))

    vdiffr::expect_doppelganger('plot(): no zero line', 
        plot(x, zeroline = FALSE))

    vdiffr::expect_doppelganger('plot(): zero line green', 
        plot(x, zero.col = 'green'))

})
