# test various edge cases
context("Edge case tests")

# set comparison tolerance
tol <- 0.0001

library("datasets")

test_that("margins() works with multicollinearity", {
    x <- lm(mpg ~ wt + I(wt*2), data = mtcars)
    suppressWarnings(m <- margins(x))
    expect_true(inherits(m, "data.frame"))
    expect_true(nrow(summary(m)) == 1L)
})

test_that("margins() works with only a term in I()", {
    x <- lm(mpg ~ I(wt*2), data = mtcars)
    suppressWarnings(m <- margins(x))
    expect_true(inherits(m, "data.frame"))
    expect_true(nrow(summary(m)) == 1L)
})

test_that("margins() works with missing factor levels", {
    mtcars2 <- mtcars
    mtcars2$cyl <- factor(mtcars2$cyl)
    mtcars2$gear <- factor(mtcars2$gear)
    x <- lm(mpg ~ cyl * gear, data = mtcars2)
    suppressWarnings(m <- margins(x))
    expect_true(inherits(m, "data.frame"))
    expect_true(nrow(summary(m)) == 4L)
})

test_that("margins() works with factor levels present in model but not in data", {
    mtcars2 <- mtcars
    mtcars2$cyl <- factor(mtcars2$cyl)
    x <- lm(mpg ~ cyl + gear, data = mtcars2)
    suppressWarnings(m <- margins(x, data = mtcars2[mtcars2$cyl == '8',]))
    expect_true(inherits(m, "data.frame"))
    expect_true(nrow(summary(m)) == 3L)
    expect_true(all(c("dydx_cyl6", "dydx_cyl8") %in% names(m)))
})

test_that("margins() works with factor levels not present in model or data", {
    mtcars2 <- mtcars
    mtcars2$cyl <- factor(mtcars2$cyl, levels = c("4", "6", "8", "10"))
    x <- lm(mpg ~ cyl + gear, data = mtcars2)
    suppressWarnings(m <- margins(x))
    expect_true(inherits(m, "data.frame"))
    expect_true(nrow(summary(m)) == 3L)
    expect_true(all(c("dydx_cyl6", "dydx_cyl8") %in% names(m)))
})

test_that("margins() works with factor levels present in data but not in model", {
    mtcars2 <- mtcars
    mtcars2$cyl <- factor(mtcars2$cyl)
    x <- lm(mpg ~ cyl + gear, data = mtcars2[mtcars2$cyl %in% c('6', '8'),])
    suppressWarnings(m <- margins(x, data = mtcars2))
    expect_true(inherits(m, "data.frame"))
    expect_true(nrow(summary(m)) == 2L)
    expect_true(all(c("dydx_cyl8") %in% names(m)))
})

test_that("margins() works with factor variable with reordered levels", {
    mtcars2 <- mtcars
    mtcars2$cyl <- factor(mtcars2$cyl, levels = c("4", "6", "8"))
    mtcars3 <- mtcars
    mtcars3$cyl <- factor(mtcars3$cyl, levels = c("8", "6", "4"))
    x <- lm(mpg ~ cyl + gear, data = mtcars2)
    suppressWarnings(m <- margins(x, data = mtcars3))
    expect_true(inherits(m, "data.frame"))
    expect_true(nrow(summary(m)) == 3L)
    expect_true(all(c("dydx_cyl6", "dydx_cyl8") %in% names(m)))
})

test_that("margins() errors correctly when there are no RHS variables", {
    x <- lm(mpg ~ 1, data = mtcars)
    expect_error(marginal_effects(x))
    expect_error(margins(x))
})

test_that("vce = 'bootstrap' works with one variable as variables argument", {
    x <- lm(mpg ~ wt + hp * cyl, data = mtcars)
    suppressWarnings(m <- margins(x, vce = "bootstrap", variables = "hp"))
    expect_true(inherits(m, "data.frame"))
    expect_true(nrow(summary(m)) == 1L)
})

test_that("vce = 'simulation' works with one variable as variables argument", {
    x <- lm(mpg ~ wt + hp * cyl, data = mtcars)
    suppressWarnings(m <- margins(x, vce = "simulation", variables = "hp"))
    expect_true(inherits(m, "data.frame"))
    expect_true(nrow(summary(m)) == 1L)
})
