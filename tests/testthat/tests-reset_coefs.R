context("Test internal function for resetting coefficients")

# Simulated dataset
set.seed(1024)
N <- 20
dat <- data.frame('y' = sample(0:1, N, replace = TRUE),
                  'x' = rnorm(N),
                  'z' = rnorm(N))

# Tests
test_that("reset_coefs() works for 'lm' objects", {
    # base object
    mod1 <- lm(x ~ y + z, data = dat)
    # modified object
    mod2 <- reset_coefs(mod1, c(y = 1, z = 2))
    # expect coefs to have been changed
    expect_true(!isTRUE(all.equal(coef(mod1), coef(mod2))), label = "coefficients reset in 'lm' object")
    # expect prediction from modified object to be correct
    expect_true(!isTRUE(all.equal(predict(mod1), predict(mod2))), label = "predictions differ from original 'lm' object")
    expect_true(isTRUE(all.equal(predict(mod2), coef(mod2)[1L] + dat$y + 2*dat$z, check.attributes = FALSE)), label = "predictions correct from reset 'lm' object")
})

test_that("reset_coefs() works for 'glm' objects", {
    # base object
    mod1 <- glm(y ~ x + z, data = dat, family = binomial())
    # modified object
    mod2 <- reset_coefs(mod1, c(x = 1, z = 2))
    # expect coefs to have been changed
    expect_true(!isTRUE(all.equal(coef(mod1), coef(mod2))), label = "coefficients reset in 'glm' object")
    # expect prediction from modified object to be correct
    ## Here's an edge case!! When `predict.glm(model, se.fit = TRUE)` is called without `newdata`, `predict.lm()` isn't called.
    ## Instead `model$linear.predictors` is returned directly if `type = "link"` and
    ## `model$fitted.values` is returned directly if `type = "response"`.
    ## `marginal_effects()` for "glm" is always called with `newdata`, so we won't hit this.
    expect_true(!isTRUE(all.equal(predict(mod1, newdata = dat), predict(mod2, newdata = dat))), label = "predictions differ from original 'glm' object")
    expect_true(isTRUE(all.equal(predict(mod2, newdata = dat), coef(mod2)[1L] + dat$x + 2*dat$z, check.attributes = FALSE)), label = "predictions correct from reset 'glm' object")
})

if (requireNamespace("survey")) {
    test_that("reset_coefs() works for 'svyglm' objects", {
        design <- survey::svydesign(data = dat, id = ~ 0, weights = ~1)
        # base object
        mod1 <- survey::svyglm(y ~ x + z, design = design, family = binomial())
        # modified object
        mod2 <- reset_coefs(mod1, c(x = 1, z = 2))
        # expect coefs to have been changed
        expect_true(!isTRUE(all.equal(coef(mod1), coef(mod2))), label = "coefficients reset in 'svyglm' object")
        # expect prediction from modified object to be correct
        expect_true(!isTRUE(all.equal(predict(mod1, newdata = dat), predict(mod2, newdata = dat))), label = "predictions differ from original 'svyglm' object")
        expect_true(isTRUE(all.equal(predict(mod2, newdata = dat)[1:20], coef(mod2)[1L] + dat$x + 2*dat$z, check.attributes = FALSE)), label = "predictions correct from reset 'svyglm' object")
    })
}
if (requireNamespace("betareg")) {
    test_that("reset_coefs() works for 'betareg' objects", {
        data("GasolineYield", package = "betareg")
        # base object
        mod1 <- betareg::betareg(yield ~ temp, data = GasolineYield)
        # modified object
        mod2 <- reset_coefs(mod1, c(temp = 0.05))
        # expect coefs to have been changed
        expect_true(!isTRUE(all.equal(coef(mod1), coef(mod2))), label = "coefficients reset in 'betareg' object")
        expect_true(!isTRUE(all.equal(predict(mod1, newdata = GasolineYield), predict(mod2, newdata = GasolineYield))),
                    label = "predictions differ from original 'betareg' object")
        expect_true(isTRUE(all.equal(predict(mod2, newdata = GasolineYield, type = "link"), coef(mod2)[1L] + 0.05*GasolineYield$temp, check.attributes = FALSE)),
                    label = "predictions correct from reset 'betareg' object")
    })
}
