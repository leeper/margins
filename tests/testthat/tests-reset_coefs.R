# Test `reset_coefs()` methods on supported model classes
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

if (requireNamespace("AER", quietly = TRUE)) {
    test_that("reset_coefs() works for 'ivreg' objects", {
        data("CigarettesSW", package = "AER")
        CigarettesSW$rprice <- with(CigarettesSW, price/cpi)
        CigarettesSW$tdiff <- with(CigarettesSW, (taxs - tax)/cpi)
        # base object
        mod1 <- AER::ivreg(log(packs) ~ rprice | tdiff, data = CigarettesSW, subset = year == "1995")
        # modified object
        mod2 <- reset_coefs(mod1, c(rprice = 0.1))
        # expect coefs to have been changed
        expect_true(!isTRUE(all.equal(coef(mod1), coef(mod2))), label = "coefficients reset in 'ivreg' object")
        # expect prediction from modified object to be correct
        expect_true(!isTRUE(all.equal(predict(mod1, newdata = CigarettesSW), predict(mod2, newdata = CigarettesSW))),
                    label = "predictions differ from original 'ivreg' object")
        expect_true(isTRUE(all.equal(predict(mod2, newdata = CigarettesSW), coef(mod2)[1L] + 0.1*CigarettesSW$rprice, check.attributes = FALSE)),
                    label = "predictions correct from reset 'ivreg' object")
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
        # expect prediction from modified object to be correct
        expect_true(!isTRUE(all.equal(predict(mod1, newdata = GasolineYield), predict(mod2, newdata = GasolineYield))),
                    label = "predictions differ from original 'betareg' object")
        expect_true(isTRUE(all.equal(predict(mod2, newdata = GasolineYield, type = "link"),
                                     coef(mod2)[1L] + 0.05*GasolineYield$temp, check.attributes = FALSE)),
                    label = "predictions correct from reset 'betareg' object")
    })
}

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
        expect_true(!isTRUE(all.equal(predict(mod1, newdata = dat), predict(mod2, newdata = dat))),
                    label = "predictions differ from original 'svyglm' object")
        expect_true(isTRUE(all.equal(predict(mod2, newdata = dat)[1:20],
                                     coef(mod2)[1L] + dat$x + 2*dat$z, check.attributes = FALSE)),
                    label = "predictions correct from reset 'svyglm' object")
    })
}

if (requireNamespace("lme4")) {
    test_that("reset_coefs() works for 'lmerMod' objects", {
        data("ChickWeight", package = "datasets")
        # base object
        mod1 <- lme4::lmer(weight ~ Diet + (1|Chick), data = ChickWeight)
        # modified object
        mod2 <- reset_coefs(mod1, c(Diet2 = 2, Diet3 = 3, Diet4 = 4))
        # expect coefs to have been changed
        expect_true(!isTRUE(all.equal(lme4::fixef(mod1), lme4::fixef(mod2))), label = "coefficients reset in 'merMod' object")
        # expect prediction from modified object to be correct
        expect_true(!isTRUE(all.equal(predict(mod1, newdata = ChickWeight), predict(mod2, newdata = ChickWeight))),
                    label = "predictions differ from original 'merMod' object")
        p <- lme4::fixef(mod2)[1L] + ifelse(ChickWeight$Diet == 2, 2, ifelse(ChickWeight$Diet == 3, 3, ifelse(ChickWeight$Diet == 4, 4, 0)))
        expect_true(isTRUE(all.equal(predict(mod2, newdata = ChickWeight, re.form = NA),
                                     p, check.attributes = FALSE)),
                    label = "predictions correct from reset 'merMod' object")
    })
}

if (requireNamespace("MASS")) {
#    # "polr" objects
#    test_that("reset_coefs() works for 'polr' objects", {
#        data("housing", package = "MASS")
#        # base object
#        mod1 <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
#        # modified object
#        mod2 <- reset_coefs(mod1, c(rprice = 0.1))
#        # expect coefs to have been changed
#        expect_true(!isTRUE(all.equal(coef(mod1), coef(mod2))), label = "coefficients reset in 'polr' object")
#        # expect prediction from modified object to be correct
#        expect_true(!isTRUE(all.equal(predict(mod1, newdata = housing), predict(mod2, newdata = housing))),
#                    label = "predictions differ from original 'polr' object")
#        expect_true(isTRUE(all.equal(predict(mod2, newdata = housing), coef(mod2)[1L] + 0.1*housing$rprice, check.attributes = FALSE)),
#                    label = "predictions correct from reset 'polr' object")
#    })
}

if (requireNamespace("nnet")) {
    # TODO
#    # "multinom" objects
#    test_that("reset_coefs() works for 'multinom' objects", {
#        data("housing", package = "MASS")
#        # base object
#        mod1 <- nnet::multinom(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
#        # modified object
#        mod2 <- reset_coefs(mod1, c(Infl = 2))
#        # expect coefs to have been changed
#        expect_true(!isTRUE(all.equal(coef(mod1), coef(mod2))), label = "coefficients reset in 'multinom' object")
#        # expect prediction from modified object to be correct
#        expect_true(!isTRUE(all.equal(predict(mod1, newdata = housing), predict(mod2, newdata = housing))),
#                    label = "predictions differ from original 'multinom' object")
#        expect_true(isTRUE(all.equal(predict(mod2, newdata = housing), coef(mod2)[1L] + 3*housing$rprice, check.attributes = FALSE)),
#                    label = "predictions correct from reset 'multinom' object")
#    })
#    # "nnet" objects
#    test_that("reset_coefs() works for 'nnet' objects", {
#        data("iris3", package = "datasets")
#        ird <- data.frame(rbind(iris3[,,1], iris3[,,2], iris3[,,3]),
#                          species = factor(c(rep("s",50), rep("c", 50), rep("v", 50))))
#        # base object
#        mod1 <- nnet::nnet(species ~ ., data = ird, size = 2, rang = 0.1,
#                        decay = 5e-4, maxit = 200, trace = FALSE)
#        # modified object
#        mod2 <- reset_coefs(mod1, c(rprice = 0.1))
#        # expect coefs to have been changed
#        expect_true(!isTRUE(all.equal(coef(mod1), coef(mod2))), label = "coefficients reset in 'nnet' object")
#        # expect prediction from modified object to be correct
#        expect_true(!isTRUE(all.equal(predict(mod1, newdata = ird), predict(mod2, newdata = ird))),
#                    label = "predictions differ from original 'nnet' object")
#        expect_true(isTRUE(all.equal(predict(mod2, newdata = ird), coef(mod2)[1L] + 0.1*ird$rprice, check.attributes = FALSE)),
#                    label = "predictions correct from reset 'nnet' object")
#    })
}

if (requireNamespace("ordinal")) {
    # TODO
#    test_that("reset_coefs() works for 'clm' objects", {
#        data("wine", package = "ordinal")
#        # base object
#        mod1 <- ordinal::clm(rating ~ temp + contact, data = wine)
#        # modified object
#        mod2 <- reset_coefs(mod1, c(tempwarm = 3, contactyes = 2))
#        # expect coefs to have been changed
#        expect_true(!isTRUE(all.equal(coef(mod1), coef(mod2))), label = "coefficients reset in 'clm' object")
#        # expect prediction from modified object to be correct
#        expect_true(!isTRUE(all.equal(predict(mod1, newdata = wine), predict(mod2, newdata = wine))),
#                    label = "predictions differ from original 'clm' object")
#        expect_true(isTRUE(all.equal(predict(mod2, newdata = wine)$fit,
#                                     3*(as.integer(wine$temp)-1L) + 2*(as.integer(wine$contact)-1L), check.attributes = FALSE)),
#                    label = "predictions correct from reset 'clm' object")
#    })
}
