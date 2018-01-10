# set comparison tolerance
tol <- 0.0001

library("datasets")

context("Test 'nnet' methods")
if (requireNamespace("nnet")) {
    data("iris3", package = "datasets")
    ird <- data.frame(rbind(iris3[,,1], iris3[,,2], iris3[,,3]),
                      species = factor(c(rep("s",50), rep("c", 50), rep("v", 50))))
    m <- nnet::nnet(species ~ ., data = ird, size = 2, rang = 0.1,
                    decay = 5e-4, maxit = 200, trace = FALSE)
    test_that("Test marginal_effects() for 'nnet'", {
        expect_true(inherits(marginal_effects(m), "data.frame"))
        expect_true(inherits(marginal_effects(m, category = "c"), "data.frame"))
    })
    test_that("Test margins() for 'nnet'", {
        expect_true(inherits(margins(m), "margins"))
        expect_true(inherits(margins(m, category = "c"), "margins"))
    })
}

context("Test 'MASS' methods")
if (requireNamespace("MASS")) {
    data("housing", package = "MASS")
    m <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
    test_that("Test marginal_effects() for 'polr'", {
        expect_true(inherits(marginal_effects(m), "data.frame"))
        expect_true(inherits(marginal_effects(m, category = "Low"), "data.frame"))
    })
    test_that("Test margins() for 'polr'", {
        expect_true(inherits(margins(m), "margins"))
        expect_true(inherits(margins(m, category = "Low"), "margins"))
    })
}

context("Test 'ordinal' methods")
if (requireNamespace("ordinal")) {
    test_that("Test marignal_effects() for 'clm'", {
        data("wine", package = "ordinal")
        m <- ordinal::clm(rating ~ temp * contact, data = wine)
        expect_true(inherits(marginal_effects(m), "data.frame"))
        expect_true(inherits(marginal_effects(m, category = 1), "data.frame"))
    })
    test_that("Test margins() for 'clm'", {
        data("wine", package = "ordinal")
        m <- ordinal::clm(rating ~ temp * contact, data = wine)
        expect_true(inherits(margins(m), "margins"))
        expect_true(inherits(margins(m, category = 1), "margins"))
    })
}
