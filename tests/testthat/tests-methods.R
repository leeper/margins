# Test `marginal_effects()` and `margins()` methods on supported model classes

# set comparison tolerance
tol <- 0.0001

library("datasets")

context("Test 'AER' methods")
if (require("AER", quietly = TRUE)) {
    data("CigarettesSW", package = "AER")
    CigarettesSW$rprice <- with(CigarettesSW, price/cpi)
    CigarettesSW$rincome <- with(CigarettesSW, income/population/cpi)
    CigarettesSW$tdiff <- with(CigarettesSW, (taxs - tax)/cpi)
    m <- AER::ivreg(log(packs) ~ log(rprice) + log(rincome) | log(rincome) + tdiff, data = CigarettesSW, subset = year == "1995")
    test_that("Test margins() for 'ivreg'", {
        expect_true(inherits(marginal_effects(m), "data.frame"))
        expect_true(inherits(margins(m), "margins"))
    })
}

context("Test 'betareg' methods")
if (requireNamespace("betareg")) {
    data("GasolineYield", package = "betareg")
    m <- betareg::betareg(yield ~ batch + temp, data = GasolineYield)
    test_that("Test marginal_effects() for 'betareg'", {
        expect_true(inherits(marginal_effects(m), "data.frame"))
    })
    test_that("Test margins() for 'betareg'", {
        expect_true(inherits(margins(m), "margins"))
    })
}

context("Test 'lme4' methods")
if (requireNamespace("lme4")) {
    data("ChickWeight", package = "datasets")

    # linear mixed effects models (random intercepts)
    m <- lme4::lmer(weight ~ Diet + (1|Chick), data = ChickWeight)
    test_that("Test marginal_effects() for 'lmerMod' (single grouping)", {
        expect_true(inherits(marginal_effects(m), "data.frame"))
        expect_true(inherits(marginal_effects(m), "data.frame"))
    })
    test_that("Test margins() for 'lmerMod' (single grouping)", {
        expect_true(inherits(margins(m), "margins"))
        expect_true(inherits(margins(m), "margins"))
    })
    test_that("Test margins(vce = 'simulation') for 'lmerMod' (single grouping)", {
        expect_true(inherits(margins(m, vce = "simulation", iterations = 5),
                             "margins"))
    })
    test_that("Test margins(vce = 'bootstrap') for 'lmerMod' (single grouping)", {
        expect_true(inherits(suppressWarnings({
            margins(m, vce = "bootstrap", iterations = 5)
        }), "margins"))
    })
    # lmer with multiple random intercepts
    m <- lme4::lmer(weight ~ Diet + (1|Time) + (1|Chick), data = ChickWeight)
    test_that("Test marginal_effects() for 'lmerMod' (multiple grouping)", {
        expect_true(inherits(marginal_effects(m), "data.frame"))
        expect_true(inherits(marginal_effects(m), "data.frame"))
    })
    test_that("Test margins() for 'lmerMod' (multiple grouping)", {
        expect_true(inherits(margins(m), "margins"))
        expect_true(inherits(margins(m), "margins"))
    })
    test_that("Test margins(vce = 'simulation') for 'lmerMod' (multiple grouping)", {
        expect_true(inherits(margins(m, vce = "simulation", iterations = 5),
                             "margins"))
    })
    test_that("Test margins(vce = 'bootstrap') for 'lmerMod' (multiple grouping)", {
        expect_true(inherits(suppressWarnings({
            margins(m, vce = "bootstrap", iterations = 5)
        }), "margins"))
    })

    # lmer with random slopes
    m <- lme4::lmer(weight ~ Diet + (1+Time|Chick), data = ChickWeight)
    test_that("Test marginal_effects() for 'lmerMod' (random slopes)", {
        expect_true(inherits(marginal_effects(m), "data.frame"))
        expect_true(inherits(marginal_effects(m), "data.frame"))
    })
    test_that("Test margins() for 'lmerMod' (random slopes)", {
        expect_true(inherits(margins(m), "margins"))
        expect_true(inherits(margins(m), "margins"))
    })
    test_that("Test margins(vce = 'simulation') for 'lmerMod' (random slopes)", {
        expect_true(inherits(margins(m, vce = "simulation", iterations = 5),
                             "margins"))
    })
    test_that("Test margins(vce = 'bootstrap') for 'lmerMod' (random slopes)", {
        expect_true(inherits(suppressWarnings({
            margins(m, vce = "bootstrap", iterations = 5)
        }), "margins"))
    })

    # generalized linear mixed effects models
    ChickWeight$high <- cut(ChickWeight$weight, 2)
    m <- lme4::glmer(high ~ Diet + (1|Chick), data = ChickWeight, binomial)
    test_that("Test marginal_effects() for 'merMod'", {
        expect_true(inherits(marginal_effects(m), "data.frame"))
        expect_true(inherits(marginal_effects(m), "data.frame"))
    })
    test_that("Test margins() for 'merMod'", {
        expect_true(inherits(margins(m), "margins"))
        expect_true(inherits(margins(m), "margins"))
    })
    test_that("Test margins(vce = 'simulation') for 'merMod'", {
        expect_true(inherits(margins(m, vce = "simulation", iterations = 5),
                             "margins"))
    })
    test_that("Test margins(vce = 'bootstrap') for 'merMod'", {
        expect_true(inherits(suppressWarnings({
            margins(m, vce = "bootstrap", iterations = 5)
        }), "margins"))
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

context("Test 'nnet' methods")
if (requireNamespace("nnet")) {
    # "nnet" objects
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

    # "multinom" objects
    data("housing", package = "MASS")
    m <- nnet::multinom(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
    test_that("Test marginal_effects() for 'polr'", {
        expect_true(inherits(marginal_effects(m), "data.frame"))
        expect_true(inherits(marginal_effects(m, category = "Low"), "data.frame"))
    })
#    test_that("Test margins() for 'polr'", {
#        expect_true(inherits(margins(m), "margins"))
#        expect_true(inherits(margins(m, category = "Low"), "margins"))
#    })
}

context("Test 'ordinal' methods")
if (requireNamespace("ordinal")) {
    test_that("Test marginal_effects() for 'clm'", {
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

test_that("Test 'svyglm' methods", {
    if (requireNamespace("survey")) {
        data("fpc", package = "survey")
        fpc$nh_f = factor(fpc$nh)
        svyd <- survey::svydesign(weights=~weight, ids=~psuid, strata=~stratid, fpc=~Nh, variables=~x + nh + nh_f, data=fpc, nest=TRUE)
        svrepd <- survey::as.svrepdesign(svyd)
        svrepd <- survey::svrepdesign(survey::make.formula(colnames(svrepd)), weights(svrepd), weights(svrepd, type="sampling"), data=fpc, combined.weights=FALSE)
        x <- survey::svyglm(x ~ nh, design = svyd)
        xf <- survey::svyglm(x ~ nh_f, design = svyd)
        xrep <- survey::svyglm(x ~ nh, design = svrepd)
        test_that("Test margins() for 'svyglm' without passing 'data'", {
            m1 <- margins(x)
            expect_true(inherits(print(m1), "margins"), label = "print() method for margins from svyglm")
            expect_true(inherits(summary(m1), "data.frame"), label = "summary() method for margins from svyglm")
            expect_true(inherits(print(summary(m1)), "data.frame"), label = "print() method for summary.margins from svyglm")
        })
        test_that("Test margins() for 'svyglm' with a factor predictor (without passing 'data')", {
            m1f <- margins(xf)
            expect_true(inherits(print(m1f), "margins"), label = "print() method for margins from svyglm")
            expect_true(inherits(summary(m1f), "data.frame"), label = "summary() method for margins from svyglm")
            expect_true(inherits(print(summary(m1f)), "data.frame"), label = "print() method for summary.margins from svyglm")
        })
        test_that("Test margins() for 'svyglm' estimated on replicate weights design without passing 'data'", {
            m1rep <- margins(xrep)
            expect_true(inherits(print(m1rep), "margins"), label = "print() method for margins from svyglm")
            expect_true(inherits(summary(m1rep), "data.frame"), label = "summary() method for margins from svyglm")
            expect_true(inherits(print(summary(m1rep)), "data.frame"), label = "print() method for summary.margins from svyglm")
            expect_equal(summary(m1rep)[, c("factor", "AME")],
                         summary(m1)[, c("factor", "AME")], label = "same point estimates using survey.design and svyrep.design objects")
        })
        test_that("Test margins() for 'svyglm' with 'data'", {
            m2 <- margins(x, data = svyd)
            expect_true(inherits(print(m2), "margins"), label = "print() method for margins from svyglm (with 'design')")
            expect_true(inherits(summary(m2), "data.frame"), label = "summary() method for margins from svyglm (with 'design')")
            expect_true(inherits(print(summary(m2)), "data.frame"), label = "print() method for summary.margins from svyglm (with 'design')")
        })
        test_that("Test margins() for 'svyglm' with 'data'", {
            m2rep <- margins(x, data = svrepd)
            expect_true(inherits(print(m2rep), "margins"), label = "print() method for margins from svyglm (with 'design')")
            expect_true(inherits(summary(m2rep), "data.frame"), label = "summary() method for margins from svyglm (with 'design')")
            expect_true(inherits(print(summary(m2rep)), "data.frame"), label = "print() method for summary.margins from svyglm (with 'design')")
            expect_equal(summary(m2rep)[, c("factor", "AME")],
                         summary(m2)[, c("factor", "AME")], label = "same point estimates using survey.design and svyrep.design objects")
        })
        test_that("Test margins() for 'svyglm' with 'data' object and 'at' specification", {
            m3 <- margins(x, at = list(nh = mean(fpc$nh)), data = svyd)
            expect_true(inherits(print(m3), "margins"), label = "print() method for margins from svyglm (with 'at')")
            expect_true(inherits(summary(m3), "data.frame"), label = "summary() method for margins from svyglm (with 'at')")
            expect_true(inherits(print(summary(m3)), "data.frame"), label = "print() method for summary.margins from svyglm (with 'at')")
        })
        test_that("Test margins() for 'svyglm' estimated on replicate weights design with 'data' object and 'at' specification", {
            m3rep <- margins(xrep, at = list(nh = mean(fpc$nh)), data = svyd)
            expect_true(inherits(print(m3rep), "margins"), label = "print() method for margins from svyglm (with 'at')")
            expect_true(inherits(summary(m3rep), "data.frame"), label = "summary() method for margins from svyglm (with 'at')")
            expect_true(inherits(print(summary(m3rep)), "data.frame"), label = "print() method for summary.margins from svyglm (with 'at')")
        })
        test_that("Test margins() for 'svyglm' with 'data' object and 'at' specification for a factor predictor", {
            m3f <- margins(xf, at = list(nh_f = levels(fpc$nh_f)[2]), data = svyd)
            expect_true(inherits(print(m3f), "margins"), label = "print() method for margins from svyglm (with 'at')")
            expect_true(inherits(summary(m3f), "data.frame"), label = "summary() method for margins from svyglm (with 'at')")
            expect_true(inherits(print(summary(m3f)), "data.frame"), label = "print() method for summary.margins from svyglm (with 'at')")
        })
        rm(fpc, svyd, svrepd, x, xf, xrep)
    }
})
