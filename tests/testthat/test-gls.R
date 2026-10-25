context("gls models")

# ------------------------------------------------------------------------------

library(nlme)

tmp_ovary <- Ovary
tmp_ovary$x <- factor(sample(0:1, size = nrow(Ovary), replace = TRUE))

gls_fit_1 <- gls(logDens ~ Block + dilut,
    data = Assay,
    correlation = corCompSymm(form = ~ 1 | sample)
)

test_that("gls models", {
    a_1 <- list(Block = "1", dilut = "1")
    b_1 <- list(Block = "2", dilut = "1")
    c_1 <- contrast(gls_fit_1, a_1, b_1)
    expect_equivalent(
        summary(gls_fit_1)$tTable["Block.L", "p-value"],
        c_1$Pvalue
    )
    expect_equal(c_1$df.residual, gls_fit_1$dims$N - gls_fit_1$dims$p)
    expect_equal(
        drop(c_1$var),
        drop(c_1$X %*% vcov(gls_fit_1) %*% t(c_1$X))
    )
})

test_that("gls formulas can use a supplied environment", {
    env <- new.env(parent = globalenv())
    expect_identical(
        environment(contrast:::modelFormula(gls_fit_1, env = env)),
        env
    )
    expect_equal(
        deparse(contrast:::formula.gls(gls_fit_1)),
        "logDens ~ Block + dilut"
    )
})

test_that("gls model frames reconstruct the fitted data", {
    frame <- model.frame(gls_fit_1)
    expected <- model.frame(logDens ~ Block + dilut, data = Assay)[
        order(Assay$sample),
    ]

    expect_equal(frame, expected)
})

test_that("gls model frames honor the fitted subset", {
    fit <- gls(
        logDens ~ Block + dilut,
        data = Assay,
        correlation = corCompSymm(form = ~ 1 | sample),
        subset = sample %in% levels(sample)[1:2]
    )
    frame <- model.frame(fit)
    expected <- model.frame(
        logDens ~ Block + dilut,
        data = Assay,
        subset = sample %in% levels(sample)[1:2]
    )
    expected <- expected[order(Assay$sample[as.integer(rownames(expected))]), ]

    expect_equal(frame, expected)
})
