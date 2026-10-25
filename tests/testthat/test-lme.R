context("lme models")

# ------------------------------------------------------------------------------

library(nlme)

tmp_ovary <- Ovary
tmp_ovary$x <- factor(sample(0:1, size = nrow(Ovary), replace = TRUE))

lme_fit_1 <- lme(logDens ~ Block + dilut,
    data = Assay,
    random = ~ 1 | sample
)

test_that("lme models", {
    a_1 <- list(Block = "1", dilut = "1")
    b_1 <- list(Block = "2", dilut = "1")
    c_1 <- contrast(lme_fit_1, a_1, b_1)
    expect_equivalent(
        summary(lme_fit_1)$tTable["Block.L", "p-value"],
        c_1$Pvalue
    )
    expect_equal(
        c_1$df.residual,
        min(lme_fit_1$fixDF$X[c_1$X[1, ] != 0])
    )
    expect_equal(
        drop(c_1$var),
        drop(c_1$X %*% vcov(lme_fit_1) %*% t(c_1$X))
    )
})

test_that("lme model frames reconstruct the fitted data", {
    frame <- model.frame(lme_fit_1)
    expected <- model.frame(logDens ~ Block + dilut, data = Assay)[
        order(Assay$sample),
    ]

    expect_equal(frame, expected)
})

test_that("lme model frames retain ML weighted fit data", {
    fit <- lme(
        logDens ~ Block + dilut,
        data = Assay,
        random = ~ 1 | sample,
        weights = varIdent(form = ~ 1 | Block),
        method = "ML"
    )
    frame <- model.frame(fit)
    expected <- model.frame(logDens ~ Block + dilut, data = Assay)[
        order(Assay$sample),
    ]

    expect_equal(frame, expected)
})

test_that("lme model frames preserve nested correlation grouping", {
    data <- expand.grid(time = 0:3, subject = 1:3, site = 1:4)
    data$site <- factor(data$site)
    data$subject <- interaction(data$site, data$subject)
    data$y <- 2 + data$time + as.numeric(data$site) * 0.1 +
        as.numeric(data$subject) * 0.05 + sin(seq_len(nrow(data)))

    fit <- lme(
        y ~ time,
        random = ~ 1 | site / subject,
        correlation = corAR1(form = ~ time | site / subject),
        data = data
    )
    frame <- model.frame(fit)
    expected <- model.frame(y ~ time, data = data)[
        order(data$site, data$subject),
    ]

    expect_equal(frame, expected)
})
