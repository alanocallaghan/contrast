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
})
