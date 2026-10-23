context("lm models")

# ------------------------------------------------------------------------------
library(splines)

lm_fit_1 <- lm(mpg ~ (wt + am), data = mtcars)
lm_fit_2 <- lm(expression ~ ., data = two_factor_crossed)
lm_fit_3 <- lm(mpg ~ ns(wt, 2) + am, data = mtcars)

summary_1 <- summary(lm_fit_1)
summary_2 <- summary(lm_fit_2)
summary_3 <- summary(lm_fit_3)

anova_1 <- anova(lm_fit_1)
anova_2 <- anova(lm_fit_2)
anova_3 <- anova(lm_fit_3)

test_that("all numeric", {
    a_1 <- list(am = 0, wt = 2)
    b_1 <- list(am = 1, wt = 2)
    c_1 <- contrast(lm_fit_1, a_1, b_1)
    expect_equivalent(
        summary_1$coefficients["am", "Pr(>|t|)"],
        c_1$Pvalue
    )
    expect_equal(c_1$df.residual, df.residual(lm_fit_1))
    expect_equal(
        drop(c_1$var),
        drop(c_1$X %*% sandwich::vcovHC(lm_fit_1, type = "const") %*% t(c_1$X))
    )

    # bad inputs
    a_2 <- list(am = "a", wt = 2)
    b_2 <- list(am = "b", wt = 2)
    expect_error(
        contrast(lm_fit_1, a_2, b_2),
        "variable 'am' was fitted with type \"numeric\" but type \"factor\""
    )
})

test_that("numeric and factor", {
    a_3 <- list(diet = "low fat", group = "vehicle")
    b_3 <- list(diet = "high fat", group = "vehicle")
    c_3 <- contrast(lm_fit_2, a_3, b_3)
    expect_equivalent(
        summary_2$coefficients["dietlow fat", "Pr(>|t|)"],
        c_3$Pvalue
    )
    a_4 <- list(diet = "low fat", group = "vehicle")
    b_4 <- list(diet = "low fat", group = "treatment")
    c_4 <- contrast(lm_fit_2, a_4, b_4)
    expect_equivalent(
        summary_2$coefficients["groupvehicle", "Pr(>|t|)"],
        c_4$Pvalue
    )
})

test_that("function in formula works as expected", {
    a_5 <- list(am = 0, wt = 2)
    b_5 <- list(am = 1, wt = 2)
    c_5 <- contrast(lm_fit_3, a_5, b_5)
    expect_equivalent(
        summary_3$coefficients["am", "Pr(>|t|)"],
        c_5$Pvalue
    )

    # bad inputs
    a_6 <- list(am = "a", wt = 2)
    b_6 <- list(am = "b", wt = 2)
    expect_error(
        contrast(lm_fit_3, a_6, b_6),
        "variable 'am' was fitted with type \"numeric\" but type \"factor\""
    )
})

test_that("average contrasts validate and apply weights", {
    interaction_fit <- lm(mpg ~ wt * am, data = mtcars)
    a <- list(am = 1, wt = c(2, 4))
    b <- list(am = 0, wt = c(2, 4))

    individual <- contrast(interaction_fit, a, b)
    weighted <- contrast(
        interaction_fit, a, b, type = "average", weights = c(1, 3)
    )
    expected_X <- colSums(individual$X * c(1, 3)) / 4

    expect_equal(drop(weighted$X), expected_X)
    expect_equal(drop(weighted$Contrast), drop(expected_X %*% coef(interaction_fit)))

    expect_error(
        contrast(interaction_fit, a, b, type = "average", weights = c(0, 0)),
        "positive sum"
    )
    expect_error(
        contrast(interaction_fit, a, b, type = "average", weights = c(1, NA)),
        "finite, non-negative"
    )
    expect_error(
        contrast(interaction_fit, a, b, type = "average", weights = c(1, -1)),
        "finite, non-negative"
    )
    expect_error(
        contrast(interaction_fit, a, b, type = "average", weights = c(1, Inf)),
        "finite, non-negative"
    )
    expect_error(
        contrast(interaction_fit, a, b, type = "average", weights = c(1e308, 1e308)),
        "finite, non-negative"
    )
    expect_error(
        contrast(interaction_fit, a, b, type = "average", weights = c("1", "2")),
        'weights must be "equal" or a numeric vector'
    )
    expect_error(
        contrast(interaction_fit, a, b, type = "average", weights = c("equal", "equal")),
        'weights must be "equal" or a numeric vector'
    )
})

test_that("contrast options and aliased coefficients are validated", {
    a <- list(wt = 2)
    b <- list(wt = 1)

    for (conf.int in list(0, 1, NA_real_, Inf, c(0.9, 0.95), "0.95")) {
        expect_error(
            contrast(lm_fit_1, a, b, conf.int = conf.int),
            "conf.int must be a single finite number between 0 and 1"
        )
    }
    expect_error(
        contrast(lm_fit_1, a, b, fcType = "unknown"),
        "'arg' should be one of"
    )
    expect_error(
        contrast(lm_fit_1, a, b, fcFunc = 1),
        "fcFunc must be a function"
    )

    aliased_data <- data.frame(
        y = c(1, 2, 2, 4, 3, 6),
        x = 1:6,
        z = 2 * (1:6)
    )
    aliased_fit <- lm(y ~ x + z, data = aliased_data)
    expect_error(
        contrast(
            aliased_fit,
            list(x = 5, z = 10),
            list(x = 4, z = 8)
        ),
        "contrasts are not supported for models with aliased coefficients"
    )
})
