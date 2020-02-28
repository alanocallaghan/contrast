context("lm models")

# ------------------------------------------------------------------------------

lm_fit_1 <- lm(mpg ~ (wt + am), data = mtcars)
lm_fit_2 <- lm(expression ~ ., data = two_factor_crossed)

summary_1 <- summary(lm_fit_1)
summary_2 <- summary(lm_fit_2)

anova_1 <- anova(lm_fit_1)
anova_2 <- anova(lm_fit_2)

test_that("all numeric", {
  a_1 <- list(am = 0, wt = 2)
  b_1 <- list(am = 1, wt = 2)
  c_1 <- contrast(lm_fit_1, a_1, b_1)
  expect_equivalent(
    summary_1$coefficients["am", "Pr(>|t|)"],
    c_1$Pvalue
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
