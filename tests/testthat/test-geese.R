context("geese models")

# ------------------------------------------------------------------------------

library(geepack)
library(nlme)

# from ?geese
data(seizure)
## Diggle, Liang, and Zeger (1994) pp166-168, compare Table 8.10
seiz.l <- reshape(
  seizure,
  varying = list(c("base", "y1", "y2", "y3", "y4")),
  v.names = "y",
  times = 0:4,
  direction = "long"
)
seiz.l <- seiz.l[order(seiz.l$id, seiz.l$time), ]
seiz.l$t <- ifelse(seiz.l$time == 0, 8, 2)
seiz.l$x <- ifelse(seiz.l$time == 0, 0, 1)
geese_fit_1 <- geese(
  y ~ offset(log(t)) + x + trt,
  id = id,
  data = seiz.l,
  corstr = "exch",
  family = poisson
)


test_that("geese models", {
  a_1 <- list(t = 8, trt = 0, x = 1)
  b_1 <- list(t = 8, trt = 1, x = 1)
  c_1 <- contrast(geese_fit_1, a_1, b_1)
  expect_equivalent(
    summary(geese_fit_1)$mean["trt", "p"],
    c_1$Pvalue
  )
})

test_that("printing", {
  a_1 <- list(t = 8, trt = 0, x = 1)
  b_1 <- list(t = 8, trt = 1, x = 1)
  c_1 <- contrast(geese_fit_1, a_1, b_1)
  expect_output(
    print(c_1),
    "geese model parameter contrast"
  )
})
