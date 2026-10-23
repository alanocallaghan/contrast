test_that("fun works with glm", {
    nSim <- 1000
    a <- 4
    b <- 4.1

    simData <- rbind(
    data.frame(trt="A", y=rpois(n=nSim, lambda=a)),
    data.frame(trt="B", y=rpois(n=nSim, lambda=b))
    )

    the_fit <- glm(y ~ -1 + trt, family = poisson(link = log), data = simData)
    the_contrast <- contrast::contrast(the_fit, list(trt="B"), list(trt="A"), fcFunc = exp)

    expect_equal(the_contrast$df.residual, the_fit$df.residual)
    expect_equal(
        drop(the_contrast$var),
        drop(the_contrast$X %*% vcov(the_fit) %*% t(the_contrast$X))
    )

    expect_output(
        print(the_contrast, X = FALSE, fun = exp),
        "glm model parameter contrast"
    )
})

test_that("saturated glm contrasts use normal inference", {
    saturated_data <- data.frame(group = c("A", "B"), y = c(3, 5))
    fit <- glm(y ~ 0 + group, family = poisson, data = saturated_data)
    result <- contrast(fit, list(group = "B"), list(group = "A"))

    expect_equal(result$df.residual, 0)
    expect_equal(result$Pvalue, 2 * (1 - pnorm(abs(result$testStat))))
})
