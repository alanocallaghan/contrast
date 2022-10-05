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

    expect_output(
        print(the_contrast, X = FALSE, fun = exp),
        "glm model parameter contrast"
    )
})
