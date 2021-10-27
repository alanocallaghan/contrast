#' General Contrasts of Regression Coefficients
#'
#' This function computes one or more contrasts of the estimated regression
#' coefficients in a fit from one of the functions in Design, along with
#' standard errors, confidence limits, t or Z statistics, P-values.
#'
#' @param fit A fit of class `lm`, `glm`, etc.
#' @return a list of class `contrast.Design` containing the elements
#'  `Contrast`, `SE`, `Z`, `var`, `df.residual` `Lower`, `Upper`, `Pvalue`, `X`,
#'  `cnames`, and `foldChange`, which denote the contrast estimates, standard
#'  errors, Z or t-statistics, variance matrix, residual degrees of freedom
#'  (this is `NULL` if the model was not `ols`), lower and upper confidence
#'  limits, 2-sided P-value, design matrix, and contrast names (or `NULL`).
#' @details
#' These functions mirror [rms::contrast.rms()] but have fewer options.
#'
#' There are some between-package inconsistencies regarding degrees of freedom
#' in some models. See the package vignette for more details.
#'
#' Fold changes are calculated for each hypothesis. When `fcType ="simple"`, the
#' ratio of the `a` group predictions over the `b`
#' group predictions are used. When `fcType = "signed"`, the ratio is used
#' if it is greater than 1; otherwise the negative inverse (e.g.,
#' `-1/ratio`) is returned.
#' @seealso [rms::contrast.rms()], [sandwich::vcovHC()]
#' @keywords models regression
#' @examples
#'
#' library(nlme)
#' Orthodont2 <- Orthodont
#' Orthodont2$newAge <- Orthodont$age - 11
#' fm1Orth.lme2 <- lme(distance ~ Sex * newAge,
#'     data = Orthodont2,
#'     random = ~ newAge | Subject
#' )
#' summary(fm1Orth.lme2)
#'
#' contrast(fm1Orth.lme2,
#'     a = list(Sex = levels(Orthodont2$Sex), newAge = 8 - 11),
#'     b = list(Sex = levels(Orthodont2$Sex), newAge = 10 - 11)
#' )
#'
#' # ---------------------------------------------------------------------------
#'
#' anova_model <- lm(expression ~ diet * group, data = two_factor_crossed)
#' anova(anova_model)
#'
#' library(ggplot2)
#' theme_set(theme_bw() + theme(legend.position = "top"))
#' ggplot(two_factor_crossed) +
#'     aes(x = diet, y = expression, col = group, shape = group) +
#'     geom_point() +
#'     geom_smooth(aes(group = group), method = lm, se = FALSE)
#'
#' int_model <- lm(expression ~ diet * group, data = two_factor_crossed)
#' main_effects <- lm(expression ~ diet + group, data = two_factor_crossed)
#'
#' # Interaction effect is probably real:
#' anova(main_effects, int_model)
#'
#' # Test treatment in low fat diet:
#' veh_group <- list(diet = "low fat", group = "vehicle")
#' trt_group <- list(diet = "low fat", group = "treatment")
#' contrast(int_model, veh_group, trt_group)
#'
#' # ---------------------------------------------------------------------------
#'
#' car_mod <- lm(mpg ~ am + wt, data = mtcars)
#' print(summary(car_mod), digits = 5)
#'
#' mean_wt <- mean(mtcars$wt)
#'
#' manual_trans <- list(am = 0, wt = mean_wt)
#' auto_trans <- list(am = 1, wt = mean_wt)
#' print(contrast(car_mod, manual_trans, auto_trans), digits = 5)
#' @export
contrast.lm <- function(fit, ...) contrast_calc(fit, ...)

## This contrast method is very much like contrast.rms, but uses
## the predictFrame function (defined earlier in this file) instead of
## the predictDesign function defined in the Design package.  It also
## uses the testStatistic function (defined earlier in this file) to make
## it more modular.

## define the gls, lme and geese versions to execute the lm version
#' @export
#' @rdname contrast.lm
contrast.gls <- function(fit, ...) contrast_calc(fit, ...)

#' @export
#' @rdname contrast.lm
contrast.lme <- function(fit, ...) contrast_calc(fit, ...)

#' @export
#' @rdname contrast.lm
contrast.geese <- function(fit, ...) contrast_calc(fit, ...)

#' @export
#' @rdname contrast.lm
#' @param a,b Lists containing conditions for all predictors in the model that
#'  will be contrasted to form the hypothesis `H0: a = b`. The `gendata`
#'  function will generate the necessary combinations and default values for
#'  unspecified predictors. See examples below.
#' @param cnames A vector of character strings naming the contrasts when
#' `type = "individual"`.  Usually `cnames` is not necessary as
#' the function tries to name the contrasts by examining which
#' predictors are varying consistently in the two lists.  `cnames` will be
#' needed when you contrast "non-comparable" settings, e.g., you compare
#' `list(treat = "drug", age = c(20,30))` with
#' `list(treat = "placebo", age = c(40,50)`.
#' @param weights A numeric vector, used when `type = "average"`, to
#' obtain weighted contrasts.
#' @param conf.int The confidence level for confidence intervals for the
#' contrasts.
#' @param env An environment in which evaluate fit.
#' @param fcFunc A function to transform the numerator and denominator
#' of fold changes.
#' @param fcType A character string: "simple", "log" or "signed".
#' @param type A character string. Set `type="average"` to average the
#' individual contrasts (e.g., to obtain a "Type II" or "Type III" contrast).
#' @param ... For `contrast()`, these pass arguments to `contrast_calc()`. For
#' `contrast_calc()`, they are not used.
#' @param covType A string matching the method for estimating the covariance
#' matrix. The default value produces the typical estimate. See
#' [sandwich::vcovHC()] for options.
contrast_calc <- function(fit, a, b, cnames = NULL,
                          type = c("individual", "average"),
                          weights = "equal",
                          conf.int = 0.95,
                          fcType = "simple",
                          fcFunc = I,
                          covType = NULL,
                          ...,
                          env = parent.frame(2)) {
    type <- match.arg(type)

    da <- do.call("generateData", list(fit = fit, factors = a, env = env))
    xa <- predictFrame(fit, da, env = env)
    ma <- nrow(xa)

    if (missing(b)) {
        xb <- 0 * xa
        db <- da
    } else {
        db <- do.call("generateData", list(fit, factors = b, env = env))
        xb <- predictFrame(fit, db, env = env)
    }
    mb <- nrow(xb)

    vary <- NULL
    if (type == "individual" && length(cnames) == 0) {
        ## If two lists have same length, label contrasts by any variable
        ## that has the same length and values in both lists
        if (ma == mb) {
            if (ncol(da) != ncol(db)) {
                stop("program logic error")
            }
            if (any(sort(names(da)) != sort(names(db)))) {
                stop("program logic error")
            }
            k <- integer(0)
            nam <- names(da)
            for (j in 1:length(da)) {
                if (all(as.character(da[[nam[j]]]) == as.character(db[[nam[j]]]))) {
                    k <- c(k, j)
                }
            }
            if (length(k) > 0) vary <- da[k]
        } else if (max(ma, mb) > 1) {
            ## Label contrasts by values of longest variable in list if
            ## it has the same length as the expanded design matrix
            d <- if (ma > 1) a else b
            l <- sapply(d, length)
            vary <- if (sum(l == max(ma, mb)) == 1) d[l == max(ma, mb)]
        }
    }

    if (max(ma, mb) > 1 && min(ma, mb) == 1) {
        if (ma == 1) {
            xa <- matrix(xa, nrow = mb, ncol = ncol(xb), byrow = TRUE)
        } else {
            xb <- matrix(xb, nrow = ma, ncol = ncol(xa), byrow = TRUE)
        }
    } else if (mb != ma) {
        stop("number of rows must be the same for observations generated\nby a and b unless one has one observation")
    }

    X <- xa - xb
    p <- ncol(X)
    m <- nrow(X)

    modelCoef <- getCoefficients(fit)
    denom <- xb %*% modelCoef
    numer <- xa %*% modelCoef
    ratio <- fcFunc(numer) / fcFunc(denom)
    fc <- switch(fcType,
        simple = ratio,
        log = log(ratio),
        signed = ifelse(ratio > 1, ratio, -1 / ratio)
    )

    if (is.character(weights)) {
        if (weights != "equal") {
            stop('weights must be "equal" or a numeric vector')
        }
        weights <- rep(1, m)
    } else if (length(weights) > 1 && type == "individual") {
        stop('can specify more than one weight only for type="average"')
    } else if (length(weights) != m) {
        stop(paste("there must be", m, "weights"))
    }
    weights <- as.vector(weights)

    if (m > 1 && type == "average") {
        X <- matrix(
            apply(weights * X, 2, sum) / sum(weights),
            nrow = 1,
            dimnames = list(NULL, dimnames(X)[[2]])
        )
    }

    if (class(fit)[1] == "lm") {
        if (is.null(covType)) {
            covType <- "const"
        }
        covMat <- try(sandwich::vcovHC(fit, type = covType), silent = TRUE)
        if (class(covMat)[1] == "try-error") {
            warning("Sandwich estimate failed; using standard estimate instead")
            covMat <- vcov(fit)
        }
    } else {
        covMat <- vcov(fit)
    }

    res <- testStatistic(fit, X, modelCoef, covMat, conf.int = conf.int)
    res$cnames <- if (type == "average") {
        NULL
    } else {
        cnames
    }
    res$nvary <- length(vary)
    res$foldChange <- fc
    res$aCoef <- xa
    res$bCoef <- xb
    res$model <- class(fit)[1]
    res$covType <- covType
    if (type == "individual") {
        res <- c(vary, res)
    }
    structure(res, class = "contrast")
}
