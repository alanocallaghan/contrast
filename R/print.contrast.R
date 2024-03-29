# This method is used for printing the objects returned by the contrast methods.
# It was copied from the rms package, written by Frank Harrell.

#' Print a Contrast Object
#' @param x Result of `contrast()`.
#' @param X A logical: set `TRUE` to print design matrix used in computing the
#' contrasts (or the average contrast).
#' @param fun A function to transform the contrast, SE, and lower and upper
#' confidence limits before printing.  For example, specify `fun = exp` to
#' anti-log them for logistic models.
#' @param ... Not used.
#' @export
print.contrast <- function(x, X = FALSE, fun = function(u) u, ...) {
    testLabels <- switch(x$model,
        lm = ,
        glm = ,
        lme = ,
        gls = c("t", "Pr(>|t|)"),
        geese = c("Z", "Pr(>|Z|)")
    )

    w <- x[c(
        "Contrast",
        "SE",
        "Lower",
        "Upper",
        "testStat",
        "df",
        "Pvalue"
    )]
    w$testStat <- round(w$testStat, 2)
    w$Pvalue <- round(w$Pvalue, 4)
    no <- names(w)
    no[no == "SE"] <- "S.E."
    no[no == "testStat"] <- testLabels[1]
    no[no == "Pvalue"] <- testLabels[2]
    names(w) <- no

    cat(x$model, "model parameter contrast\n\n")

    cnames <- x$cnames
    if (length(cnames) == 0) {
        cnames <- if (x$nvary) {
            rep("", length(x[[1]]))
        } else {
            as.character(1:length(x[[1]]))
        }
    }
    attr(w, "row.names") <- cnames
    attr(w, "class") <- "data.frame"
    w$Contrast <- fun(w$Contrast)
    w$S.E. <- fun(w$S.E.)
    if (x$model != "geese") {
        w$df <- x$df
    }
    w$Lower <- fun(w$Lower)
    w$Upper <- fun(w$Upper)
    print(as.matrix(w), quote = FALSE, ...)
    if (X) {
        attr(x$X, "contrasts") <- NULL
        attr(x$X, "assign") <- NULL
        cat("\nContrast coefficients:\n")
        if (is.matrix(x$X)) {
            dimnames(x$X) <- list(cnames, dimnames(x$X)[[2]])
        }
        print(x$X, ...)
    }

    if (x$model == "lm") {
        if (x$covType != "const") {
            cat("\nThe", x$covType, "covariance estimator was used.\n")
        }
    }

    invisible()
}
