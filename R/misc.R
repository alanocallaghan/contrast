# generic functions defined in rms
#
#   contrast <- function(fit, ...) UseMethod("contrast")
#   gendata <- function(fit, ...) UseMethod("gendata")

# Evaluate the original gls model formula in the requested environment.
formula.gls <- function(x, ..., env = NULL) {
    if (is.null(env)) {
        eval(x$call$model)
    } else {
        eval(x$call$model, env)
    }
}

modelFormula <- function(object, env = NULL) {
    if (inherits(object, "gls")) {
        formula.gls(object, env = env)
    } else {
        formula(object, env = env)
    }
}

# methods neeeded, but missing, from geepack
#' @export 
coef.geese <- function(object, ...) object$beta
#' @export 
vcov.geese <- function(object, ...) object$vbeta

# This function mimics rms:::gendata, which is unusable
# on non-rms objects
generateData <- function(fit, factors, ..., env = NULL) {
    tt <- tryCatch(
        terms(fit),
        error = function(e) {
            terms(modelFormula(fit, env = env))
        }
    )
    order <- attr(tt, "order")
    tlabs <- attr(tt, "term.labels")
    nam <- tlabs[order == 1]
    fnam <- names(factors)
    nf <- length(factors)

    if (nf == 0) {
        stop("illegal factors argument")
    }

    wh <- sapply(fnam, function(x) any(grepl(x, nam, fixed = TRUE)))
    if (!all(wh)) {
        stop(paste(
            "factor(s) not in design:",
            paste(names(factors)[!wh], collapse = " ")
        ))
    }

    if (nf < length(nam)) {
        stop("not enough factors")
    }

    settings <- list()
    for (i in 1:nf) {
        settings[[fnam[i]]] <- factors[[i]]
    }

    expand.grid(settings)
}
