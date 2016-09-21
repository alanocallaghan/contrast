\name{contrast.lm}
\alias{contrast}
\alias{contrast.lm}
\alias{contrast.gls}
\alias{contrast.lme}
\alias{contrast.geese}
\alias{print.contrast}
\title{General Contrasts of Regression Coefficients}
\description{
  This function computes one or more contrasts of the estimated
  regression coefficients in a fit from one of the functions in Design,
  along with standard errors, confidence limits, t or Z statistics, P-values.
}
\usage{
contrast(fit, \dots)
\method{contrast}{lm}(fit, \dots)
\method{contrast}{gls}(fit, \dots)
\method{contrast}{lme}(fit, \dots)
\method{contrast}{geese}(fit, \dots)

\method{print}{contrast}(x, X=FALSE, fun=function(u)u, \dots)
}
\arguments{
  \item{fit}{
  a fit of class \code{"lm"}, \code{"glm"}, etc.
  }
  \item{\dots}{arguments to pass to the computational code. The
    arguments are listed in the Details section below.}
  \item{x}{result of \code{contrast}}
  \item{X}{
  set \code{X=TRUE} to  print design matrix used in computing the contrasts (or
  the average contrast)
  }
  \item{fun}{
  a function to transform the contrast, SE, and lower and upper
  confidence limits before printing.  For example, specify \code{fun=exp} to
  anti-log them for logistic models.
  }
  
}
\value{
  a list of class \code{"contrast.Design"} containing the elements
  \code{Contrast}, \code{SE}, \code{Z}, \code{var}, \code{df.residual}
  \code{Lower}, \code{Upper}, \code{Pvalue}, \code{X}, \code{cnames}, which denote the contrast
  estimates, standard errors, Z or t-statistics, variance matrix,
  residual degrees of freedom (this is \code{NULL} if the model was not
  \code{ols}), lower and upper confidence limits, 2-sided P-value, design
  matrix, and contrast names (or \code{NULL}).
  
  Also, an element called \code{foldChange}.
}

\details{
These functions mirror \code{contrast.rms}. 

There are some between-package inconsistencies regarding degrees of freedom in some models. See the package vignette for more details.

Fold changes are calculated for each hypothesis. When \code{fcType =
  "simple"}, the ratio of the \code{a} group predictions over the
\code{b} group predictions are used. When \code{fcType = "signed"}, the
ratio is used if it is greater than 1; otherwise the negative inverse
(e.g., \code{-1/ratio}) is returned.

Arguments to the contast functions are:
  \bold{\code{a}}:  a list containing settings for all predictors that you do not wish to
  set to default (adjust-to) values.  Usually you will specify two
  variables in this list, one set to a constant and one to a sequence of
  values, to obtain contrasts for the sequence of values of an
  interacting factor.  The \code{gendata} function will generate the
  necessary combinations and default values for unspecified predictors.
  

  \bold{\code{a}}: another list that generates the same number of observations as \code{a},
  unless one of the two lists generates only one observation.  In that
  case, the design matrix generated from the shorter list will have its
  rows replicated so that the contrasts assess several differences
  against the one set of predictor values.  This is useful for comparing
  multiple treatments with control, for example.  If \code{b} is missing, the
  design matrix generated from \code{a} is analyzed alone.
  
 \bold{\code{covType}}:  a string matching the method for estimating the covariance matrix. The default value produces the typical estimate. See \code{\link[sandwich]{vcovHC}} for options.
  
  \bold{\code{cnames}}:  vector of character strings naming the contrasts when
  \code{type="individual"}.  Usually \code{cnames} is not necessary as
  \code{contrast.rms} tries to name the contrasts by examining which
  predictors are varying consistently in the two lists.  \code{cnames} will
  be needed when you contrast "non-comparable" settings, e.g., you
  compare \code{list(treat="drug", age=c(20,30))} with
  \code{list(treat="placebo"), age=c(40,50)}
  
  \bold{\code{type}}:  set \code{type="average"} to average the individual contrasts (e.g., to
  obtain a Type II or III contrast)
  
  \bold{\code{weights}}:  a numeric vector, used when \code{type="average"}, to obtain weighted contrasts
  
  \bold{\code{conf.int}}: confidence level for confidence intervals for the contrasts
  

  \bold{\code{env}}: environment in which evaluate fit

  \bold{\code{fcFun}}: a function to transform the numerator and denominator of fold changes
  
  \bold{\code{fcType}}:  a character string: "simple", "log" or "signed"
  
}

\seealso{
  \code{\link[rms]{contrast.rms}}, \code{\link[sandwich]{vcovHC}}
}
\examples{
library(nlme)
Orthodont2 <- Orthodont
Orthodont2$newAge <- Orthodont$age - 11
fm1Orth.lme2 <- lme(distance ~ Sex*newAge, data = Orthodont2, random = ~ newAge | Subject)
summary(fm1Orth.lme2)

contrast(fm1Orth.lme2,
         a = list(Sex = levels(Orthodont2$Sex), newAge = 8 - 11),
         b = list(Sex = levels(Orthodont2$Sex), newAge = 10 - 11))

}
\keyword{models}
\keyword{regression}
