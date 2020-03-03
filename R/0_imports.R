#' @importFrom nlme asOneFormula fixef getCovariateFormula getGroups
#' @importFrom nlme getGroupsFormula glsStruct lmeStruct pdSymm reStruct
#' @importFrom nlme splitFormula varFunc
#' @importFrom sandwich vcovHC
#' @importFrom stats .checkMFClasses .getXlevels  asOneSidedFormula coef
#' @importFrom stats contrasts contrasts<-  delete.response formula model.frame
#' @importFrom stats model.matrix na.fail pnorm pt qnorm qt terms vcov

# ------------------------------------------------------------------------------
# re-exports

#' @importFrom rms contrast
#' @export
rms::contrast
