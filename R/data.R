#' Complete Two-Factor Experiment
#'
#' @details
#' A gene expression experiment was run to assess the effect of a compound
#'  under two different diets: high fat and low fat. The main comparisons of
#'  interest are the difference between the treated and untreated groups within
#'  a diet. The interaction effect was a secondary hypothesis. For illustration,
#'  we only include the expression value of one of the genes. The study design
#'  was a full two-way factorial with `n = 24` samples.
#'
#' @name two_factor_crossed
#' @docType data
#' @return \item{two_factor_crossed}{A data frame}
#' @keywords datasets
#' @examples
#' two_factor_crossed
NULL

#' Incomplete Two-Factor Experiment with Repeated Measurments
#'
#' @details
#' In a gene expression experiment, stem cells were differentiated using a set
#'  of factors (such as media types, cell spreads etc.). These factors were
#'  collapsed into a single cell environment configurations variable. The cell
#'  lines were assays over three days. Two of the configurations were only run
#'  on the first day and the other two were assays at baseline.
#'
#' To get the materials, three donors provided materials. These donors
#'  provided (almost) equal replication across the two experimental factors (day
#'  and configuration).
#'
#' One of the goals of this experiment was to assess pre-specified
#'  differences in the configuration at each time point. For example, the
#'  differences between configurations `A` and `B` at day one is of interest.
#'  Also, the differences between configurations `C` and `D` at each time points
#'  were important.
#'
#' Since there are missing cells in the design, it is not a complete two-way
#'  factorial. One way to analyze this experiment is to further collapse the
#'  time and configuration data into a single variable and then specify each
#'  comparison using this factor.
#'
#' @name two_factor_incompl
#' @docType data
#' @return \item{two_factor_incompl}{A data frame}
#' @keywords datasets
#' @examples
#' two_factor_incompl
NULL
