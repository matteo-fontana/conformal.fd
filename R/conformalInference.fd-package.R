#'Tools for Conformal Inference for Regression in Multivariate
#' Functional Setting
#'
#'@description It computes split conformal and multi split
#' conformal prediction regions when the response has functional nature.
#' Moreover, the package also contain a plot function to visualize the
#' output of the split conformal.
#'
#' @details Conformal inference is a framework for converting any pre-chosen
#' estimator of
#'   the regression function into prediction regions with finite-sample
#'   validity, under essentially no assumptions on the data-generating process
#'   (aside from the the assumption of i.i.d. observations). The main functions
#'   in this package for computing such prediction regions are
#'   \code{\link{conformal.fun.split}} , i.e. a single split, and
#'    \code{\link{conformal.fun.msplit}} , i.e. joining B splits.
#'   To guarantee consistency, the package structure mimics the univariate
#' 'conformalInference' package of professor Ryan Tibshirani.
#'
#' @references
#' \itemize{
#'  \item{"Conformal Prediction Bands
#' for Multivariate Functional Data" by Diquigiovanni, Fontana, and Vantini (2021)
#' <arXiv:2106.01792>}
#'  \item{"The Importance of Being a Band: Finite-Sample Exact Distribution-Free
#' Prediction Sets for Functional Data" by Diquigiovanni, Fontana, and Vantini (2021) <arXiv:2102.06746>}
#'  \item{"Multi Split Conformal Prediction" by Solari, and Djordjilovic (2021) <arXiv:2103.00627>}
#' }
#' @keywords internal
"_PACKAGE"


