#' Winsorize
#'
#' Well it does Win-motherfucking-sorization
#' @export
windsorize <- function(x, p = .90) {
  if( all(is.na(x)) ) stop("Input must not contain only NAs!")
  if( all(is.null(x)) ) stop("Input must contain data!")
  qhigh <- quantile(x, 1-(1-p)/2, na.rm=TRUE)
  qlow <- quantile(x, (1-p)/2, na.rm=TRUE)
  x[x >= qhigh] <- qhigh
  x[x <= qlow] <- qlow
  x
}

