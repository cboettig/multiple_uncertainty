#' logistic
#' 
#' logistic growth with harvesting
#' @export
#' 
logistic <- function(x, h) max( max((x - h),0) * (1 - max((x - h),0) / 100) + max((x - h),0), 0)
