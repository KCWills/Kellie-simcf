is.cf <- function(x)
{
  if ( !any(class(x) == "counterfactual") ) {
    return(FALSE)
  }

  if (is.null(x$model) || class(x$model) != "formula") {
    cat("Warning in is.cf(): model is missing, or it is not a formula.\n")    
  }

  if ( is.null(x$x) || is.null(x$xpre) ) {
    if ( is.null(x$x) ) cat("Data frame x is missing.\n")
    if ( is.null(x$xpre) ) cat("Data frame xpre is missing.\n")
    return(FALSE)
  } else if (class(x$x) != "data.frame" | class(x$xpre) != "data.frame") {
    if (class(x$x) != "data.frame") cat("x is not a data frame.\n")
    if (class(x$xpre) != "data.frame") cat("xpre is not a data frame.\n")
    return(FALSE)
  } else {
    if (dim(x$x)[1] != dim(x$xpre)[1]) {
      cat("x and xpre must have the same number of rows.\n")
      return(FALSE)
    }
  }

  return(TRUE)
  
}
