# Model matrix helper function 
# by K. Wills

# arg "which.cf" specifies e.g. xpre
# call: modelMatrixHelper(x=xpre, b, constant, nscen)
# or
# modelMatrixHelper(x=x.cf, b, constant, nscen, which.cf="xpre")

modelMatrixHelper <- function(x, b, constant, nscen=1, which.cf="x")
{
  if ( !is.character(which.cf) ) stop("which.cf must be a string")
    
  if ( !is.na(constant) ) {
    if (length(constant) > 1 || !is.numeric(constant) || constant <= 0)
      stop("constant must be positive scalar (which column of b has model constant?)")
  }

  if (is.null(x)) {
    if (is.na(constant)) {
      stop("either x must be given, or a constant specified")
    } else {	
      mm <- matrix(1, nrow = nscen, ncol = 1)
    }
    return(mm)
  }
	  
  if ( is.cf(x) && !is.null(x$model) ) {
    mm <- model.matrix(x$model, x[[which.cf]])
    return(mm)       
  }

  if (any(class(x) == "list")) {
    if (is.null(x[[which.cf]])) stop("x[[which.cf]] is missing")
    x <- x[[which.cf]]
  }
  
  if (is.data.frame(x)) x <- as.matrix(x)
  if (!is.numeric(x)) stop("counterfactual must be numeric")
      
  if (!is.matrix(x)) {		
    if (is.matrix(b)) {		
      mm <- t(x)
      if (!is.na(constant)) {
        mm <- append(mm, 1, constant - 1)
      }
    } else {
      mm <- as.matrix(x)
      if (!is.na(constant)) {
        mm <- appendmatrix(mm, rep(1, nrow(x)), constant)
      }
    }
  } else {
    mm <- x
    if (!is.na(constant)) {
      mm <- appendmatrix(mm, rep(1, nrow(x)), constant)
    }
  }
    
  mm
}