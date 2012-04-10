# New version of linearsimev() 4/10/2012
# Begins to address the following issues: 
# issue 5: simcf modularity (one underlying linearsim() function)
# issue 15: cf objects should retain and pass on scenario info
# Not tested (linearsim() not ready)

linearsimev <- function (x=NULL, b, formula=NULL, ci = 0.95, constant = 1, sigma2 = NULL, 
    sims = 10, save = FALSE, nscen = 1) 
{
    ## create model matrix
    if (is.null(formula)) {
        mm <- modelMatrixHelper(x, b, constant, nscen, which.cf="x")
    } else {
	  tl <- attributes(terms(formula))$term.labels
	  rhs <- paste(tl, collapse = " + ")
	  rhs <- as.formula(paste("~ ", rhs))
	  if (is.null(x)) {
		mf <- model.frame(rhs) 
	  } else { 
        	x <- as.data.frame(x)
        	mf <- model.frame(rhs, x)
	  } 
        mm <- modelMatrixHelper(mf, b, constant)
    }

    ## linearsim() not quite ready yet...
    res <- linearsim(mm, type="ev", b, ci = 0.95, constant, sigma2, sims, save)
  
    ## create cf object if not passed in
    if ( is.cf(x) ) {
        cfobj <- x
    } else {
        cfobj <- list(x = NULL, xpre = NULL, model = NULL)
	  if (!is.null(formula)) {
	      cfobj$model <- formula
		cfobj$x <- cfobj$xpre <- model.frame(formula)
	  } else {
		cfobj$x <- cfobj$xpre <- x
	  }	
    }
  
    ## add results to cf obj
    cfobj$res <- res

    cfobj
}