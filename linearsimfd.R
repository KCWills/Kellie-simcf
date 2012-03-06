linearsimfd <- function (x, b, formula=NULL, ci = 0.95, constant = 1, xpre = NULL)
{
    if (is.null(x)) {
      stop("x is missing.")
    }
    if (is.null(formula)) {
	mm <- modelMatrixHelper(x, b, constant, which.cf="x")
    } else {
	  tl <- attributes(terms(formula))$term.labels
	  rhs <- paste(tl, collapse = " + ")
	  rhs <- as.formula(paste("~ ", rhs))
	  x <- as.data.frame(x)
        mf <- model.frame(rhs, x)
        mm <- modelMatrixHelper(mf, b, constant)
    }
    
    if (is.null(x$xpre)) {
      if (is.null(xpre)) stop("xpre is missing.")
    } else {
      xpre = x$xpre
    }
    if (is.null(formula)) {
      mmpre <- modelMatrixHelper(x, b, constant, which.cf="xpre")
    } else {
      xpre <- as.data.frame(xpre)
      mf <- model.frame(rhs, xpre)
      mmpre <- modelMatrixHelper(mf, b, constant)
    }

    x <- mm
    xpre <- mmpre

    esims <- nrow(as.matrix(b))
    nscen <- nrow(x)
    nci <- length(ci)
    res <- list(pe = rep(NA, nscen), lower = matrix(NA, nrow = nscen, 
        ncol = nci), upper = matrix(NA, nrow = nscen, ncol = nci))
    for (i in 1:nscen) {
        simmu1 <- b %*% xpre[i, ]
        simmu2 <- b %*% x[i, ]
        simy <- simmu2 - simmu1
        res$pe[i] <- mean(simy)
        for (k in 1:nci) {
            cint <- quantile(simy, probs = c((1 - ci[k])/2, (1 - 
                (1 - ci[k])/2)))
            res$lower[i, k] <- cint[1]
            res$upper[i, k] <- cint[2]
        }
    }
    res$lower <- drop(res$lower)
    res$upper <- drop(res$upper)
    res
}