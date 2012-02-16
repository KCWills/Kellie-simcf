linearsimfd <- function (x, b, ci = 0.95, constant = 1, xpre = NULL) 
{
    if (is.null(x)) {
      stop("x is missing.")
    }
    mm <- modelMatrixHelper(x, b, constant, which.cf="x")

    if (is.null(x$xpre)) {
      if (is.null(xpre)) stop("xpre is missing.")
      x$xpre <- modelMatrixHelper(x=xpre, b, constant)
    } else {
      xpre <- modelMatrixHelper(x, b, constant, which.cf="xpre")
    }

    x <- mm

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