linearsim <- function (x, type="ev", b, ci = 0.95, constant = 1, ...) 
# … could include save, sigma2, sims
{
  # extract the ...

  if (!is.null(sigma2)) {
    predict <- TRUE
    sigma <- sqrt(sigma2)
  }
  else predict <- FALSE
  save <- ( !is.null(save) && save)

  # set up matrices
  esims <- nrow(as.matrix(b))
  nscen <- nrow(x)
  nci <- length(ci)
  res <- list(pe = rep(NA, nscen), lower = matrix(NA, nrow = nscen, 
        ncol = nci), upper = matrix(NA, nrow = nscen, ncol = nci))
  if (predict) {
    res$plower <- matrix(NA, nrow = nscen, ncol = nci)
    res$pupper <- matrix(NA, nrow = nscen, ncol = nci)
  }
  if (save) {
    res$ev <- matrix(NA, nrow = nscen, ncol = esims)
    if (predict) {
      res$pv <- matrix(NA, nrow = nscen, ncol = esims * sims)
    }
  }

  for (i in 1:nscen) {
    simmu <- b %*% x[i, ]
    if (save) 
      res$ev[i, ] <- simmu
    res$pe[i] <- mean(simmu)
    for (k in 1:nci) {
      cint <- quantile(simmu, probs = c((1 - ci[k])/2, 
                (1 - (1 - ci[k])/2)))
      res$lower[i, k] <- cint[1]
      res$upper[i, k] <- cint[2]
    }
    if (predict) {
      pv <- rnorm(sims * esims, mean = simmu, sd = sigma)
      if (save) 
        res$pv[i, ] <- pv
      for (k in 1:nci) {
        cint <- quantile(pv, probs = c((1 - ci[k])/2, 
                  (1 - (1 - ci[k])/2)))
        res$plower[i, k] <- cint[1]
        res$pupper[i, k] <- cint[2]
      }
    }
  }

  # Maybe ev is the "default" requiring just this fn,
  # but others need specialized fn's:

  if (type != "ev") {

  # call specific simulation functions
  # create function name
  # (since "linearsimXX" are the user-level functions, I chose "linsimXX" for these functions)
    fname = paste("linsim", type)
    res <- do.call(fname, args=list(res=res, b=b, ci=ci, constant=constant, ...))
  }

  res$lower <- drop(res$lower)
  res$upper <- drop(res$upper)
  if (predict) {
    res$plower <- drop(res$plower)
    res$pupper <- drop(res$pupper)
  }
  res

}
