setwd("C:/Users/Kellie/Documents/simcf")
source("is_cf.R")
source("modelMatrixHelper.R")

# for example code
library(MASS)

## from cfMake help
library(simcf)
data(UScrime)
model <- (y ~ log(M) + So + log(Ed) + log(Po1) + log(Po2)
              + log(LF) + log(M.F) + log(Pop) + log(NW) +log(U1)
              + log(U2) + log(GDP) + log(Ineq) + log(Prob) +
              log(Time))
xscen <- cfMake(model, data=UScrime, nscen=7)
# Estimate Linear regression model
lm1.res <- lm(model, data = UScrime)
lm1.pe <- lm1.res$coefficients        # point estimates
lm1.vc <- vcov(lm1.res)               # var-cov matrix
simbetas.lm <- mvrnorm(10, lm1.pe, lm1.vc)
# for comparison with new linearsimev
linsimev.ref <- linearsimev(xscen, simbetas.lm)

## test modelMatrixHelper with example matrices
test = modelMatrixHelper(x=xscen, b=simbetas.lm, constant=1, nscen=7)
test = modelMatrixHelper(x=xscen$x, b=simbetas.lm, constant=1, nscen=7)
test = modelMatrixHelper(x=xscen, b=simbetas.lm, constant=1, nscen=7, which.cf="xpre")
test = modelMatrixHelper(x=xscen$xpre, b=simbetas.lm, constant=1, nscen=7)

## test new linearsimev doesn't break example
source("linearsimev.R")
test <- linearsimev(xscen, simbetas.lm)
test <- linearsimev(xscen$x, formula=xscen$model, b=simbetas.lm)
identical(test, linsimev.ref)

# from cfMake help
xscen <- cfName(xscen, "Pr(Prison) +0.5 sd", scen=1)
xscen <- cfChange(xscen, "Prob", x = mean(UScrime$Prob) + 0.5*sd(UScrime$Prob), scen=1)
linsimfd.ref <- linearsimfd(xscen, simbetas.lm)
## test new linearsimfd  doesn't break example
source("linearsimfd.R")
test <- linearsimfd(xscen, simbetas.lm)
identical(test, linsimfd.ref)

#############################

# test calling with formula
model <- (y ~ log(M) + So)
# Estimate Linear regression model
lm1.res <- lm(model, data = UScrime)
lm1.pe <- lm1.res$coefficients        # point estimates
lm1.vc <- vcov(lm1.res)               # var-cov matrix
simbetas.lm <- mvrnorm(10, lm1.pe, lm1.vc)
# for comparison with new linearsimev
xscen <- cfMake(model, data=UScrime, nscen=7)
linsimev.ref <- linearsimev(xscen, simbetas.lm)
test <- linearsimev(xscen$x, formula=xscen$model, b=simbetas.lm)
identical(test, linsimev.ref)

## test new linearsimev
source("linearsimev.R")
M = xscen$x$M; So = xscen$x$So; y = xscen$x$y
test <- linearsimev(formula=model, b = simbetas.lm)
x = data.frame(M=M, y=y)
test <- linearsimev(x=x, formula=model, b=simbetas.lm)
identical(test, linsimev.ref)

