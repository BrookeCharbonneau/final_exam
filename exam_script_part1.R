library(tidyverse)
library(pwr)


#Assume R2 is .20 - can't do safeguard because we don't have a sample size
## power for regression with R2=.20 with 3 predictors
f2 <- .20/(1-.20)
f2

pwr.f2.test(u=2,f2=f2, power=.85) #u=3 since 2 predictors, v from r data
# N = u + v + 1
N = 2 + 44 + 1
#want 47 people in our study


#Incremental prediction
## Assume sr2 = .10, assume R2 = .20
## f2 <- sr2 / (1-R2)

f2 <- .10 / (1-.20)
f2
pwr.f2.test(u=1,f2=f2, power=.85) #u=1 since increment of 1 predictor IN THIS LINE
N = 2 + 72 + 1  #u is now 3 because there are 2 predictors IN TOTAL
#want 75 people in our study

