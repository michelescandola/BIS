## 28/03/2021
## by Michele Scandola
## Data simulation

library(mvtnorm)
library(clusterGeneration)

## NGroups = number of simulated groups (between-subjects)
## NTrials = number of trials each condition
## NWCond  = number of levels for within-subjects conds
## NSubj   = total number of subjects
NWCond  <- 2
NSubj   <- 30
NTrials <- 30

## coefficients for fixed effects
betasH0 <- c( 1 , 0.0 )
betasH1 <- c( 1 , 0.21 )

## output list
output_H0 <- list()
output_H1 <- list()

zzz <- yyy <- NULL

for(iteration in 1:2000){
  ##################
  ## random generation of random effects for each participant
  ##################
  stddev <- runif(2, min = 0.1, max = 0.6)
  # print( iteration )
  corMat <- rcorrmatrix(2)

  covMat <- stddev %*% t(stddev) * corMat
  random.effects <- rmvnorm(NSubj, mean = c( 0 , 0 ), sigma = covMat)

  ##################
  ## adding random effects to fixed effects
  ##################
  coefsH0 <- coefsH1 <- matrix( NA , ncol = ncol(random.effects) , nrow = nrow(random.effects))
  for( i in 1:ncol(random.effects) ){
    coefsH0[ , i ] <- random.effects[ , i] + betasH0[ i ]
    coefsH1[ , i ] <- random.effects[ , i] + betasH1[ i ]
  }

  ##################
  ## matrix design
  ##################
  data.sim <- expand.grid(
    trial      = 1:NTrials,
    ID         = factor(1:NSubj),
    WCond      = factor(1:NWCond)
  )

  ##################
  ## d.v. generation
  ##################
  yH0 <- yH1 <- rep( times = nrow(data.sim) , NA )

  for( i in 1:length( levels( data.sim$ID ) ) ){

    sel <- which( data.sim$ID == as.character(i) )

    mm  <- model.matrix(~ WCond , data = data.sim[ sel , ] )

    yH0[sel] <- mm %*% as.matrix(coefsH0[ i , ]) + rnorm( n = NTrials/NWCond )

    yH1[sel] <- mm %*% as.matrix(coefsH1[ i , ]) + rnorm( n = NTrials/NWCond )

  }

  dd <- aggregate( yH1 ~ WCond * ID, data = data.sim, FUN = mean)

  aa <- t.test( yH1 ~ WCond, data = dd , paired = TRUE )

  dd <- aggregate( yH0 ~ WCond * ID, data = data.sim, FUN = mean)

  bb <- t.test( yH0 ~ WCond, data = dd , paired = TRUE )

  zzz <- c(zzz, aa$p.value < 0.05)
  yyy <- c(yyy, bb$p.value < 0.05)

  output_H0[[iteration]] <- yH0
  output_H1[[iteration]] <- yH1

}

mean(zzz)# 0.804
mean(yyy)# 0.048

dataH0 <- dataH1 <- data.sim
dataH0 <- cbind(dataH0, do.call("cbind", output_H0) )
dataH1 <- cbind(dataH1, do.call("cbind", output_H1) )

write.csv2(dataH0, file = "paired-t-test-H0.csv")
write.csv2(dataH1, file = "paired-t-test-H1.csv")
