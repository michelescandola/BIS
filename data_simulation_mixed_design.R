## 28/03/2021
## by Michele Scandola
## Data simulation

library(mvtnorm)
library(clusterGeneration)

## NGroups = number of simulated groups (between-subjects)
## NTrials = number of trials each condition
## NWCond  = number of levels for within-subjects conds
## NSubj   = total number of subjects
NGroups <- 3
NWCond  <- 3
NSubj   <- 30
NTrials <- 30

## coefficients for fixed effects
betasH0 <- c( 1 , 0 , 0 , 0 , 0 , 0 , 0 ,  0 , 0)
betasH1 <- c( 1 , 0 , 0 , 0 , 0 , 0 , 0 ,0.5, 0)

## output list
output_H0 <- list()
output_H1 <- list()

zzz <- yyy <- NULL

for(iteration in 1:2000){
  ##################
  ## random generation of random effects for each participant
  ##################
  stddev <- runif(9, min = 0.1, max = 0.6)
  # print( iteration )
  corMat <- rcorrmatrix(9)

  covMat <- stddev %*% t(stddev) * corMat
  random.effects <- rmvnorm(NSubj, mean = c( 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ), sigma = covMat)

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

  ## group is between-subjects, we must be sure that subjects are not in more than one group
  data.sim$Group <- NA

  gg <- rep(1:3,each=NSubj/NGroups)

  for( i in 1:length(levels(data.sim$ID))){
    data.sim$Group[data.sim$ID==as.character(i)] <- gg[i]
  }
  data.sim$Group <- factor( data.sim$Group )

  contrasts( data.sim$Group ) <- contr.sum( n = 3 )
  contrasts( data.sim$WCond ) <- contr.sum( n = 3 )

  ##################
  ## d.v. generation
  ##################
  yH0 <- yH1 <- rep( times = nrow(data.sim) , NA )

  for( i in 1:length( levels( data.sim$ID ) ) ){

    sel <- which( data.sim$ID == as.character(i) )

    mm  <- model.matrix(~ Group * WCond , data = data.sim[ sel , ] )

    yH0[sel] <- mm %*% as.matrix(coefsH0[ i , ]) + rnorm( n = NTrials * NWCond )

    yH1[sel] <- mm %*% as.matrix(coefsH1[ i , ]) + rnorm( n = NTrials * NWCond )

  }

  aa <- summary(
    aov(yH1 ~ Group * WCond +
          Error(ID / WCond ),
        data = data.sim  )
  )

  bb <- summary(
    aov(yH0 ~ Group * WCond +
          Error(ID / WCond ),
        data = data.sim  )
  )

  zzz <- c(zzz, aa$`Error: ID:WCond`[[1]][2,5] < 0.05)
  yyy <- c(yyy, bb$`Error: ID:WCond`[[1]][2,5] < 0.05)

  output_H0[[iteration]] <- yH0
  output_H1[[iteration]] <- yH1

}

mean(zzz)# 0.8155
mean(yyy)# 0.0765

dataH0 <- dataH1 <- data.sim
dataH0 <- cbind(dataH0, do.call("cbind", output_H0) )
dataH1 <- cbind(dataH1, do.call("cbind", output_H1) )

write.csv2(dataH0, file = "mixed-design-H0.csv")
write.csv2(dataH1, file = "mixed-design-H1.csv")
