## 28/03/2021
## by Michele Scandola
## Data simulation

library(mvtnorm)
library(clusterGeneration)

## NGroups = number of simulated groups (between-subjects)
## NTrials = number of trials each condition
## NWCond  = number of levels for within-subjects conds
## NSubj   = total number of subjects
NGroups <- 2
NSubj   <- 30
NTrials <- 30

## coefficients for fixed effects
betasH0 <- c( 1 , 0.0 )
betasH1 <- c( 1 , 0.1 )

## output list
output_H0 <- list()
output_H1 <- list()

zzz <- yyy <- NULL

for(iteration in 1:2000){
  ##################
  ## matrix design
  ##################
  data.sim <- expand.grid(
    trial      = 1:NTrials,
    ID         = factor(1:NSubj)
  )

  ## group is between-subjects, we must be sure that subjects are not in more than one group
  data.sim$Group <- NA

  gg <- rep(1:2,each=NSubj/NGroups)

  for( i in 1:length(levels(data.sim$ID))){
    data.sim$Group[data.sim$ID==as.character(i)] <- gg[i]
  }
  data.sim$Group <- factor( data.sim$Group )

  contrasts( data.sim$Group ) <- contr.sum( n = 2 )

  ##################
  ## d.v. generation
  ##################
  yH0 <- yH1 <- rep( times = nrow(data.sim) , NA )

  for( i in 1:length( levels( data.sim$ID ) ) ){

    sel <- which( data.sim$ID == as.character(i) )

    mm  <- model.matrix(~ Group , data = data.sim[ sel , ] )

    yH0[sel] <- mm %*% as.matrix(betasH0) + rnorm( n = NTrials )

    yH1[sel] <- mm %*% as.matrix(betasH1) + rnorm( n = NTrials )

  }

  dd <- aggregate( yH1 ~ Group * ID, data = data.sim, FUN = mean)

  aa <- t.test( yH1 ~ Group, data = dd , paired = FALSE )

  dd <- aggregate( yH0 ~ Group * ID, data = data.sim, FUN = mean)

  bb <- t.test( yH0 ~ Group, data = dd , paired = FALSE )

  zzz <- c(zzz, aa$p.value < 0.05)
  yyy <- c(yyy, bb$p.value < 0.05)

  output_H0[[iteration]] <- yH0
  output_H1[[iteration]] <- yH1

}

mean(zzz)# 0.8315
mean(yyy)# 0.052

dataH0 <- dataH1 <- data.sim
dataH0 <- cbind(dataH0, do.call("cbind", output_H0) )
dataH1 <- cbind(dataH1, do.call("cbind", output_H1) )

write.csv2(dataH0, file = "unpaired-t-test-H0.csv")
write.csv2(dataH1, file = "unpaired-t-test-H1.csv")
