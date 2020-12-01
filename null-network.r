# Paper: Topological impact of negative links on the stability of 
#        resting-state brain networks
# Usage: balance.metrics.r
# Date: 11/20/2020
# Author: Majid Saberi
# Description: This function receives a set of actual signals and 
#              returns a null-network corresponded to the actual network.
#              The output network has a negative to positive link ratio
#              similar to the actual network.

#act: matrix of actual signals
#thr: desired difference between link ratio of actual and null-network
#coef_min: minimum for adjusting coefficient 
#coef_min: maximum for adjusting coefficient 

nullnetwork <- function(act, thr, coef_min, coef_max){
  node <- dim(act)[1]               #signal number
  length <- dim(act)[2]             #length of signals
  #construct connectivity matrix 
  conn_act <- cor(t(act))
  #build random signal for adjusting with mean and sd equal to actual signals 
  rand_sig <- rnorm(length, mean(act), sd(act))
  err = thr + 1                     #err of link ratio
  #find a null-network with link ratio similar to actual network
  while (err > thr) {               #compare error and link ratio threshold 
    #shuffle actual signals
    shuf <- matrix(act[sample(length * node)], nrow = node, ncol = length)
    #select a random adjusting coefficient  
    coef <- runif(1, coef_min, coef_max)
    #make output signals
    out_sig <- t(t(shuf) + coef * rand_sig)
    #create connectivty matrix of output signals
    conn_out <- cor(t(out_sig))
    #calculate link ratio of actual and null connectivity matrices
    p2n_act <- sum(conn_act[upper.tri(conn_act)] > 0)/sum( conn_act[upper.tri(conn_act)] < 0)
    p2n_out <- sum(conn_out[upper.tri(conn_out)] > 0)/sum( conn_out[upper.tri(conn_out)] < 0)
    #calculate difference of link ratios
    err <- abs(p2n_act - p2n_out)
  }
  #sign the output connectivity
  output <- conn_out
  output[output > 0] <- 1
  output[output < 0] <- -1
  #return calculated null-network
  return(output)
}

