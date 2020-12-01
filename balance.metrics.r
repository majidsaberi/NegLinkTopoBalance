# Paper: Topological impact of negative links on the stability of 
#        resting-state brain networks
# Usage: balance.metrics.r
# Date: 11/20/2020
# Author: Majid Saberi
# Description: This function receives a signed connectivity matrix and returns
#              number of balanced triads, number of imbalanced triads
#              , and balance-energy of the signed network. 
#              Input matrix may be thresholded and contain NA values.

balance.metrics <- function(scm){  # scm is a input signed connectivity matrix
  # detect triad-type
  dm <- dim(scm)[1]
  triad <- array(dim = c(dm, dm, dm))
  for(i in 1:(dm-2)){
    for(j in (i+1):(dm-1)){
      for(k in (j+1):dm){
        triad[i, j, k] <- scm[i, j] + scm[i, k] + scm[j, k]
      }
    }
  }
  # number of balanced triads
  balanced <- sum(triad == +3 | triad == -1, na.rm = T)
  # number of imbalanced triads
  imbalanced <- sum(triad == -3 | triad == +1, na.rm = T)
  # balance-energy
  energy <- (imbalanced - balanced) / sum(imbalanced + balanced)
  # return the output
  output <- round(c(balanced, imbalanced, energy), 3)
  names(output) <- c("balanced", "imbalanced", "balance-energy")
  return(output)
}
