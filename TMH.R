# Paper: Topological impact of negative links on the stability of 
#        resting-state brain networks
# Usage: TMH.r
# Date: 11/20/2020
# Author: Majid Saberi
# Description: This function receives a signed connectivity matrix and returns
#              TMH, positive TMH, negative TMH.
#              Input matrix may be thresholded and contain NA values.

TMH <- function(scm){ # scm is a input signed connectivity matrix
  # calculate degrees
  degree <- apply(scm, 1, function(x) sum(!is.na(x)) )
  posdegree <- apply(scm, 1, function(x) sum(x == 1, na.rm = T) )
  negdegree <- apply(scm, 1, function(x) sum(x == -1, na.rm = T) )
  # Tendency to Make Hub (TMH)
  TMH <- sum(degree ^ 2) / sum(degree) 
  # Tendency to Make negative Hub (negative TMH)
  negTMH <- sum(negdegree ^ 2) / sum(negdegree) 
  # Tendency to Make positive Hub (positive TMH)
  posTMH <- sum(posdegree ^ 2) / sum(posdegree) 
  output <- round(c(TMH, posTMH, negTMH), 3)
  names(output) <- c("TMH", "posTMH", "negTMH")
  return(output)
}
