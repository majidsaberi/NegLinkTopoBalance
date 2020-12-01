# Paper: Topological impact of negative links on the stability of 
#        resting-state brain networks
# Usage: degree.seq.r
# Date: 11/20/2020
# Author: Majid Saberi
# Description: This function receives a signed connectivity matrix and returns
#              degree sequence, positive degree sequence, and negative degree sequence. 
#              Input matrix may be thresholded and contain NA values.

degree.seq <- function(scm){ # scm is a input signed connectivity matrix
  #degree sequence of the network
  degree <- apply(scm, 2, function(x) sum(!is.na(x)))
  #positive degree sequence of the network
  pos_degree <- apply(scm, 2, function(x) sum(x == 1, na.rm = T))
  #negative degree sequence of the network
  neg_degree <- apply(scm, 2, function(x) sum(x == -1, na.rm = T))
  output <- cbind(degree, pos_degree, neg_degree)
  colnames(output) <- c("degree","pos_degree","neg_degree")
  return(output)
}
