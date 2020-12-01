# Paper: Topological impact of negative links on the stability of 
#        resting-state brain networks
# Usage: signed.conn.r
# Date: 11/20/2020
# Author: Majid Saberi
# Description: This function receives a set of brain regional activations and returns
#              a signed connectivity matrix for them. 

signed.conn <- function(braact){ # braact is a set of brain regional activations
  # calculate functional connectivity matrix
  conn <- cor(braact, use = "na.or.complete")
  # binorize functional connections to make signed connectivity matrix
  conn[conn > 0] <- 1
  conn[conn < 0] <- -1
  return(conn)
}
