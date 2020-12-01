# Paper: Topological impact of negative links on the stability of 
#        resting-state brain networks
# Usage: signed.links.r
# Date: 11/20/2020
# Author: Majid Saberi
# Description: This function receives a signed connectivity matrix and returns
#              total numebr of links, number of positive links, number of negative,
#              percentage of positive links, percentage of negative links,
#              and positive to negative link ratio.
#              Input matrix may be thresholded and contain NA values.

signed.links <- function(scm){ # scm is a input signed connectivity matrix
  #total signed links
  tot_link <- sum(!is.na(scm[upper.tri(scm)]))
  #total positive links
  pos_link <- sum(scm[upper.tri(scm)] == 1, na.rm = T)
  #total negative links
  neg_link <- sum(scm[upper.tri(scm)] == -1, na.rm = T)
  #percentage of positive links
  perc_pos <- poslink / totlink
  #percentage of negative links
  perc_neg <- neglink / totlink
  #positive to negative link ratio
  pos_neg_ratio <- poslink / neglink 
  output <- round(c(tot_link, pos_link, neg_link, perc_pos, perc_neg, pos_neg_ratio), 3)
  names(output) <- c("tot_links", "pos_links", "neg_links", "perc_pos_links", "perc_neg_links", "pos_neg_link_ratio")
  return(output)
}
