# Provides probability calculation and objective functions for serial MaxEnt
# Requires: solver

# Author: Robert Staubs
# Last update: 4/2/2013

source("base/solver.R")

# calculates probability distribution over forms
serial.distribution <- function(o.tab, w) {

  # all intermediate forms and their single step probabilities
  ord <- unique(o.tab@data$output)
  probs <- distribution(o.tab, w)

  s.mat <- c()

  # build transition probability matrix
  for(origin in 1:length(ord)) {

    for(destination in 1:length(ord)) {
      i <- ord[origin]
      j <- ord[destination]
      pr <- probs$probability[probs$input == i & probs$output == j]
      if(length(pr) == 0) { 
        pr <- 0
      }

      s.mat <- c(s.mat, pr)
    }
  }

  s.mat <- matrix(s.mat, ncol=length(ord), nrow=length(ord), byrow=TRUE)

  # absorbing state matrix (convergence on self-loop)
  t.mat <- diag(diag(s.mat))

  # non-absorbing state matrix
  s.mat <- s.mat - t.mat

  # identity matrix
  i.mat <- diag(length(ord))

  # invert difference
  q.mat <- base::solve(i.mat - s.mat)

  r.mat <- q.mat %*% t.mat

  result <- data.frame(input=rep(ord,1,each=length(ord)), output=rep(ord,length(ord)), probability=as.vector(t(r.mat)))

  result
}

# sum squared error between predicted and empirical distributions
serial.sse <- function(o.tab, w, target) {

  dist <- serial.distribution(o.tab, w)

  sse <- sum(by(target, 1:nrow(target), function(row) {
    q <- dist[dist$input==as.character(row$input) & dist$output==as.character(row$output),]$probability
    if(length(q)==0) { q <- 0 }
    p <- row$probability
    (p-q)^2
  }))

  sse
}


# kl divergence between predicted and empirical distributions
serial.kl <- function(o.tab, w, target) {

  dist <- serial.distribution(o.tab, w)

  kl <- sum(by(target, 1:nrow(target), function(row) {
    q <- dist[dist$input==as.character(row$input) & dist$output==as.character(row$output),]$probability
    if(length(q)==0) { q <- 0 }
    p <- row$probability
    if(p != 0) { 
      p * log(p / q) 
    }
    else {
      0
    }
  }))

  kl
}

serial.kl.objective <- function(o.tab, target) {
  obj <- function(w) {
    serial.kl(o.tab, w, target)
  }

  obj
}

serial.sse.objective <- function(o.tab, target) {
  obj <- function(w) {
    serial.sse(o.tab, w, target)
  }

  obj
}
