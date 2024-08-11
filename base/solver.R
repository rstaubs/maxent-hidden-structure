# Provides objective functions and solving for MaxEnt
# Requires: tableau

# Authors: David Smith, Robert Staubs
# Contact: rstaubs@linguist.umass.edu)
# Last update: 3/11/2013

source("base/tableau.R")

# Solve for weights for data in tableau
#
# tableau      Tableau object containing problem
# weights      starting weight vector
# categorical  is the target distribution categorical?
# method       optimization method to use (see ?optim)
# reg          regularization to use (L1 or L2)
# mean         mean for regularization
# var          variance for L2 regularization
# alpha        parameter for L1 regularization
# mf           is a Markedness > Faithfulness used
# markedness   boolean vector identifying Markedness constraints
# faithfulness boolean vector identifying Faithfulness constraints
# mf.weight    weight of M > F term
# gradient     should the provided gradient be used? (FALSE generally means a numerical estimate is used)
# lower.bound  vector of lower bounds on constraint weights
# upper.bound  vector of upper bounds on constraint weights
solve <-
  function(tableau, weights, categorical = "FALSE", method = "L-BFGS-B", reg = "L2", mean = 0, var = 1, alpha = .01, mf = FALSE, markedness = 0, faithfulness = 0, mf.weight = 1, gradient = TRUE, lower.bound = 0, upper.bound = Inf, ...) {
    w <- weights

    if (categorical) {
      obj <- categorical.objective(tableau)
    } else {
      obj <- variable.objective(tableau)
    }

    if (reg == "L1") {
      obj <- l1.prior(obj, mean = mean, alpha = alpha)
    } else if (reg == "L2") {
      obj <- l2.prior(obj, mean = mean, var = var)
    }

    if (mf) {
      obj <- difference.bias(obj, markedness = markedness, faithfulness = faithfulness, mf.weight = mf.weight)
    }


    if (gradient) {
      grad.optim(weights, obj, lower = lower.bound, upper = upper.bound, method = method, ...)
    } else {
      optim(weights, obj, lower = lower.bound, upper = upper.bound, method = method, ...)
    }
  }

# OBJECTIVE FUNCTIONS

# Objective function for categorical problems
categorical.objective <- function(tableau) {
  if (tableau@theory == "MaxEnt") {
    maxent.categorical.objective(tableau)
  } else {
    warning("Objective functions not defined for theories other than Maximum Entropy.")
  }
}

# Objective function for non-categorical problems
variable.objective <- function(tableau) {
  if (tableau@theory == "MaxEnt") {
    maxent.variable.objective(tableau)
  } else {
    warning("Objective functions not defined for theories other than Maximum Entropy.")
  }
}

# Objective function for Maximum Entropy Grammar
# (Categorical using maximum likelihood)
maxent.categorical.objective <- function(tableau) {
  # constraint violations
  if (tableau@hidden) {
    violations <- as.matrix(tableau@data[, -(1:4)])
  } else {
    violations <- as.matrix(tableau@data[, -(1:3)])
  }

  obj <- function(w) {
    scores <- exp(c(violations %*% w))

    # maximum entropy calculation
    res <-
      -sum(by(
        data.frame(p = tableau@data$probability, scores = scores), tableau@data$input,
        function(form) {
          Z <- sum(form$scores)
          # num <- form$scores[form$p == 1]
          num <- sum(form$scores[form$p == 1])

          # sum(log(num) - log(Z))
          log(num) - log(Z)
        }
      ))
    grad(res) <- -colSums(do.call(
      rbind,
      by(tableau@data, tableau@data$input, function(form) {
        if (tableau@hidden) {
          features <- as.matrix(form[, -(1:4)])
        } else {
          features <- as.matrix(form[, -(1:3)])
        }

        scores <- exp(c(features %*% w))
        den.p <- scores / sum(scores)
        num.p <- scores[form$probability == 1] / sum(scores[form$probability == 1])
        den <- colSums(den.p * features)
        num <- colSums(num.p * features[form$probability == 1, , drop = FALSE])
        num - den
      })
    ))

    res
  }

  obj
}

# Objective function for Maximum Entropy Grammar
# (Variable using minimization of Kullback-Leibler divergence)
maxent.variable.objective <- function(tableau) {
  # constraint violations
  if (tableau@hidden) {
    violations <- as.matrix(tableau@data[, -(1:4)])
  } else {
    violations <- as.matrix(tableau@data[, -(1:3)])
  }

  obj <- function(w) {
    scores <- exp(c(violations %*% w))

    # K-L divergence calculation
    res <-
      # sum over inputs
      -sum(by(
        data.frame(y = tableau@data$output, q = tableau@data$probability, scores = scores), tableau@data$input,
        function(form) {
          Z <- sum(form$scores)

          # sum over outputs
          sum(by(
            form, form$y[, drop = TRUE],
            function(output) {
              q.target <- output$q[1]
              num <- sum(output$scores)

              p.weights <- sum(log(num) - log(Z))

              if (q.target == 0.0) {
                0.0
              } else {
                q.target * (-log(q.target) + p.weights)
              }
            }
          ))
        }
      ))

    grad(res) <- -colSums(do.call(
      rbind,
      by(
        tableau@data, tableau@data$input,
        # sum over inputs
        function(form) {
          if (tableau@hidden) {
            features <- as.matrix(form[, -(1:4)])
          } else {
            features <- as.matrix(form[, -(1:3)])
          }

          scores <- exp(c(features %*% w))
          den.p <- scores / sum(scores)
          den <- colSums(den.p * features)

          colSums(do.call(
            rbind,
            by(
              data.frame(p = form$probability, scores = scores, features = features), form$output[, drop = TRUE],
              # sum over all output forms
              function(output) {
                p <- output$p[1]
                feat <- as.matrix(output[, -(1:2)])

                num.p <- output$scores / sum(output$scores)

                num <- colSums(num.p * feat)

                p * (num - den)
              }
            )
          ))
        }
      )
    ))

    res
  }

  obj
}


# REGULARIZATION

# Puts an L1 prior on an optimization
l1.prior <- function(fun, alpha = 1, mean = 0, ...) {
  fun <- match.fun(fun)
  function(pv, ...) {
    res <- fun(pv, ...) + sum(alpha * abs(pv - mean))

    if (!is.null(grad(res))) grad(res) <- grad(res) + alpha

    res
  }
}

# Puts an L2 (log-space Gaussian) prior on an optimization
l2.prior <- function(fun, var = 1, mean = 0, ...) {
  fun <- match.fun(fun)
  function(pv) {
    res <- fun(pv) + sum((pv - mean) / sqrt(2 * var))^2


    if (!is.null(grad(res))) grad(res) <- grad(res) + ((pv - mean) / var)

    res
  }
}

# Adds a term for Markedness over Faithfulness
# Sum(M) - Sum(F) is maximized
# Needs a vector of booleans stating whether something
# is Markedness or Faithfulness
difference.bias <- function(fun, markedness, faithfulness, mf.weight = 1) {
  fun <- match.fun(fun)

  function(pv, ...) {
    faith <- faithfulness * pv
    mark <- markedness * pv

    diff <- faith - mark

    res <- fun(pv, ...) + mf.weight * sum(diff)

    if (!is.null(grad(res))) grad(res) <- grad(res) + mf.weight * (faithfulness - markedness)

    res
  }
}

# HELPER FUNCTIONS

# Computes an "information theoretic" logarithm
# That is, logarithm with 0 log 0 = 0
# Helper function for K-L divergence minimization
safe.log <- function(x) {
  sapply(
    x,
    function(y) {
      minval <- 1E-320
      if (is.nan(y)) {
        0
      } else if (y < minval) {
        log(minval)
      } else {
        log(y)
      }
    }
  )
}

grad <- function(x) attr(x, "gradient")

"grad<-" <- function(x, value) {
  attr(x, "gradient") <- value
  x
}

cache.grad <- function(fun) {
  fun <- match.fun(fun)
  saved.gradient <- numeric(0)
  obj <- function(...) {
    res <- fun(...)
    saved.gradient <<- grad(res)
    as.numeric(res)
  }
  grad(obj) <- function(pv) saved.gradient
  obj
}

grad.optim <- function(par, fun, method = "L-BFGS-B", ...) {
  fun <- match.fun(fun)
  gfun <- cache.grad(fun)

  optim(par, gfun, grad(gfun), method = method, ...)
}
