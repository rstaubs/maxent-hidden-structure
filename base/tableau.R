# Provides objective functions and solving for MaxEnt
# Requires: tableau

# Authors: Robert Staubs
# Contact: rstaubs@linguist.umass.edu)
# Last update: 2/6/2012


# Class for storing tableau data and theory type
setClass(
  "Tableau",
  representation(
    data = "data.frame",
    hidden = "logical",
    theory = "character"
  )
)

setGeneric("setTheory<-", function(.Object, ...) standardGeneric("setTheory<-"))
setReplaceMethod(
  "setTheory",
  "Tableau",
  function(.Object, value) {
    th <- toupper(value)
    if (th == "HG" || th == "HARMONIC GRAMMAR") {
      slot(.Object, "theory") <- "HG"
    } else if (th == "ME" || th == "MAXENT" || th == "MAXIMUM ENTROPY") {
      slot(.Object, "theory") <- "MaxEnt"
    } else if (th == "NHG" || th == "NOISYHG" || th == "NOISY HG" || th == "NOISY HARMONIC GRAMMAR") {
      slot(.Object, "theory") <- "NoisyHG"
    } else {
      warning(paste("Cannot find theory", value, ", using HG."))
      slot(.Object, "theory") <- "HG"
    }

    return(.Object)
  }
)

# Construct a tableau from data frame and theory
setMethod(
  "initialize",
  "Tableau",
  function(.Object, data, hidden = logical(0), theory = character(0)) {
    .Object@data <- data

    if (!hidden) {
      .Object@data$probability <- normalize(data, hidden)$probability
    }

    setTheory(.Object) <- theory

    .Object@hidden <- hidden

    .Object
  }
)

# Produce data frame with output distribution given weights
setGeneric("distribution", function(.Object, ...) standardGeneric("distribution"))
setMethod(
  "distribution",
  "Tableau",
  function(.Object, w, ...) {
    if (.Object@theory == "HG") {
      overt.view(hg.distribution(.Object, w, ...), theory = .Object@theory, sum = TRUE)
    } else if (.Object@theory == "MaxEnt") {
      maxent.distribution(.Object, w, sum = TRUE, ...)
    } else if (.Object@theory == "NoisyHG") {
      overt.view(noisyhg.distribution(.Object, w, ...), theory = .Object@theory, sum = TRUE)
    }
  }
)

# Produce data frame with output and hidden structure distribution given weights
setGeneric("hidden.distribution", function(.Object, ...) standardGeneric("hidden.distribution"))
setMethod(
  "hidden.distribution",
  "Tableau",
  function(.Object, w, ...) {
    if (.Object@theory == "HG") {
      hg.distribution(.Object, w, ...)
    } else if (.Object@theory == "MaxEnt") {
      maxent.distribution(.Object, w, sum = FALSE, ...)
    } else if (.Object@theory == "NoisyHG") {
      noisyhg.distribution(.Object, w, ...)
    }
  }
)

# Takes data frame of input, output, hidden, and probability
# Yields data frame of input, output, and probability
overt.view <- function(data, theory, sum = FALSE) {
  res <- list()
  inputs <- unique(data$input)
  for (i in 1:length(inputs)) {
    if (sum == FALSE) {
      dat <- data[data$input == inputs[i], ]
      dat <- dat[!duplicated(dat$output), ]
      res <- rbind(res, dat[, c(1, 2, 4)])
    } else {
      outputs <- unique(data$output[data$input == inputs[i]])
      for (j in 1:length(outputs)) {
        dd <- data[data$input == inputs[i] & data$output == outputs[j], ]
        if (theory == "HG") {
          p.sum <- max(dd$probability)
        } else {
          p.sum <- sum(dd$probability)
        }
        res <- rbind(res, list(input = as.character(inputs[i]), output = as.character(outputs[j]), probability = p.sum))
      }
    }
  }
  data.frame(res)
}

# Produce data frame with observed distribution over outputs in tableau
setGeneric("observed", function(.Object) standardGeneric("observed"))
setMethod(
  "observed",
  "Tableau",
  function(.Object) {
    if (.Object@hidden) {
      overt.view(.Object@data, theory = .Object@theory, sum = FALSE)
    } else {
      .Object@data[1:3]
    }
  }
)

# Produce data frame with error over outputs in tableau
setGeneric("error", function(.Object, ...) standardGeneric("error"))
setMethod(
  "error",
  "Tableau",
  function(.Object, w, ...) {
    expected <- distribution(.Object, w, ...)
    observed <- observed(.Object)

    data.frame(observed[1:2], probability = observed$probability - expected$probability)
  }
)

# Sum squared error over outputs in tableau
setGeneric("sse", function(.Object, ...) standardGeneric("sse"))
setMethod(
  "sse",
  "Tableau",
  function(.Object, w, ...) {
    sum(error(.Object, w, ...)$probability^2)
  }
)

# Produce data frame with error over outputs in tableau
# Takes the output distribution as categorical -- thus error is 0 or total for each input
setGeneric("categorical.error", function(.Object, ...) standardGeneric("categorical.error"))
setMethod(
  "categorical.error",
  "Tableau",
  function(.Object, w, ...) {
    expected <- overt.view(hg.distribution(.Object, w, ...), theory = .Object@theory, sum = TRUE)
    observed <- observed(.Object)

    data.frame(observed[1:2], probability = observed$probability - as.numeric(expected$probability))
  }
)

# Sum squared error over outputs in tableau
# Takes the output distribution as categorical -- thus error is 0 or total for each input
setGeneric("categorical.sse", function(.Object, ...) standardGeneric("categorical.sse"))
setMethod(
  "categorical.sse",
  "Tableau",
  function(.Object, w, ...) {
    sum(categorical.error(.Object, w, ...)$probability^2)
  }
)

# Produce input-output pair (or input-output-hidden triple) given weights
setGeneric("sample.pair", function(.Object, ...) standardGeneric("sample.pair"))
setMethod(
  "sample.pair",
  "Tableau",
  function(.Object, w, distribution = numeric(0), hidden = FALSE, ...) {
    if (length(distribution) == 0) {
      inputs <- unique(.Object@data$input)
      dist <- data.frame(input = inputs, probability = rep(1.0 / length(inputs), length(inputs)))
    } else {
      dist <- distribution
    }

    # sample an input according to distribution
    sample.input <- input.sample(.Object, dist)

    sample.list <- conditional.sample(.Object, w, sample.input, hidden = hidden...)
    sample.output <- sample.list$output

    if (hidden == FALSE || !.Object@hidden) {
      list(input = as.character(sample.input), output = as.character(sample.output))
    } else {
      list(input = as.character(sample.input), output = as.character(sample.output), hidden = as.character(sample.list$hidden))
    }
  }
)

# Produce a sample output given the input and weights
setGeneric("conditional.sample", function(.Object, ...) standardGeneric("conditional.sample"))
setMethod(
  "conditional.sample",
  "Tableau",
  function(.Object, w, input, specified.output = character(0), hidden = FALSE, ...) {
    if (.Object@theory == "HG") {
      hg.conditional.sample(.Object, w, input, specified.output, hidden, ...)
    } else if (.Object@theory == "MaxEnt") {
      maxent.conditional.sample(.Object, w, input, specified.output, hidden, ...)
    } else if (.Object@theory == "NoisyHG") {
      noisyhg.conditional.sample(.Object, w, input, specified.output, hidden, ...)
    }
  }
)


# Produce data frame with HG output distribution given weights
setGeneric("hg.distribution", function(.Object, ...) standardGeneric("hg.distribution"))
setMethod(
  "hg.distribution",
  "Tableau",
  function(.Object, w) {
    data <- .Object@data

    dist <- data.frame()

    inputs <- unique(.Object@data$input)
    for (i in 1:length(inputs)) {
      this.input <- inputs[i]

      # section of tableaux for inputs and outputs
      if (!.Object@hidden) {
        this.data <- data[data$input == this.input, 1:2]
      } else {
        # use one extra column for hidden structure
        this.data <- data[data$input == this.input, 1:3]
      }
      this.data <- cbind(this.data, probability = rep(0, length(this.data$output)))

      this.sample <- hg.conditional.sample(.Object, w, this.input, ties = TRUE)
      if (!.Object@hidden) {
        this.data$probability[this.data$output == as.character(this.sample$output)] <- 1
      } else {
        this.data$probability[this.data$output == as.character(this.sample$output) & this.data$hidden == as.character(this.sample$hidden)] <- 1
      }

      dist <- rbind(dist, this.data)
    }

    dist
  }
)

# Produce data frame with MaxEnt output distribution given weights
setGeneric("maxent.distribution", function(.Object, ...) standardGeneric("maxent.distribution"))
setMethod(
  "maxent.distribution",
  "Tableau",
  function(.Object, w, sum = TRUE, ...) {
    data <- .Object@data

    # constraint violations
    if (.Object@hidden) {
      violations <- as.matrix(data[, -(1:4)])
    } else {
      violations <- as.matrix(data[, -(1:3)])
    }

    scores <- exp(c(violations %*% w))

    # probabilities over inputs
    e <- unlist(by(
      cbind(data, scores = scores), data$input[, drop = TRUE],
      function(form) {
        f.s <- form$scores

        input <- form$input
        output <- form$output
        if (.Object@hidden) {
          hidden <- form$hidden
        }

        uniq <- unique(form$output)
        # sum probabilities over hidden structures
        if (sum == TRUE && .Object@hidden) {
          uniq <- unique(form$output)

          f.s <- c()
          for (j in 1:length(uniq)) {
            f.s <- c(f.s, sum(form$scores[form$output == uniq[j]]))
          }

          f.s <- as.numeric(f.s)
        }

        # calculate probabilities
        Z <- sum(f.s)
        num <- f.s

        p <- num / Z

        inner <- c()
        if (sum == TRUE || !.Object@hidden) {
          for (i in 1:length(uniq)) {
            inner <- c(inner, as.character(unlist(form$input[1])), as.character(unlist(uniq[i])), as.character(p[i]))
          }
        } else {
          for (i in 1:length(form$output)) {
            inner <- c(inner, as.character(unlist(form$input[1])), as.character(unlist(form$output[i])), as.character(unlist(form$hidden[i])), as.character(p[i]))
          }
        }

        unlist(inner)
      }
    ))

    if (sum == TRUE) {
      rez <- data.frame(matrix(e, ncol = 3, byrow = TRUE))
      colnames(rez) <- c("input", "output", "probability")

      rez$probability <- as.numeric(as.character(rez$probability))

      rez[order(match(
        paste(rez$input, rez$output),
        paste(data$input, data$output)
      )), ]
    } else {
      rez <- data.frame(matrix(e, ncol = 4, byrow = TRUE))
      colnames(rez) <- c("input", "output", "hidden", "probability")

      rez$probability <- as.numeric(as.character(rez$probability))

      rez[order(match(
        paste(rez$input, rez$output, rez$hidden),
        paste(data$input, data$output, data$hidden)
      )), ]
    }
  }
)

# Produce data frame with NoisyHG output distribution given weights
setGeneric("noisyhg.distribution", function(.Object, ...) standardGeneric("noisyhg.distribution"))
setMethod(
  "noisyhg.distribution",
  "Tableau",
  function(.Object, w, samples = 1000, mean = 0, sd = 1, ...) {
    data <- .Object@data

    dist <- data.frame()

    inputs <- unique(.Object@data$input)
    iterat <- samples / length(inputs)

    for (i in 1:length(inputs)) {
      this.input <- inputs[i]

      # section of tableaux for inputs and outputs
      if (!.Object@hidden) {
        this.data <- data[data$input == this.input, 1:2]
      } else {
        # use one extra column for hidden structure
        this.data <- data[data$input == this.input, 1:3]
      }
      this.data <- cbind(this.data, probability = rep(0, length(this.data$output)))

      for (iter in 1:iterat) {
        this.sample <- noisyhg.conditional.sample(.Object, w, this.input, mean = mean, sd = sd, ...)
        this.output <- this.sample$output
        if (!.Object@hidden) {
          this.data$probability[which(this.data$output == this.output)] <- 1 + this.data$probability[which(this.data$output == this.output)]
        } else {
          this.data$probability[this.data$output == this.output & this.data$hidden == this.sample$hidden] <- 1 + this.data$probability[which(this.data$output == this.output & this.data$hidden == this.sample$hidden)]
        }
      }

      this.data$probability <- this.data$probability / (1.0 * iterat)
      dist <- rbind(dist, this.data)
    }

    dist
  }
)

# Produce an input according to sampling distribution
setGeneric("input.sample", function(.Object, ...) standardGeneric("input.sample"))
setMethod(
  "input.sample",
  "Tableau",
  function(.Object, distribution) {
    # sample an input according to distribution
    inputs <- unique(.Object@data$input)

    p <- runif(1)
    p.sum <- 0
    for (i in 1:length(inputs)) {
      p.sum <- p.sum + distribution$probability[distribution$input == inputs[i]]
      if (p.sum >= p) {
        sample.input <- inputs[i]
        break
      }
    }

    sample.input
  }
)

# Produce an output according to data distribution
setGeneric("output.sample", function(.Object, ...) standardGeneric("output.sample"))
setMethod(
  "output.sample",
  "Tableau",
  function(.Object, input) {
    obs <- observed(.Object)
    segment <- obs[obs$input == input, ]

    # sample an output according to distribution
    outputs <- unique(segment$output)

    p <- runif(1)
    p.sum <- 0
    for (i in 1:length(outputs)) {
      p.sum <- p.sum + segment$probability[segment$output == outputs[i]]
      if (p.sum >= p) {
        sample.output <- outputs[i]
        break
      }
    }

    sample.output
  }
)

# Produce input-output pair from data
setGeneric("data.sample", function(.Object, ...) standardGeneric("data.sample"))
setMethod(
  "data.sample",
  "Tableau",
  function(.Object, distribution) {
    if (length(distribution) == 0) {
      inputs <- unique(.Object@data$input)
      dist <- data.frame(input = inputs, probability = rep(1.0 / length(inputs), length(inputs)))
    } else {
      dist <- distribution
    }

    # sample an input according to distribution
    sample.input <- input.sample(.Object, dist)

    # find winner for that input
    sample.output <- output.sample(.Object, sample.input)

    list(input = as.character(sample.input), output = as.character(sample.output))
  }
)

# Produce the MaxEnt winning output given the input
setGeneric("maxent.conditional.sample", function(.Object, ...) standardGeneric("maxent.conditional.sample"))
setMethod(
  "maxent.conditional.sample",
  "Tableau",
  function(.Object, w, sample.input, specified.output = character(0)) {
    # get probability distribution over output forms
    output.distribution <- maxent.distribution(.Object, w)

    output.distribution <- output.distribution[output.distribution$input == sample.input, ]

    # if output specified, remove conflicting forms
    # (used for hidden structure)
    if (length(specified.output) != 0) {
      output.distribution <- output.distribution[output.distribution$output == specified.output, ]
    }

    # TODO get hidden structures selected given specified.output
    # currently don't get any hidden structures in for maxent

    outputs <- unique(.Object@data$output[.Object@data$input == sample.input])
    q <- runif(1)
    q.sum <- 0
    for (i in 1:length(outputs)) {
      q.sum <- q.sum + output.distribution$probability[output.distribution$output == outputs[i]]
      if (q.sum >= q) {
        sample.output <- outputs[i]
        break
      }
    }

    as.character(sample.output)
  }
)

# Produce the HG winning output given the input
setGeneric("hg.conditional.sample", function(.Object, ...) standardGeneric("hg.conditional.sample"))
setMethod(
  "hg.conditional.sample",
  "Tableau",
  function(.Object, w, sample.input, specified.output = character(0), ties = FALSE) {
    data <- .Object@data[.Object@data$input == sample.input, ]

    # if output specified, remove conflicting forms
    # (used for hidden structure)
    if (length(specified.output) != 0) {
      data <- data[data$output == specified.output, ]
    }

    # constraint violations and scores
    if (.Object@hidden) {
      violations <- as.matrix(data[, -(1:4)])
    } else {
      violations <- as.matrix(data[, -(1:3)])
    }
    scores <- c(violations %*% w)

    winners <- which(max(scores) == scores)


    if (ties == FALSE) {
      winners <- sample(winners, 1)
    }

    if (.Object@hidden) {
      list(output = data$output[winners], hidden = data$hidden[winners])
    } else {
      list(output = data$output[winners])
    }
  }
)

# Produce the interpretive parse for the given input-output pair
setGeneric("interpretive.parse", function(.Object, ...) standardGeneric("interpretive.parse"))
setMethod(
  "interpretive.parse",
  "Tableau",
  function(.Object, w, input, output) {
    hg.conditional.sample(.Object, w, input, output, FALSE)
  }
)

# Produce a sample NoisyHG output given the input
setGeneric("noisyhg.conditional.sample", function(.Object, ...) standardGeneric("noisyhg.conditional.sample"))
setMethod(
  "noisyhg.conditional.sample",
  "Tableau",
  function(.Object, w, sample.input, specified.output = character(0), mean = 0, sd = 1) {
    data <- .Object@data[.Object@data$input == sample.input, ]

    # if output specified, remove conflicting forms
    # (used for hidden structure)
    if (length(specified.output) != 0) {
      data <- data[data$output == specified.output, ]
    }

    # constraint violations and scores
    if (.Object@hidden) {
      violations <- as.matrix(data[, -(1:4)])
    } else {
      violations <- as.matrix(data[, -(1:3)])
    }
    scores <- c(violations %*% (w + rnorm(length(w), mean = mean, sd = sd)))

    winners <- which(max(scores) == scores)
    winners <- sample(winners, 1)

    if (.Object@hidden) {
      list(output = data$output[winners], hidden = data$hidden[winners])
    } else {
      list(output = data$output[winners])
    }
  }
)

# Normalizes a dataframe with tableau data by input (to make probability distributions)
normalize <- function(data, hidden) {
  e <- unlist(by(
    data, data$input,
    function(t) {
      pr <- t$probability / sum(t$probability)
      inner <- c()
      for (i in 1:length(t$output)) {
        if (!hidden) {
          inner <- c(inner, as.character(unlist(t$input[1])), as.character(unlist(t$output[i])), as.character(pr[i]))
        } else {
          inner <- c(inner, as.character(unlist(t$input[1])), as.character(unlist(t$output[i])), as.character(unlist(t$hidden[i])), as.character(pr[i]))
        }
      }
      unlist(inner)
    }
  ))
  if (hidden) {
    rez <- data.frame(matrix(e, ncol = 4, byrow = TRUE))
    colnames(rez) <- c("input", "output", "hidden", "probability")

    rez$probability <- as.numeric(as.character(rez$probability))

    rez[order(match(
      paste(rez$input, rez$output, rez$hidden),
      paste(data$input, data$output, data$hidden)
    )), ]
  } else {
    rez <- data.frame(matrix(e, ncol = 3, byrow = TRUE))
    colnames(rez) <- c("input", "output", "probability")

    rez$probability <- as.numeric(as.character(rez$probability))

    rez[order(match(
      paste(rez$input, rez$output),
      paste(data$input, data$output)
    )), ]
  }
}
