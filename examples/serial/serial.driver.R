# Solves a serial MaxEnt problem
# Provide:
# 1) input file with single step candidates (normal input file for Tableau)
# 2) target probabilities (not necessarily single step -- input, output, probability)
# Outputs:
# 1) distribution from inputs to final outputs
# 2) SSE compared to target probabilities

# Author: Robert Staubs
# Last update: 4/2/2013

# solver source location
# FIXME Set your directory for these files!
source("serial/serial.solver.R")

# read from input file
tab <- read.table(head = T, sep = "\t", "examples/serial/data/french.txt")

# target probabilities
target <- read.table(head = T, sep = "\t", "examples/serial/data/french.probs.txt")

# no hidden structure
offset <- 3

constraints <- dim(tab)[2] - offset

w <- rep(0, constraints)

# make violations negative
tab[, -(1:offset)] <- -tab[, -(1:offset)]

# create a Tableau object (can change hidden to FALSE if no hidden structure)
# batch only works with MaxEnt
o.tab <- new("Tableau", data = tab, hidden = FALSE, theory = "maxent")

# solve with KL minimization and L2 prior with variance 100
obj <- l2.prior(serial.kl.objective(o.tab, target), var = 100)
opt <- optim(w, obj, method = "L-BFGS-B", lower = rep(0, constraints))

# resulting distribution and sum squared error from target
serial.distribution(o.tab, opt$par)
serial.sse(o.tab, opt$par, target)
