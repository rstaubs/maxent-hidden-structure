# solver source location
source("base/solver.R")

# read from input file
tab <- read.table(head = T, sep = "\t", "examples/base/data/manual-hidden.txt")

# hidden structure
offset <- 4
# no hidden structure
# offset <- 3

constraints <- dim(tab)[2] - offset

w <- rep(1, constraints)

# make violations negative
tab[, -(1:offset)] <- -tab[, -(1:offset)]

# create a Tableau object (can change hidden to FALSE if no hidden structure)
# batch only works with MaxEnt
o.tab <- new("Tableau", data = tab, hidden = TRUE, theory = "maxent")

# solve a problem for a gradient distribution w/ L2 regularization
opt <- solve(o.tab, w, categorical = TRUE, method = "L-BFGS-B", reg = "L2", mf = TRUE, markedness = c(TRUE, FALSE, FALSE), faithfulness = c(FALSE, FALSE, TRUE), mf.weight = 10)

# relevant other options:
# mean, var  -- parameters for regularization
# alpha -- L1 weight
# markedness, faithfulness -- M and F constraints (vectors of booleans)
# mf.weight -- weight of M > F term

# result

print(opt)

# distribution given learned weights

distribution(o.tab, opt$par)
hidden.distribution(o.tab, opt$par)

# error

error(o.tab, opt$par)
sse(o.tab, opt$par)

# categorical.error(o.tab, opt$par)
# categorical.sse(o.tab, opt$par)

opt$par
