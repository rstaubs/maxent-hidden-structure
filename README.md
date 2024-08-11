# Maximum Entropy with Hidden Structure in R

R support for various batch and online computations in Maximum Entropy Harmonic Grammar. Used to find MaxEnt solutions for learning problems (with or without hidden structure), generate distributions over forms in different theories, and perform associated learning tasks.

## Installation

1.  Install R (<https://cran.rstudio.com/>)
2.  Clone repository (e.g. `git clone git@github.com:rstaubs/maxent-hidden-structure.git` )
3.  Open repository in R environment of choice (e.g., RStudio <https://posit.co/download/rstudio-desktop/>)

## Examples

Examples are shown in the `examples` directory:

1.  `base` - Example driver file and data files for parallel MaxEnt (with and without hidden structure)
2.  `serial` Example driver file and data files for serial MaxEnt

## Notes on Optimization

The gradients used in this optimization are derived here: <https://websites.umass.edu/hgr/files/2017/07/klnotes.pdf>

## Citation

Staubs, Robert. 2011. Harmonic Grammar in R (hgR). Software package. Amherst, MA: University of Massachusetts Amherst. <https://github.com/rstaubs/maxent-hidden-structure>
