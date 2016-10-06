# README

meda: Matrix Exploratory Data Analysis is an R package for exploring
data in the form of matrices.

# INSTALLATION

To download and install run the following in R:

```R
devtools::install_github("mrae/meda")
```

----------------

### Given a new set of n putative synapse locations in R^3 with d markers in R(?)

**If n is small enough do the following, otherwise subsample.**

1. Heat map of raw data
1. Violin plots of each dimension/marker
1. Correlation matrix of features
1. Outlier plot
1. Cumulative variance (with elbows)
1. Pairs plots for top ~8 dimensions
1. mclust for k=1,...10 for all 10 models, plot BIC curves
1. color points in pairs plot by best cluster estimates
1. subsample hclust with different linkages. 

