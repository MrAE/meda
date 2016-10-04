# README

This is an R package for exploratory data analysis specifically tuned
for synaptomics data. 

# INSTALLATION

Download or clone the repository and uncompress the zip folder.
At command line `cd` to the location of the folder and run 
> R CMD INSTALL \<folder name\>
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

