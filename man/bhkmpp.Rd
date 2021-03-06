% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bhkmpp.R
\name{bhkmpp}
\alias{bhkmpp}
\title{Binary Hierarchical k-means++}
\usage{
bhkmpp(x, blevels)
}
\arguments{
\item{x}{A matrix, data.frame, or data.table with the data of
interest.}

\item{blevels}{An integer specifying how many levels in the binary
tree to compute.}
}
\value{
An object of type `'data.frame'' with the cluster labels 
of the \code{j}th level in the \code{j}th column. Labels are numbered
}
\description{
Binary Hierarchical k-means++
}
\details{
Runs the kmeans algorithm with the kmeans++ initialization
in a binary hieracrhical fasion.
}
\examples{
set.seed(13)
z <- c(0,4,12,16)[sample(4, 1e3, replace=TRUE)]
x <- rnorm(1e3, mean = z, sd = 1)
kdf <- bhkmpp(x, 2)
par(mfrow = c(3,1))
plot(x, as.factor(z), col = as.factor(z), pch = '|')
title('Original Data')
plot(x, kdf[,1], col = kdf[,1], pch = '|')
title('Clustered Data: Level 1')
plot(x, kdf[,2], col = kdf[,2], pch = '|')
title('Clustered Data: Level 2')

set.seed(23)
s1 <- sample(4, 1e3, replace = TRUE)
z <- matrix(c(0,4,0,4,0,0,4,4), ncol=2)[s1,]
xy <- data.frame(x = rnorm(1e3, mean = z[,1]), y = rnorm(1e3, mean=z[,2]))
kdf2 <- bhkmpp(xy,2)
par(mfrow = c(3,1))
opal <- palette(adjustcolor(palette(), alpha.f = 0.55))
plot(xy, col = s1, pch = 19)
title('Original Data')
plot(xy, col = kdf2[,1], pch = 17)
title('Clustered Data: Level 1')
plot(xy, col = kdf2[,2], pch = 17)
title('Clustered Data: Level 2')
palette('default')

}
\references{
Arthur, David, and Sergei Vassilvitskii. 
'k-means++: The advantages of careful seeding.'
Proceedings of the eighteenth annual ACM-SIAM
symposium on Discrete algorithms. Society for Industrial and Applied
Mathematics, 2007.
\url{http://ilpubs.stanford.edu:8090/778/1/2006-13.pdf}
}
\seealso{
\code{\link[stats]{kmeans}, \link{kmpp}}
}

