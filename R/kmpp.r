#' K-Means++ initialization to K-means
#'
#' @param x The data formatted as a matrix.
#' @param k The number of clusters desired. \code{k < nrows(x) || length(x)}
#' 
#' @return An object of class `"kmeans"'
#'
#' @details A set of centers are calculated as suggested by
#' arthur and Vassilvitskii and used to initialize the kmeans algorithm.
#' The first center point is chosen at random.  Each subsequent center
#' is chosen with probability P = D(x')^2/( ∑_{x\in X} D(x)^2) (see refernces)
#'       
#'
#' @seealso \code{\link{kmeans}}
#'
#' @examples
#' set.seed(13)
#' z <- c(0,6,18,24)[sample(4, 1e2, replace=TRUE)]
#' x <- rnorm(1e2, mean = z, sd = 1)
#' kdf <- kmpp(x, 4)
#' plot(x, rep(1,length(x)), col = kdf$cluster)
#' 
#' z2 <- cbind(c(0,8,0,8),c(0,5,5,0))[sample(4,1e6, replace = TRUE),]
#' xy <- data.frame(x = rnorm(1e3, mean = z2[, 1]), y = rnorm(1e3, mean = z2[, 2]))
#' kdf <- kmpp(xy, 4)
#' kdf <- kmeans(xy, centers = 4)
#' plot(xy, col = kdf$cluster)
#'
#' @references: \code{@inproceedings{arthur2007,
#'   title={k-means++: The advantages of careful seeding},
#'   author={Arthur, David and Vassilvitskii, Sergei},
#'   booktitle={Proceedings of the eighteenth annual ACM-SIAM symposium on Discrete algorithms},
#'   pages={1027--1035},
#'   year={2007},
#'   organization={Society for Industrial and Applied Mathematics}
#' }}
#'


kmpp <- function(x,k=2){
 ## 
 la2 <- function(a,b){
          apply(t(apply(as.matrix(b), 1, function(rr) (rr - a)^2)), 1, sum)
        }

  ## Check if data is one dimensional 
  if(is.null(dim(x))){
    la1 <- function(a,b){
      (b - a)^2
    }
             
    ### Step 1a
    cp <- sample(length(x), 1)
    ind <- c(cp)
    centers <- x[cp]
    
    p <- la1(centers[1], x)
    P <- p/sum(p) ## P = D(x')^2/( ∑_x\inX  D(x)^2)
     
    ### Step 1b-c
    for( i in 2:k ) {
        #set.seed(2^13)
        cp <- sample(length(x),1,prob=P) ## c'
        centers[i] <- x[cp]
        ind <- c(ind,cp)
        
        pnew <- la1(x[cp],x) 
        p <- apply(cbind(la1(x[cp],x),p),1,min) 
        P <- p/sum(p)
        }
    
    kObj <- kmeans(x, centers = centers)

    return(kObj)

    } else {

    ### Step 1a
    cp <- sample(nrow(x),1)
    
    ind <- c(cp)
    
    centers <- list()
    
    centers[[1]] <- x[cp,]
    
    p <- la2(x[cp,],x)
    P <- p/sum(p) ## P = D(x')^2/( ∑_x\inX  D(x)^2)
    
    ### Step 1b-c
    for( i in 2:k ) {
        #set.seed(2^13)
        cp <- sample(nrow(x),1,prob=P) ## c'
        centers[[i]] <-x[cp,]
        ind <- c(ind,cp)
        
        p <- apply(cbind(la2(x[cp,],x),p),1,min) #apply(cbind(p,ds[,cp]),1,min)
        P <- p/sum(p)
        }
    
    centers <- Reduce('rbind',centers)
    
    kObj <- kmeans(x, centers = centers)
    
    return(kObj)
    } ### END ELSE
} ### END FUNCTION
#
### Time: About 3-6 hours
### Working status: Works as expected.
### Comments: Might need work to be robust against messy data.
### Soli Deo Gloria
