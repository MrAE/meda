#' Generate an Rmarkdown with exploratory plots 
#'
#' @param x data 
#' @param outfile A string denoting the location to put the output html
#' file.
#' @param use.plotly A Boolean to specify if ggplotly is used.
#' @param scale A Boolean to specify whether column-wise scaling is
#' performed before analysis.
#' 
#' @return An html document.
#'
#' @details Generates an html file of various exploratory plots via
#' RMarkdown. "There is no excuse for failing to plot and look." ~ J. W.
#' Tukey (Exploratory Data Analysis, p 43)
#'       
#' @seealso \code{\link[rmarkdown]{render}}
#'
#' @importFrom stats cor mahalanobis
#' @importFrom rmarkdown render
#' @importFrom plotly plot_ly
#' @importFrom mclust bic
#' @import knitr
#' @import ggplot2
#' @examples
#' require(meda)
#' set.seed(73)
#' n <- 100
#' p <- 48
#' x <- data.frame(matrix(runif(n*p) + rnorm(n*p, sd = 0.01), nrow = n, ncol = p))
#' colnames(x)  <- state.name[1:dim(x)[2]]
#'
#' outfile <- "./ex.html"
#' use.plotly <- FALSE; scale <- TRUE
#' x <- iris[, -5]
#' genHTML(x, outfile, use.plotly = FALSE, scale = TRUE)
#'
#'
#' @export


genHTML <- function(x, outfile, use.plotly = TRUE, scale = TRUE){

  n <<- dim(x)[1]
  p <<- dim(x)[2]

  gg.violin.h <<- ifelse(p > 12, p/1.8,8)
  gg.violin.w <<- ifelse(n > 24, 12, 6)

  use.plotly <<- use.plotly

  if(scale){
    dat <<- scale(x, center = TRUE, scale = TRUE) 
  } else {
    dat <<- x
  }
 
  ### Heatmaps 
  p.heat <- function(dat, use.plotly){
    if(use.plotly){ 
      plty.heat <- plot_ly(z = dat, type = 'heatmap')
      return(plty.heat)
    } else {

      mdat <- data.table::melt(as.data.frame(dat), id = NULL)
      rasf <- factor(rep(colnames(dat), each = dim(dat)[1]), levels = colnames(dat), ordered = TRUE)
      ras <- data.frame(x = rasf, y = 1:(dim(dat)[1]))
      ras$z <- mdat$value 
  
      gg.heat <- 
        ggplot(ras, aes(x = x, y = y, fill = z)) + 
        geom_raster() + scale_y_reverse(expand = c(0,0)) + 
        scale_fill_gradientn(colours = gray.colors(255, start = 0)) +
        xlab("") + ylab("index") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5), 
              panel.background = element_blank())

      return(gg.heat)
      }
  }

  ### Violin plots
  p.violin <- function(dat){
    mdat <- data.table::melt(as.data.frame(dat), id = NULL)
    gg <- ggplot(mdat, aes(x = factor(variable), y = value))
    

    gg.violin <- if(p > 12){
        gg + geom_violin() + coord_flip()
      } else {
        gg + geom_violin()
      }
    return(gg.violin)
  }


  ### Correlation plots 
  p.cor <- function(dat){
    cmat <- cor(dat)
    return(corrplot(cmat, method="color", tl.cex=1))
  }


  ### Outlier plots
  p.outlier <- function(dat) {
   
    M2 <- data.frame(mx2 = mahalanobis(dat, 
                                       center = apply(dat, 2, mean), 
                                       cov = cov(dat)))

    gg.outlier <- if(diff(range(M2)) > .9e3){
      ggplot(M2, aes(x = "", y = mx2)) + 
        geom_boxplot(outlier.color = 'red') + 
        scale_y_log10() + coord_flip() + ylab(expression(Mahalanobis^2))
      } else {
      ggplot(M2, aes(x = "", y = mx2)) + 
        geom_boxplot(outlier.color = 'red') + ylab(expression(Mahalanobis^2))
      }

    #iqr <- IQR(M2$mx2)
    #h <- quantile(M2$mx2, prob = c(0.25, 0.75))
    #bounds <- c(h[1] - 1.5 * iqr, h[2] + 1.5 * iqr)
    #
    #outliers <<- which(M2$mx2 < bounds[1] | M2$mx2 > bounds[2])

    return(gg.outlier)
    }

    

    #gg.kdeM2 <- ggplot(M2, aes(x = mx2, y = ..density..)) + geom_density()

  ### Cumulative variance
  p.cumvar <- function(dat){
    tryCatch(source("http://www.cis.jhu.edu/~parky/Synapse/getElbows.R"))   
  
    pca <- prcomp(dat, center = TRUE, scale = TRUE)
    tryCatch(elb <- getElbows(pca$sdev, plot = FALSE))
  
    CS <- data.frame(index = 1:(dim(pca$x)[2]), cs = (100*cumsum(pca$sdev / sum(pca$sdev))))
    CS$col <- "" 
    tryCatch(CS$col[elb] <- "elbow")
   
    gg.cumvar <- 
      ggplot(CS, aes(x = index, y = cs)) + 
      geom_point(aes(color = col)) + 
      scale_color_manual(values = c("black", "red")) + 
      geom_line() + 
      ylab("% Cumulative Variance") + 
      ggtitle("Cumulative Sum of variace in PC's")

    return(gg.cumvar)
  }

  ### Pairs Plots
  p.pairs <- function(dat){
    pca <- prcomp(dat, center = TRUE, scale = TRUE)
    du <- ifelse(dim(pca$x)[2] > 8, 8, dim(pca$x)[2])
    
    pairs(dat[, 1:du], main = "Pairs plot of first 8 dimensions")
    pairs(pca$x[,1:du], main = "Pairs plot of first 8 PCs")
  }


  ### BIC plot
  p.bic <- function(dat){
    bic <- mclust::mclustBIC(dat, G = 1:10)
    plot(bic) 
  }
  
  
  rmd <- system.file("extdata", "skeleton.Rmd", package = "meda")

  render(rmd, output_file = outfile)
}






