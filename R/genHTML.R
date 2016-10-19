#' Generate an Rmarkdown with exploratory plots 
#'
#' @param x data 
#' @param outfile A string denoting the location to put the output html
#' file.
#' @param use.plotly A Boolean to specify if ggplotly is used.
#' @param scale A Boolean to specify whether column-wise scaling is
#' performed before analysis.
#' @param samp An integer specifying the size of the random sample to be
#' taken if the number of points exceeds 1e5.
#' 
#' @return An html document via RMarkdown.
#'
#' @details Generates an html file of various exploratory plots via
#' RMarkdown. "There is no excuse for failing to plot and look." ~ J. W.
#' Tukey (Exploratory Data Analysis, p 43)
#'       
#' @seealso \code{\link[rmarkdown]{render}}
#'
#' @importFrom graphics pairs plot
#' @importFrom grDevices gray.colors
#' @importFrom rmarkdown render
#' @importFrom plotly plot_ly
#' @importFrom mclust bic Mclust
#' @importFrom robustbase covMcd
#' @import knitr
#' @import ggplot2
#' @import stats
#' @import corrplot
#'
#' @examples
#' require(meda)
#' x <- iris[, -5]
#' y <- data.frame(matrix(rnorm(1e3*24, mean = rep(c(1,4,2,5), each =
#' 1e3), sd = 0.25), ncol = 24))
#' outfile.1 <- paste0(getwd(), '/ex1.html')
#' outfile.2 <- paste0(getwd(), '/ex2.html')
#' use.plotly <- FALSE
#' scale <- TRUE
#' print("Now run 'genHTML(x, outfile, use.plotly, scale)'")
#' \dontrun{
#' genHTML(x, outfile.1, use.plotl = TRUE, scale)
#' genHTML(W, outfile, use.plotl = TRUE, scale)
#' }
#'
#' @export


genHTML <- function(x, outfile, use.plotly = TRUE, scale = TRUE, samp = 1e4) {

  use.plotly <- use.plotly
  n <- dim(x)[1]
  p <- dim(x)[2]

  if(n > 1e5) {
    x <- x[sample(n, samp), ] 
    n <- samp
  }

  if(scale){
    dat <- scale(x, center = TRUE, scale = TRUE) 
  } else {
    dat <- x
  }
 
  p.try <- function(FUN, dat, use.plotly = NULL) {
    out <- tryCatch(
      {
        if(is.null(use.plotly)) {
          do.call(FUN, args = list(dat = dat))
          } else {
          do.call(FUN, args = list(dat = dat, use.plotly = use.plotly))
          }
      },
      error = function(cond) {
        message("Something bad happend, check your data and try again.")
        message(cond)
        return(NA)
      },
      warning = function(cond) {
        message("Function returned a warning, check your data and try again.")
        message(cond)
        return(NULL)
      }
    )
    return(out)
  }

  ### Structure of Data
  colStr <- table(Reduce(c,lapply(as.data.frame(dat), class)))
  complete <- all(complete.cases(dat))
  nas <- anyNA(dat)
  negs <- any(dat < 0)

  ### Heatmaps 
  p.heat <- function(dat, use.plotly){
    out <- 
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
  p.violin <- function(dat, use.plotly) {
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
  p.cor <- function(dat) {
    out <- list(corr = cor(dat), method = "color", tl.cex = 1)
    return(out)
  }


  ### Outlier plots
  p.outlier <- function(dat) {

    ## Create data.frame of robust distances (rd) 
    ## as in Hubert et al. 2008
    ## calculated with FAST MCD or covOGK
    mcd <- covMcd(dat)
    tmp <- 1:dim(dat)[1]

    mx <- sqrt(mahalanobis(dat, center = mcd$center, cov = mcd$cov))
    rd <- data.frame(index = as.integer(tmp), rd = mx) 

    alev <- 0.01
	  aline <- sqrt(qchisq(1 - alev / 100, p, ncp = 0, lower.tail = TRUE, log.p = FALSE))

    gg.outlier <- 
      ggplot(data = rd, aes(x = index, y = mx)) + 
    	  geom_point() + 
        geom_hline(yintercept = aline) + 
        ggtitle("Robust Distances of the data") +
        ylab("Robust Distances")

    return(gg.outlier)
    } ## END p.outlier

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
  p.pairs <- function(dat) {
    pca <- prcomp(dat, center = TRUE, scale = TRUE)
    du <- ifelse(dim(pca$x)[2] > 8, 8, dim(pca$x)[2])
    
    pairs(dat[, 1:du], pch = '.',  main = "Pairs plot of first 8 dimensions")
    pairs(pca$x[,1:du], pch = '.', main = "Pairs plot of first 8 PCs")
  }


  ### BIC plot
  p.bic <- function(dat, timeLimit = 8*60 ) {
    local({
      setTimeLimit(cpu = timeLimit, transient = FALSE)
      bicO <<- mclust::mclustBIC(dat, G = 1:10)
    })
    print(summary(bicO))
    plot(bicO) 
  }
  
  ### Mclust Classifications 
  p.mclust <- function(dat) {
    if(dim(dat)[1] > 1e5 & dim(dat)[2] > 100){
      stop("Dimensions are too large.")
      } else {
       mod1 <- Mclust(dat, x = bicO)
       plot(mod1, "classification") 
      }
  }

  rmd <- system.file("extdata", "skeleton.Rmd", package = "meda")

  render(rmd, output_file = outfile)
}
