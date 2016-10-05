#' Generate Rmarkdown
#'
#' @param x data 
#' @param outfile A string denoting the location to put the output html
#' file.
#' 
#' @return An html document
#'
#' @details Stuff to go here soon
#'       
#' @seealso \code{\link[rmarkdown]{render}}
#'
#' @importFrom reshape melt
#' @importFrom stats cor
#' @importFrom rmarkdown render
#' @import plotly
#' @import ggplot2
#' @examples
#' require(Rsynaptome)
#' set.seed(13)
#' n <- 100
#' x <- data.frame(v1 = v1 <<- sort(runif(n)), 
#'                 v2 = v2 <<- sort(rnorm(n)), 
#'                 v3 = v3 <<- ifelse(v1 > 0, v1, rnorm(1)),
#'                 v4 = v4 <<- ifelse(v2 < 0, v2, rnorm(1)))
#' genHTML(x, "./example.html")
#'
#' Not Run:
#' here <-  getwd()
#' genHTML(ex1[, seq(1,144,by=6), with = FALSE], paste0(here, "/example.html"))
#' @export


genHTML <- function(x, outfile){

  dat <- x

  fs <- scale(dat, center= TRUE, scale = TRUE)
  mdat <- reshape::melt(as.data.frame(x))
  mdats <- reshape::melt(as.data.frame(fs))
  
  plty.heat <- plot_ly(z = fs, type = 'heatmap')
  
  gg <- ggplot(mdats, aes(x = variable, y = value))
  
  gg.violin.w <- ifelse(dim(dat)[2] > 12, max((dim(dat)[2])/1.8, 18),6)
  gg.violin.h <- dim(dat)[2]/1.8
  gg.violin <- gg + geom_violin()
 
  cmat <- cor(dat)
  plty.cor <- plot_ly(z = cmat, type = 'heatmap')

  st.hm.w <- ifelse(dim(dat)[2] > 12, max(dim(dat)[2]/1.8, 18), 6)

  st.hm <- paste0("heatmap.2(as.matrix(dat), dendrogram = 'none', 
                       Colv = NA, trace = 'none', labRow= '',
                       col = gray.colors(255))")

  rmd <- system.file("extdata", "skeleton.Rmd", package = "Rsynaptome")
  render(rmd, output_file = outfile)
}





