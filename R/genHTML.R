#' Generate Rmarkdown
#'
#' @param x data 
#' @param outfile A string denoting the location to put the output html
#' file.
#' 
#' @return An html document.
#'
#' @details Generates an html file of various exploratory plots via
#' RMarkdown.
#'       
#' @seealso \code{\link[rmarkdown]{render}}
#'
#' @importFrom stats cor
#' @importFrom rmarkdown render
#' @importFrom plotly plot_ly
#' @import knitr
#' @import ggplot2
#' @examples
#' require(meda)
#' set.seed(13)
#' n <- 100
#' x <- data.frame(v1 = v1 <<- sort(runif(n)), 
#'                 v2 = v2 <<- sort(rnorm(n)), 
#'                 v3 = v3 <<- ifelse(v1 > 0, v1, rnorm(1)),
#'                 v4 = v4 <<- ifelse(v2 < 0, v2, rnorm(1)))
#' genHTML(x, outfile = "~/Desktop/ex.html")
#'
#' \dontrun{
#' here <-  getwd()
#' genHTML(ex1[, seq(1,144,by=6), with = FALSE], paste0(here, "/example.html"))
#' }
#' @export


genHTML <- function(x, outfile, use.plotly = TRUE){

  dat <- x

  fs <- scale(dat, center= TRUE, scale = TRUE)
  mdat <- data.table::melt(as.data.frame(x))
  mdats <- data.table::melt(as.data.frame(fs))
  
  plty.heat <- plot_ly(z = fs, type = 'heatmap')
  
  gg <- ggplot(mdats, aes(x = factor(variable)))
  
  gg.violin.w <- ifelse(dim(dat)[2] > 12, max((dim(dat)[2])/1.8, 18),6)
  gg.violin.h <- dim(dat)[2]/1.8
  gg.violin <- gg + geom_violin() + coord_flip()
 
  cmat <- cor(dat)
  plty.cor <- plot_ly(z = cmat, type = 'heatmap')

  st.hm.w <- ifelse(dim(dat)[2] > 12, max(dim(dat)[2]/1.8, 18), 6)

  st.hm <- paste0("heatmap.2(as.matrix(dat), dendrogram = 'none', 
                       Colv = NA, trace = 'none', labRow= '',
                       col = gray.colors(255))")


  gg.outlier <- gg + geom_boxplot(outlier.color = 'red') + coord_flip()
   
  #rmd <- system.file("extdata", "skeleton.Rmd", package = "meda")
  rmd <- "../inst/extdata/skeleton.Rmd"

  pltly <- use.plotly
  render(rmd, output_file = outfile)
}






