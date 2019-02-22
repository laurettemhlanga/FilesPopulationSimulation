library(ggplot2)
library(grid)
# setup the ploting grid and plotting sequence

#Examples of static/non volatile variables in R 

f <- function(x) {
  y <- attr(f, "sum")
  if (is.null(y)) {
    y <- 0
  }
  y <- x + y
  attr(f, "sum") <<- y
  return(y)
}

f(3)
x = 3
x <- 1:10
attr(x,"dim") <- c(2, 5)
f()




mplot.setup <- function(nrow, ncol, by.row = TRUE) {
  attributes(mplot.seq) <<- list(nrow = nrow, ncol = ncol,
                                 pos = 0, by.row = by.row)
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(nrow, ncol)))
}  



# plot at given grid location
mplot <- function(graph, row, col) {
  print(graph, vp = viewport(layout.pos.row = row,
                             layout.pos.col = col))
}



# plot the at the next position in the sequence
mplot.seq <- function(graph) {
  pos <- attr(mplot.seq, "pos")
  nrow <- attr(mplot.seq, "nrow")
  ncol <- attr(mplot.seq, "ncol")
  if (attr(mplot.seq, "by.row")) {
    col <- 1 + (pos %% ncol)
    row <- 1 + ((pos %/% ncol) %% nrow)
  }else{
    row <- 1 + (pos %% nrow)
    col <- 1 + ((pos %/% nrow) %% ncol)
  }
  attr(mplot.seq, "pos") <<- pos + 1
  mplot(graph, row, col)
}



# application example
mplot.setup(2,4, FALSE)
for (i in 1:4) {
  mplot.seq(qplot(iris[,i], xlab = names(iris)[i]))
  mplot.seq(qplot(iris[,5], iris[,i], geom = "boxplot",
                  xlab = "Species", ylab = names(iris)[i]) + coord_flip())
}





