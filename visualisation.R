require(ggplot2)

MDS <- function(distance_matrix, attributes) {
  fit <- cmdscale(distance_matrix, eig=TRUE, k=2)
  x <- fit$points[, 1]
  y <- fit$points[, 2]
  data.df <- data.frame(x, y)
  names(data.df) <- c('x', 'y')
  plot <- ggplot(data.df, aes(x, y)) +
    geom_point(color = 'red') +
    geom_text_repel(aes(label = attributes))
  print(plot)
}

grid_arrange_shared_legend <- function(plots) {
  n <- length(plots)
  nCol <- floor(sqrt(n))
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}