MDS <- function(distance_matrix, attributes) {
  fit <- cmdscale(distance_matrix, eig=TRUE, k=2)
  x <- fit$points[, 1]
  y <- fit$points[, 2]
  plot(x, y, pch = 19, xlim = c(min(x) * 1.0, max(x)*1.1))
  text(x, y, pos = 4, labels = attributes)
}