panel.lm = function(x, y, ...) {
  tmp <- lm(y ~ x, na.action = na.omit)
  if(length(x)>4) {
    points(x, y, ...)
    abline(tmp, col="blue")
    panel.smooth(x, y, col.smooth="red", ...)
  } else {
    points(x, y, ...)
  }
}

# Plot with panels featuring regression and loess lines
coplot(sales ~ youtube | facebook, data=marketing, 
       col="orange", panel=panel.lm)
