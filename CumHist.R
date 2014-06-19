#! /usr/bin/Rscript --vanilla
# JSS: CumHist.R
# -----------------------------
# Produccion de un histograma acumulado

CumHist <- function (x, breaks = "Sturges", freq = NULL, ...) {
  h <- hist(x, plot=F, breaks)
  h$counts <- cumsum(h$counts)
  h$density <- cumsum(h$density)*(h$breaks[2]-h$breaks[1])
  plot(h, freq, ...)
  invisible(h) # Lo regresa invisible
}