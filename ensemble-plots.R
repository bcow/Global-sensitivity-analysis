###################################################
# Simple plotting functions

plot.ensemble <- function(ensemble.out, x, y, pdfs=TRUE, fit.method="lm"){
  if(pdfs){
    zones <- matrix(c(1, 0, 3, 2), ncol=2, byrow=TRUE)
    layout(zones, widths=c(4/5, 1/5), heights=c(1/5, 4/5))
    pdfx <- density(ensemble.out[,x])
    pdfy <- density(ensemble.out[,y])
    par(mar=c(0,3,1,1))
    plot(pdfx$x, pdfx$y, type="l", lwd=3, axes = FALSE, bty = "n", xlab = "", ylab = "")
    par(mar=c(3,0,1,1))
    plot(pdfy$y, pdfy$x, type="l", lwd=3, axes = FALSE, bty = "n", xlab = "", ylab = "")
  }
  par(mar=c(4,4,1,1))
  form <- formula(sprintf("%s ~ %s", y, x))
  plot(form, ensemble.out, col="grey50")
  if(!is.na(fit.method)){
    if(fit.method == "lm"){
      fitline <- lm(form, data=ensemble.out)
      abline(fitline, lwd=3)
    } else if(fit.method == "spline"){
      fitline <- smooth.spline(ensemble.out[,x], ensemble.out[,y])
      lines(fitline, lwd=3)
    }
    else{
      stop("Unrecognized fit function")
    }
  }
}
