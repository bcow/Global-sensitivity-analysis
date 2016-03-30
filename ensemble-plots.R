load("ensemble.Rdata")

if(file.exists("plot_data.Rdata")){
  load("plot_data.Rdata")
}else{
  plot_data <- init
  for(v in vars){
    V <-  sapply(out, "[[", v)
    stats <- t(apply(V, 2, function(x) quantile(x, c(.975,.5,.025), na.rm=TRUE)))
    colnames(stats) <- c(paste0(v,"_CI_high"),paste0(v,"_mean"),paste0(v,"_CI_low"))
    plot_data <- cbind(plot_data, stats)
  }
  save(plot_data, file="plot_data.Rdata")
}

###################################################
# Simple plotting function 

plot_output <- function(plot_data, vars, params){
  for(v in vars){
    mean <- paste0(v,"_mean")
    high <- paste0(v,"_CI_high")
    low <- paste0(v,"_CI_low")
    
    png(filename=file.path("plots",paste0(v,"_sensitivity.png")), width = 1000, height = 1200)
    par(mfrow = c(4,3))
    for(p in params){
      plot(plot_data[,p], plot_data[,high], col="red",
           ylim=c(min(plot_data[,low]), max(plot_data[,high])))
      points(plot_data[,p],plot_data[,low], col = "blue")
      points(plot_data[,p], plot_data[,mean])
      title(xlab=(p), ylab=(v), cex.lab = 3)
    } 
    dev.off()
  }
}

plot_output_smoothed <- function(plot_data, vars, params, save_png=FALSE){
  for(v in vars){ 
    mean <- paste0(v,"_mean")
    if(save_png){png(filename=file.path("plots",paste0(v,"_sensitivity_smoothed.png")), width = 1000, height = 1200)}
    r = ceiling(sqrt(length(params)))
    c = ceiling(length(params)/r)
    par(mfrow=c(r,c))
    for(p in params){
      x <- plot_data[,p]
      y <- plot_data[,mean]
      plot(x, y, xlab="", ylab = "")
      title(xlab=(p), main=(v), cex.lab = 2)
      lines(smooth.spline(x,y,nknots=6), lwd = 3, col="red")
    } 
    if(save_png){dev.off()}
  }
}


###################################################
# All the possible plots, takes a while 


plot_output(plot_data, vars = "NPP", params = "autotrophic_respiration_fraction")


###################################################
# Plots with PDFs

# V <- "NPP"
# P <- "autotrophic_respiration_fraction"

V <- "GPP"
P <- "autotrophic_respiration_fraction"

x <- plot_data[,P]; pdfx <- density(x)
y <- plot_data[,paste0(V,"_mean")]; pdfy <- density(y)


par(mfrow=c(1,1))
plot(x,y,col="gray")
lines(smooth.spline(x,y,nknots=6), lwd = 3)
par(new=TRUE)
plot(pdfx$x, pdfx$y, col="red", ylim=c(0,max(pdfx$y)*5), type="l", lwd=3, axes = FALSE, bty = "n", xlab = "", ylab = "")
par(new=TRUE)
plot(pdfy$y, pdfy$x, col="green", xlim=c(0,max(pdfy$y)*5), type="l", lwd=3, axes = FALSE, bty = "n", xlab = "", ylab = "")

###################################################
# Boxplots

V <- "GPP"
P <- "autotrophic_respiration_fraction"

x <- plot_data[,P]; pdfx <- density(x)
y <- plot_data[,paste0(V,"_mean")]; pdfy <- density(y)


par(fig=c(0.05,0.85,0,0.85))
plot(x,y,ann=FALSE)
lines(smooth.spline(x,y,nknots=6), lwd = 4, col="red")
par(fig=c(0.05,0.85,0.5,1), new=TRUE)
boxplot(x, horizontal=TRUE, axes=FALSE)
par(fig=c(0.65,1,0,0.85),new=TRUE)
boxplot(y, axes=FALSE)
mtext("Spline smoothed with Boxplot", cex = 1.5, side=3, outer=TRUE, line=-4)
mtext(P, cex = 1.5, side=1, outer=TRUE, line=-2)
mtext(V, cex = 1.5, side=2, outer=TRUE, line=-3)


###################################################
# Interesting plots with ggplot
library(ggplot2)

par(mfrow=c(1,1))

ggplot(plot_data, aes(x=autotrophic_respiration_fraction, y=NPP_mean), col =2) + 
  geom_errorbar(aes(ymin=NPP_CI_low, ymax=NPP_CI_high), colour="gray", width=.01) +
  geom_line() +
  geom_point(size=3) +
  ylab("NPP") +
  theme_bw()


ggplot(plot_data, aes(x=root_turnover_rate, y=RootLitter_mean), col =2) + 
  geom_errorbar(aes(ymin=RootLitter_CI_low, ymax=RootLitter_CI_high), colour="gray", width=.001) +
  geom_line() +
  geom_point(size=3) +
  ylab("Root Litter") +
  theme_bw()

par(mfrow=c(1,1))

V <- "GPP"
P <- "autotrophic_respiration_fraction"

x <- plot_data[,P]
y <- plot_data[,paste0(V,"_mean")]
y1 <- plot_data[,paste0(V,"_CI_high")]
y2 <-plot_data[,paste0(V,"_CI_low")]

data <- as.data.frame(cbind(smooth.spline(x,y,nknots=6)$x,
                            smooth.spline(x,y,nknots=6)$y,
                            smooth.spline(x,y1,nknots=6)$y,
                            smooth.spline(x,y2,nknots=6)$y))
colnames(data) <- c("x","mean","high","low")

plot(data$x, data$mean)

h <- ggplot(data, aes(x=x))
h + geom_ribbon(aes(ymin=low, ymax=high), alpha=.2, fill = "blue") + geom_line(aes(y=mean), colour = "darkblue", size =2) +
  labs(
    x=P,
    y=V,
    title="95% CI"
  )+ theme_bw()+ theme(text = element_text(size=24))

