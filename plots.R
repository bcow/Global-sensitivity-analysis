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
    
    png(filename=file.path("plots",paste0(v,"_sensitivity.png")), width = 2000, height = 2200)
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


###################################################
# All the possible plots, takes a while 

plot_output(plot_data, vars, params)

###################################################
# Interesting plots with ggplot

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

