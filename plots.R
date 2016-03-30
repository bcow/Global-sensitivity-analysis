load("ensemble.Rdata")

plot_data <- init

###################################################
# NPP

NPP <- sapply(out, "[[", "NPP")
NPP_stats <- as.data.frame(matrix(NA, nrow=1000, ncol=3))

for (i in 1:1000){
  NPP_stats[i,] <- quantile(NPP[,i], c(.975,.5,.025),na.rm=TRUE)
}
colnames(NPP_stats) <- c("NPP_CI_high", "NPP_mean", "NPP_CI_low")

plot_data <- cbind(plot_data, NPP_stats)

par(mfrow = c(4,3))
for(p in params){
  plot(plot_data[,p], NPP_CI_high, ylim=c(min(NPP_CI_low), max(NPP_CI_high)), col="red")
  points(plot_data[,p], NPP_CI_low, col = "blue")
  points(plot_data[,p], NPP_mean)
}


###################################################
# Total Living Biomass

TLB <- sapply(out, "[[", "TotLivBiom")
TLB_stats <- as.data.frame(matrix(NA, nrow=1000, ncol=3))
colnames(TLB_stats) <- c("TLB_CI_high", "TLB_mean", "TLB_CI_low")

for (i in 1:1000){
  TLB_stats[i,] <- quantile(TLB[,i], c(.975,.5,.025),na.rm=TRUE)
}

plot_data <- cbind(plot_data, TLB_stats)
attach(plot_data)

par(mfrow = c(3,4))
for(p in params){
  plot(plot_data[,p], TLB_CI_high, ylim=c(min(TLB_CI_low), max(TLB_CI_high)), col="red")
  points(plot_data[,p], TLB_CI_low, col = "blue")
  points(plot_data[,p], TLB_mean)
}


###################################################
# ggplot

par(mfrow=c(1,1))

ggplot(plot_data, aes(x=autotrophic_respiration_fraction, y=NPP_mean), col =2) + 
  geom_errorbar(aes(ymin=NPP_CI_low, ymax=NPP_CI_high), colour="gray", width=.01) +
  geom_line() +
  geom_point(size=3) +
  ylab("NPP") +
  theme_bw()

ggplot(plot_data, aes(x=autotrophic_respiration_fraction, y=TLB_mean), col =2) + 
  geom_errorbar(aes(ymin=TLB_CI_low, ymax=TLB_CI_high), colour="gray", width=.01) +
  geom_line() +
  geom_point(size=3) +
  ylab("Total Living Biomass") +
  theme_bw()

# ###################################################
# 
# LB <- sapply(out, "[[", "LitterBiomass")
# LB_stats <- as.data.frame(matrix(NA, nrow=1000, ncol=3))
# colnames(LB_stats) <- c("CI_high", "mean", "CI_low")
# 
# for (i in 1:1000){
#   LB_stats[i,] <- quantile(LB[,i], c(.975,.5,.025),na.rm=TRUE)
# }
# 
# 
# plot(init$litter_respiration_rate[1:100], LB_stats$mean[1:100], xlim = c(0,.02))
# points(init$litter_respiration_rate[1:100], LB_stats$CI_high[1:100], col = "red")
# points(init$litter_respiration_rate[1:100], LB_stats$CI_low[1:100], col = "blue")
# 
