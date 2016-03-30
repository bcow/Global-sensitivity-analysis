load("ensemble.Rdata")


###################################################
# NPP


NPP <- sapply(out, "[[", "NPP")
NPP_stats <- as.data.frame(matrix(NA, nrow=1000, ncol=3))
colnames(NPP_stats) <- c("CI_high", "mean", "CI_low")

for (i in 1:1000){
  NPP_stats[i,] <- quantile(NPP[,i], c(.975,.5,.025),na.rm=TRUE)
}

par(mfrow = c(4,3))
for(p in params){
  plot(init[,p], NPP_stats$mean, xlab = p, ylab="NPP")
}

par(mfrow=c(1,1))

plot(init$autotrophic_respiration_fraction, NPP_stats$CI_high, ylim=c(min(NPP_stats$CI_low), max(NPP_stats$CI_high)), col="red")
points(init$autotrophic_respiration_fraction, NPP_stats$CI_low, col = "blue")
points(init$autotrophic_respiration_fraction, NPP_stats$mean)

plot_dat <- NPP_stats
plot_dat$autotrophic_respiration_fraction <- init$autotrophic_respiration_fraction

ggplot(plot_dat, aes(x=autotrophic_respiration_fraction, y=mean), col =2) + 
  geom_errorbar(aes(ymin=CI_low, ymax=CI_high), colour="gray", width=.01) +
  geom_line() +
  geom_point(size=3) +
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
