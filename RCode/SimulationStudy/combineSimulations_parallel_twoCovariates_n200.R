library(rlist)
library(ggplot2)
library(gridExtra)
library(grid)
library(dplyr)
library(cowplot)

#--------------
# n=200
setwd("~/Workspaces/2C_NV_n200")
load("simulation_twoCov_NV_n200_1000_1.Rdata")
allcoefs <- coefs
# hurdle
allpvalues_hurdleQap <- pvalues_hurdleQap
allcoefsHurdleQap <- coefsHurdleQap
allcoefsHurdle <- coefs_hurdle
allpvalues_hurdle <- pvalues_hurdle
allmeanLambda <- meanLambda
# poisson
allpvalues_poissonQap <- pvalues_poissonQap
allcoefsPoissonQap <- coefsPoissonQap
allpvalues_poisson <- pvalues_poisson
allcoefsPoisson <- coefs_poisson
missingID <- c()

for (i in 2:1000){
  if(file.exists(paste0("simulation_twoCov_NV_n200_1000_", i, ".Rdata"))){
    load(paste0("simulation_twoCov_NV_n200_1000_", i, ".Rdata"))
    allcoefs <- c(allcoefs, coefs)
    allpvalues_hurdleQap <- c(allpvalues_hurdleQap, pvalues_hurdleQap)
    allcoefsHurdleQap <- c(allcoefsHurdleQap, coefsHurdleQap)
    allcoefsHurdle <- c(allcoefsHurdle, coefs_hurdle)
    allmeanLambda <- c(allmeanLambda, meanLambda)
    allpvalues_hurdle <- c(allpvalues_hurdle, pvalues_hurdle)
    allpvalues_poissonQap <- c(allpvalues_poissonQap, pvalues_poissonQap)
    allcoefsPoissonQap <- c(allcoefsPoissonQap, coefsPoissonQap)
    allpvalues_poisson <- c(allpvalues_poisson, pvalues_poisson)
    allcoefsPoisson <- c(allcoefsPoisson, coefs_poisson)
  } else {
    missingID <- c(missingID, i)
  }
}
setwd("~/Workspaces/2C_GV_n200")
for (i in 1001:2000){
  if(file.exists(paste0("simulation_twoCov_GV_n200_1000_", i, ".Rdata"))){
    load(paste0("simulation_twoCov_GV_n200_1000_", i, ".Rdata"))
    allcoefs <- c(allcoefs, coefs)
    allpvalues_hurdleQap <- c(allpvalues_hurdleQap, pvalues_hurdleQap)
    allcoefsHurdleQap <- c(allcoefsHurdleQap, coefsHurdleQap)
    allcoefsHurdle <- c(allcoefsHurdle, coefs_hurdle)
    allmeanLambda <- c(allmeanLambda, meanLambda)
    allpvalues_hurdle <- c(allpvalues_hurdle, pvalues_hurdle)
    allpvalues_poissonQap <- c(allpvalues_poissonQap, pvalues_poissonQap)
    allcoefsPoissonQap <- c(allcoefsPoissonQap, coefsPoissonQap)
    allpvalues_poisson <- c(allpvalues_poisson, pvalues_poisson)
    allcoefsPoisson <- c(allcoefsPoisson, coefs_poisson)
  } else {
    missingID <- c(missingID, i)
  }
}
# add models with 0 coefficients
setwd("~/Workspaces/2C_NV_n200")
for (i in 2001:2200){
  if(file.exists(paste0("simulation_twoCov_n200_coef01000_", i, ".Rdata"))){
    load(paste0("simulation_twoCov_n200_coef01000_", i, ".Rdata"))
    allcoefs <- c(allcoefs, coefs)
    allpvalues_hurdleQap <- c(allpvalues_hurdleQap, pvalues_hurdleQap)
    allcoefsHurdleQap <- c(allcoefsHurdleQap, coefsHurdleQap)
    allcoefsHurdle <- c(allcoefsHurdle, coefs_hurdle)
    allmeanLambda <- c(allmeanLambda, meanLambda)
    allpvalues_hurdle <- c(allpvalues_hurdle, pvalues_hurdle)
    allpvalues_poissonQap <- c(allpvalues_poissonQap, pvalues_poissonQap)
    allcoefsPoissonQap <- c(allcoefsPoissonQap, coefsPoissonQap)
    allpvalues_poisson <- c(allpvalues_poisson, pvalues_poisson)
    allcoefsPoisson <- c(allcoefsPoisson, coefs_poisson)
  } else {
    missingID <- c(missingID, i)
  }
}
# add models with some 0 coefficients
setwd("~/Workspaces/2C_n200_nullCoef")
for (i in 1:140){
  if(file.exists(paste0("simulation_twoCov_n200_nullCoef1000_", i, ".Rdata"))){
    load(paste0("simulation_twoCov_n200_nullCoef1000_", i, ".Rdata"))
    allcoefs <- c(allcoefs, coefs)
    allpvalues_hurdleQap <- c(allpvalues_hurdleQap, pvalues_hurdleQap)
    allcoefsHurdleQap <- c(allcoefsHurdleQap, coefsHurdleQap)
    allcoefsHurdle <- c(allcoefsHurdle, coefs_hurdle)
    allmeanLambda <- c(allmeanLambda, meanLambda)
    allpvalues_hurdle <- c(allpvalues_hurdle, pvalues_hurdle)
    allpvalues_poissonQap <- c(allpvalues_poissonQap, pvalues_poissonQap)
    allcoefsPoissonQap <- c(allcoefsPoissonQap, coefsPoissonQap)
    allpvalues_poisson <- c(allpvalues_poisson, pvalues_poisson)
    allcoefsPoisson <- c(allcoefsPoisson, coefs_poisson)
  } else {
    missingID <- c(missingID, i)
  }
}

#__________________________________________________

#--------  Plots für hurdleQap ----------------------------------------

# Darstellung wahre gegen geschätzte Koeffizienten


# pvalue vectors
# binomial
v1 <- unlist(list.select(allpvalues_hurdleQap, xbin_dist$pgreq))
v2 <- unlist(list.select(allpvalues_hurdleQap, xbin_dist$pleeq))
p_bin_dist <- ifelse(v1 < v2, v1, v2)*2

v1 <- unlist(list.select(allpvalues_hurdleQap, xbin_pub$pgreq))
v2 <- unlist(list.select(allpvalues_hurdleQap, xbin_pub$pleeq))
p_bin_pub <- ifelse(v1 < v2, v1, v2)*2

# #poisson
v1 <- unlist(list.select(allpvalues_hurdleQap, xpois_dist$pgreq))
v2 <- unlist(list.select(allpvalues_hurdleQap, xpois_dist$pleeq))
p_pois_dist <- ifelse(v1 < v2, v1, v2)*2

v1 <- unlist(list.select(allpvalues_hurdleQap, xpois_pub$pgreq))
v2 <- unlist(list.select(allpvalues_hurdleQap, xpois_pub$pleeq))
p_pois_pub <- ifelse(v1 < v2, v1, v2)*2


## hurdleQap
# bin_dist
x <-  unlist(list.select(allcoefs, beta_binDist))
y <-  unlist(list.select(allcoefsHurdleQap, xbin_dist))
p <- log(p_bin_dist + 0.001)
dat <- as.data.frame(cbind(x, y, p))
dat <- dat[order(dat$p, decreasing = FALSE),]

g_bin_dist <- ggplot(dat, aes(x = x, y = y)) +
  geom_point(aes(col = p), size = 2.2, alpha = 0.9) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.7) +
  scale_color_gradient(low = "lightblue", high = "darkblue", limits = c(-7, 0.001), name =  bquote(log(p[binDist]))) + 
  labs(x = "",
       y = "estimated",
       title = bquote(beta[binDist])) +
  theme(axis.text = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1.5)),
        legend.position = "bottom",
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.5))) +
  ylim(-0.01, 0.01)

# bin_pub
x <-  unlist(list.select(allcoefs, beta_binPub))
y <-  unlist(list.select(allcoefsHurdleQap, xbin_pub))
p <- log(p_bin_pub + 0.001)
dat <- as.data.frame(cbind(x, y, p))
dat <- dat[order(dat$p, decreasing = FALSE),]

g_bin_pub <- ggplot(dat, aes(x = x, y = y)) +
  geom_point(aes(col = p), size = 2.2, alpha = 0.9) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.7) +
  scale_color_gradient(low = "lightblue", high = "darkblue", limits = c(-7, 0.001), name = bquote(log(p[binPub]))) +
  labs(x = "",
       y = "",
       title =  bquote(beta[binPub])) +
  theme(axis.text = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1.5)),
        legend.position = "bottom",
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.5))) +
  ylim(-0.01, 0.01)

# pois_dist
x <-  unlist(list.select(allcoefs, beta_poisDist))
y <-  unlist(list.select(allcoefsHurdleQap, xpois_dist))
p <- log(p_pois_dist + 0.001)
dat <- as.data.frame(cbind(x, y, p))
dat <- dat[order(dat$p, decreasing = FALSE),]

g_pois_dist <- ggplot(dat, aes(x = x, y = y)) +
  geom_point(aes(col = p), size = 2.2, alpha = 0.9) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.7) +
  scale_color_gradient(low = "lightblue", high = "darkblue", limits = c(-7, 0.001), name = bquote(log(p[poisDist]))) +
  labs(x = "true",
       y = "estimated",
       title =   bquote(beta[poisDist])) +
  theme(axis.text = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1.5)),
        legend.position = "bottom",
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.5))) +
  ylim(-0.01, 0.01)

# pois_pub
x <-  unlist(list.select(allcoefs, beta_poisPub))
y <-  unlist(list.select(allcoefsHurdleQap, xpois_pub))
p <- log(p_pois_pub + 0.001)
dat <- as.data.frame(cbind(x, y, p))
dat <- dat[order(dat$p, decreasing = FALSE),]

g_pois_pub <- ggplot(dat, aes(x = x, y = y)) +
  geom_point(aes(col = p), size = 2.2, alpha = 0.9) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.7) +
  scale_color_gradient(low = "lightblue", high = "darkblue", limits = c(-7, 0.001), name =  bquote(log(p[poisPub]))) +
  labs(x = "true",
       y = "",
       title = bquote(beta[poisPub])) +
  theme(axis.text = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1.5)),
        legend.position = "bottom",
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.5))) +
  ylim(-0.01, 0.01)

pdf("Plots/TrueEstimatedCoefficents_hurdleQap_n50.pdf", width = 13, height = 13)
plot_hurdleQap_estimated <- grid.arrange(g_bin_dist, g_bin_pub, g_pois_dist, g_pois_pub, ncol = 2,
                                         top = textGrob("True vs. estimated hurdle-QAP coefficients, n = 50", gp = gpar(fontsize = 20)))
grid.draw(plot_hurdleQap_estimated) 
dev.off()

#_____________________________
# color according to mean lambda value
# bin_dist
x <-  unlist(list.select(allcoefs, beta_binDist))
y <-  unlist(list.select(allcoefsHurdleQap, xbin_dist))
p <- log(p_bin_dist + 0.001)
l <- unlist(allmeanLambda)
dat <- as.data.frame(cbind(x, y, p, l))
dat <- dat[order(dat$l, decreasing = FALSE),]

g_bin_dist <- ggplot(dat, aes(x = x, y = y)) +
  geom_point(aes(col = l), size = 2.2, alpha = 0.9) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.7) +
  scale_color_gradient2(low = "yellow", mid = "white", high = "red", limits = c(1,35), name = bquote(mean(lambda))) +
  labs(x = "",
       y = "estimated",
       title = bquote(beta[binDist])) +
  theme(axis.text = element_text(size = rel(1.2)),
        legend.position = "none",
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.5))) +
  ylim(-0.01, 0.01)

# bin_pub
x <-  unlist(list.select(allcoefs, beta_binPub))
y <-  unlist(list.select(allcoefsHurdleQap, xbin_pub))
p <- log(p_bin_pub + 0.001)
l <- unlist(allmeanLambda)
dat <- as.data.frame(cbind(x, y, p, l))
dat <- dat[order(dat$l, decreasing = FALSE),]

g_bin_pub <- ggplot(dat, aes(x = x, y = y)) +
  geom_point(aes(col = l), size = 2.2, alpha = 0.9) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.7) +
  scale_color_gradient2(low = "yellow", mid = "white", high = "red", limits = c(1,35), name = bquote(mean(lambda))) +
  labs(x = "",
       y = "",
       title =  bquote(beta[binPub])) +
  theme(axis.text = element_text(size = rel(1.2)),
       legend.position = "none",
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.5))) +
  ylim(-0.01, 0.01)

# pois_dist
x <-  unlist(list.select(allcoefs, beta_poisDist))
y <-  unlist(list.select(allcoefsHurdleQap, xpois_dist))
l <- unlist(allmeanLambda)
dat <- as.data.frame(cbind(x, y, p, l))
dat <- dat[order(dat$l, decreasing = FALSE),]

g_pois_dist <- ggplot(dat, aes(x = x, y = y)) +
  geom_point(aes(col = l), size = 2.2, alpha = 0.9) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.7) +
  scale_color_gradient2(low = "yellow", mid = "white", high = "red", limits = c(1,35), name = bquote(mean(lambda))) +
  labs(x = "true",
       y = "estimated",
       title = bquote(beta[poisDist])) +
  theme(axis.text = element_text(size = rel(1.2)),
        legend.position = "none",
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.5))) +
  ylim(-0.01, 0.01)

# pois_pub
x <-  unlist(list.select(allcoefs, beta_poisPub))
y <-  unlist(list.select(allcoefsHurdleQap, xpois_pub))
p <- log(p_pois_pub + 0.001)
l <- unlist(allmeanLambda)
dat <- as.data.frame(cbind(x, y, p, l))
dat <- dat[order(dat$l, decreasing = FALSE),]

g_pois_pub <- ggplot(dat, aes(x = x, y = y)) +
  geom_point(aes(col = l), size = 2.2, alpha = 0.9) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.7) +
  scale_color_gradient2(low = "yellow", mid = "white", high = "red", limits = c(1,35), name = bquote(mean(lambda))) +
  labs(x = "true",
       y = "",
       title =  bquote(beta[poisPub])) +
  theme(axis.text = element_text(size = rel(1.2)),
        legend.position = "none",
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.5))) +
  ylim(-0.01, 0.01)

p_legend <- ggplot(dat, aes(x = x, y = y)) +
  geom_point(aes(col = l), size = 2.2, alpha = 0.9) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.7) +
  scale_color_gradient2(low = "yellow", mid = "white", high = "red", limits = c(1,35), 
                        breaks = c(1, 10, 20, 30),
                        labels = c(1, 10, 20, 30),
                        name = bquote(mean(lambda))) +
  labs(x = "true",
       y = "estimated",
       title =  bquote(beta[poisPub])) +
  theme(axis.text = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1.5)),
        legend.position = "right",
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.5))) +
  ylim(-0.01, 0.01)

legend_b <- get_legend(p_legend)
 plot_hurdleQap_lambda <- grid.arrange(g_bin_dist, g_bin_pub, g_pois_dist, g_pois_pub, ncol = 2,
                                       top = textGrob("True vs. estimated hurdle-QAP coefficients, n = 50", gp = gpar(fontsize = 20)))

p_combined <- plot_grid(plot_hurdleQap_lambda, legend_b, ncol = 2, rel_widths = c(1, .2))



pdf("Plots/TrueEstimatedCoefficents_hurdleQap_n200_lambda.pdf", width = 15, height = 13)
grid.draw(p_combined) 
dev.off()

#_________________________________

#Scatterplot mit wahrem x_dist_binom vs. x_numPub_binom
#Scatterplot mit wahrem x_dist_pois vs. x_numPub_pois


# plot
# evaluate
# binomial
v1 <- unlist(list.select(allpvalues_hurdleQap, xbin_dist$pgreq))
v2 <- unlist(list.select(allpvalues_hurdleQap, xbin_dist$pleeq))
p_bin_dist <- ifelse(v1 < v2, v1, v2)*2
sum(p_bin_dist <= 0.05)/length(p_bin_dist)

v1 <- unlist(list.select(allpvalues_hurdleQap, xbin_pub$pgreq))
v2 <- unlist(list.select(allpvalues_hurdleQap, xbin_pub$pleeq))
p_bin_pub <- ifelse(v1 < v2, v1, v2)*2
sum(p_bin_pub <= 0.05)/length(p_bin_pub)

# #poisson
v1 <- unlist(list.select(allpvalues_hurdleQap, xpois_dist$pgreq))
v2 <- unlist(list.select(allpvalues_hurdleQap, xpois_dist$pleeq))
p_pois_dist <- ifelse(v1 < v2, v1, v2)*2
sum(p_pois_dist <= 0.05)/length(p_pois_dist)

v1 <- unlist(list.select(allpvalues_hurdleQap, xpois_pub$pgreq))
v2 <- unlist(list.select(allpvalues_hurdleQap, xpois_pub$pleeq))
p_pois_pub <- ifelse(v1 < v2, v1, v2)*2
sum(p_pois_pub <= 0.05)/length(p_pois_pub)



### pvalue plot
coefs_bin_dist <- unlist(list.select(allcoefs, beta_binDist))
coefs_bin_pub <- unlist(list.select(allcoefs, beta_binPub))
coefs_pois_dist <- unlist(list.select(allcoefs, beta_poisDist))
coefs_pois_pub <- unlist(list.select(allcoefs, beta_poisPub))

col_bin_dist <- ifelse(p_bin_dist <= 0.05, "red", "blue")
col_bin_pub <- ifelse(p_bin_pub <= 0.05, "red", "blue")
col_pois_dist <- ifelse(p_pois_dist <= 0.05, "red", "blue")
col_pois_pub <- ifelse(p_pois_pub <= 0.05, "red", "blue")

data <- as.data.frame(cbind(coefs_bin_dist, coefs_bin_pub, coefs_pois_dist, coefs_pois_pub, p_bin_dist, p_bin_pub, p_pois_dist, p_pois_pub))
# log
data2 <- as.data.frame(cbind(coefs_bin_dist, coefs_bin_pub, coefs_pois_dist, coefs_pois_pub, 
                             p_bin_dist = log(p_bin_dist + 0.001), p_bin_pub = log(p_bin_pub + 0.001), 
                             p_pois_dist = log(p_pois_dist + 0.001), p_pois_pub = log(p_pois_pub + 0.001)))

data2 <- data2[order(data2$p_bin_dist, decreasing = FALSE),]

g_bin_dist <- ggplot(data2, aes(x = coefs_bin_dist, y = coefs_bin_pub)) +
  geom_point(aes(col = p_bin_dist), size = 2.2, alpha = 0.9) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.6) +
  scale_color_gradient(low = "lightblue", high = "darkblue", limits = c(-7, 0.001), name = bquote(log(p[binDist])))+
  labs(x = bquote(beta[binDist]),
       y = bquote(beta[binPub]),
       title = bquote(beta[binDist])) +
  theme(axis.text = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1.5)),
        legend.position = "bottom",
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.5)))

data2 <- data2[order(data2$p_bin_pub, decreasing = FALSE),]


g_bin_pub <- ggplot(data2, aes(x = coefs_bin_dist, y = coefs_bin_pub)) +
  geom_point(aes(col = p_bin_pub), size = 2.2, alpha = 0.9) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.6) +
  scale_color_gradient(low = "lightblue", high = "darkblue", limits = c(-7, 0.001), name = bquote(log(p[binPub])))+
  labs(x = bquote(beta[binDist]),
       y = bquote(beta[binPub]),
       title = bquote(beta[binPub])) +
  theme(axis.text = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1.5)),
        legend.position = "bottom",
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.5)))

# poisson

data2 <- data2[order(data2$p_pois_dist, decreasing = FALSE),]

g_pois_dist <- ggplot(data2, aes(x = coefs_pois_dist, y = coefs_pois_pub)) +
  geom_point(aes(col = p_pois_dist), size = 2.2, alpha = 0.9) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.6) +
  scale_color_gradient(low = "lightblue", high = "darkblue", limits = c(-7, 0.001), name = bquote(log(p[poisDist])))+
  labs(x = bquote(beta[poisDist]),
       y = bquote(beta[poisPub]),
       title = bquote(beta[poisDist])) +
  theme(axis.text = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1.5)),
        legend.position = "bottom",
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.5)))

data2 <- data2[order(data2$p_pois_pub, decreasing = FALSE),]

g_pois_pub <- ggplot(data2, aes(x = coefs_pois_dist, y = coefs_pois_pub)) +
  geom_point(aes(col = p_pois_pub), size = 2.2, alpha = 0.9) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.6) +
  scale_color_gradient(low = "lightblue", high = "darkblue", limits = c(-7, 0.001), name = bquote(log(p[poisPub])))+
  labs(x = bquote(beta[poisDist]),
       y = bquote(beta[poisPub]),
       title = bquote(beta[poisPub])) +
  theme(axis.text = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1.5)),
        legend.position = "bottom",
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.5)))


pdf("Plots/plot_hurdleQap_pvalues_n200.pdf", width = 13, height = 13)
plot_hurdleQap_pvalues <- grid.arrange(g_bin_dist, g_bin_pub, g_pois_dist, g_pois_pub, ncol = 2,
                                       top = textGrob("Two-sided hurdle-QAP p-values for coefficient pairs, n = 200", gp = gpar(fontsize = 20)))

grid.draw(plot_hurdleQap_pvalues) 
dev.off()

#--------  Plots für hurdle (ohne QAP!)------------------------------
# 
# Darstellung wahre gegen geschätzte Koeffizienten

# pvalue vectors
# binomial
p_bin_dist <- unlist(list.select(allpvalues_hurdle, bin_dist))
p_bin_pub <- unlist(list.select(allpvalues_hurdle, bin_pub))

# #poisson
p_pois_dist <- unlist(list.select(allpvalues_hurdle, pois_dist))
p_pois_pub <- unlist(list.select(allpvalues_hurdle, pois_pub))

### true coefficients
coefs_bin_dist <- unlist(list.select(allcoefs, beta_binDist))
coefs_bin_pub <- unlist(list.select(allcoefs, beta_binPub))
coefs_pois_dist <- unlist(list.select(allcoefs, beta_poisDist))
coefs_pois_pub <- unlist(list.select(allcoefs, beta_poisPub))

col_bin_dist <- ifelse(p_bin_dist <= 0.05, "red", "blue")
col_bin_pub <- ifelse(p_bin_pub <= 0.05, "red", "blue")
col_pois_dist <- ifelse(p_pois_dist <= 0.05, "red", "blue")
col_pois_pub <- ifelse(p_pois_pub <= 0.05, "red", "blue")

# bin_dist
x <-  unlist(list.select(allcoefs, beta_binDist))
y <-  unlist(list.select(allcoefsHurdle, bin_dist))
p <- log(p_bin_dist + 0.001)
dat <- as.data.frame(cbind(x, y, p))
dat <- dat[order(dat$p, decreasing = FALSE),]

g_bin_dist <- ggplot(dat, aes(x = x, y = y)) +
  geom_point(aes(col = p), size = 2.2, alpha = 0.9) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.7) +
  scale_color_gradient(low = "lightblue", high = "darkblue", limits = c(-7, 0.001), name =  bquote(log(p[binDist]))) + 
  labs(x = "",
       y = "estimated",
       title = bquote(beta[binDist])) +
  theme(axis.text = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1.5)),
        legend.position = "bottom",
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.5))) +
  ylim(-0.004,0.003)

# bin_pub
x <-  unlist(list.select(allcoefs, beta_binPub))
y <-  unlist(list.select(allcoefsHurdle, bin_pub))
p <- log(p_bin_pub + 0.001)
dat <- as.data.frame(cbind(x, y, p))
dat <- dat[order(dat$p, decreasing = FALSE),]

g_bin_pub <- ggplot(dat, aes(x = x, y = y)) +
  geom_point(aes(col = p), size = 2.2, alpha = 0.9) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.7) +
  scale_color_gradient(low = "lightblue", high = "darkblue", limits = c(-7, 0.001), name = bquote(log(p[binPub]))) +
  labs(x = "",
       y = "",
       title =  bquote(beta[binPub])) +
  theme(axis.text = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1.5)),
        legend.position = "bottom",
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.5))) +
  ylim(-0.004,0.003)

# pois_dist
x <-  unlist(list.select(allcoefs, beta_poisDist))
y <-  unlist(list.select(allcoefsHurdle, pois_dist))
p <- log(p_pois_dist + 0.001)
dat <- as.data.frame(cbind(x, y, p))
dat <- dat[order(dat$p, decreasing = FALSE),]

g_pois_dist <- ggplot(dat, aes(x = x, y = y)) +
  geom_point(aes(col = p), size = 2.2, alpha = 0.9) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.7) +
  scale_color_gradient(low = "lightblue", high = "darkblue", limits = c(-7, 0.001), name = bquote(log(p[poisDist]))) +
  labs(x = "true",
       y = "estimated",
       title =   bquote(beta[poisDist])) +
  theme(axis.text = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1.5)),
        legend.position = "bottom",
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.5))) +
  ylim(-0.004,0.003)

# pois_pub
x <-  unlist(list.select(allcoefs, beta_poisPub))
y <-  unlist(list.select(allcoefsHurdle, pois_pub))
p <- log(p_pois_pub + 0.001)
dat <- as.data.frame(cbind(x, y, p))
dat <- dat[order(dat$p, decreasing = FALSE),]

g_pois_pub <- ggplot(dat, aes(x = x, y = y)) +
  geom_point(aes(col = p), size = 2.2, alpha = 0.9) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.7) +
  scale_color_gradient(low = "lightblue", high = "darkblue", limits = c(-7, 0.001), name =  bquote(log(p[poisPub]))) +
  labs(x = "true",
       y = "",
       title = bquote(beta[poisPub])) +
  theme(axis.text = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1.5)),
        legend.position = "bottom",
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.5))) +
  ylim(-0.004,0.003)

pdf("Plots/TrueEstimatedCoefficents_hurdle_n200.pdf", width = 13, height = 13)
plot_hurdle_estimated <- grid.arrange(g_bin_dist, g_bin_pub, g_pois_dist, g_pois_pub, ncol = 2,
                                         top = textGrob("True vs. estimated hurdle coefficients, n = 200", gp = gpar(fontsize = 20)))

grid.draw(plot_hurdle_estimated) 
dev.off()

#_____________________________
# color according to mean lambda value
# bin_dist
x <-  unlist(list.select(allcoefs, beta_binDist))
y <-  unlist(list.select(allcoefsHurdle, bin_dist))
p <- log(p_bin_dist + 0.001)
l <- unlist(allmeanLambda)
dat <- as.data.frame(cbind(x, y, p, l))
dat <- dat[order(dat$l, decreasing = FALSE),]

g_bin_dist <- ggplot(dat, aes(x = x, y = y)) +
  geom_point(aes(col = l), size = 2.2, alpha = 0.9) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.7) +
  scale_color_gradient2(low = "yellow", mid = "white", high = "red", limits = c(1,35), name = bquote(mean(lambda))) +
  labs(x = "",
       y = "estimated",
       title = bquote(beta[binDist])) +
  theme(axis.text = element_text(size = rel(1.2)),
        legend.position = "none",
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.5))) +
  ylim(-0.004, 0.003)

# bin_pub
x <-  unlist(list.select(allcoefs, beta_binPub))
y <-  unlist(list.select(allcoefsHurdle, bin_pub))
p <- log(p_bin_pub + 0.001)
l <- unlist(allmeanLambda)
dat <- as.data.frame(cbind(x, y, p, l))
dat <- dat[order(dat$l, decreasing = FALSE),]

g_bin_pub <- ggplot(dat, aes(x = x, y = y)) +
  geom_point(aes(col = l), size = 2.2, alpha = 0.9) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.7) +
  scale_color_gradient2(low = "yellow", mid = "white", high = "red", limits = c(1,35), name = bquote(mean(lambda))) +
  labs(x = "",
       y = "",
       title =  bquote(beta[binPub])) +
  theme(axis.text = element_text(size = rel(1.2)),
        legend.position = "none",
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.5))) +
  ylim(-0.004, 0.003)

# pois_dist
x <-  unlist(list.select(allcoefs, beta_poisDist))
y <-  unlist(list.select(allcoefsHurdle, pois_dist))
l <- unlist(allmeanLambda)
dat <- as.data.frame(cbind(x, y, p, l))
dat <- dat[order(dat$l, decreasing = FALSE),]

g_pois_dist <- ggplot(dat, aes(x = x, y = y)) +
  geom_point(aes(col = l), size = 2.2, alpha = 0.9) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.7) +
  scale_color_gradient2(low = "yellow", mid = "white", high = "red", limits = c(1,35), name = bquote(mean(lambda))) +
  labs(x = "true",
       y = "estimated",
       title = bquote(beta[poisDist])) +
  theme(axis.text = element_text(size = rel(1.2)),
        legend.position = "none",
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.5))) +
  ylim(-0.004, 0.003)

# pois_pub
x <-  unlist(list.select(allcoefs, beta_poisPub))
y <-  unlist(list.select(allcoefsHurdle, pois_pub))
p <- log(p_pois_pub + 0.001)
l <- unlist(allmeanLambda)
dat <- as.data.frame(cbind(x, y, p, l))
dat <- dat[order(dat$l, decreasing = FALSE),]

g_pois_pub <- ggplot(dat, aes(x = x, y = y)) +
  geom_point(aes(col = l), size = 2.2, alpha = 0.9) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.7) +
  scale_color_gradient2(low = "yellow", mid = "white", high = "red", limits = c(1,35), name = bquote(mean(lambda))) +
  labs(x = "true",
       y = "",
       title =  bquote(beta[poisPub])) +
  theme(axis.text = element_text(size = rel(1.2)),
        legend.position = "none",
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.5))) +
  ylim(-0.004, 0.003)


p_legend <- ggplot(dat, aes(x = x, y = y)) +
  geom_point(aes(col = l), size = 2.2, alpha = 0.9) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.7) +
  scale_color_gradient2(low = "yellow", mid = "white", high = "red", limits = c(1,35), 
                        breaks = c(1, 10, 20, 30),
                        labels = c(1, 10, 20, 30),
                        name = bquote(mean(lambda))) +
  labs(x = "true",
       y = "",
       title =  bquote(beta[poisPub])) +
  theme(axis.text = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.5))) +
  ylim(-0.004, 0.003)

legend_b <- get_legend(p_legend)


plot_hurdle_lambda <- grid.arrange(g_bin_dist, g_bin_pub, g_pois_dist, g_pois_pub, ncol = 2,
                                      top = textGrob("True vs. estimated hurdle coefficients, n = 200", gp = gpar(fontsize = 20)))

p_combined <- plot_grid(plot_hurdle_lambda, legend_b, ncol = 2, rel_widths = c(1, .2))



pdf("Plots/TrueEstimatedCoefficents_hurdle_n200_lambda.pdf", width = 15, height = 13)
grid.draw(p_combined) 
dev.off()

#_________________________________
#Scatterplot mit wahrem x_dist_binom vs. x_numPub_binom
#Scatterplot mit wahrem x_dist_pois vs. x_numPub_pois


data <- as.data.frame(cbind(coefs_bin_dist, coefs_bin_pub, coefs_pois_dist, coefs_pois_pub, p_bin_dist, p_bin_pub, p_pois_dist, p_pois_pub))
# log
data2 <- as.data.frame(cbind(coefs_bin_dist, coefs_bin_pub, coefs_pois_dist, coefs_pois_pub, 
                             p_bin_dist = log(p_bin_dist + 0.001), p_bin_pub = log(p_bin_pub + 0.001), 
                             p_pois_dist = log(p_pois_dist + 0.001), p_pois_pub = log(p_pois_pub + 0.001)))

data2 <- data2[order(data2$p_bin_dist, decreasing = FALSE),]

g_bin_dist <- ggplot(data2, aes(x = coefs_bin_dist, y = coefs_bin_pub)) +
  geom_point(aes(col = p_bin_dist), size = 2.2, alpha = 0.9) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.6) +
  scale_color_gradient(low = "lightblue", high = "darkblue", limits = c(-7, 0.001), name = bquote(log(p[binDist])))+
  labs(x = bquote(beta[binDist]),
       y = bquote(beta[binPub]),
       title = bquote(beta[binDist])) +
  theme(axis.text = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1.5)),
        legend.position = "bottom",
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.5)))

data2 <- data2[order(data2$p_bin_pub, decreasing = FALSE),]


g_bin_pub <- ggplot(data2, aes(x = coefs_bin_dist, y = coefs_bin_pub)) +
  geom_point(aes(col = p_bin_pub), size = 2.2, alpha = 0.9) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.6) +
  scale_color_gradient(low = "lightblue", high = "darkblue", limits = c(-7, 0.001), name = bquote(log(p[binPub])))+
  labs(x = bquote(beta[binDist]),
       y = bquote(beta[binPub]),
       title = bquote(beta[binPub])) +
  theme(axis.text = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1.5)),
        legend.position = "bottom",
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.5)))

# poisson

data2 <- data2[order(data2$p_pois_dist, decreasing = FALSE),]

g_pois_dist <- ggplot(data2, aes(x = coefs_pois_dist, y = coefs_pois_pub)) +
  geom_point(aes(col = p_pois_dist), size = 2.2, alpha = 0.9) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.6) +
  scale_color_gradient(low = "lightblue", high = "darkblue", limits = c(-7, 0.001), name = bquote(log(p[poisDist])))+
  labs(x = bquote(beta[poisDist]),
       y = bquote(beta[poisPub]),
       title = bquote(beta[poisDist])) +
  theme(axis.text = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1.5)),
        legend.position = "bottom",
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.5)))

data2 <- data2[order(data2$p_pois_pub, decreasing = FALSE),]

g_pois_pub <- ggplot(data2, aes(x = coefs_pois_dist, y = coefs_pois_pub)) +
  geom_point(aes(col = p_pois_pub), size = 2.2, alpha = 0.9) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.6) +
  scale_color_gradient(low = "lightblue", high = "darkblue", limits = c(-7, 0.001), name = bquote(log(p[poisPub])))+
  labs(x = bquote(beta[poisDist]),
       y = bquote(beta[poisPub]),
       title = bquote(beta[poisPub])) +
  theme(axis.text = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1.5)),
        legend.position = "bottom",
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.5)))

pdf("Plots/plot_hurdle_pvalues_n200.pdf", width = 13, height = 13)
plot_hurdle_pvalues <- grid.arrange(g_bin_dist, g_bin_pub, g_pois_dist, g_pois_pub, ncol = 2,
                                    top = textGrob("Original hurdle p-values for coefficient pairs, n = 200", gp = gpar(fontsize = 20)))

grid.draw(plot_hurdle_pvalues) 
dev.off()


#________________________________________________________
#--------  Plots für poissonQAP --------------- -------------------------


v1 <- unlist(list.select(allpvalues_poissonQap, x_dist$pgreq))
v2 <- unlist(list.select(allpvalues_poissonQap, x_dist$pleeq))
p_dist <- ifelse(v1 < v2, v1, v2)*2
sum(p_dist <= 0.05)/length(p_dist)

v1 <- unlist(list.select(allpvalues_poissonQap, x_pub$pgreq))
v2 <- unlist(list.select(allpvalues_poissonQap, x_pub$pleeq))
p_pub <- ifelse(v1 < v2, v1, v2)*2
sum(p_pub <= 0.05)/length(p_pub)

coefs_bin_dist <- unlist(list.select(allcoefs, beta_binDist))
coefs_bin_pub <- unlist(list.select(allcoefs, beta_binPub))
coefs_pois_dist <- unlist(list.select(allcoefs, beta_poisDist))
coefs_pois_pub <- unlist(list.select(allcoefs, beta_poisPub))

col_dist <- ifelse(p_dist <= 0.05, "red", "blue")
col_pub <- ifelse(p_pub <= 0.05, "red", "blue")


# pois_dist
x <-  unlist(list.select(allcoefs, beta_poisDist))
y <-  unlist(list.select(allcoefsPoissonQap, xpois_dist))
p <- log(p_dist + 0.001)
dat <- as.data.frame(cbind(x, y, p))
dat <- dat[order(dat$p, decreasing = FALSE),]

g_pois_dist <- ggplot(dat, aes(x = x, y = y)) +
  geom_point(aes(col = p), size = 2.2, alpha = 0.9) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.7) +
  scale_color_gradient(low = "lightblue", high = "darkblue", limits = c(-7, 0.001), name = bquote(log(p[Dist]))) + 
  labs(x = "true",
       y = "estimated",
       title = bquote(beta[poisDist])) +
  theme(axis.text = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1.5)),
        legend.position = "bottom",
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.5)))

# pois_pub
x <-  unlist(list.select(allcoefs, beta_poisPub))
y <-  unlist(list.select(allcoefsPoissonQap, xpois_pub))
p <- log(p_pub + 0.001)
dat <- as.data.frame(cbind(x, y, p))
dat <- dat[order(dat$p, decreasing = FALSE),]

g_pois_pub <- ggplot(dat, aes(x = x, y = y)) +
  geom_point(aes(col = p), size = 2.2, alpha = 0.9) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.7) +
  scale_color_gradient(low = "lightblue", high = "darkblue", limits = c(-7, 0.001), name = bquote(log(p[Pub]))) +
  labs(x = "true",
       y = "",
       title =  bquote(beta[poisPub])) +
  theme(axis.text = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1.5)),
        legend.position = "bottom",
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.5)))


pdf("Plots/TrueEstimatedCoefficents_poissonQap_n200.pdf", width = 13, height = 6)
plot_poissonQap_estimated <- grid.arrange(g_pois_dist, g_pois_pub, ncol = 2,
                                          top = textGrob("True vs. estimated poisson-QAP coefficients, n = 200", gp = gpar(fontsize = 20)))

grid.draw(plot_poissonQap_estimated) 
dev.off()

#_____________________________
# color according to mean lambda value

# pois_dist
x <-  unlist(list.select(allcoefs, beta_poisDist))
y <-  unlist(list.select(allcoefsPoissonQap, xpois_dist))
p <- log(p_dist + 0.001)
l <- unlist(allmeanLambda)
dat <- as.data.frame(cbind(x, y, p, l))
dat <- dat[order(dat$l, decreasing = FALSE),]

g_pois_dist <- ggplot(dat, aes(x = x, y = y)) +
  geom_point(aes(col = l), size = 2.2, alpha = 0.9) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.7) +
  scale_color_gradient2(low = "yellow", mid = "white", high = "red", limits = c(1,35), name = bquote(mean(lambda))) +
  labs(x = "true",
       y = "estimated",
       title = bquote(beta[poisDist])) +
  theme(axis.text = element_text(size = rel(1.2)),
        legend.position = "none",
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.5)))

# pois_pub
x <-  unlist(list.select(allcoefs, beta_poisPub))
y <-  unlist(list.select(allcoefsPoissonQap, xpois_pub))
p <- log(p_pub + 0.001)
l <- unlist(allmeanLambda)
dat <- as.data.frame(cbind(x, y, p, l))
dat <- dat[order(dat$l, decreasing = FALSE),]

g_pois_pub <- ggplot(dat, aes(x = x, y = y)) +
  geom_point(aes(col = l), size = 2.2, alpha = 0.9) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.7) +
  scale_color_gradient2(low = "yellow", mid = "white", high = "red", limits = c(1,35), name = bquote(mean(lambda))) +
  labs(x = "true",
       y = "",
       title =  bquote(beta[poisPub])) +
  theme(axis.text = element_text(size = rel(1.2)),
        legend.position = "none",
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.5)))


p_legend <- ggplot(dat, aes(x = x, y = y)) +
  geom_point(aes(col = l), size = 2.2, alpha = 0.9) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.7) +
  scale_color_gradient2(low = "yellow", mid = "white", high = "red", limits = c(1,35), 
                        breaks = c(1, 10, 20, 30),
                        labels = c(1, 10, 20, 30),
                        name = bquote(mean(lambda))) +
  labs(x = "true",
       y = "",
       title =  bquote(beta[poisPub])) +
  theme(axis.text = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.5)))

legend_b <- get_legend(p_legend)


plot_poissonQap_lambda <- grid.arrange(g_pois_dist, g_pois_pub, ncol = 2,
                                      top = textGrob("True vs. estimated poisson-QAP coefficients, n = 200", gp = gpar(fontsize = 20)))

p_combined <- plot_grid(plot_poissonQap_lambda, legend_b, ncol = 2, rel_widths = c(1, .2))


pdf("Plots/TrueEstimatedCoefficents_poissonQap_n200_lambda.pdf", width = 15, height = 6)
grid.draw(p_combined) 
dev.off()


#Scatterplot mit wahrem x_dist_binom vs. x_numPub_binom
#Scatterplot mit wahrem x_dist_pois vs. x_numPub_pois


### pvalue plot
# ggplot
data <- as.data.frame(cbind(coefs_bin_dist, coefs_bin_pub, coefs_pois_dist, coefs_pois_pub, p_dist, p_pub))

# log
data2 <- as.data.frame(cbind(coefs_bin_dist, coefs_bin_pub, coefs_pois_dist, coefs_pois_pub,
                             p_dist = log(p_dist + 0.001), p_pub = log(p_pub + 0.001)))

data2 <- data2[order(data2$p_dist, decreasing = FALSE),]

g_bin_dist <- ggplot(data2, aes(x = coefs_bin_dist, y = coefs_bin_pub)) +
  geom_point(aes(col = p_dist), size = 2.2, alpha = 0.9) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.6) +
  scale_color_gradient(low = "lightblue", high = "darkblue", limits = c(-7, 0.001), name = bquote(log(p[Dist]))) +
  labs(x = bquote(beta[binDist]),
       y = bquote(beta[binPub]),
       title = bquote(beta[Dist])) +
  theme(axis.text = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1.5)),
        legend.position = "bottom",
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.5)))

data2 <- data2[order(data2$p_pub, decreasing = FALSE),]


g_bin_pub <- ggplot(data2, aes(x = coefs_bin_dist, y = coefs_bin_pub)) +
  geom_point(aes(col = p_pub), size = 2.2, alpha = 0.9) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.6) +
  scale_color_gradient(low = "lightblue", high = "darkblue", limits = c(-7, 0.001), name = bquote(log(p[Pub]))) +
  labs(x = bquote(beta[binDist]),
       y = bquote(beta[binPub]),
       title = bquote(beta[Pub])) +
  theme(axis.text = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1.5)),
        legend.position = "bottom",
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.5)))

# poisson

data2 <- data2[order(data2$p_dist, decreasing = FALSE),]

g_pois_dist <- ggplot(data2, aes(x = coefs_pois_dist, y = coefs_pois_pub)) +
  geom_point(aes(col = p_dist), size = 2.2, alpha = 0.9) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.6) +
  scale_color_gradient(low = "lightblue", high = "darkblue", limits = c(-7, 0.001), name = bquote(log(p[Dist]))) +
  labs(x = bquote(beta[poisDist]),
       y = bquote(beta[poisPub]),
       title =bquote(beta[Dist])) +
  theme(axis.text = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1.5)),
        legend.position = "bottom",
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.5)))

data2 <- data2[order(data2$p_pub, decreasing = FALSE),]

g_pois_pub <- ggplot(data2, aes(x = coefs_pois_dist, y = coefs_pois_pub)) +
  geom_point(aes(col = p_pub), size = 2.2, alpha = 0.9) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.6) +
  scale_color_gradient(low = "lightblue", high = "darkblue", limits = c(-7, 0.001), name = bquote(log(p[Pub]))) +
  labs(x = bquote(beta[poisDist]),
       y = bquote(beta[poisPub]),
       title = bquote(beta[Pub])) +
  theme(axis.text = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1.5)),
        legend.position = "bottom",
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.5)))


pdf("Plots/plot_poissonQap_pvalues_n200.pdf", width = 13, height = 13)
plot_poissonQap_pvalues <- grid.arrange(g_bin_dist, g_bin_pub, g_pois_dist, g_pois_pub, ncol = 2,
                                        top = textGrob("Two-sided poisson-QAP p-values for coefficient pairs, n = 200", gp = gpar(fontsize = 20)))

grid.draw(plot_poissonQap_pvalues) 
dev.off()

#____________________________________
#--------  Plots for poisson (ohne QAP!) ------  ----------------

p_dist <- unlist(list.select(allpvalues_poisson, pois_dist))
p_pub <- unlist(list.select(allpvalues_poisson, pois_pub))

### pvalue plot
coefs_bin_dist <- unlist(list.select(allcoefs, beta_binDist))
coefs_bin_pub <- unlist(list.select(allcoefs, beta_binPub))
coefs_pois_dist <- unlist(list.select(allcoefs, beta_poisDist))
coefs_pois_pub <- unlist(list.select(allcoefs, beta_poisPub))

col_dist <- ifelse(p_dist <= 0.05, "red", "blue")
col_pub <- ifelse(p_pub <= 0.05, "red", "blue")


# pois_dist
x <-  unlist(list.select(allcoefs, beta_poisDist))
y <-  unlist(list.select(allcoefsPoisson, pois_dist))
p <- log(p_dist + 0.001)
dat <- as.data.frame(cbind(x, y, p))
dat <- dat[order(dat$p, decreasing = FALSE),]

g_pois_dist <- ggplot(dat, aes(x = x, y = y)) +
  geom_point(aes(col = p), size = 2.2, alpha = 0.9) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.7) +
  scale_color_gradient(low = "lightblue", high = "darkblue", limits = c(-7, 0.001), name = bquote(log(p[Dist]))) + 
  labs(x = "true",
       y = "estimated",
       title = bquote(beta[poisDist])) +
  theme(axis.text = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1.5)),
        legend.position = "bottom",
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.5)))

# pois_pub
x <-  unlist(list.select(allcoefs, beta_poisPub))
y <-  unlist(list.select(allcoefsPoisson, pois_pub))
p <- log(p_pub + 0.001)
dat <- as.data.frame(cbind(x, y, p))
dat <- dat[order(dat$p, decreasing = FALSE),]

g_pois_pub <- ggplot(dat, aes(x = x, y = y)) +
  geom_point(aes(col = p), size = 2.2, alpha = 0.9) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.7) +
  scale_color_gradient(low = "lightblue", high = "darkblue", limits = c(-7, 0.001), name = bquote(log(p[Pub]))) +
  labs(x = "true",
       y = "",
       title =  bquote(beta[poisPub])) +
  theme(axis.text = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1.5)),
        legend.position = "bottom",
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.5)))


pdf("Plots/TrueEstimatedCoefficents_poisson_n200.pdf", width = 13, height = 6)
plot_poisson_estimated <- grid.arrange(g_pois_dist, g_pois_pub, ncol = 2,
                                          top = textGrob("True vs. estimated poisson coefficients, n = 200", gp = gpar(fontsize = 20)))

grid.draw(plot_poisson_estimated) 
dev.off()

#_____________________________
# color according to mean lambda value

# pois_dist
x <-  unlist(list.select(allcoefs, beta_poisDist))
y <-  unlist(list.select(allcoefsPoisson, pois_dist))
p <- log(p_dist + 0.001)
l <- unlist(allmeanLambda)
dat <- as.data.frame(cbind(x, y, p, l))
dat <- dat[order(dat$l, decreasing = FALSE),]

g_pois_dist <- ggplot(dat, aes(x = x, y = y)) +
  geom_point(aes(col = l), size = 2.2, alpha = 0.9) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.7) +
  scale_color_gradient2(low = "yellow", mid = "white", high = "red", limits = c(1,35), name = bquote(mean(lambda))) +
  labs(x = "true",
       y = "estimated",
       title = bquote(beta[poisDist])) +
  theme(axis.text = element_text(size = rel(1.2)),
        legend.position = "none",
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.5)))

# pois_pub
x <-  unlist(list.select(allcoefs, beta_poisPub))
y <-  unlist(list.select(allcoefsPoisson, pois_pub))
p <- log(p_pub + 0.001)
l <- unlist(allmeanLambda)
dat <- as.data.frame(cbind(x, y, p, l))
dat <- dat[order(dat$l, decreasing = FALSE),]

g_pois_pub <- ggplot(dat, aes(x = x, y = y)) +
  geom_point(aes(col = l), size = 2.2, alpha = 0.9) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.7) +
  scale_color_gradient2(low = "yellow", mid = "white", high = "red", limits = c(1,35), name = bquote(mean(lambda))) +
  labs(x = "true",
       y = "",
       title =  bquote(beta[poisPub])) +
  theme(axis.text = element_text(size = rel(1.2)),
        legend.position = "none",
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.5)))

p_legend <- ggplot(dat, aes(x = x, y = y)) +
  geom_point(aes(col = l), size = 2.2, alpha = 0.9) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.7) +
  scale_color_gradient2(low = "yellow", mid = "white", high = "red", limits = c(1,35), 
                        breaks = c(1, 10, 20, 30),
                        labels = c(1, 10, 20, 30),
                        name = bquote(mean(lambda))) +
  labs(x = "true",
       y = "estimated",
       title =  bquote(beta[poisPub])) +
  theme(axis.text = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.5)))


legend_b <- get_legend(p_legend)

plot_poisson_lambda <- grid.arrange(g_pois_dist, g_pois_pub, ncol = 2,
                                       top = textGrob("True vs. estimated poisson coefficients, n = 200", gp = gpar(fontsize = 20)))

p_combined <- plot_grid(plot_poisson_lambda, legend_b, ncol = 2, rel_widths = c(1, .2))

pdf("Plots/TrueEstimatedCoefficents_poisson_n200_lambda.pdf", width = 15, height = 6)
grid.draw(p_combined) 
dev.off()


# ggplot
data <- as.data.frame(cbind(coefs_bin_dist, coefs_bin_pub, coefs_pois_dist, coefs_pois_pub, p_dist, p_pub))

# log
data2 <- as.data.frame(cbind(coefs_bin_dist, coefs_bin_pub, coefs_pois_dist, coefs_pois_pub,
                             p_dist = log(p_dist + 0.001), p_pub = log(p_pub + 0.001)))

data2 <- data2[order(data2$p_dist, decreasing = FALSE),]

g_bin_dist <- ggplot(data2, aes(x = coefs_bin_dist, y = coefs_bin_pub)) +
  geom_point(aes(col = p_dist), size = 2.2, alpha = 0.9) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.6) +
  scale_color_gradient(low = "lightblue", high = "darkblue", limits = c(-7, 0.001), name = bquote(log(p[Dist]))) +
  labs(x = bquote(beta[binDist]),
       y = bquote(beta[binPub]),
       title = bquote(beta[Dist])) +
  theme(axis.text = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1.5)),
        legend.position = "bottom",
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.5)))

data2 <- data2[order(data2$p_pub, decreasing = FALSE),]


g_bin_pub <- ggplot(data2, aes(x = coefs_bin_dist, y = coefs_bin_pub)) +
  geom_point(aes(col = p_pub), size = 2.2, alpha = 0.9) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.6) +
  scale_color_gradient(low = "lightblue", high = "darkblue", limits = c(-7, 0.001), name = bquote(log(p[Pub]))) +
  labs(x = bquote(beta[binDist]),
       y = bquote(beta[binPub]),
       title = bquote(beta[Pub])) +
  theme(axis.text = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1.5)),
        legend.position = "bottom",
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.5)))

# poisson

data2 <- data2[order(data2$p_dist, decreasing = FALSE),]

g_pois_dist <- ggplot(data2, aes(x = coefs_pois_dist, y = coefs_pois_pub)) +
  geom_point(aes(col = p_dist), size = 2.2, alpha = 0.9) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.6) +
  scale_color_gradient(low = "lightblue", high = "darkblue", limits = c(-7, 0.001), name = bquote(log(p[Dist]))) +
  labs(x = bquote(beta[poisDist]),
       y = bquote(beta[poisPub]),
       title = bquote(beta[Dist])) +
  theme(axis.text = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1.5)),
        legend.position = "bottom",
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.5)))

data2 <- data2[order(data2$p_pub, decreasing = FALSE),]

g_pois_pub <- ggplot(data2, aes(x = coefs_pois_dist, y = coefs_pois_pub)) +
  geom_point(aes(col = p_pub), size = 2.2, alpha = 0.9) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.6) +
  scale_color_gradient(low = "lightblue", high = "darkblue", limits = c(-7, 0.001), name = bquote(log(p[Pub]))) +
  labs(x = bquote(beta[poisDist]),
       y = bquote(beta[poisPub]),
       title = bquote(beta[Pub])) +
  theme(axis.text = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1.5)),
        legend.position = "bottom",
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.5)))



pdf("Plots/plot_poisson_pvalues_n200.pdf", width = 13, height = 13)
plot_poisson_pvalues <- grid.arrange(g_bin_dist, g_bin_pub, g_pois_dist, g_pois_pub, ncol = 2,
                                     top = textGrob("Original poisson p-values for coefficient pairs, n = 200", gp = gpar(fontsize = 20)))

grid.draw(plot_poisson_pvalues) 
dev.off()

