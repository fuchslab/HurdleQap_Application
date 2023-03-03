# Bielefeld 
load("~/Workspaces/Permutations/EverythingNeeded_QapGamFunction1_Bielefeld.RData")
library(ggplot2)

x <- dist[upper.tri(dist, diag = FALSE)]
z <- meanPub[upper.tri(meanPub, diag = FALSE)]

data <- as.data.frame(cbind(x,z))

cor(x,z, use = "complete.obs")

g_BI <- ggplot(data, aes(x = x, y = z)) +
  geom_point(alpha = 0.3, col = "darkgrey") +
  labs(x = "distance", y = "meanPub", title = "Correlation Bielefeld University") +
  theme(axis.text = element_text(size = rel(2.2)),
        axis.title = element_text(size = rel(2.5)),
        plot.title = element_text(size = rel(2.5)))


png("Plots/Correlation_dist_meanPub_Bielefeld.png", width = 700, height = 700)
grid.draw(g_BI) 
dev.off()


#---

# Helmholtz 
load("~/Workspaces/Permutations/EverythingNeeded_QapGamFunction1_HMGU.RData")

x <- dist[upper.tri(dist, diag = FALSE)]
z <- meanPub[upper.tri(meanPub, diag = FALSE)]

data <- as.data.frame(cbind(x,z))
data1000 <- data[which(data$x <= 1000), ]
cor(data1000$x, data1000$z, use = "complete.obs")

g_HM <- ggplot(data1000, aes(x = x, y = z)) +
  geom_point(alpha = 0.3, col = "darkgrey") +
  labs(x = "distance", y = "meanPub", title = "Correlation Helmholtz Munich") +
  theme(axis.text = element_text(size = rel(2.2)),
        axis.title = element_text(size = rel(2.5)),
        plot.title = element_text(size = rel(2.5)))


png("Plots/Correlation_dist_meanPub_Helmholtz.png", width = 700, height = 700)
grid.draw(g_HM) 
dev.off()

