library(GGally)
library(cluster)

source('functions.r')

data(flea)

dat <- flea[,c("aede1", "aede2")]
scaled_dat <- data.frame(
  aede1 = scale(dat$aede1),
  aede2 = scale(dat$aede2)
)
dist_matrix <- dist(scaled_dat)

# perform hierarchical clustering
hc.complete <- hclust(dist_matrix, method = "complete")
plot(hc.complete, main = "Complete Linkage", xlab = "", sub = "", cex = .3)


# Multiple clusters, depending on depth
plot(hc.complete, main = "Complete Linkage", xlab = "", sub = "", cex = .3)

abline(h=4, col = "red", lty = 2)
abline(h=2.5, col = "red", lty = 2)
abline(h=1.8, col = "red", lty = 2)
abline(h=1.3, col = "red", lty = 2)
abline(h=0.7, col = "red", lty = 2)
abline(h=0.5, col = "red", lty = 2)


plot(hc.complete, main = "Complete Linkage", xlab = "", sub = "", cex = .3)
rect.hclust(hc.complete, h = 3.6, border = "red")


plot(hc.complete, main = "Complete Linkage", xlab = "", sub = "", cex = .3)
rect.hclust(hc.complete, k = 3, border = "red")


plot_func(scaled_dat, cutree(hc.complete, 3), "Complete Linkage")



hc.single <- hclust(dist_matrix, method = "single")
plot(hc.single, main = "Single Linkage", xlab = "", sub = "", cex = .3)
rect.hclust(hc.single, k = 3, border = "red")

plot_func(scaled_dat, cutree(hc.single, 3), "Single Linkage")


hc.average <- hclust(dist_matrix, method = "average")
plot(hc.average, main = "Average Linkage", xlab = "", sub = "", cex = .3)
rect.hclust(hc.average, k = 3, border = "red")

plot_func(scaled_dat, cutree(hc.average, 3), "Average Linkage")
