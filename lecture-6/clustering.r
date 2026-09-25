library(GGally)
library(cluster)

data(flea)
plot_data <- flea[, c("aede1","aede2","species")]


ggplot(plot_data, aes(x = aede1, y = aede2, color = species)) +
  scale_color_manual(values = c("#00AFBB", "#FC4E07", "#e7b800")) +
  geom_point(size = 3.0, alpha = 0.7) +
  labs(
    title = "Ground Truth",
    color = "species"
  ) +
  theme_grey()


ggplot(data = plot_data, mapping = aes(x = aede1, y = aede2)) +
  geom_point(size = 3.0) +
  labs(
    title = "Cluster Data"
  ) +
  theme_grey()


plot_func <- function(data, label, title = "") {

  data$label <- factor(label)

  x_col <- colnames(data)[1]
  y_col <- colnames(data)[2]
  label <- colnames(data)[3]

  return(
    ggplot(data,
           aes(x = .data[[x_col]],
               y = .data[[y_col]],
               color = .data[[label]])
           ) +
    scale_color_manual(
      values = c("#00AFBB", "#FC4E07", "#e7b800",
                 "#006400", "#9400D3", "#FF8C00","#008080")) +
    geom_point(size = 3.0, alpha = 0.7) +
    labs(
      title = title,
      x = x_col,
      y = y_col,
      color = "Cluster Result"
    ) +
    theme_grey()
  )
}


## Simple kmeans
dat <- flea[,c("aede1", "aede2")]
k.clus <- kmeans(dat, 3)
plot_func(dat, k.clus$cluster, "k-means clustering result")

k.clus
k.clus$tot.withinss


## Choose best from 20
k.clus <- kmeans(dat, 3, nstart=20)
plot_func(dat, k.clus$cluster, title = "k-means clustering result")

k.clus
k.clus$tot.withinss


## Scaling
ggplot(data = plot_data, mapping = aes(x = aede1, y = aede2, color = species)) +
  scale_color_manual(values = c("#00AFBB", "#FC4E07", "#e7b800")) +
  geom_point(size = 3.0, alpha = 0.7) +
  labs(
    title = "Ground Truth (Equal Scale on Both Axes)",
    color = "Species"
  ) +
  theme_grey() +
  coord_fixed()

set.seed(1)
scaled_dat <- scale(dat)
k.clus <- kmeans(scaled_dat, 3, nstart = 20)
plot_func(dat, k.clus$cluster, title = "k-means clustering result")

## Silhouette coefficient

plot_dat <-
  do.call('rbind', lapply(2:8, function(k) {
    clus <- kmeans(scaled_dat, k, nstart = 20)
    data.frame(
      k = k,
      kmeans_criterion = 1 - clus$tot.withinss / clus$totss
    )
  }))

ggplot(plot_dat, aes(x = k, y = kmeans_criterion)) +
  geom_line() +
  geom_point() +
  labs(
    title = "k-means criterion VS k",
    x = "Number of Clusters (k)",
    y = "1 - tot.withinss/totss"
  ) +
  theme_grey()


dist_matrix <- dist(scaled_dat)

sil <- silhouette(k.clus$cluster, dist_matrix)

summary(sil)

set.seed(1)

plot_dat <-
  do.call('rbind', lapply(2:8, function(k) {
    clus <- kmeans(scaled_dat, k, nstart = 20)
    sil <- silhouette(clus$cluster, dist_matrix)
    data.frame(
      k = k,
      sils = summary(sil)$avg.width
    )
  }))


ggplot(plot_dat, aes(x = k, y = sils)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Silhouette coefficient VS k",
    x = "Number of clusters (k)",
    y = "Silhouette coefficient"
  ) +
  theme_grey()




### high dimension
euclidean <- function(a) sqrt(sum((a)^2))

dat <-
  do.call('rbind', lapply(1:100, function(dimension) {
    points <- sapply(1:1000, function(i) rnorm(dimension))
    max.dist  <- max(dist(t(points)))
    mean.dist <- mean(dist(t(points)))
    data.frame(
      dimension = dimension,
      max_minus_mean = (max.dist - mean.dist)/max.dist )
  }))

ggplot(dat, aes(x = dimension, y = max_minus_mean)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Difference between max and mean distance of 1000 random vectors",
    x = "Dimension",
    y = "Difference"
  ) +
  theme_grey()



