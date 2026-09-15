library(caret)
library(data.table)

set.seed(462)

load(url("https://www.lock5stat.com/datasets4e/HollywoodMovies.rda"))

data <- as.data.table(HollywoodMovies)
movie <- na.omit(data[, .(Budget, WorldGross=WorldwideBO), Movie])


n <- nrow(movie)
indices <- sample(1:n)

train_size <- floor(0.8 * n)
valid_size <- floor(0.1 * n)

train_indices <- indices[1:train_size]
valid_indices <- indices[(train_size + 1):(train_size + valid_size)]
test_indices <- indices[(train_size + valid_size + 1):n]

train_data <- movie[train_indices, ]
valid_data <- movie[valid_indices, ]
test_data <- movie[test_indices, ]

cv_train <- rbind(train_data, valid_data)


kNN <- function(data="train_data", k, xstar)
  data[, .(dist=abs(Budget - xstar), WorldGross)][
       order(dist) ][1:k, mean(WorldGross) ]

ks = seq(5, 50, by = 5)
valid_mse <- sapply(ks, function(k) {
    predicted <- sapply(
      X = valid_data$Budget,
      FUN = function(xstar)
        kNN(data=train_data, k=k, xstar=xstar)
      )
    mean((valid_data$WorldGross - predicted)^2)
  })

plot(
  ks, valid_mse,
  main = "Validation Performance",
  xlab = "Choice of k",
  ylab = "Validation MSE",
  type = "b",
  pch = 0,
  col = "deepskyblue2",
  lty = 2,
  lwd = 2
)

cv_folds <- createFolds(cv_train$WorldGross, k=10)

valid_Fold1_mse <- sapply(ks, function(k) {
  predicted <- sapply(
    X = cv_train[cv_folds[[1]], ]$Budget,
    FUN = function(xstar) kNN(data=cv_train[-cv_folds[[1]], ], k=k, xstar=xstar)
  )
  mean((cv_train[cv_folds[[1]], ]$WorldGross - predicted)^2)
})

plot(
  ks, valid_Fold1_mse,
  main = "Validation Performance on One Fold",
  xlab = "Choice of k",
  ylab = "Validation Fold 1 MSE",
  type = "b",
  pch = 2,
  col = "darkgoldenrod1",
  lty = 2,
  lwd = 2
)

fold_MSEs <- lapply(seq_along(cv_folds), function(j) {
  sapply(ks, function(k) {
    predicted <- sapply(
      X = cv_train[cv_folds[[j]], ]$Budget,
      FUN = function(xstar)
        kNN(data=cv_train[-cv_folds[[j]], ],
            k=k, xstar=xstar)
    )
    mean((cv_train[cv_folds[[j]], ]$WorldGross -
          predicted)^2)
  })
})

plot(
  ks, fold_MSEs[[1]],
  main = "Validation Performance on All Folds",
  xlab = "Choice of k",
  ylab = "Validation MSE",
  type = "b",
  pch = 2,
  col = "darkgoldenrod1",
  lty = 2,
  lwd = 2,
  ylim = range(unlist(fold_MSEs))
)

# Add MSEs for the remaining folds
for (j in 2:length(cv_folds)) {
  lines(
    ks, fold_MSEs[[j]],
    type = "b",
    pch = 2,
    col = "darkgoldenrod1",
    lty = 2,
    lwd = 2
  )
}

mean_fold_MSEs <- sapply(seq_along(ks), function(i) {
  mean(sapply(fold_MSEs, function(fold) fold[i]))
})

plot(
  ks, mean_fold_MSEs,
  main = "Cross-Validation MSE",
  xlab = "Choice of k",
  ylab = "Validation MSE",
  type = "b",
  pch = 2,
  col = "darkgoldenrod1",
  lty = 2,
  lwd = 2,
  ylim = range(unlist(fold_MSEs))
)

# Add MSEs for the remaining folds
for (j in 1:length(cv_folds)) {
  lines(
    ks, fold_MSEs[[j]],
    type = "b",
    pch = 2,
    col = adjustcolor("darkgoldenrod1", alpha.f = 0.4),
    lty = 2,
    lwd = 1,
  )
}

# Add legend
legend(
  "topright",
  legend = c("Cross-Validation MSE", "Individual Fold MSE"),
  col = c("darkgoldenrod1", adjustcolor("darkgoldenrod1", alpha.f = 0.4)),
  pch = c(2, 2),
  lty = c(2, 2),
  lwd = c(2, 1),
  cex = 0.7
)

k_cv <- ks[which.min(mean_fold_MSEs)]
k_cv

cv_test_pred <- sapply(
  # Predict using validation data
  X = test_data$Budget,
  FUN = function(xstar) kNN(data = cv_train, k = k_cv, xstar = xstar)
)

cv_test_mse <- mean((test_data$WorldGross - cv_test_pred)^2)
cv_test_mse


