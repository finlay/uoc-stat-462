library(ggplot2)
library(data.table)

library(tree)
library(randomForest)
library(gbm3)

library(ISLR)
attach(Carseats)


ggplot(Carseats, aes(x = Price, y = Age, color = Sales)) +
  geom_point(size = 2) +
  scale_color_viridis_c(option = "D") +
  labs(
    title = "Segmenting the feature space",
    x = "Price",
    y = "Age",
    color = "Sales"
  ) +
  theme_gray()

set.seed(1)

train_indices <-
  sample(1:nrow(Carseats), nrow(Carseats) * 0.8)
train <- Carseats[train_indices, ]
test <- Carseats[-train_indices, ]


## Simple tree
numeric.reg.tree <- tree(Sales ~ Price + Age, train)

age_seq   <- seq(min(Carseats$Age),   max(Carseats$Age),   by=0.1)
price_seq <- seq(min(Carseats$Price), max(Carseats$Price), by=0.1)
grid <- expand.grid(Price=price_seq, Age=age_seq)
grid$predicted <- predict(numeric.reg.tree, newdata=grid)

ggplot(Carseats, aes(x=Price, y=Age, color=Sales)) +
  geom_point(size=2) +
  geom_contour(data=grid,
    aes(x=Price, y=Age, z=as.numeric(as.factor((predicted)))),
    color="black", linewidth=0.6) +
  scale_color_viridis_c(option = "D") +
  labs(
    title = "Segmenting the feature space",
    x     = "Price",
    y     = "Age",
    color = "Sales"
  ) +
  theme_gray()


numeric.reg.tree
summary(numeric.reg.tree)

plot(numeric.reg.tree)
text(numeric.reg.tree, pretty = 0, cex = 0.7)


categorical.reg.tree <- tree(Sales ~ Price + ShelveLoc, train)

plot(categorical.reg.tree)
text(categorical.reg.tree, pretty = 0, cex = 0.7)


## Convert Sales to factor
median_sales <- median(Carseats$Sales)
train$new_sales <-
  factor(ifelse(train$Sales >= median_sales, 'High', 'Low'))

classification.tree <- tree(new_sales ~ . -Sales, train)

# Plot the tree
plot(classification.tree)
text(classification.tree, pretty = 0, cex = 0.5)

summary(classification.tree)

test$new_sales <- factor(ifelse(test$Sales >= median_sales, 'High', 'Low'))

# Make predictions on test data
classification.test.pred <-
  predict(classification.tree, newdata = test, type = 'class')

# Compute the error rate
classification.er <- mean(classification.test.pred != test$new_sales)

# Output the error rate
classification.er

## Pruning

set.seed(10)
cv.classification <- cv.tree(classification.tree, FUN = prune.misclass)

plot(cv.classification$size, cv.classification$dev, type = "b")

# Find out the best tree size that corresponds to the lowest deviance
best.size <- cv.classification$size[which.min(cv.classification$dev)]

pruned.tree <-
  prune.misclass(classification.tree, best = best.size)


plot(pruned.tree)
text(pruned.tree, pretty = 0, cex = 0.7)

pruned.test.pred <- predict(pruned.tree, newdata=test, type = "class")

# Compute the error rate
pruned.er <- mean(pruned.test.pred != test$new_sales)

pruned.er


#######################
## Bagging

set.seed(10)

num_predictors <- ncol(train) -2

bag.tree <-
  randomForest(new_sales ~ . -Sales,
               data = train,
               mtry = num_predictors,
               importance=TRUE)

bag.tree

mean(test$new_sales != predict(bag.tree, newdata=test))

bag.tree$importance
varImpPlot(bag.tree)


#######################
## Random forest

set.seed(1)

rf.tree <-
  randomForest(new_sales ~ . -Sales,
               data = train,
               mtry = 3,
               importance=TRUE)

rf.tree

mean(predict(rf.tree, newdata = test) != test$new_sales)

rf.tree$importance
varImpPlot(rf.tree)


mtry.test <-
  rbindlist(lapply(1:10, function(mt) {
    tr <-
      randomForest(new_sales ~ . -Sales,
                   data = train,
                   mtry = mt,
                   importance=TRUE)
    data.table(
      mtry=mt,
      err=mean(predict(tr, newdata=test) != test$new_sales))
  }))

ggplot(mtry.test, aes(x=mtry, y=err)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks=seq(1,10)) +
  ylim(0.15,0.35) +
  labs(
    x     = "mtry",
    y     = "Error"
  ) +
  theme_gray()

library(parallel)
mtry.test.100 <-
  rbindlist(mclapply(1:100,  mc.cores=6, function(i) {
    rbindlist(lapply(1:10, function(mt) {
      tr <-
        randomForest(new_sales ~ . -Sales,
                     data = train,
                     mtry = mt,
                     importance=TRUE)
      data.table(
        g=i,
        mtry=mt,
        err=mean(predict(tr, newdata=test) != test$new_sales))
    }))
  }))

ggplot(mtry.test.100, aes(x=mtry, y=err, group=g)) +
  geom_line(alpha=0.2) +
  geom_point(alpha=0.2) +
  scale_x_continuous(breaks=seq(1,10)) +
  ylim(0.10,0.25) +
  labs(
    x     = "mtry",
    y     = "Error"
  ) +
  theme_gray()


###########################
## Boosted regression tree

set.seed(1)

train$new_sales_num <- ifelse(train$new_sales == "High", 1, 0)
test$new_sales_num  <- ifelse(test$new_sales  == "High", 1, 0)


boosting.model <- gbm(
  new_sales_num ~ . - Sales - new_sales,
  data               = train,
  distribution       = "bernoulli",
  n.trees            = 1000,
  interaction.depth  = 1,
  shrinkage          = 0.1,
  verbose            = FALSE
)

boosting.model
pl <- summary(boosting.model, plot_it=F)

boost.pred <- ifelse(
  predict(boosting.model, newdata = test, n.trees=100) >= 0.5,
  1, 0)

mean(boost.pred != test$new_sales_num)


pl$var <- factor(pl$var, levels=rev(pl$var))
ggplot(pl, aes(x=rel_inf, y=var, fill=rel_inf)) +
  geom_col() +
  guides(fill = "none") +
  labs(
    x     = "Relative importance",
    y     = "Factor"
  ) +
  theme_light()

