library(ggplot2)
library(arules)
library(arulesViz)

num.rules <- function(p) 3^p - 2^(p+1) + 1

ggplot() +
  xlim(c(1, 100)) +
  geom_function(fun=num.rules) +
  labs(
    title=expression(y = 3^p - 2^(p+1) + 1),
    xlab="number of items, p",
    ylab=""
    )


data = read.transactions(file="Basket1.txt",format="basket",sep=",")
inspect(data)

itemFrequencyPlot(data)


itemFrequencyPlot(data, support = 0.4)

itemFrequencyPlot(data, support = 2, type="absolute")

FreqItemsets <-
  apriori(data, parameter=list(
    support=0.4,
    target="frequent itemsets"
  ))
inspect(FreqItemsets)

inspect(sort(FreqItemsets, by="support")[1:5])
two_itemsets <- subset(FreqItemsets, subset=size(items) == 2)

inspect(sort(two_itemsets, by = "support"))
plot(FreqItemsets, method = "graph")


MaxFreqItemsets <-
  apriori(data, parameter=list(
    support=0.3,
    target="maximally frequent itemsets"
  ))
inspect(MaxFreqItemsets)
summary(MaxFreqItemsets)

ClosedFreqItemsets <-
  apriori(data, parameter=list(
    support=0.4,
    target="closed frequent itemsets"))
inspect(ClosedFreqItemsets)
summary(ClosedFreqItemsets)




rules <-
  apriori(data,parameter=list(
    minlen=2,
    support=0.4,
    confidence=0.8,
    target="rules"
  ))
inspect(rules)
summary(rules)

plot(rules, method="paracoord")




rules <-
  apriori(data, parameter=list(
    minlen=2,
    support=0.4,
    confidence=0.8,
    target="rules"
  ), appearance=list(rhs="Milk"))
summary(rules)
inspect(sort(rules, by="confidence"))


groceries_original <- read.csv("Groceries_dataset.csv", header=TRUE)
head(groceries_original)


groceries <- read.transactions(
  "combined_transactions.txt",
  format = "basket",
  sep = ",",
  header = TRUE
)

inspect(head(groceries))

itemFrequencyPlot(groceries, topN = 10)

itemFrequencyPlot(groceries, topN = 100, cex.names = 0.3)




