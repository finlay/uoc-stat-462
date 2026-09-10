library(ggplot2)
library(data.table)

scores <- read.csv('scores.csv')
x <- scores$x
y <- scores$y

manual_correlation <-
  sum((x - mean(x)) * (y - mean(y))) /
    sqrt(sum((x - mean(x))^2) * sum((y - mean(y))^2))


quick_correlation <- cor(x, y)


ggplot(d, aes(x=x, y=y)) +
  geom_point() +
  xlim(70,100) +
  labs(x="Mark", y="GPA")


set.seed(462)

boot <-
  rbindlist(lapply(1:1000, function(i) {
    boot_sample <-
      scores[sample(1:nrow(scores),
                    size=nrow(scores),
                    replace=T), ]
    data.table(
      correlation=cor(boot_sample$x, boot_sample$y)
    )
  }))


ggplot(boot, aes(x=correlation)) +
  geom_dotplot(
    binwidth=0.01, fill="lightblue", color="lightblue",
    dotsize=0.8, stackratio=1
    ) +
  xlim(0.2,1) +
  ylim(0,1) +
  scale_y_continuous(breaks = NULL) +
  labs(title="Distribution of Bootstrapped Correlations",
       x="Correlation", y="")


ggplot(boot, aes(x=correlation)) +
  geom_dotplot(
    binwidth=0.01, fill="lightblue", color="lightblue",
    dotsize=0.8, stackratio=1
    ) +
  xlim(0.2,1) +
  ylim(0,1) +
  scale_y_continuous(breaks = NULL) +
  labs(title="Distribution of Bootstrapped Correlations",
       x="Correlation", y="") +
  geom_vline(xintercept=mean(boot$correlation),
             color="black", linetype="solid", linewidth=1) +
  geom_vline(xintercept=manual_correlation,
             color="red", linetype="dashed", linewidth=1)

library(parallel)
boot <-
  rbindlist(mclapply(mc.cores=6, 1:100000, function(i) {
    boot_sample <-
      scores[sample(1:nrow(scores),
                    size=nrow(scores),
                    replace=T), ]
    data.table(
      correlation=cor(boot_sample$x, boot_sample$y)
    )
  }))


q <- quantile(boot$correlation, prob=c(0.025, 0.975))

ggplot(boot, aes(x=correlation)) +
  geom_density( fill="lightblue", color="lightblue") +
  xlim(0.2,1) +
  ylim(0,1) +
  scale_y_continuous(breaks = NULL) +
  labs(title="Distribution of Bootstrapped Correlations",
       x="Correlation", y="") +
  geom_vline(xintercept=mean(boot$correlation),
             color="black", linetype="solid", linewidth=0.5) +
  geom_vline(xintercept=q,
             color="grey", linetype="dashed", linewidth=1)



