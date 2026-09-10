library(ggplot2)



ns <- rnorm(1e6)


ggplot() +
  geom_density(data=data.frame(x=ns),
               aes(x=x), linetype='dashed') +
  geom_vline(xintercept=mean(ns),
             linetype='dashed') +
  labs(x=NULL, y=NULL) +
  xlim(-5,5)


ns.bias <- sample(ns[ns < 1], size=1e6, replace=TRUE)

ns.bias.mean <- mean(ns.bias)
ns.bias.sd <- sd(ns.bias)

ggplot() +
  stat_function(fun = dnorm,
    args = list(mean = ns.bias.mean,
                sd = ns.bias.sd)
  ) +
  geom_vline(xintercept=mean(ns.bias)) +
  labs(x=NULL, y=NULL,
       title=sprintf("Mean = %2f",ns.bias.mean)) +
  xlim(-5,5)


ggplot() +
  geom_density(data=data.frame(x=ns.bias), aes(x=x), colour='blue') +
  geom_vline(xintercept=mean(ns.bias), colour='blue') +
  labs(x=NULL, y=NULL) +
  xlim(-5,5)


ggplot() +
  geom_density(data=data.frame(x=ns.bias), aes(x=x), colour='blue') +
  geom_vline(xintercept=mean(ns.bias), colour='blue') +
  geom_density(data=data.frame(x=ns), aes(x=x), linetype='dashed') +
  geom_vline(xintercept=mean(ns), linetype='dashed') +
  labs(x=NULL, y=NULL) +
  xlim(-5,5)




