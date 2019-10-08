###############################################################################################################
## compare_loss_functions.r
## Amelia Bertozzi-Villa
## September 2019
## 
## Explore prior values on loss functions-- k from 16 to 18 leaves gaps in function space, fill in by 
## expanding prior to 24.
##############################################################################################################


library(data.table)
library(ggplot2)

rm(list=ls())

sigmoid<-function(t,k,L){
  v<-exp(k-k/(1-(t/L)^2))
  v[t>=L]<-0
  return(v)	
}

n_param_samples <- 20
k <- seq(16, 24, length.out = n_param_samples)
L <- seq(4, 30, length.out = n_param_samples)

samples <- data.table(expand.grid(k, L))
names(samples) <- c("k", "L")

all_sigs <- rbindlist(lapply(1:nrow(samples), function(idx){
  time_points=seq(0,10,.01)
  this_sigmoid <- data.table(id=idx,
    time=time_points,
    k=samples$k[idx],
    L=samples$L[idx],
    sig=sigmoid(time_points, samples$k[idx], samples$L[idx]))
  return(this_sigmoid)
}))


ggplot(all_sigs, aes(x=time, y=sig))  +
  geom_line(aes(color=as.factor(L), group=as.factor(id))) +
  labs(title=paste("k from", min(k), "to", max(k)))



# perhaps not identifiable, try simple exponential instead:
n_param_samples <- 50
lambda <- -log(seq(0.1, 0.7, length.out = n_param_samples))

all_exp <- rbindlist(lapply(1:length(lambda), function(idx){
  time_points=seq(0,10,.01)
  this_lambda <- lambda[idx]
  this_exp <- data.table(id=idx,
                             time=time_points,
                             lambda=this_lambda,
                             expval=exp(-this_lambda*time_points))
  return(this_exp)
}))

ggplot(all_exp, aes(x=time, y=expval))  +
  geom_line(aes(color=as.factor(lambda))) +
  theme(legend.position = "none") + 
  labs(title=paste("lambda from", min(lambda), "to", max(lambda)))

compare <- data.table(idx=1:n_param_samples,
                      logseq = -log(seq(0.1, 0.7, length.out = n_param_samples)),
                      seqlog = seq(-log(0.1), -log(0.7), length.out = n_param_samples))
compare <- melt(compare, id.vars = "idx")
compare[, expval:=exp(-value)]


ggplot(compare, aes(x=idx, y=expval)) +
  geom_point(aes(color=variable))
