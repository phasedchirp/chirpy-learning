# Some quick code for checking distribution from haskell poisson rng:

## To generate relevant input files (assuming you're in rng-test directory):
# ghc --make poisTest.hs
# ./poisTest 1 50000 > test.txt
# ./poisTest 3 50000 > test3.txt

test = read.table("test.txt",header=FALSE)
num_bins = length(unique(test$V1))
bins = seq(from=0,to=max(test$V1),length.out=num_bins)
hist(test$V1,breaks=bins,freq = FALSE)


poisson.r = rpois(length(test$V1),1)
num_bins.r = length(unique(poisson.r))
bins.r = seq(from=0,to=max(poisson.r),length.out=num_bins.r)
hist(poisson.r,breaks=bins.r,freq = FALSE)

qs = c(0.025,0.1,0.25,0.5,0.75,0.9,0.975,0.99,0.999,0.9999,0.99999)
quantile(test$V1,probs=qs)
quantile(poisson.r,probs=qs)

# Changing rate parameter:

test3 = read.table("test3.txt",header=FALSE)
num_bins3 = length(unique(test3$V1))
bins3 = seq(from=0,to=max(test3$V1),length.out=num_bins3)
hist(test3$V1,breaks=bins3,freq = FALSE)


poisson.r.3 = rpois(length(test3$V1),3)
num_bins.r.3 = length(unique(poisson.r.3))
bins.r.3 = seq(from=0,to=max(poisson.r.3),length.out=num_bins.r.3)
hist(poisson.r.3,breaks=bins.r.3,freq = FALSE)

qs = c(0.025,0.1,0.25,0.5,0.75,0.9,0.975,0.99,0.999,0.9999,0.99999)
quantile(test3$V1,probs=qs)
quantile(poisson.r.3,probs=qs)
