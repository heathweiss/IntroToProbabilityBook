v <- c(3,1,4,5,9)
#sum(v)
#max(v)
#min(v)
#sample(10,5)

r <- replicate(10000, sum(sample(100)==(1:100)))
sum(r>=1)/10000