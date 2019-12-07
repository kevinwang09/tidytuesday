
n = 100
nExp = 10000
means = replicate(n = nExp, expr = {mean(rcauchy(n))}, simplify = TRUE)
summary(means)
hist(means)
