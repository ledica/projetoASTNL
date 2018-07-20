library(tseriesChaos)
library(scatterplot3d)
x <- window(rossler.ts, start=90)
plot(rossler.ts)
window()
xyz <- embedd(x, m=3, d=8)
scatterplot3d(xyz, type="l")
window()
## embedding multivariate time series
series <- cbind(seq(1,50),seq(101,150))
head(embedd(series, m=6, d=1))

output <-lyap_k(lorenz.ts, m=3, d=2, s=200, t=40, ref=1700, k=2, eps=4)
plot(output)
lyap(output, 0.73, 2.47)

library(scatterplot3d)
x <- window(rossler.ts, start=90)
xyz <- embedd(x, m=3, d=8)
scatterplot3d(xyz, type="l")

## embedding multivariate time series
series <- cbind(seq(1,50),seq(101,150))
head(embedd(series, m=6, d=1))