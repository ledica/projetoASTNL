library(scatterplot3d)


#dados<-read.csv("FOGO_MEDIA_DIARIA.csv",header=T, sep=" ")
#str(dados)
#mes<-dados[1:6500,1]#
#temp<-dados[1:6500,2]#
#chuva<-dados[1:6500,3]#
#u10cm<-dados[1:6500,4]#
#u2m<-dados[1:6500,5]#
#fogo<-dados[1:6500,6]#
#plot(u2m) 

dados<-read.csv("FOGO_MEDIA_MENSAL.csv",header=T, sep=" ")
str(dados)
mes<-dados[1:220,1]#
temp<-dados[1:220,2]#
chuva<-dados[1:220,3]#
u10cm<-dados[1:220,4]#
u2m<-dados[1:220,5]#
fogo<-dados[1:220,6]#
 

variavel_teste=temp

suppressMessages(library('nonlinearTseries'))
library('plot3D')

old.par = par(mfrow = c(1, 2))
# tau-delay estimation based on the autocorrelation function
tau.acf = timeLag(variavel_teste, technique = "acf",
                  lag.max = 200, do.plot = T)
# tau-delay estimation based on the mutual information function
tau.ami = timeLag(temp, technique = "ami", 
                  lag.max = 200, do.plot = T)
par(old.par)


emb.dim = estimateEmbeddingDim(variavel_teste, time.lag = tau.ami,
                               max.embedding.dim = 15)


tak = buildTakens(variavel_teste,embedding.dim = emb.dim, time.lag = tau.ami)
scatter3D(tak[,1], tak[,2], tak[,3],
          main = "system reconstructed phase space",
          col = 1, type="o",cex = 0.3)

#The correlation dimension is a technique that measures the fractal dimension of the phase space of a dynamical system. To verify that the estimation of the correlation dimension does not depend on the embedding dimension, we compute the correlation sums (corrDim function) for several embedding dimensions. Once we have checked for the existence of the linear regions in different embedding dimensions, we obtain an estimation of the correlation dimension with the estimate function. This function allows to specify the range in which the linear behavior appears (regression.range parameter) as well as the embedding dimensions to be used for the estimation of the correlation dimension (use.embeddings parameter). The final estimation of the correlation dimension is an average of the slopes obtained for each embedding dimension.
cd = corrDim(variavel_teste,
             min.embedding.dim = emb.dim,
             max.embedding.dim = emb.dim + 5,
             time.lag = tau.ami, 
             min.radius = 0.001, max.radius = 50,
             n.points.radius = 40,
             do.plot=FALSE)
plot(cd)
cd.est = estimate(cd, regression.range=c(0.75,3),
                  use.embeddings = 5:7)

se = sampleEntropy(cd, do.plot = F)
se.est = estimate(se, do.plot = F,
                  regression.range = c(8,15))
cat("Sample entropy estimate: ", mean(se.est), "\n")

#Maximum Lyapunov exponent

#One of the more important characteristics of a chaotic system is its sensitivity to initial conditions. As a consequence of this sensitivity, close trajectories diverge exponentially fast. The maximum Lyapunov exponent measures the average rate of divergence of close trajectories in the system. The maxLyapunov function can be used for computing this divergence through time. To define what is a close trajectory we make use of the radius parameter. After the computation of the divergence rates we can get an estimate of the maximum Lyapunov exponent by performing a linear regression (estimate function), just as we did with the correlation dimension.
# get the sampling period of the lorenz simulation
# computing the differences of time (all differences should be equal)
sampling.period = diff(dados$time)[1]
ml = maxLyapunov(variavel_teste, 
                 sampling.period=0.01,
                 min.embedding.dim = emb.dim,
                 max.embedding.dim = emb.dim + 3,
                 time.lag = tau.ami, 
                 radius=1,
                 max.time.steps=1000,
                 do.plot=FALSE)
plot(ml,type="l", xlim = c(0,8))

ml.est = estimate(ml, regression.range = c(0,3),
                  do.plot = T,type="l")

cat("--- estimate: ", ml.est,"\n")

cat("--- estimate: ",cd.est,"\n")


st = surrogateTest(variavel_teste,significance = 0.05,one.sided = F,
                   FUN = timeAsymmetry, do.plot=F)
plot(st)

rqa.analysis=rqa(time.series = variavel_teste, embedding.dim=3, time.lag=179,
                 radius=1.2,lmin=2,do.plot=FALSE,distanceToBorder=2)
plot(rqa.analysis)
