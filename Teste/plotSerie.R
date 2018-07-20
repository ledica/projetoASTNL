library(scatterplot3d)
library(tseriesChaos)

temperatura2m = read.table("serie_air2m.txt")
ocorrenciaFogo = read.table("serie_fogo.txt")
chuva = read.table("serie_prate.txt")
umidadeSolo10cm = read.table("serie_soilw.0-10cm.txt")
umidadeSolo2m = read.table("serie_soilw.10-200cm.txt")

t=temperatura2m[1:20000,2]
f=ocorrenciaFogo[1:20000,2]
p=chuva[1:20000,2]

matplot(p,type="l")

mutual(temperatura2m)
