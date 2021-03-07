###########################################################
#
#           ARQUIVOS E DIRETORIOS
#
###########################################################

data <- read.csv("CSV/BaixoXT/NSFNET/PBB.csv")
# 6, 6 são largura e altura, respectivamente
cairo_pdf("Graficos/BaixoXT/NSFNET/PBB.pdf", 6, 6, bg="transparent")

###########################################################

#
#           PARÂMETROS
#
###########################################################

#quantidade de pontos de carga
qLoads <- 7
#Ponto de carga inicial
initialLoad <- 750
#Incremento de carga
increment <- 100
#Ponto mínimo e máximo do eixo y
yintervals <- c(10E-7, 0.1)
ylabel <- "Probabilidade de bloqueio de banda"
#ylabel <- "Probabilidade de bloqueio de circuito"
#ylabel <- "Eficiência Energética"
xlabel <- "Carga(Erlangs)"
#Tamanho da borda da reta do intervalo de confiança
intervalSize <- 0.07

###########################################################
#
#           CALCULOS
#
###########################################################

#valores dos pontos de carga
loads <- seq(initialLoad,initialLoad+increment*(qLoads-1),increment)

## Para curvas em que o limite inferior do intervalo de confiança ultrapasse
## o valor mínimo da curva, é necessário calcular o valor mínimo de cada curva
## para corrigir um bug (o intervalo de confiança desaparece nos pontos em que
## isso ocorre)
#Encontra o valor mínimo do PGNIE
minpgnie <- 1
for(i in 1:qLoads) {
  if(data$PGNIE[i] < minpgnie && data$PGNIE[i] != 0)
    minpgnie <- data$PGNIE[i]
}

#Encontra o valor mínimo do ABNE
minabne <- 1
for(i in 1:qLoads) {
  if(data$ABNE[i] < minabne && data$ABNE[i] != 0)
    minabne <- data$ABNE[i]
}

#Margens do grafico
par(mar = c(5.2, 5.5, 1.3, 0.4))
plot(data$PGNIE,
     type="l",
     xaxt="n",
     yaxt="n",
     log="y",
     lty=1,
     ylim=yintervals,
     col="blue",
     cex.lab=1.5,
     xlab="",
     ylab="")

legend("left", legend=c("PGNIE", "ABNE", "CP-RF", "RC-FF"),
       lty=c(1,1,1,1), col=c("blue", "red", "green", "purple"), bty="n", lwd=1:2, cex=1.3)

#Rótulos dos eixos
title(ylab=ylabel, mgp=c(4,1,0),cex.lab=1.5)
title(xlab=xlabel, mgp=c(4.2,1,0),cex.lab=1.5)

#Intervalo de confiança PGNIE
for(i in 1:qLoads) {
  #Linha Vertical
  segments(
    i,
    data$PGNIE[i] + data$ERROPGNIE[i],
    i,
    data$PGNIE[i] - data$ERROPGNIE[i],
    col="black"
  )
  
  #Corrigir o erro para intervalos menores que o menor valor
  if(data$PGNIE[i] - data$ERROPGNIE[i] <= minpgnie) {
    segments(
      i,
      data$PGNIE[i] + data$ERROPGNIE[i],
      i,
      yintervals[1],
      col="black"
    )
  }
  
  #Linha Horizontal Superior
  segments(
    i-intervalSize,
    data$PGNIE[i] + data$ERROPGNIE[i],
    i+intervalSize,
    data$PGNIE[i] + data$ERROPGNIE[i],
    col="black"
  )
  
  #Linha Horizontal Inferior
  segments(
    i-intervalSize,
    data$PGNIE[i] - data$ERROPGNIE[i],
    i+intervalSize,
    data$PGNIE[i] - data$ERROPGNIE[i],
    col="black"
  )
}

par(new=TRUE)
plot(data$ABNE,
     type="l",
     axes=FALSE,
     ann=FALSE,
     log="y",
     ylim=yintervals,
     col="red"
     )

#Intervalo de confiança ABNE
for(i in 1:qLoads) {
  #Linha Vertical
  segments(
    i,
    data$ABNE[i] + data$ERROABNE[i],
    i,
    data$ABNE[i] - data$ERROABNE[i],
    col="black"
  )
  
  #Corrigir o erro para intervalos menores que o menor valor
  if(data$ABNE[i] - data$ERROABNE[i] <= minabne) {
    segments(
      i,
      data$ABNE[i] + data$ERROABNE[i],
      i,
      yintervals[1],
      col="black"
    )
  }
  
  #Linha Horizontal Superior
  segments(
    i-intervalSize,
    data$ABNE[i] + data$ERROABNE[i],
    i+intervalSize,
    data$ABNE[i] + data$ERROABNE[i],
    col="black"
  )
  
  #Linha Horizontal Inferior
  segments(
    i-intervalSize,
    data$ABNE[i] - data$ERROABNE[i],
    i+intervalSize,
    data$ABNE[i] - data$ERROABNE[i],
    col="black"
  )
}

par(new=TRUE)
plot(data$CPRF,
     type="l",
     axes=FALSE,
     ann=FALSE,
     log="y",
     ylim=yintervals,
     col="green"
)

#Intervalo de confiança CP-RF
for(i in 1:qLoads) {
  #Linha Vertical
  segments(
    i,
    data$CPRF[i] + data$ERROCPRF[i],
    i,
    data$CPRF[i] - data$ERROCPRF[i],
    col="black"
  )
  
  #Linha Horizontal Superior
  segments(
    i-intervalSize,
    data$CPRF[i] + data$ERROCPRF[i],
    i+intervalSize,
    data$CPRF[i] + data$ERROCPRF[i],
    col="black"
  )
  
  #Linha Horizontal Inferior
  segments(
    i-intervalSize,
    data$CPRF[i] - data$ERROCPRF[i],
    i+intervalSize,
    data$CPRF[i] - data$ERROCPRF[i],
    col="black"
  )
}

par(new=TRUE)
plot(data$RCFF,
     type="l",
     axes=FALSE,
     ann=FALSE,
     log="y",
     ylim=yintervals,
     col="purple"
)

#Intervalo de confiança RC-FF
for(i in 1:qLoads) {
  #Linha Vertical
  segments(
    i,
    data$RCFF[i] + data$ERRORCFF[i],
    i,
    data$RCFF[i] - data$ERRORCFF[i],
    col="black"
  )
  
  #Linha Horizontal Superior
  segments(
    i-intervalSize,
    data$RCFF[i] + data$ERRORCFF[i],
    i+intervalSize,
    data$RCFF[i] + data$ERRORCFF[i],
    col="black"
  )
  
  #Linha Horizontal Inferior
  segments(
    i-intervalSize,
    data$RCFF[i] - data$ERRORCFF[i],
    i+intervalSize,
    data$RCFF[i] - data$ERRORCFF[i],
    col="black"
  )
}

#Valores do eixo x
axis(side = 1,
     at = 1:qLoads, 
     label = loads,
     cex.axis=1.5,
     las=2)

#Valores do eixo x
axis(side = 2,
     at=c(1E-6, 1E-5, 1E-4, 1E-3, 1E-2, 1E-1, 1),
     labels=c("10⁻⁷", "10⁻⁶", "10⁻⁵","10⁻⁴","10⁻³", "10⁻²", "10⁻¹"),
     las=2,
     cex.axis=1.5)

dev.off()