#preparacao do ambiente
rm(list = ls())
cat("\014")

#--------------------------------------------------%
# EXERCICIO 1
#--------------------------------------------------%

#funcoes
neuronio <- function(x, w1, w2, bias){
  #((w3*bias)-x*w1/w2)
  return ((w1*x) + (w2*bias))
}

#--------------------------------------------------%
# NEURONIO SIMPLES
#--------------------------------------------------%

dados <- read.csv2("amostrabivariada.csv", header=TRUE, sep=";")

plot(dados, col="red", xlim=c(0,6), ylim=c(0,6))

eixox <- seq(0,6,0.1)

#MUDANDO OS PESOS -------------------
w1 = -1
w2 = 6.3
#-------------------------------------

bias = 1

eixoy <- neuronio(eixox, w1, w2, bias)

par(new=T)
plot(eixox, eixoy, type="l", col="blue", xlim=c(0,6), ylim=c(0,6), xlab="", ylab="")

#--------------------------------------------------%
# EXERCICIO 2
#--------------------------------------------------%

#implementando  as funcoes de ativacao

#fazendo com que as funcoes recebem o vetor eixox e
#como resposta gerem eixoy correspondente
#depois plotando as funcoes 

#--------------------------------------------------%
# FUNÇÃO LOGISTICA
#--------------------------------------------------%
logistica <- function(x, beta){
  return( 1 / (1 + exp(-beta*x)) )
}
eixox <- seq(0,100,1)
eixoy <- logistica(eixox, 1)

plot(eixox, eixoy, type='l', main='Logística')

#--------------------------------------------------%
# FUNÇÃO DEGRAU
#--------------------------------------------------%
degrau<- function(x){
  ifelse(x>=0,1,0)
}
#eixox <- seq(-1,10,1)
eixox<- seq(0,100,1)
eixoy<-degrau(eixox)
plot(eixox, eixoy, type= 'l', main= 'Degrau')

#--------------------------------------------------%
# FUNÇÃO DEGRAU BIPOLAR
#--------------------------------------------------%
degrauB<-function(x){
  ifelse(x>0,1,-1)
}
#eixox <- seq(-2,10,1)
eixox<- seq(0,100,1)
eixoy<- degrauB(eixox)
plot(eixox, eixoy, type= 'l', main= 'Degrau Bipolar')

#--------------------------------------------------%
# FUNÇÃO RAMPA SIMÉTRICA               
# #--------------------------------------------------%
a<-1
rampaS<-function(x){
ifelse(x>a,a,0 )
ifelse(x<a,-a,a)
}
#eixox <- seq(-1,2,1)
eixox<- seq(0,100,1)
eixoy<- rampaS(eixox)
plot(eixox, eixoy, type= 'l', main= 'Ramapa Simétrica')


#--------------------------------------------------%
# FUNÇÃO TANGENTE HIPERBÓLICA
#--------------------------------------------------%
sigmoidal<-function(x,beta){
 # return((1-exp (-beta*x))/1+exp(-beta*x))
  (1 - exp(-beta*x)) / (1 + exp(-beta*x))
}
#eixox <- seq(-10,10,1)
eixox<- seq(0,100,1)
eixoy<-(sigmoidal(eixox,-1))
plot(eixox,eixoy,type ='l',main = 'Tangente Hiperbolica - Sigmoidal')
#--------------------------------------------------%
# FUNÇÃO GAUSSIANA
#--------------------------------------------------%
gaussiana <- function(x, a, b, c){
    return(a * exp(-(x-b)^2 / (2*c^2)))
  }
#eixox <- seq(-10,10,1)
eixox <- seq(0, 100,1)
eixoy <- gaussiana(eixox, 1, 0, 1)
plot(eixox, eixoy, type='l', main='Gaussiana')
#--------------------------------------------------%
# FUNÇÃO LINEAR
#--------------------------------------------------%
linear<- function(u){
  return (u)
}
eixox <- seq(1,100,1)
eixoy <- linear(eixox)

plot(eixox, eixoy, type='l', main='Linear')
