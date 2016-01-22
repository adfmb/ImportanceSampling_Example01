library(shiny)
library(reshape2)
library(ggplot2)
require(gridExtra)

G_x<-function(x,lambda=1){
  
  resultado<-(1-exp(-x*lambda))/(1-exp(-2*lambda))
  
  return (resultado)
  
} #Distribucion de una exponencial truncada en [0,2]

g_x<-function(x,lambda=1){
  
  resultado<-lambda*exp(-x*lambda)/(1-exp(-2*lambda))
  
  return (resultado)
  
} #Densidad de una exponencial truncada en [0,2]

inv_G<-function(u,lambda=1){
  
  resultado<- -log(1-u*(1-exp(-2*lambda)))/lambda
  
  return(resultado)
  
} #Inversa de una exponencial truncada en [0,2]

fphi_g<-function(m,x){
  
  m*exp(-m*x)/g_x(x,m)
  
}

fphi_g_2<-function(e,g){
  
  e/g
  
}

#montecarlo_1<-function(nsim,alpha){
  
  #x<- runif(nsim,0,2)
  #phi<-2*sqrt(4-x^2)
  #thetaF<-mean(phi)
  #zetha<-qnorm(alpha/2,lower.tail = FALSE)
  #Scuadrada<-var(phi)
  #limsup<-thetaF + zetha*sqrt(Scuadrada/nsim)
  #liminf<-thetaF - zetha*sqrt(Scuadrada/nsim)
  
  #return(data.frame(est=thetaF, LimiteSuperior=limsup, LimiteInferior=liminf))
  
#}



shinyServer(function(input, output) {

alpha<-reactive(input$alpha)
MuestraMinima<-reactive(input$MinM)
NumSimulaciones<-reactive(input$NSimulaciones)
brincosSim<-reactive(10)
MuestraMax<-reactive(MuestraMinima()+(brincosSim()*(NumSimulaciones()-1)))

N<-reactive(seq(MuestraMinima(),MuestraMax(),by=brincosSim()))
#N<-seq(10,100,by=10)

m<-reactive(input$m)
lg<-reactive(input$lg)
#m<-1

#u_is<-reactive(runif(N(),0,1))
#u_is<-runif(N,0,1)
u_is<-reactive(sapply(N(),0,1,FUN=runif))
#u_is<-sapply(N,0,1,FUN=runif)

#x<-reactive(inv_G(u_is(),m()))
#x<-inv_G(u_is,m)
x<-reactive(sapply(u_is(),lg(),FUN=inv_G))
#x<-sapply(u_is,m,FUN=inv_G)

#g_x<-reactive(m()*exp(-x()*m())/(1-exp(-2*m())))
#g_x<-m*exp(-x*m/(1-exp(-2*m)))

#f_phi_x<-reactive(m()*exp(-m()*x())/g_x())
#f_phi_x<-m*exp(-m*x)/g_x)

#densidadExp<-function(x,m){m*exp(-m*x)}


output$plot1_1 <- renderPlot({
numerador<-sapply(x(),rate=m(),FUN=dexp)
#numerador<-sapply(x,rate=m,FUN=dexp)
#numerador1<-sapply(x,m,FUN=densidadExp)

denominador<-sapply(x(),lambda=lg(),FUN=g_x)
#denominador<-sapply(x,lambda=m,FUN=g_x)

zetha<-qnorm(input$alpha/2,lower.tail = FALSE)
cociente<-numeric(length(denominador))
medias_muestrales<-numeric(length(denominador))
varianzas_muestrales<-numeric(length(denominador))
limsup<-numeric(length(denominador))
liminf<-numeric(length(denominador))

for(i in 1:length(denominador)){
  
  n1<-as.numeric(unlist(numerador[i]))
  d1<-as.numeric(unlist(denominador[i]))
  #cociente[i]<-n1/d1
  medias_muestrales[i]<-mean(n1/d1)

  Scuadrada<-var(n1/d1)
  limsup[i]<-medias_muestrales[i] + zetha*sqrt(Scuadrada/length(n1))
  liminf[i]<-medias_muestrales[i] - zetha*sqrt(Scuadrada/length(n1))
  
}

tabla<-data.frame(N=N(),Estimador=medias_muestrales,Limite_Inferior=liminf,Limite_Superior=limsup)
Lim_I<-data.frame(N=N(),Limites=liminf)
Lim_S<-data.frame(N=N(),Limites=limsup)
Lim<-rbind(Lim_I,Lim_S)
Lim_02<-melt(Lim,id="N")

Correcto<-data.frame(N=N(),Valor_Correcto=1-exp(-2*m()))
Correcto_02<-melt(Correcto,id="N")

Estimador<-data.frame(N=N(),Estimador=medias_muestrales)
Estimador_02<-melt(Estimador,id="N")

tf<-rbind(Lim_02,Estimador_02,Correcto_02)

p<-ggplot(tf,aes(x=N,y=value,fill=variable,colour = variable))+geom_line(alpha=.6) +scale_colour_manual(values=c("dodgerblue3","hotpink1","yellow1"))+scale_y_continuous("")+scale_x_continuous("Simulaciones")

p

})

})


#theta_gorrito_2<-numeric(10)
#for(i in 1:10){
  
  #i=2
 # theta_gorrito_2[i]<-mean(fphi_g_2(as.numeric(unlist(numerador[i])),as.numeric(unlist(denominador[i]))))
  
  
#}



#theta_gorrito_3<-sapply(as.numeric(unlist(numerador)),as.numeric(unlist(denominador)),FUN=fphi_g_2)
#fphi_g_2(as.numeric(unlist(numerador[i])),as.numeric(unlist(denominador[i])))

#nthetagorrito<-sapply(m(),x,FUN=fphi_g)
#nthetagorrito<-sapply(m=m,x,FUN=fphi_g)
#fphi_g<-m()*exp(-m()*x)/g_x(x,m())
#theta_gorrito<-sapply(nthetagorrito,FUN=mean)

#a_1<-reactive(sapply(N(),alpha(),FUN=montecarlo_1))

