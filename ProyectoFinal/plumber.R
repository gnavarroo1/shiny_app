#plumber.R

library(readxl)
library(mailR)
library(readxl)
library(jsonlite)
library(httr)
library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)
library(tidyr)

regresion <- function(dt,vx){3
  lista <- NULL
  x=dt$x
  y=dt$y
  sumX=sum(x)
  sumY=sum(y)
  prodXY=sum(x*y)
  promX=mean(x)
  dt$x2=x^2
  dt$y2=y^2
  x2=sum(x^2)
  y2=sum(y^2)
  
  promY<-mean(y)
  #calculo de la covarianza
  cov = (prodXY/length(x))-(promX*promY)
  #calculo de las desviacion estandar x, y
  dx=sqrt((x2/length(x))-promX^2)
  dy=sqrt((y2/length(y))-promY^2)
  #coeficiente de correlacion de Pearson
  r=cov/(dx*dy)
  #generando recta regresional
  vy=promY+(cov/dx^2)*(vx-promX)
  lista[[1]]<-dt #dt entrenamiento
  lista[[2]]<-vx #datos de validacion
  lista[[3]]<-vy #datos predecidos
  lista[[4]]<-r  #coeficiente de correlacion de Pearson
  return (lista)
}

datos<- read.csv("/home/rstudio/ProyectoFinal/dataset/forestfires.csv",stringsAsFactors = FALSE,check.names = FALSE)


#* @param sender Remitente
#* @param password Contrase??a del remitente
#* @param recipient Destinatarios
#* @param subject Asunto del correo
#* @param msg El mensaje a enviar
#* @post /sendMail
function(req,sender,password,recipient,subject,msg){
  recipients<-strsplit(recipient,",")
  smtp <- list(host.name = "smtp.gmail.com", port =587, user.name = sender, passwd = password, ssl = TRUE)
  body<- msg
  tryCatch({ send.mail(from = sender,
            to = recipients[[1]],
            subject = subject,
            body = body,
            encoding = "utf-8",
            html = T,
            smtp = smtp,
            authenticate = TRUE,
            send = TRUE)
    
  },error =function(e) {
    
    message(e)
    return(500)
    
  },finally = {return(200)}
  )
  
  
}

#' @get /plot1
#' @png
function() {
  plot(datos,xlim=c(0,100),ylim=c(0,100))
}

#' @get /plot2
#' @png
function() {
  plot(datos$DMC,datos$DC)
}

#' @get /regresion
#' @png
function() {
  datos<- read.csv("/home/rstudio/ProyectoFinal/dataset/forestfires.csv",stringsAsFactors = FALSE,check.names = FALSE)
  x<-datos$DMC
  y<-datos$DC
  dt=data.frame(x,y)
  vx=c(18,30,40,60,80)
  modelo <- lm(DC ~ DMC, data = datos)
  #Valores Predichos
  DCs <- predict(modelo,datos)
  b0 <- round(modelo$coefficients[1],2)
  b1 <- round(modelo$coefficients[2],2)
  pt<-qplot(x = DMC, y = DC,data = datos, 
            main = "Duff Moisture Code vs Drought Code", xlab = "Duff Moisture Code (DMC)",
            ylab = "Drought Code (DC)", geom = c("point")) + geom_line(aes(y=DCs), lwd = 1.2, color = 4) +
    geom_text(x = 100, y = 1000, aes(label = paste("DC^", " = ", b0, " + ", b1, "*","DMC")))
  pt
}


#' @get /getData
function(){
  datos
}
