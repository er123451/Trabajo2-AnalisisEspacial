#Cargamos Librerias 
library("fdth")

#cargamos archivo en memoria
value <- read.csv("data2.csv",sep=";", dec = ",")
#medidas de centralizacion
media <- mean(value$Imp..Conce)
mediana <- median(value$Imp..Conce)
mode <- moda(value$Imp..Conce)
modeN <- modaN(value$Imp..Conce,n = 20)

#cuantiles
cuantiles <- quantile(value$Imp..Conce)

#asimetria
asimetry <- asimetria(value$Imp..Conce)

#curtosis
kurtosis <- curtosis(value$Imp..Conce)

#Histograma 
hist <- histograma(value$Imp..Conce,n = 20)

## metemos todas las variables estadisticas en un vector y las imprimimos
variables <- c(media,mediana,mode, modeN,unname(cuantiles),asimetry,kurtosis)
print(variables)

#Funciones del script
moda <- function(data){
  unique <- unique(data)
  return(unique[which.max(tabulate(match(data,unique)))])
}
modaN <- function(data,n){
    div <-(max(data)-min(data))/n
    divVec <- seq(from = min(data), to = max(data), by=div)
    divSum <- vector(length = n)
    for(i in data){
        m = 1 
        for(j in 1:n){
            if(i>divVec[j] && i< divVec[j+1]){
                divSum[m] = divSum[m]+1
            }
            m = m+1
        }
    }
    L = which.max(divSum)
    if(which.max(divSum)==1){
        d1 = 0
        d2 = divSum[1]-divSum[2]
    }else if (which.max(divSum)==n){
       d1 = divSum[n]-divSum[n-1]
    }else{
        d1 = divSum[L]-divSum[L-1]
        d2 = divSum[L]-divSum[L+1]
    }
    moda = divVec[L] + div*d1/(d1+d2)
    return(moda)
}
histograma <- function(data,n){
    div <-(max(data)-min(data))/n
    A <-fdt(x=data,start = min(data),end = max(data),h = div)
    plot(A)
    return(A)
}
curtosis <- function(data){
    media <- mean(data)
    part1 <- 0
    n <- 0
    for(i in data){
        numAux <- (i-media)^4
        part1 <- part1+numAux
        n <- n+1
    }
    return(part1/(n*sd(data)^4))
}
asimetria <- function(data){
    x = ( mean(data) - median(data))/sd(data)
    return(x)
}