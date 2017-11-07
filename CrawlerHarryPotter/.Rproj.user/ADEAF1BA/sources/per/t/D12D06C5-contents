library(haven)
miData = read_dta("D:/Dropbox/documentos/Visual Analytics/tarea4/ESP.dta")
colsOpinion = c(13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
miData = miData[complete.cases(miData[,colsOpinion]),]


colsInfoDemo = c(3,4,5,6,7,8,9,10,11,12)

datosDemo = miData[,colsInfoDemo]

colsOpinion = c(13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)

muestra = sample(1:nrow(miData),500, replace = FALSE)

datosOpinion = miData[,colsOpinion]
datosOpinion = datosOpinion[muestra,]

summary(datosOpinion)

#Escalar cada columna
for(i in 1:length(datosOpinion)){
  datosOpinion[,i] = datosOpinion[,i]/max(datosOpinion[,i], na.rm = TRUE)
}

closenessMatrix = matrix(0,nrow = nrow(datosOpinion), ncol = nrow(datosOpinion))

#llenar matriz de cercania
for(fila in 1:nrow(closenessMatrix)){
  closenessMatrix[fila,] = rowSums(1-abs(sweep(as.matrix(datosOpinion),2,as.numeric(datosOpinion[fila,]))),na.rm = TRUE)
  closenessMatrix[fila,fila] = 0
}

#Imprimir en JSON
library(RJSONIO)
nodos = list()
for(i in 1:nrow(closenessMatrix)){
  infoNodo = list(id = i, ano = datosDemo$ano[muestra[i]],genero = datosDemo$sexo[muestra[i]],
                       edad = datosDemo$edad[muestra[i]], estrato = datosDemo$estrato_servicios[muestra[i]])
  for(j in colsOpinion){
    infoNodo[names(miData)[j]] = miData[muestra[i],j]
  }
  nodos = append(nodos,list(infoNodo))
}

#links
links = list()
for(i in 1:nrow(closenessMatrix)){
  for(j in 1:nrow(closenessMatrix)){
    if(i != j){
      if(closenessMatrix[i,j]>16){
        links = append(links,list(list(source = i, target = j, value = round(closenessMatrix[i,j]))))
      }
    }
  }
}

summary(closenessMatrix[27,])




red = list(nodes = nodos, links = links)

setwd("D:/Dropbox/documentos/Visual Analytics/tarea4")
# jsonize
write(toJSON(red,pretty= T),file = "red.json")

