# Incisio A: Delegacion con mas participantes
informacionOriginal <- read.csv("C:\\Users\\patoe\\Desktop\\Proyectos en R\\BigDataEvidencia2\\Evidencia2Avance1\\Resultados-MPSG-Open-Data-oct10-feb20.csv", check.names = FALSE, fileEncoding = "Latin1")
delegaciones <- c("01AGUASCALIENTES","02BAJA CALIFORNIA","03BAJA CALIFORNIA SUR","04CAMPECHE","05COAHUILA","06COLIMA","07CHIAPAS","08CHIHUAHUA","09CIUDAD DE MÉXICO","10DURANGO","11GUANAJUATO","12GUERRERO","13HIDALGO","14JALISCO","15MÉXICO","16MICHOACÁN DE OCAMPO","17MORELOS","18NAYARIT","19NUEVO LEÓN","20OAXACA","21PUEBLA","22QUERÉTARO","23QUINTANA ROO","24SAN LUIS POTOSÍ","25SINALOA","26SONORA","27TABASCO","28TAMAULIPAS","29TLAXCALA","30VERACRUZ DE IGNACIO DE LA LLAVE","31YUCATÁN","32ZACATECAS")
valores <- c()


mayorNumero <- 0
mayorDelegacion <- ""

for(j in 1:32)
{
  valorXDelegacion <- sum(informacionOriginal$Delegacion==delegaciones[j])
  valores <- c(valores, valorXDelegacion)
  
  if(valorXDelegacion > mayorNumero)
  {
    mayorNumero <- valorXDelegacion
    mayorDelegacion <- delegaciones[j]
  }
}

dfIncA <- data.frame(delegaciones, valores)
dfIncA <- dfIncA[order(dfIncA$valores,decreasing=TRUE),]
barplot(height = dfIncA$valores, names = dfIncA$delegaciones, las = 2)

print(mayorNumero)
print(mayorDelegacion)


# Inciso B: Porcentaje de particiapación por Delegación
informacionOriginal <- read.csv("C:\\Users\\patoe\\Desktop\\Proyectos en R\\BigDataEvidencia2\\Evidencia2Avance1\\Resultados-MPSG-Open-Data-oct10-feb20.csv", check.names = FALSE, fileEncoding = "Latin1")
delegaciones <- c("01AGUASCALIENTES","02BAJA CALIFORNIA","03BAJA CALIFORNIA SUR","04CAMPECHE","05COAHUILA","06COLIMA","07CHIAPAS","08CHIHUAHUA","09CIUDAD DE MÉXICO","10DURANGO","11GUANAJUATO","12GUERRERO","13HIDALGO","14JALISCO","15MÉXICO","16MICHOACÁN DE OCAMPO","17MORELOS","18NAYARIT","19NUEVO LEÓN","20OAXACA","21PUEBLA","22QUERÉTARO","23QUINTANA ROO","24SAN LUIS POTOSÍ","25SINALOA","26SONORA","27TABASCO","28TAMAULIPAS","29TLAXCALA","30VERACRUZ DE IGNACIO DE LA LLAVE","31YUCATÁN","32ZACATECAS")
porcentajes <- c()
total <- nrow(informacionOriginal)

for(j in 1:32)
{
  valorXDelegacion <- sum(informacionOriginal$Delegacion==delegaciones[j])
  
  porcentajeXDelegacion <- valorXDelegacion/total
  porcentajeXDelegacion <- porcentajeXDelegacion
  porcentajes <- c(porcentajes, porcentajeXDelegacion)
  
  cat(delegaciones[j]," ",porcentajeXDelegacion,"%","\n",sep = '')
}

dfIncB <- data.frame(delegaciones, porcentajes)
dfIncB <- dfIncB[order(dfIncB$porcentajes,decreasing=TRUE),]
pie(dfIncB$porcentajes, labels = dfIncB$delegaciones)
#barplot(height = dfIncB$porcentajes, names = dfIncB$delegaciones, las = 2)


# Inciso C: Preguntas respondidas, colocando quienes hayan contestado menos al principio
informacionOriginal <- read.csv("C:\\Users\\patoe\\Desktop\\Proyectos en R\\BigDataEvidencia2\\Evidencia2Avance1\\Resultados-MPSG-Open-Data-oct10-feb20.csv", check.names = FALSE, fileEncoding = "Latin1")
preguntas <- c("Pregunta 1","Pregunta 2","Pregunta 3","Pregunta 4","Pregunta 5","Pregunta 6","Pregunta 7","Pregunta 8","Pregunta 9","Pregunta 10","Pregunta 11","Pregunta 12","Pregunta 13","Pregunta 14","Pregunta 15","Pregunta 16","Pregunta 17","Pregunta 18","Pregunta 19","Pregunta 20","Pregunta 21","Pregunta 22","Pregunta 23","Pregunta 24","Pregunta 25","Pregunta 26","Pregunta 27","Pregunta 28","Pregunta 29","Pregunta 30","Pregunta 31","Pregunta 32","Pregunta 33","Pregunta 34","Pregunta 35","Pregunta 36","Pregunta 37","Pregunta 38","Pregunta 39","Pregunta 40","Pregunta 41","Pregunta 42","Pregunta 43","Pregunta 44","Pregunta 45","Pregunta 46","Pregunta 47","Pregunta 48","Pregunta 49","Pregunta 50","Pregunta 51","Pregunta 52","Pregunta 53","Pregunta 54","Pregunta 55","Pregunta 56","Pregunta 57","Pregunta 58","Pregunta 59","Pregunta 60","Pregunta 61","Pregunta 62","Pregunta 63","Pregunta 64","Pregunta 65","Pregunta 66","Pregunta 67","Pregunta 68","Pregunta 69","Pregunta 70","Pregunta 71","Pregunta 72")
vecesRespondidas <- c()

informacionOriginal["Pregunta 70"][is.na(informacionOriginal["Pregunta 70"])]<-0

for(j in 1:72)
{
  VecesPreguntaRespondida <- sum(informacionOriginal[,j+7]==1 | informacionOriginal[,j+7]=="SI")
  vecesRespondidas <- c(vecesRespondidas, VecesPreguntaRespondida)
}

listapreguntas <- data.frame(preguntas, vecesRespondidas)

print(listapreguntas[order(listapreguntas$vecesRespondidas, decreasing = FALSE),])

#Inciso D: Porcentaje de preguntas respondidas por localidad
informacionOriginal <- read.csv("C:\\Users\\patoe\\Desktop\\Proyectos en R\\BigDataEvidencia2\\Evidencia2Avance1\\Resultados-MPSG-Open-Data-oct10-feb20.csv", check.names = FALSE, fileEncoding = "Latin1")
localidades<- read.csv("C:\\Users\\patoe\\Desktop\\Proyectos en R\\BigDataEvidencia2\\Evidencia2Avance1\\Localidades.csv", check.names = FALSE, fileEncoding = "Latin1")
informacionOriginal["Pregunta 70"][is.na(informacionOriginal["Pregunta 70"])]<-0

for (j in 1:nrow(localidades)) 
{
  
  tablaFIltradaXLocalidad <- informacionOriginal[informacionOriginal$Localidad == localidades[j,2],]
  totalPreguntasRespondidasXLocalidad <- 0
  totalPreguntasRealizadasXLocalidad <- 74*nrow(tablaFIltradaXLocalidad)
  
  for(i in 1:nrow(tablaFIltradaXLocalidad))
  {
    totalPreguntasRespondidasXLocalidad <- totalPreguntasRespondidasXLocalidad + sum(tablaFIltradaXLocalidad[i,] == 1 | tablaFIltradaXLocalidad[i,] == "SI" | tablaFIltradaXLocalidad[i,] == "Si")
  }
  
  porcentajePreguntasRespondidasLocalidad <- totalPreguntasRespondidasXLocalidad/totalPreguntasRealizadasXLocalidad*100
  
  cat(localidades[j,2]," ",porcentajePreguntasRespondidasLocalidad,"%","\n",sep="")

} 