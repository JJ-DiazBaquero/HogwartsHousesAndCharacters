library(Rcrawler)
#Ejemplo de una afiliacion
Rcrawler("https://www.pottermore.com/collection/characters-gryffindors",MaxDepth = 1,
               ExtractXpathPat =c("/html/body/article[1]/div[1]/header/hgroup/div/h1",
                                   "/html/body/article[1]/div[2]/div/div[1]/div[2]/div/p",
                                  "//img/@data-src"),
               RequestsDelay = 5,PatternsName = c("Nombre", "Descripción","Imagen"))

vectorNombre = unlist(lapply(DATA, `[[`, 1))
vectorDesc = unlist(lapply(DATA, `[[`, 2))
vectorImg = unlist(lapply(DATA, `[[`, 3))
vectorCasa = rep("Gryffindor",length(vectorNombre))

datosTotales = data.frame(Nombre = vectorNombre, 
                          Descripcion = vectorDesc, 
                          Afiliacion = vectorCasa,
                          Imagen = vectorImg)
datosTotales = datosTotales[complete.cases(datosTotales),]

Rcrawler("https://www.pottermore.com/collection/characters-ravenclaws",MaxDepth = 1,
               ExtractXpathPat =c("/html/body/article[1]/div[1]/header/hgroup/div/h1",
                                  "/html/body/article[1]/div[2]/div/div[1]/div[2]/div/p",
                                  "//img/@data-src"),
               RequestsDelay = 5,PatternsName = c("Nombre", "Descripción","Imagen"))

vectorNombre = unlist(lapply(DATA, `[[`, 1))
vectorDesc = unlist(lapply(DATA, `[[`, 2))
vectorImg = unlist(lapply(DATA, `[[`, 3))
vectorCasa = rep("Ravenclaw",length(vectorNombre))

datosTotales = rbind(datosTotales,
                     data.frame(Nombre = vectorNombre, 
                                Descripcion = vectorDesc, 
                                Afiliacion = vectorCasa,
                                Imagen = vectorImg))

Rcrawler("https://www.pottermore.com/collection/characters-hufflepuffs",MaxDepth = 1,
               ExtractXpathPat =c("/html/body/article[1]/div[1]/header/hgroup/div/h1",
                                  "/html/body/article[1]/div[2]/div/div[1]/div[2]/div/p",
                                  "//img/@data-src"),
               RequestsDelay = 5,PatternsName = c("Nombre", "Descripción","Imagen"))

vectorNombre = unlist(lapply(DATA, `[[`, 1))
vectorDesc = unlist(lapply(DATA, `[[`, 2))
vectorImg = unlist(lapply(DATA, `[[`, 3))
vectorCasa = rep("Hufflepuff",length(vectorNombre))

datosTotales = rbind(datosTotales,
                     data.frame(Nombre = vectorNombre, 
                                Descripcion = vectorDesc, 
                                Afiliacion = vectorCasa,
                                Imagen = vectorImg))
datosTotales = datosTotales[complete.cases(datosTotales),]


Rcrawler("https://www.pottermore.com/collection/characters-slytherins",MaxDepth = 1,
         ExtractXpathPat =c("/html/body/article[1]/div[1]/header/hgroup/div/h1",
                            "/html/body/article[1]/div[2]/div/div[1]/div[2]/div/p",
                            "//img/@data-src"),
         RequestsDelay = 5,PatternsName = c("Nombre", "Descripción","Imagen"))

vectorNombre = unlist(lapply(DATA, `[[`, 1))
vectorDesc = unlist(lapply(DATA, `[[`, 2))
vectorImg = unlist(lapply(DATA, `[[`, 3))
vectorCasa = rep("Slytherin",length(vectorNombre))

datosTotales = rbind(datosTotales,
                     data.frame(Nombre = vectorNombre, 
                                Descripcion = vectorDesc, 
                                Afiliacion = vectorCasa,
                                Imagen = vectorImg))
datosTotales = datosTotales[complete.cases(datosTotales),]

write.csv(datosTotales, "datos.csv", row.names = F)


#Quitar libros
datosTotales = datosTotales[which(!datosTotales$Nombre %in% c("Fantastic Beasts and Where to Find Them",
                                                             "Harry Potter and the Cursed Child")),]


#pruebas de scraping
ejemplo = ContentScraper("https://www.pottermore.com/collection/characters",
               CssPatterns = c(".collection-artefact__description"),
               ManyPerPattern = T, PatternsName = c("Grupo"))

url = "https://www.pottermore.com/explore-the-story/minerva-mcgonagall"
url = "https://www.pottermore.com/explore-the-story/harry-potter"
ejemploPagChar = ContentScraper(url,
                                XpathPatterns = c("/html/body/article[1]/div[1]/header/hgroup/div/h1",
                                                  "/html/body/article[1]/div[2]/div/div[1]/div[2]/div/p",
                                                  "//img/@data-src"),
                                PatternsName = c("Nombre", "Descripción", "Imagen"))
"/html/body/article[1]/div[2]/div/div[1]/div[1]/div/picture/img"
ejemploPagChar$Imagen

for(i in length(datosTotales)){
  datosTotales[,i] = as.character(datosTotales[,i])
}
datosTotales[,3] = as.character(datosTotales[,3])

#Descarga de imagenes
imgVect = c()
for(i in datosTotales$Imagen){
  y = i
  y = paste("http:",y,sep = "")
  yArray = strsplit(y, "/")[[1]]
  imgName = yArray[length(yArray)]
  imgName= strsplit(imgName, "\\?")[[1]][1]
  imgName = gsub('[[:digit:]]+', '', imgName)
  download.file(y,paste("Pics/",imgName,sep=""), mode = 'wb')
  imgVect = append(imgVect,paste("Pics/",imgName,sep=""))
}
datosTotales$imgURI = imgVect

#Informacion de las casas
descCasas = c("With a lion as its crest and Professor McGonagall at its head, Gryffindor is the house which most values the virtues of courage, bravery and determination",
              "Ravenclaws prize wit, learning, and wisdom. It's an ethos etched into founder Rowena Ravenclaw diadem: 'wit beyond measure is man's greatest treasure'",
              "Hufflepuffs value hard work, patience, loyalty, and fair play. The house has produced its share of great wizards – not least Newt Scamander, author of Fantastic Beasts and Where to Find Them",
              "Slytherin produces more than its share of Dark wizards, but also turns out leaders who are proud, ambitious and cunning. Merlin is one particularly famous Slytherin")
imgCasas = c("Pics/Houses/Gryffindor_Pottermore.png",
             "Pics/Houses/Ravenclaw_Pottermore.png",
             "Pics/Houses/Hufflepuff_Pottermore.png",
              "Pics/Houses/Slytherin_Pottermore.png")

# http://www.color-hex.com/color-palette/813
# http://www.color-hex.com/color-palette/814
# http://www.color-hex.com/color-palette/816
# http://www.color-hex.com/color-palette/815

mainColor = c("rgb(116,0,1)", "rgb(34,47,91)", "rgb(236,185,57)", "rgb(26,71,42)")
secundaryColor = c("rgb(238,186,48)","rgb(148,107,45)", "rgb(0,0,0)", "rgb(170,170,170)")

unique(datosTotales$Afiliacion)

infoCasas = data.frame(house = unique(datosTotales$Afiliacion),
                       description = descCasas,
                       imagen = imgCasas,
                       mainColor = mainColor,
                       secondColor = secundaryColor)


#imprimir en formato JSON
library(RJSONIO)
casas = unique(datosTotales$Afiliacion)
listaCasas = list()
for(i in casas){
  listaPersonajes = list()
  personajes = datosTotales[datosTotales$Afiliacion == i,]
  for(j in 1:nrow(personajes)){
    listaPersonajes = append(listaPersonajes,
                             list(list(name = personajes$Nombre[j],
                                  descripcion =personajes$Descripcion[j],
                                  imagen = personajes$imgURI[j],
                                  mainColor = infoCasas$mainColor[infoCasas$house == i],
                                  secondColor = infoCasas$secondColor[infoCasas$house == i])))
  }
  listaCasas = append(listaCasas,list(list(name = i,
                                           descripcion = infoCasas$description[infoCasas$house == i],
                                           imagen = infoCasas$imagen[infoCasas$house == i],
                                           mainColor = infoCasas$mainColor[infoCasas$house == i],
                                           secondColor = infoCasas$secondColor[infoCasas$house == i],
                                          children = listaPersonajes)))
}
listaHogwarts = list(name = "Hogwarts",
                     descripcion = "Sprawling Scottish castle and celebrated School of Witchcraft and Wizardry",
                     imagen = "Pics/HogwartsCastle_WB_F_HogwartsThroughTheTrees_Illust__Land.jpg",
                     mainColor = "#501e63",
                     secondColor = "#8872b0",
                     children = listaCasas)
write(toJSON(listaHogwarts),file = "hogwartsHouseTree.json")
