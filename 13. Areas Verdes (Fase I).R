# Fase I: 
# Esta fase consiste en preparar los datos para an�lisis de �reas verdes
# El script se compone de varias secciones:
# Primero, se filtran las �reas verdes de inter�s. Por ancho y �rea.
# Luego, se extraen los l�mites urbanos de las ciudades de inter�s (precenso 2011) 
# y se hace un clip a las �reas verdes (ya filtradas) seg�n los l�mites urbanos 
# de las �reas de inter�s.

# cargar librer�as necesarias para este script.

t1 <- Sys.time()
library(data.table); library(dplyr); library(rgdal)

library(sp);library(rgeos); library(alphahull)   

carpeta.origen <- "E:/Cedeus Sustainability Indicators/GIS/Areas Verdes (Manzana-Based Approach)/"

# Funciones de apoyo

MBR <- function(puntos) {
  # Funci�n para calcular el Minimum Bounding Rectangle, el cual consiste en
  # el rect�ngulo de �rea m�nima que se puede construir a partir de un conjunto
  # de puntos espacialmente definidos.
  # Fuente:
  # http://gis.stackexchange.com/questions/22895/finding-minimum-area-rectangle-for-given-points
  # Args:
  #   puntos: una matriz con las coordenadas de los puntos del pol�gono,
  #           a partir del slot "coords" de cada pol�gono en un shp.
  # Returns: una matriz con las coordenadas del "minimum bounding box"
  #   
  # 
  # Analyze the convex hull edges                       
  a <- ashape(puntos, alpha=1000)                 # One way to get a convex hull...
  e <- a$edges[, 5:6] - a$edges[, 3:4]            # Edge directions
  norms <- apply(e, 1, function(x) sqrt(x %*% x)) # Edge lengths
  v <- diag(1/norms) %*% e                        # Unit edge directions
  w <- cbind(-v[,2], v[,1])                       # Normal directions to the edges
  
  # Find the MBR
  vertices <- (puntos) [a$alpha.extremes, 1:2]    # Convex hull vertices
  minmax <- function(x) c(min(x), max(x))         # Computes min and max
  x <- apply(vertices %*% t(v), 2, minmax)        # Extremes along edges
  y <- apply(vertices %*% t(w), 2, minmax)        # Extremes normal to edges
  areas <- (y[1,]-y[2,])*(x[1,]-x[2,])            # Areas
  k <- which.min(areas)                           # Index of the best edge (smallest area)
  
  # Form a rectangle from the extremes of the best edge
  return(cbind(x[c(1,2,2,1,1),k], y[c(1,1,2,2,1),k]) %*% rbind(v[k,], w[k,]))
}

# Funci�n que analiza un pol�gono, y determina si este posee un �rea mayor a 5000
# metros cuadrados, y si tiene un ancho mayor a 10 metros.

ClasificadorPoligono <- function(poligono, area.minima, ancho.minimo){
  # Area del pol�gono. Esta debe estar en un CRS (Cartographic Reference System)
  # cuya unidad de medida sea en metros. En caso de no ser as�, definirlo en los
  # shapefiles mediante proj4string, o en un SIG de preferencia.
  poligono.area <- poligono@Polygons[[1]]@area
  # si el �rea es menor a "area.minima", arrojar "FALSO" (No cumple con los 
  # requisitos m�nimos)
  if (poligono.area < area.minima){ return(FALSE)}
  # Coordenadas del pol�gono
  poligono.coords <- unique(poligono@Polygons[[1]]@coords)
  
  # Minimum Bounding Rectangle. Ya que la funci�n MBR utiliza la funci�n ashape,
  # la cual tiene como requisito que no pueden haber puntos consecutivos y 
  # colineales, se randomiza el orden de los puntos. Esto no altera la geometr�a
  # del pol�gono. En caso de shapes producidos por geoprocesos como "Union" o 
  # "intersect", ser� necesario limpiar la geometr�a de estos antes de procesarlos
  # con este script, puesto que la probabilidad de generar problemas por puntos
  # colineales es muy alta.
  
  # Los triangulos y trapecios arrojan errores con el minimum Bounding rectangle
  # Por ello, si el n�mero de puntos es igual o menor a 4, a�adir un punto 
  # "dummy" que consiste en el promedio de los puntos X e Y.
  
  if (nrow(poligono.coords) <= 4){
    poligono.coords <- rbind(poligono.coords, c(mean(poligono.coords[,1]), 
                                                mean(poligono.coords[,2])))
  }
  # C�lculo del Minimum Bounding Rectangle. En caso de que falle, se intenta de nuevo
  
  mbr <- tryCatch(MBR(poligono.coords[sample(nrow(poligono.coords)),]), 
                  error = function(e){ 
                    print("Error. Puntos colineales en pol�gono, reordenando")
                    MBR(poligono.coords[sample(nrow(poligono.coords)),])
                    })
  # Distancias entre puntos del MBR
  distancias <- unique(spDistsN1(mbr, mbr[2,]))
  # Ancho del MBR = distancia m�s peque�a mayor a 0
  ancho <- (min(distancias[distancias > 0]))
  # Largo del MBR Dado por la mediana (ancho es el m�nimo, y la diagonal es el m�ximo)
  largo <- median(distancias[distancias > 0])
  # Area Hipot�tica M�nima: La superficie m�nima que deber�a tener el pol�gono 
  # del �rea verde si tuviese un ancho de 10 metros. De esta forma, se filtran 
  # aquellas �reas verdes que tengan un ancho inferior a este.
  AHM <- largo*10
  # Si el ancho es mayor a 10, y el area mayor al ATM, retorna TRUE
  ifelse(( (ancho > 10) & (poligono.area > AHM)), return(TRUE), return(FALSE))
}

# Funci�n que "clippea" las �reas verdes al l�mite urbano
# Input: Ciudad a trabajar, carpeta con las �reas verdes,
#        Carpeta con el l�mite urbano.
# Output: �reas verdes dentro del l�mite urbano
Filtro.LimiteUrbano <- function(Ciudad, carpeta_aavv, carpeta_limurb){
 
  # Extrae el nombre del shp de limite urbano en la carpeta desginada, para luego leerlo
  lista_limurb <- gsub(".dbf", "", list.files(carpeta_limurb, ".dbf"))
  nombre_limurbshp <- lista_limurb[grep(Ciudad, lista_limurb)]
  limiteurbano <- readOGR(carpeta_limurb, nombre_limurbshp, encoding = "UTF-8")
  
  # Carpeta donde est�n los shp originales
  carpeta_aavvrawdata <- paste(carpeta_aavv, "/Areas Verdes (Raw)", sep = "")
  # Extrae el nombre del shp de �reas verdes en la carpeta desginada, para luego leerlo
  lista_aavv <- gsub(".dbf", "", list.files(carpeta_aavvrawdata, ".dbf"))
  nombre_aavv <- lista_aavv[grep(Ciudad, lista_aavv)]
  aavv <- readOGR(carpeta_aavvrawdata, nombre_aavv, encoding = "UTF-8")
  
  # Reprojectar el shp de limite urbano usando el CRS de �reas verdes
  limiteurbano <- spTransform(limiteurbano, proj4string(aavv))
  
  # El Clip de las �reas verdes seg�n l�mite urbano consiste en un "intersect".
  # Ahora, hay casos en los que arroja un error, el cual se soluciona usando un
  # buffer dummy de 0 metros. Por qu� ? No lo s� :/
  aavv_dummy <- gBuffer(aavv, byid=TRUE, width=0)
  limiteurbano_dummy <- gBuffer(limiteurbano, byid=TRUE, width=0)
  aavv_subset <- aavv[gIntersection(aavv_dummy, limiteurbano_dummy, byid = TRUE,
                                    drop_lower_td = TRUE),] #clip polygon 2 with polygon 1
  
  return(aavv_subset)
}

# Funci�n para seleccionar las �reas verdes a analizar seg�n tipo de area verde.
# El filtro var�a de Fuerte, Medio, Leve, Los cuales est�n detallados 
# en "Inclusion_AAVV_Indicador.csv
# Input: Ciudad a trabajar, archivo con los criterios a seleccionar,
#        Criterio a seleccionar, y shapefile con �reas verdes
Reglas_aavv <- function(Ciudad, reglafile, reglatipo, aavv){
  if (!(Ciudad %in% unique(reglafile$Ciudad))){ return(aavv)}
  # Primero, se hace un subset de la ciudad
  reglas_ciudad <- reglafile[reglafile$Ciudad == as.character(Ciudad),]
  # Identificar el campo que contiene los tipos de areas verdes (Hasta ahora, Tipo vs SUBTIPO)
  # Identificar los atributos del .csv que contienen las clasificaciones
  campos <- reglas_ciudad[grep("Campo_",colnames(reglas_ciudad))]
  # Registrar el nombre del campo que contiene registros v�lidos (no NA)
  reglanombre_old <- colnames(campos[colSums(!is.na(campos)) > 0])
  # Registrar la ID del campo.
  reglanombre_old_id <- which( colnames(reglas_ciudad)==reglanombre_old )
  # Registrar un nuevo nombre para el campo, eliminando el sufijo "Campo_". 
  reglanombre <- gsub("Campo_", "", reglanombre_old)
  # Usando la id del campo con registros v�lidos, cambiamos el nombre por uno sin el sufijo.
  # Esto es para posterior uso en el subset del shapefile de areas verdes.
  colnames(reglas_ciudad)[reglanombre_old_id] <- c(reglanombre)
  # Luego, un subset del campo con los tipos de areas verdes, y el tipo de reglas a usar
  reglas_ciudad <- reglas_ciudad[,c(reglanombre, reglatipo)]
  # Finalmente, un subset de aquellas que s� est�n incluidas.
  reglas_ciudad <- reglas_ciudad[which(reglas_ciudad[,2] == "Si"),]
  # Se hace un subset del shape de aavv seg�n las areas verdes definidas en reglas_ciudad.
  aavv_filtrada <- aavv[aavv[[reglanombre]] %in% reglas_ciudad[[reglanombre]],]
  return(aavv_filtrada)
}

# Funci�n para identificar las manzanas dentro del l�mite urbano, y obtener sus
# Centroides.
# Input: Carpeta donde est�n los shp de manzanas, y la ciudad a analizar.
# Output: una tabla con la ciudad, ID y coordenadas de los centroides.
centroides.fun <- function(carpeta.manzanas, ciudad){
  manzanas.shp <- readOGR(carpeta.manzanas, paste(ciudad, "_Poblacion_Manzanas", sep =""))
  centroides.shp <- gCentroid(manzanas.shp, byid = T)
  centroides.df <- data.frame("Ciudad" = ciudad, "ID" = manzanas.shp$ID_W, centroides.shp@coords)
  return(centroides.df)
}

#### Almacenar la carpeta donde est�n los shapes de areas verdes, l�mite urbano
# Y manzanas

carpeta_aavv <- paste(carpeta.origen, sep = "")
carpeta_limurb <- paste(carpeta.origen, "Limite Urbano", sep = "")
carpeta.manzanas <- paste(carpeta.origen, "Poblacion Manzanas", sep = "")

# archivo .csv que contiene datos sobre Nombres de ciudades, sus comunas y sus c�digos.
city_codes <- read.csv(paste(carpeta.origen, "/Study_codes.csv", sep = ""), 
                       sep=",")
# Vector que almacena los nombres de las ciudades
ciudades <- unique(city_codes$Ciudad)
# archivo .csv que contiene los criterios para definir qu� �reas verdes es analizar�n. 
reglafile <- read.csv(paste(carpeta.origen, "/Inclusion_AAVV_Indicador.csv", 
                            sep = ""), sep= ";", encoding = "UTF-8")
# Tipo de clasificaci�n a utilizar
reglatipo <- "Inclusion.Fuerte"
# Tabla vacia para almacenar los centroides de las manzanas a usar en Fase II
centroides <- data.frame()

# Loop que por cada ciudad:
# I) Selecciona s�lo las que est�n dentro del l�mite urbano definido en el Precenso 2011
# II) Selecciona las �reas verdes cuyo tipo/subtipo calce con los criterios de 
#     selecci�n establecidos en "reglafile"
# III) Crea una lista para ejecutar un loop en el cual 
#      Por cada pol�gono en el shape ejecuta la funci�n "ClasificadorPoligono", 
#      la cual selecciona en funci�n de �rea y ancho
# IV) Se crea un nuevo shape en base a un subset del shape original con los 
#     pol�gonos que cumplen las condiciones.

for (o in 1:length(ciudades)){
  # Identificar la Ciudad a trabajar
  Ciudad <- ciudades[o]
  # Leer el shape
  areas_verdes_limurb <- Filtro.LimiteUrbano(Ciudad, carpeta_aavv, carpeta_limurb)
  # Filtrar seg�n criterio. Si la ciudad no est� incluida en los criterios de clasificaci�n,
  # Retorna el mismo �rea verde
  areas_verdes_reglas <- Reglas_aavv(Ciudad, reglafile, reglatipo, areas_verdes_limurb)
  # Crear un vector donde se almacenan los ids de los pol�gonos que cumplan los requisitos.
  lista <- vector()
  # Por cada pol�gono se ejecuta la funci�n ClasificadorPoligono, que determina 
  # si cada pol�gono Individual cumple con los requisitos especificados de �rea 
  # m�nima y ancho del pol�gono. Cada pol�gono es asignado un valor TRUE o FALSE,
  # seg�n si cumple o no los requisitos. Luego se seleccionan s�lo los pol�gonos
  # que cumplan los requisitos, y se almacenan en un nuevo objeto llamado new_shape.
  for (i in 1:nrow(areas_verdes_reglas)){
    poligono <- areas_verdes_reglas@polygons[[i]]
    poligono.aprobado <- ClasificadorPoligono(poligono, area.minima = 5000, ancho.minimo = 10)
    lista <- append(lista, poligono.aprobado)
  }
  new_shape <- areas_verdes_reglas[lista,]
  # Almacenar el tipo de criterio, para a�adirlo como sufijo al shape final.
  reglatipo_sufijo <- gsub("Inclusion", "",reglatipo)
  
  # Se exporta el nuevo shapefile con el sufijo "AAVV" y el tipo de criterio 
  # usado (Fuerte, Medio, Leve), en la carpeta "Procesado"
  
  writeOGR(as(new_shape, "SpatialPolygonsDataFrame" ), 
           paste(carpeta_aavv, "/Procesado", sep = ""), 
           paste(Ciudad, "_AAVV_", reglatipo_sufijo,sep= ""),
           driver = "ESRI Shapefile",overwrite_layer=TRUE)
  centroides.tmp <- centroides.fun(carpeta.manzanas, Ciudad)
  centroides <- rbind(centroides, centroides.tmp)
}
write.csv(centroides, paste(carpeta.origen, "Centroides_Manzanas/Centroides_Manzanas.csv", sep = ""))
t2 <- Sys.time()
