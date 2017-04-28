# Implementaciones para Accesibilidad a Calidad y Educación:

# Hacerlo sólo para colegios menores a 10 mil. Para educación básica.

# Modelo nuevo:
# Loop por cada colegio:
#   Tomar la distancia euclidiana entre el colegio y el centroide de todas las manzanas dentro del walkshed
#   Ordenar las manzanas en función de la distancia euclidiana.
#   Crear un contador: Por cada manzana, sumar la población de esa manzana al contador.
#   Cuando el contador == número de matriculados en el colegio, el "loop" se detiene.

# En caso de conflicto, se almacena el remanente de la manzana y se suma al siguiente colegio
# Que tenga esa manzana en su walkshed.

colegios_points <- readOGR(colegio)
walksheds <- readOGR(walksheds)
manzanas <- readOGR(manzanas)


Funcion que toma un colegio en particular:
  Econtrar walkshed con el ID del colegio.
  Hacer un subset de las manzanas en función del walkshed
  Tomar los centroides de las manzanas
  calcular la distancia euclidiana entre los centroides de las manzanas y el colegio, y ordenarlas de menor a mayor.
  retorna una lista con la población de las manzanas ordenadas de en función de la distancia del colegio.
  
Funcion que identifica el porcentaje de población con acceso en cada manzana para un colegio determinado
  Llamar a la función que ordena las manzanas
  capacidad <- colegio$matriculados
  poblacion <- 0

while (poblacion =< capacidad) {
  poblacion_manzana <- manzana@data$poblacion[i]
  poblacion <- poblacion + poblacion_manzana
  i <- i + 1
}

  