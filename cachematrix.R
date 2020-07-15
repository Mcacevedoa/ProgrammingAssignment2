makeCacheMatrix <- function (x = matrix ()) {
  j <- NULL   # Ajustar j como NULL para que contenga el valor de la inversa
  set <- function (y) {
    x << - y
    ji << - NULL # Si hay una nueva matrix, j se resetea a NULL
  }
  get <- function () x # La función get devuelve el valor del argumento de matriz
  
  setinverse <- function (j) ji <<- j # Asigna un valor de inf en el "parent enviroment"
  getinverse <- function () ji # obtener el valor de ji donde se llama
  list(set = set, get = get, 
  setinverse = setinverse, getinverse = getinverse)
}

## makeCacheMatrix es una función que crea un objeto especial "matriz" que puede
## cachee su inverso para la entrada (que es una matriz cuadrada invertible)

cacheSolve <- function (x, ...) {
## Devuelve una matriz que es la inversa de 'x'
  ji <- x $ getInverse ()
  if (! is.null (ji)) {
    mensaje ("obtener datos en caché")
    volver (ji)
  }
  mat <- x $ get ()
  ji <- solve(mat, ...)
  x $ getinverse (ji)
  ji
}
## cacheSolve es una función que calcula el inverso de la "matriz" especial
## devuelto por makeCacheMatrix arriba. Si el inverso ya ha sido calculado
## (y la matriz no ha cambiado), entonces el caché debe recuperar el
## inversa desde el caché
