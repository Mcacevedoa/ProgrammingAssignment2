makeCacheMatrix <- función (x = matriz ()) {
  j <- NULL
  establecer <- función (y) {
  x << - y
  j << - NULL
  }
  obtener <- función () x
  setInverse <- función (inversa) j << - inversa
  getInverse <- function () j 
  lista (set = set, get = get, 
  setInverse = setInverse, 
  getInverse = getInverse)
}

## makeCacheMatrix es una función que crea un objeto especial "matriz" que puede
## cachee su inverso para la entrada (que es una matriz cuadrada invertible)

cacheSolve <- función (x, ...) {
## Devuelve una matriz que es la inversa de 'x'
  j <- x $ getInverse ()
  if (! is.null (j )) {
  mensaje ("obtener datos en caché")
  volver (j)
  }
  mat <- x $ get ()
  j <- resolver (mat, ...)
  x $ setInverse (j)
  j
}
## cacheSolve es una función que calcula el inverso de la "matriz" especial
## devuelto por makeCacheMatrix arriba. Si el inverso ya ha sido calculado
## (y la matriz no ha cambiado), entonces el caché debe recuperar el
## inversa desde el caché
