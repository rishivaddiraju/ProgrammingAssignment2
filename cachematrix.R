# Relationship between makeCacheMatrix and cacheSolve
# makeCacheMatrix is constructing the matrix that cacheSolve takes to solve and return the inverse of a given matrix





# makeCacheMatrix is a function that initializes a R matrix type object in which its inverse can be cached.
makeCacheMatrix <- function(x = matrix()) {
  invrse <- NULL
  set <- function(z){
    x <<- z
    invrse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invrse <<- inverse
  getInverse <- function() invrse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


# cacheSolve checks to see if the inputted matrix has been evaluated, and if it has not yet been evaluated, it goes ahead to evaluate it and return it. 
cacheSolve <- function(x, ...) {
  invrse <- x$getInverse()
  if(!is.null(invrse)){
    message("getting cached data")
    return(invrse)
  }
  matr <- x$get()
  invrse <- solve(matr, ...)
  x$setInverse(invrse)
  invrse
}
