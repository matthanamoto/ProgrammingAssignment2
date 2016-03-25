## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix's purpose is to create the cache space for the cacheSolve function.
## it is not a glamorous job. See: https://www.youtube.com/watch?v=3ht-ZyJOV2k&feature=youtu.be
 
makeCacheMatrix <- function(x = matrix()){
  ## x is assumed to be an inversible matrix
  inv = NULL
  ## set variable inv as null... this is important because it is going to dictate if
  ## R retrievs the cached data in the following function.
  set = function(y){
    inv <<- NULL
  }
  ##  
  get = function() x
  setinverse = function(solve) inv <<- solve
  getinverse = function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  }

testmatrix = diag(5,3)
cached.matrix = makeCacheMatrix(testmatrix)

## cacheSolve's purpose is to calculate the inverse of a matrix and store the matrix.
## cacheSolve can also pull the inversed matrix if the inverse has been calculated.

cacheSolve <- function(cachedmatrix, ...) {
  inv <- cached.matrix$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## If inv has a NULL value, move to inversing the matrix
  ## This should be the case if you are running this the first time.
  ## If inv has been calculated already, return the cached matrix
  matrix.data <- cached.matrix$get()
  inv <- solve(matrix.data, ...)
  # Used "solve" function to compute inverse of a square matrix.
  cached.matrix$setinverse(inv)
  inv
}

cacheSolve(cached.matrix)
