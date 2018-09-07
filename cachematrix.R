## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than computing it repeatedly.
## The functions below calculate and cache the inverse of a matrix.
## That cached value is used to save computation time until the matrix is updated.
## A new inverse will be calculated and cached every time the matrix is updated.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(dataMatrix = matrix()) {

  savedInverse <- NULL

  updateMatrix <- function(newMatrix) {
    dataMatrix <<- newMatrix
    savedInverse <<- NULL
    print("Matrix updated")
    print(newMatrix)
    print("Cached inverse is now NULL")
  }

  getMatrix <- function() dataMatrix

  updateInverse <- function(newInverse) savedInverse <<- newInverse

  getInverse <- function() savedInverse

  list(updateMatrix=updateMatrix, getMatrix=getMatrix,
       updateInverse=updateInverse, getInverse=getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(funcList = list()) {
  
  inv <- funcList$getInverse()
  
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  
  print("Calculating inverse for the following matrix for the first time...")
  print(funcList$getMatrix())
  
  print("Inverse calculated")
  
  data <- funcList$getMatrix()
  inv <- solve(data)
  funcList$updateInverse(inv)
  
  inv
}

## Sample Run Log
##
## > myMatrix = rbind(c(4, 7), c(2, 6))
## > funcList = makeCacheMatrix(myMatrix)
## > cacheSolve(funcList)
## [1] "Calculating inverse for the following matrix for the first time..."
## [,1] [,2]
## [1,]    4    7
## [2,]    2    6
## [1] "Inverse calculated"
## [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4
## > cacheSolve(funcList)
## getting cached data.
## [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4
## > newMatrix = rbind(c(1, 0, 5), c(2, 1, 6), c(3, 4, 0))
## > funcList$updateMatrix(newMatrix)
## [1] "Matrix updated"
## [,1] [,2] [,3]
## [1,]    1    0    5
## [2,]    2    1    6
## [3,]    3    4    0
## [1] "Cached inverse is now NULL"
## > cacheSolve(funcList)
## [1] "Calculating inverse for the following matrix for the first time..."
## [,1] [,2] [,3]
## [1,]    1    0    5
## [2,]    2    1    6
## [3,]    3    4    0
## [1] "Inverse calculated"
## [,1] [,2] [,3]
## [1,]  -24   20   -5
## [2,]   18  -15    4
## [3,]    5   -4    1
## > cacheSolve(funcList)
## getting cached data.
## [,1] [,2] [,3]
## [1,]  -24   20   -5
## [2,]   18  -15    4
## [3,]    5   -4    1
