## R Programming Assignment 2:  Creating function for inverse computation of matrix with ability to retrieve cached data

## Given the high degree of computational cost associated with calculating matrix inversion, 
## the purpose of this assignment will be to cahce the inverse of a matrix in order to avoid the 
## need to compute it multiple times.  

## Two functions will be created.
### makeCacheMatrix takes a matrix as input and creates a list containing a function which:
#### 1. Sets the value of the matrix
#### 2. Gets the value of the matrix
#### 3. Sets the value of the inverse of the matrix
#### 4. gets the value of the inverse of the matrix
## Usage example:
## x <- matrix(1:4, nrow=2, ncol=2)
## m <- makeCacheMatrix(x)

## This is the first function taking a matrix as input and creating a special matrix with cahced inverses
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  # Creates the set function, which sets the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  # Creates a function to set the inverse
  setinverse <- function(inverse) inv <<- inverse
  # Creates a function to get the inverse
  getinverse <- function() inv
  # Creates a list with matrix and cahced inverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The cacheSolve function returns the inverse of the matrix after first determining if the iverse
## has already been computed.  If there is no cached matrix available already, it will determine the inverse
## and then set it in the cahce via the setinverse function defined above. 
## Of note, it assume that the matrix is always invertible. 

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Retrieving cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

## Perform tests of the caching modality. 
x = rbind(c(1, -1/4), c(-1/4, 1))
m = makeCacheMatrix(x)
m$get()
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00

## No cached data were found for the first iteration.
cacheSolve(m)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

## Cached data will be available in the second iteration. 
cacheSolve(m)
## Retrieving cached data.
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
