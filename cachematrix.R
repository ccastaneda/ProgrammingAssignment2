## Sample run from console
## > source("cachematrix.r")
## > b <- makeCacheMatrix()
## > b$set(matrix(1:4,2,2))
## > cacheSolve(b)
## Creating Inverted Matrix
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(b)
## Getting Cached Matrix
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## These functions work to provide a cache value of an inverted square matrix:
## makeCacheMatrix: This function creates a special object to hold the matrix and
## 					a list of several functions to work on it.
## cacheSolve: This function computes the inverse of the special "matrix" returned 
##			   by makeCacheMatrix above. If the inverse has already been calculated
##			   then the cacheSolve should return the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
    # initialize cache to NULL
    cache <- NULL
   
    set <- function(y) {				# create matrix in working environment
            x <<- y
            cache <<- NULL
    }   
    getMatrix <- function() x				# get the value of the matrix
				
    setMatrix <- function(inverted) 		  	# store inverted matrix in cache 
			cache <<- inverted				  
    
    getInverse <- function() cache			# get the inverted matrix from cache
						

    # return the created functions to the working environment
    list(set = set, getMatrix = getMatrix, setMatrix = setMatrix,      
         getInverse = getInverse)
}

## cacheSolve computes the inverse of the matrix 
## created in makeCacheMatrix
## If the inverted matrix does not exist in cache,
## it it created in the working environment and its inverted value
## is stored in cache
cacheSolve <- function(x, ...) {
    cache <- x$getInverse()				 	# get inverse matrix from cache

    # if exists, return inverted matrix from cache 
    # else create matrix
    if (!is.null(cache)) {
        print("Getting Cached Matrix")
        return(cache)
    }
		
    print("Creating Inverted Matrix")
    matrix <- x$getMatrix()					
    cache <- solve(matrix, ...)					# invert the matrix		
    x$setMatrix(cache)
    return (cache)             
}
